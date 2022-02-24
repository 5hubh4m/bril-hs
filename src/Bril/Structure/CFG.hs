{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bril.Structure.CFG where

import           Bril.Lang.AST
import           Data.Bifunctor
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Tree
import           GHC.Generics
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

-- | a CFG is a map from block identifiers to
--   it's block, successors, and prececessors
data CFG = CFG
         { entry      :: Ident
         , blocks     :: S.HashSet Ident
         , instrs     :: M.HashMap Ident [Instruction]
         , successors :: M.HashMap Ident (S.HashSet Ident)
         }
         deriving (Show, Eq, Generic, Hashable)

-- | get all the basic blocks from a function and if it
--   has no instructions then return at least an empty
--   block
basicBlocks :: Function -> [[Instruction]]
basicBlocks (Function _ _ _ instrs) = finalize $ filter (not . null) $ process $ foldl' fn ([], []) instrs
  where
    finalize l                  = if null l then [[]] else l
    process (blocks, curr)      = blocks ++ [curr]
    terminator (Effect Jmp {})  = True
    terminator (Effect Ret {})  = True
    terminator (Effect Br {})   = True
    terminator _                = False
    fn (blocks, curr) (Label l) = (blocks ++ [curr], [Label l])
    fn (blocks, curr) instr
      | terminator instr = (blocks ++ [curr ++ [instr]], [])
      | otherwise        = (blocks, curr ++ [instr])
{-# INLINABLE basicBlocks #-}

-- | get the set of all labels in a function
allLabels :: Function -> S.HashSet Ident
allLabels (Function _ _ _ instrs) = S.fromList $ labels =<< instrs
{-# INLINABLE allLabels #-}

-- | uniquely label each block in the list of basic blocks
blockLabels :: Function -> [(Ident, [Instruction])]
blockLabels f = fn $ basicBlocks f
  where
    labels              = allLabels f
    fn []               = []
    fn (block : blocks) = (label, block) : fn blocks
      where
        findUnique idx
          | S.member (create idx) labels = findUnique $ idx + 1
          | otherwise                    = create idx
        create = Ident . T.pack . ("b" ++) . show
        label  = case head block of
                   Label l -> l
                   _       -> findUnique $ length blocks
{-# INLINABLE blockLabels #-}

-- | takes in a function and returns a map from
--   a block label to it's successors
graph :: Function -> M.HashMap Ident (S.HashSet Ident)
graph f = foldl' fn init $ zip [0..] bs
  where
    ls   = basicBlocks f
    ll   = allLabels f
    bs   = blockLabels f
    n    = length ls
    init = M.fromList $ zip (S.toList ll) $ repeat S.empty
    fn map (idx, (label, []))
      | idx < n - 1 = M.insert label (S.singleton . fst $ bs !! (idx + 1)) map
      | otherwise   = M.insert label S.empty map
    fn map (idx, (label, block)) = M.insert label succ map
      where
        succ = case last block of
                 (Effect (Br _ tl fl)) -> S.fromList [tl, fl]
                 (Effect (Jmp l))      -> S.singleton l
                 _ | idx < n - 1       -> S.singleton . fst $ bs !! (idx + 1)
                 _                     -> S.empty
{-# INLINABLE graph #-}

-- | takes in a graph of successors and returns a set
--   of prececessors in other words, invert a graph's matrix
invert :: (Eq a, Hashable a) => M.HashMap a (S.HashSet a) -> M.HashMap a (S.HashSet a)
invert succ = M.foldlWithKey' fn (S.empty <$ succ) succ
  where
    fn map k vs = M.unionWith S.union map $ S.singleton k <$ S.toMap vs
{-# SPECIALIZE invert :: M.HashMap Ident (S.HashSet Ident) -> M.HashMap Ident (S.HashSet Ident) #-}
{-# INLINEABLE invert #-}

-- | create the CFG of a function
cfg :: Function -> CFG
cfg f = CFG (fst $ head ls) ll (M.fromList ls) (graph f)
  where
    ll = allLabels f
    ls = blockLabels f
{-# INLINABLE cfg #-}

-- | get the prececcssor graph in a CFG
prececessors :: CFG -> M.HashMap Ident (S.HashSet Ident)
prececessors = invert . successors
{-# INLINABLE prececessors #-}

-- | create a list of CFG blocks in post-order traversal
--   starting from the entry block
postorder :: CFG -> [Ident]
postorder cfg = snd . fn S.empty $ entry cfg
  where
    succs        = successors cfg
    preds        = prececessors cfg
    fn seen root = if S.member root seen then (seen, [])
                   else second (++ [root]) $ foldl' upd (seen', []) next
      where
        children     = succs M.! root
        next         = S.toList $ S.difference children seen
        seen'        = S.insert root seen
        upd (s, l) n = second (l ++) $ fn s n
{-# INLINABLE postorder #-}

-- | find the intersection of all the sets in the given container
intersections :: (Foldable t, Eq a, Hashable a) => t (S.HashSet a) -> S.HashSet a
intersections x = if null x then S.empty else foldl1 S.intersection x
{-# SPECIALIZE intersections :: S.HashSet (S.HashSet Ident) -> S.HashSet Ident #-}
{-# SPECIALIZE intersections :: [S.HashSet Ident] -> S.HashSet Ident #-}
{-# INLINABLE intersections #-}

-- | find the union of all the sets in the given container
unions :: (Foldable t, Eq a, Hashable a) => t (S.HashSet a) -> S.HashSet a
unions = foldl' S.union S.empty
{-# SPECIALIZE unions :: S.HashSet (S.HashSet Ident) -> S.HashSet Ident #-}
{-# SPECIALIZE unions :: [S.HashSet Ident] -> S.HashSet Ident #-}
{-# INLINABLE unions #-}

-- | finds the dominators of all blocks in a CFG
dominators :: CFG -> M.HashMap Ident (S.HashSet Ident)
dominators cfg = finalise $ go init
  where
    ls            = blocks cfg
    succs         = successors cfg
    preds         = prececessors cfg
    verts         = reverse $ postorder cfg
    start         = entry cfg
    -- entry has itself, and others have everyone as their dominator
    init          = M.insert start (S.singleton start) $ ls <$ succs
    -- dominators of a block are the block and the
    -- intersection of the dominators of it's predecessors
    fn m v        = S.insert v $ intersections $ S.map (m M.!) $ preds M.! v
    upd (ch, m) v = let s = fn m v in (ch || s /= m M.! v, M.insert v s m)
    -- iteratively update dominators for all blocks
    -- except entry until they are changing
    go doms       = let (change, doms') = foldl' upd (False, doms) $ tail verts in
                    if not change then doms else go doms'
    -- for blocks unreachable from the entry
    -- the dominator set is empty
    unreachable   = S.difference ls $ S.fromList verts
    finalise      = M.union (S.empty <$ S.toMap unreachable)
{-# INLINABLE dominators #-}

-- | find the domination tree of the CGF
dominationTree :: CFG -> Tree Ident
dominationTree cfg = fn start
  where
    start     = entry cfg
    -- doms is your dominators, doms'
    -- is whom you dominate
    doms      = dominators cfg
    doms'     = invert doms
    strict  v = S.delete v $ doms  M.! v
    strict' v = S.delete v $ doms' M.! v
    -- r is the immediate dominator of v if the strict
    -- dominators of v are the dominators of r
    idom  r v = doms M.! r == strict v
    fn r      = Node r $ fn <$> S.toList children
      where
        -- your children in the domination tree are
        -- those for which you are an immediate dom
        children = S.filter (idom r) $ strict' r
{-# INLINABLE dominationTree #-}

-- | find the domination frontier of the CFG
dominationFrontier :: CFG -> M.HashMap Ident (S.HashSet Ident)
dominationFrontier cfg = M.mapWithKey fn doms'
  where
    succs  = successors cfg
    doms'  = invert $ dominators cfg
    -- your frontier is the set of successors of
    -- your dominatees whom you do not strictly dominate
    fn v s = S.difference (unions $ S.map (succs M.!) s) (S.delete v s)
{-# INLINABLE dominationFrontier #-}
