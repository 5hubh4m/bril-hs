{-# LANGUAGE OverloadedStrings #-}

module Bril.Structure.CFG where

import           Bril.Lang.AST
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import           Data.Maybe
import           Data.Tree
import           Util.Graph
import           Util.Misc
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

-- | a CFG is a map from block identifiers to
--   it's block, successors, and prececessors
data CFG = CFG
         { entry        :: Ident
         , blocks       :: [Ident]
         , successors   :: Graph Ident
         , instructions :: M.HashMap Ident [Instruction]
         }
         deriving (Show, Eq)

-- | get all the basic blocks from a function and if it
--   has no instructions then return at least an empty
--   block
basicBlocks :: Function -> [[Instruction]]
basicBlocks (Function _ _ _ instrs) = finalize $ filter (not . null) $ process $ foldl' fn ([], []) instrs
  where
    finalize l                  = if null l then [[]] else l
    process (blocks, curr)      = blocks ++ [curr]
    fn (blocks, curr) (Label l) = (blocks ++ [curr], [Label l])
    fn (blocks, curr) instr
      | terminator instr = (blocks ++ [curr ++ [instr]], [])
      | otherwise        = (blocks, curr ++ [instr])
{-# INLINABLE basicBlocks #-}

-- | get the set of all labels in a function
allLabels :: Function -> S.HashSet Ident
allLabels (Function _ _ _ instrs) = S.fromList $ labels =<< instrs
{-# INLINABLE allLabels #-}

-- | create a fresh label not present in the given ones
freshLabel :: S.HashSet Ident -> Ident
freshLabel ls = findUnique 0
  where
    findUnique idx
      | S.member (create idx) ls = findUnique $ idx + 1
      | otherwise                = create idx
    create = Ident . T.pack . ("b" ++) . show

-- | uniquely label each block in the list of basic blocks
blockLabels :: Function -> [(Ident, [Instruction])]
blockLabels f = snd $ fn (labels, basicBlocks f)
  where
    labels                  = allLabels f
    fn (ls, [])             = (ls, [])
    fn (ls, block : blocks) = second ((l, is) :) $ fn (S.insert l ls, blocks)
      where
        new     = freshLabel ls
        (l, is) = case listToMaybe block of
                    Just (Label l) -> (l, block)
                    _              -> (new, Label new : block)
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

-- | create the CFG of a function
cfg :: Function -> CFG
cfg f = CFG (fst $ head ls) (fst <$> ls) (Graph ll $ graph f) $ M.fromList ls
  where
    ll = allLabels f
    ls = blockLabels f
{-# INLINABLE cfg #-}

-- | get the prececcssor graph in a CFG
prececessors :: CFG -> Graph Ident
prececessors = invert . successors
{-# INLINABLE prececessors #-}

-- | finds the dominators of all blocks in a CFG
dominators :: CFG -> M.HashMap Ident (S.HashSet Ident)
dominators cfg = finalise $ go init
  where
    start         = entry cfg
    succs         = edges $ successors cfg
    preds         = edges $ prececessors cfg
    blocks        = vertices $ successors cfg
    verts         = reverse $ postorder (successors cfg) start
    -- entry has itself, and others have everyone as their dominator
    init          = M.insert start (S.singleton start) $ blocks <$ succs
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
    unreachable   = S.difference blocks $ S.fromList verts
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
    doms'     = transpose doms
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
    succs  = edges $ successors cfg
    doms'  = transpose $ dominators cfg
    -- your frontier is the set of successors of
    -- your dominatees whom you do not strictly dominate
    fn v s = S.difference (unions $ S.map (succs M.!) s) (S.delete v s)
{-# INLINABLE dominationFrontier #-}
