import           Bril.Lang.AST
import           Bril.Lang.Parse
import           Bril.Optim.DCE
import           Bril.Optim.Loop
import           Bril.Structure.CFG
import           Bril.Structure.SSA
import           Data.Aeson
import           Data.Maybe
import           Data.Tree
import           System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S

-- | print a set of objects
identSet :: Show a => S.HashSet a -> String
identSet s = if null s then "∅" else foldl1 cat $ S.map show s
  where
    cat x y = x ++ ", " ++ y

-- | print the name of a function
printName :: Function -> IO ()
printName (Function n _ _ _) = putStrLn $ show n ++ ":"

-- | print an entry in a map
printLine :: Show a => (a, S.HashSet a) -> IO ()
printLine (i, ss) = putStrLn $ "  " ++ show i ++ ": " ++ identSet ss

-- | print the dominators of each block in a function
printDoms :: Function -> IO ()
printDoms f = mapM_ printLine . M.toList . dominators $ mkCFG f

-- | append two spaces to every line in the string
appendSpaces :: String -> String
appendSpaces x = unlines $ ("  " ++ ) <$> lines x

-- | print the domination tree of a function
printDomTree :: Function -> IO ()
printDomTree f = putStrLn . appendSpaces . drawTree . (show <$>) . dominationTree $ mkCFG f

-- | print the domination frontier of each block in a function
printDomFront :: Function -> IO ()
printDomFront f = mapM_ printLine . M.toList . dominationFrontier $ mkCFG f

-- | perform the given action with the program
action :: Program -> String -> IO ()
action (Program fs) "ssa"   = B.putStr . encode . Program $ tdce . ssa . tdce <$> fs
action (Program fs) "nossa" = B.putStr . encode . Program $ tdce . ssa' . tdce <$> fs
action (Program fs) "tdce" = B.putStr . encode . Program $ tdce <$> fs
action (Program fs) "licm" = B.putStr . encode . Program $ licm <$> fs
action (Program fs) "tree"  = mapM_ (\x -> printName x *> printDomTree x) fs
action (Program fs) "front" = mapM_ (\x -> printName x *> printDomFront x) fs
action (Program fs) "doms"  = mapM_ (\x -> printName x *> printDoms x) fs
action (Program fs) _       = return ()

main :: IO ()
main = do bytes <- B.getContents
          args  <- listToMaybe <$> getArgs
          case eitherDecode bytes of
            Left e  -> putStrLn e
            Right p -> action p $ fromMaybe "" args
