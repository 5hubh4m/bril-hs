import           Bril.Lang.AST
import           Bril.Lang.Parse
import           Bril.Structure.CFG
import           Data.Aeson
import           Data.List
import           Data.Semigroup
import           Data.Tree
import           System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet as S

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
printDoms f = mapM_ printLine . M.toList . dominators $ cfg f

-- | append two spaces to every line in the string
appendSpaces :: String -> String
appendSpaces x = unlines $ ("  " ++ ) <$> lines x

-- | print the domination tree of a function
printDomTree :: Function -> IO ()
printDomTree f = putStrLn . appendSpaces . drawTree . (show <$>) . dominationTree $ cfg f

-- | print the domination frontier of each block in a function
printDomFront :: Function -> IO ()
printDomFront f = mapM_ printLine . M.toList . dominationFrontier $ cfg f

main :: IO ()
main = do args <- getArgs
          let action = if null args then "doms" else head args
          let f = case action of
                    "tree"  -> printDomTree
                    "front" -> printDomFront
                    _       -> printDoms
          bytes <- B.getContents
          case eitherDecode bytes of
            Left e             -> putStrLn e
            Right (Program fs) -> mapM_ (\x -> printName x *> f x) fs
