import           Bril.Lang.AST
import           Bril.Lang.Parse
import           Bril.Structure.SSA
import           Data.Aeson
import           System.Environment
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do bytes <- B.getContents
          args  <- getArgs
          let fn = case args of
                     "to"   : _ -> ssa
                     "from" : _ -> ssa'
                     _          -> id
          case eitherDecode bytes of
            Left e             -> putStrLn e
            Right (Program fs) -> B.putStr . encode . Program $ fn <$> fs
