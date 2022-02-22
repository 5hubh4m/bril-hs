import Test.QuickCheck

prop_helloWorld :: Bool
prop_helloWorld = True

main :: IO ()
main = do
  quickCheck prop_helloWorld
