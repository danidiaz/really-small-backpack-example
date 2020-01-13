module Main where


import qualified Lesson5Ordered
import qualified Lesson5Hash

main :: IO ()
main = do
    print $ Lesson5Ordered.lookup (1::Int) (Lesson5Ordered.fromList [(1,True),(2,False)])
    print $ Lesson5Hash.lookup (1::Int) (Lesson5Hash.fromList [(1,True),(2,False)])
