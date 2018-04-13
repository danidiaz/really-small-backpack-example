module Bar where

import Siggy

printBarVal :: IO ()
printBarVal = do
    print someVal 
    print someOtherVal
