module Bar where

import Bar.Siggy (barRequiresThis)

bar :: IO ()
bar = print barRequiresThis
