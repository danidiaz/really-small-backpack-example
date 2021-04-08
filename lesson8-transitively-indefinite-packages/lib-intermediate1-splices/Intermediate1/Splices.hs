module Intermediate1.Splices where

import Language.Haskell.TH

makeIdFunc :: Q [Dec]
makeIdFunc = pure $ [
    SigD (mkName "myIdFunc") (ArrowT `AppT` ConT (mkName "Int") `AppT` ConT (mkName "Int")),
    ValD (VarP (mkName "myIdFunc")) (NormalB ([(VarP (mkName "x"))] `LamE` (VarE (mkName "x")))) []
    ]

