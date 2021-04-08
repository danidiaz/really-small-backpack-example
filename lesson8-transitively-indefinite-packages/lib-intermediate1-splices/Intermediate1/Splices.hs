module Intermediate1.Splices where

import Language.Haskell.TH

makeIdFunc :: Name -> Q [Dec]
makeIdFunc typeName = pure $ [
    SigD (mkName "myIdFunc") (ArrowT `AppT` ConT typeName `AppT` ConT typeName),
    ValD (VarP (mkName "myIdFunc")) (NormalB ([(VarP (mkName "x"))] `LamE` (VarE (mkName "x")))) []
    ]

