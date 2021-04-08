module Core (fooAsString, makeIdFunc) where

import Core.SomeSig (foo)

import Language.Haskell.TH

makeIdFunc :: Q [Dec]
makeIdFunc = pure $ [
    SigD (mkName "myIdFunc") (ArrowT `AppT` ConT (mkName "Int") `AppT` ConT (mkName "Int")),
    ValD (VarP (mkName "myIdFunc")) (NormalB ([(VarP (mkName "x"))] `LamE` (VarE (mkName "x")))) []
    ]

-- uses the abstract value from the signature and converts it to String
fooAsString :: String
fooAsString = show foo

