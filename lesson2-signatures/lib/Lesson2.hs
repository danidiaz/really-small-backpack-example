module Lesson2 (compile,Template,format) where

-- We import a module signature, a "hole" in the component that will be filled
-- later.
import Str (Str, splitOn)

newtype Template = Template [Str] deriving (Show)

compile :: Str -> Template
compile = Template . splitOn '%'

format :: Template -> [Str] -> Str
format (Template strs) vals = 
    mconcat $ zipWith mappend strs (vals ++ repeat mempty)
