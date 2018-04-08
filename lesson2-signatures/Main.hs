module Main where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Lesson2.String
import qualified Lesson2.Text

template1 :: Lesson2.String.Template
template1 = Lesson2.String.compile "aa%bb%cc"

template2 :: Lesson2.Text.Template
template2 = Lesson2.Text.compile (T.pack "aa%bb%cc")

main :: IO ()
main = do
    putStrLn $ Lesson2.String.format template1 ["xx","yy"]
    putStrLn $ Lesson2.Text.format template2 $ map T.pack ["xx","yy"]
