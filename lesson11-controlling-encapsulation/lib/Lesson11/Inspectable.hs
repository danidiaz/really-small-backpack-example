module Lesson11.Inspectable where

import Lesson11.Mystery

class Inspectable x where
    inspect :: x -> Mystery String
