{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE TypeFamilies #-} 
module Lesson12.User (
  User (..)   
 ) where 

import Lesson12.Mystery
import Data.Text

data User = User {
    name :: Mystery Text,
    age :: Mystery Int
 }