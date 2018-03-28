{-# LANGUAGE ScopedTypeVariables #-}
{-
 RDGTest.hs

 Copyright 2018 Sebastien Soudan

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

-}
module RDGTest where

import           RDG
import           Graph
import qualified Data.Foldable        as F
import           Data.Monoid
import           Test.QuickCheck.Test

prop_empty :: Bool
prop_empty = let (g :: RDG String String) = buildGraph [] []
             in isEmptyRDG g

prop_cc :: Bool
prop_cc = let g = buildGraph ["A", "B", "C"] [("A", "B", "AB"), ("B", "C", "BC")]
              cc = connectedComp g
              v = Prelude.map vertexData $ vertices cc
           in v == [1, 1, 1]

prop_cc2 :: Bool
prop_cc2 = let g = buildGraph ["A", "B", "C"] [("A", "B", "AB")]
               cc = connectedComp g
               v = Prelude.map vertexData $ vertices cc
            in v == [1, 1, 3]