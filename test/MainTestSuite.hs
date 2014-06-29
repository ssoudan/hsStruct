{-
 MainTestSuite.hs

 Copyright 2014 Sebastien Soudan

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
module Main (
    main
 ) where

import           AVLTreeTest
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import Test.Framework.Options

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [
          testGroup "simple" 
            [ testProperty "rotations" prop_rotations
            , testProperty "insert - Integer" prop_insert_integer
            , testProperty "insert - Float" prop_insert_float
            ]
        , testGroup "complex" 
            [ testProperty "Height" prop_height
            , testProperty "Balance factor" prop_bf
            , testProperty "Sort" prop_sort
            ]
        ]