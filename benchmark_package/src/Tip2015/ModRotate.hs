-- Property about rotate and mod
module Tip2015.ModRotate where

import Tip.Prelude
import Tip2015.Rotate
import Tip2015.Mod
import qualified Prelude as P

prop_mod :: Nat -> [a] -> Equality [a]
prop_mod n xs = rotate n xs === drop (n `mod` length xs) xs ++ take (n `mod` length xs) xs

prop_structural_mod :: Nat -> [a] -> Equality [a]
prop_structural_mod n xs = rotate n xs === drop (n `mod_structural` length xs) xs ++ take (n `mod_structural` length xs) xs
