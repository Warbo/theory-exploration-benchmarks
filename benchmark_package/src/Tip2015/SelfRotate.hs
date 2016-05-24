-- | Another simple property about rotate
module Tip2015.SelfRotate where

import Tip.Prelude
import Tip2015.Rotate
import Prelude ()

prop_self :: Nat -> [a] -> Equality [a]
prop_self n xs = rotate n (xs ++ xs) === rotate n xs ++ rotate n xs
