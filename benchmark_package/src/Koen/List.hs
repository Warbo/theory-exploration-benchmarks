{-# LANGUAGE ScopedTypeVariables #-}
module Koen.List where

import Prelude ()
import Tip.Prelude

--------------------------------------------------------------------------------

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

-- pairs :: [a] -> [(a,a)]
pairs (x:y:xs) = (x,y) : pairs xs
pairs _        = []

-- unpair :: [(a,a)] -> [a]
unpair []          = []
unpair ((x,y):xys) = x : y : unpair xys

{-# NOINLINE evens #-}
evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens []     = []

{-# NOINLINE odds #-}
odds :: [a] -> [a]
odds (x:xs) = evens xs
odds []     = []

-- interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

--------------------------------------------------------------------------------

prop_Select xs =
  map fst (select xs) === xs

prop_SelectPermutations xs =
  all (`zisPermutation` xs) [ y:ys | (y,ys) <- select xs ] === True

prop_SelectPermutations' xs z =
  all ((n ==) . zcount z) [ y:ys | (y,ys) <- select xs ] === True
 where
  n = zcount z xs

prop_PairUnpair xs =
  even (length xs) ==>
    unpair (pairs xs) === xs

prop_PairEvens xs =
  even (length xs) ==>
    map fst (pairs xs) === evens xs

prop_PairOdds xs =
--  even (length xs) ==>
    map snd (pairs xs) === odds xs

prop_Interleave xs =
  interleave (evens xs) (odds xs) === xs

-- Injectivity of append
prop_append_inj_1 xs ys zs = xs ++ zs === ys ++ zs ==> xs === ys
prop_append_inj_2 xs ys zs = xs ++ ys === xs ++ zs ==> ys === zs

prop_nub_nub xs = nub (nub xs) === nub xs

prop_elem_nub_l x xs = x `elem` xs ==> x `elem` nub xs
prop_elem_nub_r x xs = x `elem` nub xs ==> x `elem` xs
prop_count_nub  x xs = x `elem` xs ==> count x (nub xs) === S Z

prop_perm_trans xs ys zs = xs `isPermutation` ys ==> ys `isPermutation` zs ==> xs `isPermutation` zs
prop_perm_refl xs        = xs `isPermutation` xs === True
prop_perm_symm xs ys     = xs `isPermutation` ys ==> ys `isPermutation` xs

prop_perm_elem x xs ys   = x `elem` xs ==> xs `isPermutation` ys ==> x `elem` ys

prop_deleteAll_count x xs = deleteAll x xs === delete x xs ==> count x xs <= S Z

-- prop_elem x xs = x `elem` xs ==> exists (\ i -> maybe False (x ==) (index xs i))
-- prop_elem_map y f xs = y `elem` map f xs ==> exists (\ x -> f x === y .&. y `elem` xs)

-- Same as above, but with ints

prop_z_nub_nub xs = znub (znub xs) === znub xs

prop_z_elem_nub_l x xs = x `zelem` xs ==> x `zelem` znub xs
prop_z_elem_nub_r x xs = x `zelem` znub xs ==> x `zelem` xs
prop_z_count_nub  x xs = x `zelem` xs ==> zcount x (znub xs) === S Z

prop_z_perm_trans xs ys zs = xs `zisPermutation` ys ==> ys `zisPermutation` zs ==> xs `zisPermutation` zs
prop_z_perm_refl xs        = xs `zisPermutation` xs === True
prop_z_perm_symm xs ys     = xs `zisPermutation` ys ==> ys `zisPermutation` xs

prop_z_perm_elem x xs ys   = x `zelem` xs ==> xs `zisPermutation` ys ==> x `zelem` ys

prop_z_deleteAll_count x xs = zdeleteAll x xs === zdelete x xs ==> zcount x xs <= S Z

-- prop_z_elem x xs = x `zelem` xs ==> exists (\ i -> maybe False (x `zeq`) (index xs i))
-- prop_z_elem_map y f xs = y `zelem` map f xs ==> exists (\ x -> f x === y .&. y `zelem` xs)

