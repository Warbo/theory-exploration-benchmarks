-- Property from "Case-Analysis for Rippling and Inductive Proof",
-- Moa Johansson, Lucas Dixon and Alan Bundy, ITP 2010

{-# LANGUAGE TypeOperators #-}
module Isaplanner.Properties where

import Prelude (Bool(..),error,toEnum,fromEnum,pred,succ,sqrt,round
               ,Enum,Eq,Ord,Show,return,(.),undefined)
import Tip
import Data.Typeable
import Isaplanner.Definitions

prop_01 n xs
  = (take n xs ++ drop n xs === xs)

prop_02 n xs ys
  = (count n xs + count n ys === count n (xs ++ ys))

prop_03 n xs ys
  = bool (count n xs <= count n (xs ++ ys))

prop_04 n xs
  = (S (count n xs) === count n (n : xs))

prop_05 n x xs
  = n === x ==> S (count n xs) === count n (x : xs)

prop_06 n m
  = (n - (n + m) === Z)

prop_07 n m
  = ((n + m) - n === m)

prop_08 k m n
  = ((k + m) - (k + n) === m - n)

prop_09 i j k
  = ((i - j) - k === i - (j + k))

prop_10 m
  = (m - m === Z)

prop_11 xs
  = (drop Z xs === xs)

prop_12 n f xs
  = (drop n (map f xs) === map f (drop n xs))

prop_13 n x xs
  = (drop (S n) (x : xs) === drop n xs)

prop_14 p xs ys
  = (filter p (xs ++ ys) === (filter p xs) ++ (filter p ys))

prop_15 x xs
  = (len (ins x xs) === S (len xs))

prop_16 x xs
  = xs === [] ==> last (x:xs) === x

prop_17 n
  = (n <= Z === n == Z)

prop_18 i m
  = bool (i < S (i + m))

prop_19 n xs
  = (len (drop n xs) === len xs - n)

-- This property is the same as prod #48
prop_20 xs
  = (len (sort xs) === len xs)

prop_21 n m
  = bool (n <= (n + m))

prop_22 a b c
  = (max (max a b) c === max a (max b c))

prop_23 a b
  = (max a b === max b a)

prop_24 a b
  = ((max a b) == a === b <= a)

prop_25 a b
  = ((max a b) == b === a <= b)

prop_26 x xs ys
  = x `elem` xs ==> x `elem` (xs ++ ys)

prop_27 x xs ys
  = x `elem` ys ==> x `elem` (xs ++ ys)

prop_28 x xs
  = bool (x `elem` (xs ++ [x]))

prop_29 x xs
  = bool (x `elem` ins1 x xs)

prop_30 x xs
  = bool (x `elem` ins x xs)

prop_31 a b c
  = (min (min a b) c === min a (min b c))

prop_32 a b
  = (min a b === min b a)

prop_33 a b
  = (min a b == a === a <= b)

prop_34 a b
  = (min a b == b === b <= a)

prop_35 xs
  = (dropWhile (\ _ -> False) xs === xs)

prop_36 xs
  = (takeWhile (\ _ -> True) xs === xs)

prop_37 x xs
  = bool (not (x `elem` delete x xs))

prop_38 n xs
  = (count n (xs ++ [n]) === S (count n xs))

prop_39 n x xs
  = (count n [x] + count n xs === count n (x:xs))

prop_40 xs
  = (take Z xs === [])

prop_41 n f xs
  = (take n (map f xs) === map f (take n xs))

prop_42 n x xs
  = (take (S n) (x:xs) === x : (take n xs))

prop_43 p xs
  = (takeWhile p xs ++ dropWhile p xs === xs)

prop_44 x xs ys
  = (zip (x:xs) ys === zipConcat x xs ys)

prop_45 x y xs ys
  = (zip (x:xs) (y:ys) === (x, y) : zip xs ys)

prop_46 xs
  = (zip [] xs === [])

prop_47 a
  = (height (mirror a) === height a)

prop_48 xs
  = not (null xs) ==> butlast xs ++ [last xs] === xs

prop_49 xs ys
  = (butlast (xs ++ ys) === butlastConcat xs ys)

prop_50 xs
  = (butlast xs === take (len xs - S Z) xs)

prop_51 xs x
  = (butlast (xs ++ [x]) === xs)

prop_52 n xs
  = (count n xs === count n (rev xs))

-- This property is the same as prod #50
prop_53 n xs
  = (count n xs === count n (sort xs))

prop_54 n m
  = ((m + n) - n === m)

prop_55 n xs ys
  = (drop n (xs ++ ys) === drop n xs ++ drop (n - len xs) ys)

prop_56 n m xs
  = (drop n (drop m xs) === drop (n + m) xs)

prop_57 n m xs
  = (drop n (take m xs) === take (m - n) (drop n xs))

prop_58 n xs ys
  = (drop n (zip xs ys) === zip (drop n xs) (drop n ys))

prop_59 xs ys
  = ys === [] ==> last (xs ++ ys) === last xs

prop_60 xs ys
  = not (null ys) ==> last (xs ++ ys) === last ys

prop_61 xs ys
  = (last (xs ++ ys) === lastOfTwo xs ys)

prop_62 xs x
  = not (null xs) ==> last (x:xs) === last xs

prop_63 n xs
  = n < len xs ==> last (drop n xs) === last xs

prop_64 x xs
  = (last (xs ++ [x]) === x)

prop_65 i m =
  bool (i < S (m + i))

prop_66 p xs
  = bool (len (filter p xs) <= len xs)

prop_67 xs
  = (len (butlast xs) === len xs - S Z)

prop_68 n xs
  = bool (len (delete n xs) <= len xs)

prop_69 n m
  = bool (n <= (m + n))

prop_70 m n
  = m <= n ==> bool (m <= S n)

prop_71 x y xs
  = (x == y) === False ==> elem x (ins y xs) === elem x xs

prop_72 i xs
  = (rev (drop i xs) === take (len xs - i) (rev xs))

prop_73 p xs
  = (rev (filter p xs) === filter p (rev xs))

prop_74 i xs
  = (rev (take i xs) === drop (len xs - i) (rev xs))

prop_75 n m xs
  = (count n xs + count n [m] === count n (m : xs))

prop_76 n m xs
  = (n == m) === False ==> count n (xs ++ [m]) === count n xs

prop_77 x xs
  = sorted xs ==> sorted (insort x xs)

-- This property is the same as prod #14
prop_78 xs
  = bool (sorted (sort xs))

prop_79 m n k
  = ((S m - n) - S k === (m - n) - k)

prop_80 n xs ys
  = (take n (xs ++ ys) === take n xs ++ take (n - len xs) ys)

prop_81 n m xs {- ys -}
  = (take n (drop m xs) === drop m (take (n + m) xs))

prop_82 n xs ys
  = (take n (zip xs ys) === zip (take n xs) (take n ys))

prop_83 xs ys zs
  = (zip (xs ++ ys) zs ===
           zip xs (take (len xs) zs) ++ zip ys (drop (len xs) zs))

prop_84 xs ys zs
  = (zip xs (ys ++ zs) ===
           zip (take (len ys) xs) ys ++ zip (drop (len ys) xs) zs)

-- One way to prove this is to first show "Nick's lemma":
-- len xs = len ys ==> zip xs ys ++ zip as bs = zip (xs ++ as) (ys ++ bs)
prop_85 xs ys
  = (len xs === len ys) ==>
    (zip (rev xs) (rev ys) === rev (zip xs ys))

prop_86 x y xs
  = x < y ==> (elem x (ins y xs) === elem x xs)
