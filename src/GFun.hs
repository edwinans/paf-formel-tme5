module GFun where

import Data.Map.Strict as M (Map, empty, foldrWithKey, fromList, insert, lookup, valid)
import Data.Sequence as S (Seq, empty, foldlWithIndex, fromList, (<|))

data Serie a = Z a (Serie a)

-- instance Encode a => Encode (Seq a) where

instance Show a => Show (Serie a) where
  show = showSerie

instance Num a => Num (Serie a) where
  (+) = sumSerie
  (*) = multSerie
  abs = gmap abs
  signum s = 0
  fromInteger x = let s = Z (fromInteger x) s in s
  negate = gmap negate

sumSerie :: Num a => Serie a -> Serie a -> Serie a
sumSerie (Z x xs) (Z y ys) = Z (x + y) (sumSerie xs ys)

multSerie :: Num a => Serie a -> Serie a -> Serie a
multSerie (Z c1 s1) (Z c2 s2) =
  Z (c1 * c2) (gmap (* c1) s2 + gmap (* c2) s1 + Z 0 (s1 * s2))

showSerie :: Show a => Serie a -> String
showSerie s =
  let coefs = S.fromList $ prend 10 s
   in (S.foldlWithIndex aux "" coefs) ++ "..."
  where
    aux acc idx c = acc ++ show c ++ ".z^" ++ show idx ++ " + "

-- tail-recursive
prend :: Integer -> Serie a -> [a]
prend n s = aux n s [] where

aux n (Z x xs) acc
  | n == 0 = reverse $ acc
  | otherwise = aux (n - 1) xs (x : acc)

gmap :: (a -> b) -> Serie a -> Serie b
gmap f (Z x xs) = Z (f x) (gmap f xs)

uns :: Serie Integer
uns = Z 1 uns

succs :: Serie Integer
succs = Z 0 $ Z 1 $ Z 2 $ succs

-- >>> uns
-- 1.z^0 + 1.z^1 + 1.z^2 + 1.z^3 + 1.z^4 + 1.z^5 + 1.z^6 + 1.z^7 + 1.z^8 + 1.z^9 + ...

-- >>> succs
-- 0.z^0 + 1.z^1 + 2.z^2 + 0.z^3 + 1.z^4 + 2.z^5 + 0.z^6 + 1.z^7 + 2.z^8 + 0.z^9 + ...

-- >>> uns + succs
-- 1.z^0 + 2.z^1 + 3.z^2 + 1.z^3 + 2.z^4 + 3.z^5 + 1.z^6 + 2.z^7 + 3.z^8 + 1.z^9 + ...

-- >>> uns * succs
-- 0.z^0 + 1.z^1 + 3.z^2 + 3.z^3 + 4.z^4 + 6.z^5 + 6.z^6 + 7.z^7 + 9.z^8 + 9.z^9 + ...

-- >>> fromInteger 5 :: Serie Int
-- 5.z^0 + 5.z^1 + 5.z^2 + 5.z^3 + 5.z^4 + 5.z^5 + 5.z^6 + 5.z^7 + 5.z^8 + 5.z^9 + ...


-- >>> gmap (^2) succs
-- 0.z^0 + 1.z^1 + 4.z^2 + 0.z^3 + 1.z^4 + 4.z^5 + 0.z^6 + 1.z^7 + 4.z^8 + 0.z^9 + ...

-- >>> prend 2 uns
-- [1,1]

-- >>> prend 10 succs
-- [0,1,2,0,1,2,0,1,2,0]
