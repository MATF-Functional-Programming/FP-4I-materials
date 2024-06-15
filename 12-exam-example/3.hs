module Zad3 where

-- Zadatak 3:

-- Definisati strukturu podataka `Stek a`, gde je `a`
-- proizvoljan tip. Stek moze biti prazan ili sadrzati
-- proizvoljan broj elemenata tipa `a`.

-- Kreirati funkcije `fromList` i `toList` koje konvertuju
-- [a] u `Stek a`, i `Stek a` u `[a]`, redom.

-- Napisati QuickCheck test za funkcije `fromList` i `toList`.

-- Instancirati Show nad `Stek a` (pretpostaviti da je
-- `a` u klasi Show) tako da se stek ispise u formatu:
--       < dno, ... , vrh >
-- (vrh i dno su elementi steka)
-- Za prazan stek:  </>

-- Instancirati Functor nad `Stek a`

data Stacc a = Stacc [a]

data Stacc' a = Empty
              | Stacc' a (Stacc' a)

fromList :: [a] -> Stacc a
fromList = Stacc
-- fromList []     = Empty
-- fromList (x:xs) = Stacc' x (fromList xs) 
-- fromList = foldr Stacc' Empty 

toList :: Stacc a -> [a]
toList (Stacc xs) = xs
-- toList Empty = []
-- toList (Stacc' x xs) = x : (toList xs)

test_fromList_toList :: (Eq a) => [a] -> Bool
test_fromList_toList lst = lst == (toList $ fromList lst)

instance (Show a) => Show (Stacc a) where
    show s = "<" ++ show' s ++ ">"
        where 
            show' :: Stacc a -> String
            show' (Stacc [])     = "/"
            show' (Stacc [x])    = show x 
            show' (Stacc (x:xs)) = show x ++ ", " ++ show' (Stacc xs) 
            
            -- show' (Stacc []) = "/"
            -- show' (Stacc xs) = filter (\c -> c `elem` ['[', ']']) $ show xs


instance Functor Stacc where
    fmap f (Stacc xs) = fromList $ fmap f xs 

    -- fmap _ Empty        = Empty
    -- fmap f (Stacc' x xs) = Stacc (f x) (fmap f xs)

-- Set
-- da li tip Set moze da bude Functor?
