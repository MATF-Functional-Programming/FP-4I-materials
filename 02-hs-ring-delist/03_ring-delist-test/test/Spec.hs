import Lib

import Data.Function (on)

-- import bib za testiranje, dodati - QuickCheck u deps
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"


-- Da biste ucitali testove u GHCi, pokrenite ghci na sledeci nacin:
-- stack ghci ring-delist:ring-delist-test

-- Testable je bilo sta sto moze da se testira

-- Pojedinacne testove mozete da pokrecete sa:
-- quickCheck ime_testa
-- quickCheck (withMaxSuccess 10000 ime_testa)
-- verboseCheck ime_testa
-- itd.

-- Postoje tipovi podataka za npr. rezultat testova:
-- quickCheckResult ime_testa

-- Args mozemo proslediti funkcijama koje se zavrsavaju sufiksom `With`
-- quickCheckWith
-- verboseCheckWith
-- verboseCheckWithResult
-- itd.

-- Arbitrary je klasa koju mozemo da instanciramo za nase tipove
-- Na taj nacin definisemo kako mogu nasi tipovi da se generisu
-- Arbitrary:
--    arbitrary - definisemo generator
--    shrink    - ako QuickCheck nadje vrednost za koju test ne prolazi,
--                pokusace da nadje "manje" vrednosti koristeci ovu 
--                funkciju kako bi test primer bio sto minimalniji


-- prop_conversions :: [Int] -> Bool
prop_conversions s =
            s == (toList $ fromList s)
            where types = (s :: [Int])

prop_focusNext_focusPrev s =
            s == (toList $ focusPrev $ focusNext $ fromList s)
            where types = (s :: [Int])

prop_focusPrev_focusNext s =
            s == (toList $ focusNext $ focusPrev $ fromList s)
            where types = (s :: [Int])


