import Control.Monad
import Test.QuickCheck

prop_Inc :: Property
prop_Inc = forAll (choose (0, 100) :: Gen Int) $ \x -> x + 1 > 0

data Color = Red | Green | Blue deriving (Show, Eq)

instance Arbitrary Color where
    arbitrary = oneof [return Red, return Green, return Blue]

cooler :: Color -> Color -> Color
cooler x y
  | Blue `elem` [x, y] = Blue
  | Green `elem` [x, y] = Green
  | otherwise = Red

prop_Cooler :: Color -> Color -> Bool 
prop_Cooler x y = cooler x y == cooler y x



suma :: Int -> Int -> Int
suma x y = x + y

prop_SumaPos :: Property
prop_SumaPos = forAll (choose (0, 100) :: Gen Int) $ \x ->
    forAll (choose (0, 100) :: Gen Int) $ \y ->
        x + y >= 0


prop_SumaNeg :: Property
prop_SumaNeg = forAll (choose (-1, -100) :: Gen Int) $ \x ->
    forAll (choose (-1, -100) :: Gen Int) $ \y ->
        x + y < 0


data Tree = Leaf Int | Branch Tree Tree deriving (Show, Eq)

instance Arbitrary Tree where
    arbitrary = sized tree

tree 0 = liftM Leaf arbitrary
tree n | n > 0 =
        frequency [(1, liftM Leaf arbitrary),
                   (4, liftM2 Branch (tree (n `div` 2)) (tree (n `div` 2)))]

data Lista = Nil | Cons Int Lista deriving (Show, Eq)                   

instance Arbitrary Lista where
    arbitrary = sized lista

lista 0 = return Nil
lista n | n > 0 = do
    x <- arbitrary
    xs <- lista (n `div` 2)
    return (Cons x xs)

con Nil ys = ys
con (Cons x xs) ys = Cons x (con xs ys)

rev Nil = Nil
rev (Cons x xs) = con (rev xs) (Cons x Nil)

prop_RevUnit :: Int -> Bool
prop_RevUnit x = rev (Cons x Nil) == Cons x Nil

concat_Lista :: Lista -> Lista -> Lista
concat_Lista Nil ys = ys
concat_Lista (Cons x xs) ys = Cons x (concat_Lista xs ys)

prop_RevApp :: Lista -> Lista -> Bool
prop_RevApp xs ys = rev (concat_Lista xs ys) == concat_Lista (rev ys) (rev xs)

prop_RevRev :: Lista -> Bool
prop_RevRev xs = rev (rev xs) == xs