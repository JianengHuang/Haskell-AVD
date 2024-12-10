import Test.QuickCheck

prop_RevUnit :: Int -> Bool
prop_RevUnit x = reverse [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y

prop_MaxLe2 :: Int -> Int -> Bool
prop_MaxLe2 x y = max x y == y

miInsert :: Int -> [Int] -> [Int]
miInsert x [] = [x]
miInsert x (y:ys) | x < y = x : y : ys
                  | otherwise = y : miInsert x ys

miSorted :: [Int] -> Bool
miSorted [] = True
miSorted [x] = True
miSorted(x:y:ys) = x <= y && miSorted (y:ys)

prop_Sorted :: Int -> [Int] -> Property
prop_Sorted x ys = miSorted ys ==> miSorted (miInsert x ys)

prop_DoubleCycle :: [Int] -> Property 
prop_DoubleCycle xs = not (null xs) ==> cycle xs == cycle (xs ++ xs)

prop_DoubleCycle2 :: [Int] -> Int -> Property
prop_DoubleCycle2 xs n = not (null xs) ==> take n (cycle xs) == take n (cycle (xs ++ xs))

prop_Sorted2 :: Int -> [Int] -> Property
prop_Sorted2 x ys = miSorted ys ==> classify (null ys) "trivial" $ miSorted (miInsert x ys)

prop_Sorted3 :: Int -> [Int] -> Property
prop_Sorted3 x ys = miSorted ys ==> collect (length ys) $ miSorted (miInsert x ys)
