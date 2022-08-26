import Data.Char

length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' [] = 0
sum' (x : xs) = x + sum' xs

product' [] = 1
product' (x : xs) = x * product' xs

take' _ [] = []
take' 0 xs = []
take' n (x : xs) = x : take' (n - 1) xs

drop' _ [] = []
drop' 0 xs = xs
drop' n (x : xs) = drop' (n - 1) xs

reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

addsub x y = (x + y, x - y)

perPoint (p, q) (a, b, c) = (x, y)
  where
    d = b * p - a * q
    x = (b * a + a * c) / (a * a + b * b)
    y = (b * c - a * d) / (a * a + b * b)

rot13 c = chr (ord 'a' + ((ord c + 13 - ord 'a') `mod` 26))

insert x [] = [x]
insert x (y : ys)
  | x < y = x : y : ys
  | otherwise = y : insert x ys

isort [] = []
isort (x : xs) = insert x (isort xs)

bswap [] = []
bswap [x] = [x]
bswap (x : xs)
  | x < y = x : y : ys
  | otherwise = y : x : ys
  where
    (y : ys) = bswap xs

bsort [] = []
bsort xs =
  y : bsort ys
  where
     (y : ys) = bswap xs

merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort [] = []
msort [x] = [x]
msort xs =
  merge (msort (take h xs)) (msort (drop h xs))
  where
    h = length xs `div` 2

qsort [] = []
qsort (x : xs) = qsort ls ++ [x] ++ qsort rs
  where
    ls = [a | a <- xs, a < x]
    rs = [a | a <- xs, a >= x]

fact 0 = 1
fact n = n * fact (n - 1)

main = do
  print $ ord 'A'
  print $ chr 65
  print $ rot13 'a'
  print $ isort [1, 3, 2, 4, 0, 8, -2]
  print $ bsort [1, 3, 2, 4, 0, 8, -2]
  print $ msort [1, 3, 2, 4, 0, 8, -2]
  print $ qsort [1, 3, 2, 4, 0, 8, -2]
  print $ [fact x | x <- [0 .. 5], x > 2]
  print $ 2 ^^ 4
