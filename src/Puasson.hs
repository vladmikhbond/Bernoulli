module Puasson
    ( entry
    ) where

import Text.Printf
-- длины последовательностей
ns = [10, 20, 30, 40, 50, 100, 150, 200]

entry :: IO ()
entry = do        
    putStrLn " n    00%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%"
    mapM_ raw ns     -- sequence_ $ map raw ns

-- строка таблицы    
raw n = do
    let space = if n < 100 then " " else ""
    putStr $ space ++ show n ++ " "
    sequence_ [cell n (n * k `div` 10) | k <- [0..10]]
    putStrLn ""

-- ячейка таблицы
cell n k = do
    let (a, b) = confInterval n k
    printf "%4d " b 
        
-- число комбинаций из n по k
comb :: Integer -> Integer -> Integer
comb n k =  product [n-k+1..n] `div` product [1..k]

-- формула Бернулли
prob ::  Double -> Integer -> Integer -> Double
prob p n k = fromInteger(comb n k) * p ** fromInteger k * (1 - p) ** fromInteger (n - k) 

-- доверительный интервал прямым подсчетом 
confInterval n k = f n k (prob p n k) 0 0 
  where 
  p = fromInteger k / fromInteger n    
  f n k a dl dr 
    | a >= 0.95 = (dl, dr) 
    | otherwise = let (sl, dl') = if k - dl >= 0 then (prob p n (k - dl), dl - 1) else (0, dl)
                      (sr, dr') = if k + dr <= n then (prob p n (k + dr), dr + 1) else (0, dr)
                  in f n k (a + sl + sr) dl' dr'

