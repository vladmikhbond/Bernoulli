module Puasson
    ( entry
    ) where

import System.Environment
import Text.Printf

-- Длины последовательностей
ns = [10, 20, 30, 40, 50, 100, 200]

entry :: IO ()
entry = do        
    putStrLn " n    00%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%"
    mapM_ lineForN ns     -- sequence_ $ map lineForN ns
    
lineForN n = do
    let space = if n < 100 then " " else ""
    putStr $ space ++ show n ++ " "
    sequence_ [elemForP (n * k `div` 10) n | k <- [0..10]]
    putStrLn ""

elemForP k n = do
    let pu = fromIntegral (puasson n k) / fromIntegral n :: Double
    putStr $ " " ++ roundToStr 2 pu
        
-- -----------------------------------------------------------
-- число комбинаций из n по k
comb :: Integer -> Integer -> Integer
comb n k =  product [n-k+1..n] `div` product [1..k]
-- вероятность получения k успехов из n попыток (формула Бернулли)
prob ::  Double -> Integer -> Integer -> Double
prob p n k = fromInteger(comb n k) * p ** fromInteger k * (1 - p) ** fromInteger (n - k) 

-- доверительный интервал прямым подсчетом комбинаций n - длина серии,  k - число успехов 
puasson n k = f n k (prob p n k) 1 
  where 
  p = fromInteger k / fromInteger n    
  f n k a d | a >= 0.95 = d 
            | otherwise = f n k (a + prob p n (k + d) + prob p n (k - d) ) (d + 1)

-- ------------------------------------------------

-- Утилита. Переводит в строку с округлением. 1-й параметр - количество дес.цифр 
roundToStr :: (PrintfArg a, Floating a) => Integer -> a -> String
roundToStr = printf "%0.*f"




