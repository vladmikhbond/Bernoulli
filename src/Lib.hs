{-----------------------------------------------------------------------------------
 Моделирует тесты, составленные из вопросов с ответами "да" и "нет".
 Моделью является случайная последовательность из 0 и 1, в которой 
 p - частота единиц, n - длина последовательности.

 Печатает таблицы для различных длин последовательностей
 В строке таблицы
 p - предполагаемый уровень знаний
 m - уровень знаний, установленный путем тестирования
 ci - доверительный интервал среднего
 pu- доверительный интервал определенный прямым подсчетом вариантов ()
  
------------------------------------------------------------------------------------}

module Lib
    ( entryPoint
    ) where

import System.Random
import System.Environment
import Text.Printf

-- Длины последовательностей
ns = [10, 20, 30, 40, 50, 100, 200]

entryPoint :: IO ()
entryPoint = do
    mapM_ tableForN ns     -- sequence_ $ map tableForN ns
    
tableForN n = do
    putStrLn $ "n = " ++ show n
    sequence_ [lineForP (x * 0.1) n | x <- [0..10]]

lineForP p n = do
    rs <- randomList p n
    let m = avg rs
    let ci = confidenceIntervalGen p n 
    let k = round (fromIntegral n * p)
    let pu = fromIntegral (puasson n k) / fromIntegral n :: Double
    
    let flag = if m < p - ci || m > p + ci then "(!)" else ""
    putStrLn $ "p = " ++ roundToStr 1 p ++
         "   m = " ++ roundToStr 2 m ++
         "   ci = " ++ roundToStr 2 ci ++
        "    pu = " ++ roundToStr 2 pu ++
         flag 
         
avg ::  [Int] -> Double
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)


-- дисперсмия генеральной совокупности
disperseGen p = ((1 - m)**2) * p + m**2 * (1 - p)   where m = p
-- доверительный интервал для 95%
confidenceIntervalGen p n = let d = disperseGen p 
                         in 1.96 * sqrt d / sqrt (fromIntegral n)  -- k = 1.96

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

r = puasson 50 25

-- ------------------------------------------------

-- Генерит случайный список из 1 и 0. p - частота единиц, n - длина списка
randomList :: Double -> Integer -> IO [Int]
randomList p n = let m = sequence [ randomIO | _ <- [1..n]] 
             in m >>= \xs -> return $ map (\x -> if x <= p then 1 else 0) xs

-- Утилита. Переводит в строку с округлением. 1-й параметр - количество дес.цифр 
roundToStr :: (PrintfArg a, Floating a) => Integer -> a -> String
roundToStr = printf "%0.*f"




