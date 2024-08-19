
{-Laboratorio 1-}

esCero::Int -> Bool
esCero n= n==0

esPositivo :: Int -> Bool 
esPositivo n = n>=0

esVocal :: Char -> Bool 
esVocal n | n == 'a' ||n ==  'e' ||n == 'i' ||n == 'o' ||n == 'u' = True
          | otherwise = False

valorAbsoluto :: Int -> Int 
valorAbsoluto n | n>= 0 = n
                | otherwise = n * (-1)

{-Laboratorio 2-}

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * (n-1)

promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)

{-Laboratorio 3-}

--a) identifica las variables libres de las expreciones 4a, 4b y 4d
--b) defini las funciones, que tomen como argumento las variables libres
--c) evaluá las funciones tomando como argumento los valores señalados en 5

funcion4a :: [Int] -> Bool
funcion4a [] = True
funcion4a (x:xs)|x<0=False
                |otherwise = funcion4a xs
-- funcion4a (x:xs) = (x>0) && funcion4a xs
{-ghci> funcion4a  [11,2,5,8] 
True  
ghci> funcion4a [-5,-3,4,8]  
False-}
funcion4b :: (Eq a)=>a->[a]->Bool
funcion4b n [] = False
funcion4b n (x:xs) | x == n = True
                   | otherwise =  funcion4b n xs  
{-ghci> funcion4b 5 [-5,-3,4,8]
False 
ghci> funcion4b 5 [11,2,5,8] 
True-}
funcion4d :: (Eq a)=> [a]-> Bool
funcion4d [] = True
funcion4d [x] = True
funcion4d (x:y:xs) | x/=y = False
                   | otherwise = funcion4d (x:xs)
{-ghci> funcion4d [-5,-3,4,8]
False
ghci> funcion4d  [11,2,5,8]
False-}

{-Laboratorio 4-}
