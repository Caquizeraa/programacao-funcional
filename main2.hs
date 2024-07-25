-- UTEIS

transforma :: (a -> b) -> [a] -> [b]
transforma func (c:r) = func(c) : transforma func r
transforma _ [] = []

acumEsq :: (b -> a -> b) -> b -> [a] -> b
acumEsq func ac l@(c:r) = acumEsq func (func ac c) r
acumEsq func ac []      = ac

acumDir :: (a -> b -> b) -> b -> [a] -> b
acumDir func ac l@(c:r) = func c (acumDir func ac r)
acumDir func ac []      = ac

merge :: Ord t => [t] -> [t] -> [t]
merge l1@(c1:r1) l2@(c2:r2)
    | c1 < c2   = [c1] ++ merge r1 l2
    | otherwise = [c2] ++ merge l1 r2
merge l1 [] = l1
merge [] l2 = l2

insertionSort :: Ord t => [t] -> [t]
insertionSort l = acumEsq merge [] (transforma (\a -> [a]) l)

tamanhoLista :: [b] -> Int
tamanhoLista l = acumEsq (\c _ -> c + 1) 0 l 

ehPar :: Int -> Bool
ehPar n = (n `mod` 2) == 0

-- Q2

maioresQue :: Real t => t -> [t] -> [t]
maioresQue n (c:r)
    | c > n     = c : maioresQue n r
    | otherwise = maioresQue n r
maioresQue _ [] = []

-- Q5

removerUltimo :: [t] -> [t]
removerUltimo [] = []
removerUltimo l@(c:r)
    | ehUltimo l = removerUltimo r
    | otherwise  = c : removerUltimo r
    where
        ehUltimo :: [a] -> Bool 
        ehUltimo [_] = True
        ehUltimo _ = False

-- Q8

geraSequencia :: Int -> [Int]
geraSequencia n = geraSequencia' [ [x, -x] | x <- [1..n]]
    where
        geraSequencia' :: [[Int]] -> [Int]
        geraSequencia' l = acumEsq (\l1 l2 -> l1 ++ l2) [] l

-- geraSequencia n = geraSequencia' 1 n
--     where
--         geraSequencia' :: Int -> Int -> [Int]
--         geraSequencia' a b
--             | a < b     = [a, -a] ++ geraSequencia' (a + 1) b
--             | otherwise = [a, -a]

-- Q11

somatorio :: Real t => [t] -> t
somatorio l = acumEsq (\x y -> x + y) 0 l

-- Q14

interseccao :: Eq t => [t] -> [t] -> [t]
interseccao [] _ = []
interseccao l1@(c1:r1) l2 = interseccao' c1 l2 ++ interseccao r1 l2
    where
        interseccao' :: Eq t => t -> [t] -> [t]
        interseccao' _ [] = []
        interseccao' n (c:r)
            | n == c    = [n]
            | otherwise = interseccao' n r

-- Q20

mediana :: Real t=> [t] -> t
mediana l = mediana' (insertionSort l)
    where
        mediana' :: Real t => [t] -> t
        mediana' l
            | ehPar (tamanhoLista l) = 0
            | otherwise              = 0