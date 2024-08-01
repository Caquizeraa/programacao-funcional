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
    | c1 < c2   = c1 : merge r1 l2
    | otherwise = c2 : merge l1 r2
merge l1 [] = l1
merge [] l2 = l2

insertionSort :: Ord t => [t] -> [t]
insertionSort l = acumEsq merge [] (transforma (\a -> [a]) l)

tamanhoLista :: Integral t => [b] -> t
tamanhoLista l = acumEsq (\c _ -> c + 1) 0 l 

ehPar :: Integral t => t -> Bool
ehPar n = (n `mod` 2) == 0

buscaK_esimo :: Integer -> [t] -> t
buscaK_esimo k l = buscaK_esimo' 0 k l
    where
        buscaK_esimo' :: Integer -> Integer -> [t] -> t
        buscaK_esimo' i k (c:r)
            | i == k    = c
            | otherwise = buscaK_esimo' (i + 1) k r

obtemSubLista :: Integer -> Integer -> [t] -> [t]
obtemSubLista k_1 k_2 l =
    let 
        (_:r1:_) = dividirLista k_1 l
        (r2:_:_) = dividirLista (k_2 - k_1 + 1) r1
    in r2

dividirLista :: Integral t => t -> [a] -> [[a]]
dividirLista k l = dividirLista' 0 k l
    where
        dividirLista' _ _ [] = [[], []]
        dividirLista' i k l@(c:r)
            | i < k     = concatenarPares ([[c], []]) (dividirLista' (i + 1) k r)
            | otherwise = [[], l]
            where concatenarPares (l1_a:l1_b:_) (l2_a:l2_b:_) = [l1_a ++ l2_a, l1_b ++ l2_b]
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
        geraSequencia' l = acumEsq (++) [] l

-- geraSequencia n = geraSequencia' 1 n
--     where
--         geraSequencia' :: Int -> Int -> [Int]
--         geraSequencia' a b
--             | a < b     = [a, -a] ++ geraSequencia' (a + 1) b
--             | otherwise = [a, -a]

-- Q11

somatorio :: Real t => [t] -> t
somatorio l = acumEsq (+) 0 l

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

mediana :: (Real t, Fractional t) => [t] -> t
mediana l = mediana' (insertionSort l)
    where
        -- mediana' :: Real t => [t] -> t
        mediana' l
            | ehPar (tamanhoLista l) = medianaPar l
            | otherwise              = medianaImpar l
            where
                medianaImpar l = 
                    let k = (tamanhoLista l) `quot` 2
                    in buscaK_esimo k l
                medianaPar l = 
                    let 
                        k = (tamanhoLista l) `quot` 2
                        (n1:n2:_) = obtemSubLista (k - 1) k l
                    in (n1 + n2) / 2

-- Q23

rodarEsquerda :: (Integral t) => t -> [a] -> [a]
rodarEsquerda n l = rodarEsquerda' (n `mod` (tamanhoLista l)) l
    where
        rodarEsquerda' 0 l = l 
        rodarEsquerda' n l =
            let (c1:c2:_) = dividirLista n l
            in c2 ++ c1

rodarDireita :: (Integral t) => t -> [a] -> [a]
rodarDireita n l = rodarEsquerda (tam - (n `mod` (tam))) l
    where tam = tamanhoLista l
-- Q26

media :: (Real t, Fractional t) => [t] -> t
media l = 
    let tam = tamanhoLista l
    in (acumEsq (+) 0.0 l) / fromIntegral tam

-- Q29

seleciona :: [t] -> [Integer] -> [t]
seleciona l1 l2 = acumEsq (\ac a -> ac ++ [(buscaK_esimo a l1)]) [] l2'
    where l2' = transforma (\a -> a - 1) l2
