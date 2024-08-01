-- Trabalho Prático de PLP - Programação Funcional
-- Autores: Gabriel Coelho Costa (202310172)
-- Isac Gonçalves Cunha (202310777)

-- Maior Número de Matrícula mod 3 = Número do grupo
-- 202310777 mod 3                 = 2
-- Questões 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 33, 35

-- Funções Úteis
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

tamanhoLista :: Integral t => [b] -> t
tamanhoLista l = acumEsq (\c _ -> c + 1) 0 l 

ehPar :: Integral t => t -> Bool
ehPar n = (n `mod` 2) == 0

buscaK_esimo :: Int -> [t] -> t
buscaK_esimo k l = buscaK_esimo' 0 k l
    where
        buscaK_esimo' :: Int -> Int -> [t] -> t
        buscaK_esimo' i k (c:r)
            | i == k    = c
            | otherwise = buscaK_esimo' (i + 1) k r


dividirLista :: Integral t => t -> [a] -> [[a]]
dividirLista k l = dividirLista' 0 k l
    where
        dividirLista' i k l@(c:r)
            | i < k     = concatenarPares ([[c], []]) (dividirLista' (i + 1) k r)
            | otherwise = [[], l]
            where concatenarPares (c1:r1) (c2:r2) = [c1 ++ c2, (acumEsq (++) [] r1) ++ (acumEsq (++) [] r2)]

-- Resolução das Questões

-- Questão 2
maioresQue :: Real t => t -> [t] -> [t]
maioresQue n (c:r)
    | c > n     = c : maioresQue n r
    | otherwise = maioresQue n r
maioresQue _ [] = []

-- Questão 5
removerUltimo :: [t] -> [t]
removerUltimo [] = []
removerUltimo l@(c:r)
    | removerUltimo' l = removerUltimo r
    | otherwise  = c : removerUltimo r
    where
        removerUltimo' :: [a] -> Bool 
        removerUltimo' [_] = True
        removerUltimo' _ = False

-- Questão 8
geraSequencia :: Int -> [Int]
geraSequencia n = geraSequencia' [ [x, -x] | x <- [1..n]]
    where
        geraSequencia' :: [[Int]] -> [Int]
        geraSequencia' l = acumEsq (++) [] l

-- Questão 11
somatorio :: Real t => [t] -> t
somatorio l = acumEsq (+) 0 l

-- Questão 14
interseccao :: Eq t => [t] -> [t] -> [t]
interseccao [] _ = []
interseccao l1@(c1:r1) l2 = interseccao' c1 l2 ++ interseccao r1 l2
    where
        interseccao' :: Eq t => t -> [t] -> [t]
        interseccao' _ [] = []
        interseccao' n (c:r)
            | n == c    = [n]
            | otherwise = interseccao' n r

-- Questão 17
insere_ordenado :: (Ord t) => [t] -> t -> [t]
insere_ordenado [] n = [n]
insere_ordenado l1@(c:r) n 
    | c > n = n : l1
    | otherwise = c : insere_ordenado r n

-- Questão 20
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
                    let k = (tamanhoLista l) `quot` 2
                    in ((buscaK_esimo (k - 1) l) + (buscaK_esimo k l)) / 2

-- Questão 23
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

-- Questão 26
media :: (Real t, Fractional t) => [t] -> t
media l = 
    let tam = tamanhoLista l
    in (acumEsq (+) 0.0 l) / fromIntegral tam

-- Questão 29
seleciona :: [t] -> [Int] -> [t]
seleciona l1 l2 = acumEsq (\ac a -> ac ++ [(buscaK_esimo a l1)]) [] l2'
    where l2' = transforma (\a -> a - 1) l2

-- Questão 32
primo :: (Integral t) => t -> Bool
primo 2 = True
primo n
    | n <= 1 = False
    | [ x | x <- [2..n-1], n `mod` x == 0] == [] = True
    | otherwise = False

-- Questão 33
soma_digitos :: (Integral t) => t -> Int 
soma_digitos n = somatorio(soma_digitos' n)
    where soma_digitos' n
            | n < 10 = [n]
            | otherwise = soma_digitos' (n `div` 10) ++ [(n `mod` 10)]

-- Questão 35
compactar :: (Integral t) => [t] -> [[t]]
compactar [] = []
compactar (c:r) = compactar' c r 1       
    where
        compactar' a [] b
            | b == 1    = [[a]]
            | otherwise = [[b, a]]
        compactar' n (c2:r2) b
            | n == c2 = compactar' c2 r2 (b+1)
            | b == 1 = [n] : compactar' c2 r2 1
            | otherwise = [b,n] : compactar' c2 r2 1