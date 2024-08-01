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

-- Resolução das Questões

-- Questão 2
maiores_que :: Real t => t -> [t] -> [t]
maiores_que n (c:r)
    | c > n     = c : maiores_que n r
    | otherwise = maiores_que n r
maiores_que _ [] = []

-- Questão 5
remover_ultimo :: [t] -> [t]
remover_ultimo [] = []
remover_ultimo l@(c:r)
    | remover_ultimo' l = remover_ultimo r
    | otherwise  = c : remover_ultimo r
    where
        remover_ultimo' :: [a] -> Bool 
        remover_ultimo' [_] = True
        remover_ultimo' _ = False

-- Questão 8
gera_sequencia :: Int -> [Int]
gera_sequencia n = gera_sequencia' [ [x, -x] | x <- [1..n]]
    where
        gera_sequencia' :: [[Int]] -> [Int]
        gera_sequencia' l = acumEsq (++) [] l

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
mediana :: (RealFrac t) => [t] -> t
mediana l = mediana' (insertionSort l)
    where
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


-- Questão 23
rodar_direita :: (Integral t) => t -> [a] -> [a]
rodar_direita n l = rodar_direita' (n `mod` tamanhoLista') l
    where
        rodar_direita' n l =
            let (c1:c2:_) = dividirLista (tamanhoLista' - n) l
            in c2 ++ c1
        tamanhoLista' = tamanhoLista l

-- Questão 26
media :: (RealFrac t) => [t] -> t
media l = 
    let tam = tamanhoLista l
    in (acumEsq (+) 0 l) / fromIntegral tam

-- Questão 29
seleciona :: [t] -> [Integer] -> [t]
seleciona l1 l2 = acumEsq (\ac a -> ac ++ [(buscaK_esimo a l1)]) [] l2'
    where l2' = transforma (\a -> a - 1) l2

-- Questão 32 *
primo :: (Integral t) => t -> Bool
-- primo 2 = True
-- primo n
--     | n <= 1                                     = False
--     | [ x | x <- [2..n-1], n `mod` x == 0] == [] = True
--     | otherwise                                  = False

primo 2 = True
primo n 
    | n <= 1 = False
    | otherwise = primo' n 2
    where 
    primo' n k
        | n == k = True
        | n `mod` k == 0 = False
        | otherwise      = primo' n (k+1)

-- Questão 33
soma_digitos :: (Integral t) => t -> t 
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