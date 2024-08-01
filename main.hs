-- Questão 2
maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que n [] = []
maiores_que t (c:r)
    | t >= c = maiores_que t r
    | otherwise = [c] ++ (maiores_que t r)

-- Questão 5
remover_ultimo :: [b] -> [b]
remover_ultimo [] = []
remover_ultimo (c:r)
    | null r = []
    | otherwise = [c] ++ remover_ultimo r

-- Questão 8
--INCOMPLETA
gera_sequencia :: (Integral t) => t -> [[t]]
gera_sequencia n = [[x,-x] | x<-[1..n]] 

-- Questão 11
-- FEITA, PORÉM NÃO IDEAL
somatorio :: (Real t) => [t] -> t
somatorio [] = 0
somatorio (c:r) = c+somatorio(r)

-- Questão 14
--interseccao :: (Eq t) => [t] -> [t] -> [t]
--interseccao

-- Questão 17
insere_ordenado :: (Ord t) => [t] -> t -> [t]
insere_ordenado [] n = [n]
insere_ordenado l1@(c:r) n 
    | c > n = [n] ++ l1
    | otherwise = [c] ++ insere_ordenado r n

-- Questão 20
-- implementar ordenacao
--mediana :: (Real t) => [t] -> t
--mediana

-- Questão 23
--rodar_direita :: (Integral t) => t -> [a] -> a
--rodar_direita

-- Questão 26
--media :: (Real t) => [t] -> t
--media

-- Questão 29

--seleciona

-- Questão 32
-- fazer
-- DECLARAÇÃO ERRADA
--primo :: Integral -> Bool
primo 1 = False
primo 2 = True
primo n = null [ x | x <- [2..n-1], n `mod` x == 0]

-- Questão 33
-- fazer
-- DECLARAÇÃO ERRADA (PODE RECEBER INTEGER, MAS RETORNAR INT)
--soma_digitos :: (Integral t) => t -> t
soma_digitos n = somatorio(soma_digitos' n)
    where soma_digitos' n
            | n < 10 = [n]
            | otherwise = soma_digitos' (n `div` 10) ++ [(n `mod` 10)]

-- Questão 35
-- fazer
--compactar :: (Integral t) => [t] -> [[t]]


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


