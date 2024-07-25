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
-- DECLARAÇÃO ERRADA
--primo :: (Integral t) (Bool a) => t -> a
--primo

-- Questão 33
-- DECLARAÇÃO ERRADA (PODE RECEBER INTEGER, MAS RETORNAR INT)
--soma_digitos :: (Integral t) => t -> t
--soma_digitos

-- Questão 35
--compactar :: (Integral t) => [t] -> [[t]]
--compactar
