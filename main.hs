-- Questão 2
maioresque n [] = []
maioresque t (c:r)
    | t >= c = maioresque t r
    | otherwise = [c] ++ (maioresque t r)

-- Questão 5
removerultimo [] = []
removerultimo (c:r)
    | null r = []
    | otherwise = [c] ++ removerultimo r

-- Questão 8
--INCOMPLETA
gerasequencia n = [[x,-x] | x<-[1..n]] 

-- Questão 11
-- FEITA, PORÉM NÃO IDEAL
somatorio [] = 0
somatorio (c:r) = c+somatorio(r)

-- Questão 14

-- Questão 17
insereordenado [] n = [n]
insereordenado l1@(c:r) n 
    | c > n = [n] ++ l1
    | otherwise = [c] ++ insereordenado r n

-- Questão 20
-- Questão 23
-- Questão 26
-- Questão 29
-- Questão 32
-- Questão 33
-- Questão 35
