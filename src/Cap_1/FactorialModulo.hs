-- FactorialModulo.hs
-- Factorial módulo.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 18 de Diciembre de 2017
-- ---------------------------------------------------------------------

module Cap_1.FactorialModulo where

-- ---------------------------------------------------------------------
-- Definir la función
--    factorialMod :: Integer -> Integer -> Integer
-- tal que (factorialMod n x) es el factorial de x módulo n. Por ejemplo,
--    factorialMod (7+10^9) 100       ==  437918130
--    factorialMod (7+10^9) (5*10^6)  ==  974067448
-- ---------------------------------------------------------------------

import Data.List (foldl')

-- 1ª definición
-- =============

factorialMod :: Integer -> Integer -> Integer
factorialMod n x =
  factorial x `mod` n

-- (factorial x) es el factorial de x. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial x = product [1..x]

-- 2ª definición
-- =============

factorialMod2 :: Integer -> Integer -> Integer
factorialMod2 n x =
  foldl' (prodMod n) 1 [1..x]

-- (prodMod n x y) es el producto de x e y módulo n. Por ejemplo,
--    prodMod 3 4 7  ==  1
--    prodMod 5 4 7  ==  -- 3
prodMod :: Integer -> Integer -> Integer -> Integer
prodMod n x y =
  ((x `mod` n) * (y `mod` n)) `mod` n
  
-- Comparación de eficiencia
-- =========================

--    λ> factorialMod (7+10^9) (5*10^4)
--    737935835
--    (2.62 secs, 2,601,358,640 bytes)
--    λ> factorialMod2 (7+10^9) (5*10^4)
--    737935835
--    (0.07 secs, 23,471,880 bytes)

