-- |
-- Module      : FactorialModulo
-- Description : Factorial módulo.
-- License     : GPL-3
-- Maintainer  : José A. Alonso
-- 
-- __Factorial módulo__
--
-- Se comparan dos definiciones de factorial con módulo para mostrar que
-- es más eficiente usar los módulos durante el cálculo.

module Cap_1.FactorialModulo where

-- | __(factorialMod n x)__ es el factorial de x módulo n. Por ejemplo,
--
-- >>> factorialMod (7+10^9) 100
-- 437918130
factorialMod :: Integer -> Integer -> Integer
factorialMod n x =
  factorial x `mod` n

-- | __(factorial x)__ es el factorial de x. Por ejemplo,
--
-- >>> factorial 3
-- 6
factorial :: Integer -> Integer
factorial x = product [1..x]

-- | 2ª definición.
--
-- >>> factorialMod2 (7+10^9) 100
-- 437918130
factorialMod2 :: Integer -> Integer -> Integer
factorialMod2 n x =
  foldr (prodMod n) 1 [1..x]

-- | __(prodMod n x y)__ es el producto de x e y módulo n. Por ejemplo,
--
-- >>> prodMod 3 4 7
-- 1
-- >>> prodMod 5 4 7
-- 3
prodMod :: Integer -> Integer -> Integer -> Integer
prodMod n x y =
  ((x `mod` n) * (y `mod` n)) `mod` n
  
-- $doc
--
-- __Comparación de eficiencia__
--
-- > > factorialMod (7+10^9) (3*10^4)
-- > 548996970
-- > (2.95 secs, 879,781,488 bytes)
--
-- > > factorialMod2 (7+10^9) (3*10^4)
-- > 548996970
-- > (0.12 secs, 14,188,936 bytes)

