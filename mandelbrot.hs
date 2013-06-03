module Mandelbrot where
import Control.Monad
import Data.Char

data Size = Size Int Int

createBrot :: Size -> String
createBrot s @ (Size w h) =
    [ascii $ intensity x y s | x <- [0..w-1], y <- [0..h-1]]

ascii :: Int -> Char
ascii i = chr $ ord ' ' + (10-i)

intensity :: Int -> Int -> Size -> Int
intensity x y s @ (Size w h) = iteration 0 0 sx sy 0 10
    where
    sx = -2.5 + 3.5 * fromIntegral x  / fromIntegral w 
    sy = -1 + 2 * fromIntegral y  / fromIntegral h 

iteration :: Float -> Float -> Float -> Float -> Int -> Int -> Int
iteration x y sx sy i mi
    | x^2 + y^2 < 2*2 && i < mi =
        iteration (x^2 - y^2 + sx) (2*x*y + sy) sx sy (i+1) mi
    | otherwise = i

