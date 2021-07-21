module Main where

import           Data.Complex
import           Graphics.Gloss


-- | Take while but take less than n elements
takeWhileLimit :: (a -> Bool) -> Int -> [a] -> [a]
takeWhileLimit _ 0 _ = []
takeWhileLimit _ _ [] = []
takeWhileLimit f n (x:xs) = if f x then
                                    x : takeWhileLimit f (n-1) xs
                            else []


-- | c -> z_{n-1} -> [z_n, z_{n+1}, ...]
mandelbrot :: RealFloat a => Complex a -> Complex a -> [Complex a]
mandelbrot c z = z : mandelbrot c ((z * z) + c)


-- | Useful part of the Mandelbrot sequence
mandelbrotUsable :: (RealFloat a, Ord a) => Complex a -> [Complex a]
mandelbrotUsable c = takeWhileLimit (\z -> magnitude z < limit)
                                     max_iter
                                     $ mandelbrot c (0 :+ 0)
                      where
                        limit = 4
                        max_iter = 80


-- | Compute the Mandelbrot coefficient used for Coloration
mandelbrotCoefficient :: (RealFloat a, Ord a) => Complex a -> Int
mandelbrotCoefficient = length . mandelbrotUsable


-- GRAPHICS --

-- | Window
window :: Display
window = InWindow "Haskell Mandelbrot" (750, 500) (0, 0)

-- Drawing

mod1 :: RealFrac a => a -> a
mod1 x | pf < 0 = pf+1
       | otherwise = pf
 where
  (_,pf) = properFraction x

hsv :: Float -> Float -> Float -> Color
hsv h s v = case hi of
    0 -> makeColor v t p 1
    1 -> makeColor q v p 1
    2 -> makeColor p v t 1
    3 -> makeColor p q v 1
    4 -> makeColor t p v 1
    _ -> makeColor v p q 1
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

getColor :: Float -> Color
getColor i = hsv h 1 v
  where c = i/80
        h = c*360
        v = if i == 80 then 0 else 1

toCoordinates :: Float -> Float -> Complex Float
toCoordinates x y = (x/250 - 0.5) :+ (y/250) -- x / 200 - 0.5  ; y / 500 * 2

setPixel :: Float -> Float -> Color -> Picture
setPixel x y c = translate x y $ color c $ rectangleSolid 1 1

getPict :: [(Float, Float)] -> [Picture]
getPict [] = []
getPict ((x, y):ps) = setPixel x y (getColor $ fromIntegral c) : getPict ps
   where c = mandelbrotCoefficient $ toCoordinates x y

drawing :: Picture
drawing = pictures $ getPict [(x, y) | x <- [-375 .. 375], y <- [-250 .. 250]]

main :: IO ()
main = display window black drawing

