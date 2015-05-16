{-# LANGUAGE RecordWildCards #-}
import Control.Monad (msum)
import Data.List (intercalate)

{- port from http://www.joshondesign.com/2014/09/17/rustlang -}

data Vector = Vector {x,y,z :: Double}
data Ray = Ray {orig,dir :: Vector}
data Color = Color {r,g,b :: Double}
data Sphere = Sphere {center :: Vector, radius :: Double, sphereColor :: Color}
data Light = Light {position :: Vector, lightColor :: Color}

light :: Light
light = Light {position=Vector 0.7 (-1) 1.7, lightColor=Color 1 1 1}

scene :: [Sphere]
scene =
  [ Sphere {center=Vector (-1) 0 3, radius=0.3, sphereColor=Color 1 0 0}
  , Sphere {center=Vector 0    0 3, radius=0.8, sphereColor=Color 0 1 0}
  , Sphere {center=Vector 1    0 3, radius=0.3, sphereColor=Color 0 0 1}]

(<**>) :: Vector -> Double -> Vector
Vector{..} <**> s = Vector {x=x*s,y=y*s,z=z*s}

scale :: Color -> Double -> Color
scale Color{..} s = Color {r=r*s,g=g*s,b=b*s}

(<+>) :: Vector -> Vector -> Vector
(Vector x1 y1 z1) <+> (Vector x2 y2 z2) = Vector {x=x1+x2,y=y1+y2,z=z1+z2}

plus :: Color -> Color -> Color
plus (Color r1 g1 b1) (Color r2 g2 b2) = Color {r=r1+r2,g=g1+g2,b=b1+b2}

(<->) :: Vector -> Vector -> Vector
(Vector x1 y1 z1) <-> (Vector x2 y2 z2) = Vector {x=x1-x2,y=y1-y2,z=z1-z2}

(<.>) :: Vector -> Vector -> Double
(Vector x1 y1 z1) <.> (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

magnitude :: Vector -> Double
magnitude v = sqrt (v <.> v)

normalize :: Vector -> Vector
normalize v = v <**> (1 / magnitude v)

normal :: Sphere -> Vector -> Vector
normal Sphere{..} pt = normalize $ pt <-> center

shapePixel :: Ray -> Sphere -> Double -> Int
shapePixel Ray{..} obj@Sphere{..} val =
  let pai = orig <+> (dir <**> val)
      Color r g b = diffuseShading pai obj light
  in  ceiling $ (r + g + b) * 2

intersectSphere :: Ray -> Sphere -> Maybe (Sphere, Double)
intersectSphere Ray{..} obj@Sphere{..} =
  let l = center <-> orig
      tca = l <.> dir
      d2 = l <.> l - tca * tca
      r2 = radius * radius
      thc = sqrt $ r2 - d2
      t0 = tca - thc
  in  if tca < 0 || d2 > r2 || t0 > 10000 then Nothing else Just (obj, t0)

diffuseShading :: Vector -> Sphere -> Light -> Color
diffuseShading pai obj@Sphere{..} Light{..} =
  let n = normal obj pai
      lam1 = normalize (position <-> pai) <.> n
      lam2 = min 1 (max 0 lam1)
  in  (lightColor `scale`(lam2 * 0.5)) `plus` (sphereColor `scale` 0.3)

render :: Int -> Int -> [String]
render w h = [[render' i j | i <- [0 .. w-1]] | j <- [0 .. h-1]]
  where (fw, fh) = (fromIntegral w, fromIntegral h)
        lut = ".-+*XM"
        render' i j =
          let (fi, fj) = (fromIntegral i, fromIntegral j)
              ray = Ray { orig=Vector 0 0 0
                        , dir= normalize $ Vector ((fi-fw/2)/fw) ((fj-fh/2)/fh) 1 }
          in case msum [intersectSphere ray obj | obj <- scene] of
               Nothing -> ' '
               Just (obj, val) -> lut !! shapePixel ray obj val

main :: IO ()
main = putStrLn . intercalate "\n" $ render (20*4) (10*4)

{- Output:
                                      ++***
                               ++++*********XXXX**
                            -+++++********XXXXXXXXXXX
                         --++++++********XXXXXXXXXXXXXX*
                       ----++++++*********XXXXXXXXXXXXXXX*
                      ----++++++++*********XXXXXXXXXXXXXXX*
                     -----+++++++++**********XXXXXXXXXXXXX**
             +      -------+++++++++************XXXXXXXXXX***      *
        ++****XXXXX--------++++++++++***************************XXXXXXXX*
      --++****XXXXXXX-------+++++++++++************************XXXXXXXXX***
     --+++****XXXXXXX*--------++++++++++++*******************++**XXXXXX****+
     ---+++*****XXXXX*---------++++++++++++++***************+++***********++
     ----++++*********-----------++++++++++++++++++*****++++++-++******++++-
      -----+++++****+--------------+++++++++++++++++++++++++++-+++++++++---
        ------+++++-------------------+++++++++++++++++++++++------------
             -      ----------------------+++++++++++++++++--      -
                     ----------------------------+++++------
                      -------------------------------------
                       -----------------------------------
                         -------------------------------
                            -------------------------
                               -------------------
                                      -----
-}
