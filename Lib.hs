-- Autor: Jakub Wróblewski 386401
module Lib where
import Mon

type R = Rational
type R2 = (R,R)
type Line = (R2, R2)
type IntLine = ((Int,Int), (Int,Int))

-- wektor 2D
data Vec = V R2
-- punkt 2D
data Point = P R2
-- rysunek złożony z linii
data Picture = Pic [Line]
-- transformacja czyli translacja o wektor (Vec) lub rotacja o liczbę (R)
data Transform = T [(Either Vec R)] deriving (Eq, Show)
-- renderowane picture
data IntRendering = RPic [IntLine]

instance Eq Point where
  P (x, y) == P (x', y') = (x == x') && (y == y')

instance Show Point where
  show (P (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Vec where
  V (x, y) == V (x', y') = (x == x') && (y == y')

instance Show Vec where
  show (V (x, y)) = "[" ++ show x ++ ", " ++ show y ++ "]"

instance Mon Vec where
  m1 = V (0, 0)
  V (x, y) >< V (x', y') = V (x + x', y + y')

instance Show IntRendering where
  show (RPic []) = ""
  show (RPic (((startX, startY), (endX, endY)):t)) =
    show startX ++ " " ++ show startY ++ " moveto "
    ++ show endX ++ " " ++ show endY ++ " lineto\n"

instance Mon Transform where
  m1 = T []
  T list1 >< T list2 = T (mergeRotations list1 list2) where
    mergeRotations [] [] = []
    mergeRotations list1 [] = list1
    mergeRotations [] list2 = list2
    mergeRotations list1 (h2:t2)
      | (isRight (last1)) && (isRight h2) = (init list1) ++ mergedRot ++ t2
      | otherwise = list1 ++ list2
      where
        last1 = last list1 
        mergedRot = [Right (normalizeDeg (fromRight last1 + fromRight h2))]

-- funkcja zapożyczona z jednej z bibliotek
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

-- zmodyfikowana funkcja zapożyczona z jednej z bibliotek
fromRight :: Either Vec R -> R
fromRight (Right b) = b
fromRight _         = 0

point :: R2 -> Point
point r2 = P r2

vec :: R2 -> Vec
vec r2 = V r2

normalizeDeg :: R -> R
normalizeDeg deg
  | deg >= 0 && deg <= fullCircle = deg
  | deg > fullCircle              = normalizeDeg (deg - fullCircle)
  | deg < 0                       = normalizeDeg (deg + fullCircle)

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R,R) -> (R,R) -> Picture
line start end = Pic [(start, end)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle width height = Pic [a, b, c, d] where
  a = ((0, 0), (0, height))
  b = ((0, height), (width, height))
  c = ((width, height), (width, 0))
  d = ((width, 0), (0, 0))

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
(&) (Pic list1) (Pic list2) = (Pic (list1 ++ list2))

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled ratio (Pic lineList) = (RPic (map mulLine lineList)) where
  mulLine (startPoint, endPoint) = (mulPoint startPoint, mulPoint endPoint)
  mulPoint (x, y) = ((round (x * (fromIntegral ratio))), (round (y * (fromIntegral ratio))))

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = T [Left v]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate deg = T [Right (normalizeDeg deg)]

-- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle :: R
fullCircle = 360

-- wyliczam wartość sinusa ze wzoru Bhaskary
getSine :: R -> R
getSine deg
  | deg < 0                = getSine (deg + 360)
  | deg >= 0 && deg <= 180 = (4 * deg * (180 - deg)) / (40500 - (deg * (180 - deg)))
  | otherwise              = -(getSine (deg - 180))

-- wyliczam wartość cosinusa z zależności z sinusem: cos(x) = sin(90-x)
getCosine deg = getSine(90 - deg)

rotatePoint :: Point -> R -> Point
rotatePoint (P (x, y)) deg = P (x', y') where
  x' = x * (getCosine deg) - y * (getSine deg)
  y' = y * (getCosine deg) + x * (getSine deg)

rotateVector :: Vec -> R -> Vec
rotateVector (V (x, y)) deg = V (x', y') where
  x' = x * (getCosine deg) - y * (getSine deg)
  y' = y * (getCosine deg) + x * (getSine deg)

translatePoint :: Point -> Vec -> Point
translatePoint (P (x, y)) (V (vx, vy)) = P (x + vx, y + vy)

-- transformacja wykonana na podanym punkcie
trpoint :: Transform -> Point -> Point
trpoint (T []) p = p
trpoint (T (h:t)) p = 
  case h of
    Left vec -> trpoint (T t) (translatePoint p vec)
    Right deg -> trpoint (T t) (rotatePoint p deg)

-- transformacja wykonana na podanym wektorze
-- w przypadku gdy transformacja jest wektorem, to jest to funkcja id
-- jesli transformacja jest rotacją to modyfikuje wektor
trvec :: Transform -> Vec -> Vec
trvec (T []) vec = vec
trvec (T (h:t)) vec = 
  case h of
    Left vec -> trvec (T t) vec
    Right deg -> trvec (T t) (rotateVector vec deg)

-- transformacja wykonana na podanym rysunku
transform :: Transform -> Picture -> Picture
transform (T []) pic = pic
transform t (Pic []) = (Pic [])
transform t (Pic picList) = (Pic (map transformList picList)) where
  transformList (start, end) = (transStart, transEnd) where
    (P transStart) = trpoint t (P start)
    (P transEnd) = trpoint t (P end)









