-- Autor: Jakub Wróblewski 386401
import Mon

cat :: String -> String
cat xs = xs
main = interact cat

type R = Rational
type R2 = (R,R)
type R2Line = (R2, R2)
type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]

-- wektor 2D
data Vec = V R2
-- punkt 2D
data Point = P R2
-- rysunek złożony z linii
data Picture = Pic [R2Line]
-- transformacja czyli translacja o wektor (Vec) lub rotacja o liczbę (R)
data Transform = T [(Either Vec R)]


instance Eq Point where
  P (x, y) == P (x', y') = (x == x') && (y == y')

instance Eq Vec where
  V (x, y) == V (x', y') = (x == x') && (y == y')

instance Show Vec where
  show (V (x, y)) = "[" ++ show x ++ ", " ++ show y ++ "]"

instance Mon Vec where
  m1 = V (0, 0)
  V (x, y) >< V (x', y') = V (x + x', y + y')

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
        mergedRot = [Right (minimizeDeg (fromRight last1 + fromRight h2))]

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

fromRight :: Either Vec R -> R
fromRight (Right b) = b
fromRight _         = 0

point :: R2 -> Point
point r2 = P r2

vec :: R2 -> Vec
vec r2 = V r2

minimizeDeg :: R -> R
minimizeDeg deg
  | deg >= -fullCircle && deg <= fullCircle = deg
  | deg > fullCircle                        = minimizeDeg (deg - fullCircle)
  | deg < -fullCircle                       = minimizeDeg (deg + fullCircle)

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
renderScaled ratio (Pic lineList) = map mulLine lineList where
  mulLine (startPoint, endPoint) = (mulPoint startPoint, mulPoint endPoint)
  mulPoint (x, y) = ((round (x * (fromIntegral ratio))), (round (y * (fromIntegral ratio))))

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = T [Left v]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate deg = T [Right (minimizeDeg deg)]

-- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle :: R
fullCircle = 360

-- transformacja wykonana na podanym punkcie
---trpoint :: Transform -> Point -> Point

-- transformacja wykonana na podanym wektorze
-- w przypadku gdy transformacja jest wektorem, to jest to funkcja id
-- jesli transformacja jest rotacją to modyfikuje wektor
---trvec :: Transform -> Vec -> Vec

-- transformacja wykonana na podanym rysunku
---transform :: Transform -> Picture -> Picture









