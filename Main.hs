-- Autor: Jakub Wróblewski 386401
import Lib
import Mon
import Text.Read
import System.Environment

-- Stan składajacy się z:
--- stosu liczb rzeczywistych
--- bieżącego punktu
--- bieżącej ścieżki - pierwszy i ostatni jej punkt
--- bieżącego obrazka
--- bieżącej transformacji
data State = S [Rational] (Maybe Point) (Maybe (Point, Point, Integer)) Picture Transform

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Nothing -> putStrLn "Pass one int value to scale resulting image (Default value = 1)"
      Just n -> do
        putStrLn prolog
        interact (program n)
        putStrLn epilog

parseArgs :: [String] -> Maybe Int
parseArgs [] = Just 1
parseArgs (h:[]) = readMaybe h :: Maybe Int
parseArgs list = Nothing

prolog :: String
prolog = "300 400 translate"

epilog :: String
epilog = "stroke showpage"

errorMessage :: String
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"

--przeczytaj inta z opcjonalnym plusem
readMaybeInt :: String -> Maybe Integer
readMaybeInt ('+':('-':n)) = Nothing
readMaybeInt ('+':n) = readMaybe n :: Maybe Integer
readMaybeInt input = readMaybe input :: Maybe Integer

--funkcja parsujaca komendy
executeCommand :: State -> String -> (State, Maybe String)
executeCommand state word
  | word == "add"         = executeOperation state (+)
  | word == "sub"         = executeOperation state (-)
  | word == "div"         = executeDiv state
  | word == "mul"         = executeOperation state (*)
  | word == "moveto"      = executeMoveTo state
  | word == "lineto"      = executeLineTo state
  | word == "closepath"   = executeClosePath state
  | word == "translate"   = executeTranslate state
  | word == "rotate"      = executeRotate state
  | otherwise             = (state, Nothing)

--pobierz dwie liczby ze stosu
getTwoFromStack :: State -> (State, Maybe (Rational, Rational))
getTwoFromStack state@(S [] _ _ _ _) = (state, Nothing)
getTwoFromStack state@(S (h:[]) _ _ _ _) = (state, Nothing)
getTwoFromStack (S (second:(first:t)) current_point current_path picture transforms) =
  ((S t current_point current_path picture transforms), Just (first, second))

--pobierz jedną liczbę ze stosu
getOneFromStack :: State -> (State, Maybe Rational)
getOneFromStack state@(S [] _ _ _ _) = (state, Nothing)
getOneFromStack (S (first:t) current_point current_path picture transforms) =
  ((S t current_point current_path picture transforms), Just first)

--funkcja wykonująca jedne z działań arytmetycznych
executeOperation :: State -> (Rational -> Rational -> Rational) -> (State, Maybe String)
executeOperation state f =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    ((S stack current_point current_path picture transforms), Just (first, second)) ->
      ((S (result:stack) current_point current_path picture transforms), Just "Success") where
        result = f first second

executeDiv :: State -> (State, Maybe String)
executeDiv state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture transforms), Just (first, second)) ->
      if second == 0 then (newState, Nothing)
      else ((S (result:stack) current_point current_path picture transforms), Just "Success") where
        result = first / second

--funkcja wykonująca komendę moveto
executeMoveTo :: State -> (State, Maybe String)
executeMoveTo state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    ((S stack current_point current_path picture transforms), Just (x, y)) ->
      ((S stack (Just (P(tx, ty))) (Just (P(tx, ty), P(tx, ty), 1)) picture transforms),
      Just "Success") where
        (P (tx, ty)) = trpoint transforms (P (x, y)) 

--funkcja wykonująca komendę lineto
executeLineTo :: State -> (State, Maybe String)
executeLineTo state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture transforms), Just (x, y)) ->
      getResult newState (tx, ty) where
        (P (tx, ty)) = trpoint transforms (P (x, y))
        getResult (S stack (Just (P (cpoX, cpoY))) (Just (firstP, _, n)) picture transforms) (x, y) =
          ((S stack (Just (P(x, y))) (Just (firstP, P(x, y), n + 1)) (picture & (line (cpoX, cpoY) (x, y))) transforms),
          Just "Success")
        getResult state pair = (state, Nothing)

--funkcja wykonująca komendę closepath
executeClosePath :: State -> (State, Maybe String)
executeClosePath state@(S stack current_point current_path picture transforms) =
  case current_path of
    Nothing -> (state, Just "Success")
    Just(_, _, 1) -> (state, Just "Success")
    Just(P(firstX, firstY), _, n) -> 
      case current_point of
        Nothing -> (state, Just "Success")
        Just(P(cX, cY)) ->
          ((S stack (Just (P(firstX, firstY)))
                    (Just (P(firstX, firstY), P(firstX, firstY), n + 1))
                    (picture & (line (cX, cY) (firstX, firstY))))
                    transforms,
          Just "Success")

--funkcja pobierająca dwa elementy ze stosu i wykonująca komendę translate
executeTranslate :: State -> (State, Maybe String)
executeTranslate state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture transforms), Just (x, y)) ->
      ((S stack current_point current_path picture ((translate (V (x, y))) >< transforms)), Just "Success")

--funkcja pobierająca jeden element ze stosu i wykonująca komendę rotate
executeRotate :: State -> (State, Maybe String)
executeRotate state =
  case getOneFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture transforms), Just deg) ->
      ((S stack current_point current_path picture ((rotate deg) >< transforms)), Just "Success")

--funkcja przechodząca przez całe wejście, wykonująca komendy i zwracająca końcowy obrazek
handleInput :: State -> [String] -> Int -> (String)
handleInput (S _ _ _ picture _) [] ratio =
  show (renderScaled ratio picture)
handleInput state@(S stack current_point current_path picture transforms) (word:t) ratio =
  case readMaybeInt word of
    Just n -> handleInput (S ((fromIntegral n):stack) current_point current_path picture transforms) t ratio
    Nothing -> case executeCommand state word of
      (newState, Just commandOutput) -> handleInput newState t ratio
      (newState, Nothing) -> errorMessage

--funkcja przyjmująca całe wejście i ustalająca stan początkowy
program :: Int -> String -> String
program ratio input = handleInput (S [] Nothing Nothing (Pic []) (T [])) (words input) ratio