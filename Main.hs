-- Autor: Jakub Wróblewski 386401
import Lib
import Mon
import Text.Read
import System.Environment

-- Stan składajacy się z:
--- stosu liczb rzeczywistych
--- bieżącego punktu
--- bieżącej ścieżki - pierwszy i ostatni jej punkt
--- pełnego obrazka
data State = S [Rational] (Maybe Point) (Maybe (Point, Point, Integer)) Picture

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Nothing -> putStrLn "Pass one int value to scale resulting image (Default value = 1)"
      Just n -> interact (program n)

parseArgs :: [String] -> Maybe Int
parseArgs [] = Just 1
parseArgs (h:[]) = readMaybe h :: Maybe Int
parseArgs list = Nothing

prolog :: String
prolog = "300 400 translate\n"

epilog :: String
epilog = "stroke showpage\n"

errorMessage :: String
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"

readMaybeInt :: String -> Maybe Integer
readMaybeInt ('+':('-':n)) = Nothing
readMaybeInt ('+':n) = readMaybe n :: Maybe Integer
readMaybeInt input = readMaybe input :: Maybe Integer

executeCommand :: State -> String -> (State, Maybe String)
executeCommand state word
  | word == "add"         = executeOperation state (+)
  | word == "sub"         = executeOperation state (-)
  | word == "div"         = executeDiv state
  | word == "mul"         = executeOperation state (*)
  | word == "moveto"      = executeMoveTo state
  | word == "lineto"      = executeLineTo state
  | word == "closepath"   = executeClosePath state
  | otherwise             = (state, Nothing)

getTwoFromStack :: State -> (State, Maybe (Rational, Rational))
getTwoFromStack state@(S [] _ _ _) = (state, Nothing)
getTwoFromStack state@(S (h:[]) _ _ _) = (state, Nothing)
getTwoFromStack (S (second:(first:t)) current_point current_path picture) =
  ((S t current_point current_path picture), Just (first, second))

executeOperation :: State -> (Rational -> Rational -> Rational) -> (State, Maybe String)
executeOperation state f =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    ((S stack current_point current_path picture), Just (first, second)) ->
      ((S (result:stack) current_point current_path picture), Just "") where
        result = f first second

executeDiv :: State -> (State, Maybe String)
executeDiv state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture), Just (first, second)) ->
      if second == 0 then (newState, Nothing)
      else ((S (result:stack) current_point current_path picture), Just "") where
        result = first / second

executeMoveTo :: State -> (State, Maybe String)
executeMoveTo state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    ((S stack current_point current_path picture), Just (x, y)) ->
      ((S stack (Just (P(x, y))) (Just (P(x, y), P(x, y), 1)) picture),
      Just (show x ++ " " ++ show y ++ "moveto "))

executeLineTo :: State -> (State, Maybe String)
executeLineTo state =
  case getTwoFromStack state of
    (newState, Nothing) -> (newState, Nothing)
    (newState@(S stack current_point current_path picture), Just (x, y)) ->
      getResult newState (x, y) where
        getResult (S stack (Just (P (cpoX, cpoY))) (Just (firstP, _, n)) picture) (x, y) =
          ((S stack (Just (P(x, y))) (Just (firstP, P(x, y), n + 1)) (picture & (line (cpoX, cpoY) (x, y)))),
          Just (show x ++ " " ++ show y ++ "lineto\n"))
        getResult state pair = (state, Nothing)

executeClosePath :: State -> (State, Maybe String)
executeClosePath state@(S stack current_point current_path picture) =
  case current_path of
    Nothing -> (state, Just "")
    Just(_, _, 1) -> (state, Just "")
    Just(P(firstX, firstY), _, n) -> 
      case current_point of
        Nothing -> (state, Just "")
        Just(P(cX, cY)) ->
          ((S stack (Just (P(firstX, firstY)))
                    (Just (P(firstX, firstY), P(firstX, firstY), n + 1))
                    (picture & (line (cX, cY) (firstX, firstY)))),
          Just ("closepath "))

handleInput :: State -> [String] -> Int -> (String)
handleInput (S stack current_point current_path picture) [] ratio =
  prolog ++ (show (renderScaled ratio picture)) ++ epilog
handleInput state@(S stack current_point current_path picture) (word:t) ratio =
  case readMaybeInt word of
    Just n -> handleInput (S ((fromIntegral n):stack) current_point current_path picture) t ratio
    Nothing -> case executeCommand state word of
      (newState, Just commandOutput) -> handleInput newState t ratio
      (newState, Nothing) -> errorMessage

program :: Int -> String -> String
program ratio input = handleInput (S [] Nothing Nothing (Pic [])) (words input) ratio