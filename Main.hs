-- Autor: Jakub Wróblewski 386401
import Lib
import Mon
import Text.Read

-- Stan składajacy się z:
--- stosu liczb rzeczywistych
--- bieżącego punktu
--- bieżącej ścieżki - pierwszy i ostatni jej punkt
--- pełnego obrazka
data State = S [Rational] (Maybe Point) (Maybe (Point, Point, Integer)) Picture

main :: IO ()
main = interact program

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
  | word == "lineto"      = (state, Nothing)
  | word == "closepath"   = (state, Nothing)
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

handleInput :: State -> [String] -> String -> String
handleInput _ [] output = output ++ "\n" ++ epilog
handleInput state@(S stack current_point current_path picture) (word:t) output =
  case readMaybeInt word of
    Just n -> handleInput (S ((fromIntegral n):stack) current_point current_path picture) t output
    Nothing -> case executeCommand state word of
      (newState, Just commandOutput) -> handleInput newState t (output ++ commandOutput)
      (newState, Nothing) -> errorMessage

program :: String -> String
program input = handleInput (S [] Nothing Nothing (Pic [])) (words input) prolog