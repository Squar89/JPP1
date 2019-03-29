module Mon where

infixl 5 ><

class Mon m where
  m1 :: m               -- element neutralny operacji ><
  (><) :: m -> m -> m   -- >< musi być łączne

-- ** Properties:
-- * leftUnit x = m1 >< x == x
-- * rightUnit x =  x >< m1 == x
-- * assoc x y z = (x >< y) >< z == x >< (y >< z)
