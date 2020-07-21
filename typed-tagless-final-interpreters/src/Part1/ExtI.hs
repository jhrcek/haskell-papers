-- | Lack of extensibility in the data type view
module Part1.ExtI where

import qualified Part1.Intro1 as Old

-- Attempt to add a new expression form: multiplication
-- We would like to reuse the old code, in Intro1.hs (show the code)
-- We don't want to extend the data type declaration in Intro1.hs
-- as that would require adjusting and recompiling all the code
-- that uses Intro1.hs (in particular, all the interpreters)

data Exp
  = EOld Old.Exp
  | Mul Exp Exp -- add a new variant

-- An extended sample expression
tim2 :: Exp
tim2 = Mul (EOld (Old.Lit 7)) (EOld Old.ti1)

-- tim2 is a bit ugly. But this is only a part of a problem

-- Why does the following fail to type check?
{-
tim1 :: Exp
tim1 = EOld (Old.Add (Old.Lit 7)
          (Old.Neg (Mul (EOld (Old.Lit 1)) (EOld (Old.Lit 2)))))
-}

-- So, we are stuck. Data type variants are NOT extensible.

main :: IO ()
main = do
  print "Done"
