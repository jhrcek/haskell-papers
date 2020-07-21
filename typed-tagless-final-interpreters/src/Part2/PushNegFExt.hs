{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Demonstrating `non-compositional', context-sensitive processing
--  Extending the final style
module Part2.PushNegFExt where

-- Explain the imports
-- Exp in the final form
-- Push_neg interpreter
import Part1.ExtF hiding (main) -- `mul' extension
import Part1.Intro2 hiding (main)
import Part2.PushNegF as PushNegF hiding (main)

-- * //

-- But the multiplication is not a homomorphism with respect to negation!

-- * neg (a * b) /= (neg a) * (neg b)

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 Pos = mul (e1 Pos) (e2 Pos)
  mul e1 e2 Neg = mul (e1 Pos) (e2 Neg)

-- Let us recall how an extended term looked like
tfm1_view :: String
tfm1_view = view tfm1

-- "(7 + (-(1 * 2)))"

tfm1_eval :: Int
tfm1_eval = eval tfm1

-- 5

tfm1_norm :: (ExpSYM t, MulSYM t) => t
tfm1_norm = push_neg tfm1

-- The new expression can be evaluated with any interpreter
tfm1_norm_view :: String
tfm1_norm_view = view tfm1_norm

-- "(7 + ((-1) * 2))"
-- The result of the standard evaluation (the `meaning') is preserved
tfm1_norm_eval :: Int
tfm1_norm_eval = eval tfm1_norm

-- 5

-- Add an extra negation
tfm1n_norm :: (ExpSYM t, MulSYM t) => t
tfm1n_norm = push_neg (neg tfm1)

-- see the result
tfm1n_norm_view :: String
tfm1n_norm_view = view tfm1n_norm

-- "((-7) + (1 * 2))"
tfm1n_norm_eval :: Int
tfm1n_norm_eval = eval tfm1n_norm

-- -5

-- Negate the already negated term
tfm1nn_norm :: (ExpSYM t, MulSYM t) => t
tfm1nn_norm = push_neg (neg tfm1n_norm)

tfm1nn_norm_view :: String
tfm1nn_norm_view = view tfm1nn_norm

-- "(7 + ((-1) * 2))"
tfm1nn_norm_eval :: Int
tfm1nn_norm_eval = eval tfm1nn_norm

-- 5

-- The same for tmf2
-- We can even use a previously defined unextended expression (tf1)
-- as a part of the extended expression.
-- We can indeed mix-and-match

tfm2_view :: String
tfm2_view = view tfm2

-- "(7 * (8 + (-(1 + 2))))"

tfm2_eval :: Int
tfm2_eval = eval tfm2

-- 35

tfm2_norm :: (MulSYM t, ExpSYM t) => t
tfm2_norm = push_neg tfm2

tfm2_norm_view :: String
tfm2_norm_view = view tfm2_norm

-- "(7 * (8 + ((-1) + (-2))))"
tfm2_norm_eval :: Int
tfm2_norm_eval = eval tfm2_norm

-- 35

-- Add an extra negation
tfm2n_norm :: (ExpSYM t, MulSYM t) => t
tfm2n_norm = push_neg (neg tfm2)

-- see the result
tfm2n_norm_view :: String
tfm2n_norm_view = view tfm2n_norm

-- "(7 * ((-8) + (1 + 2)))"
tfm2n_norm_eval :: Int
tfm2n_norm_eval = eval tfm2n_norm

-- -35

-- Negate the already negated term
tfm2nn_norm :: (ExpSYM t, MulSYM t) => t
tfm2nn_norm = push_neg (neg tfm2n_norm)

tfm2nn_norm_view :: String
tfm2nn_norm_view = view tfm2nn_norm

-- "(7 * (8 + ((-1) + (-2))))"
tfm2nn_norm_eval :: Int
tfm2nn_norm_eval = eval tfm2nn_norm

-- 35

main :: IO ()
main = do
  print PushNegF.tf1_norm_view -- old terms still work
  print Part2.PushNegFExt.tfm1_view
  print Part2.PushNegFExt.tfm1_eval
  print tfm1_norm_view
  print tfm1_norm_eval
  print tfm1n_norm_view
  print tfm1n_norm_eval
  print tfm1nn_norm_view
  print tfm1nn_norm_eval
  if tfm1_norm_view == tfm1nn_norm_view
    then return ()
    else error "Double neg"
  if Part2.PushNegFExt.tfm1_eval == tfm1_norm_eval
    then return ()
    else error "Normalization"
  if Part2.PushNegFExt.tfm1_eval == - tfm1n_norm_eval
    then return ()
    else error "Normalization"

  print Part2.PushNegFExt.tfm2_view
  print Part2.PushNegFExt.tfm2_eval
  print tfm2_norm_view
  print tfm2_norm_eval
  print tfm2n_norm_view
  print tfm2n_norm_eval
  print tfm2nn_norm_view
  print tfm2nn_norm_eval
  if tfm2_norm_view == tfm2nn_norm_view
    then return ()
    else error "Double neg"
  if Part2.PushNegFExt.tfm2_eval == tfm2_norm_eval
    then return ()
    else error "Normalization"
  if Part2.PushNegFExt.tfm2_eval == - tfm2n_norm_eval
    then return ()
    else error "Normalization"
