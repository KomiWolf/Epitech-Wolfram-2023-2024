{-
-- EPITECH PROJECT, 2024
-- wolfram
-- File description:
-- RuleHandler
-}

module RuleHandler 
    (Line(..), rule0, rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8,
    rule9, rule10, rule30, rule90, rule110) where

data Line a = Line [a] a [a]

instance Functor Line where
    fmap f (Line left cell right) = Line (fmap f left) (f cell) (fmap f right)

rule0 :: Line Bool -> Bool
rule0 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> False
    (False, True, False)  -> False
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule0 _ = error "Unhandled case"

rule1 :: Line Bool -> Bool
rule1 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> True
    (False, False, True)  -> False
    (False, True, False)  -> False
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule1 _ = error "Unhandled case"

rule2 :: Line Bool -> Bool
rule2 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> True
    (False, True, False)  -> False
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule2 _ = error "Unhandled case"

rule3 :: Line Bool -> Bool
rule3 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> True
    (False, False, True)  -> True
    (False, True, False)  -> False
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule3 _ = error "Unhandled case"

rule4 :: Line Bool -> Bool
rule4 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> False
    (False, True, False)  -> True
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule4 _ = error "Unhandled case"

rule5 :: Line Bool -> Bool
rule5 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> True
    (False, False, True)  -> False
    (False, True, False)  -> True
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule5 _ = error "Unhandled case"

rule6 :: Line Bool -> Bool
rule6 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> True
    (False, True, False)  -> True
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule6 _ = error "Unhandled case"

rule7 :: Line Bool -> Bool
rule7 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> True
    (False, False, True)  -> True
    (False, True, False)  -> True
    (False, True, True)   -> False
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule7 _ = error "Unhandled case"

rule8 :: Line Bool -> Bool
rule8 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> False
    (False, True, False)  -> False
    (False, True, True)   -> True
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule8 _ = error "Unhandled case"

rule9 :: Line Bool -> Bool
rule9 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> True
    (False, False, True)  -> False
    (False, True, False)  -> False
    (False, True, True)   -> True
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule9 _ = error "Unhandled case"

rule10 :: Line Bool -> Bool
rule10 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> True
    (False, True, False)  -> False
    (False, True, True)   -> True
    (True, False, False)  -> False
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule10 _ = error "Unhandled case"

rule30 :: Line Bool -> Bool
rule30 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> True
    (False, True, False)  -> True
    (False, True, True)   -> True
    (True, False, False)  -> True
    (True, False, True)   -> False
    (True, True, False)   -> False
    (True, True, True)    -> False
rule30 _ = error "Unhandled case"

rule90 :: Line Bool -> Bool
rule90 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True)  -> True
    (False, True, False)  -> False
    (False, True, True)   -> True
    (True, False, False)  -> True
    (True, False, True)   -> False
    (True, True, False)   -> True
    (True, True, True)    -> False
rule90 _ = error "Unhandled case"

rule110 :: Line Bool -> Bool
rule110 (Line (left:_) cell (right:_)) = case (left, cell, right) of
    (False, False, False) -> False
    (False, False, True) -> True
    (False, True, False) -> True
    (False, True, True) -> True
    (True, False, False) -> False
    (True, False, True) -> True
    (True, True, False) -> True
    (True, True, True) -> False
rule110 _ = error "Unhandled case"
