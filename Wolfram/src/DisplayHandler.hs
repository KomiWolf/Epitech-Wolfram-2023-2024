{-
-- EPITECH PROJECT, 2024
-- wolfram
-- File description:
-- DisplayHandler
-}

module DisplayHandler (launch) where

import RuleHandler

setLeft :: Line a -> Line a
setLeft (Line (left:ls) cell right) = Line ls left (cell:right)
setLeft _ = error "Unhandled case"

setRight :: Line a -> Line a
setRight (Line left cell (right:rs)) = Line (cell:left) right rs
setRight _ = error "Unhandled case"

setLine :: Line a -> Line (Line a)
setLine l = Line (tail $ iterate setLeft l) l (tail $ iterate setRight l)

evolveLine :: (Line Bool -> Bool) -> Line Bool -> Line Bool
evolveLine rule l = fmap rule (setLine l)

evolveLines :: (Line Bool -> Bool) -> Line Bool -> Int -> [Line Bool]
evolveLines rule l totalLine = take totalLine $ iterate (evolveLine rule) l

run :: (Line Bool -> Bool) -> Line Bool -> Int -> Int -> [[Bool]]
run r line total width = let
    toList (Line left cell right) = reverse left ++ [cell] ++ right
    cutToWidth w (Line left c right) = Line(take w left) c (take (w - 1) right)
    in case total of
        -1 -> map (toList . (cutToWidth width)) (iterate (evolveLine r) line)
        _ -> map (toList . (cutToWidth width)) (evolveLines r line total)

displayCell :: Bool -> String
displayCell True = "*"
displayCell False = " "

display :: (Line Bool -> Bool) -> Int -> Line Bool -> Int -> Int -> IO ()
display rule start l totalLines width = mapM_ putStrLn (drop start generations)
    where
        generations = map displayLine $ run rule l totalLines width
        displayLine line = concat $ map displayCell line

launch :: Int -> Int -> Maybe Int -> Int -> IO ()
launch 0 s (Just value) w =
    display rule0 s (Line (repeat False) True (repeat False)) (value + s) w
launch 0 s Nothing w =
    display rule0 s (Line (repeat False) True (repeat False)) (-1) w

launch 1 s (Just value) w =
    display rule1 s (Line (repeat False) True (repeat False)) (value + s) w
launch 1 s Nothing w =
    display rule1 s (Line (repeat False) True (repeat False)) (-1) w

launch 2 s (Just value) w =
    display rule2 s (Line (repeat False) True (repeat False)) (value + s) w
launch 2 s Nothing w =
    display rule2 s (Line (repeat False) True (repeat False)) (-1) w

launch 3 s (Just value) w =
    display rule3 s (Line (repeat False) True (repeat False)) (value + s) w
launch 3 s Nothing w =
    display rule3 s (Line (repeat False) True (repeat False)) (-1) w

launch 4 s (Just value) w =
    display rule4 s (Line (repeat False) True (repeat False)) (value + s) w
launch 4 s Nothing w =
    display rule4 s (Line (repeat False) True (repeat False)) (-1) w

launch 5 s (Just value) w =
    display rule5 s (Line (repeat False) True (repeat False)) (value + s) w
launch 5 s Nothing w =
    display rule5 s (Line (repeat False) True (repeat False)) (-1) w

launch 6 s (Just value) w =
    display rule6 s (Line (repeat False) True (repeat False)) (value + s) w
launch 6 s Nothing w =
    display rule6 s (Line (repeat False) True (repeat False)) (-1) w

launch 7 s (Just v) w =
    display rule7 s (Line (repeat False) True (True:(repeat False))) (v + s) w
launch 7 s Nothing w =
    display rule7 s (Line (repeat False) True (True:(repeat False))) (-1) w

launch 8 s (Just v) w =
    display rule8 s (Line (repeat False) True (True:(repeat False))) (v + s) w
launch 8 s Nothing w =
    display rule8 s (Line (repeat False) True (repeat False)) (-1) w

launch 9 s (Just value) w =
    display rule9 s (Line (repeat False) True (repeat False)) (value + s) w
launch 9 s Nothing w =
    display rule9 s (Line (repeat False) True (repeat False)) (-1) w

launch 10 s (Just value) w =
    display rule10 s (Line (repeat False) True (repeat False)) (value + s) w
launch 10 s Nothing w =
    display rule10 s (Line (repeat False) True (repeat False)) (-1) w

launch 30 s (Just value) w =
    display rule30 s (Line (repeat False) True (repeat False)) (value + s) w
launch 30 s Nothing w =
    display rule30 s (Line (repeat False) True (repeat False)) (-1) w

launch 90 s (Just value) w =
    display rule90 s (Line (repeat False) True (repeat False)) (value + s) w
launch 90 s Nothing w =
    display rule90 s (Line (repeat False) True (repeat False)) (-1) w

launch 110 s (Just value) w =
    display rule110 s (Line (repeat False) True (repeat False)) (value + s) w
launch 110 s Nothing w =
    display rule110 s (Line (repeat False) True (repeat False)) (-1) w
launch _ _ _ _ = putStrLn "Bad Argument provided"
