{-
-- EPITECH PROJECT, 2024
-- wolfram
-- File description:
-- Lib
-}

module GetArgs (Conf(..), defaultConf, getOpts) where

import Text.Read (readMaybe)

data Conf = Conf {
    rule :: Maybe Int,
    start :: Maybe Int,
    o_lines :: Maybe Int,
    win :: Maybe Int,
    mv :: Maybe Int
} deriving Show

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing,
    start = Just 0,
    o_lines = Nothing,
    win = Just 80,
    mv = Nothing
}

setRule :: Conf -> Maybe Int -> Conf
setRule conf val = conf { rule = val }

setStart :: Conf -> Maybe Int -> Conf
setStart conf val = conf { start = val }

setLines :: Conf -> Maybe Int -> Conf
setLines conf val = conf { o_lines = val }

setWindow :: Conf -> Maybe Int -> Conf
setWindow conf val = conf { win = val }

setMove :: Conf -> Maybe Int -> Conf
setMove conf val = conf { mv = val }

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf (arg:val:other) =
    lookup arg options >>= \fieldSetter -> 
        maybe Nothing (\fieldValue ->
            getOpts (fieldSetter conf (Just fieldValue)) other) (readMaybe val)
    where options = [("--rule", setRule), ("--start", setStart),
                    ("--lines", setLines), ("--window", setWindow),
                    ("--move", setMove)]
getOpts _ _ = Nothing
