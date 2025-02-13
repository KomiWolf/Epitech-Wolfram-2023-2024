{-
-- EPITECH PROJECT, 2024
-- wolfram
-- File description:
-- Main
-}

import System.Environment (getArgs)
import GetArgs (Conf(..), defaultConf, getOpts)
import Data.Maybe (fromJust)
import System.Exit
import DisplayHandler (launch)

displayErrorMsg :: String -> IO ()
displayErrorMsg str = putStrLn str >> exitWith(ExitFailure 84)

checkArgsLen :: [String] -> IO ()
checkArgsLen [] = displayErrorMsg "Argument list must not be empty"
checkArgsLen args 
    | length args < 2 = displayErrorMsg "Argument list must be greater than 2"
    | length args > 10 = displayErrorMsg "Argument list must be lesser than 10"
    | odd (length args) == True = displayErrorMsg "Argument list must be even"
    | otherwise = return ()

checkArgument :: Maybe Conf -> IO()
checkArgument (Just _) = return ()
checkArgument Nothing = displayErrorMsg "Invalid Argument provided"

checkRule :: Int -> Int -> Maybe Int -> Int -> IO ()
checkRule 0 startVal totalLines width = launch 0 startVal totalLines width
checkRule 1 startVal totalLines width = launch 1 startVal totalLines width
checkRule 2 startVal totalLines width = launch 2 startVal totalLines width
checkRule 3 startVal totalLines width = launch 3 startVal totalLines width
checkRule 4 startVal totalLines width = launch 4 startVal totalLines width
checkRule 5 startVal totalLines width = launch 5 startVal totalLines width
checkRule 6 startVal totalLines width = launch 6 startVal totalLines width
checkRule 7 startVal totalLines width = launch 7 startVal totalLines width
checkRule 8 startVal totalLines width = launch 8 startVal totalLines width
checkRule 9 startVal totalLines width = launch 9 startVal totalLines width
checkRule 10 startVal totalLines width = launch 10 startVal totalLines width
checkRule 30 startVal totalLines width = launch 30 startVal totalLines width
checkRule 90 startVal totalLines width = launch 90 startVal totalLines width
checkRule 110 startVal totalLines width = launch 110 startVal totalLines width
checkRule _ _ _ _ = displayErrorMsg "Unknow or unhandled rule"

toVar :: Maybe a -> a -> a
toVar maybeValue defaultValue =
    case maybeValue of
        Just value -> value
        Nothing -> defaultValue

main :: IO ()
main = do
    args <- getArgs
    checkArgsLen args
    let conf = defaultConf
    let updated = getOpts conf args
    checkArgument updated
    let r = toVar (rule (fromJust updated)) 0
    let s = toVar (start (fromJust updated)) 0
    let w = toVar (win (fromJust updated)) 80 + toVar (mv (fromJust updated)) 0
    checkRule r s (o_lines (fromJust updated)) (w `div` 2)
