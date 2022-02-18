module Main where

import Type
import Evaluation
import Parser

import System.IO

initial_environment :: Environment
initial_environment = []

showEnvironment :: Environment -> IO()
showEnvironment [] = return ()
showEnvironment ((variable_name, value) : next) = do
    putStrLn $ variable_name ++ "\t: " ++ show value
    showEnvironment next

main :: IO ()
main = do
    handle <- openFile "script.txt" ReadMode
    source <- hGetContents handle
    case statement source of
        Just (s, remain) -> do
            showEnvironment (statementEvaluation initial_environment s)
        Nothing -> putStrLn "Parse error"
