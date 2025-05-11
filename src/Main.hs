module Main where

-- Import needed for exception handling

import System.IO.Error (catchIOError)
import System.Environment
import System.IO
import qualified Data.Map as Map
import AST
import Data.List (isPrefixOf)
import Parser
import TypeChecker
import Evaluator
import Control.Monad (when)

-- REPL Execution Modes
data ExecutionMode = Interactive | BatchFile String
  deriving (Eq, Show)

-- Display a welcome message for the REPL
welcomeMessage :: String
welcomeMessage = unlines [
  "MiniFunc Interpreter - A small functional language",
  "Type :help for available commands",
  "Type :quit to exit"
  ]

-- Display help for the REPL
helpMessage :: String
helpMessage = unlines [
  "Available commands:",
  "  :help               - Show this help message",
  "  :type <expression>  - Show the type of an expression",
  "  :load <file>        - Load and execute a file",
  "  :quit               - Exit the interpreter",
  "",
  "Any other input will be evaluated as an expression."
  ]

-- Process a line of input in the REPL
processLine :: String -> IO Bool
processLine line = 
  case words line of
    [":quit"] -> return False
    [":help"] -> putStrLn helpMessage >> return True
    [":type", expr] -> processType expr >> return True
    [":load", file] -> processFile file >> return True
    (":type":rest) -> processType (unwords rest) >> return True
    (":load":rest) -> processFile (unwords rest) >> return True
    [] -> return True  -- Empty line
    _ -> processExpr line >> return True

-- Process a type command
processType :: String -> IO ()
processType input = 
  case parseExpr input of
    Left err -> putStrLn err
    Right expr -> 
      case typeCheck expr of
        Left err -> putStrLn err
        Right ty -> putStrLn $ "Type: " ++ prettyType ty

-- Process an expression
processExpr :: String -> IO ()
processExpr input = 
  case parseExpr input of
    Left err -> putStrLn err
    Right expr -> do
      -- First check the type
      case typeCheck expr of
        Left err -> putStrLn $ "Type error: " ++ err
        Right ty -> do
          putStrLn $ "Type: " ++ prettyType ty
          -- Then evaluate
          case evaluate expr of
            Left err -> putStrLn $ "Evaluation error: " ++ err
            Right val -> putStrLn $ "Result: " ++ val

-- Process a file
processFile :: String -> IO ()
processFile fileName = do
  exists <- doesFileExist fileName
  if exists
    then do
      contents <- readFile fileName
      let expressions = lines contents
      let filteredExpressions = filter (not . isComment) expressions
      putStrLn $ "Loaded file: " ++ fileName
      processExpressions filteredExpressions
    else putStrLn $ "File not found: " ++ fileName
  where
    isComment line = "//" `isPrefixOf` line

-- Process multiple expressions
processExpressions :: [String] -> IO ()
processExpressions [] = return ()
processExpressions (expr:exprs) = do
  when (not $ null $ trim expr) $ do
    putStrLn $ "> " ++ expr
    processExpr expr
    putStrLn ""
  processExpressions exprs

-- Check if file exists (minimal implementation)
doesFileExist :: String -> IO Bool
doesFileExist fileName = do
  result <- try (openFile fileName ReadMode) :: IO (Either IOError Handle)
  case result of
    Left _ -> return False
    Right handle -> do
      hClose handle
      return True

-- Trim whitespace from a string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
        isSpace c = c `elem` " \t\n\r"

-- Main function handling both REPL and file processing
main :: IO ()
main = do
  args <- getArgs
  mode <- case args of
    [] -> return Interactive
    [fileName] -> return $ BatchFile fileName
    _ -> do
      putStrLn "Usage: minifunc [file]"
      return Interactive
  
  case mode of
    Interactive -> do
      putStrLn welcomeMessage
      repl
    BatchFile fileName -> do
      exists <- doesFileExist fileName
      if exists
        then do
          contents <- readFile fileName
          let expressions = lines contents
          processExpressions expressions
        else putStrLn $ "File not found: " ++ fileName

-- REPL implementation
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  continue <- processLine input
  when continue repl

-- Helper for exception handling
try :: IO a -> IO (Either IOError a)
try action = (Right <$> action) `catch` (return . Left)
  where
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch = catchIOError
    