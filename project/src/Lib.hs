module Lib where

import           Control.Monad
import           Data.List          (isInfixOf)
import           Parser
import           Semant
import           System.Directory
import           Text.Pretty.Simple (pPrint)


run s =
  case parse s of
    Right e -> do
      pPrint e
      case runTrans e of
        Right t -> pPrint t
        Left e  -> putStrLn "type error" >> pPrint e
    Left e -> putStrLn "parse error" >> putStrLn e

runC :: IO ()
runC = getContents >>= run

file :: String -> IO ()
file f =
  readFile f >>= run

skips = ["test16.tig", "test17.tig", "test18.tig", "test28.tig", "test29.tig"
        , "test47.tig", "test49.tig" -- todo: handle syntax error
        ]

tests :: IO ()
tests = do
  let base = "/home/daishi/Documents/tiger/testcases"
  fs <- filter (`notElem` skips) <$> listDirectory base
  forM_ fs $ \f -> do
    putStrLn $ "\n----" <> base <> "/" <> f <> "----"
    b <- readFile (base <> "/" <> f)
    let h = head $ lines b
    putStrLn h
    let isE = "error" `isInfixOf` h
    let isSE = "syntax error" `isInfixOf` h

    case parse b of
      Right e -> do
        case runTrans e of
          Right t -> do
            putStrLn "passed"
            when isE $ error "unexpected passed"
          Left e  -> unless isE $ error $ "tc error " <> show e
      Left e ->
        unless isSE $ error $ "parse error" <> e
