{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Lib where

import           Assem
import qualified Assem.Arm              as Arm
import           Canon
import           Capability.State
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (isInfixOf)
import           Frame.Arm              (ArmFrame (ArmFrame))
import           Parser
import           Semant
import           System.Directory
import           Text.Pretty.Simple     (pPrint)
import           Translate.Type         (Frag)

debug :: (Show a, MonadIO m) => a -> m ()
debug = liftIO . print

run s =
  case parse s of
    Right e -> do
      pPrint e
      runTrans $ do
        r <- trans e
        case r of
          Right t -> do
            pPrint t
            fs <- get @"frags"
            debug "frags"
            mapM_ debug fs
            (stms, bs) <- runCanon (fst t)
            debug "canon"
            debug  "-- stms --"
            mapM_ debug stms
            debug  "-- bs --"
            let pblock (i, b) = debug i >> mapM_ (\x -> debug $ "  " <> show x) b
            mapM_ pblock $ zip [0..] (fst bs)
            debug  "instruction"
            assems <- runCodegen Arm.munchStm (undefined :: ArmFrame) (concat $ fst bs)
            mapM_ debug assems
          Left e  -> debug "type error" >> pPrint e
    Left e -> putStrLn "parse error" >> putStrLn e

runC :: IO ()
runC = getContents >>= run

file :: String -> IO ()
file f =
  readFile f >>= run

skips = ["test16.tig", "test17.tig", "test18.tig", "test28.tig", "test29.tig"
        , "test47.tig", "test49.tig" -- todo: handle syntax error
        ]

-- tests :: IO ()
-- tests = do
--   let base = "/home/daishi/Documents/tiger/testcases"
--   fs <- filter (`notElem` skips) <$> listDirectory base
--   forM_ fs $ \f -> do
--     putStrLn $ "\n----" <> base <> "/" <> f <> "----"
--     b <- readFile (base <> "/" <> f)
--     let h = head $ lines b
--     putStrLn h
--     let isE = "error" `isInfixOf` h
--     let isSE = "syntax error" `isInfixOf` h

--     case parse b of
--       Right e -> do
--         case runTrans e of
--           Right t -> do
--             putStrLn "passed"
--             when isE $ error "unexpected passed"
--           Left e  -> unless isE $ error $ "tc error " <> show e
--       Left e ->
--         unless isSE $ error $ "parse error" <> e
