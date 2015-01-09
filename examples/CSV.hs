{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parsing a CSV file row-by-row.

module CSVParsing where

import Control.Condition
import Control.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.CSV.Conduit
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Typeable
import Text.Read

-- | A file opening condition.
data DummyCondition = DummyCondition [String] (Maybe Int)
  deriving (Typeable)
instance Show DummyCondition where show _ = "Dummy condition"
instance Exception DummyCondition
instance Condition DummyCondition (ConduitM [[Char]] Int (ResourceT IO) ())

-- | Get dummy ints from the CSV file.
getDummyInts :: Handlers => FilePath -> IO [Int]
getDummyInts fp =
  runResourceT $
  sourceFile fp $=
  intoCSV defCSVSettings $=
  loop $$
  consume
  where loop =
          do mrow <- await
             case mrow of
               Just xs@[s,readMaybe -> Just i] ->
                 do case s of
                      "dummy" -> yield i
                      _ -> signal (DummyCondition xs (Just i))
                    loop
               Just xs -> do signal (DummyCondition xs Nothing)
                             loop
               Nothing -> return ()

-- | Main entry point.
main :: IO ()
main =
  withConditions
    (do is <- handler (\(DummyCondition xs mi) ->
                         do liftIO (do putStrLn ("Invalid row in CSV file: ")
                                       print xs
                                       putStrLn "Restarts: "
                                       case mi of
                                         Just{} -> putStrLn "(0) Ignore the first column."
                                         _ -> return ()
                                       putStrLn "(1) Skip the row."
                                       putStrLn "(2) Provide a different value for this row.")
                            fix (\loop ->
                                   do n <- liftIO readLn
                                      case n :: Int of
                                        0 -> case mi of
                                               Just i -> yield i
                                               _ -> return ()
                                        1 -> return ()
                                        2 -> do this <- liftIO (do putStrLn "Enter a value: "
                                                                   readLn)
                                                yield this
                                        _ -> loop))
                      (getDummyInts "example.csv")
        print is)
