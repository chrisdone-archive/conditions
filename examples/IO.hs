{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Simple file IO example.

module IOExample where

import           Control.Condition
import           Control.Exception
import           Data.Typeable
import qualified System.IO as IO (openFile)
import           System.IO hiding (openFile)

-- | A file opening condition.
data OpenFileCondition = OpenFileCondition FilePath IOMode
  deriving (Typeable,Show)
instance Exception OpenFileCondition
instance Condition OpenFileCondition (IO Handle)

-- | File opening utility.
openFile :: Handlers => FilePath -> IOMode -> IO Handle
openFile fp mode =
  do result <- try (IO.openFile fp mode)
     case result of
       Left (_ :: IOException) -> signal (OpenFileCondition fp mode)
       Right h -> return h

-- | Example of using 'openFile'.
example :: IO Handle
example =
  withConditions
    (handler (\(OpenFileCondition _ mode) ->
                do putStrLn "Oh noes. How about a different file:"
                   fp <- getLine
                   openFile fp mode)
             (handler (\c@(OpenFileCondition fp mode) ->
                         do putStrLn "Oops, problem opening file. Retry once more?"
                            y <- getLine
                            if y == "y"
                               then openFile fp mode
                               else signal c)
                      (openFile "/foo/bar" ReadMode)))
