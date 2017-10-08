{-# LANGUAGE TemplateHaskell #-}
module Models.Data where

import Database.Persist.TH

-- This is to have a field in the db
data Category =
    Movies
  | Books
  | TvSeries
  deriving (Eq, Show, Read)
derivePersistField "Category"
