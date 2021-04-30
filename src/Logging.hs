{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Logging
  ( myLogFunc
  )
where

import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Char8         as BS8
import           RIO

-- Creating a logger this way is a hack. It works for stdout only, where the file 
-- handle is already open by default. See https://stackoverflow.com/a/67165507/4090111
myLogFunc :: LogFunc
myLogFunc = mkLogFunc $ \_cs _source level msg -> do
  BS8.putStr "["
  BS8.putStr $ levelStr level
  BS8.putStr "] "
  BS8.putStrLn $ toStrictBytes $ toLazyByteString $ getUtf8Builder msg

levelStr :: LogLevel -> ByteString
levelStr = \case
  LevelDebug   -> "Debug"
  LevelInfo    -> "Info"
  LevelWarn    -> "Warn"
  LevelError   -> "Error"
  LevelOther x -> encodeUtf8 x
