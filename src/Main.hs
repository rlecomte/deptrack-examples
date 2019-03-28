{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Devops.Cli
import PetClinic
import Prelude
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["up"] -> do
          simpleMain installAndRunPetClinicPlatform [] ["down"] ()
          simpleMain installAndRunPetClinicPlatform [] ["up-seq"] ()
      ["dot"] ->
          simpleMain installAndRunPetClinicPlatform [] ["dot"] ()
      _ -> pure ()
