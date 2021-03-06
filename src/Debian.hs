{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Debian where

import Base
import Data.Proxy (Proxy (..))
import Devops.Base
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import Prelude
import Turtle
import qualified Data.Text as Text

newtype DebianPackage (c :: Symbol) = DebianPackage Binary

debianPackage :: KnownSymbol a => Text -> DevOp env (DebianPackage a)
debianPackage bin = go Proxy
  where
    go :: (KnownSymbol a) => Proxy a -> DevOp env (DebianPackage a)
    go proxy =
        let install  = procs "apt-get" ["install", "-y", "-q", pkg] empty
            pkg = Text.pack (symbolVal proxy)
            preOp    = buildPreOp ("install package " <> pkg)
                                  ("ensures that " <> pkg <> " are installed.")
                                  noCheck
                                  install
                                  noAction
                                  noAction
        in declare preOp $ return $ DebianPackage $ Binary bin

git :: DevOp env (DebianPackage "git")
git = debianPackage "git"

wget :: DevOp env (DebianPackage "wget")
wget = debianPackage "wget"

tar :: DevOp env (DebianPackage "tar")
tar = debianPackage "tar"

openjdk8 :: DevOp env (DebianPackage "openjdk-8-jdk")
openjdk8 = debianPackage "java"
