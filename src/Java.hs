{-# LANGUAGE OverloadedStrings #-}

module Java where

import Base
import Debian
import Devops.Base
import Turtle

maven3 :: DevOp env Binary
maven3 = declare preOp $ wget >> tar >> return binary
    where
        name = "apache-maven-3.6.0"

        archive = name <> "-bin.tar.gz"

        binary = Binary $ "/opt/" <> name <> "/bin/mvn"

        download =
            procs "wget"
                [ "https://www-eu.apache.org/dist/maven/maven-3/3.6.0/binaries/" <> archive
                , "-P", "/tmp/"
                ] empty

        extract = procs "tar" ["-xzf", "/tmp/" <> archive, "-C", "/tmp/"] empty

        checkIfExist = fmap fromBool $ testdir (fromText $ "/tmp/" <> name)

        preOp = buildPreOp "install maven 3"
                           "install maven 3 from donwloaded binaries."
                           checkIfExist
                           (download >> extract)
                           noAction
                           noAction

