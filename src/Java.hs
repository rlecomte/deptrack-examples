{-# LANGUAGE OverloadedStrings #-}

module Java where

import Base
import Debian
import Devops.Base
import Turtle

newtype Maven3 = Maven3 Binary

maven3 :: DevOp env Maven3
maven3 = declare preOp $ wget >> tar >> return (Maven3 binary)
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

