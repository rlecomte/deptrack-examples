{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Base
import Control.Concurrent (forkIO,threadDelay)
import Data.Proxy (Proxy (..))
import Debian
import Devops.Base
import Devops.Cli
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude
import System.Environment (getArgs)
import Turtle
import qualified Data.Text as Text

newtype Maven3 = Maven3 Binary

data PetClinicRepo = PetClinicRepo { repo :: !Text }

data PetClinicDatabase = PetClinicDatabase { host :: !Text, port :: !Int }

data PetClinicApp = PetClinicApp { host :: !Text, port :: !Int }

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


installAndRunPetClinicPlatform :: DevOp env PetClinicApp
installAndRunPetClinicPlatform = runPetClinic installPetClinic runPetClinicDatabase


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


installPetClinic :: DevOp env PetClinicRepo
installPetClinic = declare preOp $ git >> return (PetClinicRepo "/tmp/spring-petclinic")
    where
        preOp = buildPreOp "clone pet clinic app"
                           "clone pet clinic app"
                           noCheck
                           clone
                           delete
                           noAction

        clone = procs "git" [ "clone"
                            , "https://github.com/spring-projects/spring-petclinic.git"
                            , "/tmp/spring-petclinic"] empty

        delete = void $ proc "rm" [ "-r", "/tmp/spring-petclinic" ] empty

runPetClinicDatabase :: DevOp env PetClinicDatabase
runPetClinicDatabase = declare preOp $ return (PetClinicDatabase "localhost" 3306)
    where
        preOp = buildPreOp "run mysql pet clinic database"
                           "run mysql pet clinic database in docker container."
                           noCheck
                           start
                           stop
                           noAction

        start = procs "docker" [ "run"
                               , "--rm"
                               , "-d"
                               , "-e"
                               , "MYSQL_ROOT_PASSWORD=petclinic"
                               , "-e"
                               , "MYSQL_DATABASE=petclinic"
                               , "--name", "petclinic-db"
                               , "-p"
                               , "3306:3306"
                               , "mysql:5.7.8"
                               ] empty

        stop = void $ proc "docker" [ "stop", "petclinic-db"] empty

runPetClinic :: DevOp env PetClinicRepo -> DevOp env PetClinicDatabase -> DevOp env PetClinicApp
runPetClinic mkRepo mkDb = devop snd (preOp . fst) $ do
    _                     <- openjdk8
    (Maven3 (Binary mvn)) <- maven3
    repo                  <- mkRepo
    _                     <- mkDb
    return ((mvn, repo), (PetClinicApp "localhost" 8080))
    where
        preOp (mvn, repo) =
            buildOp "start pet clinic"
                    "start pet clinic in local on port 8080."
                    noCheck
                    (start mvn repo)
                    noAction
                    noAction

        start mvn (PetClinicRepo repo) = do
            cd (fromText repo)
            procs mvn ["package"] empty
            shells "java -jar target/*.jar" empty
