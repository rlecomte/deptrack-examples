{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PetClinic where

import Base
import Data.Text (Text)
import Debian
import Devops.Base
import Java
import Turtle

data PetClinicRepo = PetClinicRepo { repo :: !Text }

data PetClinicDatabase = PetClinicDatabase { host :: !Text, port :: !Int }

data PetClinicApp = PetClinicApp { host :: !Text, port :: !Int }

installAndRunPetClinicPlatform :: DevOp env PetClinicApp
installAndRunPetClinicPlatform = runPetClinic installPetClinic runPetClinicDatabase

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
    DebianPackage java <- openjdk8
    mvn                <- maven3
    PetClinicRepo rep  <- mkRepo
    _                  <- mkDb
    return ((java, mvn, rep), (PetClinicApp "localhost" 8080))
    where
        preOp dep =
            buildOp "start pet clinic"
                    "start pet clinic in local on port 8080."
                    noCheck
                    (start dep)
                    noAction
                    noAction

        start ((Binary java), (Binary mvn), rep) = do
            cd (fromText rep)
            procs mvn ["package"] empty
            shells (java <> " -jar target/*.jar") empty
