{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Happstack.State
import Happstack.Server
import Data.Typeable
import Control.Monad.State (put, get, modify)
import Control.Monad.Reader
import Control.Concurrent
import Data.Monoid
import Data.Generics
import Control.Arrow
import Data.Function


data Measurement = Measurement {
    test::String,
    duration::Int, --milliseconds
    host::String,
    revision::String
} deriving (Data, Typeable, Show)

instance FromData Measurement where
    fromData = do
        test <- look "test"
        duration <- lookRead "duration"
        host <- look "host"
        revision <- look "revision"
        return $ Measurement test duration host revision

instance Version Measurement
$(deriveSerialize ''Measurement)

data State = State {measurements :: [Measurement]} deriving (Typeable)
instance Version State
$(deriveSerialize ''State)

instance Component State where 
    type Dependencies State = End
    initialValue = State []

addMeasurement :: Measurement -> Update State [Measurement]
addMeasurement m = do
    State measurements <- get
    put . State $ m:measurements
    return (m:measurements)

getMeasurements :: Query State [Measurement]
getMeasurements = fmap measurements ask

$(mkMethods ''State ['addMeasurement, 'getMeasurements])

report :: ServerPart String
report = do
    Just measurement <- getData
    ms <- update (AddMeasurement measurement)
    let relevantMs = filter (comparable measurement) ms
    let best = foldl min 99999999 $ map duration relevantMs
    return $ if fromIntegral (duration measurement) <= fromIntegral best * 1.1
                then "PASS"
                else "FAIL"
    where comparable = (==) `on` (test &&& host)
    
listMeasurements :: ServerPart String
listMeasurements = do
    measurments <- query (GetMeasurements)
    return $ show measurments

entryPoint :: Proxy State
entryPoint = Proxy

controller = dir "report" report `mappend` dir "measurements" listMeasurements

main = do 
    control <- startSystemState entryPoint
    tid <- forkIO $ simpleHTTP nullConf $ controller
    waitForTermination
    killThread tid
    createCheckpoint control
    shutdownSystem control

