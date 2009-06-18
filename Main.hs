{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Happstack.State
import Happstack.Server hiding (Host)
import Data.Typeable
import Control.Monad.State (put, get, modify)
import Control.Monad.Reader
import Control.Concurrent
import Data.Monoid
import Data.Generics
import qualified Data.Map as Map

type Host = String
type TestName = String
type Revision = String
type Duration = Int
type Measurement = (Host, TestName, Revision, Duration)

duration :: Measurement -> Duration
duration (_,_,_,d) = d

instance FromData Measurement where
    fromData = do
        test <- look "test"
        duration <- lookRead "duration"
        host <- look "host"
        revision <- look "revision"
        return (host, test, revision, duration)

type Measurements = Map.Map (Host, TestName) (Duration, [(Revision, Duration, Bool)])

data State = State {measurements :: Measurements} deriving (Typeable)
instance Version State
$(deriveSerialize ''State)

instance Component State where 
    type Dependencies State = End
    initialValue = State $ Map.fromList []

addMeasurement :: Measurement -> Update State Bool
addMeasurement (host, tname, rev, duration) = do
    State measurements <- get
    let newmap = upd measurements
    put . State $ newmap
    let (_,_,res) = head.snd $ newmap Map.! (host, tname)
    return res
    where upd = Map.insertWith upd' (host, tname) (duration, [(rev, duration, True)])
          upd' _ (mdur, ms) = (min mdur duration, (rev, duration, fromIntegral mdur <= fromIntegral (min mdur duration) * 1.05):ms)

getMeasurements :: Query State Measurements
getMeasurements = fmap measurements ask

$(mkMethods ''State ['addMeasurement, 'getMeasurements])

report :: ServerPart String
report = do
    Just measurement <- getData
    res <- update (AddMeasurement measurement)
    return $ if res
                then "PASS"
                else "FAIL"
    
listMeasurements :: ServerPart String
listMeasurements = do
    measurments <- query (GetMeasurements)
    return $ show measurments

entryPoint :: Proxy State
entryPoint = Proxy

controller = dir "report" report `mappend` (nullDir >> listMeasurements)

main = do 
    control <- startSystemState entryPoint
    tid <- forkIO $ simpleHTTP nullConf $ controller
    waitForTermination
    killThread tid
    createCheckpoint control
    shutdownSystem control

