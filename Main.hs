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
import Data.Maybe
import Data.Generics
import System.Environment
import qualified Data.Map as Map

type Host = String
type TestName = String
type Revision = String
type Duration = Float
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

addMeasurement :: Measurement -> Float -> Update State (Bool, Float)
addMeasurement (host, tname, rev, duration) margin = do
    State measurements <- get
    let newmap = upd measurements
    put . State $ newmap
    let (best,  (_, _, res):_) = newmap Map.! (host, tname)
    return (res, best) 
    where upd = Map.insertWith upd' (host, tname) (duration, [(rev, duration, True)])
          upd' _ (mdur, ms) = (min mdur duration, (rev, duration, duration <= mdur * (1.0+margin)):ms)

getMeasurements :: Query State Measurements
getMeasurements = fmap measurements ask

$(mkMethods ''State ['addMeasurement, 'getMeasurements])

report :: ServerPart String
report = do
    Just measurement <- getData
    marginInput <- getDataFn $ lookRead "margin"
    let margin = fromMaybe 0.1 marginInput
    (res, best) <- update (AddMeasurement measurement margin)
    return $ if res
                then "PASS" 
                else "FAIL\n" ++ show best
    
listMeasurements :: ServerPart String
listMeasurements = do
    measurements <- query (GetMeasurements)
    return . foldr g "" $ Map.toList measurements
    where g ((host, test), val) = ss host . ss " " . ss test . ss "\n" . shows val . ss "\n\n"
          ss = showString


displayDetails :: String -> String -> ServerPart String
displayDetails hostName testName = do
    allMeasurements <- query (GetMeasurements)
    let measurements = allMeasurements Map.! (hostName, testName)
    return $ show measurements


entryPoint :: Proxy State
entryPoint = Proxy

controller = dir "report" report  
    `mappend`  (nullDir >> listMeasurements) 
    `mappend`  (dir "details" $ path (\testName -> path (\hostName -> displayDetails hostName testName)))


main = do 
    args <- getArgs
    let stateName = case args of
         [name] -> name
         [] -> "apprazor"
    withProgName stateName $ do
        control <- startSystemState entryPoint
        tid <- forkIO $ simpleHTTP (Conf 5003 Nothing) $ controller
        putStrLn "listening on port 5003"
        waitForTermination
        killThread tid
        createCheckpoint control
        shutdownSystem control

