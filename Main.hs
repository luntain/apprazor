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
import Text.JSON
import Data.List

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


deleteResult :: Measurement -> Update State ()
deleteResult (host, test, revision, dur) = do
    State measurements <- get
    put $ State $ Map.adjust g (host, test) measurements
    where g (best, ms) = let newMs = filter (not.toDelete) ms in (bestResult newMs, newMs)
          toDelete (r, d, _) = r == revision && d == d
          bestResult = foldl' max 99999999.0 . map duration 
          duration (_, d, _) = d


getMeasurements :: Query State Measurements
getMeasurements = fmap measurements ask

$(mkMethods ''State ['addMeasurement, 'getMeasurements, 'deleteResult])

report :: ServerPart Response
report = do
    Just measurement <- getData
    marginInput <- getDataFn $ lookRead "margin"
    let margin = fromMaybe 0.1 marginInput
    (res, best) <- update (AddMeasurement measurement margin)
    return . toResponse $ if res
                then "PASS" 
                else "FAIL\n" ++ show best
    
listMeasurements :: ServerPart Response
listMeasurements = do
    measurements <- query (GetMeasurements)
    return . toResponse . foldr g "" $ Map.toList measurements
    where g ((host, test), val) = ss host . ss " " . ss test . ss "\n" . shows val . ss "\n\n"
          ss = showString

tests :: ServerPart Response
tests = do
    measurements <- query (GetMeasurements)
    return . toResponse . encode . Map.keys $ measurements

displayDetails :: String -> String -> ServerPart Response
displayDetails hostName testName = do
    allMeasurements <- query GetMeasurements
    if (hostName, testName) `Map.member` allMeasurements
        then fileServeStrict [] "static/test-details.html"
        else fail $ "no such test or host" ++ show (hostName, testName)

testInfo :: String -> String -> ServerPart Response
testInfo hostName testName = do
    allMeasurements <- query GetMeasurements
    let measurements = allMeasurements Map.! (hostName, testName)
    return . toResponse . encode $ measurements


handleRemoveResult :: String -> String -> ServerPart Response
handleRemoveResult host test = do
    Just revision <- getDataFn $ look "revision"
    Just dur <- getDataFn $ lookRead "duration"
    update (DeleteResult (host, test, revision, dur))
    return $ toResponse $ "ok" ++ show (revision, dur)
    

entryPoint :: Proxy State
entryPoint = Proxy

testHostPart test host = msum [
        (nullDir >> displayDetails host test)
      , (dir "json" $ testInfo host test)
      , (dir "remove" $ methodSP POST $ handleRemoveResult host test)
    ]


controller = msum [
          dir "report" report  
        , (dir "static" $ fileServeStrict [] "static")
        , (nullDir >> fileServeStrict [] "static/index.html")
        , (dir "tests" tests)
        , (dir "list" listMeasurements)
        , path (\testName -> path (\hostName -> testHostPart testName hostName))
    ]


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

