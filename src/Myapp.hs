module Myapp(appMain,state) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(newIORef, readIORef, writeIORef)
import Control.Concurrent.Timer(repeatedTimer, stopTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata(Mana, toMana, state, (.>))

appMain :: IO ()
appMain = do
  hSetBuffering stdout NoBuffering
  st <- newIORef state
  let appLoop = do
        com <- putStr "> " >> getLine
        s <- readIORef st
        if (com=="exit") then return () else do
          let s' = exeCom com s
          case s' of
            Nothing -> putStrLn "Error!"
            Just js -> do
              putStrLn (showS js)
              writeIORef st js
          appLoop
  let timerR = do
        s <- readIORef st
        let s' = doWithTime s
        if (s/=s') then putStrLn ("\n"++(show s')) >> putStr "> " else return ()
        writeIORef st s'
  tm <- repeatedTimer timerR (msDelay 1000)
  appLoop
  stopTimer tm

showS :: Mana -> String
showS = show 

exeCom :: String -> Mana -> Maybe Mana
exeCom com s = let coms =  words com
                   mns = map toMana coms
                in foldl (.>) (Just s) mns

doWithTime :: Mana -> Mana
doWithTime = id
