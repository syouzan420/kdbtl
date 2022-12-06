module Myapp(appMain,state) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(newIORef, readIORef, writeIORef)
import Control.Concurrent.Timer(repeatedTimer, stopTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata(State(..), Mana, toMana, state, (.>))

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

showS :: State -> String
showS = show 

exeCom :: String -> State -> Maybe State 
exeCom com s = let coms =  words com
                   mns = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] mns
                in if (res==[]) then Nothing else Just (makeState s res)

makeState :: State -> [Mana] -> State
makeState st mns2 = st{mns=mns2}

doWithTime :: State -> State 
doWithTime = id
