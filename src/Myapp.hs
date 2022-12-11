module Myapp(appMain,state) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(newIORef, readIORef, writeIORef)
import Control.Concurrent.Timer(repeatedTimer, stopTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes(..), toMana, state, applyMana, (.>))

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
  tm <- repeatedTimer timerR (msDelay 500)
  appLoop
  stopTimer tm

showS :: State -> String
showS = show 

exeCom :: String -> State -> Maybe State 
exeCom com s = let coms =  words com
                   mns = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] mns
                in if (res==[]) then Nothing else Just (makeState s{mns=[]} res)

makeState :: State -> [Mana] -> State
makeState st [] = st
makeState st (mn:mns) = makeState (applyMana st mn) mns 

doWithTime :: State -> State 
doWithTime (State p es ts s ms mns) =
  let (ms1,np,ts1) = changePly ms p ts []
      (ms2,nes,ts2) = changeEnms ms1 es ts1 [] []
      (ms3,ts3) = changeBuls ms2 ts2 []
   in State np nes ts3 s ms3 mns

changePly :: Mes -> Ply -> [Bul] -> [Bul] -> (Mes,Ply,[Bul])
changePly m p [] bls = (m, p, bls)
changePly (Mes m) (Ply pki' pac' pst' py' px0' px1' pdx') (b@(Bul _ bs' by' bx' bdy' _):bs) bls =
  if (bdy'<0 && by'<=py' && bx'>=px0' && bx'<=px1') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (Mes (m++"\nattacked!"))
                        (Ply (pki'-bs') pac' pst' py' (px0'+pdx') (px1'+pdx') pdx') bs bls
                 else (Mes (m++"\nlose!"), Ply 0 pac' pst' py' px0' px1' 0, [])
     else changePly (Mes m)
            (Ply (pki'- (abs pdx')) pac' pst' py' (px0'+pdx') (px1'+pdx') pdx') bs (bls++[b])
  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms (Mes m) ((Enm eki' eac' est' ey' ex0' ex1' edx'):es)
                    (b@(Bul _ bs' by' bx' bdy' _):bs) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex0' && bx'<=ex1') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (Mes (m++"\nhit!"))
                        ((Enm (eki'-bs') eac' est' ey' (ex0'+edx') (ex1'+edx') edx'):es) bs enms bls
                 else changeEnms (Mes (m++"\ndefeat!")) es bs enms bls
     else changeEnms (Mes m)
            ((Enm (eki'- (abs edx')) eac' est' ey' (ex0'+edx') (ex1'+edx') edx'):es) bs enms (bls++[b])

changeBuls :: Mes -> [Bul] -> [Bul] -> (Mes,[Bul])
changeBuls m [] bls = (m,bls) 
changeBuls (Mes m) ((Bul bt' bs' by' bx' bdy' bdx'):bs) bls =
  let nby = by'+bdy'
   in if (nby>10 || nby<0)
         then changeBuls (Mes m) bs bls
         else changeBuls (Mes m) bs (bls++[Bul bt' bs' nby (bx'+bdx') bdy' bdx'])
  


