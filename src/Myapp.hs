module Myapp(appMain,state) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.IORef(newIORef, readIORef, writeIORef)
import Control.Concurrent.Timer(repeatedTimer, stopTimer)
import Control.Concurrent.Suspend(msDelay)
import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes(..)
             ,toMana, state, applyMana, (.>), maxY)

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
changePly m p [] bls = (m, normalPly p, bls)
changePly (Mes m) p@(Ply pki' pmki' prt' pmrt' py' px0' px1' pdx') (b@(Bul _ bs' by' bx' bdy' _):bs) bls =
  if (bdy'<0 && by'<=py' && bx'>=px0' && bx'<=px1') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (Mes (m++"\nattacked!"))
                        (Ply (pki'-bs') pmki' prt' pmrt' py' (px0'+dr) (px1'+dr) (pdx'-dr)) bs bls
                 else (Mes (m++"\nlose!"), Ply 0 pmki' prt' pmrt' py' px0' px1' 0, [])
     else changePly (Mes m) (normalPly p) bs (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' py' px0' px1' pdx') =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then Ply (pki'+1) pmki' pmrt' pmrt' py' px0' px1' pdx'
                             else if(pki'<pmki') then Ply pki' pmki' (prt'-1) pmrt' py' px0' px1' pdx'
                                                 else p
              else Ply pki' pmki' prt' pmrt' py' (px0'+dr) (px1'+dr) (pdx'-dr) 
                where dr=if(pdx'>0) then 1 else (-1)
  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms (Mes m) (e@(Enm eki' emki' ert' emrt' ey' ex0' ex1' edx'):es)
                    (b@(Bul _ bs' by' bx' bdy' _):bs) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex0' && bx'<=ex1') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (Mes (m++"\nhit!"))
                       ((Enm (eki'-bs') emki' ert' emrt' ey' (ex0'+dr) (ex1'+dr) (edx'-dr)):es) bs enms bls
                 else changeEnms (Mes (m++"\ndefeat!")) es bs enms bls
     else changeEnms (Mes m) ((normalEnm e):es) bs enms (bls++[b])
                          where dr=if(edx'>0) then 1 else if(edx'<0) then (-1) else 0

normalEnm :: Enm -> Enm 
normalEnm e@(Enm eki' emki' ert' emrt' ey' ex0' ex1' edx') =
  if(edx'==0) then
    if(ert'<0 && eki'<emki') then Enm (eki'+1) emki' emrt' emrt' ey' ex0' ex1' edx'
                             else if(eki'<emki') then Enm eki' emki' (ert'-1) emrt' ey' ex0' ex1' edx'
                                                 else e
              else Enm eki' emki' ert' emrt' ey' (ex0'+dr) (ex1'+dr) (edx'-dr) 
                where dr=if(edx'>0) then 1 else (-1)

changeBuls :: Mes -> [Bul] -> [Bul] -> (Mes,[Bul])
changeBuls m [] bls = (m,bls) 
changeBuls (Mes m) ((Bul bt' bs' by' bx' bdy' bdx'):bs) bls =
  let nby = by'+bdy'
   in if (nby>(maxY+bs') || nby<(0-bs'))
         then changeBuls (Mes m) bs bls
         else changeBuls (Mes m) bs (bls++[Bul bt' bs' nby (bx'+bdx') bdy' bdx'])
  


