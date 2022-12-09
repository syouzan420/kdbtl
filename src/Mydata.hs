module Mydata(State(..), Mana(..), Ply(..), Enm(..), Bul(..), Mes(..)
             ,toMana, applyMana, state, (.>), toKaz) where

import qualified Data.Map.Strict as M
import Data.List (findIndex, isInfixOf)
import Data.List.Split (splitOn)

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = [T] -> T -> [T] 

type Na = String

data Ta = Kaz Int
        | Zyo Char
        | Hou [(Dr,Int)]
        | Tam [(Bu,Int)]
        | Dou [String] [String] [T] [T]
        deriving (Eq, Show)

data State = State {pl  :: !Ply
                   ,ens :: ![Enm]
                   ,tms  :: ![Bul]
                   ,sw  :: !Swi
                   ,mes :: !Mes
                   ,mns :: ![Mana]
                   } deriving (Eq, Show)
data Ply = Ply {pki :: !Int,pac :: !Int,pst :: !Int,py :: !Int,px0 :: !Int,px1 :: !Int,pdx :: !Int}
                                                                                    deriving (Eq, Show)
data Enm = Enm {eki :: !Int,eac :: !Int,est :: !Int,ey :: !Int,ex0 :: !Int,ex1 :: !Int,edx :: !Int}
                                                                                    deriving (Eq, Show)
data Bul = Bul {bt :: !Bu,bs :: !Int,by :: !Int,bx :: !Int,bdy :: !Int,bdx :: !Int} deriving (Eq, Show)
data Swi = Swi {itm :: !Bool} deriving (Eq, Show)
data Mes = Mes {ms1 :: !String} deriving (Eq, Show)

type Fun = [T] -> [T] -> State -> State

data Bu = Ho | Mi deriving (Eq, Show)           -- Tama Type Hodama, Mizutama
data Dr = Mg | Hd | Ue | Si deriving (Eq, Show) -- Direction Migi, Hidari, Ue, Sita

instance Eq Fun where
  (==) f1 f2 = f1==f2

instance Show Fun where
  show _ = "FUNC"

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1==t2

instance Show Mana where
  show (Mana t _) = show t

(.>) :: [Mana] -> Mana -> [Mana]
(.>) [] mn = [mn]
(.>) ms (Mana t y) = makeManas (y (getTs ms) t) y

getTs :: [Mana] -> [T]
getTs mns = map (\(Mana t' _) -> t') mns

makeManas :: [T] -> Y -> [Mana]
makeManas ts y = map (\t -> Mana t y) ts

kazElem :: M.Map String Int
kazElem = M.fromList [("hi",1),("fu",2),("mi",3),("yo",4),("yi",5)
                     ,("mu",6),("na",7),("ya",8),("ko",9),("so",10)]

toKaz :: String -> Maybe Int
toKaz str = if (isInfixOf "so" str) then
                let sps = splitOn "so" str
                    mbs = map (istoKaz . (++ "so")) (init sps)
                    mbs' = if(last sps=="") then mbs else mbs++[istoKaz (last sps)]
                 in foldl (flip (<*>)) (Just 0) $ ((+) <$>) <$> mbs' 
                                    else istoKaz str

istoKaz :: String -> Maybe Int
istoKaz [] = Nothing
istoKaz [_] = Nothing
istoKaz (a:b:xs) = let res = M.lookup (a:b:[]) kazElem 
                    in case res of
                         Just 10 -> (+) <$> res <*> (if(xs==[]) then Just 0 else istoKaz xs)
                         Just _  -> (*) <$> res <*> (if(xs==[]) then Just 1 else istoKaz xs)
                         Nothing -> Nothing

manas :: M.Map String Ta 
manas = M.fromList [("to",Zyo 't'),("ga",Zyo 'g'),("de",Zyo 'd')
                   ,("hodama",Tam [(Ho,1)]),("mizutama",Tam [(Mi,1)])
                   ,("migi",Hou [(Mg,1)]),("hidari",Hou [(Hd,1)])
                   ,("nageru",Dou ["Tam"] ["Hou","Kaz"] [] [])]

toMana :: String -> Maybe Mana
toMana str = let ta = case toKaz str of
                        Just i  -> Just (Kaz i)
                        Nothing -> M.lookup str manas
              in (\t -> (Mana (T str t) youM)) <$> ta 

funcName :: M.Map String Fun 
funcName = M.fromList [("nageru",nageru)]

state :: State 
state = State player [enemy] [] switch message [] 

player :: Ply
player = Ply{pki=50, pac=10, pst=10, py=0, px0=5, px1=7, pdx=0}

enemy :: Enm
enemy = Enm{eki=20, eac=8, est=8, ey=10, ex0=4, ex1=8, edx=0}

switch :: Swi
switch = Swi{itm=False}

message :: Mes
message = Mes{ms1=""}

youM :: Y
youM [] _ = []
youM ts@((T _ ta1):_) t@(T _ ta2)
  | toConstr ta1 == toConstr ta2 = ts .+. t
  | otherwise                    = ts .*. t

toConstr :: Ta -> String
toConstr = head . words . show

(.+.) :: [T] -> T -> [T]
(.+.) ((T n1 (Kaz a)):ts) (T n2 (Kaz b)) = (T (n1++"+"++n2) (Kaz (a+b))):ts
(.+.) ((T n1 (Hou la)):ts) (T n2 (Hou lb)) = (T (n1++"+"++n2) (Hou (la++lb))):ts
(.+.) ((T n1 (Tam la)):ts) (T n2 (Tam lb)) = (T (n1++"+"++n2) (Tam (la++lb))):ts
(.+.) (t1@(T _ (Dou _ _ _ _)):ts) t2@(T _ (Dou _ _ _ _)) = t2:t1:ts 
(.+.) ts t = t:ts

(.*.) :: [T] -> T -> [T]
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Kaz b)) = (T (n1++"*"++n2) (Kaz (a*b))):ts
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Hou ls)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Tam ls)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Hou ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Tam ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ts t@(T _ (Dou _ _ _ _)) = makeDou t ts
(.*.) ((T _ (Zyo ch)):ts) t = applZyo ch t ts 
(.*.) (t:ts) (T _ (Zyo 'd')) = uniSameTai t ts
(.*.) ts t = t:ts

applZyo :: Char -> T -> [T] -> [T]
applZyo 't' t ts = t:ts
applZyo 'g' t ts = ts .*. t
applZyo _ t ts = t:ts

uniSameTai :: T -> [T] -> [T]
uniSameTai t [] = [t]
uniSameTai t@(T _ ta1) ts@((T _ ta2):_) 
  | toConstr ta1 == toConstr ta2 = case ts .+. t of (nt:nts) -> uniSameTai nt nts; [] -> []
  | otherwise = t:ts

makeDou :: T -> [T] -> [T]
makeDou  d [] = [d] 
makeDou d@(T na (Dou ca cb t1 t2)) tal@(t3@(T nat ta):ts) 
  | elem cstr ca = let ca' = eraseFrom cstr ca
                    in makeDou (T (na++" "++nat) (Dou ca' cb (t3:t1) t2)) ts
  | elem cstr cb = let cb' = eraseFrom cstr cb
                    in makeDou (T (na++" "++nat) (Dou ca cb' t1 (t3:t2))) ts
  | otherwise = d:tal 
    where cstr = toConstr ta
makeDou _ ts = ts

applyMana :: State -> Mana -> State
applyMana st m@(Mana (T na (Dou _ _ ts1 ts2)) _) = 
  let nam = head$words na
      fnc = M.lookup nam funcName
      st' = st{mns = mns st ++ [m]}
   in case fnc of
        Nothing -> st'
        Just f  -> f ts1 ts2 st'
applyMana st m = st{mns= mns st ++ [m]}

eraseFrom :: Eq a => a -> [a] -> [a]
eraseFrom t ls = let ind = findIndex (== t) ls
                  in case ind of
                       Nothing -> ls
                       Just i  -> take i ls ++ drop (i+1) ls

-----
er1 :: String
er1 = "No Tama"

nageru :: Fun
nageru [] _ st = st{mes=Mes er1}
nageru ((T _ (Tam tm)):[]) [] st = st{tms=(tms st) ++ makeBullets tm [] 1 (getPlp st)} 
nageru ((T _ (Tam tm)):[]) ((T _ (Hou hus)):[]) st = st{tms=(tms st) ++ makeBullets tm hus 1 (getPlp st)} 
nageru ((T _ (Tam tm)):[]) ((T _ (Kaz kz)):[]) st = st{tms=(tms st) ++ makeBullets tm [] kz (getPlp st)} 
nageru ((T _ (Tam tm)):[]) ((T _ (Hou hus)):(T _ (Kaz kz)):[]) st =
                                          st{tms=(tms st) ++ makeBullets tm hus kz (getPlp st)} 
nageru ((T _ (Tam tm)):[]) ((T _ (Kaz kz)):(T _ (Hou hus)):[]) st =
                                          st{tms=(tms st) ++ makeBullets tm hus kz (getPlp st)} 
nageru _ _ st = st

makeBullets :: [(Bu,Int)] -> [(Dr,Int)] -> Int -> (Int, Int, Int) -> [Bul]
makeBullets [] _ _ _ = []
makeBullets ((b,s):bs) hus sp (y,x0,x1) = 
  let (dy, dx) = calcDelta hus sp
   in (Bul{bt=b, bs=s, by=y, bx=(x0+x1) `div` 2, bdy=dy, bdx=dx}):makeBullets bs hus sp (y,x0,x1)

getPlp :: State -> (Int, Int, Int)
getPlp st = let p = pl st in (py p, px0 p, px1 p)

calcDelta :: [(Dr,Int)] -> Int -> (Int, Int)
calcDelta hus sp = (sp, calcDx hus)
  where calcDx [] = 0
        calcDx ((dir,i):hs) 
          | dir==Mg = i + calcDx hs
          | dir==Hd = (-i) + calcDx hs
          | otherwise = calcDx hs
