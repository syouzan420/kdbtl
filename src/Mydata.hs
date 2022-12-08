module Mydata(State(..), Mana(..), toMana, applyMana, state, (.>), toKaz) where

import qualified Data.Map.Strict as M
import Data.List (findIndex, isInfixOf)
import Data.List.Split (splitOn)

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = [T] -> T -> [T] 

type Na = String

data Ta = Kaz Int
        | Zyo Char
        | Hou [(String,Int)]
        | Tam [(String,Int)]
        | Dou [String] [String] [T] [T]
        deriving (Eq, Show)

data State = State {pl  :: !Ply
                   ,ens :: ![Enm]
                   ,tms  :: ![Bul]
                   ,sw  :: !Swi
                   ,mes :: !Mes
                   ,fun :: ![Fun]
                   ,mns :: ![Mana]
                   } deriving (Eq, Show)
data Ply = Ply {pki :: !Int,pac :: !Int,pst :: !Int,py :: !Int,px0 :: !Int,px1 :: !Int} deriving (Eq, Show)
data Enm = Enm {eki :: !Int,eac :: !Int,est :: !Int,ey :: !Int,ex0 :: !Int,ex1 :: !Int} deriving (Eq, Show)
data Bul = Bul {bt :: !Int,bs :: !Int,by :: !Int,bx :: !Int} deriving (Eq, Show)
data Swi = Swi {itm :: !Bool} deriving (Eq, Show)
data Mes = Mes {ms1 :: !String} deriving (Eq, Show)

type Fun = State -> (State,Int)

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
                   ,("hodama",Tam [("ho",1)]),("mizutama",Tam [("mi",1)])
                   ,("migi",Hou [("mg",1)]),("hidari",Hou [("hd",1)])
                   ,("nageru",Dou ["Tam"] ["Hou","Kaz"] [] [])]

toMana :: String -> Maybe Mana
toMana str = let ta = case toKaz str of
                        Just i  -> Just (Kaz i)
                        Nothing -> M.lookup str manas
              in (\t -> (Mana (T str t) youM)) <$> ta 

funcName :: M.Map String (Int -> [T] -> [T] -> Fun) 
funcName = M.fromList [("nageru",nageru)]

state :: State 
state = State player [enemy] [tama] switch message [] [] 

player :: Ply
player = Ply{pki=50, pac=10, pst=10, py=0, px0=5, px1=7}

enemy :: Enm
enemy = Enm{eki=20, eac=8, est=8, ey=10, ex0=4, ex1=8}

tama :: Bul
tama = Bul{bt=0, bs=0, by=0, bx=0}

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
      sfn = fun st
   in case fnc of
        Nothing -> st{mns = mns st ++ [m]}
        Just f  -> st{fun = sfn ++ [f 0 ts1 ts2], mns = mns st ++ [m]}
applyMana st m = st{mns= mns st ++ [m]}

eraseFrom :: Eq a => a -> [a] -> [a]
eraseFrom t ls = let ind = findIndex (== t) ls
                  in case ind of
                       Nothing -> ls
                       Just i  -> take i ls ++ drop (i+1) ls

-----

nageru :: Int -> [T] -> [T] -> Fun
nageru c t1 t2 st = (st,c+1)
