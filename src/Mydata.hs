module Mydata(State(..), Mana(..), toMana, state, (.>)) where

import qualified Data.Map.Strict as M
import Data.List (findIndex)

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = [T] -> T -> [T] 

type Na = String

data Ta = Kaz Int
        | Hou [(String,Int)]
        | Tam [(String,Int)]
        | Dou [String] [String] [T] [T]
        deriving (Eq, Show)

data State = State Ply [Enm] deriving (Eq, Show)
data Ply = Ply {pki :: Int, pac :: Int, pst :: Int} deriving (Eq, Show)
data Enm = Enm {eki :: Int, eac :: Int, est :: Int} deriving (Eq, Show)

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

manas :: M.Map String Ta 
manas = M.fromList [("hi",Kaz 1),("fu",Kaz 2),("mi",Kaz 3),("yo",Kaz 4),("yi",Kaz 5)
                   ,("mu",Kaz 6) ,("na",Kaz 7),("ya",Kaz 8),("ko",Kaz 9),("so",Kaz 10)
                   ,("hodama",Tam [("ho",1)]),("mizutama",Tam [("mi",1)])
                   ,("migi",Hou [("mg",5)]),("hidari",Hou [("hd",5)])
                   ,("nageru",Dou ["Tam"] ["Hou"] [] [])]

toMana :: String -> Maybe Mana
toMana str = let ta = M.lookup str manas
              in (\t -> (Mana (T str t) youM)) <$> ta 

state :: State 
state = State player [enemy] 

player :: Ply
player = Ply{pki=50, pac=10, pst=10}

enemy :: Enm
enemy = Enm{eki=20, eac=8, est=8}

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
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Hou ls)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Tam ls)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Hou ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Tam ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ts t@(T _ (Dou _ _ _ _)) = makeDou t ts
(.*.) ts t = t:ts

makeDou :: T -> [T] -> [T]
makeDou  _ [] = []
makeDou d@(T na (Dou ca cb t1 t2)) tal@(t3@(T nat ta):ts) 
  | elem cstr ca = let ca' = eraseFrom cstr ca
                             in makeDou (T (na++"-"++nat) (Dou ca' cb (t3:t1) t2)) ts
  | elem cstr cb = let cb' = eraseFrom cstr cb
                             in makeDou (T (na++"-"++nat) (Dou ca cb' t1 (t3:t2))) ts
  | otherwise = d:tal 
    where cstr = toConstr ta
makeDou _ ts = ts

eraseFrom :: Eq a => a -> [a] -> [a]
eraseFrom t ls = let ind = findIndex (== t) ls
                  in case ind of
                       Nothing -> ls
                       Just i  -> take i ls ++ drop (i+1) ls

-----

