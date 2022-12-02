module Mydata(Mana(..), toMana, state, (.>)) where

import qualified Data.Map.Strict as M

data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = T -> T -> T 

type Na = String

data Ta = Kaz Int
        | Hou [(String,Int)]
        | Tam [(String,Int)]
        | Dou
        | Zyu [T]
        | Sta Ply [Enm] [T]

data Ply = Ply {pki :: Int, pac :: Int, pst :: Int} deriving (Eq, Show)
data Enm = Enm {eki :: Int, eac :: Int, est :: Int} deriving (Eq, Show)

instance Eq Ta where
  (==) (Zyu ts1) (Zyu ts2) = map tName ts1 == map tName ts2
  (==) (Sta p1 e1 ts1) (Sta p2 e2 ts2) = p1==p2 && e1==e2 && map tName ts1 == map tName ts2 
  (==) a b = a==b

tName :: T -> Na
tName (T na _) = na

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1==t2

instance Show Ta where
  show (Zyu ts) = concat$map tName ts
  show (Sta p es ts) = (show p)++(concat$map show es)++(concat$map tName ts)
  show a = show a

instance Show Mana where
  show (Mana t _) = show t

(.>) :: Maybe Mana -> Maybe Mana -> Maybe Mana
(.>) (Just (Mana t1 y1)) (Just (Mana t2 y2)) = Just (Mana (y2 t1 t2) y1) 
(.>) _ _ = Nothing

manas :: M.Map String Ta 
manas = M.fromList [("hi",Kaz 1),("fu",Kaz 2),("mi",Kaz 3),("yo",Kaz 4),("yi",Kaz 5)
                   ,("mu",Kaz 6) ,("na",Kaz 7),("ya",Kaz 8),("ko",Kaz 9),("so",Kaz 10)
                   ,("hodama",Tam [("ho",1)]),("mizutama",Tam [("mi",1)])
                   ,("migi",Hou [("mg",5)]),("hidari",Hou [("hd",5)])
                   ,("nageru",Dou)]

toMana :: String -> Maybe Mana
toMana str = let ta = M.lookup str manas
              in (\t -> (Mana (T str t) youM)) <$> ta 

state :: Mana
state = Mana (T "state" (Sta player [enemy] [])) finSt 

player :: Ply
player = Ply{pki=50, pac=10, pst=10}

enemy :: Enm
enemy = Enm{eki=20, eac=8, est=8}

finSt :: Y
finSt _ t = t  

youM :: Y
youM t1@(T _ ta1) t2@(T _ ta2)
  | toConstr ta1 == toConstr ta2 = t1 .+. t2
  | otherwise                    = t1 .*. t2

toConstr :: Ta -> String
toConstr = head . words . show

(.+.) :: T -> T -> T
(.+.) (T n1 (Kaz a)) (T n2 (Kaz b)) = T (n1++"+"++n2) (Kaz (a+b))
(.+.) (T n1 (Hou la)) (T n2 (Hou lb)) = T (n1++"+"++n2) (Hou (la++lb))
(.+.) (T n1 (Tam la)) (T n2 (Tam lb)) = T (n1++"+"++n2) (Tam (la++lb))
(.+.) t1@(T n1 Dou) t2@(T n2 Dou) = T (n1++"+"++n2) (Zyu [t1,t2])
(.+.) (T n1 (Zyu la)) (T n2 (Zyu lb)) = T (n1++"+"++n2) (Zyu (la++lb))
(.+.) _ t = t

(.*.) :: T -> T -> T
(.*.) (T n1 (Kaz a)) (T n2 (Hou ls)) = T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))
(.*.) (T n1 (Kaz a)) (T n2 (Tam ls)) = T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))
(.*.) (T n1 (Hou ls)) (T n2 (Kaz a)) = T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))
(.*.) (T n1 (Tam ls)) (T n2 (Kaz a)) = T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))
(.*.) _ t = t

-----

