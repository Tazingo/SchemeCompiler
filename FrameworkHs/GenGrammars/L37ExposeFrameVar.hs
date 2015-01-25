{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L37ExposeFrameVar where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Letrec [(Label,Tail)] Tail
data Tail
  = App Triv
  | Begin [Effect] Tail
data Effect
  = Set1 Var Triv
  | Set2 Var Binop Triv Triv
data Triv
  = Var Var
  | Integer Integer
  | Label Label
data Var
  = Reg Reg
  | Disp Disp

instance PP Prog where
  pp (Letrec l t) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,t) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp []),(pp t)])])) l)),(pp t)])
  ppp (Letrec l t) = (pppSexp [text "letrec",(pppSexp (map (\(l,t) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp []),(ppp t)])])) l)),(ppp t)])
instance PP Tail where
  pp (App t) = (ppSexp [(pp t)])
  pp (Begin l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  ppp (App t) = (pppSexp [(ppp t)])
  ppp (Begin l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
instance PP Effect where
  pp (Set1 v t) = (ppSexp [fromByteString "set!",(pp v),(pp t)])
  pp (Set2 v b t t2) = (ppSexp [fromByteString "set!",(pp v),(ppSexp [(pp b),(pp t),(pp t2)])])
  ppp (Set1 v t) = (pppSexp [text "set!",(ppp v),(ppp t)])
  ppp (Set2 v b t t2) = (pppSexp [text "set!",(ppp v),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
instance PP Triv where
  pp (Var v) = (pp v)
  pp (Integer i) = (pp i)
  pp (Label l) = (pp l)
  ppp (Var v) = (ppp v)
  ppp (Integer i) = (ppp i)
  ppp (Label l) = (ppp l)
instance PP Var where
  pp (Reg r) = (pp r)
  pp (Disp d) = (pp d)
  ppp (Reg r) = (ppp r)
  ppp (Disp d) = (ppp d)

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Eq Tail
deriving instance Read Tail
deriving instance Show Tail
deriving instance Eq Effect
deriving instance Read Effect
deriving instance Show Effect
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Eq Var
deriving instance Read Var
deriving instance Show Var

