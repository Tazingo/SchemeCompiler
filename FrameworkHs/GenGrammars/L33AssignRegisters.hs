{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L33AssignRegisters where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Letrec [(Label,Body)] Body
data Tail
  = IfT Pred Tail Tail
  | BeginT [Effect] Tail
  | AppT Triv [Loc]
data Pred
  = TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP Relop Triv Triv
data Effect
  = Nop
  | Set1 Var Triv
  | Set2 Var Binop Triv Triv
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
data Triv
  = Var Var
  | Integer Integer
  | Label Label
data Var
  = UVarV UVar
  | Loc Loc
data Loc
  = RegL Reg
  | FVar FVar
data Conflict
  = RegC Reg
  | UVarC UVar
data Body
  = Locate [(UVar,Reg)] Tail

instance PP Prog where
  pp (Letrec l b) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,b) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp []),(pp b)])])) l)),(pp b)])
  ppp (Letrec l b) = (pppSexp [text "letrec",(pppSexp (map (\(l,b) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp []),(ppp b)])])) l)),(ppp b)])
instance PP Tail where
  pp (IfT p t t2) = (ppSexp [fromByteString "if",(pp p),(pp t),(pp t2)])
  pp (BeginT l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (AppT t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (AppT t l) = (pppSexp ((ppp t) : (map ppp l)))
instance PP Pred where
  pp (TrueP) = (ppSexp [fromByteString "true"])
  pp (FalseP) = (ppSexp [fromByteString "false"])
  pp (IfP p p2 p3) = (ppSexp [fromByteString "if",(pp p),(pp p2),(pp p3)])
  pp (BeginP l p) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp p)])))
  pp (AppP r t t2) = (ppSexp [(pp r),(pp t),(pp t2)])
  ppp (TrueP) = (pppSexp [text "true"])
  ppp (FalseP) = (pppSexp [text "false"])
  ppp (IfP p p2 p3) = (pppSexp [text "if",(ppp p),(ppp p2),(ppp p3)])
  ppp (BeginP l p) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp p)])))
  ppp (AppP r t t2) = (pppSexp [(ppp r),(ppp t),(ppp t2)])
instance PP Effect where
  pp (Nop) = (ppSexp [fromByteString "nop"])
  pp (Set1 v t) = (ppSexp [fromByteString "set!",(pp v),(pp t)])
  pp (Set2 v b t t2) = (ppSexp [fromByteString "set!",(pp v),(ppSexp [(pp b),(pp t),(pp t2)])])
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (Set1 v t) = (pppSexp [text "set!",(ppp v),(ppp t)])
  ppp (Set2 v b t t2) = (pppSexp [text "set!",(ppp v),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
instance PP Triv where
  pp (Var v) = (pp v)
  pp (Integer i) = (pp i)
  pp (Label l) = (pp l)
  ppp (Var v) = (ppp v)
  ppp (Integer i) = (ppp i)
  ppp (Label l) = (ppp l)
instance PP Var where
  pp (UVarV u) = (pp u)
  pp (Loc l) = (pp l)
  ppp (UVarV u) = (ppp u)
  ppp (Loc l) = (ppp l)
instance PP Loc where
  pp (RegL r) = (pp r)
  pp (FVar f) = (pp f)
  ppp (RegL r) = (ppp r)
  ppp (FVar f) = (ppp f)
instance PP Conflict where
  pp (RegC r) = (pp r)
  pp (UVarC u) = (pp u)
  ppp (RegC r) = (ppp r)
  ppp (UVarC u) = (ppp u)
instance PP Body where
  pp (Locate l t) = (ppSexp [fromByteString "locate",(ppSexp (map (\(u,r) -> (ppSexp [(pp u),(pp r)])) l)),(pp t)])
  ppp (Locate l t) = (pppSexp [text "locate",(pppSexp (map (\(u,r) -> (pppSexp [(ppp u),(ppp r)])) l)),(ppp t)])

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Tail
deriving instance Read Tail
deriving instance Show Tail
deriving instance Ord Tail
deriving instance Eq Pred
deriving instance Read Pred
deriving instance Show Pred
deriving instance Ord Pred
deriving instance Eq Effect
deriving instance Read Effect
deriving instance Show Effect
deriving instance Ord Effect
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv
deriving instance Eq Var
deriving instance Read Var
deriving instance Show Var
deriving instance Ord Var
deriving instance Eq Loc
deriving instance Read Loc
deriving instance Show Loc
deriving instance Ord Loc
deriving instance Eq Conflict
deriving instance Read Conflict
deriving instance Show Conflict
deriving instance Ord Conflict
deriving instance Eq Body
deriving instance Read Body
deriving instance Show Body
deriving instance Ord Body

