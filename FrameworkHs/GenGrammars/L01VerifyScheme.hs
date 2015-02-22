{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L01VerifyScheme where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Letrec [(Label,[UVar],Body)] Body
data Body
  = Locals [UVar] Tail
data Tail
  = IfT Pred Tail Tail
  | BeginT [Effect] Tail
  | AppT1 Binop Value Value
  | AppT2 Value [Value]
  | TrivT Triv
data Pred
  = TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP Relop Value Value
data Effect
  = Nop
  | Set UVar Value
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
data Value
  = IfV Pred Value Value
  | BeginV [Effect] Value
  | AppV Binop Value Value
  | TrivV Triv
data Triv
  = UVar UVar
  | Integer Integer
  | Label Label

instance PP Prog where
  pp (Letrec l b) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,l2,b) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp (map pp l2)),(pp b)])])) l)),(pp b)])
  ppp (Letrec l b) = (pppSexp [text "letrec",(pppSexp (map (\(l,l2,b) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp (map ppp l2)),(ppp b)])])) l)),(ppp b)])
instance PP Body where
  pp (Locals l t) = (ppSexp [fromByteString "locals",(ppSexp (map pp l)),(pp t)])
  ppp (Locals l t) = (pppSexp [text "locals",(pppSexp (map ppp l)),(ppp t)])
instance PP Tail where
  pp (IfT p t t2) = (ppSexp [fromByteString "if",(pp p),(pp t),(pp t2)])
  pp (BeginT l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (AppT1 b v v2) = (ppSexp [(pp b),(pp v),(pp v2)])
  pp (AppT2 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (TrivT t) = (pp t)
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (AppT1 b v v2) = (pppSexp [(ppp b),(ppp v),(ppp v2)])
  ppp (AppT2 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (TrivT t) = (ppp t)
instance PP Pred where
  pp (TrueP) = (ppSexp [fromByteString "true"])
  pp (FalseP) = (ppSexp [fromByteString "false"])
  pp (IfP p p2 p3) = (ppSexp [fromByteString "if",(pp p),(pp p2),(pp p3)])
  pp (BeginP l p) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp p)])))
  pp (AppP r v v2) = (ppSexp [(pp r),(pp v),(pp v2)])
  ppp (TrueP) = (pppSexp [text "true"])
  ppp (FalseP) = (pppSexp [text "false"])
  ppp (IfP p p2 p3) = (pppSexp [text "if",(ppp p),(ppp p2),(ppp p3)])
  ppp (BeginP l p) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp p)])))
  ppp (AppP r v v2) = (pppSexp [(ppp r),(ppp v),(ppp v2)])
instance PP Effect where
  pp (Nop) = (ppSexp [fromByteString "nop"])
  pp (Set u v) = (ppSexp [fromByteString "set!",(pp u),(pp v)])
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (Set u v) = (pppSexp [text "set!",(ppp u),(ppp v)])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
instance PP Value where
  pp (IfV p v v2) = (ppSexp [fromByteString "if",(pp p),(pp v),(pp v2)])
  pp (BeginV l v) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp v)])))
  pp (AppV b v v2) = (ppSexp [(pp b),(pp v),(pp v2)])
  pp (TrivV t) = (pp t)
  ppp (IfV p v v2) = (pppSexp [text "if",(ppp p),(ppp v),(ppp v2)])
  ppp (BeginV l v) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp v)])))
  ppp (AppV b v v2) = (pppSexp [(ppp b),(ppp v),(ppp v2)])
  ppp (TrivV t) = (ppp t)
instance PP Triv where
  pp (UVar u) = (pp u)
  pp (Integer i) = (pp i)
  pp (Label l) = (pp l)
  ppp (UVar u) = (ppp u)
  ppp (Integer i) = (ppp i)
  ppp (Label l) = (ppp l)

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Body
deriving instance Read Body
deriving instance Show Body
deriving instance Ord Body
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
deriving instance Eq Value
deriving instance Read Value
deriving instance Show Value
deriving instance Ord Value
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv

