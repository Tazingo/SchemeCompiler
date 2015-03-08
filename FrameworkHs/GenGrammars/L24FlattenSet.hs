{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L24FlattenSet where

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
  | Triv Triv
  | AppT1 Binop Triv Triv
  | AppT2 Triv [Triv]
data Pred
  = TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP Relop Triv Triv
data Effect
  = Nop
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
  | Set1 UVar Triv
  | Set2 UVar Binop Triv Triv
  | Set3 UVar Triv [Triv]
  | AppE Triv [Triv]
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
  pp (Triv t) = (pp t)
  pp (AppT1 b t t2) = (ppSexp [(pp b),(pp t),(pp t2)])
  pp (AppT2 t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (Triv t) = (ppp t)
  ppp (AppT1 b t t2) = (pppSexp [(ppp b),(ppp t),(ppp t2)])
  ppp (AppT2 t l) = (pppSexp ((ppp t) : (map ppp l)))
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
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (Set1 u t) = (ppSexp [fromByteString "set!",(pp u),(pp t)])
  pp (Set2 u b t t2) = (ppSexp [fromByteString "set!",(pp u),(ppSexp [(pp b),(pp t),(pp t2)])])
  pp (Set3 u t l) = (ppSexp [fromByteString "set!",(pp u),(ppSexp ((pp t) : (map pp l)))])
  pp (AppE t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (Set1 u t) = (pppSexp [text "set!",(ppp u),(ppp t)])
  ppp (Set2 u b t t2) = (pppSexp [text "set!",(ppp u),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
  ppp (Set3 u t l) = (pppSexp [text "set!",(ppp u),(pppSexp ((ppp t) : (map ppp l)))])
  ppp (AppE t l) = (pppSexp ((ppp t) : (map ppp l)))
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
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv

