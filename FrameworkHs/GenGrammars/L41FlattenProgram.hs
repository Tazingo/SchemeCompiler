{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L41FlattenProgram where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Statement
  = Set1 Var Triv
  | Set2 Var Binop Triv Triv
  | Jump Triv
  | LabelS Label
data Triv
  = Var Var
  | Integer Integer
  | LabelT Label
data Var
  = Reg Reg
  | Disp Disp
data Prog
  = Code [Statement] Statement

instance PP Statement where
  pp (Set1 v t) = (ppSexp [fromByteString "set!",(pp v),(pp t)])
  pp (Set2 v b t t2) = (ppSexp [fromByteString "set!",(pp v),(ppSexp [(pp b),(pp t),(pp t2)])])
  pp (Jump t) = (ppSexp [fromByteString "jump",(pp t)])
  pp (LabelS l) = (pp l)
  ppp (Set1 v t) = (pppSexp [text "set!",(ppp v),(ppp t)])
  ppp (Set2 v b t t2) = (pppSexp [text "set!",(ppp v),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
  ppp (Jump t) = (pppSexp [text "jump",(ppp t)])
  ppp (LabelS l) = (ppp l)
instance PP Triv where
  pp (Var v) = (pp v)
  pp (Integer i) = (pp i)
  pp (LabelT l) = (pp l)
  ppp (Var v) = (ppp v)
  ppp (Integer i) = (ppp i)
  ppp (LabelT l) = (ppp l)
instance PP Var where
  pp (Reg r) = (pp r)
  pp (Disp d) = (pp d)
  ppp (Reg r) = (ppp r)
  ppp (Disp d) = (ppp d)
instance PP Prog where
  pp (Code l s) = (ppSexp (fromByteString "code" : ((map pp l) ++ [(pp s)])))
  ppp (Code l s) = (pppSexp (text "code" : ((map ppp l) ++ [(ppp s)])))

deriving instance Eq Statement
deriving instance Read Statement
deriving instance Show Statement
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Eq Var
deriving instance Read Var
deriving instance Show Var
deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog

