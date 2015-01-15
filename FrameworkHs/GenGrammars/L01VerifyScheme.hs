{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L01VerifyScheme where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Begin [Statement] Statement
data Statement
  = Set1 Var Integer
  | Set2 Var Var
  | Set3 Var Binop Var Integer
  | Set4 Var Binop Var Var
data Var
  = Reg Reg

instance PP Prog where
  pp (Begin l s) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp s)])))
  ppp (Begin l s) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp s)])))
instance PP Statement where
  pp (Set1 v i) = (ppSexp [fromByteString "set!",(pp v),(pp i)])
  pp (Set2 v v2) = (ppSexp [fromByteString "set!",(pp v),(pp v2)])
  pp (Set3 v b v2 i) = (ppSexp [fromByteString "set!",(pp v),(ppSexp [(pp b),(pp v2),(pp i)])])
  pp (Set4 v b v2 v3) = (ppSexp [fromByteString "set!",(pp v),(ppSexp [(pp b),(pp v2),(pp v3)])])
  ppp (Set1 v i) = (pppSexp [text "set!",(ppp v),(ppp i)])
  ppp (Set2 v v2) = (pppSexp [text "set!",(ppp v),(ppp v2)])
  ppp (Set3 v b v2 i) = (pppSexp [text "set!",(ppp v),(pppSexp [(ppp b),(ppp v2),(ppp i)])])
  ppp (Set4 v b v2 v3) = (pppSexp [text "set!",(ppp v),(pppSexp [(ppp b),(ppp v2),(ppp v3)])])
instance PP Var where
  pp (Reg r) = (pp r)
  ppp (Reg r) = (ppp r)

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Eq Statement
deriving instance Read Statement
deriving instance Show Statement
deriving instance Eq Var
deriving instance Read Var
deriving instance Show Var

