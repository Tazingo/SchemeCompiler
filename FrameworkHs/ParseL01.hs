
module FrameworkHs.ParseL01 where

import Data.IntMap (toList, (!), Key)
import Control.Applicative ((<$>), (<*>))
import Debug.Trace         (trace)
import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers (parseListWithFinal, parseInt32, parseInt64, parseLabel, parseUVar,
                            parseFailureM, PassM, orPassM, isInt64, 
                            parseValPrim, parseEffectPrim, parsePredPrim)

<<<<<<< HEAD
<<<<<<< HEAD
parseProg :: LispVal -> PassM Prog
parseProg l = Expr <$> parseExpr l

parseBinds :: LispVal -> PassM [(UVar,Expr)]
parseBinds (List ls) = mapM fn ls
 where
   fn (List [lhs,rhs]) = 
     do uv   <- parseUVar lhs
        rhs' <- parseExpr rhs
        return (uv,rhs')

<<<<<<< HEAD
parseLetrecBinds :: LispVal -> PassM [(UVar, [UVar], Expr)]
parseLetrecBinds (List ls) = mapM fn ls
  where 
    fn (List [label,List [Symbol "lambda", List formals, bod]]) = do
      label' <- parseUVar label
      formals' <- mapM parseUVar formals
      bod' <- parseExpr bod
      return (label', formals', bod')
    

parseExpr :: LispVal -> PassM Expr
parseExpr (List [Symbol "begin"]) = parseFailureM "empty begin form"
=======
parseLetrecBinds :: LispVal -> PassM [(UVar, Lamb)]
parseLetrecBinds (List ls) = mapM fn ls
  where 
    fn (List [label,lambda]) = do
      label' <- parseUVar label
      lambda' <- parseLamb lambda
      return (label', lambda')

parseLamb :: LispVal -> PassM Lamb
parseLamb (List [Symbol "lambda", List formals, bod]) = do
  formals' <- mapM parseUVar formals
  bod' <- parseExpr bod
  return $ Lambda formals' bod'
    

parseExpr :: LispVal -> PassM Expr

parseExpr (List [Symbol "begin"]) = parseFailureM "empty begin form"

>>>>>>> 37633c159eba0fd6618d28baa3436d32ebb939ef
parseExpr (List (Symbol "begin" : rst)) = do  
  es' <- mapM parseExpr (init rst)
  v'  <- parseExpr (last rst)
  return$ Begin es' v'
<<<<<<< HEAD
=======

>>>>>>> 37633c159eba0fd6618d28baa3436d32ebb939ef
parseExpr (List [Symbol "letrec",binds,bod]) = do
  binds' <- parseLetrecBinds binds
  bod' <- parseExpr bod
  return $ Letrec binds' bod'
<<<<<<< HEAD
  
=======
>>>>>>> 37633c159eba0fd6618d28baa3436d32ebb939ef

parseExpr (List [Symbol "if",p,v1,v2]) = do
  p'  <- parseExpr p
  v1' <- parseExpr v1
  v2' <- parseExpr v2
  return$ If p' v1' v2'

<<<<<<< HEAD
=======
parseExpr (List [Symbol "lambda", List formals, bod]) = do
  formals' <- mapM parseUVar formals
  bod' <- parseExpr bod
  return . Lamb $ Lambda formals' bod'

>>>>>>> 37633c159eba0fd6618d28baa3436d32ebb939ef
parseExpr (List [(Symbol "let"),binds,bod]) = do
  binds' <- parseBinds binds
  bod'   <- parseExpr bod
  return (Let binds' bod')

parseExpr (List [Symbol "quote",imm]) =
  Quote <$> parseImmediate imm   
  
parseExpr (List (op:rst)) = do
  firstItem <- (fmap App1 $ parseValPrim op) `orPassM` 
               (fmap App2 $ parseEffectPrim op) `orPassM`
               (fmap App3 $ parsePredPrim op) `orPassM`
               (fmap App4 $ parseExpr op)
  exprs <- mapM parseExpr rst
  return $ firstItem exprs

parseExpr sym@(Symbol _) =
<<<<<<< HEAD
  (UVar  <$> parseUVar sym) -- `orPassM`
--  (Label <$> parseLabel sym)
=======
  (UVar  <$> parseUVar sym)

>>>>>>> 37633c159eba0fd6618d28baa3436d32ebb939ef

parseImmediate :: LispVal -> PassM Immediate
parseImmediate x =
  case x of
    IntNumber i | isInt64 i -> return$ Fixnum (fromInteger i)
                | otherwise -> parseFailureM
                               ("Invalid Immediate/fixnum: won't fit in Int64 " ++ show i)
    List []       -> return NullList
    Boolean True  -> return HashT
    Boolean False -> return HashF
    _ -> error$"TODO: parseImmediate: "++show x
=======
>>>>>>> e90f2287d50048c329135484e7a745199bdb7ef3
=======
>>>>>>> edf9417267fd10d85b539715d1e4245ac0d4b7bc

parseProg :: LispVal -> PassM Prog
parseProg l = Expr <$> parseExpr l

parseBinds :: LispVal -> PassM [(UVar,Expr)]
parseBinds (List ls) = mapM fn ls
 where
   fn (List [lhs,rhs]) = 
     do uv   <- parseUVar lhs
        rhs' <- parseExpr rhs
        return (uv,rhs')

parseLetrecBinds :: LispVal -> PassM [(UVar, Expr)]
parseLetrecBinds (List ls) = mapM fn ls
  where 
    fn (List [name, body]) = do
      name' <- parseUVar name
      body' <- parseExpr body
      return (name', body')

parseExpr :: LispVal -> PassM Expr

parseExpr (List [Symbol "begin"]) = parseFailureM "empty begin form"

parseExpr (List (Symbol "begin" : rst)) = do  
  es' <- mapM parseExpr (init rst)
  v'  <- parseExpr (last rst)
  return$ Begin es' v'

parseExpr (List [Symbol "letrec",binds,bod]) = do
  binds' <- parseLetrecBinds binds
  bod' <- parseExpr bod
  return $ Letrec binds' bod'

parseExpr (List [Symbol "if",p,v1,v2]) = do
  p'  <- parseExpr p
  v1' <- parseExpr v1
  v2' <- parseExpr v2
  return$ If p' v1' v2'

parseExpr (List [Symbol "set!",uv,e]) = do
  uv' <- parseUVar uv
  e' <- parseExpr e
  return$ Set uv' e'

parseExpr (List [Symbol "lambda", List formals, bod]) = do
  formals' <- mapM parseUVar formals
  bod' <- parseExpr bod
  return $ Lambda formals' bod'

parseExpr (List [(Symbol "let"),binds,bod]) = do
  binds' <- parseBinds binds
  bod'   <- parseExpr bod
  return (Let binds' bod')

parseExpr (List [Symbol "quote",d]) =
  Quote <$> parseDatum d   
  
parseExpr (List (op:rst)) = do
  firstItem <- (fmap App1 $ parseValPrim op) `orPassM` 
               (fmap App2 $ parseEffectPrim op) `orPassM`
               (fmap App3 $ parsePredPrim op) `orPassM`
               (fmap App4 $ parseExpr op)
  exprs <- mapM parseExpr rst
  return $ firstItem exprs

parseExpr sym@(Symbol _) =
  (UVar  <$> parseUVar sym)

parseDatum :: LispVal -> PassM Datum
parseDatum (IntNumber i) | isInt64 i =
  return . ImmediateDatum . Fixnum $ fromInteger i
parseDatum (Boolean True) = return $ ImmediateDatum HashT
parseDatum (Boolean False) = return $ ImmediateDatum HashF
parseDatum (List [s1, Symbol ".", s2]) =
  PairDatum <$> parseDatum s1 <*> parseDatum s2
parseDatum (List sexps) = do
  sexps' <- mapM parseDatum sexps
  return $ foldr PairDatum (ImmediateDatum NullList) sexps'
parseDatum (Vector n m) =
  VectorDatum <$> mapM parseDatum (tabulate (m!) $ fromInteger n)
parseDatum sexp = parseFailureM $ "Parse error: invalid datum " ++ show sexp

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = t 0
  where
    --t :: Int -> [a]
    t i = if i == 0 then [] else f i : t (i + 1)
