module Crisp

import Crisp.App
import Crisp.Data
import Control.App
import Data.Either
import Data.List
import Data.Maybe

parseVex : Has Eval e => HasErr CrispError e => Lst Datum -> App e (Vex Datum)
parseVex = ?_parseVex

parseLam : Has Eval e => HasErr CrispError e => Lst Datum -> App e (Lam Datum)
parseLam = ?_parseLam

parseLetBindings
  :  (Has Eval e, HasErr CrispError e)
  => Vec Datum -> App e (List (Binding, Datum))
parseLetBindings vec = plb vec.values
  where
    plb : List Datum -> App e (List (Binding, Datum))
    plb [] = pure []
    plb [x] = throw $ BadLet (OddBindings vec)
    plb (DSym s :: expr :: rest) = do
      s <- parseBinding s
      ((s, expr)::) <$> plb rest
    plb (bad :: _) = throw $ BadLet (BadBinding bad vec)

parseLet : (Has Eval e, HasErr CrispError e) => Lst Datum -> App e (Let Datum)
parseLet call = case call.values of
  (_ :: DVec v :: body) => do
    b <- parseLetBindings v
    pure $ MkLet b body
  _ => throw $ BadLet (ExpectedBindingVector call)

parseIf : (Has Eval e, HasErr CrispError e) => Lst Datum -> App e (If Datum)
parseIf call = case call.values of
  [_, cond, ifTrue, ifFalse] => pure $ MkIf cond ifTrue ifFalse
  [_, cond, ifTrue] =>  pure $ MkIf cond ifTrue DNil
  [_, cond] => throw $ BadIf (MissingBranches call)
  [_] => throw $ BadIf (MissingCond call)
  _ => throw $ BadIf (Max3Args call)

parseApply : (Has Eval e, HasErr CrispError e) => Lst Datum -> App e (Apply Datum)
parseApply call = ?parseApply_

mutual
  evalDatum : Has Eval e => HasErr CrispError e => Datum -> App e Datum
  evalDatum (DSym s) = lookupLexical s
  evalDatum d@(DList l) = case l.values of
    (h :: t) => evalDatum h >>= evalCall l
    Nil => pure d -- empty list is data
  evalDatum (DVec v) = evalVec v
  evalDatum d = pure d

  -- evaluate a list, returning all the results
  evalData : Has Eval e => HasErr CrispError e => List Datum -> App e (List Datum)
  evalData ls = ed ls []
    where
      ed : List Datum -> List Datum -> App e (List Datum)
      ed [] ret = pure $ reverse ret
      ed (h :: t) acc = evalDatum h >>= (ed t . (:: acc))

  -- evaluate a list, returning the last result (progn)
  evalData' : Has Eval e => HasErr CrispError e => List Datum -> App e Datum
  evalData' ls = ed ls DNil
    where
      ed : List Datum -> Datum -> App e Datum
      ed [] acc = pure acc
      ed (h :: t) _ = scoped (evalDatum h >>= ed t)

  evalCall : Has Eval e => HasErr CrispError e => Lst Datum -> Datum -> App e Datum
  evalCall call (DPrim PLet) = parseLet call >>= evalLet
  evalCall call (DPrim PLambda) = DLam <$> parseLam call
  evalCall call (DPrim PVex) = DVex <$> parseVex call
  evalCall call (DPrim PIf) = parseIf call >>= evalIf
  evalCall call (DPrim PApply) = parseApply call >>= evalApply
  evalCall call (DLam lam) = evalCallLam lam call
  evalCall call (DVex vex) = evalCallVex vex call
  evalCall l d = throw $ NotCallable d l

  evalLet : Has Eval e => HasErr CrispError e => Let Datum -> App e Datum
  evalLet (MkLet bindings body) = el bindings
    where
      el : List (Binding, Datum) -> App e Datum
      el [] = evalData' body
      el ((name, val) :: rest) = do
        val <- evalDatum val -- pay attention: every binding causes a new scope!
        scoped $ do
          bind name val
          el rest

  evalIf : Has Eval e => HasErr CrispError e => If Datum -> App e Datum
  evalIf if_ = do
    should <- evalDatum if_.condition
    evalDatum $ if isTruthy should then if_.whenTrue else if_.whenFalse

  evalApply : (Has Eval e, HasErr CrispError e) => Apply Datum -> App e Datum
  evalApply d = ?evalApply_

  evalCallLam : (Has Eval e, HasErr CrispError e) => Lam Datum -> Lst Datum -> App e Datum
  evalCallLam lam call =
    case call.values of
      (_ :: rest) => do
        vals <- evalData rest -- eval in our own environment
        inEnv lam.closure $ scoped $ do
          binds lam.params ({values := rest} call)
          evalData' lam.body
      _ => throw $ Unimplemented "impossible"

  evalCallVex : (Has Eval e, HasErr CrispError e) => Vex Datum -> Lst Datum -> App e Datum
  evalCallVex vex call = do
    caller <- get Lexicals
    inEnv vex.closure $ scoped $ do
      binds vex.call call
      bind vex.env  (DEnv caller)
      evalData' vex.body

  -- a vector makes a vector of evaluating its contents
  evalVec : Has Eval e => HasErr CrispError e => Vec Datum -> App e Datum
  evalVec v = scoped $ (DVec . \x => {values := x} v) <$> evalData v.values

-- mutual
--   data Tree
--     = TConst Datum
--     | TEval Tree
--     | TCall Tree Datum
--     | TBuiltin Builtin (Lst Datum)

--   data Builtin
--     = If Tree Tree Tree

-- -- Attempts to ascribe some meaning to the program in a compilation
-- -- environment.
-- compile : List (Symbol, Datum) -> Datum -> Either CrispError Tree
-- compile env d@(DNil _) = Right $ TConst d
-- compile env d@(DInt _) = Right $ TConst d
-- compile env d@(DStr _) = Right $ TConst d
-- compile env d@(DSym s) = TConst <$> maybeToEither (Undef d) (lookup s env)
-- -- compile env (DQ q) = ?d5_
-- -- compile env (DQQ q) = ?d_6
-- -- compile env (DUQ q) = ?d_7
-- -- compile env (DUS q) = ?d_8
-- compile env (DList l) = ?l
-- compile env (DVec v) = ?d_10

-- record Crisplet (t : Type) where
--   constructor MkCrisplet
--   env : Env Datum



--   data Builtin
--     = BLet (List (Binding, Tree)) Tree
--     | BLocal Symbol Tree -- A function-local name declaration
--     | BIf Tree Tree Tree
--     | BEval Tree
--     | BEvalIn Tree Tree
--     | BDo (List Tree)
    -- | BMath MathOp (List Tree)

-- -- eval : Tree -> Env -> Either EvalError (Tree, Env)

  
-- data Prim
--    = PrimAdd
--    | PrimVex
--    | PrimFn
-- -- | PrimImport
-- -- | PrimDef
-- -- | PrimDefv
-- -- | PrimDefn
-- -- | PrimSpawn
-- -- | PrimEval

-- data Syntax : Type where
--   SCom : String -> PrefixSpan -> Syntax
--   SInt : Int -> Span -> Syntax
--   SStr : String -> SurroundSpan -> Syntax
--   SSym : Symbol -> Span -> Syntax
--   SQuo : Span -> Syntax
--   -- SDot : Span -> Syntax
--   SList : List Syntax -> SurroundSpan -> Syntax
--   -- SVec  : List Syntax -> SurroundSpan -> Syntax
--   -- SMap  : List Syntax -> SurroundSpan -> Syntax

--   data Literal
--     = LInt Int Span
--     | LStr String SurroundSpan
--     | LSym Symbol Span
--     | LQuo Literal Span
--     -- | LQua Literal Span
--     -- | LVec (List Literal) SurroundSpan
--     -- | LMap (List Literal) SurroundSpan
--     | LList LitList
  

-- -- l1ify : Literal -> L1 -> L1
-- -- l1ify l@(LInt x y) = Const l
-- -- l1ify l@(LStr x y) = Const l
-- -- l1ify (LSym x y) = Lookup x y
-- -- l1ify (LQuo x y) = Const x
-- -- -- l1ify l@(LVec xs x) = ?l_6
-- -- l1ify l@(LList list) =
-- --   case list.values of
-- --     [] => Const l
-- --     y :: _ => l1ify y . Call list
  
-- Env : Type
-- Env = List (List (Symbol, (Datum, Span)))

-- -- The PartEnv represents an environment which has been modified
-- -- during a partial evaluation step. We may have knowledge of values
-- -- that don't exist yet

-- -- record PartEnv : Type where
-- --   env : Env

-- record Vex where
--   closure : Env
--   params : LitList
  
-- data Binding = Anon | Named Symbol

-- boundName : Binding -> Maybe Symbol
-- boundName Anon = Nothing
-- boundName (Named s) = Just s

-- data Bindings
--   = SingleBinding Binding
--   | ListBinding (List Binding)
--   | VarListBinding (List Binding) Binding

-- boundNames : Bindings -> List Symbol
-- boundNames (SingleBinding b) = catMaybes [(boundName b)]
-- boundNames (ListBinding b) = catMaybes (map boundName b)
-- boundNames (VarListBinding b c) = catMaybes (map boundName (c::b))

-- -- record Evaluator where
-- --   constructor Eval
-- --   acc : Datum
-- --   next : L1
-- --   env : Env

-- -- data EvalResult = Partial Evaluator | Done Datum

-- lookup : Symbol -> Env -> Maybe (Datum,Span)
-- lookup name = head' . catMaybes . map (lookup name)

-- -- mutual
-- --   eval : Evaluator -> Either EvalError EvalResult
-- --   eval e =
-- --     case e.next of
-- --       Nil => Right (Done DNil)
-- --       Lookup x y next =>
-- --         case lookup x e.env of
-- --           Just (val, _) => Right (Partial ({ acc := val, next := next } e))
-- --           Nothing => Left (Undef x y)
-- --       Const val next => Right (Partial ({ acc := DLit val, next := next } e))
-- --       Call list next => call e list next
  
--   -- call : Evaluator -> Literal -> L1 -> Either EvalError EvalResult


-- -- evalL1 : L1 -> Evaluator -> Either EvalError Datum
-- -- evalL1 

-- data Name = Simple Symbol | Qualified (List1 Symbol)

-- data MathOp = Plus | Minus | Mul | Div | Mod

-- -- calling a vex calls with the provided params and environment
-- -- when returning from a vex, modifications may be accrued to the
-- -- originating environment

-- data Delta = D

-- data EvalError = Undefined Name

-- -- data Module : Type -> Type where

-- -- data Symbolic = StaticSymbol Sy

-- -- data Partial = 

-- -- data Kind = 

-- record Shadow where

-- data Eager = Known Datum | Unknown


-- record PartEnv where
--   constructor Part
--   static : Env
--   dynamic : List (List Eager)



-- cannotHas : Symbol -> PartEnv -> PartEnv
-- cannotHas name env = { unavailable := name :: env.unavailable } env

-- isTruthy : Datum -> Bool

-- mutual
--   --- The primary objective here is to use the static environment to
--   --- resolve as many symbols as possible, thus giving us a chance of
--   --- ascribing some meaning to the program.
--   partEval : PartEnv -> Tree -> Either EvalError Tree
--   partEval e (Const x) = Right (Const x)
--   partEval e (Call x y) = Right (Call x y)
--   partEval e (CallBuiltin x y) = partEvalBuiltin e x y
--   partEval e (Lookup x) = partEvalLookup e x

--   partEvalLookup : PartEnv -> Symbol -> Either EvalError Tree
--   partEvalLookup e name =
--     -- if the name is unavailable, it's not a free variable so we can't expand it
--     if name `elem` e.unavailable then Right (Lookup name)
--     else case lookup name e.static of
--       Just (val,_) => Right (Const val)
--       Nothing => ?todo 

--   partEvalBuiltin : PartEnv -> Builtin -> LitList -> Either EvalError Tree
--   partEvalBuiltin e (BMath x ys) l = ?b_1
--   partEvalBuiltin e (BLet xs x) l = partEvalLet e xs x l
--   partEvalBuiltin e (BLocal x y) l = partEvalLocal e x y l
--   partEvalBuiltin e (BIf x y z) l = partEvalIf e x y z l
--   partEvalBuiltin e (BEval x) l = partEvalEval e x l
--   partEvalBuiltin e (BEvalIn x y) l = partEvalEvalIn e x y l
--   partEvalBuiltin e (BDo xs) l = ?b_7

--   partEvalLet : PartEnv -> List (Binding, Tree) -> Tree -> LitList -> Either EvalError Tree
--   partEvalLet e bs body l = pel bs [] e where
--     pel : List (Binding, Tree) -> List (Binding, Tree) -> PartEnv -> Either EvalError Tree
--     pel [] [] e = partEval e body -- eliminate
--     pel [] acc e = do -- rebuild
--       body <- partEval e body
--       Right (CallBuiltin (BLet (reverse acc) body) l)
--     pel ((b,x) :: xs) acc e = do -- iterate
--       x <- partEval e x
--       case b of -- if the binding is named, mark it unavailable
--         Anon => pel xs ((b,x)::acc) e
--         Named name => pel xs ((b,x)::acc) (cannotHas name e)

--   partEvalLocal : PartEnv -> Symbol -> Tree -> LitList -> Either EvalError Tree
--   partEvalLocal e name tree l = do
--     tree <- partEval (cannotHas name e) tree
--     Right (CallBuiltin (BLocal name tree) l)

--   partEvalIf : PartEnv -> Tree -> Tree -> Tree -> LitList -> Either EvalError Tree
--   partEvalIf e x y z l = do
--     -- 1. Part evaluate the contents
--     x <- partEval e x
--     y <- partEval e y
--     z <- partEval e z
--     -- 2. If x is now a constant, judge it already and eliminate the branching
--     case x of
--       Const x => if isTruthy x then Right y else Right z
--       _ => Right (CallBuiltin (BIf x y z) l)

--   partEvalEval : PartEnv -> Tree -> LitList -> Either EvalError Tree
--   partEvalEval e x l = do
--     x <- partEval e x
--     Right (CallBuiltin (BEval x) l)

--   partEvalEvalIn : PartEnv -> Tree -> Tree -> LitList -> Either EvalError Tree
--   partEvalEvalIn e x y l = do
--     x <- partEval e x
--     y <- partEval e y
--     Right (CallBuiltin (BEvalIn x y) l)


-- -- data Crisp :

-- -- callPrim : Prim -> LitList -> Env -> Op
-- -- callPrim
 
-- -- data L2 : Type where
-- --   CurrentEnv : L2
--   CallPrim : Prim -> LitList -> L2
--   Const : Datum -> L2 
