module Crisp.Data

import public Crisp.Data.Env
import public Crisp.Data.Meta
import public Crisp.Data.Types

public export
data Binding = Named Symbol | Unnamed Symbol

public export
data Bindings
  = BindSingle Binding
  | BindList (Lst Binding)
  | BindVarList (Lst Binding) Binding

public export
record Vex t where
  constructor MkVex
  call    : Bindings
  env     : Binding
  body    : List t
  closure : Env t

public export
record Lam t where
  constructor MkLam
  name    : Maybe Binding
  params  : Bindings
  body    : List t
  closure : Env t

public export
record Let (t : Type) where
  constructor MkLet
  bindings : List (Binding, t)
  body : List t

public export
record If (t : Type) where
  constructor MkIf
  condition : t
  whenTrue  : t
  whenFalse : t

public export
record Apply (t : Type) where
  constructor MkApply
  what : t
  args : Lst t

mutual
  public export
  data Datum
    = DNil
    | DBool Bool
    | DInt  Inte
    | DStr  Str
    | DPrim Prim
    | DSym  Symbol
    -- | DQ    Datum  (Maybe QuoteMeta) -- quoted
    -- | DQQ   Datum  (Maybe QuoteMeta) -- quasiquoted
    -- | DUQ   Datum  (Maybe QuoteMeta) -- unquoted
    -- | DUS   Datum  (Maybe QuoteMeta) -- unquoted splicing
    | DList (Lst Datum)
    | DVec  (Vec Datum)
    | DVex  (Vex Datum)
    | DLam  (Lam Datum)
    | DEnv  (Env Datum)
    -- | DLies Datum Datum

public export
isTruthy : Datum -> Bool
isTruthy DNil = False
isTruthy (DBool b) = b
isTruthy _ = True

public export
data IfError
  = MissingBranches (Lst Datum)
  | MissingCond (Lst Datum)
  | Max3Args (Lst Datum)

public export
data LetError
  = ExpectedBindingVector (Lst Datum)
  | OddBindings (Vec Datum)
  | BadBinding Datum (Vec Datum)

public export
data CrispError
  = Unimplemented String
  | Undef Symbol
  | NotCallable Datum (Lst Datum)
  | BadCall (Lst Datum)
  | BadIf IfError
  | BadLet LetError
  | Many (List CrispError)

