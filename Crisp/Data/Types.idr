module Crisp.Data.Types

import Crisp.Data.Meta

public export
data Nix = MkNix (Maybe NilMeta)

public export
nil : Nix
nil = MkNix Nothing

public export
record Lst (t : Type) where
  constructor MkList
  values : List t
  meta   : Maybe ListMeta

public export
record Vec (t : Type) where
  constructor MkVec
  values : List t
  meta   : Maybe VecMeta

public export
data Inte = MkInt Int (Maybe IntMeta)

public export
data Str = MkStr String (Maybe StringMeta)

public export
data Symbol = Sym String (Maybe SymbolMeta)

public export
Eq Symbol where
  (Sym x _) == (Sym y _) = x == y

public export
Ord Symbol where
  compare (Sym x _) (Sym y _) = compare x y

public export
data Prim
  = PLet
  | PLambda
  | PVex
  | PIf
  | PApply
  -- | PLocal
  -- | PDef
