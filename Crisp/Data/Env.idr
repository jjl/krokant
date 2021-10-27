module Crisp.Data.Env

import Control.App
import Crisp.Data.Types
import Crisp.Cell
import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap

export
data Frame : (0 t : Type) -> Type where
  MkFrame : SortedMap Symbol t -> Frame t

Semigroup (Frame t) where
  (<+>) (MkFrame x) (MkFrame y) = MkFrame (mergeLeft x y)

export
emptyFrame : {0 t : Type} -> Frame t
emptyFrame = MkFrame empty

export
lookupFrame : {0 t : Type} -> Symbol -> Frame t -> Maybe t
lookupFrame name (MkFrame map) = lookup name map

export
defineFrame : {0 t : Type} -> Symbol -> t -> Frame t -> Frame t
defineFrame name value (MkFrame map) = MkFrame (insert name value map)

export
data Scope : (0 t : Type) -> Type where
  MutScope : Cell (Frame t) -> Scope t

export
newScope : {0 t : Type} -> (Cells e) => App e (Scope t)
newScope = MutScope <$> newCell emptyFrame

export
scopeFrame : {0 t : Type} -> (Cells e) => Scope t -> App {l} e (Frame t)
scopeFrame (MutScope cell) = readCell cell

export
scopeLookup : {0 t : Type} -> (Cells e) => Symbol -> Scope t -> App {l} e (Maybe t)
scopeLookup name scope = lookupFrame name <$> scopeFrame scope

export
scopeDefine : {0 t : Type} -> (Cells e) => Symbol -> t -> Scope t -> App e ()
scopeDefine name value (MutScope cell) = modifyCell (defineFrame name value) cell

export
data Env : (0 t : Type) -> Type where
  MkEnv : List1 (Scope t) -> Env t

export
envPushScope : {0 t : Type} -> Cells e => Env t -> App e (Env t)
envPushScope (MkEnv e) = (\s => MkEnv (cons s e)) <$> newScope

export
envCurrentScope : {0 t : Type} -> Env t -> Scope t
envCurrentScope (MkEnv e) = head e

export
envDefine : {0 t : Type} -> Cells e => Symbol -> t -> Env t -> App e ()
envDefine name value env = scopeDefine name value $ envCurrentScope env

export
envLookup : {0 t : Type} -> Cells e => Symbol -> Env t -> App e (Maybe t)
envLookup name (MkEnv env) = el $ toList env
  where
    el : List (Scope t) -> App e (Maybe t)
    el [] = pure Nothing
    el (h :: t) = do
      val <- scopeLookup name h
      case val of
        Just val => pure $ Just val
        Nothing => el t
