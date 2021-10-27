module Crisp.App

import public Control.App
import public Crisp.Cell
import Crisp.Data
import Data.Either
import Data.List

--~ overrides a State with an effectful action for the duration of
--~ another effectful action.
public export
locally : (0 tag : _) -> State tag t e => (t -> App e t) -> App e u -> App e u
locally tag xform action = do
  old <- get tag
  (xform old >>= put tag) *> action <* put tag old

--~ Lexicals are a state tag for an `Env Datum`
public export
data Lexicals : Type where

public export
Eval : List (List Error -> Type)
Eval = [Cells, State Lexicals (Env Datum)]

--~ Adds a frame to the current environment for the duration of an action.
public export
scoped : Has Eval e => App e t -> App e t
scoped f = locally Lexicals envPushScope f

--~ Performs an action in the provided environment
public export
inEnv : Has Eval e => Env Datum -> App e t -> App e t
inEnv e c = do
  old <- get Lexicals
  put Lexicals e *> c <* put Lexicals old

public export
lookupLexical : Has Eval e => HasErr CrispError e => Symbol -> App e Datum
lookupLexical name = do
  value <- get Lexicals >>= envLookup name
  case value of
    Just value => pure value
    Nothing => throw $ Undef name

public export
defineLexical : Has Eval e => Symbol -> Datum -> App e ()
defineLexical name value = get Lexicals >>= envDefine name value


public export
parseBinding : (Has Eval e, HasErr CrispError e) => Symbol -> App e Binding
parseBinding s@(Sym sym _) = case sym of
  "_" => pure (Unnamed s)
  "&" => ?invalid
  _ => pure (Named s)

public export
bind : Has Eval e => Binding -> Datum -> App e ()
bind (Named b) val = defineLexical b val
bind _ _ = pure ()

public export
binds : Has Eval e => HasErr CrispError e => Bindings -> Lst Datum -> App e ()
-- binds : Has Eval e =>

