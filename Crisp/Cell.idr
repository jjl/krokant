module Crisp.Cell

import Control.App
import Data.IORef

export
data Cell : (t : Type) -> Type where
  MkCell : IORef t -> Cell t

public export
interface Cells e where
  newCell   : {t : Type} -> t -> App {l} e (Cell t)
  readCell  : {t : Type} -> Cell t -> App {l} e t
  writeCell : {t : Type} -> t -> Cell t -> App {l} e ()

PrimIO e => Cells e where
  newCell t = MkCell <$> primIO (newIORef t)
  readCell (MkCell c) = primIO $ readIORef c
  writeCell t (MkCell c) = primIO $ writeIORef c t

export
modifyCell : {t : Type} -> (Cells e) => (t -> t) -> Cell t -> App e ()
modifyCell f c = do
   v <- (f <$> readCell c)
   writeCell v c
