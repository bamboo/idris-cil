module CIL.FFI.Effects

import public CIL.FFI
import public Effects
import Effect.StdIO

%access export

implementation Handler StdIO CIL_IO where
  handle () (PutStr s) k = do putStr' s; k () ()
  handle () GetStr     k = do x <- getLine'; k x ()
  handle () (PutCh c)  k = do Write c; k () ()
  handle () GetCh      k = do x <- Read; k (cast x) ()
