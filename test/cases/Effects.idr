{-
Hello world!
-}
import CIL.FFI
import Effects
import Effect.StdIO

ConsoleTy : CILTy
ConsoleTy = corlibTy "System.Console"

Write : Char -> CIL_IO ()
Write = invoke (CILStatic ConsoleTy "Write") (Char -> CIL_IO ())

Read : CIL_IO Int
Read = invoke (CILStatic ConsoleTy "Read") (CIL_IO Int)

-- TODO: Move to CIL.Effects
implementation Handler StdIO CIL_IO where
  handle () (PutStr s) k = do putStr' s; k () ()
  handle () GetStr     k = do x <- getLine'; k x ()
  handle () (PutCh c)  k = do Write c; k () ()
  handle () GetCh      k = do x <- Read; k (cast x) ()

hello : Eff () [STDIO]
hello = putStrLn "Hello world!"

main : CIL_IO ()
main = run hello
