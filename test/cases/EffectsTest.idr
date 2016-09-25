{-
Hello world!
-}
import CIL.FFI
import CIL.FFI.Effects
import Effects
import Effect.StdIO

hello : Eff () [STDIO]
hello = putStrLn "Hello world!"

main : CIL_IO ()
main = run hello
