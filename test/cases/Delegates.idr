{-
before thread
in thread
after thread
True
False
>> True
>> False
-}
module Main

import CIL.FFI

Thread : Type
Thread = corlib "System.Threading.Thread"

%inline
invokeThread : String -> Thread -> CIL_IO ()
invokeThread fn = invoke (CILInstance fn)
                         (Thread -> CIL_IO ())

Start : Thread -> CIL_IO ()
Start = invokeThread "Start"

Join : Thread -> CIL_IO ()
Join = invokeThread "Join"

ThreadStartTy : CILTy
ThreadStartTy = corlibTy "System.Threading.ThreadStart"

ThreadStart : Type
ThreadStart = CilFn ThreadStartTy (CIL_IO ())

%inline
BoolToStringTy : CILTy
BoolToStringTy = CILTyGen (corlibTy "System.Func") [CILTyBool, CILTyStr]

BoolToString : Type
BoolToString = CilFn BoolToStringTy (Bool -> String)

%inline
Invoke : BoolToString -> Bool -> CIL_IO String
Invoke = invoke (CILInstanceCustom "Invoke" [CILTyGenParam "0"] (CILTyGenParam "1"))
                (BoolToString -> Bool -> CIL_IO String)

boolToString : Bool -> String
boolToString = show

testBoolToString : BoolToString -> CIL_IO ()
testBoolToString bts =
  for_ [True, False] $ \b =>
    Invoke bts b >>= putStrLn

threadMain : CIL_IO ()
threadMain = putStrLn "in thread"

startThread : CIL_IO () -> CIL_IO Thread
startThread f = do
  ts <- delegate ThreadStartTy (CIL_IO ()) f
  new (ThreadStart -> CIL_IO Thread) ts

main : CIL_IO ()
main = do
  -- And now for something completely different... Threads!
  t <- startThread threadMain
  putStrLn "before thread"
  Start t; Join t
  putStrLn "after thread"

  bts <- delegate BoolToStringTy (Bool -> String) boolToString
  testBoolToString bts

  --        new String ('>', 2)
  prefix <- new (Char -> Int -> CIL_IO String) '>' 2 -- using FFI to avoid inlining into the lambda below
  let lambda = \b => prefix ++ " " ++ show b
  bts <- delegate BoolToStringTy (Bool -> String) lambda
  testBoolToString bts

-- Local Variables:
-- idris-load-packages: ("cil")
-- End:
