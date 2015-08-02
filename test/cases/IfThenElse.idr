{-
then
else
-}
module Main

test : Bool -> IO ()
test b =
  putStrLn $
    if b
      then "then"
      else "else"

main : IO ()
main = do
  test True
  test False
