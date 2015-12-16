{-
Ni!
Ni!
Ni!
-}
module Main

knight : IO ()
knight = putStrLn "Ni!"

knights : Stream (IO ())
knights = repeat knight

main : IO ()
main = for_ (take 3 knights) id
