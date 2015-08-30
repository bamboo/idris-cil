{-
Idris
-}
module Main

record Language where
  constructor MkLanguage
  name: String

-- intentionally convoluted to avoid inlining
Idris : IO Language
Idris = pure $ MkLanguage "Idris"

main : IO ()
main = putStrLn $ name !Idris
