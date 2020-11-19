lucky :: (Integral a) => a -> String
lucky 7 = "Lucky 7!"
lucky x = "Sorry, not 7. Womp womp"

theProgram = lucky 8

main = putStrLn theProgram
