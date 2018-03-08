import Botcleanr

main :: IO()
main = do
  lines <- sequence $ replicate 6 getLine
  let field = readString $ unlines lines
      action = whatToDo field
  case action of
    CLEAN -> print "CLEAN"
    Go dir -> print dir
