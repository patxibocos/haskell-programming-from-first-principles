module Chapter27 where

-- 27.14 Chapter Exercises

-- Make the expression bottom

x = undefined
y = "blah"
main = do
  print (snd (x, x `seq` y))