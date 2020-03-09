class Equal a where
  equal :: a -> a -> Bool

instance Equal Bool where
  equal True True   = True
  equal False False = True
  equal True False  = False
  equal False True  = False


-- ordering comparision
data OrderD = LTD | EQD | GTD

instance Equal OrderD where
  equal LTD LTD = True
  equal EQD EQD = True
  equal GTD GTD = True
  equal _ _   = False


-- Equal lists depends on Equal elements
instance (Equal a) => Equal [a] where
  equal [] [] = True   -- Empty lists are equal
  equal [] ys = False  -- Lists of unequal size are not equal
  equal xs [] = False
  -- equal x y is only allowed here due to the constraint (Equal a)
  equal (x:xs) (y:ys) = equal x y && equal xs ys

instance (Equal a, Equal b) => Equal (a,b) where
  equal (x0, x1) (y0, y1) = equal x0 y0 && equal x1 y1

main = do
  putStrLn "DSR"
