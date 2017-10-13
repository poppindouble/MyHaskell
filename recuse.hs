incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n  = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq times, Num times) => times -> (b -> b) -> b -> b
applyTimes 0 _ baseValue = baseValue
applyTimes times f baseValue = f (applyTimes (times - 1) f baseValue)

applyTimes' :: (Eq times, Num times) => times -> (b -> b) -> b -> b
applyTimes' 0 _ baseValue = baseValue
applyTimes' times f baseValue = f . applyTimes' (times - 1) f $ baseValue