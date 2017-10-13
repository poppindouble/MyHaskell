module Test where
  func = z / x + y
    where x = 7
          y = negate x
          z = y * 10

  func2 = let x = 7
              y = negate x
              z = y * 10
            in z / x + y
  waxOn = x * 5
    where z = 7
          x = y ^ 2
          y = z + 8

  waxOn2 = let z = 7
               x = y ^ 2
               y = z + 8
            in x * 5

  triple x = x * 3
  waxOff x = triple x