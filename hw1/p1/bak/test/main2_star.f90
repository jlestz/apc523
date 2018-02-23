program main2 
  
  real*8 :: testin
  real*8 :: mypi
  

  mypi = 0.0
  print *, "mypi pre (0.0)", mypi

  testin = 1.0

  mypi = test_fun(testin)
  print *, "mypi",mypi

end program main2
