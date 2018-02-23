! test function for debugging

function test_fun(n) result(pir)
  implicit none 
  real*8, intent(in) :: n
  real*8 :: pir
  
  pir = 2.0
  return 
end function test_fun
