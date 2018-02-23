! test function for debugging

function test_fun(n) result(pir)
  implicit none 
  real, intent(in) :: n
  real :: pir
  
  pir = 2.0
  return 
end function test_fun
