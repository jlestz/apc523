! function for computing pi via Monte Carlo simulations 
! Jeff Lestz
! 22 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

function pi_mc(n) result(pir)
  implicit none 
  integer, intent(in) :: n
  real*8 :: pir
  
  integer*8 :: i,u,v,w
  real*8 :: r1,r2,nout
  integer, allocatable :: isOut(:)

  ! array specifying if the point is out of the circle or not
  allocate(isOut(n))

  ! initialize random number generator 
  ! call ran1_init(1,u,v,w)

  do i=1,n
    
    ! generate x and y coordinates 
    ! generate random numbers 
    ! call random_number(r1)
    !call random_number(r2)
    
    call ran1(u,v,w,r1)
    call ran1(u,v,w,r2)
    ! convert [0,1] to [-1,1] domain
    r1 = 2*r1 - 1; 
    r2 = 2*r2 - 1; 
    
    ! determine if the point is inside or outside the unit circle 
    ! floor(r^2) = 0 when r^2 < 1 (inside circle),
    ! floor(r^2) = 1 when r^2 > 1 (outside circle)
    ! (floor used instead of if statements to reduce branching
    ! and improve parallelism)
    isOut(i) = floor(r1**2 + r2**2)

    ! print *,i,r1,r2

  end do

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  nout = sum(isOut)
  pir = 4*(1 - nout/n)

  print *,"nout",nout
  print *,"pir",pir

  ! free array memory and return 
  deallocate(isOut)
  return 
end function pi_mc
