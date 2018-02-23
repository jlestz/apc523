! function for computing pi via Monte Carlo simulations 
! Jeff Lestz
! 22 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

function pi_omp(n) result(pir)
  implicit none 
  integer*8, intent(in) :: n
  real*8 :: pir
  
  integer*8 :: i,u,v,w
  real*8 :: r1,r2,nout
  integer :: myid, OMP_GET_THREAD_NUM

  !$OMP PARALLEL PRIVATE(myid,i,u,v,w,r1,r2),REDUCTION(+:nout)
  myid = OMP_GET_THREAD_NUM()
  
  ! initialize random number generator 
  ! note: seeds must be different for each thread
  ! else only doing n/threads unique trials
  call ran1_init(myid,u,v,w)

  !$OMP DO
  do i=1,n
    
    ! generate x and y coordinates 
    ! generate random numbers 
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
    nout = nout + floor(r1**2 + r2**2)

  end do  
  !$OMP END DO 
  !$OMP END PARALLEL

  ! debug to determine how many trials actually executed 
  print *, i

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  pir = 4*(1 - nout/n)

  return 
end function pi_omp
