! subroutine for computing pi via Monte Carlo simulations 
! Jeff Lestz
! 22 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

subroutine pi_omp(n,pir) 
  implicit none 
  integer*8, intent(in) :: n
  real*8, intent(out) :: pir
  
  integer*8 :: i,u,v,w
  integer*8 :: nout,jcount
  real*8 :: r1,r2
  integer :: myid,maxth,nth
  integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS

  ! initialize variables 
  ! jcount is number of iterations done by each proc (for debug)
  jcount = 0
  ! nout is number of points falling outside unit circle
  nout = 0

  !$OMP PARALLEL PRIVATE(myid,i,u,v,w,r1,r2),REDUCTION(+:nout),REDUCTION(+:jcount)
  myid = OMP_GET_THREAD_NUM()

  !if (myid < 1) then 
  !  maxth = OMP_GET_MAX_THREADS()
  !  nth = OMP_GET_NUM_THREADS()
  !  print *, nth,maxth,"nth,maxth (pi_omp)"
  !end if 
  
  ! initialize random number generator 
  ! note: seeds should be different for each thread
  ! else may only do n/threads unique trials. 
  ! although ran1_init uses the system clock to avoid this, 
  ! it would not provide independent random numbers for
  ! all threads initializing with j = 0 for instance
  call ran1_init(myid,u,v,w)

  !$OMP DO
  do i=1,n

    ! for debugging, count iterations done by each proc
    jcount = jcount + 1
    
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

    ! for debugging only 
    ! print *, myid, i,jcount,"id,i,jcount (pi_omp)"
    
  end do  
  !$OMP END DO 
  
  ! debugging
  ! print *, myid, jcount,"id,jcount (pi_omp)"
  
  !$OMP END PARALLEL

  ! debug to determine how many trials actually executed 
  ! print *, jcount,"jcount (pi_omp)"

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  pir = 4*(1 - dble(nout)/dble(n))
  
  ! db print statement 
  ! print *,jcount,n,nout,pir,"jcount,n,nout,pi (pi_omp)"

  return 
end subroutine pi_omp
