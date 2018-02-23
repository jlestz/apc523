! function for computing pi via Monte Carlo simulations 
! parallelization with MPI 
! Jeff Lestz
! 23 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

function pi_mpi(n) result(pir)
  use mpi
  
  implicit none 
  integer*8, intent(in) :: n
  real*8 :: pir
  
  integer*8 :: i,u,v,w
  real*8 :: r1,r2,mynout,nout
  integer :: ierr,iproc,nproc

  ! Initialize MPI 
  call MPI_INIT(ierr) 
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  ! initialize random number generator 
  ! note: seeds must be different for each thread
  ! else only doing n/threads unique trials
  call ran1_init(iproc,u,v,w)

  ! stepping by nproc is effective 1D domain decomposition
  do i=1,n,nproc
    
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
    mynout = mynout + floor(r1**2 + r2**2)

  end do  

  ! all workers send mynout to master, which sums to nout
  call MPI_REDUCE(mynout,nout,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)

  ! close MPI
  call MPI_FINALIZE(ierr)
  
  ! debug to determine how many trials actually executed 
  print *, i

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  pir = 4*(1 - nout/n)

  return 
end function pi_mpi
