! function for computing pi via Monte Carlo simulations 
! parallelization with MPI 
! Jeff Lestz
! 23 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

subroutine pi_mpi(n,pir) 
  use mpi
  
  implicit none 
  integer*8, intent(in) :: n
  real*8, intent(out) :: pir
  real*8 :: mypir 

  integer*8 :: i,jcount,jtot
  integer*8 :: u,v,w
  real*8 :: r1,r2,mynout,nout
  integer :: ierr,iproc,nproc

  ! Initialize MPI 
  ! call MPI_INIT(ierr) 
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  jcount = 0 

  ! initialize random number generator 
  ! note: seeds must be different for each thread
  ! else only doing n/threads unique trials
  call ran1_init(iproc,u,v,w)

  ! stepping by nproc is effective 1D domain decomposition
  do i=1,n,nproc

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
    mynout = mynout + floor(r1**2 + r2**2)

  end do  

  ! all reduce nout (for result) and jtot (for debugging) 
  ! not sure if this is the best way to communicate yet... 
  call MPI_ALLREDUCE(mynout,nout,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
  call MPI_ALLREDUCE(jcount,jtot,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

  ! option to calculate local value of pi instead 
  ! but then really should take a weighted average of values, kind of messy 
  mypir = 4*(1 - nout/jcount)

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  pir = 4*(1 - nout/n)
  
  ! debug to determine how many trials actually executed 
  print *, iproc,jcount,mynout,mypir,"i,j,nout,pi (pi_mpi)"
  if (iproc < 1) then  
    print *, n,jtot,nout,pir,"n,jtot,nout,pi (pi_mpi)"
  end if 
  
  ! close MPI
  ! call MPI_FINALIZE(ierr)

  return 
end subroutine pi_mpi
