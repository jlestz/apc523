! function for computing pi via Monte Carlo simulations 
! parallelization with MPI 
! Jeff Lestz
! 23 Feb 2018
!
! n is the number of trials to use
! pir is the calcualted "random" approximation to pi 
! note: random walks will converge with sqrt(n)
! so accuracy to 10^n requires 10^2n trials

subroutine pi_mpi(ntrials,pir) 
  use mpi
  
  implicit none 
  integer*8, intent(in) :: ntrials
  real*8, intent(out) :: pir

  integer*8 :: i,mynout,nout
  integer*8 :: u,v,w
  real*8 :: r1,r2
  integer :: ierr,iproc,nproc 

  ! MPI parameters  
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  mynout = 0

  ! initialize random number generator 
  ! note: seeds must be different for each thread
  ! else only doing n/threads unique trials
  call ran1_init(iproc,u,v,w)

  ! stepping by nproc is effective 1D domain decomposition
  do i=iproc,ntrials-1,nproc

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

  ! reduce nout to all procs
  ! (to only calculate pi, a reduce to master would be sufficient, but 
  ! the allreduce is more general since it allows the procs to do further
  ! calculations with the result. For nprocs <= 40, the extra cost of 
  ! communication should be minimal)
  call MPI_ALLREDUCE(mynout,nout,1,MPI_INTEGER8,MPI_SUM,MPI_COMM_WORLD,ierr)

  ! calculate pi by taking the ratio of points inside to outside 
  ! the unit cirle (compare area of circle to square)
  pir = 4*(1 - dble(nout)/dble(ntrials))
  
    return 
end subroutine pi_mpi
