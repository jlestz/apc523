program test 
  use mpi
  
  implicit none 
  integer, parameter :: ntrials = 10**6
  integer, parameter :: pi = 3.14159265358979323846
  real*8 :: pir,mypir,err,myerr

  integer :: i
  integer :: mynout,nout
  integer :: jcount,jtot 
  integer*8 :: u,v,w
  real*8 :: r1,r2
  integer :: ierr,iproc,nproc 

  ! MPI parameters  
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  ! variable to count iterations done by each proc 
  jcount = 0 
  mynout = 0

  ! initialize random number generator 
  ! note: seeds must be different for each thread
  ! else only doing n/threads unique trials
  call ran1_init(iproc,u,v,w)

  ! stepping by nproc is effective 1D domain decomposition
  do i=iproc,ntrials,nproc

    ! increment iterations done by this proc
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

  mypir = 4*(1 - dble(mynout)/dble(jcount))
  myerr = abs(pi - mypir)/pi
  print *, iproc,mynout,jcount,myerr,"i,mynout,jcount,myerr"
  
  ! reduce nout (for result) and jtot (for debugging) to master
  call MPI_REDUCE(mynout,nout,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  call MPI_REDUCE(jcount,jtot,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)

  if (iproc < 1) then 
    pir = 4*(1 - dble(nout)/dble(ntrials))
    err = abs(pir - pi)/pi
    print *, nout,jtot,ntrials,"nout,jtot,ntrials"
    print *, pir,err,"pi,err"
  end if 

  call MPI_FINALIZE(ierr)
end program test 
