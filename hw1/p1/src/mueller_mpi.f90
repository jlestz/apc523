program mueller_mpi
  use mpi

  implicit none
  integer, parameter :: WP = kind(1.0d0)
  integer :: n, i
  integer :: nproc, iproc, ierr, tag
  real(WP) :: w, x, sum, pi, mypi, f, a

  ! Function to integrate
  f(a) = 4.0_WP / (1.0_WP + a*a)

  ! Initialize MPI and get rank/size
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  ! Set number of intervals and broadcast value
  if (iproc.eq.0) then
     ! One billion intervals
     n = 1000000000
  end if
  call MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  w = 1.0_WP/real(n,WP)

  ! Integrate using midpoint rule
  sum = 0.0_WP

  do i=1,n,nproc
     x = w*(real(i,WP)-0.5_WP)
     sum = sum + f(x)
  end do

  ! Compute pi
  mypi = w * sum

  !call MPI_REDUCE(mypi,pi,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  call MPI_ALLREDUCE(mypi,pi,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
  !call MPI_SEND(mypi,1,MPI_DOUBLE_PRECISION,0,tag,MPI_COMM_WORLD,ierr)

  ! Print pi
  print*, "The value of pi is ", pi, " on process ", iproc

  ! Finish MPI
  call MPI_FINALIZE(ierr)

end program mueller_mpi
