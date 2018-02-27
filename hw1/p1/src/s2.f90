subroutine s2(x,y)

  use mpi 
  implicit none 
  
  integer, intent(in) :: x
  integer, intent(out) :: y
  integer :: dum,yloc

  integer :: iproc,nproc,ierr

  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)
  
  dum = 5
  yloc = 100 + x + iproc
  y = 0 
  
  print *, iproc,yloc,y,"s2 pre reduce"
  call MPI_ALLREDUCE(yloc,y,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  ! call MPI_REDUCE(yloc,y,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  print *, iproc,yloc,y,"s2 post reduce"

  return 
end subroutine s2
