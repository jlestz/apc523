subroutine s1(x,y)

  use mpi 
  implicit none 
  
  integer, intent(in) :: x
  integer, intent(out) :: y
  integer :: yloc

  integer :: iproc,nproc,ierr,tag
  integer :: stat(MPI_STATUS_SIZE)

  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)
  tag = 5

  print *, iproc,nproc, "iproc,nproc (s1)"

  yloc = 100 + x + iproc
  y = 0 
  
  print *, iproc, "iproc (s1 pre-bar)"
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  print *, iproc,yloc,y,"iproc,yloc,y, (s1 pre-comm)"
  ! call MPI_ALLREDUCE(yloc,y,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  call MPI_REDUCE(yloc,y,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  ! call MPI_Recv(y,1,MPI_INTEGER,0,tag,MPI_COMM_WORLD,stat,ierr)
  print *, iproc,yloc,y,"iproc,yloc,y, (s1 post-comm)"

  return 
end subroutine s1
