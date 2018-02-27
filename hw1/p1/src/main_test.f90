program main_test 
  
  use mpi
  implicit none

  integer :: ierr,iproc
  integer :: x,y,z

  ! initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)
  
  x = 0
  y = 0 
  z = 0
  
  if (iproc < 1) then
    print *,iproc,"in main calling s1"
    call s1(x,y)
  else if (iproc > 0) then
   print *,iproc,"in main calling s2"
   call s2(x,z)
  end if 

  print *,iproc,y,z,"in main (y,z)"
  
  ! close MPI 
  call MPI_FINALIZE(ierr)
end program main_test
