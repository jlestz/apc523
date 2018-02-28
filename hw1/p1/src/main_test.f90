! program for testing how MPI communciation works in function calls 
! Jeff Lestz
! 27 Feb 2018 
!
! designed to be run with 2 threads

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
    print *,iproc,"i calling s0 (main)"
    call s0(x,y)
  else if (iproc > 0) then
   print *,iproc,"i calling s1 (main)"
   call s1(x,z)
  end if 

  print *,iproc,y,z,"i,y,z, (main)"
  
  ! close MPI 
  call MPI_FINALIZE(ierr)
end program main_test
