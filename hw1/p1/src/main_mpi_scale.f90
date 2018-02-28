! program to measure scaling with number of mpi threads
! for a fixed problem size in subroutine pi_mpi.f90
! Jeff Lestz 
! 26 Feb 2018 
!
! set the parameters maxwork, minwork, dwork
! to set the number of MPI threads to loop over
! set the parameter ntrials to set the fixed problem size

program main_mpi_scale 
  use mpi
  
  implicit none 

  real*8, parameter :: pi = 3.14159265358979323846
  integer*8, parameter :: ntrials = 10**11
  integer, parameter :: maxwork=40,minwork=20,dwork=2
  
  integer :: nwork
  real*8 :: pi_appx,err
  real*8 :: tbeg,tend

  integer :: ierr,iproc,subproc,subtot
  integer :: comm,color 

  ! initialize MPI 
  call MPI_INIT(ierr) 
  call MPI_COMM_RANK(MPI_COMM_WORLD,iproc,ierr)

  ! run Monte Carlo calucation with nwork mpi threads
  ! fixed problem size: ntrials = 10^11
  ! compare the time elapsed to determine strong scaling 
  do nwork = maxwork, minwork, -dwork

    ! split the pool of workers into two new communicators
    ! the first nwork threads go into color 0, rest into color 1.
    ! then we can simultaneously solve the problem with nwork and maxwork-nwork
    ! threads at once
    ! note: this is a somewhat convoluted solution, however I was 
    ! interested in writing, a solution within MPI for my own experience 
    ! instead of writing a bash/python script to loop over the submission 
    ! of many batch jobs with varying -n argument, which I already knew how to do. 
    if (iproc < nwork) then 
      color = 0 
    else 
      color = 1 
    end if 
    call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,iproc,comm,ierr)
    call MPI_COMM_SIZE(comm,subtot,ierr)
    call MPI_COMM_RANK(comm,subproc,ierr)

    ! have master of each comm begin clock 
    if (subproc < 1) then 
      ! time each call for scaling 
      tbeg = MPI_WTIME()
    end if 
    
    ! run the calculation 
    call pi_mpi(comm,ntrials,pi_appx)
    err = abs(pi_appx - pi)/pi

    ! master of each comm ends clock and prints results to file 
    if (subproc < 1) then 
      tend = MPI_WTIME()
      print *, subtot,err,tend-tbeg
    end if 
  end do 

  ! finalize MPI 
  call MPI_Finalize(ierr)

end program main_mpi_scale
