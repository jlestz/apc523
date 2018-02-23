program pi
  integer, parameter :: WP = kind(1.0D0)
  integer :: n, i
  real(WP) :: r1,r2,nout,pir
  integer, allocatable :: out(:)
  integer :: u,v,w

  ! number of interations
  n = 10**6

  ! array specifying if the point is out of the circle or not
  allocate(out(n))

  ! initialize random number generator (thanks, Jon)
  call ran1_init(0,u,v,w)

  ! run the Monte Carlo simulation 
  do i=1,n
    ! generate x and y coordinates 
    call ran1(u,v,w,r1)
    r1 = 2*r1 - 1; 
    call ran1(u,v,w,r2)
    r2 = 2*r2 - 1; 
    ! determine if the point is inside or outside the unit circle 
    ! floor(r^2) = 0 when r^2 < 1 (inside circle),
    ! floor(r^2) = 1 when r^2 > 1 (outside circle)
    ! (floor used instead of if statements to reduce branching
    ! and improve parallelism)
    out(i) = floor(r1**2 + r2**2)
  end do

  nout = sum(out)
  pir = 4*(1 - nout/n)

  print *,n,pir

  deallocate(out)

end program pi


