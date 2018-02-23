! ========================================================================================!
!                                                                                         !
! Random number generator from Numerical Recipes, 3rd Ed. (2007)                          !
!   Implementation of the highest quality recommended generator.                          !
!   Adapted by J.F. MacArt (2018) from ran.h (added system_clock to seed)                 !
!                                                                                         !
!   The initialization routine (ran1_init) is called with an integer seed (j) and returns !
!       the initial state vector (u,v,w). The user is responsible for ensuring that the   !
!       state vector is thread-safe, e.g., !$OMP PARALLEL PRIVATE(u,v,w).                 !
!                                                                                         !
!   The member function (ran1) is called with the state vector (u,v,w) and returns the    !
!       next value in the random sequence as a double-precision real number (out) with    !
!       bounds [0,1]. The subroutine can be modified to return a signed integer (x).      !
!                                                                                         !
!   The period of the generator is approx. 3.138E+57.                                     !
!                                                                                         !
! ========================================================================================!
subroutine ran1_init(j,u,v,w)
  implicit none
  integer*8, intent(in) :: j
  integer*8 :: count
  integer*8, intent(inout) :: u, v, w
  real*8 :: tmp

  call SYSTEM_CLOCK(count)
  v = 4101842887655102017_8
  w = 1_8
  u = ieor(j*count,v)
  call ran1(u,v,w,tmp)
  v = u
  call ran1(u,v,w,tmp)
  w = v
  call ran1(u,v,w,tmp)

  return
end subroutine ran1_init


subroutine ran1(u,v,w,out)
  implicit none
  integer*8, intent(inout) :: u, v, w
  integer*8 :: x
  real*8, intent(out) :: out

  u = u * 2862933555777941757_8 + 7046029254386353087_8
  v = ieor(v,ishft(v,-17))
  v = ieor(v,ishft(v, 31))
  v = ieor(v,ishft(v, -8))
  w = 4294957665_8*iand(w, 4294967295_8) + ishft(w,-32)
  x = ieor(u,ishft(u, 21))
  x = ieor(x,ishft(x,-35))
  x = ieor(x,ishft(x,  4))
  x = ieor(x+v,w)
  out = 2.0E+0*abs(5.42101086242752217E-20 * real(x,8))
  
  return
end subroutine ran1