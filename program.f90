module bench_helper

  use, intrinsic :: iso_fortran_env, only: real64
  implicit none

  integer, private :: inttmp
  private real64

  integer, parameter :: RNDSEED = 100
  real(real64), parameter :: DISTSCALE = 15.0

! integer, parameter :: nsh = 50
! integer, parameter :: nfock = 10
  integer, parameter :: mxang = 5
  integer, parameter :: shsize(5) = [(inttmp*(inttmp+1)/2, inttmp = 1, 5)]

contains


  subroutine init_rng(iseed)
    integer :: iseed

    integer :: nseed
    integer, allocatable :: seed(:)

    call random_seed(size=nseed)
    allocate(seed(nseed))
    seed = iseed
    call random_seed(put=seed)

  end subroutine


  subroutine init_shells(typsh, ptrbf, nsh)
    integer :: typsh(:), ptrbf(:)

    real, allocatable :: rnbfsh(:)
    integer :: i, nsh

    nsh = ubound(typsh,1)
    allocate(rnbfsh(nsh))

    call random_number(rnbfsh)
    typsh = int(rnbfsh*mxang)+1
    deallocate(rnbfsh)

    ptrbf(1) = 0
    do i = 2, nsh
      ptrbf(i) = ptrbf(i-1) + shsize(typsh(i-1))
    end do

  end subroutine


  subroutine init_d(d)
    real(real64) :: d(:,:,:)
    call random_number(d)
  end subroutine


  subroutine gen_g(g, typsh, scal, i, j, k, l)
    real(real64) :: g(*), scal
    integer :: typsh(:)
    integer :: i, j, k, l
    integer :: n(4), ntot
    n = shsize(typsh([i,j,k,l]))
    ntot = product(n)
    call random_number(g(1:ntot))
    g(1:ntot) = g(1:ntot) * scal
  end subroutine


  subroutine upd_fi(f, d, g, typsh, ptrbf, shi, shj, shk, shl)
    real(real64) :: f(:,:,:), d(:,:,:)
    real(real64), target :: g(:)
    integer :: typsh(:), ptrbf(:)
    integer :: shi, shj, shk, shl
    integer :: i, j, k, l, n
    integer :: i0, j0, k0, l0
    integer :: ni, nj, nk, nl
    integer :: bfi, bfj, bfk, bfl
    real(real64) :: v
    real(real64), pointer :: pg(:,:,:,:)

    i0 = ptrbf(shi)
    j0 = ptrbf(shj)
    k0 = ptrbf(shk)
    l0 = ptrbf(shl)

    ni = shsize(typsh(shi))
    nj = shsize(typsh(shj))
    nk = shsize(typsh(shk))
    nl = shsize(typsh(shl))

    pg(1:nl,1:nk,1:nj,1:ni) =>g(1:)

    do n = 1, ubound(f,3)
    do i = 1, ni
    do j = 1, nj
    do k = 1, nk
    do l = 1, nl
      bfi = i0 + i
      bfj = j0 + j
      bfk = k0 + k
      bfl = l0 + l
      v = pg(l,k,j,i)

    !do n = 1, ubound(f,3)
        f(bfi,bfj,n) = d(bfk,bfl,n) * v
        f(bfk,bfl,n) = d(bfi,bfj,n) * v
        f(bfi,bfk,n) = d(bfj,bfl,n) * v
        f(bfj,bfl,n) = d(bfi,bfk,n) * v
        f(bfi,bfl,n) = d(bfj,bfk,n) * v
        f(bfj,bfk,n) = d(bfi,bfl,n) * v
    end do

    end do
    end do
    end do
    end do

  end subroutine


  subroutine upd_if(f, d, g, typsh, ptrbf, shi, shj, shk, shl)
    real(real64) :: f(:,:,:), d(:,:,:)
    real(real64), target :: g(:)
    integer :: typsh(:), ptrbf(:)
    integer :: shi, shj, shk, shl
    integer :: i, j, k, l, n
    integer :: i0, j0, k0, l0
    integer :: ni, nj, nk, nl
    integer :: bfi, bfj, bfk, bfl
    real(real64) :: v
    real(real64), pointer :: pg(:,:,:,:)

    i0 = ptrbf(shi)
    j0 = ptrbf(shj)
    k0 = ptrbf(shk)
    l0 = ptrbf(shl)

    ni = shsize(typsh(shi))
    nj = shsize(typsh(shj))
    nk = shsize(typsh(shk))
    nl = shsize(typsh(shl))

    pg(1:nl,1:nk,1:nj,1:ni) =>g(1:)

    !do n = 1, ubound(f,3)
    do i = 1, ni
    do j = 1, nj
    do k = 1, nk
    do l = 1, nl
      bfi = i0 + i
      bfj = j0 + j
      bfk = k0 + k
      bfl = l0 + l
      v = pg(l,k,j,i)

    do n = 1, ubound(f,3)
        f(bfi,bfj,n) = d(bfk,bfl,n) * v
        f(bfk,bfl,n) = d(bfi,bfj,n) * v
        f(bfi,bfk,n) = d(bfj,bfl,n) * v
        f(bfj,bfl,n) = d(bfi,bfk,n) * v
        f(bfi,bfl,n) = d(bfj,bfk,n) * v
        f(bfj,bfk,n) = d(bfi,bfl,n) * v
    end do

    end do
    end do
    end do
    end do

  end subroutine


end module


program bench

  use, intrinsic :: iso_fortran_env, only: real64, int64
  use bench_helper

  implicit none

  real(real64) :: g(shsize(mxang)**4)
  real(real64), allocatable :: f(:,:,:)
  real(real64), allocatable :: d(:,:,:)
  integer, allocatable :: typsh(:)
  integer, allocatable :: ptrbf(:)
  integer :: i, j, k, l, lmax, nbf, nbf2
  real(real64) :: scal
  character(1), parameter :: cret = achar(13)

  integer(int64) :: t0, t1, rate

  integer :: ierr, nsh, nfock
  character(20) :: arg1, arg2
! integer :: arg1, arg2

  ! Read the first command-line argument as an integer
  call get_command_argument(1, arg1)
  read(arg1, *, iostat=ierr) nsh
  if (ierr /= 0) then
    write(*,*) "Error: argument 1 must be an integer"
    stop
  end if

  ! Read the second command-line
  ! argument as an integer
  call get_command_argument(2, arg2)
  read(arg2, *, iostat=ierr) nfock
  if (ierr /= 0) then
    write(*,*) "Error: argument 2 must be an integer"
    stop
  end if


  allocate(typsh(nsh), ptrbf(nsh))

  call init_rng(RNDSEED)
  call init_shells(typsh, ptrbf, nsh)

  nbf = ptrbf(nsh) + shsize(typsh(nsh))
  nbf2 = nbf*(nbf+1)/2

  allocate(f(nbf,nbf,nfock))
  allocate(d(nbf,nbf,nfock))

  call init_d(d)
  g = 1

  write(*,'(/,2X,A)') "Benchmark options:"
  write(*,'(A50," = ",i8)') "Number of shells", nsh
  write(*,'(A50," = ",i8)') "Max. angular momentum", mxang
  write(*,'(A50," = ",i8)') "Random seed", RNDSEED
  write(*,'(A50," = ",i8)') "Number of basis functions (randomly generated)", nbf

  call system_clock(t0, rate)

  do i = 1, nsh
    write(*,advance='no',fmt='(A,"current shell:",I4)') cret, i
    do j = 1, i
      do k = 1, i
        lmax = k
        if (k==i) lmax = j
        do l = 1, lmax
            scal = 1.0d0
            !call gen_g(g, typsh, scal, i, j, k, l)
            call upd_fi(f, d, g, typsh, ptrbf, i, j, k, l)
        end do
      end do
    end do
  end do

  call system_clock(t1)
  write(*,'(/A,F12.5)') 'FI elapsed time (s):', (t1-t0)*1.0d0/rate

  call system_clock(t0, rate)

  do i = 1, nsh
    write(*,advance='no',fmt='(A,"current shell:",I4)') cret, i
    do j = 1, i
      do k = 1, i
        lmax = k
        if (k==i) lmax = j
        do l = 1, lmax
            scal = 1.0d0
            !call gen_g(g, typsh, scal, i, j, k, l)
            call upd_if(f, d, g, typsh, ptrbf, i, j, k, l)
        end do
      end do
    end do
  end do

  call system_clock(t1)
  write(*,'(/A,F12.5)') 'IF elapsed time (s):', (t1-t0)*1.0d0/rate

end program
