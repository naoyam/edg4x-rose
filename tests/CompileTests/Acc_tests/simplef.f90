!simplef.f90

! ACC tests (just syntax)

! (Mixed directives and comments.)

subroutine f0(n, a, b)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(:), b(:)
  integer i
  !comments-before
  !comments-before
  !$ACC KERNELS &
  !$ACC & LOOP
  !comments-after
  !comments-after
  !$omp parallel do
  do i = 1, n
     b(i) = a(i) * 2.0
  end do
  !$omp end parallel do
  !comments-before
  !comments-before
  !$ACC END KERNELS LOOP
  !comments-after
  !comments-after
end subroutine f0

! (Multiple target blocks.)

subroutine f1(n, a, b, c)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(:,:), b(:,:), c(:,:)
  integer :: i, j, k
  real :: t
  !$ACC DATA COPYIN(A,B) COPY(C)
  !$ACC KERNELS
  do j=1,n
     do i=1,n
        c(i,j) = 0.0
     end do
  end do
  do j=1,n
     do i=1,n
        t = 0.0
        do k=1,n
           t = t + a(i,k) * b(k,j)
        end do
        c(i,j) = t
     end do
  end do
  !$ACC END KERNELS
  !$ACC END DATA
end subroutine f1

! (Keywords for variables.)

subroutine f2(n)
  implicit none
  integer, intent(in) :: n
  real :: present_or_copy(1:n), pcopy(1:n)
  integer :: i
  !$ACC DATA COPY (present_or_copy, pcopy)
  !$ACC PARALLEL LOOP
  do i = 1, n
     present_or_copy(i) = pcopy(i) * 2.0
  end do
  !$ACC END DATA
end subroutine f2

! (Many LOOP clauses, and optional END directives.)

subroutine f3(n, a, x, y)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a, x(:), y(:)
  integer :: i
  !$ACC PARALLEL LOOP &
  !$ACC & DEVICE_TYPE(pacs_g) VECTOR_LENGTH(4096) &
  !$ACC &   GANG(NUM:n) WORKER(NUM:1) VECTOR(LENGTH:4096) &
  !$ACC & DEVICE_TYPE(xeonphi) VECTOR_LENGTH(8) &
  !$ACC &   GANG(NUM:n, STATIC:4) WORKER(NUM:60) VECTOR(8) &
  !$ACC & DEVICE_TYPE(ndivia, radeon) VECTOR_LENGTH(256) &
  !$ACC &   GANG(n, STATIC:*) WORKER VECTOR &
  !$ACC & DEVICE_TYPE(*) VECTOR_LENGTH(32) &
  !$ACC & DEFAULT(NONE)
  do i = 1, n
     y(i) = a * x(i) + y(i)
  end do
  !$ACC END PARALLEL LOOP
end subroutine f3

! (Expressions of calls and array references in if clauses.)

subroutine f4(n, a, x, y)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a, x(:), y(:)
  integer :: i
  if (int(log10(real(n))) >= 4) then
     !$ACC PARALLEL LOOP IF(int(log10(real(n))) >= 4)
     do i = 1, n
        y(i) = a * x(i) + y(i)
     end do
  end if
  if (x(1) < 0.0) then
     !$ACC PARALLEL LOOP IF(x(1) < 0.0)
     do i = 1, n
        y(i) = a * x(i) + y(i)
     end do
  end if
end subroutine f4

! (REDUCTION clauses.)

subroutine f5(n, a, x, y)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a, x(:), y(:)
  integer :: i
  !$ACC PARALLEL
  !$ACC LOOP REDUCTION(+:a)
  a = 0.0
  do i = 1, n
     a = a + x(i)
  end do
  !$ACC END PARALLEL
end subroutine f5

! (Subsections.)

subroutine f6(n, a, x, y)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a, x(:,:), y(:)
  !$ACC DATA COPY(x(1:n-1,1:n-1), y(1:n-1))
  !$ACC UPDATE HOST(x(1:n-1,1:n-1))
  call f0(10, x, y)
  !$ACC UPDATE DEVICE(x(1:n-1,1:n-1))
  !$ACC END DATA
end subroutine f6

! (ATOMIC directives.)

subroutine f7(a)
  implicit none
  real, intent(inout) :: a
  real :: x
  !$ACC ATOMIC READ
  x = a
  !$ACC ATOMIC WRITE
  a = x
  !$ACC END ATOMIC
  !$ACC ATOMIC CAPTURE
  x = a
  a = 0.0
  !$ACC END ATOMIC
end subroutine f7

! (ROUTINE directives.)

subroutine f8(n, a, b)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(:), b(:)
  integer i
  !$ACC ROUTINE BIND(foo)
  !$ACC KERNELS LOOP
  !$omp parallel do
  do i = 1, n
     b(i) = a(i) * 2.0
  end do
  !$omp end parallel do
  !$ACC END KERNELS LOOP
end subroutine f8

! (WAIT directives.)

subroutine f9(n, a, b)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(:), b(:)
  integer i
  !$ACC KERNELS LOOP ASYNC(0)
  do i = 1, n
     b(i) = a(i) * 2.0
  end do
  !$ACC WAIT(0)
end subroutine f9

! (Dangling OMP directives.)

! The parser may have difficulity in handling non-ACC directives.  An
! ACC directive makes a block for the body, but then it should
! recognize the end markers of non-ACC directives to put them inside
! the block.

subroutine f10(n, a, b)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(:), b(:)
  integer i, j
  !$ACC KERNELS LOOP
  !$omp parallel do
  do i = 1, n
     b(i) = a(i) * 2.0
  end do
  !$omp end parallel do
  !$omp parallel do
  do i = 1, n
     b(i) = a(i) * 2.0
  end do
  !$omp end parallel do
  do j = 1, n
     !$ACC KERNELS LOOP
     !$omp parallel do
     do i = 1, n
        b(i) = a(i) * 2.0
     end do
     !$omp end parallel do
     !$omp parallel do
     do i = 1, n
        b(i) = a(i) * 2.0
     end do
     !$omp end parallel do
  end do
end subroutine f10

program main
  implicit none
  !$ACC ROUTINE (bar) BIND("foo")
  stop
end program main
