!simplef.f90
! ACC tests (just syntax)
! (Mixed directives and comments.)
SUBROUTINE f0(n,a,b)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a(:), b(:)
INTEGER :: i
  !comments-before
  !comments-before
!$acc kernels loop
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
  !comments-before
  !comments-before
  !comments-after
  !comments-after
END SUBROUTINE f0

! (Multiple target blocks.)
SUBROUTINE f1(n,a,b,c)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a(:,:), b(:,:), c(:,:)
INTEGER :: i, j, k
REAL :: t
!$acc data copyin (a, b), copy (c)
!$acc kernels
DO j = 1, n
DO i = 1, n
c(i,j) = 0.0
END DO
END DO
DO j = 1, n
DO i = 1, n
t = 0.0
DO k = 1, n
t = t + a(i,k) * b(k,j)
END DO
c(i,j) = t
END DO
END DO
!$acc end kernels
!$acc end data
END SUBROUTINE f1

! (Keywords for variables.)
SUBROUTINE f2(n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL :: present_or_copy(1:n), pcopy(1:n)
INTEGER :: i
!$acc data copy (present_or_copy, pcopy)
!$acc parallel loop
DO i = 1, n
present_or_copy(i) = pcopy(i) * 2.0
END DO
!$acc end data
END SUBROUTINE f2

! (Many LOOP clauses, and optional END directives.)
SUBROUTINE f3(n,a,x,y)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a, x(:), y(:)
INTEGER :: i
!$acc parallel loop dtype (pacs_g), vector_length (4096), gang (num: n), worker (1), vector (4096), dtype (xeonphi), vector_length (8), gang (num: n, static: 4), worker (60), vector (8), dtype (ndivia, radeon), vector_length (256), gang (num: n, static: *), worker , vector , dtype (*), vector_length (32), default(none) 
DO i = 1, n
y(i) = a * x(i) + y(i)
END DO
END SUBROUTINE f3

! (Expressions of calls and array references in if clauses.)
SUBROUTINE f4(n,a,x,y)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a, x(:), y(:)
INTEGER :: i
IF (int((log10((real(n))))) >= 4) THEN
!$acc parallel loop if (int((log10((real(n))))) >= 4)
DO i = 1, n
y(i) = a * x(i) + y(i)
END DO
END IF
IF (x(1) < 0.0) THEN
!$acc parallel loop if (x(1) < 0.0)
DO i = 1, n
y(i) = a * x(i) + y(i)
END DO
END IF
END SUBROUTINE f4

! (REDUCTION clauses.)
SUBROUTINE f5(n,a,x,y)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a, x(:), y(:)
INTEGER :: i
!$acc parallel
!$acc loop reduction (+ : a)
a = 0.0
DO i = 1, n
a = a + x(i)
END DO
!$acc end parallel
END SUBROUTINE f5

! (Subsections.)
SUBROUTINE f6(n,a,x,y)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a, x(:,:), y(:)
!$acc data copy (x(1:(n - 1),1:(n - 1)), y(1:(n - 1)))
!$acc update host (x(1:(n - 1),1:(n - 1)))
CALL f0(10,x,y)
!$acc update device (x(1:(n - 1),1:(n - 1)))
!$acc end data
END SUBROUTINE f6

! (ATOMIC directives.)
SUBROUTINE f7(a)
IMPLICIT NONE
REAL, INTENT(INOUT) :: a
REAL :: x
!$acc atomic read
x = a
!$acc atomic write
a = x
!$acc end atomic
!$acc atomic capture
x = a
a = 0.0
!$acc end atomic
END SUBROUTINE f7

! (ROUTINE directives.)
SUBROUTINE f8(n,a,b)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a(:), b(:)
INTEGER :: i
!$acc routine device (foo)
!$acc kernels loop
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
END SUBROUTINE f8

! (WAIT directives.)
SUBROUTINE f9(n,a,b)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a(:), b(:)
INTEGER :: i
!$acc kernels loop async (0)
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$acc wait (0)
END SUBROUTINE f9

! (Dangling OMP directives.)
! The parser may have difficulity in handling non-ACC directives.  An
! ACC directive makes a block for the body, but then it should
! recognize the end markers of non-ACC directives to put them inside
! the block.
SUBROUTINE f10(n,a,b)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(INOUT) :: a(:), b(:)
INTEGER :: i, j
!$acc kernels loop
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
DO j = 1, n
!$acc kernels loop
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
!$omp parallel 
!$omp do 
DO i = 1, n
b(i) = a(i) * 2.0
END DO
!$omp end do 
!$omp end parallel 
END DO
END SUBROUTINE f10

PROGRAM main
IMPLICIT NONE
!$acc routine (bar), device ("foo")
STOP 
END PROGRAM main

