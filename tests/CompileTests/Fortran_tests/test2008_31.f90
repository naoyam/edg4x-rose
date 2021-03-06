PROGRAM  Cubes
   IMPLICIT   NONE

   INTEGER, PARAMETER :: Iterations = 10
   INTEGER            :: i
   REAL               :: x

   DO i = 1, Iterations
      x = i
      WRITE(*,*)  i, x, intCube(i)
   END DO

! If contains is used then intCube(i) will not have an implicitly defined return type!
contains

INTEGER  FUNCTION  intCube(Number)
   IMPLICIT   NONE

   INTEGER, INTENT(IN) :: Number

   intCube = Number*Number*Number
END FUNCTION  intCube

END PROGRAM  Cubes
