! EXAMPLE creating TEST_FPU.EXE using Compaq Visual Fortran -- CVF 6.6
! DF /c LAPACK.F
! DF TEST_FPU LAPACK.OBJ /LINK INTEL.LIB /STACK:8000000
! 128mb Memory is required to avoid paging

! --------------------------------------------------------------------
MODULE kinds
   INTEGER, PARAMETER :: RK8 = SELECTED_REAL_KIND(15, 300)
END MODULE kinds
! --------------------------------------------------------------------
PROGRAM TEST_FPU  ! A number-crunching benchmark using matrix inversion.
USE kinds         ! Implemented by:    David Frank  Dave_Frank@hotmail.com
IMPLICIT NONE     ! Gauss  routine by: Tim Prince   N8TM@aol.com
                  ! Crout  routine by: James Van Buskirk  torsop@ix.netcom.com
                  ! Lapack routine by: Jos Bergervoet bergervo@IAEhv.nl

! - - - local variables - - -
REAL(RK8) :: pool(101,101,1000), pool3(1001,1001) ! random numbers to invert
EQUIVALENCE (pool,pool3)               ! use same pool numbers for test 3,4
REAL(RK8) :: a(101,101), a3(1001,1001)     ! working matrices

REAL(RK8) :: avg_err, dt(4)
INTEGER :: i, n, t(8), clock1, clock2, rate

CHARACTER (LEN=36) :: invert_id(4) = &
                      (/ 'Test1 - Gauss 2000 (101x101) inverts', &
                         'Test2 - Crout 2000 (101x101) inverts', &
                         'Test3 - Crout  2 (1001x1001) inverts', &
                         'Test4 - Lapack 2 (1001x1001) inverts' /)
! - - - - - - - - - - - - - -

WRITE (*,*) ' Benchmark running, hopefully as only ACTIVE task'

CALL DATE_AND_TIME ( values = t )

CALL RANDOM_SEED()               ! set seed to random number based on time
CALL RANDOM_NUMBER(pool)         ! fill pool with random data ( 0. -> 1. )

! - - - begin benchmark - - -

DO n = 1,4

   CALL SYSTEM_CLOCK (clock1,rate)  ! get benchmark (n) start time

   SELECT CASE (n)
   CASE (1:2)
      DO i = 1,1000
         a = pool(:,:,i)         ! get next matrix to invert
         IF (n == 1) THEN
            CALL Gauss (a,101)  ! invert a
            CALL Gauss (a,101)  ! invert a
         ELSE
            CALL Crout (a,101)  ! invert a
            CALL Crout (a,101)  ! invert a
         END IF
      END DO
      avg_err = SUM(ABS(a-pool(:,:,1000)))/(101*101)   ! last matrix error

   CASE (3:4)
      a3 = pool3                 ! get 1001x1001 matrix
      IF (n == 3) THEN
         CALL Crout (a3,1001)     ! invert a3
         CALL Crout (a3,1001)     ! invert a3
      ELSE
         CALL Lapack (a3,1001)    ! invert a3
         CALL Lapack (a3,1001)    ! invert a3
      END IF
      avg_err = SUM(ABS(a3-pool3))/(1001*1001)    ! invert err.

   END SELECT

   CALL SYSTEM_CLOCK (clock2,rate)
   dt(n) = (clock2-clock1)/DBLE(rate)  ! get benchmark (n) elapsed sec.

   WRITE (*,92) invert_id(n), dt(n), ' sec  Err=', avg_err

END DO                         ! for test 1-4

WRITE (*,92) '                             total =',SUM(dt), ' sec'
WRITE (*,*)


91 FORMAT (A,I4,2('/',I2.2))
92 FORMAT (A,F5.1,A,F18.15)

END PROGRAM TEST_FPU

! --------------------------------------------------------------------
SUBROUTINE Gauss (a,n)       ! Invert matrix by Gauss method
! --------------------------------------------------------------------
USE kinds
IMPLICIT NONE

INTEGER :: n
REAL(RK8) :: a(n,n)

! - - - Local Variables - - -
REAL(RK8) :: b(n,n), c, d, temp(n)
INTEGER :: i, j, k, m, imax(1), ipvt(n)
! - - - - - - - - - - - - - -
b = a
ipvt = (/ (i, i = 1, n) /)
    
DO k = 1,n
   imax = MAXLOC(ABS(b(k:n,k)))
   m = k-1+imax(1)

   IF (m /= k) THEN
      ipvt( (/m,k/) ) = ipvt( (/k,m/) )
      b((/m,k/),:) = b((/k,m/),:)
   END IF
   d = 1/b(k,k)

   temp = b(:,k)
   DO j = 1, n
      c = b(k,j)*d
      b(:,j) = b(:,j)-temp*c
      b(k,j) = c
   END DO
   b(:,k) = temp*(-d)
   b(k,k) = d
END DO
a(:,ipvt) = b

END SUBROUTINE Gauss

! -------------------------------------------------------------------
SUBROUTINE Crout (a,n)      ! Invert matrix by Crout method
! -------------------------------------------------------------------
USE kinds
IMPLICIT NONE

INTEGER :: n                ! Order of the matrix
REAL(RK8) :: a(n,n)         ! Matrix to be inverted

INTEGER :: i, j, m, imax(1)      ! Current row & column, max pivot loc
INTEGER :: index(n)              ! Partial pivot record
REAL(RK8) :: b(n,n), temp(n)     ! working arrays, temp

index = (/(i,i=1,n)/)        ! initialize column index

DO j = 1, n        ! Shuffle matrix a -> b
   DO i = 1, j-1
      b(i, j) = a(i, j)
   END DO
   DO i = j, n
      b(i, j) = a(n+1-j, i+1-j)
   END DO
END DO

DO j = 1, n   ! LU decomposition; reciprocals of diagonal elements in L matrix

   DO i = j, n    ! Get current column of L matrix
      b(n-i+j,n+1-i) = b(n-i+j,n+1-i)-DOT_PRODUCT(b(n+1-i:n-i+j-1,n+1-i), b(1:j-1,j))
   END DO

   imax = MAXLOC(ABS( (/ (b(j+i-1,i),i=1,n-j+1) /) ))
   m = imax(1)
   b(j+m-1,m) = 1/b(j+m-1,m)

   IF (m /= n+1-j) THEN   ! Swap biggest element to current pivot position
      index((/j,n+1-m/))     = index((/n+1-m,j/))
      b((/j,n+1-m/),n+2-m:n) = b((/n+1-m,j/),n+2-m:n)
      temp(1:n+1-m)          = b(m:n, m)
      b(m:j-1+m, m)          = b(n+1-j:n, n+1-j)
      b(j+m:n, m)            = b(j, j+1:n+1-m)
      b(n+1-j:n, n+1-j)      = temp(1:j)
      b(j, j+1:n+1-m)        = temp(j+1:n+1-m)
   END IF

   DO i = j+1, n   ! Get current row of U matrix
      b(j,i) = b(n,n+1-j)*(b(j,i)-DOT_PRODUCT(b(n+1-j:n-1,n+1-j),b(1:j-1,i)))
   END DO
END DO

DO j = 1, n-1     ! Invert L matrix
   temp(1) = b(n, n+1-j)
   DO i = j+1, n
      b(n-i+j,n+1-i) = -DOT_PRODUCT(b(n-i+j:n-1,n+1-i),temp(1:i-j))*b(n,n+1-i)
      temp(i-j+1) = b(n-i+j,n+1-i)
   END DO
END DO

DO i = 1, (n+1)/3      ! Reshuffle matrix
   temp(1:n+2-3*i) = b(2*i:n+1-i,i)
   DO j = 2*i, n+1-i
      b(j, i) = b(n+i-j, n+1-j)
   END DO
   DO j = i, n+1-2*i
      b(i+j-1, j) = b(n+1-i, n+2-i-j)
   END DO
   b(n+1-i, i+1:n+2-2*i) = temp(1:n+2-3*i)
END DO

DO i = 1, n-1      ! Invert U matrix
   DO j = i+1, n
      b(i,j) = -b(i,j)-DOT_PRODUCT(temp(1:j-i-1), b(i+1:j-1,j))
      temp(j-i) = b(i,j)
   END DO
END DO

DO i = 1, n-1      ! Multiply inverses in reverse order
   temp(1:n-i) = b(i,i+1:n)
   DO j = 1,i
      b(i,j) = b(i,j)+DOT_PRODUCT(temp(1:n-i),b(i+1:n,j))
   END DO
   DO j = i+1, n
      b(i,j) = DOT_PRODUCT(temp(j-i:n-i),b(j:n,j))
   END DO
END DO

a(:,index) = b    ! output straightened columns of the inverse

END SUBROUTINE Crout

! --------------------------------------------------------------------
SUBROUTINE Lapack (a,n)     ! Invert matrix by Lapack method
! --------------------------------------------------------------------
USE kinds
IMPLICIT NONE

INTEGER   :: n
REAL(RK8) :: a(n,n)

INTEGER :: ipiv(n)
INTEGER :: info, lwork, ILAENV
REAL(RK8), ALLOCATABLE :: work(:)

lwork = n * ILAENV( 1, 'DGETRI', ' ', n, -1, -1, -1 )
ALLOCATE ( work(lwork) )

CALL DGETRF( n, n, a, n, ipiv, info )
CALL DGETRI( n, a, n, ipiv, work, lwork, info )

DEALLOCATE ( work )

END SUBROUTINE Lapack

!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGEMM
   INTERFACE
      SUBROUTINE DGEMM(Transa,Transb,M,N,K,Alpha,A,Lda,B,Ldb,Beta,C,Ldc)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      DOUBLE PRECISION :: Alpha , Beta
      INTEGER :: K , Lda , Ldb , Ldc , M , N
      CHARACTER(1) :: Transa , Transb
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      DOUBLE PRECISION , DIMENSION(Ldc,*) :: C
      INTENT (IN) A , Alpha , B , Beta , K , Lda , Ldb , Ldc , M , N
      INTENT (INOUT) C
      END SUBROUTINE DGEMM
   END INTERFACE
END MODULE S_DGEMM
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGEMV
   INTERFACE
      SUBROUTINE DGEMV(Trans,M,N,Alpha,A,Lda,X,Incx,Beta,Y,Incy)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      DOUBLE PRECISION :: Alpha , Beta
      INTEGER :: Incx , Incy , Lda , M , N
      CHARACTER(1) :: Trans
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X , Y
      INTENT (IN) A , Alpha , Beta , Incx , Incy , Lda , M , N , X
      INTENT (INOUT) Y
      END SUBROUTINE DGEMV
   END INTERFACE
END MODULE S_DGEMV
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGER
   INTERFACE
      SUBROUTINE DGER(M,N,Alpha,X,Incx,Y,Incy,A,Lda)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0
      DOUBLE PRECISION :: Alpha
      INTEGER :: Incx , Incy , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X , Y
      INTENT (IN) Alpha , Incx , Incy , Lda , M , N , X , Y
      INTENT (INOUT) A
      END SUBROUTINE DGER
   END INTERFACE
END MODULE S_DGER
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DSCAL
   INTERFACE
      SUBROUTINE DSCAL(N,Da,Dx,Incx)
      IMPLICIT NONE
      DOUBLE PRECISION :: Da
      INTEGER :: Incx , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx
      INTENT (IN) Da , Incx , N
      INTENT (INOUT) Dx
      END SUBROUTINE DSCAL
   END INTERFACE
END MODULE S_DSCAL
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DSWAP
   INTERFACE
      SUBROUTINE DSWAP(N,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
      INTEGER :: Incx , Incy , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx , Dy
      INTENT (IN) Incx , Incy , N
      INTENT (INOUT) Dx , Dy
      END SUBROUTINE DSWAP
   END INTERFACE
END MODULE S_DSWAP
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DTRMM
   INTERFACE
      SUBROUTINE DTRMM(Side,Uplo,Transa,Diag,M,N,Alpha,A,Lda,B,Ldb)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      DOUBLE PRECISION :: Alpha
      CHARACTER(1) :: Diag , Side , Transa , Uplo
      INTEGER :: Lda , Ldb , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      INTENT (IN) A , Alpha , Lda , Ldb , M , N
      INTENT (INOUT) B
      END SUBROUTINE DTRMM
   END INTERFACE
END MODULE S_DTRMM
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DTRMV
   INTERFACE
      SUBROUTINE DTRMV(Uplo,Trans,Diag,N,A,Lda,X,Incx)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0
      CHARACTER(1) :: Diag , Trans , Uplo
      INTEGER :: Incx , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X
      INTENT (IN) A , Incx , Lda , N
      INTENT (INOUT) X
      END SUBROUTINE DTRMV
   END INTERFACE
END MODULE S_DTRMV
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DTRSM
   INTERFACE
      SUBROUTINE DTRSM(Side,Uplo,Transa,Diag,M,N,Alpha,A,Lda,B,Ldb)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      DOUBLE PRECISION :: Alpha
      CHARACTER(1) :: Diag , Side , Transa , Uplo
      INTEGER :: Lda , Ldb , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      INTENT (IN) A , Alpha , Lda , Ldb , M , N
      INTENT (INOUT) B
      END SUBROUTINE DTRSM
   END INTERFACE
END MODULE S_DTRSM
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_IDAMAX
   INTERFACE
      FUNCTION IDAMAX(N,Dx,Incx)
      IMPLICIT NONE
      INTEGER :: Incx , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx
      INTEGER :: IDAMAX
      INTENT (IN) Dx , Incx , N
      END FUNCTION IDAMAX
   END INTERFACE
END MODULE S_IDAMAX
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_XERBLA
   INTERFACE
      SUBROUTINE XERBLA(Srname,Info)
      IMPLICIT NONE
      INTEGER :: Info
      CHARACTER(6) :: Srname
      INTENT (IN) Info , Srname
      END SUBROUTINE XERBLA
   END INTERFACE
END MODULE S_XERBLA
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGETF2
   INTERFACE
      SUBROUTINE DGETF2(M,N,A,Lda,Ipiv,Info)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      INTEGER :: Info , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (IN) M
      INTENT (OUT) Ipiv
      INTENT (INOUT) Info
      END SUBROUTINE DGETF2
   END INTERFACE
END MODULE S_DGETF2
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGETRF
   INTERFACE
      SUBROUTINE DGETRF(M,N,A,Lda,Ipiv,Info)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0
      INTEGER :: Info , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (INOUT) Info , Ipiv
      END SUBROUTINE DGETRF
   END INTERFACE
END MODULE S_DGETRF
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DGETRI
   INTERFACE
      SUBROUTINE DGETRI(N,A,Lda,Ipiv,Work,Lwork,Info)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0 , ONE = 1.0D+0
      INTEGER :: Info , Lda , Lwork , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      DOUBLE PRECISION , DIMENSION(Lwork) :: Work
      INTENT (IN) Ipiv , Lwork
      INTENT (INOUT) A , Info
      END SUBROUTINE DGETRI
   END INTERFACE
END MODULE S_DGETRI
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DLASWP
   INTERFACE
      SUBROUTINE DLASWP(N,A,Lda,K1,K2,Ipiv,Incx)
      IMPLICIT NONE
      INTEGER :: Incx , K1 , K2 , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (IN) Incx , Ipiv , K1 , K2
      END SUBROUTINE DLASWP
   END INTERFACE
END MODULE S_DLASWP
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DTRTI2
   INTERFACE
      SUBROUTINE DTRTI2(Uplo,Diag,N,A,Lda,Info)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0
      CHARACTER :: Diag , Uplo
      INTEGER :: Info , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTENT (IN) N
      INTENT (INOUT) A , Info
      END SUBROUTINE DTRTI2
   END INTERFACE
END MODULE S_DTRTI2
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_DTRTRI
   INTERFACE
      SUBROUTINE DTRTRI(Uplo,Diag,N,A,Lda,Info)
      IMPLICIT NONE
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
      CHARACTER :: Diag , Uplo
      INTEGER :: Info , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTENT (INOUT) Info
      END SUBROUTINE DTRTRI
   END INTERFACE
END MODULE S_DTRTRI
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_ILAENV
   INTERFACE
      FUNCTION ILAENV(Ispec,Name,Opts,N1,N2,N3,N4)
      IMPLICIT NONE
      INTEGER :: Ispec , N1 , N2 , N3 , N4
      CHARACTER(*) :: Name , Opts
      INTEGER :: ILAENV
      INTENT (IN) Ispec , N1 , N2 , N4 , Name
      END FUNCTION ILAENV
   END INTERFACE
END MODULE S_ILAENV
!*==intfaces.f90  created by SPAG 6.55Dc at 12:01 on  5 Feb 2004
MODULE S_LSAME
   INTERFACE
      FUNCTION LSAME(Ca,Cb)
      IMPLICIT NONE
      CHARACTER :: Ca , Cb
      LOGICAL :: LSAME
      INTENT (IN) Ca , Cb
      END FUNCTION LSAME
   END INTERFACE
END MODULE S_LSAME
!*==DGETF2.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
!****************************************************************************
! LAPACK routines for inversion of a matrix are included below; they are:
!         dgetf2  dgetrf  dgetri  dlaswp  dtrti2  dtrtri  ilaenv  lsame
!
!       The BLAS routines that are called by LAPACK are not included.
!       You can either use standard BLAS (from netlib) or processor-
!       optimized (like INTEL mkl library). The necessary BLAS routines
!       are:
!         dgemm   dger    dswap   dtrmv   idamax
!         dgemv   dscal   dtrmm   dtrsm   xerbla
!
!       J. Bergervoet
!       May, 1998
!****************************************************************************
 
      SUBROUTINE DGETF2(M,N,A,Lda,Ipiv,Info)
      USE S_DGER
      USE S_DSCAL
      USE S_DSWAP
      USE S_IDAMAX
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - M
!A INPUT  - N
!A INPUT  - A
!A INPUT  - LDA
!A OUTPUT - IPIV
!A OUTPUT - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DGER     DSCAL    DSWAP    IDAMAX   XERBLA
! called by   DGETRF
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  J        JP
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      INTEGER :: Info , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (IN) M
      INTENT (OUT) Ipiv
      INTENT (INOUT) Info
!
! Local variables
!
      INTEGER :: j , jp
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     June 30, 1992
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. External Functions ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      Info = 0
      IF ( M<0 ) THEN
         Info = -1
      ELSEIF ( N<0 ) THEN
         Info = -2
      ELSEIF ( Lda<MAX(1,M) ) THEN
         Info = -4
      ENDIF
      IF ( Info/=0 ) THEN
         CALL XERBLA('DGETF2',-Info)
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF ( M==0 .OR. N==0 ) RETURN
!
      DO j = 1 , MIN(M,N)
!
!        Find pivot and test for singularity.
!
         jp = j - 1 + IDAMAX(M-j+1,A(j,j),1)
         Ipiv(j) = jp
         IF ( A(jp,j)/=ZERO ) THEN
!
!           Apply the interchange to columns 1:N.
!
            IF ( jp/=j ) CALL DSWAP(N,A(j,1),Lda,A(jp,1),Lda)
!
!           Compute elements J+1:M of J-th column.
!
            IF ( j<M ) CALL DSCAL(M-j,ONE/A(j,j),A(j+1,j),1)
!
         ELSEIF ( Info==0 ) THEN
!
            Info = j
         ENDIF
!
!
!           Update trailing submatrix.
!
         IF ( j<MIN(M,N) ) CALL DGER(M-j,N-j,-ONE,A(j+1,j),1,A(j,j+1),  &
     &                               Lda,A(j+1,j+1),Lda)
      ENDDO
!
!     End of DGETF2
!
      END SUBROUTINE DGETF2
!*==DGETRF.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
 
 
      SUBROUTINE DGETRF(M,N,A,Lda,Ipiv,Info)
      USE S_DGEMM
      USE S_DGETF2
      USE S_DLASWP
      USE S_DTRSM
      USE S_ILAENV
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - M
!A INPUT  - N
!A PASSED - A
!A INPUT  - LDA
!A OUTPUT - IPIV
!A OUTPUT - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DGEMM    DGETF2   DLASWP   DTRSM    ILAENV   XERBLA
! called by   ** NOTHING **
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        IINFO    J        JB       NB
! uses PARAMs ONE
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0
!
! Dummy arguments
!
      INTEGER :: Info , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (INOUT) Info , Ipiv
!
! Local variables
!
      INTEGER :: i , iinfo , j , jb , nb
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
!     .. Parameters ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. External Functions ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      Info = 0
      IF ( M<0 ) THEN
         Info = -1
      ELSEIF ( N<0 ) THEN
         Info = -2
      ELSEIF ( Lda<MAX(1,M) ) THEN
         Info = -4
      ENDIF
      IF ( Info/=0 ) THEN
         CALL XERBLA('DGETRF',-Info)
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF ( M==0 .OR. N==0 ) RETURN
!
!     Determine the block size for this environment.
!
      nb = ILAENV(1,'DGETRF',' ',M,N,-1,-1)
      IF ( nb<=1 .OR. nb>=MIN(M,N) ) THEN
!
!        Use unblocked code.
!
         CALL DGETF2(M,N,A,Lda,Ipiv,Info)
      ELSE
!
!        Use blocked code.
!
         DO j = 1 , MIN(M,N) , nb
            jb = MIN(MIN(M,N)-j+1,nb)
!
!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
            CALL DGETF2(M-j+1,jb,A(j,j),Lda,Ipiv(j),iinfo)
!
!           Adjust INFO and the pivot indices.
!
            IF ( Info==0 .AND. iinfo>0 ) Info = iinfo + j - 1
            DO i = j , MIN(M,j+jb-1)
               Ipiv(i) = j - 1 + Ipiv(i)
            ENDDO
!
!           Apply interchanges to columns 1:J-1.
!
            CALL DLASWP(j-1,A,Lda,j,j+jb-1,Ipiv,1)
!
            IF ( j+jb<=N ) THEN
!
!              Apply interchanges to columns J+JB:N.
!
               CALL DLASWP(N-j-jb+1,A(1,j+jb),Lda,j,j+jb-1,Ipiv,1)
!
!              Compute block row of U.
!
               CALL DTRSM('Left','Lower','No transpose','Unit',jb,      &
     &                    N-j-jb+1,ONE,A(j,j),Lda,A(j,j+jb),Lda)
!
!                 Update trailing submatrix.
!
               IF ( j+jb<=M ) CALL DGEMM('No transpose','No transpose', &
     &              M-j-jb+1,N-j-jb+1,jb,-ONE,A(j+jb,j),Lda,A(j,j+jb),  &
     &              Lda,ONE,A(j+jb,j+jb),Lda)
            ENDIF
         ENDDO
      ENDIF
!
!     End of DGETRF
!
      END SUBROUTINE DGETRF
!*==DGETRI.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
 
 
      SUBROUTINE DGETRI(N,A,Lda,Ipiv,Work,Lwork,Info)
      USE S_DGEMM
      USE S_DGEMV
      USE S_DSWAP
      USE S_DTRSM
      USE S_DTRTRI
      USE S_ILAENV
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - N
!A OUTPUT - A
!A INPUT  - LDA
!A INPUT  - IPIV
!A OUTPUT - WORK
!A INPUT  - LWORK
!A OUTPUT - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DGEMM    DGEMV    DSWAP    DTRSM    DTRTRI   ILAENV
!             XERBLA
! called by   ** NOTHING **
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        IWS      J        JB       JJ       JP
!             LDWORK   NB       NBMIN    NN
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0 , ONE = 1.0D+0
!
! Dummy arguments
!
      INTEGER :: Info , Lda , Lwork , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      DOUBLE PRECISION , DIMENSION(Lwork) :: Work
      INTENT (IN) Ipiv , Lwork
      INTENT (INOUT) A , Info
!
! Local variables
!
      INTEGER :: i , iws , j , jb , jj , jp , ldwork , nb , nbmin , nn
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGETRI computes the inverse of a matrix using the LU factorization
!  computed by DGETRF.
!
!  This method inverts U and then computes inv(A) by solving the system
!  inv(A)*L = inv(U) for inv(A).
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the factors L and U from the factorization
!          A = P*L*U as computed by DGETRF.
!          On exit, if INFO = 0, the inverse of the original matrix A.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from DGETRF; for 1<=i<=N, row i of the
!          matrix was interchanged with row IPIV(i).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
!          On exit, if INFO=0, then WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= max(1,N).
!          For optimal performance LWORK >= N*NB, where NB is
!          the optimal blocksize returned by ILAENV.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
!                singular and its inverse could not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. External Functions ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      Info = 0
      Work(1) = MAX(N,1)
      IF ( N<0 ) THEN
         Info = -1
      ELSEIF ( Lda<MAX(1,N) ) THEN
         Info = -3
      ELSEIF ( Lwork<MAX(1,N) ) THEN
         Info = -6
      ENDIF
      IF ( Info/=0 ) THEN
         CALL XERBLA('DGETRI',-Info)
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF ( N==0 ) RETURN
!
!     Form inv(U).  If INFO > 0 from DTRTRI, then U is singular,
!     and the inverse is not computed.
!
      CALL DTRTRI('Upper','Non-unit',N,A,Lda,Info)
      IF ( Info>0 ) RETURN
!
!     Determine the block size for this environment.
!
      nb = ILAENV(1,'DGETRI',' ',N,-1,-1,-1)
      nbmin = 2
      ldwork = N
      IF ( nb>1 .AND. nb<N ) THEN
         iws = MAX(ldwork*nb,1)
         IF ( Lwork<iws ) THEN
            nb = Lwork/ldwork
            nbmin = MAX(2,ILAENV(2,'DGETRI',' ',N,-1,-1,-1))
         ENDIF
      ELSE
         iws = N
      ENDIF
!
!     Solve the equation inv(A)*L = inv(U) for inv(A).
!
      IF ( nb<nbmin .OR. nb>=N ) THEN
!
!        Use unblocked code.
!
         DO j = N , 1 , -1
!
!           Copy current column of L to WORK and replace with zeros.
!
            DO i = j + 1 , N
               Work(i) = A(i,j)
               A(i,j) = ZERO
            ENDDO
!
!           Compute current column of inv(A).
!
            IF ( j<N ) CALL DGEMV('No transpose',N,N-j,-ONE,A(1,j+1),   &
     &                            Lda,Work(j+1),1,ONE,A(1,j),1)
         ENDDO
      ELSE
!
!        Use blocked code.
!
         nn = ((N-1)/nb)*nb + 1
         DO j = nn , 1 , -nb
            jb = MIN(nb,N-j+1)
!
!           Copy current block column of L to WORK and replace with
!           zeros.
!
            DO jj = j , j + jb - 1
               DO i = jj + 1 , N
                  Work(i+(jj-j)*ldwork) = A(i,jj)
                  A(i,jj) = ZERO
               ENDDO
            ENDDO
!
!           Compute current block column of inv(A).
!
            IF ( j+jb<=N ) CALL DGEMM('No transpose','No transpose',N,  &
     &                                jb,N-j-jb+1,-ONE,A(1,j+jb),Lda,   &
     &                                Work(j+jb),ldwork,ONE,A(1,j),Lda)
            CALL DTRSM('Right','Lower','No transpose','Unit',N,jb,ONE,  &
     &                 Work(j),ldwork,A(1,j),Lda)
         ENDDO
      ENDIF
!
!     Apply column interchanges.
!
      DO j = N - 1 , 1 , -1
         jp = Ipiv(j)
         IF ( jp/=j ) CALL DSWAP(N,A(1,j),1,A(1,jp),1)
      ENDDO
!
      Work(1) = iws
!
!     End of DGETRI
!
      END SUBROUTINE DGETRI
!*==DLASWP.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
 
 
      SUBROUTINE DLASWP(N,A,Lda,K1,K2,Ipiv,Incx)
      USE S_DSWAP
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - N
!A PASSED - A
!A INPUT  - LDA
!A INPUT  - K1
!A INPUT  - K2
!A INPUT  - IPIV
!A INPUT  - INCX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DSWAP
! called by   DGETRF
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        IP       IX
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Incx , K1 , K2 , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTEGER , DIMENSION(*) :: Ipiv
      INTENT (IN) Incx , Ipiv , K1 , K2
!
! Local variables
!
      INTEGER :: i , ip , ix
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     October 31, 1992
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!
!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.
!
!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.
!
!  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.
!
!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.
!
! =====================================================================
!
!     .. Local Scalars ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. Executable Statements ..
!
!     Interchange row I with row IPIV(I) for each of rows K1 through K2.
!
      IF ( Incx==0 ) RETURN
      IF ( Incx>0 ) THEN
         ix = K1
      ELSE
         ix = 1 + (1-K2)*Incx
      ENDIF
      IF ( Incx==1 ) THEN
         DO i = K1 , K2
            ip = Ipiv(i)
            IF ( ip/=i ) CALL DSWAP(N,A(i,1),Lda,A(ip,1),Lda)
         ENDDO
      ELSEIF ( Incx>1 ) THEN
         DO i = K1 , K2
            ip = Ipiv(ix)
            IF ( ip/=i ) CALL DSWAP(N,A(i,1),Lda,A(ip,1),Lda)
            ix = ix + Incx
         ENDDO
      ELSEIF ( Incx<0 ) THEN
         DO i = K2 , K1 , -1
            ip = Ipiv(ix)
            IF ( ip/=i ) CALL DSWAP(N,A(i,1),Lda,A(ip,1),Lda)
            ix = ix + Incx
         ENDDO
      ENDIF
!
!
!     End of DLASWP
!
      END SUBROUTINE DLASWP
!*==DTRTI2.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
 
 
      SUBROUTINE DTRTI2(Uplo,Diag,N,A,Lda,Info)
      USE S_DSCAL
      USE S_DTRMV
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - UPLO
!A PASSED - DIAG
!A INPUT  - N
!A OUTPUT - A
!A INPUT  - LDA
!A OUTPUT - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DSCAL    DTRMV    LSAME    XERBLA
! called by   DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  AJJ      J        NOUNIT   UPPER
! uses PARAMs ONE
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0
!
! Dummy arguments
!
      CHARACTER :: Diag , Uplo
      INTEGER :: Info , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTENT (IN) N
      INTENT (INOUT) A , Info
!
! Local variables
!
      DOUBLE PRECISION :: ajj
      INTEGER :: j
      LOGICAL :: nounit , upper
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DTRTI2 computes the inverse of a real upper or lower triangular
!  matrix.
!
!  This is the Level 2 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the matrix A is upper or lower triangular.
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  DIAG    (input) CHARACTER*1
!          Specifies whether or not the matrix A is unit triangular.
!          = 'N':  Non-unit triangular
!          = 'U':  Unit triangular
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading n by n upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n by n lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. External Functions ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      Info = 0
      upper = LSAME(Uplo,'U')
      nounit = LSAME(Diag,'N')
      IF ( .NOT.upper .AND. .NOT.LSAME(Uplo,'L') ) THEN
         Info = -1
      ELSEIF ( .NOT.nounit .AND. .NOT.LSAME(Diag,'U') ) THEN
         Info = -2
      ELSEIF ( N<0 ) THEN
         Info = -3
      ELSEIF ( Lda<MAX(1,N) ) THEN
         Info = -5
      ENDIF
      IF ( Info/=0 ) THEN
         CALL XERBLA('DTRTI2',-Info)
         RETURN
      ENDIF
!
      IF ( upper ) THEN
!
!        Compute inverse of upper triangular matrix.
!
         DO j = 1 , N
            IF ( nounit ) THEN
               A(j,j) = ONE/A(j,j)
               ajj = -A(j,j)
            ELSE
               ajj = -ONE
            ENDIF
!
!           Compute elements 1:j-1 of j-th column.
!
            CALL DTRMV('Upper','No transpose',Diag,j-1,A,Lda,A(1,j),1)
            CALL DSCAL(j-1,ajj,A(1,j),1)
         ENDDO
      ELSE
!
!        Compute inverse of lower triangular matrix.
!
         DO j = N , 1 , -1
            IF ( nounit ) THEN
               A(j,j) = ONE/A(j,j)
               ajj = -A(j,j)
            ELSE
               ajj = -ONE
            ENDIF
            IF ( j<N ) THEN
!
!              Compute elements j+1:n of j-th column.
!
               CALL DTRMV('Lower','No transpose',Diag,N-j,A(j+1,j+1),   &
     &                    Lda,A(j+1,j),1)
               CALL DSCAL(N-j,ajj,A(j+1,j),1)
            ENDIF
         ENDDO
      ENDIF
!
!
!     End of DTRTI2
!
      END SUBROUTINE DTRTI2
!*==DTRTRI.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
 
 
      SUBROUTINE DTRTRI(Uplo,Diag,N,A,Lda,Info)
      USE S_DTRMM
      USE S_DTRSM
      USE S_DTRTI2
      USE S_ILAENV
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - UPLO
!A INPUT  - DIAG
!A INPUT  - N
!A INPUT  - A
!A INPUT  - LDA
!A OUTPUT - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       DTRMM    DTRSM    DTRTI2   ILAENV   LSAME    XERBLA
! called by   DGETRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  J        JB       NB       NN       NOUNIT   UPPER
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      CHARACTER :: Diag , Uplo
      INTEGER :: Info , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      INTENT (INOUT) Info
!
! Local variables
!
      INTEGER :: j , jb , nb , nn
      LOGICAL :: nounit , upper
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     March 31, 1993
!
!     .. Scalar Arguments ..
!     ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DTRTRI computes the inverse of a real upper or lower triangular
!  matrix A.
!
!  This is the Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  A is upper triangular;
!          = 'L':  A is lower triangular.
!
!  DIAG    (input) CHARACTER*1
!          = 'N':  A is non-unit triangular;
!          = 'U':  A is unit triangular.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the triangular matrix A.  If UPLO = 'U', the
!          leading N-by-N upper triangular part of the array A contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading N-by-N lower triangular part of the array A contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of A is not referenced.  If DIAG = 'U', the
!          diagonal elements of A are also not referenced and are
!          assumed to be 1.
!          On exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
!               matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!     .. Parameters ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. External Functions ..
!     ..
!     .. External Subroutines ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      Info = 0
      upper = LSAME(Uplo,'U')
      nounit = LSAME(Diag,'N')
      IF ( .NOT.upper .AND. .NOT.LSAME(Uplo,'L') ) THEN
         Info = -1
      ELSEIF ( .NOT.nounit .AND. .NOT.LSAME(Diag,'U') ) THEN
         Info = -2
      ELSEIF ( N<0 ) THEN
         Info = -3
      ELSEIF ( Lda<MAX(1,N) ) THEN
         Info = -5
      ENDIF
      IF ( Info/=0 ) THEN
         CALL XERBLA('DTRTRI',-Info)
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF ( N==0 ) RETURN
!
!     Check for singularity if non-unit.
!
      IF ( nounit ) THEN
         DO Info = 1 , N
            IF ( A(Info,Info)==ZERO ) RETURN
         ENDDO
         Info = 0
      ENDIF
!
!     Determine the block size for this environment.
!
      nb = ILAENV(1,'DTRTRI',Uplo//Diag,N,-1,-1,-1)
      IF ( nb<=1 .OR. nb>=N ) THEN
!
!        Use unblocked code
!
         CALL DTRTI2(Uplo,Diag,N,A,Lda,Info)
!
!        Use blocked code
!
      ELSEIF ( upper ) THEN
!
!           Compute inverse of upper triangular matrix
!
         DO j = 1 , N , nb
            jb = MIN(nb,N-j+1)
!
!              Compute rows 1:j-1 of current block column
!
            CALL DTRMM('Left','Upper','No transpose',Diag,j-1,jb,ONE,A, &
     &                 Lda,A(1,j),Lda)
            CALL DTRSM('Right','Upper','No transpose',Diag,j-1,jb,-ONE, &
     &                 A(j,j),Lda,A(1,j),Lda)
!
!              Compute inverse of current diagonal block
!
            CALL DTRTI2('Upper',Diag,jb,A(j,j),Lda,Info)
         ENDDO
      ELSE
!
!           Compute inverse of lower triangular matrix
!
         nn = ((N-1)/nb)*nb + 1
         DO j = nn , 1 , -nb
            jb = MIN(nb,N-j+1)
            IF ( j+jb<=N ) THEN
!
!                 Compute rows j+jb:n of current block column
!
               CALL DTRMM('Left','Lower','No transpose',Diag,N-j-jb+1,  &
     &                    jb,ONE,A(j+jb,j+jb),Lda,A(j+jb,j),Lda)
               CALL DTRSM('Right','Lower','No transpose',Diag,N-j-jb+1, &
     &                    jb,-ONE,A(j,j),Lda,A(j+jb,j),Lda)
            ENDIF
!
!              Compute inverse of current diagonal block
!
            CALL DTRTI2('Lower',Diag,jb,A(j,j),Lda,Info)
         ENDDO
      ENDIF
!
!
!     End of DTRTRI
!
      END SUBROUTINE DTRTRI
!*==ILAENV.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      FUNCTION ILAENV(Ispec,Name,Opts,N1,N2,N3,N4)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - ISPEC
!A INPUT  - NAME
!A UNUSED - OPTS
!A INPUT  - N1
!A INPUT  - N2
!A UNUSED - N3
!A INPUT  - N4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGETRF   DGETRI   DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  C1       C2       C3       C4       CNAME    I        IC
!             IZ       NB       NBMIN    NX       SNAME    SUBNAM
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Ispec , N1 , N2 , N3 , N4
      CHARACTER(*) :: Name , Opts
      INTEGER :: ILAENV
      INTENT (IN) Ispec , N1 , N2 , N4 , Name
!
! Local variables
!
      CHARACTER(1) :: c1
      CHARACTER(2) :: c2 , c4
      CHARACTER(3) :: c3
      LOGICAL :: cname , sname
      INTEGER :: i , ic , iz , nb , nbmin , nx
      CHARACTER(6) :: subnam
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  ILAENV is called from the LAPACK routines to choose problem-dependent
!  parameters for the local environment.  See ISPEC for a description of
!  the parameters.
!
!  This version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  Users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.
!
!  This routine will not function correctly if it is converted to all
!  lower case.  Converting it to all upper case is allowed.
!
!  Arguments
!  =========
!
!  ISPEC   (input) INTEGER
!          Specifies the parameter to be returned as the value of
!          ILAENV.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for N less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!          = 6: the crossover point for the SVD (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a QR factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift QR and QZ methods
!               for nonsymmetric eigenvalue problems.
!
!  NAME    (input) CHARACTER*(*)
!          The name of the calling subroutine, in either upper case or
!          lower case.
!
!  OPTS    (input) CHARACTER*(*)
!          The character options to the subroutine NAME, concatenated
!          into a single character string.  For example, UPLO = 'U',
!          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!          be specified as OPTS = 'UTN'.
!
!  N1      (input) INTEGER
!  N2      (input) INTEGER
!  N3      (input) INTEGER
!  N4      (input) INTEGER
!          Problem dimensions for the subroutine NAME; these may not all
!          be required.
!
! (ILAENV) (output) INTEGER
!          >= 0: the value of the parameter specified by ISPEC
!          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  The following conventions have been used when calling ILAENV from the
!  LAPACK routines:
!  1)  OPTS is a concatenation of all of the character options to
!      subroutine NAME, in the same order that they appear in the
!      argument list for NAME, even if they are not used in determining
!      the value of the parameter specified by ISPEC.
!  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!      that they appear in the argument list for NAME.  N1 is used
!      first, N2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  The parameter value returned by ILAENV is checked for validity in
!      the calling subroutine.  For example, ILAENV is used to retrieve
!      the optimal blocksize for STRTRI as follows:
!
!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )
!
!  =====================================================================
!
!     .. Local Scalars ..
!     ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
      SELECT CASE (Ispec)
      CASE (1,2,3)
!
!
!     Convert NAME to upper case if the first character is lower case.
!
         ILAENV = 1
         subnam = Name
         ic = ICHAR(subnam(1:1))
         iz = ICHAR('Z')
         IF ( iz==90 .OR. iz==122 ) THEN
!
!        ASCII character set
!
            IF ( ic>=97 .AND. ic<=122 ) THEN
               subnam(1:1) = CHAR(ic-32)
               DO i = 2 , 6
                  ic = ICHAR(subnam(i:i))
                  IF ( ic>=97 .AND. ic<=122 ) subnam(i:i) = CHAR(ic-32)
               ENDDO
            ENDIF
!
         ELSEIF ( iz==233 .OR. iz==169 ) THEN
!
!        EBCDIC character set
!
            IF ( (ic>=129 .AND. ic<=137) .OR. (ic>=145 .AND. ic<=153)   &
     &           .OR. (ic>=162 .AND. ic<=169) ) THEN
               subnam(1:1) = CHAR(ic+64)
               DO i = 2 , 6
                  ic = ICHAR(subnam(i:i))
                  IF ( (ic>=129 .AND. ic<=137) .OR.                     &
     &                 (ic>=145 .AND. ic<=153) .OR.                     &
     &                 (ic>=162 .AND. ic<=169) ) subnam(i:i)            &
     &                 = CHAR(ic+64)
               ENDDO
            ENDIF
!
         ELSEIF ( iz==218 .OR. iz==250 ) THEN
!
!        Prime machines:  ASCII+128
!
            IF ( ic>=225 .AND. ic<=250 ) THEN
               subnam(1:1) = CHAR(ic-32)
               DO i = 2 , 6
                  ic = ICHAR(subnam(i:i))
                  IF ( ic>=225 .AND. ic<=250 ) subnam(i:i) = CHAR(ic-32)
               ENDDO
            ENDIF
         ENDIF
!
         c1 = subnam(1:1)
         sname = c1=='S' .OR. c1=='D'
         cname = c1=='C' .OR. c1=='Z'
         IF ( .NOT.(cname .OR. sname) ) RETURN
         c2 = subnam(2:3)
         c3 = subnam(4:6)
         c4 = c3(2:3)
!
         SELECT CASE (Ispec)
         CASE (2)
!
!
!     ISPEC = 2:  minimum block size
!
            nbmin = 2
            IF ( c2=='GE' ) THEN
               IF ( c3=='QRF' .OR. c3=='RQF' .OR. c3=='LQF' .OR.        &
     &              c3=='QLF' ) THEN
                  IF ( sname ) THEN
                     nbmin = 2
                  ELSE
                     nbmin = 2
                  ENDIF
               ELSEIF ( c3=='HRD' ) THEN
                  IF ( sname ) THEN
                     nbmin = 2
                  ELSE
                     nbmin = 2
                  ENDIF
               ELSEIF ( c3=='BRD' ) THEN
                  IF ( sname ) THEN
                     nbmin = 2
                  ELSE
                     nbmin = 2
                  ENDIF
               ELSEIF ( c3=='TRI' ) THEN
                  IF ( sname ) THEN
                     nbmin = 2
                  ELSE
                     nbmin = 2
                  ENDIF
               ENDIF
            ELSEIF ( c2=='SY' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     nbmin = 8
                  ELSE
                     nbmin = 8
                  ENDIF
               ELSEIF ( sname .AND. c3=='TRD' ) THEN
                  nbmin = 2
               ENDIF
            ELSEIF ( cname .AND. c2=='HE' ) THEN
               IF ( c3=='TRD' ) nbmin = 2
            ELSEIF ( sname .AND. c2=='OR' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nbmin = 2
               ELSEIF ( c3(1:1)=='M' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nbmin = 2
               ENDIF
            ELSEIF ( cname .AND. c2=='UN' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nbmin = 2
               ELSEIF ( c3(1:1)=='M' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nbmin = 2
               ENDIF
            ENDIF
            ILAENV = nbmin
            RETURN
         CASE (3)
!
!
!     ISPEC = 3:  crossover point
!
            nx = 0
            IF ( c2=='GE' ) THEN
               IF ( c3=='QRF' .OR. c3=='RQF' .OR. c3=='LQF' .OR.        &
     &              c3=='QLF' ) THEN
                  IF ( sname ) THEN
                     nx = 128
                  ELSE
                     nx = 128
                  ENDIF
               ELSEIF ( c3=='HRD' ) THEN
                  IF ( sname ) THEN
                     nx = 128
                  ELSE
                     nx = 128
                  ENDIF
               ELSEIF ( c3=='BRD' ) THEN
                  IF ( sname ) THEN
                     nx = 128
                  ELSE
                     nx = 128
                  ENDIF
               ENDIF
            ELSEIF ( c2=='SY' ) THEN
               IF ( sname .AND. c3=='TRD' ) nx = 1
            ELSEIF ( cname .AND. c2=='HE' ) THEN
               IF ( c3=='TRD' ) nx = 1
            ELSEIF ( sname .AND. c2=='OR' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nx = 128
               ENDIF
            ELSEIF ( cname .AND. c2=='UN' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nx = 128
               ENDIF
            ENDIF
            ILAENV = nx
            RETURN
         CASE DEFAULT
!
!
!     ISPEC = 1:  block size
!
!     In these examples, separate code is provided for setting NB for
!     real and complex.  We assume that NB will take the same value in
!     single or double precision.
!
            nb = 1
!
            IF ( c2=='GE' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ELSEIF ( c3=='QRF' .OR. c3=='RQF' .OR. c3=='LQF' .OR.    &
     &                  c3=='QLF' ) THEN
                  IF ( sname ) THEN
                     nb = 32
                  ELSE
                     nb = 32
                  ENDIF
               ELSEIF ( c3=='HRD' ) THEN
                  IF ( sname ) THEN
                     nb = 32
                  ELSE
                     nb = 32
                  ENDIF
               ELSEIF ( c3=='BRD' ) THEN
                  IF ( sname ) THEN
                     nb = 32
                  ELSE
                     nb = 32
                  ENDIF
               ELSEIF ( c3=='TRI' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ENDIF
            ELSEIF ( c2=='PO' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ENDIF
            ELSEIF ( c2=='SY' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ELSEIF ( sname .AND. c3=='TRD' ) THEN
                  nb = 1
               ELSEIF ( sname .AND. c3=='GST' ) THEN
                  nb = 64
               ENDIF
            ELSEIF ( cname .AND. c2=='HE' ) THEN
               IF ( c3=='TRF' ) THEN
                  nb = 64
               ELSEIF ( c3=='TRD' ) THEN
                  nb = 1
               ELSEIF ( c3=='GST' ) THEN
                  nb = 64
               ENDIF
            ELSEIF ( sname .AND. c2=='OR' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nb = 32
               ELSEIF ( c3(1:1)=='M' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nb = 32
               ENDIF
            ELSEIF ( cname .AND. c2=='UN' ) THEN
               IF ( c3(1:1)=='G' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nb = 32
               ELSEIF ( c3(1:1)=='M' ) THEN
                  IF ( c4=='QR' .OR. c4=='RQ' .OR. c4=='LQ' .OR.        &
     &                 c4=='QL' .OR. c4=='HR' .OR. c4=='TR' .OR.        &
     &                 c4=='BR' ) nb = 32
               ENDIF
            ELSEIF ( c2=='GB' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     IF ( N4<=64 ) THEN
                        nb = 1
                     ELSE
                        nb = 32
                     ENDIF
                  ELSEIF ( N4<=64 ) THEN
                     nb = 1
                  ELSE
                     nb = 32
                  ENDIF
               ENDIF
            ELSEIF ( c2=='PB' ) THEN
               IF ( c3=='TRF' ) THEN
                  IF ( sname ) THEN
                     IF ( N2<=64 ) THEN
                        nb = 1
                     ELSE
                        nb = 32
                     ENDIF
                  ELSEIF ( N2<=64 ) THEN
                     nb = 1
                  ELSE
                     nb = 32
                  ENDIF
               ENDIF
            ELSEIF ( c2=='TR' ) THEN
               IF ( c3=='TRI' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ENDIF
            ELSEIF ( c2=='LA' ) THEN
               IF ( c3=='UUM' ) THEN
                  IF ( sname ) THEN
                     nb = 64
                  ELSE
                     nb = 64
                  ENDIF
               ENDIF
            ELSEIF ( sname .AND. c2=='ST' ) THEN
               IF ( c3=='EBZ' ) nb = 1
            ENDIF
!** Reduce NB ??? !!!
            ILAENV = nb
                    ! max (NB/2, 1) ???
            RETURN
         END SELECT
      CASE (4)
!
!
!     ISPEC = 4:  number of shifts (used by xHSEQR)
!
         ILAENV = 6
         RETURN
      CASE (5)
!
!
!     ISPEC = 5:  minimum column dimension (not used)
!
         ILAENV = 2
         RETURN
      CASE (6)
!
!
!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
!
         ILAENV = INT(REAL(MIN(N1,N2))*1.6E0)
         RETURN
      CASE (7)
!
!
!     ISPEC = 7:  number of processors (not used)
!
         ILAENV = 1
         RETURN
      CASE (8)
!
!
!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
!
         ILAENV = 50
         GOTO 99999
      CASE DEFAULT
      END SELECT
!
!     Invalid value for ISPEC
!
      ILAENV = -1
      RETURN
!
!     End of ILAENV
!
99999 END FUNCTION ILAENV
!*==LSAME.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      FUNCTION LSAME(Ca,Cb)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - CA
!A INPUT  - CB
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGEMM    DGEMV    DTRMM    DTRMV    DTRSM    DTRTI2
!             DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  INTA     INTB     ZCODE
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER :: Ca , Cb
      LOGICAL :: LSAME
      INTENT (IN) Ca , Cb
!
! Local variables
!
      INTEGER :: inta , intb , zcode
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!     .. Scalar Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  LSAME returns .TRUE. if CA is the same letter as CB regardless of
!  case.
!
!  Arguments
!  =========
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!          CA and CB specify the single characters to be compared.
!
! =====================================================================
!
!     .. Intrinsic Functions ..
!     ..
!     .. Local Scalars ..
!     ..
!     .. Executable Statements ..
!
!     Test if the characters are equal
!
      LSAME = Ca==Cb
      IF ( LSAME ) RETURN
!
!     Now test for equivalence if both characters are alphabetic.
!
      zcode = ICHAR('Z')
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
      inta = ICHAR(Ca)
      intb = ICHAR(Cb)
!
      IF ( zcode==90 .OR. zcode==122 ) THEN
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
         IF ( inta>=97 .AND. inta<=122 ) inta = inta - 32
         IF ( intb>=97 .AND. intb<=122 ) intb = intb - 32
!
      ELSEIF ( zcode==233 .OR. zcode==169 ) THEN
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
         IF ( inta>=129 .AND. inta<=137 .OR. inta>=145 .AND.            &
     &        inta<=153 .OR. inta>=162 .AND. inta<=169 ) inta = inta +  &
     &        64
         IF ( intb>=129 .AND. intb<=137 .OR. intb>=145 .AND.            &
     &        intb<=153 .OR. intb>=162 .AND. intb<=169 ) intb = intb +  &
     &        64
!
      ELSEIF ( zcode==218 .OR. zcode==250 ) THEN
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
         IF ( inta>=225 .AND. inta<=250 ) inta = inta - 32
         IF ( intb>=225 .AND. intb<=250 ) intb = intb - 32
      ENDIF
      LSAME = inta==intb
!
!     RETURN
!
!     End of LSAME
!
      END FUNCTION LSAME
!*==DGEMM.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
!****************************************************************************
! This file contains the BLAS routines that are called by LAPACK routines
! for inversion of a matrix. The Lapack routines (NOT in this file!) are:
!         dgetf2  dgetrf  dgetri  dlaswp  dtrti2  dtrtri  ilaenv  lsame
!
! The BLAS routines (in this file) are:
!         dgemm   dger    dswap   dtrmv   idamax
!         dgemv   dscal   dtrmm   dtrsm   xerbla
!
! Instead of these routines you can better use a processor-optimized
! library (like INTEL mkl library).
!
!       J. Bergervoet
!       May, 1998
!****************************************************************************
 
      SUBROUTINE DGEMM(Transa,Transb,M,N,K,Alpha,A,Lda,B,Ldb,Beta,C,Ldc)
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - TRANSA
!A PASSED - TRANSB
!A INPUT  - M
!A INPUT  - N
!A INPUT  - K
!A INPUT  - ALPHA
!A INPUT  - A
!A INPUT  - LDA
!A INPUT  - B
!A INPUT  - LDB
!A INPUT  - BETA
!A OUTPUT - C
!A INPUT  - LDC
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       LSAME    XERBLA
! called by   DGETRF   DGETRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     J        L        NCOLA    NOTA
!             NOTB     NROWA    NROWB    TEMP
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      DOUBLE PRECISION :: Alpha , Beta
      INTEGER :: K , Lda , Ldb , Ldc , M , N
      CHARACTER(1) :: Transa , Transb
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      DOUBLE PRECISION , DIMENSION(Ldc,*) :: C
      INTENT (IN) A , Alpha , B , Beta , K , Lda , Ldb , Ldc , M , N
      INTENT (INOUT) C
!
! Local variables
!
      INTEGER :: i , info , j , l , ncola , nrowa , nrowb
      LOGICAL :: nota , notb
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGEMM  performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     .. Local Scalars ..
!     .. Parameters ..
!     ..
!     .. Executable Statements ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!
      nota = LSAME(Transa,'N')
      notb = LSAME(Transb,'N')
      IF ( nota ) THEN
         nrowa = M
         ncola = K
      ELSE
         nrowa = K
         ncola = M
      ENDIF
      IF ( notb ) THEN
         nrowb = K
      ELSE
         nrowb = N
      ENDIF
!
!     Test the input parameters.
!
      info = 0
      IF ( (.NOT.nota) .AND. (.NOT.LSAME(Transa,'C')) .AND.             &
     &     (.NOT.LSAME(Transa,'T')) ) THEN
         info = 1
      ELSEIF ( (.NOT.notb) .AND. (.NOT.LSAME(Transb,'C')) .AND.         &
     &         (.NOT.LSAME(Transb,'T')) ) THEN
         info = 2
      ELSEIF ( M<0 ) THEN
         info = 3
      ELSEIF ( N<0 ) THEN
         info = 4
      ELSEIF ( K<0 ) THEN
         info = 5
      ELSEIF ( Lda<MAX(1,nrowa) ) THEN
         info = 8
      ELSEIF ( Ldb<MAX(1,nrowb) ) THEN
         info = 10
      ELSEIF ( Ldc<MAX(1,M) ) THEN
         info = 13
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DGEMM ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( (M==0) .OR. (N==0) .OR.                                      &
     &     (((Alpha==ZERO) .OR. (K==0)) .AND. (Beta==ONE)) ) RETURN
!
!     And if  alpha.eq.zero.
!
      IF ( Alpha==ZERO ) THEN
         IF ( Beta==ZERO ) THEN
            DO j = 1 , N
               DO i = 1 , M
                  C(i,j) = ZERO
               ENDDO
            ENDDO
         ELSE
            DO j = 1 , N
               DO i = 1 , M
                  C(i,j) = Beta*C(i,j)
               ENDDO
            ENDDO
         ENDIF
         RETURN
      ENDIF
!
!     Start the operations.
!
      IF ( notb ) THEN
         IF ( nota ) THEN
!
!           Form  C := alpha*A*B + beta*C.
!
            DO j = 1 , N
               IF ( Beta==ZERO ) THEN
                  DO i = 1 , M
                     C(i,j) = ZERO
                  ENDDO
               ELSEIF ( Beta/=ONE ) THEN
                  DO i = 1 , M
                     C(i,j) = Beta*C(i,j)
                  ENDDO
               ENDIF
               DO l = 1 , K
                  IF ( B(l,j)/=ZERO ) THEN
                     temp = Alpha*B(l,j)
                     DO i = 1 , M
                        C(i,j) = C(i,j) + temp*A(i,l)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ELSE
!
!           Form  C := alpha*A'*B + beta*C
!
            DO j = 1 , N
               DO i = 1 , M
                  temp = ZERO
                  DO l = 1 , K
                     temp = temp + A(l,i)*B(l,j)
                  ENDDO
                  IF ( Beta==ZERO ) THEN
                     C(i,j) = Alpha*temp
                  ELSE
                     C(i,j) = Alpha*temp + Beta*C(i,j)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ELSEIF ( nota ) THEN
!
!           Form  C := alpha*A*B' + beta*C
!
         DO j = 1 , N
            IF ( Beta==ZERO ) THEN
               DO i = 1 , M
                  C(i,j) = ZERO
               ENDDO
            ELSEIF ( Beta/=ONE ) THEN
               DO i = 1 , M
                  C(i,j) = Beta*C(i,j)
               ENDDO
            ENDIF
            DO l = 1 , K
               IF ( B(j,l)/=ZERO ) THEN
                  temp = Alpha*B(j,l)
                  DO i = 1 , M
                     C(i,j) = C(i,j) + temp*A(i,l)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ELSE
!
!           Form  C := alpha*A'*B' + beta*C
!
         DO j = 1 , N
            DO i = 1 , M
               temp = ZERO
               DO l = 1 , K
                  temp = temp + A(l,i)*B(j,l)
               ENDDO
               IF ( Beta==ZERO ) THEN
                  C(i,j) = Alpha*temp
               ELSE
                  C(i,j) = Alpha*temp + Beta*C(i,j)
               ENDIF
            ENDDO
         ENDDO
      ENDIF
!
!
!     End of DGEMM .
!
      END SUBROUTINE DGEMM
!*==DGEMV.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DGEMV(Trans,M,N,Alpha,A,Lda,X,Incx,Beta,Y,Incy)
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - TRANS
!A INPUT  - M
!A INPUT  - N
!A INPUT  - ALPHA
!A INPUT  - A
!A INPUT  - LDA
!A INPUT  - X
!A INPUT  - INCX
!A INPUT  - BETA
!A OUTPUT - Y
!A INPUT  - INCY
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       LSAME    XERBLA
! called by   DGETRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     IX       IY       J        JX       JY
!             KX       KY       LENX     LENY     TEMP
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      DOUBLE PRECISION :: Alpha , Beta
      INTEGER :: Incx , Incy , Lda , M , N
      CHARACTER(1) :: Trans
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X , Y
      INTENT (IN) A , Alpha , Beta , Incx , Incy , Lda , M , N , X
      INTENT (INOUT) Y
!
! Local variables
!
      INTEGER :: i , info , ix , iy , j , jx , jy , kx , ky , lenx ,    &
     &           leny
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGEMV  performs one of the matrix-vector operations
!
!     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
!     .. Local Scalars ..
!     .. External Functions ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      info = 0
      IF ( .NOT.LSAME(Trans,'N') .AND. .NOT.LSAME(Trans,'T') .AND.      &
     &     .NOT.LSAME(Trans,'C') ) THEN
         info = 1
      ELSEIF ( M<0 ) THEN
         info = 2
      ELSEIF ( N<0 ) THEN
         info = 3
      ELSEIF ( Lda<MAX(1,M) ) THEN
         info = 6
      ELSEIF ( Incx==0 ) THEN
         info = 8
      ELSEIF ( Incy==0 ) THEN
         info = 11
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DGEMV ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( (M==0) .OR. (N==0) .OR. ((Alpha==ZERO) .AND. (Beta==ONE)) )  &
     &     RETURN
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
      IF ( LSAME(Trans,'N') ) THEN
         lenx = N
         leny = M
      ELSE
         lenx = M
         leny = N
      ENDIF
      IF ( Incx>0 ) THEN
         kx = 1
      ELSE
         kx = 1 - (lenx-1)*Incx
      ENDIF
      IF ( Incy>0 ) THEN
         ky = 1
      ELSE
         ky = 1 - (leny-1)*Incy
      ENDIF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
      IF ( Beta/=ONE ) THEN
         IF ( Incy/=1 ) THEN
            iy = ky
            IF ( Beta==ZERO ) THEN
               DO i = 1 , leny
                  Y(iy) = ZERO
                  iy = iy + Incy
               ENDDO
            ELSE
               DO i = 1 , leny
                  Y(iy) = Beta*Y(iy)
                  iy = iy + Incy
               ENDDO
            ENDIF
         ELSEIF ( Beta==ZERO ) THEN
            DO i = 1 , leny
               Y(i) = ZERO
            ENDDO
         ELSE
            DO i = 1 , leny
               Y(i) = Beta*Y(i)
            ENDDO
         ENDIF
      ENDIF
      IF ( Alpha==ZERO ) RETURN
      IF ( LSAME(Trans,'N') ) THEN
!
!        Form  y := alpha*A*x + y.
!
         jx = kx
         IF ( Incy==1 ) THEN
            DO j = 1 , N
               IF ( X(jx)/=ZERO ) THEN
                  temp = Alpha*X(jx)
                  DO i = 1 , M
                     Y(i) = Y(i) + temp*A(i,j)
                  ENDDO
               ENDIF
               jx = jx + Incx
            ENDDO
         ELSE
            DO j = 1 , N
               IF ( X(jx)/=ZERO ) THEN
                  temp = Alpha*X(jx)
                  iy = ky
                  DO i = 1 , M
                     Y(iy) = Y(iy) + temp*A(i,j)
                     iy = iy + Incy
                  ENDDO
               ENDIF
               jx = jx + Incx
            ENDDO
         ENDIF
      ELSE
!
!        Form  y := alpha*A'*x + y.
!
         jy = ky
         IF ( Incx==1 ) THEN
            DO j = 1 , N
               temp = ZERO
               DO i = 1 , M
                  temp = temp + A(i,j)*X(i)
               ENDDO
               Y(jy) = Y(jy) + Alpha*temp
               jy = jy + Incy
            ENDDO
         ELSE
            DO j = 1 , N
               temp = ZERO
               ix = kx
               DO i = 1 , M
                  temp = temp + A(i,j)*X(ix)
                  ix = ix + Incx
               ENDDO
               Y(jy) = Y(jy) + Alpha*temp
               jy = jy + Incy
            ENDDO
         ENDIF
      ENDIF
!
!
!     End of DGEMV .
!
      END SUBROUTINE DGEMV
!*==DGER.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DGER(M,N,Alpha,X,Incx,Y,Incy,A,Lda)
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - M
!A INPUT  - N
!A INPUT  - ALPHA
!A INPUT  - X
!A INPUT  - INCX
!A INPUT  - Y
!A INPUT  - INCY
!A OUTPUT - A
!A INPUT  - LDA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       XERBLA
! called by   DGETF2
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     IX       J        JY       KX
!             TEMP
! uses PARAMs ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0
!
! Dummy arguments
!
      DOUBLE PRECISION :: Alpha
      INTEGER :: Incx , Incy , Lda , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X , Y
      INTENT (IN) Alpha , Incx , Incy , Lda , M , N , X , Y
      INTENT (INOUT) A
!
! Local variables
!
      INTEGER :: i , info , ix , j , jy , kx
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DGER   performs the rank 1 operation
!
!     A := alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters
!  ==========
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
!     .. Local Scalars ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      info = 0
      IF ( M<0 ) THEN
         info = 1
      ELSEIF ( N<0 ) THEN
         info = 2
      ELSEIF ( Incx==0 ) THEN
         info = 5
      ELSEIF ( Incy==0 ) THEN
         info = 7
      ELSEIF ( Lda<MAX(1,M) ) THEN
         info = 9
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DGER  ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( (M==0) .OR. (N==0) .OR. (Alpha==ZERO) ) RETURN
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF ( Incy>0 ) THEN
         jy = 1
      ELSE
         jy = 1 - (N-1)*Incy
      ENDIF
      IF ( Incx==1 ) THEN
         DO j = 1 , N
            IF ( Y(jy)/=ZERO ) THEN
               temp = Alpha*Y(jy)
               DO i = 1 , M
                  A(i,j) = A(i,j) + X(i)*temp
               ENDDO
            ENDIF
            jy = jy + Incy
         ENDDO
      ELSE
         IF ( Incx>0 ) THEN
            kx = 1
         ELSE
            kx = 1 - (M-1)*Incx
         ENDIF
         DO j = 1 , N
            IF ( Y(jy)/=ZERO ) THEN
               temp = Alpha*Y(jy)
               ix = kx
               DO i = 1 , M
                  A(i,j) = A(i,j) + X(ix)*temp
                  ix = ix + Incx
               ENDDO
            ENDIF
            jy = jy + Incy
         ENDDO
      ENDIF
!
!
!     End of DGER  .
!
      END SUBROUTINE DGER
!*==DSCAL.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DSCAL(N,Da,Dx,Incx)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - N
!A INPUT  - DA
!A OUTPUT - DX
!A INPUT  - INCX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGETF2   DTRTI2
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        M        MP1      NINCX
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      DOUBLE PRECISION :: Da
      INTEGER :: Incx , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx
      INTENT (IN) Da , Incx , N
      INTENT (INOUT) Dx
!
! Local variables
!
      INTEGER :: i , m , mp1 , nincx
!
!*** End of declarations rewritten by SPAG
!
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
      IF ( N<=0 .OR. Incx<=0 ) RETURN
      IF ( Incx==1 ) THEN
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
         m = MOD(N,5)
         IF ( m/=0 ) THEN
            DO i = 1 , m
               Dx(i) = Da*Dx(i)
            ENDDO
            IF ( N<5 ) RETURN
         ENDIF
         mp1 = m + 1
         DO i = mp1 , N , 5
            Dx(i) = Da*Dx(i)
            Dx(i+1) = Da*Dx(i+1)
            Dx(i+2) = Da*Dx(i+2)
            Dx(i+3) = Da*Dx(i+3)
            Dx(i+4) = Da*Dx(i+4)
         ENDDO
      ELSE
!
!        code for increment not equal to 1
!
         nincx = N*Incx
         DO i = 1 , nincx , Incx
            Dx(i) = Da*Dx(i)
         ENDDO
         RETURN
      ENDIF
      END SUBROUTINE DSCAL
!*==DSWAP.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DSWAP(N,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - N
!A OUTPUT - DX
!A INPUT  - INCX
!A OUTPUT - DY
!A INPUT  - INCY
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGETF2   DGETRI   DLASWP
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  DTEMP    I        IX       IY       M        MP1
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Incx , Incy , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx , Dy
      INTENT (IN) Incx , Incy , N
      INTENT (INOUT) Dx , Dy
!
! Local variables
!
      DOUBLE PRECISION :: dtemp
      INTEGER :: i , ix , iy , m , mp1
!
!*** End of declarations rewritten by SPAG
!
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal one.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
      IF ( N<=0 ) RETURN
      IF ( Incx==1 .AND. Incy==1 ) THEN
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
         m = MOD(N,3)
         IF ( m/=0 ) THEN
            DO i = 1 , m
               dtemp = Dx(i)
               Dx(i) = Dy(i)
               Dy(i) = dtemp
            ENDDO
            IF ( N<3 ) RETURN
         ENDIF
         mp1 = m + 1
         DO i = mp1 , N , 3
            dtemp = Dx(i)
            Dx(i) = Dy(i)
            Dy(i) = dtemp
            dtemp = Dx(i+1)
            Dx(i+1) = Dy(i+1)
            Dy(i+1) = dtemp
            dtemp = Dx(i+2)
            Dx(i+2) = Dy(i+2)
            Dy(i+2) = dtemp
         ENDDO
      ELSE
!
!       code for unequal increments or equal increments not equal
!         to 1
!
         ix = 1
         iy = 1
         IF ( Incx<0 ) ix = (-N+1)*Incx + 1
         IF ( Incy<0 ) iy = (-N+1)*Incy + 1
         DO i = 1 , N
            dtemp = Dx(ix)
            Dx(ix) = Dy(iy)
            Dy(iy) = dtemp
            ix = ix + Incx
            iy = iy + Incy
         ENDDO
         RETURN
      ENDIF
      END SUBROUTINE DSWAP
!*==DTRMM.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DTRMM(Side,Uplo,Transa,Diag,M,N,Alpha,A,Lda,B,Ldb)
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - SIDE
!A PASSED - UPLO
!A PASSED - TRANSA
!A PASSED - DIAG
!A INPUT  - M
!A INPUT  - N
!A INPUT  - ALPHA
!A INPUT  - A
!A INPUT  - LDA
!A OUTPUT - B
!A INPUT  - LDB
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       LSAME    XERBLA
! called by   DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     J        K        LSIDE    NOUNIT
!             NROWA    TEMP     UPPER
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      DOUBLE PRECISION :: Alpha
      CHARACTER(1) :: Diag , Side , Transa , Uplo
      INTEGER :: Lda , Ldb , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      INTENT (IN) A , Alpha , Lda , Ldb , M , N
      INTENT (INOUT) B
!
! Local variables
!
      INTEGER :: i , info , j , k , nrowa
      LOGICAL :: lside , nounit , upper
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DTRMM  performs one of the matrix-matrix operations
!
!     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
!
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:
!
!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!
!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     .. Local Scalars ..
!     .. Parameters ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      lside = LSAME(Side,'L')
      IF ( lside ) THEN
         nrowa = M
      ELSE
         nrowa = N
      ENDIF
      nounit = LSAME(Diag,'N')
      upper = LSAME(Uplo,'U')
!
      info = 0
      IF ( (.NOT.lside) .AND. (.NOT.LSAME(Side,'R')) ) THEN
         info = 1
      ELSEIF ( (.NOT.upper) .AND. (.NOT.LSAME(Uplo,'L')) ) THEN
         info = 2
      ELSEIF ( (.NOT.LSAME(Transa,'N')) .AND. (.NOT.LSAME(Transa,'T'))  &
     &         .AND. (.NOT.LSAME(Transa,'C')) ) THEN
         info = 3
      ELSEIF ( (.NOT.LSAME(Diag,'U')) .AND. (.NOT.LSAME(Diag,'N')) )    &
     &         THEN
         info = 4
      ELSEIF ( M<0 ) THEN
         info = 5
      ELSEIF ( N<0 ) THEN
         info = 6
      ELSEIF ( Lda<MAX(1,nrowa) ) THEN
         info = 9
      ELSEIF ( Ldb<MAX(1,M) ) THEN
         info = 11
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DTRMM ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( N==0 ) RETURN
!
!     And when  alpha.eq.zero.
!
      IF ( Alpha==ZERO ) THEN
         DO j = 1 , N
            DO i = 1 , M
               B(i,j) = ZERO
            ENDDO
         ENDDO
         RETURN
      ENDIF
!
!     Start the operations.
!
      IF ( lside ) THEN
         IF ( LSAME(Transa,'N') ) THEN
!
!           Form  B := alpha*A*B.
!
            IF ( upper ) THEN
               DO j = 1 , N
                  DO k = 1 , M
                     IF ( B(k,j)/=ZERO ) THEN
                        temp = Alpha*B(k,j)
                        DO i = 1 , k - 1
                           B(i,j) = B(i,j) + temp*A(i,k)
                        ENDDO
                        IF ( nounit ) temp = temp*A(k,k)
                        B(k,j) = temp
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = 1 , N
                  DO k = M , 1 , -1
                     IF ( B(k,j)/=ZERO ) THEN
                        temp = Alpha*B(k,j)
                        B(k,j) = temp
                        IF ( nounit ) B(k,j) = B(k,j)*A(k,k)
                        DO i = k + 1 , M
                           B(i,j) = B(i,j) + temp*A(i,k)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!
!           Form  B := alpha*B*A'.
!
         ELSEIF ( upper ) THEN
            DO j = 1 , N
               DO i = M , 1 , -1
                  temp = B(i,j)
                  IF ( nounit ) temp = temp*A(i,i)
                  DO k = 1 , i - 1
                     temp = temp + A(k,i)*B(k,j)
                  ENDDO
                  B(i,j) = Alpha*temp
               ENDDO
            ENDDO
         ELSE
            DO j = 1 , N
               DO i = 1 , M
                  temp = B(i,j)
                  IF ( nounit ) temp = temp*A(i,i)
                  DO k = i + 1 , M
                     temp = temp + A(k,i)*B(k,j)
                  ENDDO
                  B(i,j) = Alpha*temp
               ENDDO
            ENDDO
         ENDIF
      ELSEIF ( LSAME(Transa,'N') ) THEN
!
!           Form  B := alpha*B*A.
!
         IF ( upper ) THEN
            DO j = N , 1 , -1
               temp = Alpha
               IF ( nounit ) temp = temp*A(j,j)
               DO i = 1 , M
                  B(i,j) = temp*B(i,j)
               ENDDO
               DO k = 1 , j - 1
                  IF ( A(k,j)/=ZERO ) THEN
                     temp = Alpha*A(k,j)
                     DO i = 1 , M
                        B(i,j) = B(i,j) + temp*B(i,k)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ELSE
            DO j = 1 , N
               temp = Alpha
               IF ( nounit ) temp = temp*A(j,j)
               DO i = 1 , M
                  B(i,j) = temp*B(i,j)
               ENDDO
               DO k = j + 1 , N
                  IF ( A(k,j)/=ZERO ) THEN
                     temp = Alpha*A(k,j)
                     DO i = 1 , M
                        B(i,j) = B(i,j) + temp*B(i,k)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
!
!           Form  B := alpha*B*A'.
!
      ELSEIF ( upper ) THEN
         DO k = 1 , N
            DO j = 1 , k - 1
               IF ( A(j,k)/=ZERO ) THEN
                  temp = Alpha*A(j,k)
                  DO i = 1 , M
                     B(i,j) = B(i,j) + temp*B(i,k)
                  ENDDO
               ENDIF
            ENDDO
            temp = Alpha
            IF ( nounit ) temp = temp*A(k,k)
            IF ( temp/=ONE ) THEN
               DO i = 1 , M
                  B(i,k) = temp*B(i,k)
               ENDDO
            ENDIF
         ENDDO
      ELSE
         DO k = N , 1 , -1
            DO j = k + 1 , N
               IF ( A(j,k)/=ZERO ) THEN
                  temp = Alpha*A(j,k)
                  DO i = 1 , M
                     B(i,j) = B(i,j) + temp*B(i,k)
                  ENDDO
               ENDIF
            ENDDO
            temp = Alpha
            IF ( nounit ) temp = temp*A(k,k)
            IF ( temp/=ONE ) THEN
               DO i = 1 , M
                  B(i,k) = temp*B(i,k)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
!
!     End of DTRMM .
!
      END SUBROUTINE DTRMM
!*==DTRMV.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DTRMV(Uplo,Trans,Diag,N,A,Lda,X,Incx)
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - UPLO
!A PASSED - TRANS
!A PASSED - DIAG
!A INPUT  - N
!A INPUT  - A
!A INPUT  - LDA
!A OUTPUT - X
!A INPUT  - INCX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       LSAME    XERBLA
! called by   DTRTI2
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     IX       J        JX       KX
!             NOUNIT   TEMP
! uses PARAMs ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ZERO = 0.0D+0
!
! Dummy arguments
!
      CHARACTER(1) :: Diag , Trans , Uplo
      INTEGER :: Incx , Lda , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(*) :: X
      INTENT (IN) A , Incx , Lda , N
      INTENT (INOUT) X
!
! Local variables
!
      INTEGER :: i , info , ix , j , jx , kx
      LOGICAL :: nounit
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DTRMV  performs one of the matrix-vector operations
!
!     x := A*x,   or   x := A'*x,
!
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   x := A*x.
!
!              TRANS = 'T' or 't'   x := A'*x.
!
!              TRANS = 'C' or 'c'   x := A'*x.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x. On exit, X is overwritten with the
!           tranformed vector x.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
!     .. Local Scalars ..
!     .. External Functions ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      info = 0
      IF ( .NOT.LSAME(Uplo,'U') .AND. .NOT.LSAME(Uplo,'L') ) THEN
         info = 1
      ELSEIF ( .NOT.LSAME(Trans,'N') .AND. .NOT.LSAME(Trans,'T') .AND.  &
     &         .NOT.LSAME(Trans,'C') ) THEN
         info = 2
      ELSEIF ( .NOT.LSAME(Diag,'U') .AND. .NOT.LSAME(Diag,'N') ) THEN
         info = 3
      ELSEIF ( N<0 ) THEN
         info = 4
      ELSEIF ( Lda<MAX(1,N) ) THEN
         info = 6
      ELSEIF ( Incx==0 ) THEN
         info = 8
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DTRMV ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( N==0 ) RETURN
!
      nounit = LSAME(Diag,'N')
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
      IF ( Incx<=0 ) THEN
         kx = 1 - (N-1)*Incx
      ELSEIF ( Incx/=1 ) THEN
         kx = 1
      ENDIF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF ( LSAME(Trans,'N') ) THEN
!
!        Form  x := A*x.
!
         IF ( LSAME(Uplo,'U') ) THEN
            IF ( Incx==1 ) THEN
               DO j = 1 , N
                  IF ( X(j)/=ZERO ) THEN
                     temp = X(j)
                     DO i = 1 , j - 1
                        X(i) = X(i) + temp*A(i,j)
                     ENDDO
                     IF ( nounit ) X(j) = X(j)*A(j,j)
                  ENDIF
               ENDDO
            ELSE
               jx = kx
               DO j = 1 , N
                  IF ( X(jx)/=ZERO ) THEN
                     temp = X(jx)
                     ix = kx
                     DO i = 1 , j - 1
                        X(ix) = X(ix) + temp*A(i,j)
                        ix = ix + Incx
                     ENDDO
                     IF ( nounit ) X(jx) = X(jx)*A(j,j)
                  ENDIF
                  jx = jx + Incx
               ENDDO
            ENDIF
         ELSEIF ( Incx==1 ) THEN
            DO j = N , 1 , -1
               IF ( X(j)/=ZERO ) THEN
                  temp = X(j)
                  DO i = N , j + 1 , -1
                     X(i) = X(i) + temp*A(i,j)
                  ENDDO
                  IF ( nounit ) X(j) = X(j)*A(j,j)
               ENDIF
            ENDDO
         ELSE
            kx = kx + (N-1)*Incx
            jx = kx
            DO j = N , 1 , -1
               IF ( X(jx)/=ZERO ) THEN
                  temp = X(jx)
                  ix = kx
                  DO i = N , j + 1 , -1
                     X(ix) = X(ix) + temp*A(i,j)
                     ix = ix - Incx
                  ENDDO
                  IF ( nounit ) X(jx) = X(jx)*A(j,j)
               ENDIF
               jx = jx - Incx
            ENDDO
         ENDIF
!
!        Form  x := A'*x.
!
      ELSEIF ( LSAME(Uplo,'U') ) THEN
         IF ( Incx==1 ) THEN
            DO j = N , 1 , -1
               temp = X(j)
               IF ( nounit ) temp = temp*A(j,j)
               DO i = j - 1 , 1 , -1
                  temp = temp + A(i,j)*X(i)
               ENDDO
               X(j) = temp
            ENDDO
         ELSE
            jx = kx + (N-1)*Incx
            DO j = N , 1 , -1
               temp = X(jx)
               ix = jx
               IF ( nounit ) temp = temp*A(j,j)
               DO i = j - 1 , 1 , -1
                  ix = ix - Incx
                  temp = temp + A(i,j)*X(ix)
               ENDDO
               X(jx) = temp
               jx = jx - Incx
            ENDDO
         ENDIF
      ELSEIF ( Incx==1 ) THEN
         DO j = 1 , N
            temp = X(j)
            IF ( nounit ) temp = temp*A(j,j)
            DO i = j + 1 , N
               temp = temp + A(i,j)*X(i)
            ENDDO
            X(j) = temp
         ENDDO
      ELSE
         jx = kx
         DO j = 1 , N
            temp = X(jx)
            ix = jx
            IF ( nounit ) temp = temp*A(j,j)
            DO i = j + 1 , N
               ix = ix + Incx
               temp = temp + A(i,j)*X(ix)
            ENDDO
            X(jx) = temp
            jx = jx + Incx
         ENDDO
      ENDIF
!
!
!     End of DTRMV .
!
      END SUBROUTINE DTRMV
!*==DTRSM.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE DTRSM(Side,Uplo,Transa,Diag,M,N,Alpha,A,Lda,B,Ldb)
      USE S_LSAME
      USE S_XERBLA
      IMPLICIT NONE
!*--********************************************************************
!A PASSED - SIDE
!A PASSED - UPLO
!A PASSED - TRANSA
!A PASSED - DIAG
!A INPUT  - M
!A INPUT  - N
!A INPUT  - ALPHA
!A INPUT  - A
!A INPUT  - LDA
!A OUTPUT - B
!A INPUT  - LDB
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       LSAME    XERBLA
! called by   DGETRF   DGETRI   DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  I        INFO     J        K        LSIDE    NOUNIT
!             NROWA    TEMP     UPPER
! uses PARAMs ONE      ZERO
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
      DOUBLE PRECISION , PARAMETER :: ONE = 1.0D+0 , ZERO = 0.0D+0
!
! Dummy arguments
!
      DOUBLE PRECISION :: Alpha
      CHARACTER(1) :: Diag , Side , Transa , Uplo
      INTEGER :: Lda , Ldb , M , N
      DOUBLE PRECISION , DIMENSION(Lda,*) :: A
      DOUBLE PRECISION , DIMENSION(Ldb,*) :: B
      INTENT (IN) A , Alpha , Lda , Ldb , M , N
      INTENT (INOUT) B
!
! Local variables
!
      INTEGER :: i , info , j , k , nrowa
      LOGICAL :: lside , nounit , upper
      DOUBLE PRECISION :: temp
!
!*** End of declarations rewritten by SPAG
!
!     .. Scalar Arguments ..
!     .. Array Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  DTRSM  solves one of the matrix equations
!
!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
!
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  The matrix X is overwritten on B.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:
!
!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
!
!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
!     .. External Subroutines ..
!     .. Intrinsic Functions ..
!     .. Local Scalars ..
!     .. Parameters ..
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      lside = LSAME(Side,'L')
      IF ( lside ) THEN
         nrowa = M
      ELSE
         nrowa = N
      ENDIF
      nounit = LSAME(Diag,'N')
      upper = LSAME(Uplo,'U')
!
      info = 0
      IF ( (.NOT.lside) .AND. (.NOT.LSAME(Side,'R')) ) THEN
         info = 1
      ELSEIF ( (.NOT.upper) .AND. (.NOT.LSAME(Uplo,'L')) ) THEN
         info = 2
      ELSEIF ( (.NOT.LSAME(Transa,'N')) .AND. (.NOT.LSAME(Transa,'T'))  &
     &         .AND. (.NOT.LSAME(Transa,'C')) ) THEN
         info = 3
      ELSEIF ( (.NOT.LSAME(Diag,'U')) .AND. (.NOT.LSAME(Diag,'N')) )    &
     &         THEN
         info = 4
      ELSEIF ( M<0 ) THEN
         info = 5
      ELSEIF ( N<0 ) THEN
         info = 6
      ELSEIF ( Lda<MAX(1,nrowa) ) THEN
         info = 9
      ELSEIF ( Ldb<MAX(1,M) ) THEN
         info = 11
      ENDIF
      IF ( info/=0 ) THEN
         CALL XERBLA('DTRSM ',info)
         RETURN
      ENDIF
!
!     Quick return if possible.
!
      IF ( N==0 ) RETURN
!
!     And when  alpha.eq.zero.
!
      IF ( Alpha==ZERO ) THEN
         DO j = 1 , N
            DO i = 1 , M
               B(i,j) = ZERO
            ENDDO
         ENDDO
         RETURN
      ENDIF
!
!     Start the operations.
!
      IF ( lside ) THEN
         IF ( LSAME(Transa,'N') ) THEN
!
!           Form  B := alpha*inv( A )*B.
!
            IF ( upper ) THEN
               DO j = 1 , N
                  IF ( Alpha/=ONE ) THEN
                     DO i = 1 , M
                        B(i,j) = Alpha*B(i,j)
                     ENDDO
                  ENDIF
                  DO k = M , 1 , -1
                     IF ( B(k,j)/=ZERO ) THEN
                        IF ( nounit ) B(k,j) = B(k,j)/A(k,k)
                        DO i = 1 , k - 1
                           B(i,j) = B(i,j) - B(k,j)*A(i,k)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = 1 , N
                  IF ( Alpha/=ONE ) THEN
                     DO i = 1 , M
                        B(i,j) = Alpha*B(i,j)
                     ENDDO
                  ENDIF
                  DO k = 1 , M
                     IF ( B(k,j)/=ZERO ) THEN
                        IF ( nounit ) B(k,j) = B(k,j)/A(k,k)
                        DO i = k + 1 , M
                           B(i,j) = B(i,j) - B(k,j)*A(i,k)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!
!           Form  B := alpha*inv( A' )*B.
!
         ELSEIF ( upper ) THEN
            DO j = 1 , N
               DO i = 1 , M
                  temp = Alpha*B(i,j)
                  DO k = 1 , i - 1
                     temp = temp - A(k,i)*B(k,j)
                  ENDDO
                  IF ( nounit ) temp = temp/A(i,i)
                  B(i,j) = temp
               ENDDO
            ENDDO
         ELSE
            DO j = 1 , N
               DO i = M , 1 , -1
                  temp = Alpha*B(i,j)
                  DO k = i + 1 , M
                     temp = temp - A(k,i)*B(k,j)
                  ENDDO
                  IF ( nounit ) temp = temp/A(i,i)
                  B(i,j) = temp
               ENDDO
            ENDDO
         ENDIF
      ELSEIF ( LSAME(Transa,'N') ) THEN
!
!           Form  B := alpha*B*inv( A ).
!
         IF ( upper ) THEN
            DO j = 1 , N
               IF ( Alpha/=ONE ) THEN
                  DO i = 1 , M
                     B(i,j) = Alpha*B(i,j)
                  ENDDO
               ENDIF
               DO k = 1 , j - 1
                  IF ( A(k,j)/=ZERO ) THEN
                     DO i = 1 , M
                        B(i,j) = B(i,j) - A(k,j)*B(i,k)
                     ENDDO
                  ENDIF
               ENDDO
               IF ( nounit ) THEN
                  temp = ONE/A(j,j)
                  DO i = 1 , M
                     B(i,j) = temp*B(i,j)
                  ENDDO
               ENDIF
            ENDDO
         ELSE
            DO j = N , 1 , -1
               IF ( Alpha/=ONE ) THEN
                  DO i = 1 , M
                     B(i,j) = Alpha*B(i,j)
                  ENDDO
               ENDIF
               DO k = j + 1 , N
                  IF ( A(k,j)/=ZERO ) THEN
                     DO i = 1 , M
                        B(i,j) = B(i,j) - A(k,j)*B(i,k)
                     ENDDO
                  ENDIF
               ENDDO
               IF ( nounit ) THEN
                  temp = ONE/A(j,j)
                  DO i = 1 , M
                     B(i,j) = temp*B(i,j)
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
!
!           Form  B := alpha*B*inv( A' ).
!
      ELSEIF ( upper ) THEN
         DO k = N , 1 , -1
            IF ( nounit ) THEN
               temp = ONE/A(k,k)
               DO i = 1 , M
                  B(i,k) = temp*B(i,k)
               ENDDO
            ENDIF
            DO j = 1 , k - 1
               IF ( A(j,k)/=ZERO ) THEN
                  temp = A(j,k)
                  DO i = 1 , M
                     B(i,j) = B(i,j) - temp*B(i,k)
                  ENDDO
               ENDIF
            ENDDO
            IF ( Alpha/=ONE ) THEN
               DO i = 1 , M
                  B(i,k) = Alpha*B(i,k)
               ENDDO
            ENDIF
         ENDDO
      ELSE
         DO k = 1 , N
            IF ( nounit ) THEN
               temp = ONE/A(k,k)
               DO i = 1 , M
                  B(i,k) = temp*B(i,k)
               ENDDO
            ENDIF
            DO j = k + 1 , N
               IF ( A(j,k)/=ZERO ) THEN
                  temp = A(j,k)
                  DO i = 1 , M
                     B(i,j) = B(i,j) - temp*B(i,k)
                  ENDDO
               ENDIF
            ENDDO
            IF ( Alpha/=ONE ) THEN
               DO i = 1 , M
                  B(i,k) = Alpha*B(i,k)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
!
!     End of DTRSM .
!
      END SUBROUTINE DTRSM
!*==IDAMAX.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      FUNCTION IDAMAX(N,Dx,Incx)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - N
!A INPUT  - DX
!A INPUT  - INCX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGETF2
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  DMAX     I        IX
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Incx , N
      DOUBLE PRECISION , DIMENSION(*) :: Dx
      INTEGER :: IDAMAX
      INTENT (IN) Dx , Incx , N
!
! Local variables
!
      DOUBLE PRECISION , INTRINSIC :: DABS
      DOUBLE PRECISION :: dmax
      INTEGER :: i , ix
!
!*** End of declarations rewritten by SPAG
!
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!
      IDAMAX = 0
      IF ( N<1 .OR. Incx<=0 ) RETURN
      IDAMAX = 1
      IF ( N==1 ) RETURN
      IF ( Incx==1 ) THEN
!
!        code for increment equal to 1
!
         dmax = DABS(Dx(1))
         DO i = 2 , N
            IF ( DABS(Dx(i))>dmax ) THEN
               IDAMAX = i
               dmax = DABS(Dx(i))
            ENDIF
         ENDDO
         GOTO 99999
      ENDIF
!
!        code for increment not equal to 1
!
      ix = 1
      dmax = DABS(Dx(1))
      ix = ix + Incx
      DO i = 2 , N
         IF ( DABS(Dx(ix))>dmax ) THEN
            IDAMAX = i
            dmax = DABS(Dx(ix))
         ENDIF
         ix = ix + Incx
      ENDDO
      RETURN
99999 END FUNCTION IDAMAX
!*==XERBLA.spg  processed by SPAG 6.55Dc at 12:01 on  5 Feb 2004
      SUBROUTINE XERBLA(Srname,Info)
      IMPLICIT NONE
!*--********************************************************************
!A INPUT  - SRNAME
!A INPUT  - INFO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! calls       ** NOTHING **
! called by   DGEMM    DGEMV    DGER     DGETF2   DGETRF   DGETRI
!             DTRMM    DTRMV    DTRSM    DTRTI2   DTRTRI
! modifies    ** NOTHING **
! uses value  ** NOTHING **
! local vars  *** NONE ****
! uses PARAMs *** NONE ****
!*++********************************************************************
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Info
      CHARACTER(6) :: Srname
      INTENT (IN) Info , Srname
!
!*** End of declarations rewritten by SPAG
!
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
!     ..
!
!  Purpose
!  =======
!
!  XERBLA  is an error handler for the LAPACK routines.
!  It is called by an LAPACK routine if an input parameter has an
!  invalid value.  A message is printed and execution stops.
!
!  Installers may consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!
!  Arguments
!  =========
!
!  SRNAME  (input) CHARACTER*6
!          The name of the routine which called XERBLA.
!
!  INFO    (input) INTEGER
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!
      WRITE (*,FMT=99001) Srname , Info
!
      STOP
!
99001 FORMAT (' ** On entry to ',A6,' parameter number ',I2,' had ',    &
     &        'an illegal value')
!
!     End of XERBLA
!
      END SUBROUTINE XERBLA
