C RUN: %dragonegg -S %s -o - -O1 -ftree-vectorize -fplugin-arg-dragonegg-enable-gcc-optzns -fdefault-integer-8 -ffixed-line-length-120 -fno-second-underscore
C PR13561

      SUBROUTINE scratc(hpspnp)
      IMPLICIT NONE

      INTEGER HEAPDM
      PARAMETER (HEAPDM=2)
      INTEGER HEAP(HEAPDM)
      COMMON // HEAP

      integer hpspnp(*)
      INTEGER N

      INTEGER p_UPHBLS
      COMMON p_UPHBLS

      LOGICAL UPBOND, UPANGL, UPDIHE, UPIMPR
      COMMON  /UPCOD/ UPBOND, UPANGL, UPDIHE, UPIMPR

      UPBOND=.TRUE.
      UPANGL=.TRUE.
      UPDIHE=.TRUE.
      UPIMPR=.TRUE.
      DO N=1,10
         call assign_logical(heap(p_UPHBLS),N,.TRUE.)
      ENDDO
      RETURN
      END
