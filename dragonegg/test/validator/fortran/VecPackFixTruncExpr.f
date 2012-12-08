C RUN: %dragonegg %s -S -msse3 -ffast-math -funroll-loops -O3 -fplugin-arg-dragonegg-enable-gcc-optzns
C PR12664
      SUBROUTINE MLIST()
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3) , HIN(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      EQUIVALENCE (XIN(1,-2),HIN(1,1))
      IF ( LPBcsm.GT.0 ) THEN
         DO i = 1 , MOLsp
            boxjmp = PBCx*INT(X0(1,i)+SIGN(HALf,X0(1,i)))
            X0(1,i) = X0(1,i) - boxjmp
            XIN(1,i) = XIN(1,i) - boxjmp
            boxjmp = PBCy*INT(X0(2,i)+SIGN(HALf,X0(2,i)))
            X0(2,i) = X0(2,i) - boxjmp
            XIN(2,i) = XIN(2,i) - boxjmp
            boxjmp = PBCz*INT(X0(3,i)+SIGN(HALf,X0(3,i)))
            X0(3,i) = X0(3,i) - boxjmp
            XIN(3,i) = XIN(3,i) - boxjmp
         ENDDO
      ENDIF
      END
