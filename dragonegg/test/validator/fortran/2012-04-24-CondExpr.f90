! RUN: %dragonegg -S %s -O3 -fplugin-arg-dragonegg-enable-gcc-optzns
! PR12609
      MODULE MAIN1
      INTEGER :: IHOUR , ISEAS , IREC , IGRP , NUMREC , NUMGRP
      REAL , ALLOCATABLE :: SHVALS(:,:,:,:,:)
      INTEGER :: NSEAHR(4,24) , NSEACM(4,24)
      END

      PROGRAM SHAVE
      USE MAIN1
      REAL :: SNUM
      DO ISEAS = 1 , 4
         DO IHOUR = 1 , 24
            SNUM = NSEAHR(ISEAS,IHOUR) - NSEACM(ISEAS,IHOUR)
            DO IGRP = 1 , NUMGRP
               DO IREC = 1 , NUMREC
                  SHVALS(IREC,IGRP,ISEAS,IHOUR,1) = (1./SNUM)           &
     &               *SHVALS(IREC,IGRP,ISEAS,IHOUR,1)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      CONTINUE
      END
