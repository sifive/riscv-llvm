MODULE module_ra_gfdleta
      INTEGER, PARAMETER              :: NBLY=15
      REAL   , SAVE :: EM1(28,180),EM1WDE(28,180),TABLE1(28,180),     &
                           SOURCE(28,NBLY), DSRCE(28,NBLY)
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:,:) :: CO251,CDT51,CDT58,C2D51,&
                                           C2D58,CO258
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:)   :: STEMP,GTEMP,CO231,CO238, &
                                           C2DM51,C2DM58
CONTAINS
      SUBROUTINE GFDLETAINIT(SFULL,SHALF,PPTOP,JULYR,MONTH,IDAY,GMT,    &
     &                       kds,kde,kms,kme,kts,kte)
      END SUBROUTINE GFDLETAINIT
      SUBROUTINE ETARA(DT,THRATEN,THRATENLW,THRATENSW,PI3D              & 
     &                ,XLAND,p8w,dz8w,RHO_PHY,P_PHY,T                   &
     &                ,QV,QL,TSK2D,GLW,GSW                              &
     &                ,TOTSWDN,TOTLWDN,RSWTOA,RLWTOA,CZMEAN             & !Added
     &                ,GLAT,GLON,HTOP,HBOT,ALBEDO,CUPPT                 &
     &                ,VEGFRA,SNOW,G,GMT                                & !Modified
     &                ,NSTEPRA,NPHS,itimestep                           & !Modified
     &                ,julyr,julday,gfdl_lw,gfdl_sw                     &
     &                ,CFRACL,CFRACM,CFRACH                             & !Added
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                      & !Added
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,NPHS,NSTEPRA
      INTEGER,INTENT(INOUT),DIMENSION(ims:ime,jms:jme) :: NCFRST        & !Added
                                                         ,NCFRCV          !Added
      REAL,INTENT(INOUT),DIMENSION(ims:ime, kms:kme, jms:jme)::         &
                                         THRATEN,THRATENLW,THRATENSW
      REAL,INTENT(IN),DIMENSION(ims:ime, kms:kme, jms:jme)::p8w,dz8w,   &
     &                                                      PI3D
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: ALBEDO,SNOW,      &
     &                                                TSK2D,VEGFRA,     &
     &                                                XLAND
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: GLAT,GLON
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: HTOP,HBOT,CUPPT
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: RSWTOA,        & !Added
     &                                                   RLWTOA,        & !Added
     &                                                   ACFRST,        & !Added
     &                                                   ACFRCV
      REAL,INTENT(INOUT),DIMENSION(ims:ime, jms:jme):: GLW,GSW
      REAL,INTENT(OUT),DIMENSION(ims:ime, jms:jme):: CZMEAN,            &
     &                                               TOTLWDN,TOTSWDN
      REAL,INTENT(OUT),DIMENSION(ims:ime, jms:jme):: CFRACL,CFRACM,     & !Added
     &                                               CFRACH               !Added
      LOGICAL, INTENT(IN) :: gfdl_lw,gfdl_sw
      REAL, DIMENSION(its:ite, kms:kme, jts:jte):: PFLIP,QFLIP,QLFLIP,  &
     &                                             TFLIP
      REAL, DIMENSION(its:ite, kms:kme, jts:jte)::P8WFLIP,PHYD
      REAL, DIMENSION(its:ite, kts:kte, jts:jte)::TENDS,TENDL
      INTEGER :: IDAT(3),Jmonth,Jday
      DO J=JTS,JTE
      DO I=ITS,ITE
      ENDDO
      ENDDO
      DO K = KMS,KME
         DO J = jts,jte
         DO I = its,ite
         ENDDO
         ENDDO
      ENDDO
      CALL RADTN (DT,TFLIP,QFLIP,QLFLIP,PFLIP,P8WFLIP,XLAND,TSK2D,      &
     &            GLAT,GLON,HTOP,HBOT,ALBEDO,CUPPT,                     &
     &            ACFRCV,NCFRCV,ACFRST,NCFRST,                          &
     &            VEGFRA,SNOW,GLW,GSW,                                  &
     &            TOTSWDN,TOTLWDN,                                      & !Added
     &            IDAT,IHRST,                                           &
     &            NSTEPRA,NSTEPRA,NPHS,itimestep,                       & !Modified
     &            TENDS,TENDL,RSWTOA,RLWTOA,CZMEAN,                     &
     &            CFRACL,CFRACM,CFRACH,                                 & !Added
     &            ids,ide, jds,jde, kds,kde,                            &
     &            ims,ime, jms,jme, kms,kme,                            &
     &            its,ite, jts,jte, kts,kte                          )
      IF ( gfdl_lw ) then
        DO J=JTS,JTE
        DO K = KTS,KTE
          DO I=ITS,ITE
          ENDDO
        ENDDO
        ENDDO
      ENDIF
      IF ( gfdl_sw ) then
      DO J=JTS,JTE
      DO K = KTS,KTE
      ENDDO
      ENDDO
      ENDIF
      DO J=JTS,JTE
      DO I=ITS,ITE
      ENDDO
      ENDDO
 100  IF ( gfdl_sw ) then
        DO J=JTS,JTE
        DO K = KTS,KTE
          DO I=ITS,ITE
          ENDDO
        ENDDO
        ENDDO
      ENDIF
  END SUBROUTINE ETARA
      SUBROUTINE RADTN(DT,T,Q,CWM,PFLIP,P8WFLIP,XLAND,TSK2D,            &
     &                 GLAT,GLON,HTOP,HBOT,ALB,CUPPT,                   &
     &                 ACFRCV,NCFRCV,ACFRST,NCFRST,                     &
     &                 VEGFRC,SNO,GLW,GSW,                              & 
     &                 RSWIN,RLWIN,                                     & !Added
     &                 IDAT,IHRST,                                      &
     &                 NRADS,NRADL,NPHS,NTSD,                           &
     &                 TENDS,TENDL,RSWTOA,RLWTOA,CZMEAN,                &
     &                 CFRACL,CFRACM,CFRACH,                            & !Added
     &                 ids,ide, jds,jde, kds,kde,                       &
     &                 ims,ime, jms,jme, kms,kme,                       &
     &                 its,ite, jts,jte, kts,kte                       )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,         &
     &                              its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN), DIMENSION(3) :: IDAT
      REAL,    PARAMETER :: CAPA=287.04/1004.6,DTR=3.1415926/180.,      &
     &                      WA=.10,WG=1.-WA,KSMUD=0
      REAL,    PARAMETER :: SLPM=1.01325E5,EPSQ1=1.E-5,EPSQ=1.E-12,     &
     &                      PI2=2.*3.14159265,RLAG=14.8125
      INTEGER, PARAMETER :: NB=12
      LOGICAL :: SHORT,LONG
      LOGICAL :: BITX,BITY,BITZ,BITW,BIT1,BIT2,BITC,BITS,BITCP1,BITSP1
      LOGICAL :: CNCLD
      REAL, INTENT(IN), DIMENSION(ims:ime,jms:jme) :: XLAND,TSK2D
      REAL, INTENT(IN), DIMENSION(its:ite, kms:kme, jts:jte):: Q,CWM,T
      REAL, INTENT(IN), DIMENSION(its:ite, kms:kme, jts:jte):: PFLIP,   &
     &                                                         P8WFLIP
      REAL, INTENT(OUT), DIMENSION(ims:ime, jms:jme):: GLW,GSW,CZMEAN   &
     &                                                ,RSWIN,RLWIN      & !Added
     &                                                ,CFRACL,CFRACM    &
     &                                                ,CFRACH
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme)  :: HTOP,HBOT
      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: ALB,SNO
      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: GLAT,GLON
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: CUPPT
      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: ACFRCV,ACFRST &
                                                          ,RSWTOA,RLWTOA
      INTEGER,INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: NCFRCV,NCFRST
      REAL,   INTENT(IN),   DIMENSION(ims:ime,jms:jme) :: VEGFRC
      REAL,   INTENT(INOUT),DIMENSION(its:ite,kts:kte,jts:jte) :: TENDL,&
     &                                                            TENDS 
      REAL,   DIMENSION(its:ite)  :: PSFC,TSKN,ALBEDO,XLAT,COSZ,        &
     &                               SLMSK,CV,SV,FLWUP,                 &
     &                               FSWDN,FSWUP,FSWDNS,FSWUPS,FLWDNS,  &
     &                               FLWUPS
      REAL,   DIMENSION(its:ite,kts:kte) :: PMID,TMID
      REAL,   DIMENSION(its:ite,kts:kte) :: QMID,THMID,OZN,POZN
      REAL,   DIMENSION(its:ite,kts:kte+1) :: PINT,EMIS,CAMT
      INTEGER,DIMENSION(its:ite,kts:kte+1) :: ITYP,KBTM,KTOP
      INTEGER,DIMENSION(its:ite)   :: NCLDS,KCLD 
      REAL,   DIMENSION(its:ite)   :: CSTR,TAUC,TAUDAR
      REAL,   DIMENSION(its:ite,NB,kts:kte+1) ::RRCL,TTCL
      REAL    :: CWMKL,TMT15,AI,BI,PP,QW,P1,CC2,CC1,PMOD,CLPFIL,        &
     &           DTHDP,DDP
      INTEGER :: I,J,MYJS,MYJE,MYIS,MYIE,NTSPH,NRADPP,ITIMSW,ITIMLW,    &
     &           JD,II
      IF(SHORT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
        ENDDO
        ENDDO
        DO II=0,NRADS,NPHS
        ENDDO
      ENDIF
      DO I=MYIS,MYIE
      ENDDO
      DO L=1,LML
      ENDDO
      IF(LVLIJ.GT.0)THEN
        DO L=LVLIJ,1,-1
        ENDDO
      ENDIF
      IF(LVLIJ.EQ.0) THEN
      ENDIF
      IF ((XLAND(I,J)-1.5) .gt. 0.) then
      ENDIF
      IF(RQKL.GE.0.9999)THEN                                      
      ENDIF                                                             
      IF((XLAND(I,J)-1.) .LT. 0.5)THEN
        DO L=1,LML
        ENDDO
      ENDIF
      IF((XLAND(I,J)-1.) .GT. 0.5)THEN
        DO L=1,LML-1
          IF(DTHDP.LE.CLAPSE)THEN
          ENDIF
        ENDDO
        IF(CSMID(I,LBASE-1).LE.0..AND.CSMID(I,LBASE-2).LE.0. &
                                     .AND.LBASE.LT.LM)THEN
          IF(DTHDP.GT.CLPSE)THEN
          ENDIF
          DO L=1,LML
          ENDDO
          DO L=1,LML
          ENDDO
        ENDIF
      ENDIF
      DO L=1,LML
      ENDDO
      DO LL=L400,2,-1
        IF(DTHDP.LT.-0.0025.OR.QMID(I,LL).LE.EPSQ1)THEN
        ENDIF
      ENDDO
  340 IF(LTROP.LT.LM)THEN
        DO LL=LTROP,1,-1
        ENDDO
      ENDIF
      IF(BIT1)THEN
        IF(ITYP(I,KCLD(I)).EQ.0)THEN
          IF(BITC)THEN
          ENDIF 
          IF(BITC)THEN
            IF(BITCP1)THEN
            ENDIF
          ENDIF 
        ENDIF 
      ENDIF
      IF(NCLD.GE.1)THEN
        IF(LL.GE.KTOP(I,NC).AND.LL.LE.KBTM(I,NC).AND.BITX)THEN
          IF(ITYP(I,NC).EQ.2 &
                 .OR.PINT(I,KTOP(I,NC)).LE.PTOPC(3))THEN
            IF(TCLD.LE.-10.0)THEN
            ENDIF
            IF(TCLD.LE.-20.0)THEN
              TAUC(I)=TAUC(I)+DELP*AMAX1(0.1E-3,2.56E-5* &
                     (TCLD+82.5)**2)
            ENDIF
          ENDIF
        ENDIF
        IF(QSUM.GE.EPSQ1)THEN
          IF(BITX)THEN
            IF(ABS(EEX).GE.1.E-8)THEN
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      DO L=1,LM
      DO I=MYIS,MYIE
      ENDDO
      ENDDO
      CALL OZON2D(LM,POZN,XLAT,RSIN1,RCOS1,RCOS2,OZN,              &
                  MYIS,MYIE,                                       &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte                        )
      CALL RADFS &
           (PSFC,PMID,PINT,QMID,TMID,OZN,TSKN,SLMSK,ALBEDO,XLAT &
      ,     CAMT,KTOP,KBTM,NCLDS,EMIS,RRCL,TTCL                 &
      ,     COSZ,TAUDAR,1                                       &
      ,     1,0                                                 &
      ,     ITIMSW,ITIMLW,JD,HOUR                               &
      ,     TENDS(its,kts,j),TENDL(its,kts,j)                   &
      ,     FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS       &
      ,     ids,ide, jds,jde, kds,kde                           &
      ,     ims,ime, jms,jme, kms,kme                           &
      ,     its,ite, jts,jte, kts,kte                           )
      IF(LONG)THEN
        DO I=MYIS,MYIE
        ENDDO
      ENDIF
      IF(SHORT)THEN
        IF(CNCLD)THEN
          IF(PMOD.LE.PPT(1))THEN
          ENDIF
        ENDIF
      ENDIF
      DO L=1,LM
      ENDDO
      IF(LONG)THEN
      ENDIF
      IF(SHORT) THEN
      ENDIF
      IF(LONG)THEN
      ENDIF
      END SUBROUTINE RADTN
      SUBROUTINE ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,     &
                        its,ite, jts,jte, kts,kte)
      REAL,    PARAMETER :: GSTC1=24110.54841,GSTC2=8640184.812866,    &
                            ZEROJD=2451545.0
      REAL    :: DAY,YFCTR,ADDDAY,STARTYR,DATJUL,DIFJD,SLONM,   &
                 ANOM,SLON,DEC,RA,DATJ0,TU,STIM0,SIDTIM,HRANG
      LOGICAL :: LEAP
      IF(MOD(IDAT(3),4).EQ.0)THEN
      ENDIF
      IF(DAYI.GT.365.)THEN
        IF(.NOT.LEAP)THEN
        ENDIF
      ENDIF
      END SUBROUTINE ZENITH
  SUBROUTINE OZON2D (LK,POZN,XLAT,RSIN1,RCOS1,RCOS2,QO3,              &
                     MYIS,MYIE,                                       &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte                        )
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte) :: POZN
      REAL,    INTENT(IN), DIMENSION(its:ite)  :: XLAT
      REAL,    INTENT(INOUT), DIMENSION(its:ite,kts:kte) :: QO3
      DO I=MYIS,MYIE
      ENDDO
      DO I=MYIS,MYIE
        IF(POZN(I,K).LT.PRGFDL(JJROW(I)-1))THEN
        ENDIF
      ENDDO
      IF(POZN(I,K).LT.PRGFDL(1))THEN
        QO3(I,K)=QO3O3(I,JJROW(I))+(ALOG(POZN(I,K))-APHI)/ &
                   (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
      ENDIF
  END SUBROUTINE OZON2D
      SUBROUTINE O3INT(PHALF,DDUO3N,DDO3N2,DDO3N3,DDO3N4, &
                 its,ite, jts,jte, kts,kte             )
      REAL ::   O3HI(10,25),O3LO1(10,16),O3LO2(10,16),O3LO3(10,16), &
                O3LO4(10,16)
      REAL ::   O3HI1(10,16),O3HI2(10,9),PH1(45),PH2(37),P1(48),P2(33)
      REAL ::   RSTD(81),RO3(10,41),RO3M(10,40),RBAR(kts:kte),RDATA(81), &
                PHALF(kts:kte+1),P(81),PH(82)
      EQUIVALENCE (PH1(1),PH(1)),(PH2(1),PH(46))
      DATA PH1/      0., &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04, &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04, &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04, &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03, &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03, &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03, &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03, &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03, &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02, &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02, &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/
      DATA PH2/ &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02, &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01, &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01, &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01, &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01, &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00, &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00, &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00, &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00, &
           0.1000000E+01/
      DATA P1/ &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04, &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04, &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04, &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03, &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03, &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03, &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03, &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03, &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02, &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02, &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02, &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/
      DATA O3HI1/ &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30, &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35, &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38, &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41, &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45, &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47, &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60, &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71, &
       3.6,3.8,3.9,4.2,4.7,5.3,5.6,5.7,5.5,5.2, &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2, &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7, &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2, &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3, &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5, &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/
      DATA O3LO1/ &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4, &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4, &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2, &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1, &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9, &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1, &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2, &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9, &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5, &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6, &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90, &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40, &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13, &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07, &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06, &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/
      DATA O3LO2/ &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9, &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6, &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1, &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8, &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5, &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0, &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1, &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0, &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0, &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80, &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40, &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15, &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07, &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05, &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04, &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/
      DATA O3LO3/ &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3, &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8, &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7, &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5, &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4, &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2, &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6, &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3, &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0, &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8, &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90, &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25, &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08, &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05, &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04, &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/
      DATA O3LO4/ &
       14.2,13.8,13.2,12.5,11.7,10.5,8.6,7.8,7.5,6.6, &
       12.5,12.4,12.2,11.7,10.8,9.8,7.8,7.2,6.5,6.1, &
       10.6,10.5,10.4,10.1,9.6,9.0,7.1,6.8,6.1,5.9, &
       7.0,7.4,7.9,7.8,7.6,7.3,6.2,6.1,5.8,5.6, &
       4.2,4.6,5.1,5.6,5.9,5.9,5.9,5.8,5.6,5.3, &
       2.1,2.3,2.6,2.9,3.5,4.3,4.8,4.9,5.1,5.1, &
       0.7,0.8,1.0,1.5,2.0,2.8,3.5,3.6,3.7,4.0, &
       .15,.20,.40,.50,.60,1.4,2.1,2.2,2.3,2.5, &
       .08,.10,.15,.25,.30,.90,1.2,1.3,1.4,1.6, &
       .07,.08,.10,.14,.20,.50,.70,.90,.90,.80, &
       .05,.06,.08,.12,.14,.20,.35,.40,.60,.50, &
       .05,.05,.08,.09,.09,.09,.11,.12,.15,.18, &
       .04,.05,.06,.07,.07,.08,.08,.08,.08,.08, &
       .04,.04,.05,.07,.07,.07,.07,.07,.06,.05, &
       .02,.02,.04,.05,.05,.05,.05,.05,.04,.04, &
       .02,.02,.03,.04,.04,.04,.04,.04,.03,.03/
      IF(PH(K).LT.PHALF(KK).AND.PH(K+1).GE.PHALF(KK+1)) RBAR(KK)=RDATA(K)
      IF (IPLACE.EQ.1) THEN
      END IF
      END SUBROUTINE O3INT
  SUBROUTINE CLO89(CLDFAC,CAMT,NCLDS,KBTM,KTOP                  &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          its,ite, jts,jte, kts,kte                      )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    its,ite, jts,jte, kts,kte
      IF (NCLDS(IR).EQ.0) THEN
          IF(K2+1.LE.K1-1) THEN
          ENDIF
      ENDIF
  END SUBROUTINE CLO89
      SUBROUTINE LWR88(HEATRA,GRNFLX,TOPFLX,                         &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       ids,ide, jds,jde, kds,kde,                    &
                       its,ite, jts,jte, kts,kte                     )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    its,ite, jts,jte, kts,kte  
      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM
      REAL,    DIMENSION(its:ite,kts:kte+1) :: TEXPSL,TOTPHI,TOTO3,CNTVAL,&
                                               CO2R2,D2CD22,DCO2D2,CO2SP1,&
                                               TLSQU,DIFT
      REAL,    DIMENSION(its:ite,kts:kte)   :: DELP2,DELP,CO2NBL,&
                                               QH2O,VV,VAR1,VAR2,VAR3,VAR4
      REAL,    DIMENSION(its:ite,kts:kte*2+1):: EMPL
      REAL,    DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CO21
      EMPL(I,LP2+K-1)=QH2O(I,K+1)*P(I,K+1)*(PRESS(I,K+1)-P(I,K+1)) &
                     *GP0INV
      CO2SP1(I,K)=CO2R1(I,K)+DIFT(I,K)*(DCO2D1(I,K)+HAF*DIFT(I,K)* &
       D2CD22(I,K))
      DIFT(I,KP)=(TDAV(I,KP)-TDAV(I,K))/ &
                    (TSTDAV(I,KP)-TSTDAV(I,K))
      CO21(I,KP,K)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
  END SUBROUTINE LWR88
  SUBROUTINE FST88(HEATRA,GRNFLX,TOPFLX, &
                       QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
                       CO21,CO2NBL,CO2SP1,CO2SP2, &
                       VAR1,VAR2,VAR3,VAR4,CNTVAL, &
                       ids,ide, jds,jde, kds,kde,                    &
                       its,ite, jts,jte, kts,kte                     )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: CO2NBL,DELP2, &
                                               VAR1,VAR2,VAR3,VAR4
      REAL, DIMENSION(its:ite,kts:kte+1) :: VTMP3,FXO,DT,FXOE2,DTE2, &
                                            CTSO3,CTS
  END SUBROUTINE FST88
  SUBROUTINE E1E290(G1,G2,G3,G4,G5,EMISS,FXOE1,DTE1,FXOE2,DTE2,      &
                       its,ite, jts,jte, kts,kte                     )
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte+1) :: G1,G4,G3,EMISS
      G1(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1V(IT1(I,K+1))+ &
              DTE1(I,K)*DU(I,K)*EM1V(IT1(I,K+LP2-1)+29)
      G3(I,KP)=WW1(I,1)*WW2(I,KP)*EM1V(IT1(I,LL+KP))+ &
              DTE1(I,1)*DU(I,KP-1)*EM1V(IT1(I,LL+KP)+29)
      G4(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1VW(IT1(I,K+1))+ &
              DTE1(I,K)*DU(I,K)*EM1VW(IT1(I,K+LP2-1)+29)
  END SUBROUTINE E1E290
 SUBROUTINE SPA88(EXCTS,CTSO3,GXCTS,SORC,CSOUR,                      &
                       RADCON,                                 &
                       ids,ide, jds,jde, kds,kde,                    &
                       its,ite, jts,jte, kts,kte                     )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                         RADCON
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)  :: CTSO3
      CTSO3(I,K)=RADCON*DELP(I,K)* &
           (CSOUR(I,K)*(CTMP2(I,K+1)-CTMP2(I,K)) + &
            SORC(I,K,13)*(CTMP3(I,K+1)-CTMP3(I,K)))
 END SUBROUTINE SPA88
 SUBROUTINE E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2, &
                       ids,ide, jds,jde, kds,kde,                    &
                       its,ite, jts,jte, kts,kte                     )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    its,ite, jts,jte, kts,kte
 END SUBROUTINE E290
  SUBROUTINE E2SPEC(EMISS,AVEPHI,FXOSP,DTSP,                         &
                       its,ite, jts,jte, kts,kte                     )
  END SUBROUTINE E2SPEC
  SUBROUTINE E3V88(EMV,TV,AV, &
                       ids,ide, jds,jde, kds,kde,                    &
                       its,ite, jts,jte, kts,kte                     )
      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    its,ite, jts,jte, kts,kte
      REAL, INTENT(OUT), DIMENSION(its:ite,kts:kte*2+1) :: EMV
      REAL,DIMENSION(its:ite,kts:kte*2+1) ::FXO,TMP3,DT,WW1,WW2,DU,&
                                            FYO
        EMV(I,K)=WW1(I,K)*WW2(I,K)*EM3V(IT(I,K)-9)+ &
                 DT(I,K)*DU(I,K)*EM3V(IT(I,K)+20)
  END SUBROUTINE E3V88
  SUBROUTINE SWR93(FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,             &
                       its,ite, jts,jte, kts,kte                      )
  END SUBROUTINE SWR93
  SUBROUTINE RADFS & 
                (QS,PP,PPI,QQH2O,TT,O3QO3,TSFC,SLMSK,ALBEDO,XLAT &
      ,          CAMT,KTOP,KBTM,NCLDS,EMCLD,RRCL,TTCL &
      ,          COSZRO,TAUDAR,IBEG &
      ,          KO3,KALB &
      ,          ITIMSW,ITIMLW &
      ,          JD,GMT &
      ,          SWH,HLW &
      ,          FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS  &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          ims,ime, jms,jme, kms,kme                      &
      ,          its,ite, jts,jte, kts,kte                      )
 INTEGER, PARAMETER :: NB=12
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: PP,TT
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: QQH2O
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1):: PPI,CAMT,EMCLD
      REAL,    INTENT(IN), DIMENSION(its:ite):: QS,TSFC,SLMSK,ALBEDO,XLAT
      REAL,    INTENT(IN), DIMENSION(its:ite):: COSZRO,TAUDAR
      REAL,    INTENT(OUT), DIMENSION(its:ite):: FLWUPS
      INTEGER, INTENT(IN), DIMENSION(its:ite):: NCLDS
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1):: KTOP,KBTM
      REAL,    INTENT(INOUT), DIMENSION(its:ite,NB,kts:kte+1):: TTCL,RRCL
      REAL, intent(IN), DIMENSION(its:ite,kts:kte):: O3QO3
      REAL,  DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                BCOMB,BETACM
      DATA ACOMB  / &
        -0.106346E-02,  0.641531E-02,  0.137362E-01,  0.922513E-02, &
         0.136162E-01,  0.169791E-01,  0.206959E-01,  0.166223E-01, &
         0.171776E-01,  0.229724E-01,  0.275530E-01,  0.302731E-01, &
         0.281662E-01,  0.199525E-01,  0.370962E-01/
       REAL, INTENT(OUT), DIMENSION(its:ite):: FSWUP,FSWUPS,FSWDN, &
                           FSWDNS,FLWUP,FLWDNS
      REAL,   DIMENSION(21,20) :: ALBD
      REAL,   DIMENSION(21)    :: TRN
      DATA TRN/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65, &
               .70,.75,.80,.85,.90,.95,1.00/
      REAL ::  ALB11(21,7),ALB22(21,7),ALB33(21,6)
      EQUIVALENCE (ALB11(1,1),ALBD(1,1)),(ALB22(1,1),ALBD(1,8)), &
                  (ALB33(1,1),ALBD(1,15))
      DATA ALB11/ .061,.062,.072,.087,.115,.163,.235,.318,.395,.472,.542, &
       .604,.655,.693,.719,.732,.730,.681,.581,.453,.425,.061,.062,.070, &
       .083,.108,.145,.198,.263,.336,.415,.487,.547,.595,.631,.656,.670, &
       .652,.602,.494,.398,.370,.061,.061,.068,.079,.098,.130,.174,.228, &
       .290,.357,.424,.498,.556,.588,.603,.592,.556,.488,.393,.342,.325, &
       .061,.061,.065,.073,.086,.110,.150,.192,.248,.306,.360,.407,.444, &
       .469,.480,.474,.444,.386,.333,.301,.290,.061,.061,.065,.070,.082, &
       .101,.131,.168,.208,.252,.295,.331,.358,.375,.385,.377,.356,.320, &
       .288,.266,.255,.061,.061,.063,.068,.077,.092,.114,.143,.176,.210, &
       .242,.272,.288,.296,.300,.291,.273,.252,.237,.266,.220,.061,.061, &
       .062,.066,.072,.084,.103,.127,.151,.176,.198,.219,.236,.245,.250, &
       .246,.235,.222,.211,.205,.200/
      DATA ALB22/ .061,.061,.061,.065,.071,.079,.094,.113,.134,.154,.173, &
       .185,.190,.193,.193,.190,.188,.185,.182,.180,.178,.061,.061,.061, &
       .064,.067,.072,.083,.099,.117,.135,.150,.160,.164,.165,.164,.162, &
       .160,.159,.158,.157,.157,.061,.061,.061,.062,.065,.068,.074,.084, &
       .097,.111,.121,.127,.130,.131,.131,.130,.129,.127,.126,.125,.122, &
       .061,.061,.061,.061,.062,.064,.070,.076,.085,.094,.101,.105,.107, &
       .106,.103,.100,.097,.096,.095,.095,.095,.061,.061,.061,.060,.061, &
       .062,.065,.070,.075,.081,.086,.089,.090,.088,.084,.080,.077,.075, &
       .074,.074,.074,.061,.061,.060,.060,.060,.061,.063,.065,.068,.072, &
       .076,.077,.076,.074,.071,.067,.064,.062,.061,.061,.061,.061,.061, &
       .060,.060,.060,.060,.061,.062,.065,.068,.069,.069,.068,.065,.061, &
       .058,.055,.054,.053,.052,.052/
      DATA ALB33/ .061,.061,.060,.060,.060,.060,.060,.060,.062,.065,.065, &
       .063,.060,.057,.054,.050,.047,.046,.045,.044,.044,.061,.061,.060, &
       .060,.060,.059,.059,.059,.059,.059,.058,.055,.051,.047,.043,.039, &
       .035,.033,.032,.031,.031,.061,.061,.060,.060,.060,.059,.059,.058, &
       .057,.056,.054,.051,.047,.043,.039,.036,.033,.030,.028,.027,.026, &
       .061,.061,.060,.060,.060,.059,.059,.058,.057,.055,.052,.049,.045, &
       .040,.036,.032,.029,.027,.026,.025,.025,.061,.061,.060,.060,.060, &
       .059,.059,.058,.056,.053,.050,.046,.042,.038,.034,.031,.028,.026, &
       .025,.025,.025,.061,.061,.060,.060,.059,.058,.058,.057,.055,.053, &
       .050,.046,.042,.038,.034,.030,.028,.029,.025,.025,.025/
 1000 FORMAT(1H ,' YOU ARE CALLING GFDL RADIATION CODE FOR',I5,' PTS', &
                 'AND',I4,' LYRS,WITH KDAPRX,KO3,KCZ,KEMIS,KALB = ',5I2)
  END SUBROUTINE RADFS 
    SUBROUTINE O3CLIM
       INTEGER, PARAMETER :: NL=81,NLP1=NL+1,NLGTH=37*NL,NKK=41,NK=81,NKP=NK+1
       REAL :: PH1(45),PH2(37),P1(48),P2(33),O3HI1(10,16),O3HI2(10,9) &
              ,O3LO1(10,16),O3LO2(10,16),O3LO3(10,16),O3LO4(10,16)
       REAL :: PSTD(NL),TEMPN(19),O3O3(37,NL,4),O35DEG(37,NL) &
      ,PHALF(NL),P(81),PH(82)
      DATA PH1/      0.,     &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04,     &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04,     &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04,     &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03,     &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03,     &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03,     &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03,     &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03,     &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02,     &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02,     &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/     
      DATA PH2/     &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02,     &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01,     &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01,     &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01,     &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01,     &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00,     &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00,     &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00,     &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00,     &
           0.1000000E+01/     
      DATA P1/     &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04,     &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04,     &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04,     &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03,     &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03,     &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03,     &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03,     &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03,     &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02,     &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02,     &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02,     &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/     
      DATA P2/     &
           0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01,     &
           0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01,     &
           0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01,     &
           0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01,     &
           0.1015345E+00, 0.1189603E+00, 0.1391863E+00, 0.1630739E+00,     &
           0.1908004E+00, 0.2235461E+00, 0.2609410E+00, 0.3036404E+00,     &
           0.3513750E+00, 0.4055375E+00, 0.4656677E+00, 0.5335132E+00,     &
           0.6083618E+00, 0.6923932E+00, 0.7845676E+00, 0.8875882E+00,     &
           0.1000000E+01/     
      DATA O3HI1/     &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30,     &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35,     &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38,     &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41,     &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45,     &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47,     &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60,     &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71,     &
       1.10,1.05,1.00,.90,.90,.90,.85,.83,.80,.80,        &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2,     &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7,     &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2,     &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3,     &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5,  &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/     
      DATA O3LO1/     &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4,  &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4,   &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2,    &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1,     &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9,     &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1,     &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2,     &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9,     &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5,     &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6,     &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90,     &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40,     &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13,     &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07,     &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06,     &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/     
      DATA O3LO2/     &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9,   &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6,   &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1,    &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8,     &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5,     &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0,     &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1,     &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0,     &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0,     &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80,     &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40,     &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15,    &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07,  &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05,  &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/     
      DATA O3LO3/     &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3,    &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8,     &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7,     &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5,     &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4,     &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2,     &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6,     &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3,     &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0,     &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8,     &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90,     &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25,     &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08,     &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05,     &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/      
      DO I=1,NLGTH
      ENDDO
      DO N=1,NL
      ENDDO
    END SUBROUTINE O3CLIM
      SUBROUTINE TABLE 
      INTEGER :: IBAND(40)
      REAL :: BANDL1(64),BANDL2(64),BANDL3(35)
      REAL :: BANDH1(64),BANDH2(64),BANDH3(35) 
      REAL ::  &
               R1T(28),R2(28),S2(28),T3(28),R1WD(28)
      REAL ::   ARNDM1(64),ARNDM2(64),ARNDM3(35)
      REAL ::   BRNDM1(64),BRNDM2(64),BRNDM3(35)
      REAL ::   BETAD1(64),BETAD2(64),BETAD3(35)
      DATA IBAND  / &
          2,   1,   2,   2,   1,   2,   1,   3,   2,   2, &
          3,   2,   2,   4,   2,   4,   2,   3,   3,   2, &
          4,   3,   4,   3,   7,   5,   6,   7,   6,   5, &
          7,   6,   7,   8,   6,   6,   8,   8,   8,   8/
      DATA BANDL1 / &
         0.000000E+00,  0.100000E+02,  0.200000E+02,  0.300000E+02, &
         0.400000E+02,  0.500000E+02,  0.600000E+02,  0.700000E+02, &
         0.800000E+02,  0.900000E+02,  0.100000E+03,  0.110000E+03, &
         0.120000E+03,  0.130000E+03,  0.140000E+03,  0.150000E+03, &
         0.160000E+03,  0.170000E+03,  0.180000E+03,  0.190000E+03, &
         0.200000E+03,  0.210000E+03,  0.220000E+03,  0.230000E+03, &
         0.240000E+03,  0.250000E+03,  0.260000E+03,  0.270000E+03, &
         0.280000E+03,  0.290000E+03,  0.300000E+03,  0.310000E+03, &
         0.320000E+03,  0.330000E+03,  0.340000E+03,  0.350000E+03, &
         0.360000E+03,  0.370000E+03,  0.380000E+03,  0.390000E+03, &
         0.400000E+03,  0.410000E+03,  0.420000E+03,  0.430000E+03, &
         0.440000E+03,  0.450000E+03,  0.460000E+03,  0.470000E+03, &
         0.480000E+03,  0.490000E+03,  0.500000E+03,  0.510000E+03, &
         0.520000E+03,  0.530000E+03,  0.540000E+03,  0.550000E+03, &
         0.560000E+03,  0.670000E+03,  0.800000E+03,  0.900000E+03, &
         0.990000E+03,  0.107000E+04,  0.120000E+04,  0.121000E+04/
      DATA BANDL2 / &
         0.122000E+04,  0.123000E+04,  0.124000E+04,  0.125000E+04, &
         0.126000E+04,  0.127000E+04,  0.128000E+04,  0.129000E+04, &
         0.130000E+04,  0.131000E+04,  0.132000E+04,  0.133000E+04, &
         0.134000E+04,  0.135000E+04,  0.136000E+04,  0.137000E+04, &
         0.138000E+04,  0.139000E+04,  0.140000E+04,  0.141000E+04, &
         0.142000E+04,  0.143000E+04,  0.144000E+04,  0.145000E+04, &
         0.146000E+04,  0.147000E+04,  0.148000E+04,  0.149000E+04, &
         0.150000E+04,  0.151000E+04,  0.152000E+04,  0.153000E+04, &
         0.154000E+04,  0.155000E+04,  0.156000E+04,  0.157000E+04, &
         0.158000E+04,  0.159000E+04,  0.160000E+04,  0.161000E+04, &
         0.162000E+04,  0.163000E+04,  0.164000E+04,  0.165000E+04, &
         0.166000E+04,  0.167000E+04,  0.168000E+04,  0.169000E+04, &
         0.170000E+04,  0.171000E+04,  0.172000E+04,  0.173000E+04, &
         0.174000E+04,  0.175000E+04,  0.176000E+04,  0.177000E+04, &
         0.178000E+04,  0.179000E+04,  0.180000E+04,  0.181000E+04, &
         0.182000E+04,  0.183000E+04,  0.184000E+04,  0.185000E+04/
      DATA BANDL3 / &
         0.186000E+04,  0.187000E+04,  0.188000E+04,  0.189000E+04, &
         0.190000E+04,  0.191000E+04,  0.192000E+04,  0.193000E+04, &
         0.194000E+04,  0.195000E+04,  0.196000E+04,  0.197000E+04, &
         0.198000E+04,  0.199000E+04,  0.200000E+04,  0.201000E+04, &
         0.202000E+04,  0.203000E+04,  0.204000E+04,  0.205000E+04, &
         0.206000E+04,  0.207000E+04,  0.208000E+04,  0.209000E+04, &
         0.210000E+04,  0.211000E+04,  0.212000E+04,  0.213000E+04, &
         0.214000E+04,  0.215000E+04,  0.216000E+04,  0.217000E+04, &
         0.218000E+04,  0.219000E+04,  0.227000E+04/
      DATA BANDH1 / &
         0.100000E+02,  0.200000E+02,  0.300000E+02,  0.400000E+02, &
         0.500000E+02,  0.600000E+02,  0.700000E+02,  0.800000E+02, &
         0.900000E+02,  0.100000E+03,  0.110000E+03,  0.120000E+03, &
         0.130000E+03,  0.140000E+03,  0.150000E+03,  0.160000E+03, &
         0.170000E+03,  0.180000E+03,  0.190000E+03,  0.200000E+03, &
         0.210000E+03,  0.220000E+03,  0.230000E+03,  0.240000E+03, &
         0.250000E+03,  0.260000E+03,  0.270000E+03,  0.280000E+03, &
         0.290000E+03,  0.300000E+03,  0.310000E+03,  0.320000E+03, &
         0.330000E+03,  0.340000E+03,  0.350000E+03,  0.360000E+03, &
         0.370000E+03,  0.380000E+03,  0.390000E+03,  0.400000E+03, &
         0.410000E+03,  0.420000E+03,  0.430000E+03,  0.440000E+03, &
         0.450000E+03,  0.460000E+03,  0.470000E+03,  0.480000E+03, &
         0.490000E+03,  0.500000E+03,  0.510000E+03,  0.520000E+03, &
         0.530000E+03,  0.540000E+03,  0.550000E+03,  0.560000E+03, &
         0.670000E+03,  0.800000E+03,  0.900000E+03,  0.990000E+03, &
         0.107000E+04,  0.120000E+04,  0.121000E+04,  0.122000E+04/
      DATA BANDH2 / &
         0.123000E+04,  0.124000E+04,  0.125000E+04,  0.126000E+04, &
         0.127000E+04,  0.128000E+04,  0.129000E+04,  0.130000E+04, &
         0.131000E+04,  0.132000E+04,  0.133000E+04,  0.134000E+04, &
         0.135000E+04,  0.136000E+04,  0.137000E+04,  0.138000E+04, &
         0.139000E+04,  0.140000E+04,  0.141000E+04,  0.142000E+04, &
         0.143000E+04,  0.144000E+04,  0.145000E+04,  0.146000E+04, &
         0.147000E+04,  0.148000E+04,  0.149000E+04,  0.150000E+04, &
         0.151000E+04,  0.152000E+04,  0.153000E+04,  0.154000E+04, &
         0.155000E+04,  0.156000E+04,  0.157000E+04,  0.158000E+04, &
         0.159000E+04,  0.160000E+04,  0.161000E+04,  0.162000E+04, &
         0.163000E+04,  0.164000E+04,  0.165000E+04,  0.166000E+04, &
         0.167000E+04,  0.168000E+04,  0.169000E+04,  0.170000E+04, &
         0.171000E+04,  0.172000E+04,  0.173000E+04,  0.174000E+04, &
         0.175000E+04,  0.176000E+04,  0.177000E+04,  0.178000E+04, &
         0.179000E+04,  0.180000E+04,  0.181000E+04,  0.182000E+04, &
         0.183000E+04,  0.184000E+04,  0.185000E+04,  0.186000E+04/
      DATA BANDH3 / &
         0.187000E+04,  0.188000E+04,  0.189000E+04,  0.190000E+04, &
         0.191000E+04,  0.192000E+04,  0.193000E+04,  0.194000E+04, &
         0.195000E+04,  0.196000E+04,  0.197000E+04,  0.198000E+04, &
         0.199000E+04,  0.200000E+04,  0.201000E+04,  0.202000E+04, &
         0.203000E+04,  0.204000E+04,  0.205000E+04,  0.206000E+04, &
         0.207000E+04,  0.208000E+04,  0.209000E+04,  0.210000E+04, &
         0.211000E+04,  0.212000E+04,  0.213000E+04,  0.214000E+04, &
         0.215000E+04,  0.216000E+04,  0.217000E+04,  0.218000E+04, &
         0.219000E+04,  0.220000E+04,  0.238000E+04/
      DATA ARNDM1  / &
         0.354693E+00,  0.269857E+03,  0.167062E+03,  0.201314E+04, &
         0.964533E+03,  0.547971E+04,  0.152933E+04,  0.599429E+04, &
         0.699329E+04,  0.856721E+04,  0.962489E+04,  0.233348E+04, &
         0.127091E+05,  0.104383E+05,  0.504249E+04,  0.181227E+05, &
         0.856480E+03,  0.136354E+05,  0.288635E+04,  0.170200E+04, &
         0.209761E+05,  0.126797E+04,  0.110096E+05,  0.336436E+03, &
         0.491663E+04,  0.863701E+04,  0.540389E+03,  0.439786E+04, &
         0.347836E+04,  0.130557E+03,  0.465332E+04,  0.253086E+03, &
         0.257387E+04,  0.488041E+03,  0.892991E+03,  0.117148E+04, &
         0.125880E+03,  0.458852E+03,  0.142975E+03,  0.446355E+03, &
         0.302887E+02,  0.394451E+03,  0.438112E+02,  0.348811E+02, &
         0.615503E+02,  0.143165E+03,  0.103958E+02,  0.725108E+02, &
         0.316628E+02,  0.946456E+01,  0.542675E+02,  0.351557E+02, &
         0.301797E+02,  0.381010E+01,  0.126319E+02,  0.548010E+01, &
         0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01, &
         0.178110E-01,  0.170166E+00,  0.273514E-01,  0.983767E+00/
      DATA ARNDM2  / &
         0.753946E+00,  0.941763E-01,  0.970547E+00,  0.268862E+00, &
         0.564373E+01,  0.389794E+01,  0.310955E+01,  0.128235E+01, &
         0.196414E+01,  0.247113E+02,  0.593435E+01,  0.377552E+02, &
         0.305173E+02,  0.852479E+01,  0.116780E+03,  0.101490E+03, &
         0.138939E+03,  0.324228E+03,  0.683729E+02,  0.471304E+03, &
         0.159684E+03,  0.427101E+03,  0.114716E+03,  0.106190E+04, &
         0.294607E+03,  0.762948E+03,  0.333199E+03,  0.830645E+03, &
         0.162512E+04,  0.525676E+03,  0.137739E+04,  0.136252E+04, &
         0.147164E+04,  0.187196E+04,  0.131118E+04,  0.103975E+04, &
         0.621637E+01,  0.399459E+02,  0.950648E+02,  0.943161E+03, &
         0.526821E+03,  0.104150E+04,  0.905610E+03,  0.228142E+04, &
         0.806270E+03,  0.691845E+03,  0.155237E+04,  0.192241E+04, &
         0.991871E+03,  0.123907E+04,  0.457289E+02,  0.146146E+04, &
         0.319382E+03,  0.436074E+03,  0.374214E+03,  0.778217E+03, &
         0.140227E+03,  0.562540E+03,  0.682685E+02,  0.820292E+02, &
         0.178779E+03,  0.186150E+03,  0.383864E+03,  0.567416E+01/ 
      DATA ARNDM3  / &
         0.225129E+03,  0.473099E+01,  0.753149E+02,  0.233689E+02, &
         0.339802E+02,  0.108855E+03,  0.380016E+02,  0.151039E+01, &
         0.660346E+02,  0.370165E+01,  0.234169E+02,  0.440206E+00, &
         0.615283E+01,  0.304077E+02,  0.117769E+01,  0.125248E+02, &
         0.142652E+01,  0.241831E+00,  0.483721E+01,  0.226357E-01, &
         0.549835E+01,  0.597067E+00,  0.404553E+00,  0.143584E+01, &
         0.294291E+00,  0.466273E+00,  0.156048E+00,  0.656185E+00, &
         0.172727E+00,  0.118349E+00,  0.141598E+00,  0.588581E-01, &
         0.919409E-01,  0.155521E-01,  0.537083E-02/
      DATA BRNDM1  / &
         0.789571E-01,  0.920256E-01,  0.696960E-01,  0.245544E+00, &
         0.188503E+00,  0.266127E+00,  0.271371E+00,  0.330917E+00, &
         0.190424E+00,  0.224498E+00,  0.282517E+00,  0.130675E+00, &
         0.212579E+00,  0.227298E+00,  0.138585E+00,  0.187106E+00, &
         0.194527E+00,  0.177034E+00,  0.115902E+00,  0.118499E+00, &
         0.142848E+00,  0.216869E+00,  0.149848E+00,  0.971585E-01, &
         0.151532E+00,  0.865628E-01,  0.764246E-01,  0.100035E+00, &
         0.171133E+00,  0.134737E+00,  0.105173E+00,  0.860832E-01, &
         0.148921E+00,  0.869234E-01,  0.106018E+00,  0.184865E+00, &
         0.767454E-01,  0.108981E+00,  0.123094E+00,  0.177287E+00, &
         0.848146E-01,  0.119356E+00,  0.133829E+00,  0.954505E-01, &
         0.155405E+00,  0.164167E+00,  0.161390E+00,  0.113287E+00, &
         0.714720E-01,  0.741598E-01,  0.719590E-01,  0.140616E+00, &
         0.355356E-01,  0.832779E-01,  0.128680E+00,  0.983013E-01, &
         0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01, &
         0.875182E-01,  0.857907E-01,  0.358808E+00,  0.178840E+00/
      DATA BRNDM2  / &
         0.254265E+00,  0.297901E+00,  0.153916E+00,  0.537774E+00, &
         0.267906E+00,  0.104254E+00,  0.400723E+00,  0.389670E+00, &
         0.263701E+00,  0.338116E+00,  0.351528E+00,  0.267764E+00, &
         0.186419E+00,  0.238237E+00,  0.210408E+00,  0.176869E+00, &
         0.114715E+00,  0.173299E+00,  0.967770E-01,  0.172565E+00, &
         0.162085E+00,  0.157782E+00,  0.886832E-01,  0.242999E+00, &
         0.760298E-01,  0.164248E+00,  0.221428E+00,  0.166799E+00, &
         0.312514E+00,  0.380600E+00,  0.353828E+00,  0.269500E+00, &
         0.254759E+00,  0.285408E+00,  0.159764E+00,  0.721058E-01, &
         0.170528E+00,  0.231595E+00,  0.307184E+00,  0.564136E-01, &
         0.159884E+00,  0.147907E+00,  0.185666E+00,  0.183567E+00, &
         0.182482E+00,  0.230650E+00,  0.175348E+00,  0.195978E+00, &
         0.255323E+00,  0.198517E+00,  0.195500E+00,  0.208356E+00, &
         0.309603E+00,  0.112011E+00,  0.102570E+00,  0.128276E+00, &
         0.168100E+00,  0.177836E+00,  0.105533E+00,  0.903330E-01, &
         0.126036E+00,  0.101430E+00,  0.124546E+00,  0.221406E+00/ 
      DATA BETAD1  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.234879E+03,  0.217419E+03,  0.201281E+03,  0.186364E+03, &
         0.172576E+03,  0.159831E+03,  0.148051E+03,  0.137163E+03, &
         0.127099E+03,  0.117796E+03,  0.109197E+03,  0.101249E+03, &
         0.939031E+02,  0.871127E+02,  0.808363E+02,  0.750349E+02, &
         0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01, &
         0.589554E+01,  0.495227E+01,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD2  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD3  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00/ 
      IF (IA.EQ.2) THEN
      ENDIF
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
      ENDIF
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
      ENDIF
      END SUBROUTINE TABLE
    SUBROUTINE SOLARD(IHRST,IDAY,MONTH,JULYR)
      JD=IDAY-32075                                                     &
             -3*((JULYR+4900+(MONTH-14)/12)/100)/4
      IF(JHR.GE.12)THEN
      END IF
    END SUBROUTINE SOLARD
    SUBROUTINE CAL_MON_DAY(JULDAY,julyr,Jmonth,Jday)     
    LOGICAL :: LEAP,NOT_FIND_DATE
    DO WHILE (NOT_FIND_DATE)
       IF(itmpday.GT.MONTH(i))THEN
       ENDIF
    END DO
    END SUBROUTINE CAL_MON_DAY
      FUNCTION ANTEMP(L,Z)
      REAL :: ZB(10,7),C(11,7),DELTA(10,7),TSTAR(7)
      DATA (C(N,2),N=1,11)/ -4.0,  -6.0,  -6.5,   0.0,   1.2, &
                        2.2,   2.5,   0.0,  -3.0,  -0.25,  0.0/
      DATA (DELTA(N,2),N=1,10)/ .5,  1.0,    .5,    .5,   1.0, &
                              1.0,  1.5,  1.0,   1.0,   1.0/
      DATA (ZB(N,5),N=1,10)/ 1.0,   3.2,   8.5,   15.5,   25.0, &
                              30.0,  35.0,  50.0,  70.0,  100.0/
      DATA (C(N,5),N=1,11)/ 3.0,  -3.2,  -6.8,  0.0,  -0.6, &
                              1.0,   1.2,   2.5, -0.7,  -1.2,  0.0/
      DATA (DELTA(N,5),N=1,10)/ .4,   1.5,    .3 ,   .5,   1.0, &
                             71.0,  84.8520,  90.0,  91.0,  92.0/
      DATA (C(N,6),N=1,11)/ -6.5,   0.0,   1.0,   2.80,  0.0, &
                             -2.80, -2.00,  0.0,   0.0,   0.0,  0.0/
      END FUNCTION ANTEMP
      SUBROUTINE COEINT(RAT,IR)
      END SUBROUTINE COEINT
      SUBROUTINE CO2INS(T22,T23,T66,IQ,L,LP1)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)
      IF (IQ.EQ.1.OR.IQ.EQ.4) THEN
      ENDIF
      END SUBROUTINE CO2INS
      SUBROUTINE CO2INT(ITAPE,T15A,T15B,T22,RATIO,IR,NMETHD,NLEVLS,NLP1,NLP2)
      END SUBROUTINE CO2INT
      SUBROUTINE CO2IN1(T20,T21,T66,IQ,L,LP1)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)
      IF (IQ.EQ.1) THEN
      ENDIF
      IF (IQ.GE.1.AND.IQ.LE.4) THEN
      ENDIF
      END SUBROUTINE CO2IN1
      SUBROUTINE CO2PTZ(SGTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                        SFULL,SHALF,PPTOP,LREAD,NL,NLP,NLP2)
      END SUBROUTINE CO2PTZ
      FUNCTION PATH(A,B,C,E)
      END FUNCTION PATH
      SUBROUTINE QINTRP(XM,X0,XP,FM,F0,FP,X,F)
      END SUBROUTINE QINTRP
      SUBROUTINE QUADSR(NLV,NLP1V,NLP2V,P,PD,TRNS)
      END SUBROUTINE QUADSR
      SUBROUTINE SIGP(PSTAR,PD,GTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                      SIGLV,SIGLY,PPTOP,LREAD,KD,KP,KM,KP2)
      DIMENSION T41(KP2,2),T42(KP), &
                T43(KP2,2),T44(KP)
      END SUBROUTINE SIGP
      SUBROUTINE SINTR2
      END SUBROUTINE SINTR2
      SUBROUTINE CO2O3(SFULL,SHALF,PPTOP,L,LP1,LP2)
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      DIMENSION T41(LP2,2),T42(LP1), &
                T43(LP2,2),T44(LP1)
      DO K=1,L
      ENDDO
      DO K1=1,LP1
      DO K2=1,LP1
      ENDDO
      ENDDO
         IF ( wrf_dm_on_monitor() ) THEN
         ENDIF
      END SUBROUTINE CO2O3
      SUBROUTINE CONRAD(KDS,KDE,KMS,KME,KTS,KTE)
      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 14,99
          IF ( .NOT. opened ) THEN
          ENDIF
        ENDDO
        ENDIF
      DO KK=1,2
      ENDDO
      DO K=1,LP1
      ENDDO
      DO J=1,LP1
      DO I=1,LP1
      ENDDO
      ENDDO
      DO K=1,LP1
      ENDDO
      END SUBROUTINE CONRAD
      END MODULE module_RA_GFDLETA
