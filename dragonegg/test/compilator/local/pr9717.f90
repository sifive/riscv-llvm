      MODULE MAIN1
      IMPLICIT NONE
      INTEGER , PARAMETER :: NSEC = 36 , NWSCAT = 6 , NKST = 6 ,        &
     &                       NHR = 24 , NPAIR = 100 , NWET = 2 ,        &
     &                       NHIANN = 10 , NMXPM = 10 , MXPLVL = 50 ,   &
     &                       MXGLVL = 87
      REAL , PARAMETER :: PI = 3.141593 , TWOPI = 6.283185 ,            &
     &                    RTOFPI = 1.772454 , SRT2PI = 2.506628 ,       &
     &                    G = 9.80616 , VONKAR = 0.4 ,                  &
     &                    GOVRCP = 0.00977 , RTOF2 = 1.414214 ,         &
     &                    RTPIBY2 = 1.2533141 , DTORAD = 0.017453293 ,  &
     &                    RTODEG = 57.29578 , DCTODK = 273.16 ,         &
     &                    BETA1 = 0.6 , BETA2 = 0.4 , AT1PT2 = 1.2 ,    &
     &                    STABIY = 0.04 , STABIZ = 0.016 ,              &
     &                    UMINGR = 0.01 , GSIGV = 0.073864 ,            &
     &                    EFOLDH = 0.44 , SVUMIN = 0.05 , SVMIN = 0.2 , &
     &                    SWMIN = 0.02 , XVAL = 0.0 , TVAL = 0.01 ,     &
     &                    FFVAL = 0.0050 , SPTGMN = 0.002 ,             &
     &                    ALPHAF = 15.0 , ALPHA1 = 1.0 , BSUBC = 0.5 ,  &
     &                    SZEPS = 0.01 , SZCOEF = 2.15 , HTFACT = 1.0 , &
     &                    ALPHAR = 1.4 , LAMDAY = 2.3 , ASUBE = 0.1 ,   &
     &                    REFPOP = 2000000. , DELTRUR = 12.0 ,          &
     &                    CSUBD = 0.15 , RGAS = 8.3145
      INTEGER , PARAMETER :: IFMAX = 40 , IKN = 85 , ISTRG = 132 ,      &
     &                       IERRN = 170 , ILEN_FLD = 80
      INTEGER :: INUNIT , IOUNIT , MFUNIT , MPUNIT , IERUNT , IERWRT ,  &
     &           IDPUNT , IDPUN2 , IRSUNT , IEVUNT , ITEVUT , IHREMI ,  &
     &           IZUNIT , INCUNT , DBGUNT , DBMUNT , ICVUNT , IO3UNT
      LOGICAL BLINE , INFLD , MARK , ECHO
      CHARACTER PATH*2 , PPATH*2 , KEYWRD*8 , PKEYWD*8 , KEYWD*8 ,      &
     &          KTYPE*5 , RUNST*1
      CHARACTER(LEN=ILEN_FLD) :: FIELD , INPFIL , OUTFIL , INCFIL
      CHARACTER(LEN=ISTRG) :: RUNST1
      INTEGER :: LOCB(IFMAX) , LOCE(IFMAX) , IFC , IDC1 , IPNUM , IPPNUM
      DIMENSION FIELD(IFMAX) , KEYWD(IKN) , RUNST(ISTRG)
      LOGICAL FATAL , ISTART , IFINIS , RECERR , ERRLST , EOF
      CHARACTER ERRMSG*50 , ERRCOD*3 , VERSN*5
      CHARACTER(LEN=ILEN_FLD) :: MSGFIL
      DIMENSION ERRMSG(IERRN) , ERRCOD(IERRN)
      INTEGER :: ILINE , IERROR , IFTL , IWRN , INFO , ICLM , IMSG ,    &
     &           IHEZ , NFATAL , NWARN , IPAGE
      REAL :: EXPLIM
      INTEGER :: ICSTAT(30) , ISSTAT(30) , IRSTAT(30) , IMSTAT(30) ,    &
     &           IOSTAT(30) , ITSTAT(30) , IESTAT(30)
      INTEGER :: INCSET , IXYSET , IEVSET , IHLSET , IFGSET
      LOGICAL :: DFAULT , CONC , DEPOS , DDEP , WDEP , RURAL , URBAN ,  &
     &        GRDRIS , NOSTD , NOBID , CLMPRO , MSGPRO , PERIOD ,       &
     &        ANNUAL , MONTH , FLAT , ELEV , FLGPOL , RUN , EVENTS ,    &
     &        RSTSAV , RSTINP , DAYTAB , MXFILE , PPFILE , PLFILE ,     &
     &        ANPOST , ANPLOT , STATOK , FSTREC , MULTYR , TXFILE ,     &
     &        RKFILE , DDPLETE , WDPLETE , FSTCMP , EVONLY , SOCONT ,   &
     &        DETAIL , NEWMET , SEASONHR , ARDPLETE , DEBUG , METEOR ,  &
     &        NOCHKD , NOWARN , SCIM , WETSCIM , SCIMHR ,               &
     &        WETHR , FIRSTWET , TOXICS , SCREEN , URBSTAB ,            &
     &        PRM_FSTREC , O3FILE
      LOGICAL ::  PM10AVE=.FALSE. , ROMBERG=.FALSE. , PVMRM=.FALSE. ,   &
     &            OLM=.FALSE.    
      CHARACTER ELTYPE*6 , TITLE1*68 , TITLE2*68 , EVPARM*6 , CHRAVE*5 ,&
     &          CHIDEP*4 , SOELEV*6 , REELEV*6 , TGELEV*6 , OUTTYP*5
      CHARACTER(LEN=ILEN_FLD) :: SAVFIL , SAVFL2 , INIFIL , EVFILE ,    &
     &                           DBGFIL , DBMFIL , URBNAM , OZONFL ,    &
     &                           O3FILUNITS , O3VALUNITS , O3FORM
      REAL :: O3CONC , O3BACK , NO2EQUIL
      LOGICAL :: O3MISS
      INTEGER :: NHIVAL , NMXVAL , NDUMP
      INTEGER :: NSRC , NREC , NGRP , NQF,                              &
     &           NPDMAX , NNET , IXM , IYM  , NUMEVE , IEVENT ,         &
     &           NARC , NOLM
      INTEGER :: NAVE=0 , NVAL=0 , NTYP=0 , NMAX=0 , NEVE=0
      INTEGER , ALLOCATABLE :: KAVE(:)
      LOGICAL , ALLOCATABLE :: EVAL(:)
      ALLOCATABLE ::CHRAVE(:) , CHIDEP(:,:) , OUTTYP(:)
      CHARACTER(LEN=6) :: MODOPS(18)
      CHARACTER SRCID*8 , SRCTYP*8 , SOPCRD*1 , SOGAS*1 , URBSRC*1 ,    &
     &          GRPID*8 , EMILBL*40 , OUTLBL*40 , POLLUT*8 , PSOID*8 ,  &
     &          QFLAG*8 , PERLBL*40 , OLMID*8
      CHARACTER(LEN=ILEN_FLD) :: HRFILE
      CHARACTER PREVGRPID*8
      LOGICAL LDPART , LWPART , LDGAS , LWGAS
      REAL , ALLOCATABLE :: AXS(:) , AYS(:) , AZS(:) , AQS(:) , AHS(:) ,&
     &                      ATS(:) , AVS(:) , ADS(:) , ASYINI(:) ,      &
     &                      ASZINI(:) , ANO2_RATIO(:)
      REAL , ALLOCATABLE :: ADSBH(:,:) , ADSBW(:,:) , ADSBL(:,:) ,      &
     &                      ADSXADJ(:,:) , ADSYADJ(:,:)
      INTEGER , ALLOCATABLE :: INPD(:) , NDXSTK(:)
      REAL , ALLOCATABLE :: QFACT(:,:)
      REAL :: EMICON , HAFLIF , DECOEF , VAIRMS , ZRDEP , VDPHOR
      REAL , ALLOCATABLE :: EMIFAC(:) , APDIAM(:,:) , APHI(:,:) ,       &
     &                      APDENS(:,:) , AVGRAV(:,:) , ATSTOP(:,:)
      REAL :: HRQS , HRTS , HRVS
      REAL , ALLOCATABLE :: EV_HRQS(:,:) , EV_HRTS(:,:) , EV_HRVS(:,:)
      INTEGER :: KURHRQ
      INTEGER , ALLOCATABLE :: IGROUP(:,:) , IGRP_OLM(:,:)
      ALLOCATABLE ::SRCID(:) , SRCTYP(:) , SOPCRD(:) , SOGAS(:) ,       &
     &            URBSRC(:) , GRPID(:) , QFLAG(:) , EMILBL(:) ,         &
     &            OUTLBL(:) , PERLBL(:) , OLMID(:)
      LOGICAL , ALLOCATABLE :: L_OLMGRP(:)
      INTEGER , PARAMETER :: NVMAX = 24 , NVMAX2 = NVMAX*2
      LOGICAL LSEG
      INTEGER :: IVERT , NVERT , NSEGS
      REAL :: UVERT(NVMAX) , VVERT(NVMAX) , VNVERT(NVMAX) , WVERT(NVMAX)&
     &        , UASEGS(NVMAX) , UBSEGS(NVMAX) , XVERT(NVMAX) ,          &
     &        YVERT(NVMAX)
      REAL :: SPA(NVMAX,2)
      REAL , ALLOCATABLE :: AXINIT(:) , AYINIT(:) , AANGLE(:) ,         &
     &                      AXVERT(:,:) , AYVERT(:,:) , RADIUS(:) ,     &
     &                      AXCNTR(:) , AYCNTR(:)
      INTEGER , ALLOCATABLE :: NVERTS(:)
      REAL , PARAMETER :: ALPHA = 0.029
      REAL , ALLOCATABLE :: AALPHA(:) , APDEFF(:) , AVOLUM(:)
      REAL , ALLOCATABLE :: EFRAC(:) , QPART(:)
      REAL :: PALPHA , THETA , PDEFF , PDREL , PITFRA , QEFF
      REAL :: PITLEN , PITWID , PITL , EMIHGT , XEFF , YEFF
      REAL , ALLOCATABLE :: PDIFF(:) , PDIFFW(:) , RMOLWT(:) , ALPHAS(:)&
     &                      , REACT(:) , HENRY(:) , RCLI(:) ,           &
     &                      FINEMASS(:) , SCF(:)
      LOGICAL , ALLOCATABLE :: L_METHOD2(:)
      INTEGER :: ISEAS_GD(12) , ILAND_GD(36) , NCLOUD
      REAL :: RM , RCUT , QSW , XLAI , VDEPG , USERVD , ZSUBP ,         &
     &        DELTA_Z , FO , FSEAS2 , FSEAS5 , FRACSAT , LIQCONT ,      &
     &        DENOM , XNU
      REAL :: WOLD , WNEW , F2 , ESTA
      CHARACTER*40 REFSPE
      LOGICAL LUSERVD
      LOGICAL ISTA , IEND , NEWID
      CHARACTER NETID*8 , NETIDT*8 , PNETID*8 , NTID*8 , NTTYP*8 ,      &
     &          RECTYP*2 , PXSOID*8 , PESOID*8 , ARCID*8
      REAL , ALLOCATABLE :: AXR(:) , AYR(:) , AZELEV(:) , AZFLAG(:) ,   &
     &                      AZHILL(:)
      INTEGER , ALLOCATABLE :: IREF(:) , NDXARC(:)
      ALLOCATABLE ::NETID(:) , RECTYP(:) , NTID(:) , NTTYP(:) , ARCID(:)
      INTEGER :: ICOUNT , JCOUNT , IZE , IZH , IZF , IRZE , IRZH ,      &
     &           IRZF , IRXR , IRYR , IRHZ , IBND , IBELEV , INNET
      REAL :: XINT , YINT
      REAL , ALLOCATABLE :: XCOORD(:,:) , YCOORD(:,:) , XORIG(:) ,      &
     &                      YORIG(:)
      INTEGER , ALLOCATABLE :: NETSTA(:) , NETEND(:) , NUMXPT(:) ,      &
     &                         NUMYPT(:)
      CHARACTER SFNAME*40 , UANAME*40 , ONNAME*40 , ALAT*10 , ALON*10
      CHARACTER(LEN=ILEN_FLD) :: METINP , SCIM_SFCFIL , SCIM_PROFIL ,   &
     &                           PROINP
      CHARACTER(LEN=105) :: METFRM , PROFRM
      LOGICAL SCIMOUT
      INTEGER :: ISDATE , IEDATE , ISYR , ISMN , ISDY , ISHR , IEYR ,   &
     &           IEMN , IEDY , IEHR , IPROC(366) , ISYEAR , IUYEAR ,    &
     &           IOYEAR , IDSURF , IDUAIR , IDSITE , ISJDAY , IEJDAY ,  &
     &           NDAYS , INCRST , ISTRT_CENT , ISTRT_WIND , NREGSTART , &
     &           NREGINT , IFIRSTHR , ISUNIT , IPUNIT , NSKIPTOT ,      &
     &           NSKIPWET , NSKIPDRY , NSWETCLM , NSDRYCLM , NSWETMSG , &
     &           NSDRYMSG , NWETHR , NWETINT , NWETSTART
      REAL :: UCAT(5) , ZREF , ROTANG , UMIN , VIRTPNT_URB(NKST) ,      &
     &        VIRTPNT_RUR(NKST) , VP_FACT
      LOGICAL CLMHR , MSGHR , UNSTAB , NEUTRL , STABLE , RUNERR ,       &
     &        PFLERR , NEWDAY , ENDMON , METHDR , HOURLY
      INTEGER :: KSTMSG
      INTEGER :: IHOUR , IYEAR , IMONTH , IDAY , KURDAT , ISEAS ,       &
     &           KHOUR , KYEAR , KMONTH , KDAY , KURPFL , NTOTHRS ,     &
     &           IPHOUR , IPDATE , IPCODE , KST , IYR , IDAY_OF_WEEK ,  &
     &           IDAY_OF_WEEK7 , NPLVLS , NTGLVL , IFLAG(MXPLVL)
      INTEGER :: JDAY=0
      INTEGER :: FULLDATE
      REAL :: SFCHF , USTAR , WSTAR , VPTGZI , ZICONV , ZIMECH ,        &
     &        OBULEN , SFCZ0 , BOWEN , ALBEDO , UREF , WDREF , UREFHT , &
     &        TA , TREFHT , ZI , AFV , BVF , BVPRIM , XLAT , TSIGN ,    &
     &        ZIRUR , ZIURB , URBWSTR , URBPOP , PRATE , PREC1 , PREC2 ,&
     &        UREF10 , URBZ0 , URBUSTR , URBOBULEN , RURUSTR ,          &
     &        RUROBULEN , RH , SFCP
      INTEGER :: IKST(NHR) , IAPCODE(NHR) , NACLOUD(NHR)
      REAL :: APRATE(NHR) , AQSW(NHR) , ARH(NHR) , ASFCP(NHR)
      REAL :: ASFCHF(NHR) , AUREF(NHR) , AUREFHT(NHR) , ATA(NHR) ,      &
     &        ATREFHT(NHR) , AWDREF(NHR) , AUSTAR(NHR) , AWSTAR(NHR) ,  &
     &        AZICONV(NHR) , AZIMECH(NHR) , AOBULEN(NHR) , AVPTGZI(NHR) &
     &        , ASFCZ0(NHR) , ABOWEN(NHR) , AALBEDO(NHR) , AWNEW(NHR) , &
     &        AWOLD(NHR) , AESTA(NHR) , AF2(NHR) , APREC1(NHR) ,        &
     &        APREC2(NHR)
      INTEGER :: IENDHOUR , IENDDY , IENDMN , NUMYRS , NREMAIN , NDX4ZI
      REAL :: PFLHT(MXPLVL) , PFLWD(MXPLVL) , PFLWS(MXPLVL) ,           &
     &        PFLTA(MXPLVL) , PFLSA(MXPLVL) , PFLSW(MXPLVL) ,           &
     &        PFLSV(MXPLVL) , PFLTG(MXPLVL) , PFLTGZ(MXPLVL)
      REAL :: APFLHT(NHR,MXPLVL) , APFLWD(NHR,MXPLVL) ,                 &
     &        APFLWS(NHR,MXPLVL) , APFLTA(NHR,MXPLVL) ,                 &
     &        APFLSA(NHR,MXPLVL) , APFLSW(NHR,MXPLVL) ,                 &
     &        APFLSV(NHR,MXPLVL) , APFLTG(NHR,MXPLVL) ,                 &
     &        APFLTGZ(NHR,MXPLVL)
      INTEGER :: AIFLAG(NHR,MXPLVL) , ANPLVLS(NHR) , ANTGLVL(NHR)
      REAL :: GRIDHT(MXGLVL) , GRIDWD(MXGLVL) , GRIDWS(MXGLVL) ,        &
     &        GRIDSW(MXGLVL) , GRIDSV(MXGLVL) , GRIDTG(MXGLVL) ,        &
     &        GRIDPT(MXGLVL) , GRIDRHO(MXGLVL) , GRIDEPS(MXGLVL)
      REAL :: GRDSWU(MXGLVL) , GRDSVU(MXGLVL) , GRDTGU(MXGLVL) ,        &
     &        GRDSWR(MXGLVL) , GRDSVR(MXGLVL) , GRDTGR(MXGLVL) ,        &
     &        GRDPTU(MXGLVL) , GRDPTR(MXGLVL)
      REAL :: HNPREV , HDPREV , USPREV , HN , HTRANS , SFCLVL , TG4PFL ,&
     &        TG4XTR , THSTAR , SVAVG , SWAVG , UAVG , SVATZI , SWATZI ,&
     &        UATZI , PTATZI , UATHE , SVATHE , SWATHE , UAVH3 ,        &
     &        SVAVH3 , SWAVH3 , SWRMAX
      CHARACTER(LEN=ILEN_FLD) :: TERINP
      LOGICAL LTGRID
      REAL :: TGX0 , TGY0 , GRDXLL , GRDXUR , GRDYLL , GRDYUR , XYINT
      LOGICAL CALCS , WAKE , WAKESS , BUOYNT , TALL , SQUAT , SSQUAT
      LOGICAL SURFAC
      DOUBLE PRECISION PHID1 , PHID2 , PHIN1 , PHIN2
      INTEGER :: IREC , ISRC , IGRP , IAVE , ITYP , ISET , NUMREC ,     &
     &           NUMSRC , NUMGRP , NUMAVE , NUMARC , NUMTYP , NUMYR ,   &
     &           ICYEAR , NUMURB , NPD , IFVSEC , IUCAT , IOLM ,        &
     &           NUMOLM=0
      REAL :: XS , YS , ZS , QS , HS , DS , VS , TS , SYINIT , SZINIT , &
     &        XINIT , YINIT , ANGLE , XCNTR , YCNTR , DSBH , DSBW ,     &
     &        DSBL , XADJ , YADJ , B_SUBS , B_SUBL , RSCALE , D , VD ,  &
     &        E , WDRAD , WDSIN , WDCOS , ZBASE
      REAL , ALLOCATABLE :: PDIAM(:) , PHI(:) , PDENS(:) , VGRAV(:) ,   &
     &                      TSTOP(:) , SCHMIDT(:) , VDEP(:) , WQCOR(:) ,&
     &                      DQCOR(:) , PSCVRT(:) , WASHOUT(:)
      REAL :: WQCORG , GSCVRT , DQCORG , WASHOUTG , VSETL
      REAL :: XR , YR , X , Y , ZELEV , ZFLAG , ZR , ZEFF , DISTR ,     &
     &        ZHILL , HCRIT , ZRT , XDIST
      REAL :: HE , HSP , HEFLAT , HTER , HEMWAK , HEDHH , ZB , ZM ,     &
     &        HED1 , HED2 , HEN1 , HEN2 , HE3 , HPEN , HED1M , HED2M ,  &
     &        HEN1M , HEN2M , HE3M , HSBL , QSUBN , QSUB3 , XY , XZ ,   &
     &        SBID , FM , FB , DTDZ , DHF , DHFAER , DHP , DHP1 , DHP2 ,&
     &        DHP3 , DELT , DHPB , DHPM , XF , XMAX , XFM , XFB , XRAD ,&
     &        WPB , DHCRIT , HTEFF , CENTER , Z4GAMMA , XTR4GAMMA
      REAL :: HESETL , HE3SETL , HV
      REAL :: US , SVS , SWS , TGS , TYS , PTS , UP , WDIR , DA , ZLY , &
     &        ZLB , RINIT , CB , CM , QTK , PPF , PSUBS , FHC , SY ,    &
     &        SYB , SYN , SY3 , SZ , SZUPR , SYAMB , SZAMB , SZAS ,     &
     &        SZAD1 , SZAD2 , SZAN1 , SZAN2 , SYAN , SZA3 , SZB , SZBD ,&
     &        SZBN , SZ3 , SZD1 , SZD2 , SZN1 , SZN2 , SZEFF , SZSURF , &
     &        SYA3 , SYB3 , SZB3 , VSY3 , VSIGY , VSIGZ , VSYN , VSZD1 ,&
     &        VSZD2 , VSZN1 , VSZN2 , VSZ3 , SZD1M , SZD2M , SZN1M ,    &
     &        SZN2M , SZ3M , U3 , SV3 , SW3 , TGP
      DOUBLE PRECISION :: FSUBY , FSUBYD , FSUBYN , FSUBY3
      REAL :: FSUBZ , FSUBZD , FSUBZN , FSUBZ3 , PHEE , FOPT , CWRAP ,  &
     &        CLIFT , XMDBG , CWRAPC , CLIFTC , FSUBYC , FSBY3C
      REAL :: UEFF , SVEFF , SWEFF , TGEFF , UEFFD , SVEFFD , SWEFFD ,  &
     &        UEFFN , SVEFFN , SWEFFN , UEFF3 , SVEFF3 , SWEFF3 ,       &
     &        TGEFF3 , EPSEFF , EPSEFFD , EPSEFF3 , XMIXED , XFINAL ,   &
     &        ZMIDMX
      REAL :: SKEW , R , ALPHPD , BETAPD , ASUB1 , ASUB2 , BSUB1 ,      &
     &        BSUB2 , LAMDA1 , LAMDA2
      REAL :: CHIW , CHIDW , CHINW , CHI3W , CHIL , CHIDL , CHINL ,     &
     &        CHI3L
      REAL :: GAMFACT
      CHARACTER EVNAME*8 , EVGRP*8
      INTEGER , ALLOCATABLE :: EVAPER(:) , EVDATE(:) , EVJDAY(:) ,      &
     &                         IDXEV(:)
      ALLOCATABLE ::EVNAME(:) , EVGRP(:)
      LOGICAL OUTPART
      LOGICAL , ALLOCATABLE :: ANPART(:) , ALLPARTS(:) , ALLPARTG(:)
      CHARACTER(LEN=ILEN_FLD) :: THRFIL , PSTFIL , PLTFIL , ANNPST ,    &
     &                           ANNPLT , THRFRM , PSTFRM , PLTFRM ,    &
     &                           TOXFIL , SEAHRS , RNKFIL , RNKFRM ,    &
     &                           EVLFIL , ANNPART
      INTEGER , ALLOCATABLE :: NHIAVE(:,:) , MAXAVE(:) , IMXVAL(:) ,    &
     &                         IDYTAB(:) , MAXFLE(:,:) , IPSTFL(:,:) ,  &
     &                         IPLTFL(:,:,:) , IANPST(:) , IANPLT(:) ,  &
     &                         INHI(:) , ITOXFL(:) , ISEAHR(:) ,        &
     &                         IRNKFL(:) , IRKVAL(:) , IANPART(:)
      REAL , ALLOCATABLE :: THRESH(:,:) , TOXTHR(:)
      INTEGER , ALLOCATABLE :: IMXUNT(:,:) , IPSUNT(:,:) , IPSFRM(:,:) ,&
     &                         IPLUNT(:,:,:) , IAPUNT(:) , IANFRM(:) ,  &
     &                         IPPUNT(:) , ITXUNT(:) , ISHUNT(:) ,      &
     &                         IRKUNT(:) , IELUNT(:) , IUPART(:)
      ALLOCATABLE ::THRFIL(:,:) , PSTFIL(:,:) , PLTFIL(:,:,:) ,         &
     &            ANNPST(:) , ANNPLT(:) , TOXFIL(:) , SEAHRS(:) ,       &
     &            RNKFIL(:) , EVLFIL(:) , ANNPART(:)
      INTEGER , ALLOCATABLE :: IDCONC(:,:)
      INTEGER :: ITAB , NXTOX , NYTOX , NHOURS , IPAIR
      REAL , ALLOCATABLE :: TXCONC(:,:)
      CHARACTER WORKID*8 , DUMMY*8
      INTEGER :: IMIT , INUM , IDUM , INDAVE , INDGRP , INDVAL , ISC ,  &
     &           IOERRN , NCPP , NRPP , NGPP , NPPX , NPPY
      REAL :: FNUM , RNUM
      ALLOCATABLE ::WORKID(:)
      INTEGER , ALLOCATABLE :: IWRK2(:,:)
      REAL , ALLOCATABLE :: ZETMP1(:) , ZETMP2(:)
      REAL , ALLOCATABLE :: ZFTMP1(:) , ZFTMP2(:)
      REAL , ALLOCATABLE :: ZHTMP1(:) , ZHTMP2(:)
      SAVE 
      CHARACTER HCLMSG , MCLMSG , HMCLM
      REAL , ALLOCATABLE :: HRVAL(:) , AVEVAL(:,:,:,:) , HRVALD(:) ,    &
     &                      HRVALJD(:,:) , AERVAL(:) , PRMVAL(:) ,      &
     &                      AERVALD(:) , PRMVALD(:)
      REAL , ALLOCATABLE :: HIVALU(:,:,:,:,:) , HMAX(:,:,:,:)
      INTEGER , ALLOCATABLE :: HMLOC(:,:,:,:) , HMDATE(:,:,:,:) ,       &
     &                         NHIDAT(:,:,:,:,:)
      REAL , ALLOCATABLE :: ANNVAL(:,:,:) , AMXVAL(:,:,:) ,             &
     &                      SHVALS(:,:,:,:,:) , ANNVALD(:,:,:) ,        &
     &                      ANNVALW(:,:,:) , ANNVALJD(:,:,:,:) ,        &
     &                      ANNVALJW(:,:,:,:)
      INTEGER , ALLOCATABLE :: IMXLOC(:,:,:)
      INTEGER :: IANHRS , IANCLM , IANMSG , NSEAHR(4,24) , NSEACM(4,24) &
     &           , IANWET , IWETCLM , IWETMSG
      REAL , ALLOCATABLE :: RMXVAL(:,:,:,:)
      INTEGER , ALLOCATABLE :: MXDATE(:,:,:,:) , MXLOCA(:,:,:,:)
      INTEGER , ALLOCATABLE :: NUMHRS(:) , NUMCLM(:) , NUMMSG(:)
      ALLOCATABLE ::HCLMSG(:,:,:,:,:) , MCLMSG(:,:,:,:) , HMCLM(:,:,:,:)
      REAL , ALLOCATABLE :: SUMANN(:,:,:)
      REAL , ALLOCATABLE :: SUMH4H(:,:) , MXPMVAL(:,:)
      INTEGER , ALLOCATABLE :: MXPMLOC(:,:)
      REAL , ALLOCATABLE :: CHI(:,:,:) , HECNTR(:,:) , HECNTR3(:,:) ,   &
     &                      PPFACT(:) , UEFFS(:,:) , UEFF3S(:,:) ,      &
     &                      FOPTS(:,:)
      REAL , ALLOCATABLE :: ARCMAX(:) , QMAX(:) , DXMAX(:) , UMAX(:) ,  &
     &                      SVMAX(:) , SWMAX(:) , SYMAX(:) , SY3MX(:) , &
     &                      U3MAX(:) , HEMAX(:) , ARCCL(:) , SZMAX(:) , &
     &                      CHIDMW(:) , CHINMW(:) , CHI3MW(:) ,         &
     &                      CHIDML(:) , CHINML(:) , CHI3ML(:) ,         &
     &                      HSBLMX(:)
      REAL , ALLOCATABLE :: EV_AVEVAL(:) , HRVALS(:,:) , GRPVAL(:)
      REAL :: GRPAVE
      INTEGER :: EV_NUMHRS , EV_NUMCLM , EV_NUMMSG , ISTAHR , IENDHR
      DATA VERSN/'04300'/
      DATA INUNIT/7/ , IOUNIT/8/ , MFUNIT/19/ , MPUNIT/21/ ,            &
     &     IERUNT/10/ , IERWRT/11/ , IDPUNT/12/ , IZUNIT/13/ ,          &
     &     IDPUN2/14/ , IRSUNT/15/ , IHREMI/16/ , IEVUNT/17/ ,          &
     &     ITEVUT/18/ , INCUNT/20/ , ISUNIT/22/ , IPUNIT/23/ ,          &
     &     DBGUNT/24/ , DBMUNT/25/ , ICVUNT/26/ , IO3UNT/29/
      DATA INPFIL/' '/ , OUTFIL/' '/
      INTEGER , PRIVATE :: I
      DATA (KEYWD(I),I=1,IKN)/'STARTING' , 'FINISHED' , 'TITLEONE' ,    &
     &      'TITLETWO' , 'MODELOPT' , 'AVERTIME' , 'POLLUTID' ,         &
     &      'HALFLIFE' , 'DCAYCOEF' , 'DEBUGOPT' , 'ELEVUNIT' ,         &
     &      'FLAGPOLE' , 'RUNORNOT' , 'EVENTFIL' , 'SAVEFILE' ,         &
     &      'INITFILE' , 'MULTYEAR' , 'ERRORFIL' , 'GASDEPDF' ,         &
     &      'GDSEASON' , 'GASDEPVD' , 'GDLANUSE' , 'EVENTFIL' ,         &
     &      'URBANOPT' , 'METHOD_2' , 'LOCATION' , 'SRCPARAM' ,         &
     &      'BUILDHGT' , 'BUILDWID' , 'BUILDLEN' , 'XBADJ   ' ,         &
     &      'YBADJ   ' , 'EMISFACT' , 'EMISUNIT' , 'PARTDIAM' ,         &
     &      'MASSFRAX' , 'PARTDENS' , '        ' , '        ' ,         &
     &      '        ' , 'CONCUNIT' , 'DEPOUNIT' , 'HOUREMIS' ,         &
     &      'GASDEPOS' , 'URBANSRC' , 'EVENTPER' , 'EVENTLOC' ,         &
     &      'SRCGROUP' , 'GRIDCART' , 'GRIDPOLR' , 'DISCCART' ,         &
     &      'DISCPOLR' , 'SURFFILE' , 'PROFFILE' , 'PROFBASE' ,         &
     &      '        ' , 'SURFDATA' , 'UAIRDATA' , 'SITEDATA' ,         &
     &      'STARTEND' , 'DAYRANGE' , 'WDROTATE' , 'DTHETADZ' ,         &
     &      'WINDCATS' , 'RECTABLE' , 'MAXTABLE' , 'DAYTABLE' ,         &
     &      'MAXIFILE' , 'POSTFILE' , 'PLOTFILE' , 'TOXXFILE' ,         &
     &      'EVENTOUT' , 'INCLUDED' , 'SCIMBYHR' , 'SEASONHR' ,         &
     &      'AREAVERT' , 'PARTSIZE' , 'RANKFILE' , 'EVALCART' ,         &
     &      'EVALFILE' , 'NO2EQUIL' , 'OZONEVAL' , 'OZONEFIL' ,         &
     &      'NO2RATIO' , 'OLMGROUP'/
      DATA IPROC/366*1/ , EXPLIM/ -50.0/
      DATA UCAT/1.54 , 3.09 , 5.14 , 8.23 , 10.8/
      DATA MODOPS/18*'      '/
      DATA VIRTPNT_URB/3.5 , 3.5 , 5.5 , 10.5 , 15.5 , 15.5/ ,          &
     &     VIRTPNT_RUR/3.5 , 5.5 , 7.5 , 12.5 , 15.5 , 25.5/
      DATA ICSTAT/30*0/ , ISSTAT/30*0/ , IRSTAT/30*0/ , IMSTAT/30*0/ ,  &
     &     IOSTAT/30*0/ , ITSTAT/30*0/
      DATA GRIDHT/0.0 , 0.5 , 1.0 , 2.0 , 4.0 , 8.0 , 14.0 , 20.0 ,     &
     &     30.0 , 40.0 , 50.0 , 60.0 , 70.0 , 80.0 , 90.0 , 100.0 ,     &
     &     120.0 , 140.0 , 160.0 , 180.0 , 200.0 , 250.0 , 300.0 ,      &
     &     350.0 , 400.0 , 450.0 , 500.0 , 550.0 , 600.0 , 650.0 ,      &
     &     700.0 , 750.0 , 800.0 , 850.0 , 900.0 , 950.0 , 1000.0 ,     &
     &     1050.0 , 1100.0 , 1150.0 , 1200.0 , 1250.0 , 1300.0 ,        &
     &     1350.0 , 1400.0 , 1450.0 , 1500.0 , 1550.0 , 1600.0 ,        &
     &     1650.0 , 1700.0 , 1750.0 , 1800.0 , 1850.0 , 1900.0 ,        &
     &     1950.0 , 2000.0 , 2100.0 , 2200.0 , 2300.0 , 2400.0 ,        &
     &     2500.0 , 2600.0 , 2700.0 , 2800.0 , 2900.0 , 3000.0 ,        &
     &     3100.0 , 3200.0 , 3300.0 , 3400.0 , 3500.0 , 3600.0 ,        &
     &     3700.0 , 3800.0 , 3900.0 , 4000.0 , 4100.0 , 4200.0 ,        &
     &     4300.0 , 4400.0 , 4500.0 , 4600.0 , 4700.0 , 4800.0 ,        &
     &     4900.0 , 5000.0/
      DATA ERRCOD(1)/'100'/ , ERRMSG(1)                                 &
     &     /'Invalid Pathway Specified. The Troubled Pathway is'/
      DATA ERRCOD(2)/'105'/ , ERRMSG(2)                                 &
     &     /'Invalid Keyword Specified. The Troubled Keyword is'/
      DATA ERRCOD(3)/'110'/ , ERRMSG(3)                                 &
     &     /'Keyword is Not Valid for This Pathway.  Keyword is'/
      DATA ERRCOD(4)/'115'/ , ERRMSG(4)                                 &
     &     /'STARTING or FINISHED Out of Sequence:  Pathway =  '/
      DATA ERRCOD(5)/'120'/ , ERRMSG(5)                                 &
     &     /'Pathway is Out of Sequence:  Pathway =            '/
      DATA ERRCOD(6)/'125'/ , ERRMSG(6)                                 &
     &     /'Missing FINISHED-Runstream File Incomplete: ISTAT='/
      DATA ERRCOD(7)/'130'/ , ERRMSG(7)                                 &
     &     /'Missing Mandatory Keyword.  The Missing Keyword is'/
      DATA ERRCOD(8)/'135'/ , ERRMSG(8)                                 &
     &     /'Duplicate Nonrepeatable Keyword Specified:Keyword='/
      DATA ERRCOD(9)/'140'/ , ERRMSG(9)                                 &
     &     /'Invalid Order of Keyword.  The Troubled Keyword is'/
      DATA ERRCOD(10)/'141'/ , ERRMSG(10)                               &
     &     /'Conflicting Options:  PVMRM and OLM both specified'/
      DATA ERRCOD(11)/'142'/ , ERRMSG(11)                               &
     &     /'Following Keyword Invalid Without PVMRM or OLM:   '/
      DATA ERRCOD(12)/'143'/ , ERRMSG(12)                               &
     &     /'Following Keyword Invalid Without PVMRM Option:   '/
      DATA ERRCOD(13)/'144'/ , ERRMSG(13)                               &
     &     /'Following Keyword Invalid Without OLM Option:     '/
      DATA ERRCOD(14)/'145'/ , ERRMSG(14)                               &
     &     /'Conflicting Options: MULTYEAR and Re-Start Option '/
      DATA ERRCOD(15)/'150'/ , ERRMSG(15)                               &
     &     /'Conflicting Options: MULTYEAR for Wrong Pollutant '/
      DATA ERRCOD(16)/'152'/ , ERRMSG(16)                               &
     &     /'ELEVUNIT card must be first for this Pathway:     '/
      DATA ERRCOD(17)/'154'/ , ERRMSG(17)                               &
     &     /'Conflicting options:  SCIM cannot be used with    '/
      DATA ERRCOD(18)/'155'/ , ERRMSG(18)                               &
     &     /'Conflicting Decay Keyword. Inputs Ignored for     '/
      DATA ERRCOD(19)/'156'/ , ERRMSG(19)                               &
     &     /'Option ignored - not valid with SCIM.  Option =   '/
      DATA ERRCOD(20)/'157'/ , ERRMSG(20)                               &
     &     /'Wet SCIM Option Not Operational Yet. Input Ignored'/
      DATA ERRCOD(21)/'158'/ , ERRMSG(21)                               &
     &     /'EMISUNIT Keyword Used With More Than 1 Output Type'/
      DATA ERRCOD(22)/'159'/ , ERRMSG(22)                               &
     &     /'EMISUNIT Keyword Used With the Following Keyword: '/
      DATA ERRCOD(23)/'160'/ , ERRMSG(23)                               &
     &     /'Duplicate ORIG Secondary Keyword for GRIDPOLR:    '/
      DATA ERRCOD(24)/'170'/ , ERRMSG(24)                               &
     &     /'Invalid Secondary Keyword for Receptor Grid:      '/
      DATA ERRCOD(25)/'175'/ , ERRMSG(25)                               &
     &     /'Missing Secondary Keyword END for Receptor Grid:  '/
      DATA ERRCOD(26)/'180'/ , ERRMSG(26)                               &
     &     /'Conflicting Secondary Keyword for Receptor Grid:  '/
      DATA ERRCOD(27)/'185'/ , ERRMSG(27)                               &
     &     /'Missing Receptor Keywords. No Receptors Specified.'/
      DATA ERRCOD(28)/'190'/ , ERRMSG(28)                               &
     &     /'No Keywords for OU Path and No PERIOD/ANNUAL Aves.'/
      DATA ERRCOD(29)/'195'/ , ERRMSG(29)                               &
     &     /'Incompatible Option Used With SAVEFILE or INITFILE'/
      DATA ERRCOD(30)/'196'/ , ERRMSG(30)                               &
     &     /'Incompatible Keyword Used With GASDEPVD           '/
      DATA ERRCOD(31)/'197'/ , ERRMSG(31)                               &
     &     /'Post-97 PM10 without MAXIFILE is incompatible with'/
      DATA ERRCOD(32)/'198'/ , ERRMSG(32)                               &
     &     /'TOXICS Option is Required in Order to Use Option  '/
      DATA ERRCOD(33)/'200'/ , ERRMSG(33)                               &
     &     /'Missing Parameter(s). No Options Specified For    '/
      DATA ERRCOD(34)/'201'/ , ERRMSG(34)                               &
     &     /'Not Enough Parameters Specified For the Keyword of'/
      DATA ERRCOD(35)/'202'/ , ERRMSG(35)                               &
     &     /'Too Many Parameters Specified For the Keyword of  '/
      DATA ERRCOD(36)/'203'/ , ERRMSG(36)                               &
     &     /'Invalid Parameter Specified.  Troubled Parameter: '/
      DATA ERRCOD(37)/'204'/ , ERRMSG(37)                               &
     &     /'Option Parameters Conflict.  Forced by Default to '/
      DATA ERRCOD(38)/'205'/ , ERRMSG(38)                               &
     &     /'No Option Parameter Setting.  Forced by Default to'/
      DATA ERRCOD(39)/'206'/ , ERRMSG(39)                               &
     &     /'Regulatory DFAULT Overrides Non-DFAULT Option For '/
      DATA ERRCOD(40)/'207'/ , ERRMSG(40)                               &
     &     /'No Parameters Specified. Default Values Will Used.'/
      DATA ERRCOD(41)/'208'/ , ERRMSG(41)                               &
     &     /'Illegal Numerical Field Encountered in            '/
      DATA ERRCOD(42)/'209'/ , ERRMSG(42)                               &
     &     /'Negative Value Appears For Non-negative Variable. '/
      DATA ERRCOD(43)/'210'/ , ERRMSG(43)                               &
     &     /'Number of Short Term Averages Exceeds Max:  NAVE= '/
      DATA ERRCOD(44)/'211'/ , ERRMSG(44)                               &
     &     /'Duplicate Averaging Period Specified for Keyword  '/
      DATA ERRCOD(45)/'212'/ , ERRMSG(45)                               &
     &     /'END Encountered Without (X,Y) Points Properly Set '/
      DATA ERRCOD(46)/'213'/ , ERRMSG(46)                               &
     &     /'ELEV Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(47)/'214'/ , ERRMSG(47)                               &
     &     /'ELEV Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(48)/'215'/ , ERRMSG(48)                               &
     &     /'FLAG Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(49)/'216'/ , ERRMSG(49)                               &
     &     /'FLAG Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(50)/'217'/ , ERRMSG(50)                               &
     &     /'More Than One Delimiter In A Field for Keyword    '/
      DATA ERRCOD(51)/'218'/ , ERRMSG(51)                               &
     &     /'Number of (X,Y) Points Not Match With Number Of   '/
      DATA ERRCOD(52)/'219'/ , ERRMSG(52)                               &
     &     /'Number Of Receptors Specified Exceeds Max:  NREC= '/
      DATA ERRCOD(53)/'220'/ , ERRMSG(53)                               &
     &     /'Missing Origin (Use Default = 0,0) In GRIDPOLR    '/
      DATA ERRCOD(54)/'221'/ , ERRMSG(54)                               &
     &     /'Missing Distance Setting In Polar Network         '/
      DATA ERRCOD(55)/'222'/ , ERRMSG(55)                               &
     &     /'Missing Degree Or Dist Setting In Polar Network   '/
      DATA ERRCOD(56)/'223'/ , ERRMSG(56)                               &
     &     /'Missing Distance or Degree Field in               '/
      DATA ERRCOD(57)/'224'/ , ERRMSG(57)                               &
     &     /'Number of Receptor Networks Exceeds Max:  NNET=   '/
      DATA ERRCOD(58)/'225'/ , ERRMSG(58)                               &
     &     /'Number of X-Coords Specified Exceeds Max:  IXM=   '/
      DATA ERRCOD(59)/'226'/ , ERRMSG(59)                               &
     &     /'Number of Y-Coords Specified Exceeds Max:  IYM=   '/
      DATA ERRCOD(60)/'227'/ , ERRMSG(60)                               &
     &     /'No Receptors Were Defined on the RE Pathway.      '/
      DATA ERRCOD(61)/'228'/ , ERRMSG(61)                               &
     &     /'Default(s) Used for Missing Parameters on Keyword '/
      DATA ERRCOD(62)/'229'/ , ERRMSG(62)                               &
     &     /'Too Many Parameters - Inputs Ignored on Keyword   '/
      DATA ERRCOD(63)/'231'/ , ERRMSG(63)                               &
     &     /'Too Many Numerical Values Specified for           '/
      DATA ERRCOD(64)/'232'/ , ERRMSG(64)                               &
     &     /'Number Of Specified Sources Exceeds Maximum: NSRC='/
      DATA ERRCOD(65)/'233'/ , ERRMSG(65)                               &
     &     /'Building Dimensions Specified for Non-POINT Source'/
      DATA ERRCOD(66)/'234'/ , ERRMSG(66)                               &
     &     /'Too Many Sectors Input for                        '/
      DATA ERRCOD(67)/'235'/ , ERRMSG(67)                               &
     &     /'Number of Source Groups Exceeds Maximum:  NGRP=   '/
      DATA ERRCOD(68)/'236'/ , ERRMSG(68)                               &
     &     /'Not Enough BUILDHGTs Specified for SourceID       '/
      DATA ERRCOD(69)/'237'/ , ERRMSG(69)                               &
     &     /'Not Enough BUILDWIDs Specified for SourceID       '/
      DATA ERRCOD(70)/'239'/ , ERRMSG(70)                               &
     &     /'Not Enough QFACTs Specified for SourceID          '/
      DATA ERRCOD(71)/'240'/ , ERRMSG(71)                               &
     &     /'Inconsistent Number of Particle Categories for    '/
      DATA ERRCOD(72)/'241'/ , ERRMSG(72)                               &
     &     /'Not Enough BUILDLENs Specified for SourceID       '/
      DATA ERRCOD(73)/'242'/ , ERRMSG(73)                               &
     &     /'No Particle Cat. or Gas Depos. Specified for SRCID'/
      DATA ERRCOD(74)/'243'/ , ERRMSG(74)                               &
     &     /'Scav. Coef. may be out-of-range for SRCID         '/
      DATA ERRCOD(75)/'244'/ , ERRMSG(75)                               &
     &     /'Too Many Particle Categories Specified for        '/
      DATA ERRCOD(76)/'245'/ , ERRMSG(76)                               &
     &     /'No. of Particle Categories Exceeds Max:  NPDMAX=  '/
      DATA ERRCOD(77)/'246'/ , ERRMSG(77)                               &
     &     /'Not Enough XBADJs Specified for SourceID          '/
      DATA ERRCOD(78)/'247'/ , ERRMSG(78)                               &
     &     /'Not Enough YBADJs Specified for SourceID          '/
      DATA ERRCOD(79)/'248'/ , ERRMSG(79)                               &
     &     /'No Sources Were Defined on the SO Pathway.        '/
      DATA ERRCOD(80)/'250'/ , ERRMSG(80)                               &
     &     /'Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'/
      DATA ERRCOD(81)/'252'/ , ERRMSG(81)                               &
     &     /'Duplicate Receptor Network ID Specified.  NETID = '/
      DATA ERRCOD(82)/'254'/ , ERRMSG(82)                               &
     &     /'Number of Receptor ARCs Exceeds Max:       NARC=  '/
      DATA ERRCOD(83)/'256'/ , ERRMSG(83)                               &
     &     /'EVALFILE Option Used Without EVALCART Receptors   '/
      DATA ERRCOD(84)/'260'/ , ERRMSG(84)                               &
     &     /'Number of Emission Factors Exceeds Max:      NQF= '/
      DATA ERRCOD(85)/'262'/ , ERRMSG(85)                               &
     &     /'First Vertex Does Not Match LOCATION for AREAPOLY '/
      DATA ERRCOD(86)/'264'/ , ERRMSG(86)                               &
     &     /'Too Many Vertices Specified for AREAPOLY Source   '/
      DATA ERRCOD(87)/'265'/ , ERRMSG(87)                               &
     &     /'Not Enough Vertices Specified for AREAPOLY Source '/
      DATA ERRCOD(88)/'270'/ , ERRMSG(88)                               &
     &     /'Number of High Values Specified Exceeds Max: NVAL='/
      DATA ERRCOD(89)/'280'/ , ERRMSG(89)                               &
     &     /'Number of Max Values Specified Exceeds Max:  NMAX='/
      DATA ERRCOD(90)/'281'/ , ERRMSG(90)                               &
     &     /'Number of OLMGROUPs Specified Exceeds Max: NOLM=  '/
      DATA ERRCOD(91)/'282'/ , ERRMSG(91)                               &
     &     /'Following SRCID Included in Multiple OLMGROUPs:   '/
      DATA ERRCOD(92)/'283'/ , ERRMSG(92)                               &
     &     /'Either OZONEVAL or OZONEFIL Card Needed for Option'/
      DATA ERRCOD(93)/'284'/ , ERRMSG(93)                               &
     &     /'Invalid POLLUTID Specified for PVMRM/OLM; Must Use'/
      DATA ERRCOD(94)/'290'/ , ERRMSG(94)                               &
     &     /'Number of Output Types Specified Exceeds Max:NTYP='/
      DATA ERRCOD(95)/'294'/ , ERRMSG(95)                               &
     &     /'PERIOD and ANNUAL averages are both selected for  '/
      DATA ERRCOD(96)/'295'/ , ERRMSG(96)                               &
     &     /'Invalid Averaging Period Specified for SCREEN Mode'/
      DATA ERRCOD(97)/'298'/ , ERRMSG(97)                               &
     &     /'Error Allocating Storage for Setup Arrays!        '/
      DATA ERRCOD(98)/'299'/ , ERRMSG(98)                               &
     &     /'Error Allocating Storage for Result Arrays!       '/
      DATA ERRCOD(99)/'300'/ , ERRMSG(99)                               &
     &     /'Specified SRCID Has Not Been Defined Yet: KEYWORD='/
      DATA ERRCOD(100)/'305'/ , ERRMSG(100)                             &
     &     /'Terrain Grid Does Not Cover Modeling Area, Change:'/
      DATA ERRCOD(101)/'310'/ , ERRMSG(101)                             &
     &     /'Attempt to Define Duplicate LOCATION Card for SRC:'/
      DATA ERRCOD(102)/'313'/ , ERRMSG(102)                             &
     &     /'Attempt to Define Duplicate EVENTPER card for     '/
      DATA ERRCOD(103)/'315'/ , ERRMSG(103)                             &
     &     /'Attempt to Define Duplicate SRCPARAM Card for SRC:'/
      DATA ERRCOD(104)/'319'/ , ERRMSG(104)                             &
     &     /'No Sources Included in Specified Source Group:    '/
      DATA ERRCOD(105)/'320'/ , ERRMSG(105)                             &
     &     /'Input Parameter May Be Out-of-Range for Parameter '/
      DATA ERRCOD(106)/'322'/ , ERRMSG(106)                             &
     &     /'Release Height Exceeds Effective Depth for OPENPIT'/
      DATA ERRCOD(107)/'323'/ , ERRMSG(107)                             &
     &     /'No Particle Categories Specified for OPENPIT Src. '/
      DATA ERRCOD(108)/'325'/ , ERRMSG(108)                             &
     &     /'Negative Exit Velocity (Set=1.0E-5) for SRCID:    '/
      DATA ERRCOD(109)/'330'/ , ERRMSG(109)                             &
     &     /'Mass Fraction Parameters Do Not Sum to 1. for Src '/
      DATA ERRCOD(110)/'332'/ , ERRMSG(110)                             &
     &     /'Mass Fraction Parameter Out-of-Range for Source   '/
      DATA ERRCOD(111)/'334'/ , ERRMSG(111)                             &
     &     /'Particle Density Out-of-Range for Source          '/
      DATA ERRCOD(112)/'336'/ , ERRMSG(112)                             &
     &     /'Value Specified for NO2RATIO is Out-of-Range for  '/
      DATA ERRCOD(113)/'338'/ , ERRMSG(113)                             &
     &     /'Neg Emis Rate Cannot be Used with OLM/PVMRM. Src: '/
      DATA ERRCOD(114)/'340'/ , ERRMSG(114)                             &
     &     /'Possible Error in PROFBASE Input:  Value is < 0   '/
      DATA ERRCOD(115)/'342'/ , ERRMSG(115)                             &
     &     /'Src ID Mismatch in Hourly Emissions File for ID = '/
      DATA ERRCOD(116)/'344'/ , ERRMSG(116)                             &
     &     /'Hourly Emission Rate is Zero for KURDAT =         '/
      DATA ERRCOD(117)/'350'/ , ERRMSG(117)                             &
     &     /'Julian Day Out Of Range at                        '/
      DATA ERRCOD(118)/'352'/ , ERRMSG(118)                             &
     &     /'Missing Field on MULTYEAR Card for Pre-1997 PM10  '/
      DATA ERRCOD(119)/'353'/ , ERRMSG(119)                             &
     &     /'MULTYEAR Card for PM10 Processing Applies Only for'/
      DATA ERRCOD(120)/'354'/ , ERRMSG(120)                             &
     &     /'High-4th-High Only Required for Post-1997 PM10    '/
      DATA ERRCOD(121)/'360'/ , ERRMSG(121)                             &
     &     /'2-Digit Year Specified: Valid for Range 1950-2049 '/
      DATA ERRCOD(122)/'363'/ , ERRMSG(122)                             &
     &     /'24HR and ANNUAL Averages Only for Post-1997 PM10  '/
      DATA ERRCOD(123)/'365'/ , ERRMSG(123)                             &
     &     /'Year Input is Greater Than 2147                   '/
      DATA ERRCOD(124)/'370'/ , ERRMSG(124)                             &
     &     /'Invalid Date: 2/29 In a Non-leap Year.            '/
      DATA ERRCOD(125)/'380'/ , ERRMSG(125)                             &
     &     /'This Input Variable is Out-of-Range:              '/
      DATA ERRCOD(126)/'381'/ , ERRMSG(126)                             &
     &     /'Latitude in Surface File Is Not Valid:            '/
      DATA ERRCOD(127)/'382'/ , ERRMSG(127)                             &
     &     /'Error Decoding Latitude:                          '/
      DATA ERRCOD(128)/'383'/ , ERRMSG(128)                             &
     &     /'NWETFREQ > 0, but Wet Dep/Depletion not selected  '/
      DATA ERRCOD(129)/'385'/ , ERRMSG(129)                             &
     &     /'Averaging Period .NE. 1-Hr for TOXXFILE Option    '/
      DATA ERRCOD(130)/'390'/ , ERRMSG(130)                             &
     &     /'Aver. Period must be .LE. 24 for EVENT Processing '/
      DATA ERRCOD(131)/'391'/ , ERRMSG(131)                             &
     &     /'Aspect ratio (L/W) of area source greater than 10 '/
      DATA ERRCOD(132)/'392'/ , ERRMSG(132)                             &
     &     /'Aspect ratio (L/W) of open pit is greater than 10 '/
      DATA ERRCOD(133)/'393'/ , ERRMSG(133)                             &
     &     /'Terrain Grid Value Differs >50% From Source Elev. '/
      DATA ERRCOD(134)/'394'/ , ERRMSG(134)                             &
     &     /'Terrain Grid Value Differs >50% From Receptor Elev'/
      DATA ERRCOD(135)/'395'/ , ERRMSG(135)                             &
     &     /'Met. Data Error; Incompatible Version of AERMET:  '/
      DATA ERRCOD(136)/'396'/ , ERRMSG(136)                             &
     &     /'Met. Data Generated by Older Version of AERMET:   '/
      DATA ERRCOD(137)/'405'/ , ERRMSG(137)                             &
     &     /'Value of PHEE Exceeds 1.0 on KURDAT =             '/
      DATA ERRCOD(138)/'406'/ , ERRMSG(138)                             &
     &     /'Increase NVMAX for Complex AREAPOLY Source        '/
      DATA ERRCOD(139)/'410'/ , ERRMSG(139)                             &
     &     /'Wind Direction Out-of-Range.  KURDAT=             '/
      DATA ERRCOD(140)/'413'/ , ERRMSG(140)                             &
     &     /'Number of Threshold Events > 9999 for Ave Period  '/
      DATA ERRCOD(141)/'420'/ , ERRMSG(141)                             &
     &     /'Wind Speed Out-of-Range.   KURDAT=                '/
      DATA ERRCOD(142)/'430'/ , ERRMSG(142)                             &
     &     /'Ambient Temperature Data Out-of-Range.  KURDAT=   '/
      DATA ERRCOD(143)/'432'/ , ERRMSG(143)                             &
     &     /'Friction Velocity Out-of-Range.   KURDAT=         '/
      DATA ERRCOD(144)/'435'/ , ERRMSG(144)                             &
     &     /'Surface Roughness Length Out-of-Range.  KURDAT=   '/
      DATA ERRCOD(145)/'438'/ , ERRMSG(145)                             &
     &     /'Convective Velocity Data Out-of-Range.  KURDAT=   '/
      DATA ERRCOD(146)/'439'/ , ERRMSG(146)                             &
     &     /'Monin-Obukhov Length Out-of-Range.  KURDAT=       '/
      DATA ERRCOD(147)/'440'/ , ERRMSG(147)                             &
     &     /'Calm Hour Identified in Meteorology Data File at  '/
      DATA ERRCOD(148)/'450'/ , ERRMSG(148)                             &
     &     /'Error in Meteor. File - Record Out of Sequence at '/
      DATA ERRCOD(149)/'455'/ , ERRMSG(149)                             &
     &     /'Date/time Mismatch: Hourly Emission File. KURDAT ='/
      DATA ERRCOD(150)/'456'/ , ERRMSG(150)                             &
     &     /'Date/time Mismatch on Surface & Profile. KURDAT = '/
      DATA ERRCOD(151)/'457'/ , ERRMSG(151)                             &
     &     /'Date/time Mismatch on OZONEFIL Data.  KURDAT =    '/
      DATA ERRCOD(152)/'458'/ , ERRMSG(152)                             &
     &     /'Substitution made for missing ozone data. KURDAT= '/
      DATA ERRCOD(153)/'459'/ , ERRMSG(153)                             &
     &     /'Missing ozone data; Full conversion used. KURDAT= '/
      DATA ERRCOD(154)/'460'/ , ERRMSG(154)                             &
     &     /'Missing Hour Identified in Meteor. Data File at   '/
      DATA ERRCOD(155)/'465'/ , ERRMSG(155)                             &
     &     /'Number of Profile Levels Exceeds Max:   MXPLVL=   '/
      DATA ERRCOD(156)/'470'/ , ERRMSG(156)                             &
     &     /'Mixing Height Value is < or = 0.0.   KURDAT=      '/
      DATA ERRCOD(157)/'475'/ , ERRMSG(157)                             &
     &     /'Reference height is higher than 100m.  KURDAT=    '/
      DATA ERRCOD(158)/'480'/ , ERRMSG(158)                             &
     &     /'Less Than 1 Year Found for ANNUAL Averages        '/
      DATA ERRCOD(159)/'485'/ , ERRMSG(159)                             &
     &     /'Data Remaining After End of Year. Number of Hours='/
      DATA ERRCOD(160)/'487'/ , ERRMSG(160)                             &
     &     /'User Start Date is Earlier Than Start of Data File'/
      DATA ERRCOD(161)/'500'/ , ERRMSG(161)                             &
     &     /'Fatal Error Occurs Opening the Data File of       '/
      DATA ERRCOD(162)/'510'/ , ERRMSG(162)                             &
     &     /'Fatal Error Occurs During Reading of the File of  '/
      DATA ERRCOD(163)/'520'/ , ERRMSG(163)                             &
     &     /'Fatal Error Occurs During Writing to the File of  '/
      DATA ERRCOD(164)/'530'/ , ERRMSG(164)                             &
     &     /'CAUTION! Met Station ID Mismatch with SURFFILE for'/
      DATA ERRCOD(165)/'540'/ , ERRMSG(165)                             &
     &     /'No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '/
      DATA ERRCOD(166)/'550'/ , ERRMSG(166)                             &
     &     /'File Unit/Name Conflict for the Output Option:    '/
      DATA ERRCOD(167)/'560'/ , ERRMSG(167)                             &
     &     /'User Specified File Unit .LE. 25 for OU Keyword:  '/
      DATA ERRCOD(168)/'565'/ , ERRMSG(168)                             &
     &     /'Possible Conflict With Dynamically Allocated FUNIT'/
      DATA ERRCOD(169)/'570'/ , ERRMSG(169)                             &
     &     /'Problem Reading Temporary Event File for Event:   '/
      DATA ERRCOD(170)/'580'/ , ERRMSG(170)                             &
     &     /'End of File Reached Trying to Read the File of    '/
      END
      MODULE DEPVAR
      IMPLICIT NONE
      REAL , PARAMETER :: RTPIBY2 = 1.2533141 , RT2 = 1.4142136 ,       &
     &                    RTPI = 1.7724539
      LOGICAL :: RURAL , URBAN , DEBUG , LTOXICS
      CHARACTER :: SRCTYP*8
      REAL :: VD , VS , ZD , AP , BP , CP , AR , BR , CR , HMIX ,       &
     &        ONEBYU , ER , EP , XSRC , YSRC , XREC , YREC , XR , XV ,  &
     &        H , SGZ , SGZ0 , XTD , SZTD , SZMN
      INTEGER :: IGRAV , KST , IOUNIT , KURDAT
      LOGICAL LTGRID
      INTEGER(KIND=2) , ALLOCATABLE :: IZARRAY(:,:)
      REAL :: XLLM , YLLM , SIZEM , XURM , YURM
      INTEGER :: NTX , NTY
      END
      SUBROUTINE HRLOOP
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: IEND_DAY , I , J , K , L , M , ILSAVE
      REAL :: RDUM
      REAL :: O3VALUES(24) , O3MIN , O3MAX24
      DATA O3VALUES/24*78.4/
      SAVE 
      MODNAM = 'HRLOOP'
      EOF = .FALSE.
      KURDAT = 0
      FULLDATE = 0
      NTOTHRS = 0
      DO WHILE ( FULLDATE.LT.IEDATE .AND. .NOT.EOF )
         CALL METEXT
         IF ( FULLDATE.LE.IEDATE .AND. .NOT.EOF ) THEN
            NTOTHRS = NTOTHRS + 1
         ELSE
            GOTO 200
         ENDIF
         ILSAVE = ILINE
         ILINE = 0
         DO ISRC = 1 , NUMSRC
            IF ( QFLAG(ISRC).EQ.'HOURLY' ) CALL HRQEXT(ISRC)
         ENDDO
         ILINE = ILSAVE
         IF ( PVMRM .OR. OLM ) THEN
            IF ( O3FILE ) THEN
               CALL O3EXT
               IF ( .NOT.O3MISS ) THEN
                  O3VALUES(IHOUR) = O3CONC
               ELSE
                  O3VALUES(IHOUR) = 0.0
               ENDIF
               IF ( STABLE ) THEN
                  O3MAX24 = MIN(78.40,MAXVAL(O3VALUES))
                  IF ( OBULEN.GT.0.0 .AND. OBULEN.LE.50.0 ) THEN
                     O3MIN = O3MAX24
                  ELSEIF ( OBULEN.GT.250.0 ) THEN
                     O3MIN = 0.0
                  ELSE
                     O3MIN = O3MAX24*(250.-OBULEN)/200.
                  ENDIF
                  O3CONC = MAX(O3CONC,O3MIN)
               ENDIF
            ELSE
               O3CONC = O3BACK
            ENDIF
         ENDIF
         IF ( (IHOUR.EQ.1 .OR. ILINE.EQ.1) .AND. .NOT.NOCHKD ) THEN
            WRITE (*,909) JDAY , IYR
 909        FORMAT ('+','Now Processing Data For Day No. ',I4,' of ',I4)
         ELSEIF ( NOCHKD ) THEN
            WRITE (*,910) KURDAT
 910        FORMAT ('+','Now Processing Data For     ',I8.8)
         ENDIF
         IF ( SCIM .AND. .NOT.EOF ) THEN
            SCIMHR = .FALSE.
            WETHR = .FALSE.
            NSKIPTOT = NSKIPTOT + 1
            IF ( PRATE.GT.0.0 ) THEN
               NSKIPWET = NSKIPWET + 1
            ELSE
               NSKIPDRY = NSKIPDRY + 1
            ENDIF
            IF ( CLMHR .AND. CLMPRO ) THEN
               IF ( PRATE.GT.0.0 ) THEN
                  NSWETCLM = NSWETCLM + 1
               ELSE
                  NSDRYCLM = NSDRYCLM + 1
               ENDIF
            ELSEIF ( MSGHR .AND. MSGPRO ) THEN
               IF ( PRATE.GT.0.0 ) THEN
                  NSWETMSG = NSWETMSG + 1
               ELSE
                  NSDRYMSG = NSDRYMSG + 1
               ENDIF
            ENDIF
            IF ( ILINE.LE.24 .AND. IHOUR.EQ.NREGSTART ) THEN
               IFIRSTHR = ILINE
               SCIMHR = .TRUE.
               IF ( WETSCIM .AND. PRATE.GT.0.0 ) THEN
                  NWETHR = NWETHR + 1
                  IF ( FIRSTWET .AND. NWETHR.EQ.NWETSTART ) THEN
                     FIRSTWET = .FALSE.
                     WETHR = .TRUE.
                     NWETHR = 0
                  ELSEIF ( NWETHR.EQ.NWETINT ) THEN
                     WETHR = .TRUE.
                     NWETHR = 0
                  ENDIF
               ENDIF
            ELSEIF ( ILINE.GT.NREGSTART .AND.                           &
     &               MOD(ILINE-IFIRSTHR,NREGINT).EQ.0 ) THEN
               SCIMHR = .TRUE.
               IF ( WETSCIM .AND. PRATE.GT.0.0 ) THEN
                  NWETHR = NWETHR + 1
                  IF ( FIRSTWET .AND. NWETHR.EQ.NWETSTART ) THEN
                     FIRSTWET = .FALSE.
                     WETHR = .TRUE.
                     NWETHR = 0
                  ELSEIF ( NWETHR.EQ.NWETINT ) THEN
                     WETHR = .TRUE.
                     NWETHR = 0
                  ENDIF
               ENDIF
            ELSEIF ( WETSCIM .AND. PRATE.GT.0.0 .AND.                   &
     &               (DEPOS .OR. WDEP .OR. WDPLETE) ) THEN
               NWETHR = NWETHR + 1
               IF ( FIRSTWET .AND. NWETHR.EQ.NWETSTART ) THEN
                  FIRSTWET = .FALSE.
                  WETHR = .TRUE.
                  NWETHR = 0
               ELSEIF ( NWETHR.EQ.NWETINT ) THEN
                  WETHR = .TRUE.
                  NWETHR = 0
               ELSE
                  CALL CHK_ENDYR
                  GOTO 100
               ENDIF
            ELSE
               CALL CHK_ENDYR
               GOTO 100
            ENDIF
            IF ( SCIMOUT ) CALL METSUM
         ENDIF
         IF ( FULLDATE.GT.ISDATE .AND. FULLDATE.LE.IEDATE .AND.         &
     &        IPROC(JDAY).EQ.1 .AND. .NOT.EOF .AND. .NOT.RUNERR ) THEN
            IF ( CLMHR .AND. CLMPRO ) THEN
               DO IAVE = 1 , NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMCLM(IAVE) = NUMCLM(IAVE) + 1
               ENDDO
               IF ( PERIOD .OR. ANNUAL ) THEN
                  IF ( .NOT.SCIM .OR. (SCIM .AND. SCIMHR) ) THEN
                     IANHRS = IANHRS + 1
                     IANCLM = IANCLM + 1
                  ENDIF
                  IF ( SCIM .AND. WETHR ) THEN
                     IANWET = IANWET + 1
                     IWETCLM = IWETCLM + 1
                  ENDIF
               ENDIF
               IF ( SEASONHR ) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
                  NSEACM(ISEAS,IHOUR) = NSEACM(ISEAS,IHOUR) + 1
               ENDIF
            ELSEIF ( MSGHR .AND. MSGPRO ) THEN
               DO IAVE = 1 , NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMMSG(IAVE) = NUMMSG(IAVE) + 1
               ENDDO
               IF ( PERIOD .OR. ANNUAL ) THEN
                  IF ( .NOT.SCIM .OR. (SCIM .AND. SCIMHR) ) THEN
                     IANHRS = IANHRS + 1
                     IANMSG = IANMSG + 1
                  ENDIF
                  IF ( SCIM .AND. WETHR ) THEN
                     IANWET = IANWET + 1
                     IWETMSG = IWETMSG + 1
                  ENDIF
               ENDIF
               IF ( SEASONHR ) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
                  NSEACM(ISEAS,IHOUR) = NSEACM(ISEAS,IHOUR) + 1
               ENDIF
            ELSEIF ( ZI.LE.0 ) THEN
               WRITE (DUMMY,'(I8.8)') KURDAT
               CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
               DO IAVE = 1 , NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
               ENDDO
               IF ( PERIOD .OR. ANNUAL ) THEN
                  IF ( .NOT.SCIM .OR. (SCIM .AND. SCIMHR) )             &
     &                 IANHRS = IANHRS + 1
                  IF ( SCIM .AND. WETHR ) IANWET = IANWET + 1
               ENDIF
               IF ( SEASONHR ) NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR)&
     &              + 1
            ELSE
               CALCS = .TRUE.
               DO IAVE = 1 , NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
               ENDDO
               IF ( PERIOD .OR. ANNUAL ) THEN
                  IF ( .NOT.SCIM .OR. (SCIM .AND. SCIMHR) )             &
     &                 IANHRS = IANHRS + 1
                  IF ( SCIM .AND. WETHR ) IANWET = IANWET + 1
               ENDIF
               IF ( SEASONHR ) NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR)&
     &              + 1
               IF ( DEBUG ) THEN
                  WRITE (DBGUNT,*)
                  WRITE (DBGUNT,*) '--------------------------------' , &
     &                             '--------------------'
                  WRITE (DBGUNT,*) '---  JDAY, IHOUR =  ' , JDAY , IHOUR
                  WRITE (DBGUNT,*) '--------------------------------' , &
     &                             '--------------------'
               ENDIF
               CALL CALC
            ENDIF
            IF ( PVMRM .AND. .NOT.O3MISS .AND. .NOT.CLMHR .AND.         &
     &           .NOT.MSGHR ) THEN
               CALL PVMRM_CALC
            ELSEIF ( OLM .AND. .NOT.O3MISS .AND. .NOT.CLMHR .AND.       &
     &               .NOT.MSGHR ) THEN
               CALL OLM_CALC
            ENDIF
            DO IAVE = 1 , NUMAVE
               IF ( MOD(IHOUR,KAVE(IAVE)).EQ.0 .OR.                     &
     &              (KAVE(IAVE).EQ.720 .AND. ENDMON) ) THEN
                  IF ( CONC ) CALL AVER
                  CALL HIVALS
                  IF ( DAYTAB .AND. IDYTAB(IAVE).EQ.1 ) THEN
                     DO ITYP = 1 , NUMTYP
                        CALL PRTDAY
                     ENDDO
                  ENDIF
                  IF ( MXFILE ) CALL MAXFIL
                  IF ( PPFILE ) CALL POSTFL
                  IF ( TXFILE ) CALL TOXXFL
                  DO ITYP = 1 , NUMTYP
                     DO IGRP = 1 , NUMGRP
                        DO IREC = 1 , NUMREC
                           AVEVAL(IREC,IGRP,IAVE,ITYP) = 0.0
                        ENDDO
                     ENDDO
                  ENDDO
               ENDIF
            ENDDO
            IF ( RSTSAV .AND. IHOUR.EQ.24 ) THEN
               NDAYS = NDAYS + 1
               IF ( NDAYS.EQ.INCRST ) THEN
                  CALL RSDUMP
                  NDAYS = 0
               ENDIF
            ENDIF
            DO ITYP = 1 , NUMTYP
               HRVAL(ITYP) = 0.0
               HRVALD(ITYP) = 0.0
               AERVAL(ITYP) = 0.0
               AERVALD(ITYP) = 0.0
               PRMVAL(ITYP) = 0.0
               PRMVALD(ITYP) = 0.0
            ENDDO
            IF ( PVMRM .OR. OLM ) THEN
               DO ITYP = 1 , NUMTYP
                  DO ISRC = 1 , NUMSRC
                     DO IREC = 1 , NUMREC
                        CHI(IREC,ISRC,ITYP) = 0.0
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
         IF ( (PM10AVE .OR. ANNUAL) .AND. FULLDATE.GT.ISDATE .AND.      &
     &        .NOT.EOF ) CALL CHK_ENDYR
         CALCS = .FALSE.
         ENDMON = .FALSE.
         PREC2 = PREC1
         PREC1 = PRATE
 100  ENDDO
 200  IF ( TXFILE ) THEN
         IDUM = 0
         RDUM = 0.0
         DO IAVE = 1 , NUMAVE
            IF ( ITOXFL(IAVE).EQ.1 ) THEN
               DO I = IPAIR + 1 , NPAIR
                  IDCONC(IAVE,I) = IDUM
                  TXCONC(IAVE,I) = RDUM
               ENDDO
               WRITE (ITXUNT(IAVE)) (IDCONC(IAVE,I),I=1,NPAIR)
               WRITE (ITXUNT(IAVE)) (TXCONC(IAVE,I),I=1,NPAIR)
               CLOSE (ITXUNT(IAVE))
            ENDIF
         ENDDO
      ENDIF
      WRITE (*,919)
 919  FORMAT ('+','Now Processing Output Options               ')
      CONTINUE
      END
