C
C  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  !                                                                  !
C  !               PROGRAMMPAKET   " H Y D R E T  "                   !
C  !                                                                  !
C  !                    VERSION VOM FEBRUAR 2009                      !
C  !                                                                  !
C  !                    (ZUSAETZLICHER COMMON BLOCK / VORSCH /        !
C  !                     EINGEFUEGT, UM DEN AUSFUEHRLICHEN TEST-      !
C  !                     AUSDRUCK UNTERDRUECKEN ZU KOENNEN!)          !
C  !                                                                  !
C  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  !                                                                  !
C  !    ===>  ERWEITERT UM RETENTIONSFLAECHEN                         !
C  !                                                                  !
C  !          (ALLE EINFUEGUNGEN SIND DURCH:   C&&&   GEKENNZEICHNET) !
C  !                                                                  !
C  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  !                                                                  !
C  !  STAND: ORIGINALPROGRAMM                                         !
C  !         PLUS                                                     !
C  !         - BELIEBIGE KNOTENNUMERIERUNG                            !
C  !         - EIN/AUSGABE IN SI-EINHEITEN                            !
C  !         - NEUE FORM DES EINGABEDATENSATZES                       !
C  !         - NEUE FORM DER PROGRAMMAUSGABE                          !
C  !         - ABFLUSS- UND WASSERSTANDSGANGLINIE FUER BELIEBIGE      !
C  !             QUERSCHNITTE                                         !
C  !         - VARIABLE PROGRAMMDIMENSIONIERUNG                       !
C  !         - MOEGLICHKEIT VON UNREGELMAESSIGEN QUERSCHNITTEN        !
C  !            (AUCH AM GEBIETSAUSLASS)                              !
C  !         - GATES                                                  !
C  !         - WEHR (NUR FREIER UEBERFALL)                            !
C  !         - NEUE BERECHNUNGSWEISE DES SEITLICHEN UEBERLAUFES       !
C  !               (VERBESSERT UND GETESTET)                          !
C  !         - KOPPLUNG AN DRAINAGEMODELL MOEGLICH                    !
C  !         - AUSGABE DER ERGEBNISSE AUF PLOTDATEIEN                 !
C  !                                                                  !
C  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C      PROGRAMM BERECHNET DEN INSTATIONAEREN ABFLUSSVORGANG IN
C      EINEM GEWAESSERNETZ MIT HILFE EINES IMPLIZITEN DIFFERENZEN-
C      VERFAHRENS.
C
C      PROGRAMMHERKUNFT : U.S.A. (ERHALTEN BEI SHORT COURSE "INNOVATION
C                                IN STORMWATER MANAGEMENT" IN SOUTH-
C                                HAMPTON (1981)
C      WEITERENTWICKLUNG: INSTITUT FUER HYDROLOGIE UND WASSERWIRTSCHAFT
C                         AN DER UNIVERSITAET KARLSRUHE (TH)
C      SACHBEARBEITER:    WOLFGANG KRON UND JOACHIM WALD
C
C  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      PROGRAM HYDRET
C
C
      INCLUDE 'DIMENS.INC'
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /TEXT/         TITLE1,TITLE2
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) ,XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      CHARACTER*80 TITLE1,TITLE2,TITLE3,TITLE4
      CHARACTER*12 SIMDAT
      CHARACTER*10 SIMTIM
      CHARACTER*30 DATIN,DATOUT,VERSION
      CHARACTER*1 PLOT,FORM
      CHARACTER*255 GEWSHP,GEWDBF
      COMMON /SHPDBF/ GEWSHP,GEWDBF
C
      CHARACTER*6 OVFMOD
      COMMON /OVFBIL/ OVFMOD,NOVF(IPSEC),IABOVF(IPSEC),IOVF(IPSEC),
     1                QOFLSN(100),NOVFMX,LIRE(IPSEC),OVFAN(IPSEC),
     2                OVFAUS(IPSEC),IOVFSTAT(IPSEC),QOMAX(IPSEC),
     3                QREGEL(IPSEC),TOVFAN(IPSEC),TOVFAUS(IPSEC)
C             wird WSP > OVFAN wird QOFLOW() berechnet
C             wird WSP wieder < OVFAUS wird QOFLOW() nicht berechnet
C
      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
C
      COMMON /RELAX/  IRELAXFOR, IRELAXBACK, DHZUL, DHRELZUL, DVRELZUL
C
      COMMON /BNDRY/  FLOW, DHH, DHT, ZSILL
C
      DATA PLOT /'N'/
      DATA FORM /'A'/
C
      VERSION='08/2018 Dr. Gerd R. Schiffler'
C
      OPEN(7,FILE='Hydret06.mon',STATUS='UNKNOWN')
C
      N5=15
      N6=16
C
      CALL LICTEST
C
      OPEN(10,FILE='STOP',STATUS='UNKNOWN')
      CLOSE(10,STATUS='KEEP')
C
      WRITE(*,*)  'PROGRAMM HYDRET FUER BATCH-BETRIEB LAEUT !'
      WRITE(*,*)
      WRITE(*,*)
c     WRITE(*,*)  'EINGABEDATEI :'
c     READ(*,1) DATIN
c     WRITE(*,*)  'AUSGABEDATEI :'
c     READ(*,1) DATOUT
      GEWSHP(1:5)='keine'
      OVFMOD(1:5)='keine'
      KSTMOD(1:5)='keine'

      DHZUL=999999.9
      DHRELZUL=999999.9
      DVRELZUL=999999.9

      OPEN (17,FILE='HYDRET06.RUN',STATUS='OLD')
      READ(17,1) DATIN
      READ(17,1) DATOUT
      PRINT'(T2,A)',DATIN
      PRINT'(T2,A)',DATOUT
      READ(17,1) PLOT
      READ(17,1,END=13) FORM
      READ(17,1,END=13) GEWSHP
      GEWDBF=GEWSHP
      DO IP=1,255
         IF (GEWSHP(IP:IP).EQ.'.') THEN
            GEWDBF(IP:(IP+3))='.dbf'
         END IF
      END DO
      READ(17,1,END=13) OVFMOD
      READ(17,*,END=13) DHZUL, DHRELZUL, DVRELZUL
      READ(17,1,END=13) KSTMOD
   13 CLOSE(17,STATUS='KEEP')
      PRINT'(T2,A,A)','GEWSHP = ',GEWSHP
      PRINT'(T2,A,A)','GEWDBF = ',GEWDBF
      PRINT'(T2,A,A)','OVFMOD = ',OVFMOD
      PRINT'(T2,A,F10.2)',' WSP / SF-DAEMPFUNG AB:   DHZUL = ',DHZUL
      PRINT'(T2,A,F10.2)','                       DHRELZUL = ',DHRELZUL
      PRINT'(T2,A,F10.2)','   DV/DT-DAEMPFUNG AB: DVRELZUL = ',DVRELZUL
      PRINT'(T2,A,A)','KSTMOD = ',KSTMOD
    1 FORMAT(A)
C
C     PRINT'(T2,A,F10.3)',' DHH = ',ENGSI(DHH,1)
C     PRINT'(T2,A,F10.3)',' DHT = ',ENGSI(DHT,1)
C
      OPEN (UNIT=N5,FILE=DATIN,STATUS='OLD')
      OPEN (UNIT=N6,FILE=DATOUT)

      CALL DATE1 (SIMDAT,SIMTIM,HA)
      WRITE (*,90) SIMDAT,SIMTIM
      WRITE (N6,90) SIMDAT,SIMTIM
  90  FORMAT (//1X,'SIMULATIONSLAUF VOM ',A,' UM ',A,' UHR'//)
C
      READ (N5,101,END=111) TITLE1
      READ (N5,101,END=111) TITLE2
      READ (N5,101,END=111) TITLE3
      READ (N5,101,END=111) TITLE4

      WRITE (N6,100) TITLE1
      WRITE (N6,100) TITLE2
      WRITE (N6,100) TITLE3
      WRITE (N6,100) TITLE4
C
      CALL UNSTDY(VERSION,TITLE4,DATIN,PLOT,FORM)
C
      CALL DATE1 (SIMDAT,SIMTIM,HE)
C
      WRITE (*,91) SIMDAT,SIMTIM
      WRITE (N6,91) SIMDAT,SIMTIM
  91  FORMAT (//1X,'SIMULATIONSLAUF BEENDET AM ',A,' UM ',A,' UHR'//)
      DELTA = (HE-HA)/100.
      WRITE (N6,92) DELTA
      WRITE (*,92) DELTA
  92  FORMAT (/1X,'BENOETIGTE RECHENZEIT : ',F7.2,' SEC'//,1X,80('*'))
C
  100 FORMAT (1X,A)
  101 FORMAT (A)
C
      OPEN(10,FILE='STOP',STATUS='UNKNOWN')
      CLOSE(10,STATUS='DELETE')
C
  111 WRITE (*,*) 'N5 und N6 werden geschlossen!'
C
      CLOSE (UNIT=N5)
      CLOSE (UNIT=N6)
C
      STOP
      END
C&&&  =================================================================
      BLOCKDATA INIT
      INCLUDE 'DIMENS.INC'
C
      COMMON /READ/         ITERAT
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /FLOWS/        NQIN   , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /LIN/          QINFLO(IPSEC)
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /BILAN/ QKNOTN(IPSEC),SQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) ,XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      DATA (QOSUM(I), I=1,IPSEC)/IPSEC*0./
      DATA (QKNOTN(I), I=1,IPSEC)/IPSEC*0./
      DATA (SQG(1,I), I=1,IPLAT)/IPLAT*0./
      DATA (SQG(2,I), I=1,IPLAT)/IPLAT*0./
      DATA (SQG(3,I), I=1,IPLAT)/IPLAT*0./
      DATA (SQG(4,I), I=1,IPLAT)/IPLAT*0./
      DATA (NXJ(I,1), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,2), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,3), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,4), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,5), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,6), I=1,IPJUN)/IPJUN*0./
      DATA (NXJ(I,7), I=1,IPJUN)/IPJUN*0./
C
      DATA DHH/0.1/,DHT/0.2/
      DATA QLAT1/IPSEC*0./
      DATA ITERAT/0/
      DATA DZERO(12)/0.000/
      DATA NUP(1)/1/
      DATA QINFLO/IPSEC*0.0/
      DATA L1/IPLAT*0/
      DATA IGRAPH/0/
      DATA GRAV/32.174/
      DATA GATEL(IPGAT)/0.999/
      END
