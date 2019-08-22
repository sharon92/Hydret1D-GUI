      SUBROUTINE INPUT
C     THIS SUBROUTINE READS IN AND PRINTS OUT DATA
C
      INCLUDE 'DIMENS.INC'

      CHARACTER*1  GATMOD
      CHARACTER*2  XSECMO
      CHARACTER*30 PRODAT
      CHARACTER*30 WHQDAT

      COMMON /READ/         ITERAT
      COMMON /NUMB/         LEAD
      COMMON /REGEN/         SUMN
      COMMON /DOWNCO/       QDON(IPRAT) , YDON(IPRAT) , IPTS        ,
     1        IDOWN       , A           , B
      COMMON /STG/          DSTAGE(IPHYD)
      COMMON /FLOWS/        NQIN   , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /ERR/          TOL
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /TIB/          DT    , TIME , ITOTH, ITOTM,TOTH,JTIME
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /CONV/         CONVEC
      COMMON /TBLE/         DYDQ(IPRAT)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP), PSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOWP/        HWP(IPSEC,IPSTP),IWP(IPSEC,IPSTP),
     1                      RNFAK(IPSEC,IPSTP),NNWP(IPSEC)
      COMMON /CREEP/        GINITL(IPGAT,IPPRC)
      COMMON /DATA/         BB          , Z1          , Z2          ,
     1        DIA         , RN          , S           , YO
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) ,XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /QLH/          QLI(IPHYD,IPLAT)
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /WAVE/         HWELLE(IPOUT,IPWEL), QWELLE(IPOUT,IPWEL),
     1               ITUN, IWEL, DTWEL, NWEL, KWEL(IPOUT), TOT, STIME
      COMMON /AB/           IAB(IPSEC)
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /PRECIP/       PE(IPPRC),NPREC,DTPREC
      COMMON /JUNC/         JUNNAM(IPJUN)
      COMMON /URBAN/        SPK(IPURB),SPN(IPURB),ARED(IPURB),
     1                      QBASA(IPURB),QBASE(IPURB),QMAX(IPURB)
      COMMON /BILAN/ QKNOTN(IPSEC),SQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      COMMON /QGAT/ AGA(IPGAT),QGAMIN(IPGAT),QGAMAX(IPGAT),
     1              GATTOL(IPGAT),RMUE(IPGAT),GATMIN(IPGAT),
     1              GATWSO(IPGAT)
      COMMON /MODGAT/ GATMOD(IPGAT)
      COMMON /GANGLT/ TINC,TTEST,TPRIN
      COMMON /QF/ QFAK,QBAS
C
      COMMON /QQGWSTAT/     QQMIN(IPLAT),QQMAX(IPLAT)
      REAL QLEAMIN(IPSEC),QLEAMAX(IPSEC)
C
      DIMENSION H8(8),R8(8)
C
C&&&  ==================================================================
C
      INTEGER   NSPE, NSP, NRET, NWERTE, NSTORE, NVERB
      REAL ZETA, DGRENZ
C
      COMMON /VORSCH/  IPRO
      COMMON /RET1/    NSPE(IPSEC,IPSPE),NSP(IPSEC,IPSPE),NRET,NSTORE,
     1                 NWERTE(IPSPE),ZETA(IPSPE),
     2                 DGRENZ(IPSPE),NVERB(IPSEC),INDEX(IPSEC)
      COMMON /RETO/    RETOUT(IPSPE)
C     COMMON /RND/     HR0(IPSEC),HR1(IPSEC),HR2(IPSEC),
C    &                            RN1(IPSEC),RN2(IPSEC)
      COMMON /RND/     HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)
      COMMON /QPARTS/ AHR0(IPSEC),AHR(IPSEC,7),WHR0(IPSEC),WHR(IPSEC,7),
     &                THR0(IPSEC),THR(IPSEC,7),QHR0(IPSEC),QHR(IPSEC,7)
C
      DOUBLE PRECISION XK(IPSEC),YK(IPSEC)
      COMMON /XYKOORD/ XK,YK
C
      COMMON /GEWID/ ID(IPSEC)
C
      CHARACTER*255 GEWSHP,GEWDBF
      COMMON /SHPDBF/ GEWSHP,GEWDBF
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
C-----------------------------------------------------------------------
C     VEREINBARUNGEN LINIEN-SHAPE (gegebene Flussachse)
 
C     Felder für Shape (X,Y = Koordinaten; Z = Höhe; M = Maß = 4. Dimension):
C     4D-Box (=Limits)
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX
      DOUBLE PRECISION ZMIN,ZMAX,MMIN,MMAX
C     Zeigersystem auf Punkte und Objekte
      INTEGER*4 ISUMPARTS(250),ISUMPOINTS(250),IPARTS(250)
C     Punktwolke X,Y mit Z und M
      DOUBLE PRECISION XPOINT(200000),YPOINT(200000)
      DOUBLE PRECISION ZPOINT(200000),MPOINT(200000)

C     spezifische Attribute (Gewässer-ID und Startstation)
      INTEGER*4 IGEWID(250),KNOVON(250),KNOBIS(250)
      INTEGER*4 IVON(250),IBIS(250)
      DOUBLE PRECISION SSTART(250)
      DOUBLE PRECISION SACHS(20000),XACHS(20000),YACHS(20000)
      DOUBLE PRECISION DIX,DIY,DIS,XLD,XKU,XKO,YKU,YKO,XKI,YKI,SUD,SOD
C-----------------------------------------------------------------------
C
      REAL      AGAG(IPHYD), QSTADT(IPHYD)
      REAL      SO(IPSEC)
      REAL      DXL(IPJUN)
      LOGICAL IP
      CHARACTER*10 TYPE,TYPE1,TYPE2,TYPE3,TYPE4,TYPE5,TYPE8,TYPE9
      CHARACTER*40  JUNNAM
      CHARACTER*30  DATEI, RETOUT, LINE30
      CHARACTER*80  ZEILE, STARTDAT
      DATA TYPE1/10HKREIS     /,TYPE2/10HRECHTECK  /
      DATA TYPE3/10HTRAPEZ    /,TYPE4/10HPOLYGONZUG/
      DATA TYPE5/10HPOLYGINTER/,TYPE9/10HPOLYSONDER/
      DATA TYPE8/10HPOLYGINTER/
      DATA ICHECK/0/
      REAL HHR(17),RRN(17)
C-----------------------------------------------------------------------
C
      CHARACTER*6 OVFMOD
      COMMON /OVFBIL/ OVFMOD,NOVF(IPSEC),IABOVF(IPSEC),IOVF(IPSEC),
     1                QOFLSN(100),NOVFMX,LIRE(IPSEC),OVFAN(IPSEC),
     2                OVFAUS(IPSEC),IOVFSTAT(IPSEC),QOMAX(IPSEC),
     3                QREGEL(IPSEC),TOVFAN(IPSEC),TOVFAUS(IPSEC)
C                     wird WSP > OVFAN wird QOFLOW() berechnet
C                     wird WSP wieder < OVFAUS wird QOFLOW() nicht berechnet
C
      COMMON /RELAX/  IRELAXFOR, IRELAXBACK, DHZUL, DHRELZUL, DVRELZUL

      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
C-----------------------------------------------------------------------

      DO 3 I=1,IPHYD
      DO 1 J=1,IPLAT
    1 QLI(I,J) = 0.0
      DO 2 J=1,IPGAT
    2 GINITL(J,I) = 0.0
    3 CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC
CCCCC     REQUIRED INPUT VARIABLES:
CCCCC
CCCCC          STIME- STARTUHRZEIT (DEZIMAL)
CCCCC          TOT  - TOTAL LENGHT OF ROUTING TIME
CCCCC          DT   - INTEGRATION TIME INTERVAL USED FOR ROUTING COMPUTA
CCCCC                 (MINUTES)
CCCCC          LEAD - NUMBER OF HYDROGRAPH POINTS WHICH WILL BE INPUT
CCCCC          ITUN - ZEITANGABE BEI DEN GANGLINIEN
CCCCC                    0  - AUSDRUCK DER MINUTEN (SEKUNDEN GERUNDET)
CCCCC                    1  - AUSDRUCK DER SEKUNDEN
CCCCC
CCCCC          NL   - NUMBER OF REACH LENGTHS IN THE SYSTEM
CCCCC         NJUNC - NUMBER OF JUNCTIONS
CCCCC          NQIN - NUMBER OF INFLOW HYDROGRAPHS, NOT COUNTING
CCCCC                 LATERAL INFLOWS
CCCCC        LATINF - NUMBER OF LOCATIONS WHERE LATERAL INFLOW OCCURS
CCCCC        NWEIRS - NUMBER OF WEIRS
CCCCC        NGATES - NUMBER OF GATES
CCCCC
CCCCC          LIST - PRINT CONTROL
CCCCC                 0 - ALL DIAGNOSTIC MESSAGES AND RESULTS AT EVERY T
CCCCC                     PERIOD ARE PRINTED
CCCCC                 1 - THE RESULTS FOR EVERY TIME PERIOD ARE PRINTED
CCCCC                 2-N - RESULTS FROM EVERY N TIME PERIODS ARE PRINTE
CCCCC          NWEL - ANZAHL DER AUSZUDRUCKENDEN GANGLINIEN (H UND Q)
CCCCC         DTWEL - AUSDRUCKFREQUENZ DER GANGLINIEN IN MINUTEN
CCCCC         IDOWN - INDICATES METHOD OF SPECIFYING DOWNSTREAM BOUNDARY
CCCCC                 1 - RATING CURVE EQUATION USED; SPECIFY A AND B
CCCCC                     IN Q = A*Y**B
CCCCC                 2 - RATING CURVE TABLE USED; SPECIFY DISCHARGES AN
CCCCC                     CORRESPONDING STAGES
CCCCC                 3 - STAGE HYDROGRAPH USED; SPECIFY STAGES
CCCCC                 4 - DISCHARGE HYDROGRAPH USED; SPECIFY DISCHARGES
CCCCC           TOL - MAXIMUM ALLOWABLE FROUDE NUMBER
CCCCC        CONVEC - MINIMUM DEPTH FOR USING CONVECTIVE TERMS
CCCCC
CCCCC       KWEL(I) - NUMMERN DER KNOTEN,FUER DIE GANGLINIEN AUSGEDRUCKT
CCCCC
CCCCC         JPRTP - TAPE FROM WHICH INFLOWS MAY BE READ
CCCCC        QUP(I) - INFLOW HYDROGRAPH ORDINATES FOR UPSTREAM BOUNDARY
CCCCC        NUP(I) - SECTION NUMBERS FOR INFLOW HYDROGRAPHS
CCCCC      NXJ(J,1) - FIRST SECTION UPSTREAM OF JUNCTION ON BRANCH 1
CCCCC         (J,2) - UPSTREAM SECTION OF BRANCH 1
CCCCC         (J,3) - SAME AS 1 & 2 FOR BRANCH 2
CCCCC         (J,4) -
CCCCC         (J,5) - DOWNSTREAM SECTION OF JUNCTION
CCCCC         (J,6) - SECTIONS FOR BRANCH 3;
CCCCC         (J,7) - SET TO ZERO IF ONLY 2 BRANCHES ARE NEEDED
CCCCC        DXL(J) - STATION OF THE JUNCTION
CCCCC      GAM(J,3) - LOSS COEFFICIENTS FOR THE JUNCTION
CCCCC         L1(I) - SECTION NUMBERS FOR LATERAL INFLOW
CCCCC    CLOSS(2,I) - REDUKTIONS-FAKTOR FUER NIEDERSCHLAEGE
CCCCC           QLI - LATERAL INFLOW HYDROGRAPH;
CCCCC      IGATE(I) - SECTIONS WHERE GATES ARE LOCATED
CCCCC          AGAG - GATE OPENINGS SPECIFIED FOR EACH TIME PERIOD (FEET
CCCCC         ITYPE - 1=CIRCLE,2=RECTANGULAR,3=TRAPEZOID,4=POLYGON
CCCCC         ITYPE - 5=POLYGON INTERPOLIERT
CCCCC         WIDTH - WIDTH OF THE RECTANGLE OR THE DIAMTR OF THE CIRCLE
CCCCC          HEIT - HEIGHT OF THE RECTANGULAR SECTION.
CCCCC            ZO - INVERT ELEVATION
CCCCC            XL - DISTANCE FROM UPSTREAM BOUNDARY
CCCCC            ZS - WEIR SILL HEIGHT FOR OVERFLOW SECTIONS.
CCCCC         DZERO - INITIAL DEPTH.
CCCCC         QZERO - INITIAL DISCHARGE
CCCCC           CKS - WEIR COEFFICIENT SPECIFIED FOR EACH SECTION
CCCCC           RNI - MANNING N VALUE FOR EACH SECTION
CCCCC           ZTR - SIDE SLOPE OF TRAPEZOIDAL CHANNEL (RIGHT SIDE)
CCCCC           ZTL - SIDE SLOPE OF TRAPEZOIDAL CHANNEL (LEFT SIDE)
CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     EINLESEN DER FLUSSACHSEN AUS GEWSHP UND DER ATTRIBUTE AUS GEWDBF
C
C     Aufruf zum Lesen der Geometrie aus SHPLIN und SHXLIN:
C
      IF (GEWSHP(1:5).NE.'keine') THEN

         CALL READ_SHP (66,GEWSHP,ITYPSHP,XMIN,XMAX,YMIN,YMAX,ZMIN,
     &                  ZMAX,MMIN,MMAX,NRECSHP,ISUMPARTS,ISUMPOINTS,
     &                  IPARTS,XPOINT,YPOINT,ZPOINT,MPOINT,NPMAX,
     &                  IMOPT)

C        wichtig für die weitere Programmorganisation sind evtl.:
C        ITYPSHP = Shape-Typ Gewässerachse
C        NRECSHP = Anzahl der Objekte = Anzahl der Achsen
C        IMOPT   = 0/1 (kein M belegt/ M belegt)

C        Aufrufe zum Lesen der Attribute aus DBFLIN
         CALL READ_DBF_INTE (GEWDBF,99,'GEW_ID',6,IGEWID,20,NDBF)
         CALL READ_DBF_DBLE (GEWDBF,99,'SSTART',6,SSTART,20,NDBF)
         CALL READ_DBF_INTE (GEWDBF,99,'KNOVON',6,KNOVON,10,NDBF)
         CALL READ_DBF_INTE (GEWDBF,99,'KNOBIS',6,KNOBIS,10,NDBF)

      END IF

C
      N11 = 11
C
C     EINLESEN DER SIMULATIONSDATEN, NETZDATEN UND KENNGROESSEN

      READ (N5,*) STIME,TOTH,DT,LEAD,TINC,ITUN,IPRO
      READ (N5,*) NL,NJUNC,NQIN,LATINF,NWEIRS,NGATES,NRET,NSTORE
      READ (N5,*) LIST,NWEL,DTWEL,IDOWN,TOL,CONVEC
      READ (N5,*) (KWELE(I),I=1,NWEL)

      IF (NL.LT.0) THEN
         NXDAT = -NL + 1
         NL = NXDAT - 1
      ELSE
         NXDAT=0
      END IF

      LLIST = LIST
      LIST = ABS(LIST)
      ITOTD = INT(TOTH/24.)
      TOT = TOTH * 60.
      IF (TOT.LE.(TINC*LEAD)) GO TO 5
      WRITE(N6,4) TOT,TINC,LEAD
   4  FORMAT(//' SIMULATIONSDAUER IST GROESSER ALS LAENGE DER EINGEGEBEN
     1EN GANGLINIEN  TOTH*60 =',F7.2,' > TINC*LEAD =',F5.1,'*',I3)
      STOP 5
   5  CALL  ZEIT(TOT,ITOTH,ITOTM,ITOTS)
C
C      NX = TOTAL NUMBER OF SECTIONS
C
      NX = NL + 1
C
C      0.5 IS ADDED TO AVOID TRUNCATION ERROR
C
      NTINC = IFIX(REAL((TOT/DT) + .5))
      NT = NTINC + 1
C     INCRE = NTINC/(LEAD - 1)
C     TINC = TOT/(LEAD-1)
      INCRE = IFIX (REAL(TINC/DT))
      CRE = INCRE * DT
      IF(ABS(CRE-TINC).LT.0.0001) GO TO 7
      WRITE(N6,6)
  6   FORMAT(//1X,'ZEITSCHRITT VON INPUTGANGLINIE UND SIMULATIONSZEITSCH
     &RITT NICHT KOMPATIBEL   --->  STOP')
      STOP 5
 7    CONTINUE
      STIME = STIME * 60.
      CALL ZEIT(STIME,ITH,ITM,ITS)
      IF (ITS.GT.30) ITM = ITM + 1
C
C      NOTE: LEAD SHOULD BE SET SO THAT INCRE IS AN EVEN NUMBER
C      HERE THE TOTAL TIME LENGTH IS COMPUTED
C
      WRITE (N6,802)
      WRITE (N6,549)
      IF(ITOTD.GT.0) GO TO 8
      WRITE (N6,550) ITH,ITM,ITOTH,ITOTM,DT,LEAD,TINC,INCRE
      GO TO 9
   8  WRITE (N6,551) ITH,ITM,ITOTD,ITOTH,ITOTM,DT,LEAD,TINC,INCRE
   9  WRITE (N6,802)
      WRITE (N6,560)
      WRITE (N6,570) NL,NX,NJUNC,NQIN,LATINF,NWEIRS,NGATES,NRET,NSTORE
      WRITE (N6,802)
      WRITE (N6,580)
      WRITE (N6,590) LIST,NWEL,DTWEL,IDOWN,TOL,CONVEC
      WRITE (N6,802)
      WRITE (N6,803)
      CONVEC = SIENG(CONVEC,1)
C
C-----------------------------------------------------------------
C ABFANGEN VON EINGABEN, DIE DIE AKTUELLE PROGRAMMDIMENSIONIERUNG
C UEBERSCHREITEN
C-----------------------------------------------------------------
C
      IF (IPHYD.GE.LEAD) GO TO 11
      WRITE(N6,10) LEAD,IPHYD
   10 FORMAT(///1X,'ZUVIELE GANGLINIENSTUETZSTELLEN   LEAD =',I5,5X,
     1'MOEGLICH SIND IPHYD =',I5)
      STOP 10
   11 IF (IPSEC.GE.NX) GO TO 13
      WRITE(N6,12) NX,IPSEC
   12 FORMAT(///1X,'ZUVIELE GEWAESSERABSCHNITTE    NX =',I5,5X,
     1'MOEGLICH SIND IPSEC =',I5)
      STOP 12
   13 IF (IPJUN.GE.NJUNC) GO TO 15
      WRITE(N6,14) NJUNC,IPJUN
   14 FORMAT(///1X,'ZUVIELE VERBINDUNGSKNOTEN   NJUNC =',I5,5X,
     1'MOEGLICH SIND IPJUN =',I5)
      STOP 14
   15 IF (IPHYD.GE.LEAD) GO TO 17
      WRITE(N6,16) NQIN, IPINF
   16 FORMAT(///1X,'ZUVIELE ZUFLUSSGANGLINIEN   NQIN =',I5,5X,
     1'MOEGLICH SIND IPINF =',I5)
      STOP 16
   17 IF (IPLAT.GE.LATINF) GO TO 19
      WRITE(N6,18) LATINF,IPLAT
   18 FORMAT(///1X,'ZUVIELE SEITLICHE ZUFLUESSE  LATINF =',I5,5X,
     1'MOEGLICH SIND IPLAT =',I5)
      STOP 18
   19 IF (IPWEI.GE.NWEIRS) GO TO 21
      WRITE(N6,20) NWEIRS,IPWEI
   20 FORMAT(///1X,'ZUVIELE WEHRE      NWEIRS =',I5,5X,
     1'MOEGLICH SIND IPWEI =',I5)
      STOP 20
   21 IF (IPGAT.GE.NGATES) GO TO 23
      WRITE(N6,22) NGATES,IPGAT
   22 FORMAT(///1X,'ZUVIELE STEUERBARE DURCHLAESSE   NGATES =',I5,5X,
     1'MOEGLICH SIND IPGAT =',I5)
      STOP 22
   23 IF (IPOUT.GE.NWEL) GO TO 25
      WRITE(N6,24) NWEL,IPOUT
   24 FORMAT(///1X,'ZUVIELE AUSZUDRUCKENDE GANGLINIEN  NWEL =',I5,5X,
     1'MOEGLICH SIND IPOUT =',I5)
      STOP 24
   25 IWEL = INT(DTWEL/DT)
      WCHK = IWEL*DT
      IF(ABS(WCHK-DTWEL).LT.1E-6) GO TO 27
      WRITE (N6,26) DT,DTWEL
   26 FORMAT (///1X,'ZEITSCHRITTE FUER SIMULATION UND FUER GANGLINIENAUS
     1DRUCK PASSEN NICHT ZUSAMMEN'/10X,'DT =',F5.2,5X,
     2'DTWEL =',F5.2)
      STOP 26
   27 IWCHK = INT(TOT/DTWEL+1)
      IF (IPWEL.GT.IWCHK) GO TO 29
      WRITE (N6,28) IWCHK,IPWEL
   28 FORMAT(///1X,'ZU VIELE AUSZUDRUCKENDE GANGLINIENPUNKTE   IWCHK =',
     1  I5,5X,'MAXIMAL MOEGLICH  IPWEL =',I5//1X,'AENDERN ENTWEDER DTWEL
     2 (GROESSER MACHEN) ODER PARAMETER IPWEL HOCHSETZEN')
      STOP 28
   29 CONTINUE
C
C&&&  =================================================================

      IF (IPSEC.GE.NRET) GOTO 31
      WRITE (N6,30)   NRET,IPSEC
   30 FORMAT(///2X,' ZU VIELE RETENTIONSKNOTEN NRET =',I5,5X,
     1' MOEGLICH SIND IPSEC =',I5)
      STOP 30
   31 CONTINUE
      IF (IPSEC.GE.NSTORE)   GOTO 33
      WRITE (N6,32)   NSTORE,IPSEC
   32 FORMAT (///2X,' ZU VIELE UEBERFLUTUNGSFLAECHEN  NSTORE =',I5,5X,
     1' MOEGLICH SIND IPSEC =',I5)
      STOP 32
   33 CONTINUE

C&&&  =================================================================
C
C hier WELLEN mit Prozentwerten (3 Klassen) für saisonabhängige Verkrautung
C
      IF (KSTMOD.EQ.'KSTIME') THEN
         DO I = 1,3
            READ(N5,901) DATEI
            WRITE (N6,*) 'DATEI =',DATEI
            CALL WELLE(DATEI,LEAD,FAKTKS(1,I),TINC)
         END DO
         DO IT = 1,LEAD
C?          WRITE (N6,*) (('J = ',J,' FAKTKS = ',FAKTKS(IT,J)),J = 1,3)
            WRITE (N6,*) ('J = ',J,' FAKTKS = ',FAKTKS(IT,J),  J = 1,3)
         END DO
      END IF
C
C NIEDERSCHLAGSDATEN EINLESEN
C
      SUMN = 0.0
      DO 40 J=1,IPPRC
   40 PE(J) = 0.
      READ(N5,901) DATEI
      IF (DATEI.EQ.'NORAIN') THEN
         JPREC = 0
         SUMN = 0.0
         GOTO 41
      END IF
  901 FORMAT(A)
      CALL RAIN(DATEI,ZEILE,JPREC)
   41 NPREC = JPREC
      WRITE(N11,*)  JPREC,DT
      IF (JPREC .EQ. 0) GOTO 60
      WRITE(N11,44) (PE(IH),IH=1,JPREC)
      WRITE(N6,901) ZEILE
      WRITE(N6,902)  JPREC,DT
  902 FORMAT(/2X,'N = ',I6,' DT = ',F5.1/)
      WRITE(N6,44) (PE(IH),IH=1,JPREC)
      DO 43 I = 1,JPREC
   43 SUMN = SUMN+PE(I)
      WRITE (N6,1387) SUMN
 1387 FORMAT (1X//,'GESAMTNIEDERSCHLAGSMENGE  ',F10.2,' MM'/)
   44 FORMAT(12F6.2)
      GO TO 60

C     EINLESEN DER ZUFLUSSWELLEN ANDEN EINTRITTSKNOTEN
C
   60 READ (N5,780) (NUPE(J),J = 1,NQIN)
C
C
      DO 120 I = 1,NQIN
         READ (N5,'(A,3F10.0)') DATEI,QFAK,QSTAT,QBAS
         CALL WELLE(DATEI,LEAD,QUP(1,I),TINC)
  120 CONTINUE
  130 CONTINUE
      WRITE (N6,640)
      WRITE (N6,800) (NUPE(J),J = 1,NQIN)
      WRITE (N6,801)
      DO 140 IT = 1,LEAD
      IF (IPRO.EQ.1) GO TO 134
         WRITE (N6,810) (QUP(IT,J),J = 1,NQIN)
  134    CONTINUE
          DO 135 J=1,NQIN
          QUP(IT,J)=SIENG(QUP(IT,J),3)
  135     CONTINUE
  140 CONTINUE
      WRITE (N6,802)
      IF (NJUNC.EQ.0) GO TO 180
C
C      READ AND PRINT OUT JUNCTION DATA
C
      WRITE (N6,820)
      DO 160 J = 1,NJUNC
      READ (N5,789) JUNNAM(J)
  789 FORMAT(A40)
      READ (N5,790) (NXJE(J,K),K = 1,7),DXL(J),(GAM(J,L),L = 1,3)
      WRITE (N6,830) J,DXL(J),NXJE(J,5),(GAM(J,L),L = 1,3)
      DXL(J) = SIENG(DXL(J),1)
  160 CONTINUE
      IF (IPRO.EQ.2) GO TO 171
      IF (IPRO.EQ.1) GO TO 172
      WRITE (N6,802)
      GO TO 172
  171 WRITE (N6,803)
  172 CONTINUE
C
C
C      DOWNSTREAM BOUNDARY CONDITIONS ARE READ
C
  180 GO TO (190,200,230,250), IDOWN
C
C      RATING EQUATION
C           ZSILL IS HEIGHT OF DOWNSTREAM WEIR IF ONE EXISTS
C           FLOW IS MAXIMUM DISCHARGE ALLOWED
C
  190 READ (N5,680) WB,WMUE,ZSILL,FLOW
C
C   URSPRUENGLICH WURDEN STATT WEHRBREITE "WB" UND ABFL.KOEFF "WMUE"
C   "A" UND "B" FUER DIE GL. " Q = A * Y ** B " EINGELESEN; DABEI IST
C   A = 2/3 * MUE * SQRT(2*GRAV) * WB      ("GRAV" UND "WB" IN "FEET")
C   B = 1.5 FUER WEHR
C
      A = 2./3. * WMUE * SQRT(2.*GRAV) * SIENG(WB,1)
      AA= 2./3. * WMUE * SQRT(2.*9.81) * WB
      B = 1.5
      WRITE(N6,849)
      WRITE (N6,850) AA,B,WB,WMUE
      IF (FLOW.EQ.0.) FLOW = 1.E7
      IF (ZSILL.GT.0.) WRITE (N6,870) ZSILL
      IF (FLOW.NE.1.E7) WRITE (N6,880) FLOW
      ZSILL = SIENG(ZSILL,1)
      FLOW  = SIENG(FLOW,3)
      GO TO 270
C
C      RATING TABLE - STAGE VS DISCHARGE
C
  200 READ (N5,670) IPTS
      IF (IPTS.LE.IPRAT) GO TO 205
      WRITE (N6,201) IPTS,IPRAT
  201 FORMAT(///1X,'ZU VIELE PUNKTE FUER DIE SCHLUESSELKURVE   IPTS =',
     1  I5,5X,'MAXIMAL MOEGLICH  IPRAT =',I5)
      STOP 201
  205 READ (N5,*) (QDON(I),YDON(I),I = 1,IPTS)
      IF (YDON(1).LE.0.) GO TO 222
      WRITE(N6,849)
      WRITE (N6,890)
      DO 210 I = 1,IPTS
      IF (IPRO.EQ.1) GO TO 209
      WRITE (N6,900) QDON(I),YDON(I)
  209 YDON(I) = SIENG(YDON(I),1)
  210 QDON(I) = SIENG(QDON(I),3)
      IPTS = IPTS - 1
      DO 220 I = 1,IPTS
  220 DYDQ(I) = (YDON(I + 1) - YDON(I))/(QDON(I + 1) - QDON(I))
      GO TO 270
  222 DO 223 I=1,IPTS
      YDON(I) = SIENG(YDON(I),1)
  223 QDON(I) = SIENG(QDON(I),3)
      GO TO 270
C
C      STAGE HYDROGRAPH
C
C 230 READ(N5,901) DATEI
  230 READ (N5,'(A,3F10.0)') DATEI,QFAK,QSTAT,QBAS
      CALL WELLE(DATEI,LEAD,DSTAGE,TINC)
      WRITE(N6,849)
      WRITE (N6,910)
      DO 240 I = 1,LEAD
      IF (IPRO.EQ.1) GO TO 240
      WRITE (N6,920) DSTAGE(I)
  240 DSTAGE(I) = SIENG(DSTAGE(I),1)
      GO TO 270
C
C      DISCHARGE HYDROGRAPH
C
C 250 READ (N5,'(A,2X,F8.0)') DATEI,QFAK
  250 READ (N5,'(A,3F10.0)') DATEI,QFAK,QSTAT,QBAS
      CALL WELLE(DATEI,LEAD,CDOWN,TINC)
      WRITE(N6,849)
      WRITE (N6,930)
      DO 260 I = 1,LEAD
      IF (IPRO.EQ.1) GO TO 260
      WRITE (N6,920) CDOWN(I)
  260 CDOWN(I) = SIENG(CDOWN(I),3)
  270 CONTINUE
C
C      READ LATERAL INFLOW DATA
C
      JGW=0
      IF (LATINF.EQ.0) GO TO 340
      IF (LATINF.EQ.-1) LATINF=0
      JA = 1
  290 READ(N5,*) NAQ
      IF (NAQ.LE.0) GOTO 291
      IF (NAQ.GT.1000) THEN
      READ(N5,901) DATEI
      CALL GWELLEN(DATEI,LEAD,TINC,JGW)
      NAQ=NAQ-1000
      JA=JGW+1
      LATINF=LATINF+JGW
      IF(IPLAT.GE.LATINF) GOTO 9291
      WRITE(N6,18) LATINF,IPLAT
      STOP 'LATINF > IPLAT !'
 9291 CONTINUE
      END IF
      IF (NAQ.GT.IPAQU) THEN
         WRITE (N6,'(T2,A,I5)') '  NAQ = ',NAQ
         WRITE (N6,'(T2,A,I5)') 'IPAQU = ',IPAQU
         STOP ' NAQ > IPAQU !'
      END IF
      READ(N5,605) (TRANS(J),POR(J),BETA(J),J=1,NAQ)
  291 WRITE (N6,614)
      DO 300 J = JA,LATINF
         READ (N5,600) L1E(J),LATCOM(J),DGW(1,J),DGW(2,J),IML,IMR,
     1                GHA(1,J),GHA(2,J),GS(1,J),GS(2,J),CLOSS(1,J),
     2                CLOSS(2,J),XGW(J)
         IF (IML.EQ.0) IML=1
         IF (IMR.EQ.0) IMR=1
         IAQUI(1,J) = IML
         IAQUI(2,J) = IMR
         IF(XGW(J).EQ.0.) XGW(J)=DGW(2,J)
      IF (IPRO.EQ.1) GO TO 299
         WRITE (N6,615) L1E(J),DGW(1,J),DGW(2,J),BETA(IMR),TRANS(IML),
     1                  TRANS(IMR),POR(IML),POR(IMR),GHA(1,J),GHA(2,J),
     2                  GS(1,J),GS(2,J),CLOSS(1,J),CLOSS(2,J)
  299    CONTINUE
C
         GOTO (297,300,298) LATCOM(J)+1
         GOTO 300
C 297    READ (N5,'(A,2X,F8.0)') DATEI,QFAK
  297    READ (N5,'(A,3F10.0)') DATEI,QFAK,QSTAT,QBAS
         CALL WELLE(DATEI,LEAD,QSTADT,TINC)
         DO 1297 IJ=1,LEAD
            QLI(IJ,J)=QLI(IJ,J)+QSTADT(IJ)
            SQG(1,J)=SQG(1,J)+QSTADT(IJ)*TINC*60.
 1297    CONTINUE
         SQG(1,J)=SQG(1,J)-QSTADT(IJ-1)*TINC*60.
         GOTO 300
  298    JU=J-JA+1
         IF (JU.LT.1.OR.JU.GT.IPURB) THEN
            WRITE(N6,'(T2,A,I5)') 'LATINF = ',LATINF
            WRITE(N6,'(T2,A,I5)') '     J = ',J
            WRITE(N6,'(T2,A,I5)') '    JA = ',JA
            WRITE(N6,'(T2,A,I5)') '    JU = ',JU
            WRITE(N6,'(T2,A,I5)') ' IPURB = ',IPURB
            STOP ' JU > IPURB !'
         END IF
         READ (N5,903) SPK(JU),SPN(JU),ARED(JU),
     &   QBASA(JU),QBASE(JU),QMAX(JU)
  903    FORMAT (6F10.3)
         CALL SURFAC(JU,LEAD,NPREC,QSTADT,DT,TINC,L1E(J))
         DO 1298 IJ=1,LEAD
            QLI(IJ,J)=QLI(IJ,J)+QSTADT(IJ)
            SQG(3,J)=SQG(3,J)+QSTADT(IJ)*TINC*60.
 1298    CONTINUE
         SQG(3,J)=SQG(3,J)-QSTADT(IJ-1)*TINC*60.
  300 CONTINUE
      IF(IPRO.NE.2) GO TO 311
      WRITE (N6,803)
  311 WRITE (N6,700)
      WRITE (N6,620)
      DO 330 J = 1,LATINF
         JU=J-JA+1
         IF (LATCOM(J).EQ.1) GO TO 321
         IF (IPRO.EQ.1) GO TO 312
         IF(LATCOM(J).EQ.3) WRITE(N6,628) L1E(J)
         IF(LATCOM(J).EQ.0) WRITE(N6,626) L1E(J)
         IF(LATCOM(J).EQ.2) WRITE(N6,627) L1E(J),SPK(JU),SPN(JU),
     &   ARED(JU),QBASA(JU),QBASE(JU),QMAX(JU)
         WRITE (N6,625) (QLI(I,J),I = 1,LEAD)
  312    CONTINUE
           DO 315 I=1,LEAD
  315      QLI(I,J) = SIENG(QLI(I,J),3)
         GO TO 330
  321    WRITE (N6,611) L1E(J)
  330 CONTINUE
      WRITE (N6,802)
  340 CONTINUE
C
C      READ IN WEIR DATA
C
      IF (NWEIRS.EQ.0) GO TO 344
      IF (IPRO.NE.2) GO TO 342
      WRITE(N6,803)
  342 WRITE(N6,715)

      OPEN (70,FILE='WEIRS_READ.DAT')

      DO 343 KWEI = 1,NWEIRS
        READ(N5,629) IWEIRE(KWEI),HW(KWEI),BW(KWEI),WCO(KWEI),
     &               WNEIL(KWEI),WNEIR(KWEI),IRAUS(KWEI),V2CO(KWEI),
     &               CCFAK(KWEI),WHQDAT(KWEI)

        WRITE(70,*)  IWEIRE(KWEI),HW(KWEI),BW(KWEI),WCO(KWEI),
     &               WNEIL(KWEI),WNEIR(KWEI),IRAUS(KWEI),V2CO(KWEI),
     &               CCFAK(KWEI),WHQDAT(KWEI)
      
        DO ISTP=1,IPSTP
          WH(KWEI,ISTP)=0.0
          WQ(KWEI,ISTP)=0.0
        END DO

        IF (WHQDAT(KWEI)(1:3).EQ.'H-Q') THEN
          OPEN (99,FILE=WHQDAT(KWEI))
          READ (99,'(A)') LINE30
          DO ISTP=1,IPSTP
            READ (99,*,END=345) WH(KWEI,ISTP),WQ(KWEI,ISTP)
            WH(KWEI,ISTP)=SIENG(WH(KWEI,ISTP),1)
            WQ(KWEI,ISTP)=SIENG(WQ(KWEI,ISTP),3)
          END DO
  345     CLOSE (99,STATUS='KEEP')
        END IF
      
        IF (CCFAK(KWEI).EQ.0.0) CCFAK(KWEI)=1.0
C       WENN IRAUS(KWEI)=0 ==> RAUSSCHMISS DES WEHRS NICHT ZULŽSSIG!
        IF (WNEIR(KWEI).GT.0.0) THEN
          WNEI(KWEI)=0.5*(WNEIL(KWEI)+WNEIR(KWEI))
        ELSE
          WNEI(KWEI)=WNEIL(KWEI)
        END IF
        IF (IPRO.EQ.1) GO TO 1342
        WRITE (N6,716) KWEI,IWEIRE(KWEI),HW(KWEI),BW(KWEI),WCO(KWEI)
 1342 CONTINUE
      HW(KWEI) = SIENG(HW(KWEI),1)
      BW(KWEI) = SIENG(BW(KWEI),1)
  343 CONTINUE

      CLOSE (70,STATUS='KEEP')
   
      WRITE(N6,802)
C
C      READ IN GATE DATA
C
  344 IF (NGATES.EQ.0) GO TO 390
      DO 360 KNOCK = 1,NGATES
C      READ(N5,*)IGATEE(KNOCK),AGA
       READ(N5,361)GATMOD(KNOCK),IGATEE(KNOCK),AGA(KNOCK),QGAMIN(KNOCK),
     & QGAMAX(KNOCK),GATTOL(KNOCK),RMUE(KNOCK),GATMIN(KNOCK),
     & GATWSO(KNOCK)
       IF (QGAMIN(KNOCK).EQ.0.0.AND.QGAMAX(KNOCK).EQ.0.0) THEN
C         PRINT '(T2,A,I5)','KEINE Q-LIMITS FšR GATE ',IGATEE(KNOCK)
          QGAMIN(KNOCK)=-100000.
          QGAMAX(KNOCK)=100000.
       END IF
       IF (QGAMIN(KNOCK).EQ.0.0) QGAMIN(KNOCK)=0.010
       IF (QGAMAX(KNOCK).EQ.0.0) QGAMAX(KNOCK)=-0.010
       IF (GATTOL(KNOCK).EQ.0.0) GATTOL(KNOCK)=0.05
       IF (GATMIN(KNOCK).EQ.0.0) GATMIN(KNOCK)=0.01
       IF (RMUE(KNOCK).EQ.0.0) RMUE(KNOCK)=0.75
       AGA(KNOCK)=SIENG(AGA(KNOCK),1)
       QGAMIN(KNOCK)=SIENG(QGAMIN(KNOCK),3)
       QGAMAX(KNOCK)=SIENG(QGAMAX(KNOCK),3)
       GATWSO(KNOCK)=SIENG(GATWSO(KNOCK),1)
C 361  FORMAT(A1,4X,I5,6F10.0)
  361  FORMAT(A1,4X,I5,7F10.0)
           DO ITIME = 1,LEAD
C             VORBELEGUNG VON AGAG
              AGAG(ITIME) = - AGA(KNOCK)
           END DO
       IF(AGA(KNOCK).GE.0.) THEN
C        READ (N5,'(A)') DATEI
         READ (N5,'(A,3F10.0)') DATEI,QFAK,QSTAT,QBAS
         CALL WELLE(DATEI,LEAD,AGAG(1),TINC)
         ENDIF
           DO 346 ITIME = 1,LEAD
  346      AGAG(ITIME) = SIENG(AGAG(ITIME),1)
C
C     THE VECTOR GINITL IS INITIAL TRAJECTORY FOR GATES SPATIALLY.
C
         DO 350 ITIME = 1,NT
            GINITL(KNOCK,ITIME) = Y(ITIME,AGAG)
  350    CONTINUE
  360 CONTINUE
  370 CONTINUE
      IF (IPRO.NE.2) GO TO 373
      WRITE(N6,803)
  373 WRITE (N6,730)
      DO 380 KNOCK = 1,NGATES
         WRITE (N6,740) IGATEE(KNOCK)
           DO 374 ITIME=1,NT
  374      GINITL(KNOCK,ITIME) = ENGSI(GINITL(KNOCK,ITIME),1)
         WRITE (N6,720) (GINITL(KNOCK,ITIME),ITIME = 1,NT)
           DO 375 ITIME=1,NT
  375      GINITL(KNOCK,ITIME) = SIENG(GINITL(KNOCK,ITIME),1)
  380 CONTINUE
      WRITE (N6,802)
  390 CONTINUE
C
C      DT IS SET TO SECONDS
C
C
      DT = DT * 60.
C
C     READ CHANNEL GEOMETRY.
C     SLO IS USED IF THE SAME SLOPE APPLIES TO THE WHOLE SYSTEM.
C     ELEVATIONS FOR ALL BUT DOWNSTREAM SECTION WILL BE COMPUTED
C     BY THE PROGRAM.   -SET TO ZERO IF ELEVATIONS ARE INPUT TO PROGRAM
C
      READ (N5,'(F10.0,8X,A2,10X,A30)') SLO,XSECMO,PRODAT
      IF (XSECMO.EQ.'VK'.OR.XSECMO.EQ.'VF'.OR.XSECMO.EQ.'RK'.OR.XSECMO.E
     &Q.'RF') THEN
         PRINT *
         PRINT '(T2,A,A)','Default-Berechnungsmodus XSECMO = ',XSECMO
         PRINT *
      ELSE
      PRINT *
      PRINT'(T2,A)','Default-Berechnungsmodus fr Querschnittsparameter'
      PRINT'(T2,A)','in HYD-Datei nicht definiert !'
      PRINT *
      PRINT'(T2,A)','Moegliche Eingaben in Zeile 22 auf Pos. 19-20:'
      PRINT *
      PRINT'(T2,A)','"VK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitflaechiger Abfluss erst wenn '
      PRINT'(T2,A)','       Wasserspiegel hoeher als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitflaechiger Abfluss erst wenn '
      PRINT'(T2,A)','       Wasserspiegel hoeher als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wird auf Bereich zwischen '
      PRINT'(T2,A)','       linkem und rechtem Hochpunkt reduziert !'
      PRINT *
      PRINT'(T2,A)','"VF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitflaechiger Abfluss von Anfang an !'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluss von Anfang an !'
      PRINT'(T2,A)','       Querprofil wird auf Bereich zwischen '
      PRINT'(T2,A)','       linkem und rechtem Hochpunkt reduziert !'
      PRINT *
      STOP 'Vielen Dank fr Ihre Aufmerksamkeit !'
      END IF

      ISEC = 0
      IAB4A=0
       
      IF (NXDAT.GT.0) THEN
         READ(N5,'(A80)') STARTDAT
         OPEN (46,FILE=STARTDAT,STATUS='OLD')
         READ(46,'(A)') LINE30
         WRITE(7,'(A)') LINE30
      END IF
       
      NOVFMX=0

C
      PRINT '(T2,A,I5)','  JWG = ',JGW
C
      IF (JGW.GT.0) THEN
        OPEN (23,FILE='QLEASTAT.dat',STATUS='UNKNOWN')
        WRITE(23,'(A)') '   "I"  "IAB"  "QLEAMIN"  "QLEAMAX"'
      END IF 
C

      DO 400 I = 1,NX

         QLEAMIN(I)=0.0
         QLEAMAX(I)=0.0

         OVFAN(I)  = 0.0
         OVFAUS(I) = 0.0
         QOMAX(I)  = 0.0
         QREGEL(I) = 0.0

         KRAUT(I) = 0

C
C     THESE VARIABLES WILL BE SET EQUAL TO THE PREVIOUS VALUE IF A
C     ZERO IS READ
C
         IF (NXDAT.GT.0) THEN
           IF (OVFMOD.EQ.'OVFBIL') THEN
            IF (KSTMOD.EQ.'KSTIME') THEN
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),KRAUT(I),DZERO(I),HZERO,
     2                 QZERO(I),ZS(I),CKM,LIRE(I),NOVF(I),IABOVF(I),
     3                 HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
            ELSE
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2                 CKM,LIRE(I),NOVF(I),IABOVF(I),HR0(I),
     3                 (HR(I,N),RNV(I,N),N=1,7),SF(I)
            END IF
            IF (NOVF(I).GT.NOVFMX) NOVFMX=NOVF(I)
           ELSE IF (OVFMOD.EQ.'OVFREG') THEN
            IF (KSTMOD.EQ.'KSTIME') THEN
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),KRAUT(I),DZERO(I),HZERO,
     2                 QZERO(I),ZS(I),CKM,LIRE(I),NOVF(I),IABOVF(I),
     3                 OVFAN(I),OVFAUS(I),QOMAX(I),QREGEL(I),TOVFAN(I),
     4                 TOVFAUS(I),HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
            ELSE
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2                 CKM,LIRE(I),NOVF(I),IABOVF(I),OVFAN(I),OVFAUS(I),
     3                 QOMAX(I),QREGEL(I),TOVFAN(I),TOVFAUS(I),
     4                 HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
            END IF
            IF (NOVF(I).GT.NOVFMX) NOVFMX=NOVF(I)
           ELSE
            IF (KSTMOD.EQ.'KSTIME') THEN
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),KRAUT(I),DZERO(I),HZERO,
     2                 QZERO(I),ZS(I),CKM,HR0(I),
     3                 (HR(I,N),RNV(I,N),N=1,7),SF(I)
            ELSE
             READ(46,*)IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2                 CKM,HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
            END IF
           END IF
           DZERO(I)=HZERO-ZO(I)
         ELSE
           READ (N5,685) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                 XL(I),ZO(I),RNI(I),DZERO(I),QZERO(I),ZS(I),CKM,
     2                 HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
         END IF

C
         IF (JGW.GT.0) THEN
           DO JL=1,JGW
             IF (L1E(JL).EQ.IAB(I)) THEN
                QLEAMIN(I)=QQMIN(JL)
                QLEAMAX(I)=QQMAX(JL)
             END IF
           END DO
           WRITE (23,'(I5,2X,I5,2(1X,F10.3))')
     &           I,IAB(I),QLEAMIN(I),QLEAMAX(I)    
         END IF
C
C
C        NACH STEIGENDEM WASSERSTAND SORTIEREN
C
C        HILFSFELDER H8 UND R8 BELEGEN
C
         H8(1)=HR0(I)
         R8(1)=RNI(I)
         DO 405 N8=2,8
            H8(N8)=HR(I,N8-1)
            R8(N8)=RNV(I,N8-1)
  405    CONTINUE
C
C        SORTIEREN
C
  419    DO 406 N8=1,7
            IF (H8(N8).GT.H8(N8+1).AND.H8(N8+1).GT.0.0) THEN
               HH8=H8(N8)
               RR8=R8(N8)
               H8(N8)=H8(N8+1)
               R8(N8)=R8(N8+1)
               H8(N8+1)=HH8
               R8(N8+1)=RR8
            END IF
  406    CONTINUE
C
         DO 408 N8=1,7
            IF (H8(N8).GT.H8(N8+1).AND.H8(N8+1).GT.0.0) GOTO 419
  408    CONTINUE
C
C        HILFSFELDER H8 UND R8 WIEDER AUSLESEN
C
         HR0(I)=H8(1)
         RNI(I)=R8(1)
         DO 407 N8=2,8
            HR(I,N8-1)=H8(N8)
            RNV(I,N8-1)=R8(N8)
  407    CONTINUE
C
C
         INEU=I
         IABNEU=IAB(I)
         DO 4070 IN=1,INEU-1
            IF (IABNEU.EQ.IAB(IN)) THEN
               PRINT '(T2,A,I5,A)','KNOTEN ',IABNEU,' DOPPELT !'
               STOP 'BITTE NEUE KNOTENNUMMER VERGEBEN !'
            END IF
 4070    CONTINUE
C
C
         WRITE(N6,685) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1               XL(I),ZO(I),RNI(I),DZERO(I),QZERO(I),ZS(I),CKM,
     2               HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
C    2               HR0(I),HR1(I),RN1(I),HR2(I),RN2(I)
C
C        IF (I.GT.1) THEN
C           DXX=XL(I-1)-XL(I)
C           IF (DXX.GT.100) THEN
C              PRINT '(T2,A,I5,A)','HALTUNG ',IAB(I-1),' > 100 M !'
C              WRITE (N6,'(T2,A,I5,A)') 'HALTUNG ',IAB(I-1),' > 100 M !'
C              DXX=0.0
C           END IF
C        END IF
C
         RNIBAK(I)=RNI(I)
C
         IF (ITYPE(I).EQ.1) THEN
            RNI1=RNI(I)
            D1=WIDTH(I)
            CALL PRANTL(RNI1,D1)
            RNI(I)=RNI1
         ENDIF
C
Calt     IF (ITYPE(I).EQ.4.OR.ITYPE(I).EQ.5) ISEC = ISEC + 1
         IF (ITYPE(I).GT.3) ISEC = ISEC + 1
C
         IF (IAB4A.EQ.0.AND.ITYPE(I).EQ.4) IAB4A=IAB(I)
C
         CKS(I)   = 2./3. * CKM * SQRT(2.*GRAV)

         OVFAN(I)  = SIENG (OVFAN(I),1)
         OVFAUS(I) = SIENG (OVFAUS(I),1)
         QOMAX(I)  = SIENG (QOMAX(I),3) 
         QREGEL(I) = SIENG (QREGEL(I),3) 

         WIDTH(I) = SIENG (WIDTH(I),1)
         HEIT (I) = SIENG (HEIT (I),1)
         XL   (I) = SIENG (XL   (I),1)
         ZS   (I) = SIENG (ZS   (I),1)
         ZO   (I) = SIENG (ZO   (I),1)
         DZERO(I) = SIENG (DZERO(I),1)
         QZERO(I) = SIENG (QZERO(I),3)
         HR0  (I) = SIENG (HR0  (I),1)
C        HR1  (I) = SIENG (HR1  (I),1)
C        HR2  (I) = SIENG (HR2  (I),1)
         HR(I,1) = SIENG (HR(I,1),1)
         HR(I,2) = SIENG (HR(I,2),1)
         HR(I,3) = SIENG (HR(I,3),1)
         HR(I,4) = SIENG (HR(I,4),1)
         HR(I,5) = SIENG (HR(I,5),1)
         HR(I,6) = SIENG (HR(I,6),1)
         HR(I,7) = SIENG (HR(I,7),1)
         IF (I.EQ.1) GO TO 400
         IF (WIDTH(I).EQ.0.) WIDTH(I) = WIDTH(I - 1)
         IF (HEIT(I).EQ.0.) HEIT(I) = HEIT(I - 1)
C   BEI TRAPEZQUERSCNITT WIRD UEBERLAUFKANTE GLEICH HOEHE GESETZT
          IF (ITYPE(I).NE.3) GO TO 399
          IF (ZS(I).GT.0. .AND. ZS(I).LT.HEIT(I))  GO TO 399
          ZS(I) = HEIT(I)
  399    IF (ZS(I).EQ.0.) ZS(I) = ZS(I - 1)
         IF (DZERO(I).EQ.0.) DZERO(I) = DZERO(I - 1)
         IF (QZERO(I).EQ.0.) QZERO(I) = QZERO(I - 1)
c        IF (CKS(I).EQ.0.) CKS(I) = CKS(I - 1)
         IF (RNI(I).EQ.0.) RNI(I) = RNI(I - 1)
         IF (ZTR(I).EQ.0.) ZTR(I) = ZTR(I - 1)
         IF (ZTL(I).EQ.0.) ZTL(I) = ZTL(I - 1)
  400 CONTINUE

      IF (JGW.GT.0) THEN
         CLOSE (23)
      END IF

      IF (OVFMOD(1:3).EQ.'OVF') THEN

         WRITE(*,*) ' NOVFMX = ',NOVFMX

C?       WRITE (50,9000) ' "ZEIT[h]"', ((' "NOVF',NO,'"'),NO=1,NOVFMX)
         WRITE (50,9000) ' "ZEIT[h]"', (' "NOVF',NO,'"',  NO=1,NOVFMX)

 9000    FORMAT (A,200(A,I3,A))

C        IABOVF(II) ist der Einleitknoten bzw. Zielknoten einer Ausbordung
C          IOVF(II) ist der interne Index I=1,NX des Zielknotens
C          IOVF(II) wird hier ermittelt und belegt
C        IABOVF(II) negativ ==> Einleitung in den Speicher -IABOVF(I)

         DO J=1,IPSEC
           NVERB(J)=0
         END DO
         NRET = 0
         NSTORE = 0

         DO II=1,NX
           IF (IABOVF(II).GT.0) THEN
             DO I=1,NX
C              interner Index des Zielknotens einer Ausbordung wird belegt
               IF (IAB(I).EQ.IABOVF(II)) IOVF(II)=I
             END DO
           ELSE IF (IABOVF(II).LT.0) THEN
C            Gesamtanzahl der Knoten an Speichern wird hochgezählt
             NRET = NRET +1
c            PRINT '(T2,A,I5)','  NRET = ',NRET
C            Knoten IAB(II) hängt am Speicher J = -IABOVF(II)
             J = -IABOVF(II)
c            PRINT '(T2,A,I5)','      J = ',J
C            Anzahl der Speichern wird ermittelt
             IF (J.GT.NSTORE) NSTORE = J
c            PRINT '(T2,A,I5)','NSTORE = ',NSTORE
C            Anzahl der Knoten an Speicher J wird hochgezählt
             NVERB(J) = NVERB(J) + 1
c            PRINT '(T2,A,I5)','  NVERB = ',NVERB(J)
C            aktueller Knoten an Speicher J wird festgehalten in NSPE(K,J)
             K = NVERB(J)
             NSPE(K,J) = IAB(II)
c            PRINT '(T2,A,I5)','   NSPE = ',NSPE(K,J)
C            interner Index wird in NSP(K,J) gespeichert
             NSP(K,J) = II
c            PRINT '(T2,A,I5)','    NSP = ',NSP(K,J)
           END IF
         END DO

         PRINT '(T2,A,I5)','NSTORE = ',NSTORE
         PRINT '(T2,A,I5)','  NRET = ',NRET

      END IF

C     ggf. Gewässerachse suchen
   
      IF (GEWSHP(1:5).NE.'keine') THEN
   
        DO ND=1,NDBF
          IVON(ND)=0
          IBIS(ND)=0
        END DO
   
        KO=IAB(1)
        WRITE(7,*) 'KO = ',KO
        IO=1

        DO I=2,NX
Calt      IF (I.LT.NX.AND.XL(I).GT.XL(I-1)) THEN
          IF (I.LT.NX) THEN
            KU=IAB(I-1)
            WRITE(7,*) 'KU = ',KU
            IU=I-1
            DO ND=1,NDBF
              IF (KU.EQ.KNOVON(ND).AND.KO.EQ.KNOBIS(ND)) THEN
                IVON(ND)=IU
                IBIS(ND)=IO
                DO II=IU,IO,-1
                  ID(II)=IGEWID(ND)
                END DO
                KO=IAB(I)
                WRITE(7,*) 'KO = ',KO
                IO=I
              END IF
            END DO
          ELSE IF (I.EQ.NX) THEN
            KU=IAB(I)
            WRITE(7,*) 'KU = ',KU
            IU=I
            DO ND=1,NDBF
              IF (KU.EQ.KNOVON(ND).AND.KO.EQ.KNOBIS(ND)) THEN
                IVON(ND)=IU
                IBIS(ND)=IO
                DO II=IU,IO,-1
                  ID(II)=IGEWID(ND)
                END DO
              END IF
            END DO
          END IF
        END DO

        WRITE(7,*) 'KO = KBIS = ',KO
        WRITE(7,*) 'KU = KVON = ',KU

        DO ND=1,NDBF
          IF (IVON(ND).GT.0.AND.IBIS(ND).GT.0) THEN
             WRITE(7,*) '  IVON = ',IVON(ND),'    IBIS = ',IBIS(ND)
             WRITE(7,*) 'KNOVON = ',KNOVON(ND),'KNOBIS = ',KNOBIS(ND)

C           2D-Objekt mit NACHS Punkten wird in XACHS und YACHS abgelegt
            CALL GETXYSHP (ND,NACHS,XACHS,YACHS,ISUMPOINTS,ISUMPARTS,
     &                     IPARTS,XPOINT,YPOINT)
            SACHS(1)=SSTART(ND)

C           Schleife über Flussachsen-Shape zur Abwicklungsberechnung
            DO NA=1,NACHS
              IF (NA.GT.1) THEN
                DIX=XACHS(NA)-XACHS(NA-1)
                DIY=YACHS(NA)-YACHS(NA-1)
                DIS=DSQRT(DIX*DIX+DIY*DIY)
                SACHS(NA)=SACHS(NA-1)+DIS
              END IF
C             WRITE(7,*) XACHS(NA),YACHS(NA),SACHS(NA)
            END DO

C           Schleife durch Teilgewässer aus Knotentabelle (rückwärts!)
            DO II=IVON(ND),IBIS(ND),-1
              XLD= DBLE(ENGSI(XL(II),1))
              DO NA=1,NACHS-1
                SUD=SACHS(NA)
                SOD=SACHS(NA+1)
                IF (XLD.GE.SUD.AND.XLD.LT.SOD) THEN
                  XKU=XACHS(NA)
                  XKO=XACHS(NA+1)
                  YKU=YACHS(NA)
                  YKO=YACHS(NA+1)
C                 Interpolation der Koordinaten
                  XKI=XKU+((XKO-XKU)/(SOD-SUD))*(XLD-SUD)
                  YKI=YKU+((YKO-YKU)/(SOD-SUD))*(XLD-SUD)
                END IF
              END DO
              XK(II)=XKI
              YK(II)=YKI
            END DO
          END IF
        END DO

      END IF

C  ---------------------------------------
C
C  INTERNE VERKNUEPFUNGEN
C  ----------------------
C
C     INFLOWS
C
      DO 4002 J=1,NQIN
      DO 4001 KK = 1,NX
      IF(NUPE(J).NE.IAB(KK)) GO TO 4001
      NUP(J) = KK
      GO TO 4002
 4001 CONTINUE
 4002 CONTINUE
C
C     JUNCTIONS
C
      DO 4006 J = 1,NJUNC
       DO 4004 K = 1,7
       DO 4003 KK = 1,NX
       IF(NXJE(J,K).NE.IAB(KK)) GO TO 4003
       NXJ(J,K) = KK
       GO TO 4004
 4003  CONTINUE
 4004  CONTINUE
C
C      CHECK FOR INCORRECT NUMBERING OF JUNCTION SECTIONS
C
         IF (NXJ(J,1).LT.NXJ(J,2)) GO TO 4005
         IF (NXJ(J,3).LT.NXJ(J,4)) GO TO 4005
         IF (NXJ(J,5).LT.NXJ(J,1)) GO TO 4005
         IF (NXJ(J,5).LT.NXJ(J,3)) GO TO 4005
         IF (NXJ(J,6).LT.NXJ(J,7)) GO TO 4005
         GO TO 4006
 4005    ICHECK = 1
         WRITE (N6,840) J
 4006 CONTINUE
C
C     LATERAL INFLOWS
C
           DO 4008  J = 1,LATINF
           DO 4007 KK = 1,NX
           IF(L1E(J).NE.IAB(KK)) GO TO 4007
           L1(J) = KK
           GO TO 4008
 4007      CONTINUE
 4008      CONTINUE
C
C     WEIRS
C
       DO 4010 KWEI = 1,NWEIRS
         IWEIR(KWEI) = 0
         DO 4009 KK=1,NX
         IF (IWEIRE(KWEI).NE.IAB(KK)) GO TO 4009
         IWEIR(KWEI) = KK
         GO TO 4010
 4009    CONTINUE
         IF (IWEIR(KWEI).EQ.0) THEN
           WRITE(*,*) ' Wehr ',IWEIRE(KWEI),' nicht in Knotentabelle!'
           STOP ' Ein Wehr fehlt in der Knotentabelle!'
         END IF
         DO L=1,LATINF
           IF (L1E(L).EQ.IWEIRE(KWEI)) THEN
             WRITE(*,*) ' Wehr ',IWEIRE(KWEI),' mit Zufluss!'
             STOP ' Unzulässiger seitlicher Zufluss an einem Wehr!'
           END IF
         END DO
 4010  CONTINUE
C
C     GATES
C
       DO 4012 KNOCK = 1,NGATES
       DO 4011 KK = 1,NX
       IF(IGATEE(KNOCK).NE.IAB(KK)) GO TO 4011
       IGATE(KNOCK) = KK
       GO TO 4012
 4011  CONTINUE
 4012  CONTINUE
C
C     AUSGEDRUCKTE WELLEN
C
      DO 4014  I=1,NWEL
      DO 4013 KK=1,NX
      IF(KWELE(I).NE.IAB(KK)) GO TO 4013
      KWEL(I)=KK
      GO TO 4014
 4013 CONTINUE
 4014 CONTINUE
C
C&&&  =================================================================
C
C     RETENTIONSKNOTEN EINLESEN
C
      IF (NRET.EQ.0)  GOTO 4060
C
      OPEN (53,FILE='QUEBRET.dat',STATUS='UNKNOWN')
C?    WRITE(53,9000) ' "ZEIT[h]"', ((' "QUEB',NO,'"'),NO=1,NSTORE)
      WRITE(53,9000) ' "ZEIT[h]"', (' "QUEB',NO,'"',  NO=1,NSTORE)
C
      OPEN (54,FILE='WSPRET.dat',STATUS='UNKNOWN')
      WRITE(54,9000) ' "ZEIT[h]"',
C?   &               ((' "ZRET',NO,'"',' "ZRETO',NO,'"'),NO=1,NSTORE)
     &               (' "ZRET',NO,'"',' "ZRETO',NO,'"'  ,NO=1,NSTORE)
C
      WRITE(N6,803)
      WRITE(N6,1001)

      IF (OVFMOD(1:3).EQ.'OVF') THEN

C       neuer Modus Speicher einzulesen
        PRINT '(T2,A,I5)','NSTORE = ',NSTORE
        PRINT '(T2,A,I5)','  NRET = ',NRET

        DO J = 1,NSTORE
          READ (N5,1003)   DATEI
          CALL  RETINP (J,DATEI,RETOUT(J),LEAD,TINC)
        END DO
         
      ELSE

C       alter Modus Speicher einzulesen
        READ (N5,*)  ( NVERB(J), J = 1,NSTORE)
C
        DO J = 1,NSTORE
          READ(N5,*)   ( NSPE(K,J), K = 1,NVERB(J))
          WRITE(N6,1020)  J,NVERB(J)
          WRITE (N6,1002)  ( NSPE(K,J), K = 1,NVERB(J))
C
          READ (N5,1003)   DATEI
          CALL  RETINP (J,DATEI,RETOUT(J),LEAD,TINC)
C
C         Zeiger auf internen Index festlegen
C
          DO K = 1,NVERB(J)
            DO KK = 1,NX
              IF (NSPE(K,J).EQ.IAB(KK)) NSP(K,J) = KK
            END DO
          END DO

        END DO
           
      END IF
C
 4060 CONTINUE
C
 1000 FORMAT (14I5)
 1001 FORMAT (/132('-')///2X,
     1' **********      UEBERFLUTUNGSFLAECHEN        ********** '//)
 1002 FORMAT (10X,10I5)
 1003 FORMAT (A)
 1020 FORMAT (//2X,'===>  UEBERFLUTUNGSFLAECHE NUMMER',I3,2X,
     1'MIT INSGESAMT',I3,' ANGESCHLOSSENEN KNOTEN '/)
C
C&&&  ==================================================================
C
C ---------------------------------------------------------------------
C
C         UNREGELMAESSIGE SECTIONS
C           (BERECHNUNG UND ABSPEICHERUNG DER WERTE FUER FLAECHE,
C            BENETZTEN UMFANG UND WASSERSPIEGELBREITE IN TABELLEN
C            MIT JEWEILS "ISTEP" WERTEN)
C
C===>   XSECT MUSS IMMER AM ENDE DES INPUTS AUFGERUFEN WERDEN!
C
      IF (PRODAT(1:5).NE.'keine') THEN
         CALL XSECT(NX,IPRO,XSECMO,PRODAT,IAB4A)
      END IF
C
      WRITE (N6,'(1X,A,I5)') '  NX = ',NX
      WRITE (N6,'(1X,A,I5)') 'ISEC = ',ISEC
      WRITE (N6,*) '--------------------------------------------------'
      WRITE (N6,*) 'BELEGEN VON A,W UND T AN MANNINGS-N-STUETZSTELLEN '
      WRITE (N6,*) '--------------------------------------------------'
      WRITE (N6,*) 'UND ALTERNATIVE ANFANGS-DATEI MIT RHY-WENDEPUNKTEN'
      WRITE (N6,*) '--------------------------------------------------'
      DO 401 I=1,NX
         HHR0=0.0
C
C        WRITE(N6,'(T2,A,I5)')' I = ',I
C        WRITE(N6,'(T2,A,F10.3)')' HR0(I) = ',ENGSI(HR0(I),1)
         IF (HR0(I).GT.ZO(I)) THEN
            CALL SHAPE (I,HR0(I),AHR0(I),WHR0(I),THR0(I))
         END IF
C
         DO 402 N=1,7
            HHR(N)=0.0
            RRN(N)=0.0
C
C           WRITE(N6,'(T2,A,I5)')' N = ',N
C           WRITE(N6,'(T2,A,F10.3)')' HR(I,N) = ',ENGSI(HR(I,N),1)
            IF (HR(I,N).GT.ZO(I)) THEN
               CALL SHAPE (I,HR(I,N),AHR(I,N),WHR(I,N),THR(I,N))
            END IF
C
  402    CONTINUE
         DO 403 KS=1,ISEC
            IF (LAB(KS).EQ.IAB(I)) THEN
C              UMRECHNUNG IN SI-EINHEITEN
               WIDTH(I) = ENGSI (WIDTH(I),1)
               HEIT (I) = ENGSI (HEIT (I),1)
               XL   (I) = ENGSI (XL   (I),1)
               ZS   (I) = ENGSI (ZS   (I),1)
               ZO   (I) = ENGSI (ZO   (I),1)
               DZERO(I) = ENGSI (DZERO(I),1)
               QZERO(I) = ENGSI (QZERO(I),3)
C              HHR0=KOTE DES ERSTEN RHY-WENDEPUNKTES
C              BIS HIERHIN GILT RNI(I) ALS MANNINGS N
               HHR0=ENGSI(HWP(KS,1),1)
               NRP=0
               DO 404 NWP=2,NNWP(KS)
C                 HHR=KOTEN WEITERER RHY-WENDEPUNKTE
                  IF (NWP.EQ.2) THEN
                     NRP=NRP+1
                     IF (NRP.GT.7) THEN
                        WRITE(N6,*) 'MEHR ALS 7 RHY-WENDEPUNKTE !'
                     END IF
                     HHR(NRP)=ENGSI(HWP(KS,NWP),1)
                     RRN(NRP)=RNI(I)*RNFAK(KS,NWP)
                     NWPV=NWP
                  ELSE IF (NWP.GT.2) THEN
                     IWPDIF=IWP(KS,NWP)-IWP(KS,NWPV)
                     IF (IWPDIF.GT.1) THEN
                        NRP=NRP+1
                        IF (NRP.GT.7) THEN
                           WRITE(N6,*) 'MEHR ALS 7 RHY-WENDEPUNKTE !'
                        END IF
                        HHR(NRP)=ENGSI(HWP(KS,NWP),1)
                        RRN(NRP)=RNI(I)*RNFAK(KS,NWP)
                        NWPV=NWP
                     ELSE IF (IWPDIF.EQ.1) THEN
                        IF (RNFAK(KS,NWPV).LE.RNFAK(KS,NWP)) THEN
                           HHR(NRP)=ENGSI(HWP(KS,NWPV),1)
                           RRN(NRP)=RNI(I)*RNFAK(KS,NWPV)
                           NWPV=NWPV
                        ELSE IF (RNFAK(KS,NWPV).GT.RNFAK(KS,NWP)) THEN
                           HHR(NRP)=ENGSI(HWP(KS,NWP),1)
                           RRN(NRP)=RNI(I)*RNFAK(KS,NWP)
                           NWPV=NWP
                        END IF
                     END IF
                  END IF
  404          CONTINUE
               WRITE (N6,685) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),
     1               ZTL(I),XL(I),ZO(I),RNI(I),DZERO(I),QZERO(I),
     2               ZS(I),CKM,HHR0,(HHR(N),RRN(N),N=1,7),SF(I)
C              UMRECHNUNG IN ENGLISCHE EINHEITEN
               WIDTH(I) = SIENG (WIDTH(I),1)
               HEIT (I) = SIENG (HEIT (I),1)
               XL   (I) = SIENG (XL   (I),1)
               ZS   (I) = SIENG (ZS   (I),1)
               ZO   (I) = SIENG (ZO   (I),1)
               DZERO(I) = SIENG (DZERO(I),1)
               QZERO(I) = SIENG (QZERO(I),3)
            END IF
  403    CONTINUE
  401 CONTINUE
      WRITE (N6,*) '----------------------------------------------'
C
C ---------------------------------------------------------------------
C
C
      DO 410 I = 1,NL
C
C      DX IS REACH LENGTH
C
      DX(I) = (XL(I) - XL(I + 1))
      IF(DX(I).EQ.0.) THEN
        WRITE(*,409) IAB(I)
  409   FORMAT(1X,'DX = 0 ==> STATIONIERUNGSFEHLER BEI KNOTEN ',I5)
        STOP 544
      ENDIF
  410 CONTINUE
C
C     CALCULATION OF SLOPE OR BED ELEVATIONS
C
      DO 420 I = 1,NL
         IB = NX - I
         IF (SLO.EQ.0.) SO(I) = (ZO(I) - ZO(I + 1))/DX(I) * 1000.
  420 IF (SLO.GT.0.) ZO(IB) = ZO(IB + 1) + SLO * DX(IB)
      DX(NX) = DX(1)
      IF (SLO.EQ.0.) SO(NX) = SO(NL)
      IF (NJUNC.EQ.0) GO TO 470
      DO 430 K = 1,NJUNC
c        write(*,*) 'Zeile 785  K = ',K
c        write(*,*) '     NXJ(K,6)= ',NXJ(K,6)
         NXJ1 = NXJ(K,1)
         NXJ3 = NXJ(K,3)
         NXJ5 = NXJ(K,5)
         NXJ6 = NXJ(K,6)
         XXL(K) = DXL(K) - XL(NXJ5)
         DX(NXJ1) = XL(NXJ1) - XL(NXJ5)
         IF (XL(NXJ1).LT.XL(NXJ5)) DX(NXJ1) = XL(NXJ1) + XXL(K)
         DX(NXJ3) = XL(NXJ3) + XXL(K)
         IF(SLO.EQ.0.) SO(NXJ1) = (ZO(NXJ1) - ZO(NXJ5))/DX(NXJ1) * 1000.
         IF(SLO.EQ.0.) SO(NXJ3) = (ZO(NXJ3) - ZO(NXJ5))/DX(NXJ3) * 1000.
c        write(*,*) 'Zeile 795  K = ',K,' NXJ6 = ',NXJ6
         IF(NXJ6.LT.1) GO TO 430
c        write(*,*) 'Zeile 797  K = ',K,' NXJ6 = ',NXJ6
         DX(NXJ6) = XL(NXJ6) + XXL(K)
         IF(SLO.EQ.0.) SO(NXJ6) = (ZO(NXJ6) - ZO(NXJ5))/DX(NXJ6) * 1000.
  430 CONTINUE
      IF (SLO.EQ.0.) GO TO 470
      DO 460 K = 1,NJUNC
         LM = NJUNC - K + 1
         NJ3 = NXJ(LM,5)
         ND = NXJ(LM,1)
         NU = NXJ(LM,2)
         ZO(ND) = ZO(NJ3) + SLO * DX(ND)
         NDD = ND - 1
         DO 440 L = NU,NDD
            LB = NDD - L + NU
  440    ZO(LB) = ZO(LB + 1) + SLO * DX(LB)
         ND = NXJ(LM,3)
         NU = NXJ(LM,4)
         ZO(ND) = ZO(NJ3) + SLO * DX(ND)
         NDD = ND - 1
         DO 450 L = NU,NDD
            LB = NDD - L + NU
  450    ZO(LB) = ZO(LB + 1) + SLO * DX(LB)
  460 CONTINUE
  470 CONTINUE
      IF (IDOWN.NE.2) GO TO 500
      IF (YDON(1).GT.0.) GO TO 500
      DIA = 0.
      IF (ITYPE(NX).EQ.1.) DIA = WIDTH(NX)
      RN = RNI(NX)
      S = SLO
      IF (SLO.EQ.0.) S = SO(NL)/1000.
      BB = WIDTH(NX)
      Z1 = ZTL(NX)
      Z2 = ZTR(NX)
      YO = DZERO(NX)
      CALL TRA
      WRITE(N6,849)
      WRITE (N6,890)
      DO 480 I = 1,IPTS
         YDON(I) = YDON(I) + ZO(NX)
           YDON(I) = ENGSI(YDON(I),1)
           QDON(I) = ENGSI(QDON(I),3)
      IF (IPRO.EQ.1) GO TO  479
      WRITE (N6,900) QDON(I),YDON(I)
  479 CONTINUE
           YDON(I) = SIENG(YDON(I),1)
  480      QDON(I) = SIENG(QDON(I),3)
      IPTS = IPTS - 1
      DO 490 I = 1,IPTS
  490 DYDQ(I) = (YDON(I + 1) - YDON(I))/(QDON(I + 1) - QDON(I))
  500 CONTINUE
      WRITE (N6,760)
      DO 510 I = 1,NX
         TYPE = TYPE1
         IF (ITYPE(I).EQ.2) TYPE = TYPE2
         IF (ITYPE(I).EQ.3) TYPE = TYPE3
         IF (ITYPE(I).EQ.4) TYPE = TYPE4
         IF (ITYPE(I).EQ.5) TYPE = TYPE5
         IF (ITYPE(I).EQ.8) TYPE = TYPE8
         IF (ITYPE(I).EQ.9) TYPE = TYPE9
C
C     COMPUTE BED SLOPE AND PRINT-OUT.
C
C     SZERO=SO(I)
         SZERO = SLO * 1000.
         IF (SLO.EQ.0.) SZERO = SO(I)
          WIDTH(I) = ENGSI (WIDTH(I),1)
          HEIT (I) = ENGSI (HEIT (I),1)
          XL   (I) = ENGSI (XL   (I),1)
          ZS   (I) = ENGSI (ZS   (I),1)
          ZO   (I) = ENGSI (ZO   (I),1)
          DZERO(I) = ENGSI (DZERO(I),1)
          QZERO(I) = ENGSI (QZERO(I),3)
          DO 504 IW=1,NWEIRS
          IF (IWEIR(IW).NE.I) GO TO 504
          SZERO = 9999.99
          GO TO 505
  504     CONTINUE
         IF (IPRO.EQ.1) GO TO  509
  505    GO TO (506,506,507,508,508), ITYPE(I)
  506    WRITE (N6,770)IAB(I),TYPE,WIDTH(I),HEIT(I),         XL(I),
     1                  ZO(I),SZERO,RNI(I),DZERO(I),QZERO(I),ZS(I)
         GO TO 509
  507    WRITE (N6,771)IAB(I),TYPE,WIDTH(I),HEIT(I),ZTR(I),ZTL(I),XL(I),
     1                  ZO(I),SZERO,RNI(I),DZERO(I),QZERO(I),ZS(I)
         GO TO 509
  508    WRITE (N6,772)IAB(I),TYPE,    HEIT(I),             XL(I),
     1                  ZO(I),SZERO,RNI(I),DZERO(I),QZERO(I),ZS(I)
  509     WIDTH(I) = SIENG (WIDTH(I),1)
          HEIT (I) = SIENG (HEIT (I),1)
          XL   (I) = SIENG (XL   (I),1)
          ZS   (I) = SIENG (ZS   (I),1)
          ZO   (I) = SIENG (ZO   (I),1)
          DZERO(I) = SIENG (DZERO(I),1)
          QZERO(I) = SIENG (QZERO(I),3)
  510 CONTINUE
C
CCC   CHECK FOR Q'S LESS THAN INITIAL CONDITIONS
CCC    SET EQUAL TO QZERO FOR SMALL DISCHARGES
C
      DO 530 J = 1,NQIN
         IX = NUP(J)
         IP = .TRUE.
         DO 520 L = 1,LEAD
            IF (QUP(L,J).GE.QZERO(IX)) GO TO 520
C           QUP(L,J) = QZERO(IX)
C           IF (IP) WRITE (N6,650) NUPE(J)
            IF (IP) WRITE (N6,655) NUPE(J)
            IP = .FALSE.
  520    CONTINUE
  530 CONTINUE
C
C     CHECK FOR ERROR IN INPUT DATA
C        (DUERFTE HIERHER GAR NICHT KOMMEN, DA VORHER SCHON ABGEFANGEN)
      IF (ICHECK.NE.1) GO TO 545
  543 WRITE (N6,544)
  544 FORMAT (////1X,'ERROR IN INPUT DATA ! ! ')
      STOP 544
  545 RETURN
C
  549 FORMAT (1X,'SIMULATIONSPARAMETER'/1X,20('-'))
  550 FORMAT (5X,'BEGINN DER SIMULATION     ',39X,I2,':',I2,'  UHR',/5X,
     1'GESAMTE SIMULATIONSDAUER             ',27X,I3,':',I2,'  STD',/5X,
     2'ZEITSCHRITT                          ',25X,F8.2,'  MIN'/5X,
     3'ANZAHL VON GANGLINIENSTUETZSTELLEN   ',25X,I8/5X,
     4'ABSTAND DER GANGLINIENSTUETZSTELLEN  ',25X,F8.2,'  MIN'/5X,
     5'ANZAHL DER INTERPOLATIONSINTERVALLE  ',25X,I8)
  551 FORMAT (5X,'BEGINN DER SIMULATION     ',39X,I2,':',I2,'  UHR',/5X,
     1'GESAMTE SIMULATIONSDAUER',31X,I2,' TAG(E)',I3,':',I2,'  STD',/5X,
     2'ZEITSCHRITT                          ',25X,F8.2,'  MIN'/5X,
     3'ANZAHL VON GANGLINIENSTUETZSTELLEN   ',25X,I8/5X,
     4'ABSTAND DER GANGLINIENSTUETZSTELLEN  ',25X,F8.2,'  MIN'/5X,
     5'ANZAHL DER INTERPOLATIONSINTERVALLE  ',25X,I8)
  560 FORMAT (1X,'PARAMETER DES GEWAESSERSYSTEMS'/1X,30('-'))
  570 FORMAT (5X,
     1'ANZAHL DER GEWAESSERABSCHNITTE                       ',9X,I8/5X,
     2'GESAMTZAHL DER KNOTEN                                ',9X,I8/5X,
     3'ANZAHL DER VERBINDUNGSKNOTEN                         ',9X,I8/5X,
     4'ANZAHL DER ZUFLUSSGANGLINIEN                         ',9X,I8/5X,
     5'ANZAHL DER GEWAESSERABSCHNITTE MIT SEITLICHEM ZUFLUSS',9X,I8/5X,
     6'ANZAHL DER WEHRE IM GEWAESSERNETZ                    ',9X,I8/5X,
     7'ANZAHL DER KONTROLLBAUWERKE                          ',9X,I8/5X,
     8'ANZAHL DER KNOTEN MIT RETENTIONSFLAECHEN             ',9X,I8/5X,
     9'ANZAHL DER UEBERFLUTUNGSFLAECHEN                     ',9X,I8)
  580 FORMAT (1X,'STEUERPARAMETER'/1X,15('-'))
  590 FORMAT (5X,
     1'AUSDRUCKFREQUENZ FUER ERGEBNISTABELLEN ',23X,I8/5X,
     3'ANZAHL DER AUSGEDRUCKTEN GANGLINIEN    ',23X,I8/5X,
     5'ZEITSCHRITT FUER DEN GANGLINIENAUSDRUCK',23X,F8.2,'  MIN'/5X,
     7'KENNZAHL FUER DIE UNTERE RANDBEDINGUNG ',23X,I8/5X,
     9'OBERGRENZE FUER DIE FROUDEZAHL         ',23X,F8.2/5X,
     1'MINIMALE WASSERTIEFE FUER DIE VERWENDUNG KONVEKTIVER TERME    ',
     2  F8.2,'  M')
  600 FORMAT (2I4,2F8.0,2I4,4F8.2,F4.2,F4.2,F8.0)
  605 FORMAT (F8.2,F8.2,F8.5)
  610 FORMAT (6X,I5,11X,'KOEFFIZIENT FUER SEITLICHEN ZUFLUSS :',F10.2)
  611 FORMAT (6X,I5,11X,'SEITLICHER ZUFLUSS WIRD UEBER GRUNDWASSERMODELL
     1 BERECHNET')
  614 FORMAT (1H1,//132('-')//
     1          1X,' KNO-  AQUIFERBREITE   DURCH-  TRANSMISSIVITAET  POR
     2OSITAET   ANFANGSHOEHE GW  GELAENDEHOEHE  REDUKTIONSFAKTOR'/
     3          1X,' TEN                LAESSIGKEIT
     4                                           NIEDERSCHLAG  '/
     5          1X,'       LINKS  RECHTS GEWAESSER   LINKS  RECHTS   LIN
     6KS RECHTS   LINKS  RECHTS   LINKS  RECHTS   LINKS  RECHTS'/
     7          1X,'        {m}     {m}    {1/h}    {qm/h}  {qm/h}    {-
     8}    {-}   {m+NN}  {m+NN}  {m+NN}  {m+NN}    {-}     {-}'/,
     9          132('-')//)
  615 FORMAT(I5,F8.0,F8.0,F9.5,F9.2,F9.2,F7.3,F7.3,4F8.2,2F8.0)
  620 FORMAT (/5X,'ABSCHNITT',6X,'GANGLINIEN DER SEITLICHEN ZUFLUESSE
     1{CBM/SEC}'/)
  625 FORMAT (16X,10F10.4)
  626 FORMAT (1X,I10,5X,'GANGLINIENORDINATEN EINGEGEBEN')
  627 FORMAT (1X,I10,5X,'GANGLINIE UEBER SPEICHERKASKADENMODELL ',
     1'BERECHNET'/16X,'PARAMETER:   K =',F8.3,'  N =',F8.3,'  ARED =',
     2F8.3,'  QBASA =',F8.4,'  QBASE =',F8.4,'  QMAX =',F8.4)
  628 FORMAT (1X,I10,5X,'GRUNDWASSERZUFLUSS MIT MODFLOW BERECHNET')
  629 FORMAT (I10,5F10.2,I10,2F10.0,5X,A30)
  630 FORMAT (8F10.0)
  640 FORMAT (/10X,'ZUFLUSSGANGLINIEN {CBM/SEC}'/10X,27('-'))
  650 FORMAT (/' DIE ZUFLUSSGANGLINIE FUER KNOTEN',I4,' WURDE SO MODIFIZ
     1IERT, DASS DER ZUFLUSS MINDESTENS GLEICH DEM BASISABFLUSS IST')
  655 FORMAT (/' ACHTUNG: ZUFLUSSGANGLINIE FšR KNOTEN',I4,' WIRD KLEINER
     1ALS DER VORGEGEBENE BASISABFLUSS !!!')
  660 FORMAT (2F10.3,I10,2F10.3,3I10)
  670 FORMAT (16I5)
  680 FORMAT (8F10.0)
C 685 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.3,F7.3,F7.2,F5.2,F5.3,5F8.3)
  685 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.4,F7.3,F7.2,F5.2,F5.3,
     &        F8.3,7(F8.3,F8.4),F12.8)
  700 FORMAT (/,10X,'SEITLICHE ZUFLUESSE :'/10X,21('-')/)
  710 FORMAT (1X,F9.2,13X,I3,15X,I3,15X,F4.2,16X,F4.2)
  715 FORMAT (///10X,'WEHRE'/10X,5('-')//
     1 5X,'LFD.NR.    KNOTENNUMMER     WEHRHOEHE {M}    WEHRBREITE {M}
     2   UEBERFALLBEIWERT'/)
  716 FORMAT (/7X,I2,10X,I5,10X,F7.2,10X,F8.2,15X,F6.3)
  720 FORMAT((1X,8(F10.4,' M')))
  730 FORMAT (///,10X,'STEUERBARE DURCHLAESSE:'/10X,23('-'))
  740 FORMAT (/1X,'OEFFNUNGSKURVE DES DURCHLASSES BEI KNOTEN',I4/)
  750 FORMAT (3X,I3,9F7.2,2F5.2)
  760 FORMAT (1H1,//132('-')//
     1          1X,' KNO-  PROFIL      BREITE   HOEHE     NEIGUNG   NEIG
     2UNG     STATION     SOHL-        SOHL-    MANNINGS   ANFANGS-   BA
     3SIS UEBERLAUF-'/
     4 1X,         ' TEN                                   RECHTS    LIN
     5KS                  HOEHE      GEFAELLE       N       TIEFE    ABF
     6LUSS  HOEHE   '/
     7 1X,         '                    {M}      {M}
     8          {M}       {M+NN}     {X 1000}                {M}    {CBM
     9/SEC}  {M}'//132('-')//)
  770 FORMAT (1X,I4,3X,A10,F7.2,F9.2,'       ----      ----',
     1        F12.0,F11.2,F13.2,2X,F11.5,F8.2,F10.2,F8.2)
  771 FORMAT (1X,I4,3X,A10,F7.2,F9.2,F11.3,F10.3,
     1        F12.0,F11.2,F13.2,F11.3,F10.2,F10.2,F8.2)
  772 FORMAT (1X,I4,3X,A10,'   ----',F9.2,'       ----      ----',
     1        F12.0,F11.2,F13.2,F11.3,F10.2,F10.2,F8.2)
  780 FORMAT (8I10)
  790 FORMAT (7I5,5X,4F10.3)
  800 FORMAT (/2X,'KNOTEN',8I10)
  801 FORMAT(' ')
  802 FORMAT(/132('-')/)
  803 FORMAT (1H1)
  810 FORMAT (10X,8F10.3)
  820 FORMAT (/10X,'ZUSAMMENFLUESSE (VERBINDUNGSKNOTEN)'/10X,35('-')
     1       //5X,'LFD.NR.',5X,'ENTFERNUNG',5X,'UNTERSTROMIGER',5X,
     2'ENERGIEVERLUSTKOEFFIZIENTEN'/15X,'VOM NAECHSTEN',8X,'KNOTEN'/
     315X,'ZUSAMMENFLUSS'/21X,'{M}')
  830 FORMAT (I9,5X,F11.2,8X,I7,12X,3F8.2)
  840 FORMAT (/1X,'DATENFEHLER BEI DER NUMERIERUNG DER ABSCHNITTE AM KNO
     1TEN',I5/1X,'ABSCHNITTSNUMMERN MUESSEN IN FLIESSRICHTUNG ZUNEHMEN')
  849 FORMAT (///10X,'RANDBEDINGUNG AM GEBIETSAUSLASS'/10X,31('-')/)
  850 FORMAT (///5X,'ALS RANDBEDINGUNG WURDE EINE ANALYTISCHE ABFLUSSKUR
     1VE DER FORM " Q = A * Y ** B " VERWENDET;'/
     2  1X,      'DIE KOEFFIZIENTEN SIND            A =',F10.2/
     3  1X,      '                                  B =',F10.2/
     4  1X,      'WEHRBREITE AM GEBIETSAUSLASS :       ',F10.2,' M'/
     5  1X,      'ABFLUSSKOEFFIZIENT :                 ',F10.2/)
  870 FORMAT (1X,'WEHRHOEHE AM GEBIETSAUSLASS :        ',F9.2,' M'/)
  880 FORMAT (1X,'MAXIMALER ABFLUSS AM GEBIETSAUSLASS :',F9.2,' CBM/SEC'
     1//)
  890 FORMAT (///5X,'ALS RANDBEDINGUNG WURDE EINE EMPIRISCHE ABFLUSSKURV
     1E VERWENDET;'
     2//10X,'ABFLUSS',7X,'WASSERSTAND'/9X,'{CBM/SEC}',10X,'{M}'/)
  900 FORMAT (7X,F9.2,8X,F8.2)
  910 FORMAT (///5X,'ALS RANDBEDINGUNG WURDE EINE WASSERSTANDSGANGLINIE
     1VERWENDET'//8X,'WASSERSTAND'/10X,'{M+NN}'/)
  920 FORMAT (8X,F8.2)
  930 FORMAT (///5X,'ALS RANDBEDINGUNG WURDE EINE ABFLUSSGANGLINIE VERWE
     1NDET'//9X,'ABFLUSS'/8X,'{CBM/SEC}'/)
      END
