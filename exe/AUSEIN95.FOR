      PROGRAM AUSEIN
C-----------------------------------------------------------------------
C     EXPLIZITE BERECHNUNG EINER AUS- BZW. EINLEITUNGSWELLE ZWISCHEN
C     ZWEI GETRENNTEN GEWéSSERSYSTEMEN
C-----------------------------------------------------------------------
      PARAMETER(NDIM=25000)
      CHARACTER*30 KENGRD
      CHARACTER*4 KOMENT
      DIMENSION XK(NDIM),QK(NDIM),ZK(NDIM),DXK(NDIM),DYK(NDIM)
      DIMENSION DELH(1),QOW(1),HOW(1),HUW(1),QAB(1)
      CHARACTER*30 HCHAUS,HCHEIN,QCHLIM,AUSDAT(50)
      CHARACTER*80 TEXT,LEER
      CHARACTER*11 MODUS
      CHARACTER*5  TYPE(5),WEHR(50)
      CHARACTER*5  FORMHA,FORMHE,FORMQL
      CHARACTER*3  EXTAUS,EXTEIN
      CHARACTER*4  KENN,QCHMOD
      CHARACTER*408 ZEILE
      DIMENSION NCMX(5),HOHU(5,50),CC(5,50)
      DIMENSION KAUS(50),KEIN(50),HS(50),B(50),RMUE(50),QMIN(50)
      DIMENSION AUSMIN(50),AUSMAX(50),HMAX(50),HOMIN(50)
      DIMENSION KNO(3000),H(3000),Q(3000)
      DIMENSION HO(50,3000),HU(50,3000),QALT(3000),QO(50,3000)
      DIMENSION ABFL(3000),ABFINT(3000)
      DIMENSION ISPALT(50),HWSP(50)
      DIMENSION RKF(50),DX(50),DW(50),DS(50),WPE(50,3000)
C-----------------------------------------------------------------------
C     HCHAUS   C      HCH-WASSERSTANDSDATEI FöR SYSTEM MIT AUSLEITUNG
C     HCHEIN   C      HCH-WASSERSTANDSDATEI FöR SYSTEM MIT EINLEITUNG
C     QCHLIM   C      QCH-ABFLUSSDATEI FöR SYSTEM MIT AUSLEITUNG
C     AUSDAT   C   A  DATEI FöR AUSGABE DER AUSLEITUNGSWELLE
C     TYPE     C   A  WEHRTYP IN DER KENNLINIENDATEI 'AUSEIN.DAT'
C     WEHR     C   A  WEHRTYP IM EINZELNEN BERECHNUNGSFALL
C     KAUS     I   A  KNOTENNUMMER AUSLEITUNG
C     KEIN     I   A  KNOTENNUMMER EINLEITUNG
C     HS       R   A  SCHWELLENHôHE
C     B        R   A  öBERFALLBREITE
C     RMUE     R   A  öBERFALLBEIWERT
C     HOHU     R   A  VERHéLTNIS öBERFALLHôHE/UNTERWASSERSTAND
C     CC       R   A  BEIWERT FöR UNVOLLKOMMENEN öBERFALL
C     DT       R      HYDRAIN-ZEITSCHRITT IN MINUTEN
C     TMIN     R      START DER GANGLINIENAUSGABE
C     TMAX     R      ENDE DER GANGLINIENAUSGABE
C     KNO      I   A  KNOTENNUMMERN AUS HCH-DATEI
C     H        R   A  WASSERSTéNDE AN KNOTEN AUS HCH-DATEI
C     HO       R 2DA  WASSERSTAND OBERWASSER (SYSTEM MIT AUSLEITUNG)
C     HU       R 2DA  WASSERSTAND UNTERWASSER (SYSTEM MIT EINLEITUNG)
C     Q        R 2DA  AUSLEITUNGWASSERMENGE (m**3/sec)
C-----------------------------------------------------------------------
C
C     EINLESEN DER RöCKSTAUKENNLINIEN FöR VERSCHIEDENE WEHRTYPEN
C
      QUOT=0.0
      C=0.0
      QNEU=0.0
C
      OPEN (11,FILE='AUSEIN.DAT',STATUS='OLD')
      READ (11,*) NTYP
      IF (NTYP.GT.5) STOP 'NTYP > 5 !'
      DO 50 NT=1,NTYP
         READ(11,'(A5)') TYPE(NT)
         WRITE(*,'(A5)') TYPE(NT)
         READ (11,*) NCMX(NT)
         IF (NCMX(NT).GT.50) STOP 'NCMX > 50 !'
         DO 60 NC=1,NCMX(NT)
            READ (11,*) HOHU(NT,NC),CC(NT,NC)
C           WRITE(*,'(T2,2F6.2)') HOHU(NT,NC),CC(NT,NC)
   60    CONTINUE
   50 CONTINUE
      CLOSE(11,STATUS='KEEP')
C
C     EINLESEN DER STEUERDATEI UND ABLAUF DER BERECHNUNGEN
C
      OPEN (11,FILE='AUSEIN95.RUN',STATUS='OLD')
      OPEN (33,FILE='PSIGES.DAT',STATUS='UNKNOWN')
C
      READ (11,'(A11)') MODUS
      READ (11,*) NRUN
C
      DO 77 NR=1,NRUN
C
      READ (11,'(A30,A5)') HCHAUS,FORMHA
      READ (11,'(A30,A5)') HCHEIN,FORMHE
      READ (11,'(A30,A5)') QCHLIM,FORMQL
      QCHMOD=QCHLIM(1:4)
      IF (FORMHA.EQ.'     ') FORMHA='ASCII'
      IF (FORMHE.EQ.'     ') FORMHE='ASCII'
      IF (FORMQL.EQ.'     ') FORMQL='ASCII'
      READ (11,'(3F10.0,10X,2F10.0)') DT,TMIN,TMAX,GEWI,GEWI2
      IF (GEWI.EQ.0.0) GEWI=0.025
      IF (GEWI2.EQ.0.0) GEWI2=0.025
      GEWIBAK=GEWI
      READ (11,*) NANZ
      READ (11,'(A)') LEER
      DO 1 N=1,NANZ
       READ(11,111) WEHR(N),KAUS(N),KEIN(N),HS(N),B(N),RMUE(N),HMAX(N),
     &              HOMIN(N),QMIN(N),AUSMIN(N),AUSMAX(N),AUSDAT(N)
       IF (WEHR(N).EQ.'DARCY') THEN
          RKF(N)=HS(N)
          DX(N)=B(N)
          DW(N)=RMUE(N)
          DS(N)=HMAX(N)
       END IF
C
       IF (WEHR(N).EQ.'KENQD'.OR.WEHR(N).EQ.'KENHH') THEN
          READ(11,'(A)') KENGRD
          CALL LESSUR (KENGRD,KOMENT,XK,QK,ZK,NMX,NMY,DXK,DYK,
     &                XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,NDIM)
          NKMAX=NMX*NMY
          PRINT '(T2,A,A,A)','KENNFELD ',KENGRD,' GELESEN !'
       END IF
C
       PRINT '(T2,A,I5,A)','AUSLEITUNG ',KAUS(N),' GELESEN !'
  111  FORMAT(A5,2I5,2F7.2,2F5.2,4F7.2,1X,A30)
    1 CONTINUE
C
      DO 200 I=1,30
         IF (HCHAUS(I:I).EQ.' '.AND.HCHAUS((I-1):(I-1)).NE.' ') THEN
            EXTAUS=HCHAUS((I-3):(I-1))
            PRINT '(T2,A,A)','EXTAUS = ',EXTAUS
         END IF
  200 CONTINUE
C
      IF (EXTAUS.EQ.'HCH') THEN
C
         IF (FORMHA.EQ.'ASCII') THEN
            OPEN(44,FILE=HCHAUS,STATUS='OLD')
         ELSE IF (FORMHA.EQ.'BINéR') THEN
            OPEN(44,FILE=HCHAUS,STATUS='OLD',FORM='UNFORMATTED')
         END IF
C
         PRINT '(T2,A,A,A)','DATEI ',HCHAUS,' WIRD GELESEN !'
C
         IF (FORMHA.EQ.'ASCII') THEN
            READ(44,'(8X,I4)') JMAX
         ELSE IF (FORMHA.EQ.'BINéR') THEN
            READ(44) JMAX
         END IF
         PRINT '(T2,A,I5)','JMAX = ',JMAX
C
    4    IF (FORMHA.EQ.'ASCII') THEN
            READ(44,'(I5)',END=4444) IT
         ELSE IF (FORMHA.EQ.'BINéR') THEN
            READ(44,END=4444) IT
         END IF
C        PRINT '(T2,A,I5)','  IT = ',IT
C
         IF (FORMHA.EQ.'ASCII') THEN
            READ(44,444,END=4444) (KNO(J),H(J),J=1,JMAX)
  444       FORMAT(5(I4,F8.0,2X))
         ELSE IF (FORMHA.EQ.'BINéR') THEN
            READ(44,END=4444) (KNO(J),H(J),J=1,JMAX)
         END IF
C
         DO 3 N=1,NANZ
            DO 2 J=1,JMAX
               IF (KAUS(N).EQ.KNO(J)) THEN
                  HO(N,IT)=0.5*(H(J)+H(J+1))
C                 PRINT '(T2,A,I5,A)','KNOTEN ',KAUS(N),' GEFUNDEN !'
               END IF
    2       CONTINUE
    3    CONTINUE
C
         GOTO 4
C
 4444    CLOSE(44,STATUS='KEEP')
C
         ITHAUS=IT
C
         PRINT '(T2,A,A,A)','DATEI ',HCHAUS,' FERTIG !'
C
      ELSE IF (EXTAUS.EQ.'DAT'.OR.EXTAUS.EQ.'RET') THEN
         OPEN(44,FILE=HCHAUS,STATUS='OLD')
C
         PRINT '(T2,A,A,A)','DATEI ',HCHAUS,' WIRD GELESEN !'
C
         READ(44,'(A)') ZEILE
C        WRITE(*,'(1X,A)') ZEILE
         I1=5
         I2=8
         ISPMAX=50
         DO 90 ISP=2,ISPMAX
            I1=I1+8
            I2=I2+8
            KENN=ZEILE(I1:I2)
            WRITE(*,'(T2,A,A)') 'KENN = ',KENN
            READ(KENN,'(I4)') NUMMER
            WRITE(*,'(T2,A,I4)') 'NUMMER = ',NUMMER
            DO 100 N=1,NANZ
               IF (KAUS(N).EQ.NUMMER) THEN
                  ISPALT(N)=ISP
                  PRINT '(T2,A,I5,A)','KNOTEN ',KAUS(N),' GEFUNDEN !'
               END IF
  100       CONTINUE
            IF (KENN.EQ.'    ') THEN
               ISPMAX=ISP-1
               PRINT '(T2,A,I2)','ISPMAX = ',ISPMAX
               GOTO 91
            END IF
   90    CONTINUE
   91    CONTINUE
  109    IT=0
  110    IT=IT+1
         READ(44,*,END=7777) TIME,(HWSP(ISP),ISP=2,ISPMAX)
C        WRITE(*,'(1X,I4,3X,F7.2)') IT,TIME
         IF (EXTAUS.EQ.'RET'.AND.TIME.EQ.0.0) GOTO 109
         DO 120 ISP=2,ISPMAX
            DO 130 N=1,NANZ
               IF (ISPALT(N).EQ.ISP) THEN
                  HO(N,IT)=HWSP(ISP)
C                 PRINT '(T2,A,I2,A)','SPALTE ',ISP,' GEFUNDEN !'
               END IF
  130       CONTINUE
  120    CONTINUE
         GOTO 110
C
 7777    CLOSE(44,STATUS='KEEP')
C
         ITHAUS=IT-1
C
         PRINT '(T2,A,A,A)','DATEI ',HCHAUS,' FERTIG !'
         PRINT '(T2,A,I4)','ITHAUS ',ITHAUS
C
      END IF
C
      DO 210 I=1,30
         IF (HCHEIN(I:I).EQ.' '.AND.HCHEIN((I-1):(I-1)).NE.' ') THEN
            EXTEIN=HCHEIN((I-3):(I-1))
            PRINT '(T2,A,A)','EXTEIN = ',EXTEIN
         END IF
  210 CONTINUE
C
      IF (EXTEIN.EQ.'HCH') THEN
C
         IF (FORMHE.EQ.'ASCII') THEN
            OPEN(55,FILE=HCHEIN,STATUS='OLD')
         ELSE IF (FORMHE.EQ.'BINéR') THEN
            OPEN(55,FILE=HCHEIN,STATUS='OLD',FORM='UNFORMATTED')
         END IF
C
         PRINT '(T2,A,A,A)','DATEI ',HCHEIN,' WIRD GELESEN !'
C
         IF (FORMHE.EQ.'ASCII') THEN
            READ(55,'(8X,I4)') JMAX
         ELSE IF (FORMHE.EQ.'BINéR') THEN
            READ(55) JMAX
         END IF
         PRINT '(T2,A,I5)','JMAX = ',JMAX
C
    5    IF (FORMHE.EQ.'ASCII') THEN
            READ(55,'(I5)',END=5555) IT
         ELSE IF (FORMHE.EQ.'BINéR') THEN
            READ(55,END=5555) IT
         END IF
C        PRINT '(T2,A,I5)','  IT = ',IT
C
         IF (FORMHE.EQ.'ASCII') THEN
            READ(55,555,END=5555) (KNO(J),H(J),J=1,JMAX)
  555       FORMAT(5(I4,F8.0,2X))
         ELSE IF (FORMHE.EQ.'BINéR') THEN
            READ(55,END=5555) (KNO(J),H(J),J=1,JMAX)
         END IF
C
         DO 6 N=1,NANZ
            DO 7 J=1,JMAX
               IF (KEIN(N).EQ.KNO(J)) HU(N,IT)=H(J)
    7       CONTINUE
    6    CONTINUE
C
         GOTO 5
C
 5555    CLOSE(55,STATUS='KEEP')
C
         ITHEIN=IT
C
         PRINT '(T2,A,A,A)','DATEI ',HCHEIN,' FERTIG !'
C
      ELSE IF (EXTEIN.EQ.'DAT'.OR.EXTEIN.EQ.'RET') THEN
         OPEN(55,FILE=HCHEIN,STATUS='OLD')
C
         PRINT '(T2,A,A,A)','DATEI ',HCHEIN,' WIRD GELESEN !'
C
         READ(55,'(A)') ZEILE
         WRITE(*,'(A)') ZEILE
         I1=5
         I2=8
         ISPMAX=50
         DO 140 ISP=2,ISPMAX
            I1=I1+8
            I2=I2+8
            KENN=ZEILE(I1:I2)
c           WRITE(*,'(1X,A)') KENN
            READ(KENN,'(I4)') NUMMER
c           WRITE(*,'(1X,I4)') NUMMER
            DO 150 N=1,NANZ
               IF (KEIN(N).EQ.NUMMER) THEN
                  ISPALT(N)=ISP
C                 PRINT '(T2,A,I5,A)','KNOTEN ',KEIN(N),' GEFUNDEN !'
               END IF
  150       CONTINUE
            IF (KENN.EQ.'    ') THEN
               ISPMAX=ISP-1
               PRINT '(T2,A,I2)','ISPMAX = ',ISPMAX
               GOTO 141
            END IF
  140    CONTINUE
  141    CONTINUE
  159    IT=0
  160    IT=IT+1
         READ(55,*,END=8888) TIME,(HWSP(ISP),ISP=2,ISPMAX)
C        WRITE(*,'(1X,I4,3X,F7.2)') IT,TIME
         IF (EXTEIN.EQ.'RET'.AND.TIME.EQ.0.0) GOTO 159
         DO 170 ISP=2,ISPMAX
            DO 180 N=1,NANZ
               IF (ISPALT(N).EQ.ISP) THEN
                  HU(N,IT)=HWSP(ISP)
C                 PRINT '(T2,A,I2,A)','SPALTE ',ISP,' GEFUNDEN !'
               END IF
  180       CONTINUE
  170    CONTINUE
         GOTO 160
C
 8888    CLOSE(55,STATUS='KEEP')
C
         ITHEIN=IT-1
C
         PRINT '(T2,A,A,A)','DATEI ',HCHEIN,' FERTIG !'
         PRINT '(T2,A,I4)','ITHEIN ',ITHEIN
C
      END IF
C
      IF (QCHMOD.NE.'NULL') THEN
         IF (FORMQL.EQ.'ASCII') THEN
            OPEN(66,FILE=QCHLIM,STATUS='OLD')
         ELSE IF (FORMQL.EQ.'BINéR') THEN
            OPEN(66,FILE=QCHLIM,STATUS='OLD',FORM='UNFORMATTED')
         END IF
C
         PRINT '(T2,A,A,A)','DATEI ',QCHLIM,' WIRD GELESEN !'
C
         IF (FORMQL.EQ.'ASCII') THEN
            READ(66,'(8X,I4)') JMAX
         ELSE IF (FORMQL.EQ.'BINéR') THEN
            READ(66) JMAX
         END IF
         PRINT '(T2,A,I5)','JMAX = ',JMAX
C
    8    IF (FORMQL.EQ.'ASCII') THEN
            READ(66,'(I5)',END=6666) IT
         ELSE IF (FORMQL.EQ.'BINéR') THEN
            READ(66,END=6666) IT
         END IF
C        PRINT '(T2,A,I5)','  IT = ',IT
C
         IF (FORMQL.EQ.'ASCII') THEN
            READ(66,666,END=6666) (KNO(J),Q(J),J=1,JMAX)
  666       FORMAT(5(I4,F8.0,2X))
         ELSE IF (FORMQL.EQ.'BINéR') THEN
            READ(66,END=6666) (KNO(J),Q(J),J=1,JMAX)
         END IF
C
         DO 9 N=1,NANZ
            DO 11 J=1,JMAX
               IF (KAUS(N).EQ.KNO(J)) THEN
                  QO(N,IT)=Q(J)
                  IF (WEHR(N).EQ.'DARCY') WPE(N,IT)=Q(J)
               END IF
   11       CONTINUE
    9    CONTINUE
C
         GOTO 8
C
 6666    CLOSE(66,STATUS='KEEP')
C
         ITQLIM=IT
C
         PRINT '(T2,A,A,A)','DATEI ',QCHLIM,' FERTIG !'
C
      END IF
C
      IF (ITHAUS.NE.ITHEIN) THEN
         IF (ITHEIN.GT.ITHAUS) ITHEIN=ITHAUS
         IF (ITHAUS.GT.ITHEIN) ITHAUS=ITHEIN
         PRINT'(T2,2(A,I3))','ITHAUS = ',ITHAUS,'  ITHEIN = ',ITHEIN
      END IF
C
      IF (QCHMOD.NE.'NULL') THEN
         IF (ITQLIM.NE.ITHAUS) THEN
            IF (ITQLIM.GT.ITHAUS) ITQLIM=ITHAUS
            IF (ITHEIN.GT.ITQLIM) ITHEIN=ITQLIM
            IF (ITHAUS.GT.ITQLIM) ITHAUS=ITQLIM
            PRINT'(T2,A,I3)','ITQLIM = ',ITQLIM
            PRINT'(T2,2(A,I3))','ITHAUS = ',ITHAUS,'  ITHEIN = ',ITHEIN
         END IF
      ELSE
         ITQLIM=ITHAUS
      END IF
C
C     BEGINN DER HYDRAULISCHEN BERECHNUNGEN
C
      DTAUS=DT/60.
C
      DO 10 N=1,NANZ
         PRINT*
         PRINT'(T2,A,I4,A,I4)','AUSLEIT. VON ',KAUS(N),' NACH ',KEIN(N)
         PRINT*
         PRINT'(T2,2(A,F8.3),A)','VON ',TMIN,' [h] BIS ',TMAX,' [h]'
         PRINT*
         SUMABF=0.0
         OPEN(22,FILE=AUSDAT(N),STATUS='UNKNOWN')
         READ(22,'(A)') TEXT
         WRITE(33,'(A)') TEXT
         READ(22,*) ITDMAX,DTEIN
         DIFFDT=ABS(DTEIN-DTAUS)
         IF (DIFFDT.GE.0.000001) THEN
               PRINT '(T2,A,F10.6)','DTEIN = ',DTEIN
               PRINT '(T2,A,F10.6)','DTAUS = ',DTAUS
               STOP 'ZEITSCHRITTE NICHT KOMPATIBEL !'
         END IF
         SUMALT=0.0
         IF (ITDMAX.LT.0) ITDEIN=(-1.)*ITDMAX
         IF (ITDMAX.GT.0) ITDEIN=ITDMAX
         PRINT '(T2,A,I5)','ITDEIN = ',ITDEIN
         DO 15 IT=1,ITDEIN
            IF (ITDMAX.GT.0) READ (22,*,END=16) QALT(IT)
            IF (ITDMAX.LT.0) READ (22,*,END=16) DUMMY,QALT(IT)
            TIME=DTAUS*REAL(IT)
            IF (TIME.LT.TMIN) GOTO 15
            IF (TIME.GT.TMAX) GOTO 15
            SUMALT = SUMALT + QALT(IT)
   15    CONTINUE
         PRINT '(T2,A,F10.1)','SUMALT = ',SUMALT
         GOTO 17
   16    STOP 'DATEILéNGE IN GANGLINIE ZU KURZ !'
C
   17    REWIND 22
C
         WRITE(22,'(A)') TEXT
         WRITE(22,'(I5,3X,F10.6)') ITDEIN,DTEIN
C
C        SUCHE DES WEHRTYPS
C
         IF (WEHR(N).NE.'DARCY') THEN
            IF (WEHR(N)(1:3).NE.'KEN') THEN
               IF (WEHR(N)(2:5).NE.'GATE') THEN
                  CALL WERTYP(NTYP,TYPE,WEHR(N),NTP)
               END IF
            END IF
         END IF
C
C        SCHLEIFE öBER ZEITSCHRITTE
C
         ITMIT=0
         DO 20 IT=1,ITDEIN
C
C         VerlÑngerung der Ganglinie
C
          IF (IT.GT.ITHEIN.OR.IT.GT.ITHAUS.OR.IT.GT.ITQLIM) THEN
             IF (IT.LT.(ITDEIN-3)) THEN
C               bisherige Werte aus Gangliniendatei
                ABFL(IT)=QALT(IT)
             ELSE IF (IT.GE.(ITDEIN-3)) THEN
C               letzter berechneter Wert
                ABFL(IT)=ABFL(IT-1)
             END IF
                GOTO 20
          END IF
C
C         AuffÅllung der Ganglinie mit bisherigen Werten:
C
          TIME=REAL(DTEIN*IT)
          IF (TIME.LT.TMIN) THEN
             ABFL(IT)=QALT(IT)
             GOTO 20
          END IF
          IF (TIME.GT.TMAX) THEN
             ABFL(IT)=QALT(IT)
             GOTO 20
          END IF
          ITMIT=ITMIT+1
C
C-----------------------------------------------------------------------
C
C        ERMITTLUNG DER öBERFALLBEIWERTE
C
          IF (WEHR(N).NE.'DARCY'.AND.WEHR(N)(1:3).NE.'KEN'.
     &       AND.WEHR(N)(2:5).NE.'GATE') THEN

            IF (HO(N,IT).LT.HU(N,IT)) THEN
               VORZ=-1.0
               HUE=HU(N,IT)-HS(N)
                DU=HO(N,IT)-HS(N)
              HDIF=HU(N,IT)-HO(N,IT)
            ELSE
               VORZ=1.0
               HUE=HO(N,IT)-HS(N)
                DU=HU(N,IT)-HS(N)
              HDIF=HO(N,IT)-HU(N,IT)
            END IF
            IF (HUE.NE.0.0) THEN
               QUOT=DU/HUE
            END IF
            IF (QUOT.LE.0.0) QUOT = 0.0
            IF (QUOT.GE.1.0) QUOT = 1.0
            IF (HUE.LE.0.0) HUE = 0.0
C
C           SUCHE DES INTERVALLS AUF DER RöCKSTAUKURVE
C
            NCCC=NCMX(NTP)-1
            QUOTMI=HOHU(NTP,1)
            IFLAG=0
            DO 21 NC=1,NCCC
               IF (QUOT.GT.HOHU(NTP,NC).AND.QUOT.LE.HOHU(NTP,NC+1)) THEN
                  Q1=HOHU(NTP,NC)
                  Q2=HOHU(NTP,NC+1)
                  C1=CC(NTP,NC)
                  C2=CC(NTP,NC+1)
                  IFLAG=1
               ELSE IF (QUOT.LE.QUOTMI.AND.QUOT.GE.0.0) THEN
                  IFLAG=1
                  C1=1.0
                  C2=1.0
               END IF
   21       CONTINUE
            IF (IFLAG.NE.1) THEN
               WRITE(*,'(T2,F6.2)') QUOT
               DO 22 NC=1,NCCC
                  WRITE(*,'(T2,2F6.2)') HOHU(NTP,NC),CC(NTP,NC)
   22          CONTINUE
               STOP 'INTERVALL NICHT GEFUNDEN !'
            END IF
            IF (QUOT.GT.QUOTMI) THEN
C              unvollkommener öberfall
               DC=C1-C2
               DQ=Q1-Q2
c              PRINT '(T2,2(A,F6.2))','C1=',C1,'    C2= ',C2
c              PRINT '(T2,2(A,F6.2))','Q1=',Q1,'    Q2= ',Q2
c              PRINT '(T2,2(A,F6.2))','DQ=',DQ,'  QUOT= ',QUOT
               C=C1-DC/DQ*(Q1-QUOT)
            ELSE IF(QUOT.LE.QUOTMI) THEN
C              vollkommener öberfall
               C=1.0
            END IF
C
C           FESTLEGUNG DER MAXIMALEN QUERSCHNITTSFLéCHE
C
            IF (HUE.GT.HMAX(N)) THEN
               AREA=B(N)*HMAX(N)
            ELSE
               AREA=B(N)*HUE
            END IF
C
C           BERECHNUNG DES ABFLUSSES
C
            ABFL(IT)=VORZ*C*2./3.*RMUE(N)*AREA*SQRT(2.*9.81*HUE)
C
C           BERECHNUNG VON PSI-GESAMT
C
            IF (AREA.GT.0.0) THEN
               V=ABFL(IT)/AREA
               IF (V.NE.0.0) THEN
                  PSIGES=HDIF/(V*V)*2.*9.81
                  WRITE(33,'(T2,5F8.2)')ABFL(IT),HO(N,IT),HU(N,IT),
     &            HDIF,PSIGES
               END IF
            END IF

          ELSE IF (WEHR(N)(2:5).EQ.'GATE') THEN
C
C           BERECHNUNG DER GATE-ôFFNUNGWEITE
C
            IF (IT.GT.1.AND.IT.LT.ITDEIN) THEN
               QGATE=(QO(N,IT-1)+QO(N,IT)+QO(N,IT+1))/3.
            ELSE
               QGATE=QO(N,IT)
            END IF
C
            DH=ABS(HO(N,IT)-HU(N,IT))
            QUOTDH=(B(N)*QALT(IT)*SQRT(2*9.81*DH))
            IF (QUOTDH.NE.0.0) THEN
               RMUEAKT = ABS(QGATE)/QUOTDH
               IF (RMUEAKT.GT.RMUE(N)) RMUEAKT=RMUE(N)
            ELSE
               RMUEAKT = RMUE(N)
            END IF
C
            PRINT '(T2,A,F10.3)','  QGATE = ',QGATE
            PRINT '(T2,A,F10.3)','RMUEAKT = ',RMUEAKT
C
            IF (WEHR(N).EQ.'AGATE') THEN
C
             IF (HO(N,IT).GT.HU(N,IT).AND.HOMIN(N).GT.HU(N,IT)) THEN
               DH=HOMIN(N)-HU(N,IT)
               PRINT '(T2,A,F10.3)','AGATE1  DH = ',DH
               ABFL(IT) = QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
             ELSE IF(HO(N,IT).LE.HU(N,IT).AND.HOMIN(N).GT.HU(N,IT))THEN
               DH=HOMIN(N)-HO(N,IT)
               PRINT '(T2,A,F10.3)','AGATE2  DH = ',DH
               ABFL(IT) = -QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
             ELSE IF (HOMIN(N).LE.HU(N,IT)) THEN
               ABFL(IT)=AUSMIN(N)*(-1.)
             END IF
C
            ELSE IF (WEHR(N).EQ.'BGATE') THEN
C
             IF (HO(N,IT).GT.HU(N,IT).AND.HO(N,IT).GT.HOMIN(N)) THEN
               DH=HO(N,IT)-HOMIN(N)
               PRINT '(T2,A,F10.3)','BGATE1  DH = ',DH
               ABFL(IT) = QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
             ELSE IF(HO(N,IT).LE.HU(N,IT).AND.HO(N,IT).GT.HOMIN(N))THEN
               DH=HU(N,IT)-HOMIN(N)
               PRINT '(T2,A,F10.3)','BGATE2  DH = ',DH
               IF (QGATE.GT.0.0) THEN
                  ABFL(IT) = QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
               ELSE
                  ABFL(IT) = -QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
               END IF
             ELSE IF(HO(N,IT).LE.HU(N,IT).AND.HO(N,IT).LE.HOMIN(N))THEN
               DH=HU(N,IT)-HOMIN(N)
               PRINT '(T2,A,F10.3)','BGATE3  DH = ',DH
               IF (DH.LT.0.0) DH=ABS(DH)
               IF (QGATE.GT.0.0) THEN
                  ABFL(IT) = QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
               ELSE
                  ABFL(IT) = -QGATE/(RMUEAKT*B(N)*SQRT(2*9.81*DH))
               END IF
             ELSE IF (HOMIN(N).GE.HO(N,IT)) THEN
               ABFL(IT)=AUSMIN(N)*(-1.)
             END IF
C
            END IF
C
            ABFL(IT) = ABFL(IT)*(-1.)

          ELSE IF (WEHR(N).EQ.'DARCY') THEN
C
C           BERECHNUNG DER SICKERRATE
C

            DH=HO(N,IT)-HU(N,IT)
            IF (QCHMOD.NE.'NULL') THEN
               ABFL(IT)=RKF(N)/DS(N)*DX(N)*WPE(N,IT)*DH
            ELSE IF (QCHMOD.EQ.'NULL') THEN
               ABFL(IT)=RKF(N)/DS(N)*DX(N)*DW(N)*DH
            END IF

          ELSE IF (WEHR(N).EQ.'KENQD') THEN
C
C           BERECHNUNG DES DURCHFLUSSES AUS KENNFELD Q-DH
C
            IF (HO(N,IT).GE.HU(N,IT)) THEN
               DELH(1)=HO(N,IT)-HU(N,IT)
               VORZ=1.
            ELSE IF (HU(N,IT).GT.HO(N,IT)) THEN
               DELH(1)=HU(N,IT)-HO(N,IT)
               VORZ=-1.
            END IF
            QOW(1)=QO(N,IT)
            DMX=HS(N)
            FAKTOR=RMUE(N)

            IF (QCHMOD.NE.'NULL') THEN
               IF (DELH(1).GE.XMIN.AND.DELH(1).LT.XMAX.AND.
     &             QOW(1).GE.YMIN.AND.QOW(1).LT.YMAX) THEN
C                  PRINT '(T2,A)','Interpolation lÑuft !'
                   CALL INDIUP(NKMAX,XK,QK,ZK,DMX,4,1.0,1,DELH,QOW,QAB)
                   ABFL(IT) = QAB(1)*FAKTOR*VORZ
               ELSE
                   IF (VORZ.GT.0.AND.HO(N,IT).GE.HOMIN(N)) THEN
                      PRINT '(T2,A,F10.3)','HO = ',HO(N,IT)
                      PRINT '(T2,A,F10.3)','HU = ',HU(N,IT)
                      PRINT '(T2,A,F10.3)','DH = ',DELH(1)
                      PRINT '(T2,A,F10.3)','QO = ',QOW(1)
                      PRINT '(T2,A)','WERTE NICHT IM QD-KENNFELD!'
                   END IF
                   IF (VORZ.LT.0.AND.HU(N,IT).GE.HOMIN(N)) THEN
                      PRINT '(T2,A,F10.3)','HO = ',HO(N,IT)
                      PRINT '(T2,A,F10.3)','HU = ',HU(N,IT)
                      PRINT '(T2,A,F10.3)','DH = ',DELH(1)
                      PRINT '(T2,A,F10.3)','QO = ',QOW(1)
                      PRINT '(T2,A)','WERTE NICHT IM QD-KENNFELD!'
                   END IF
                   ABFL(IT) = 0.0
               END IF
            ELSE IF (QCHMOD.EQ.'NULL') THEN
               STOP' QCHMOD = NULL ist nicht zulÑssig!'
            END IF

          ELSE IF (WEHR(N).EQ.'KENHH') THEN
C
C           BERECHNUNG DES DURCHFLUSSES AUS KENNFELD H-H
C
            IF (HO(N,IT).GE.HU(N,IT)) THEN
               HOW(1)=HO(N,IT)
               HUW(1)=HU(N,IT)
               VORZ=1.
            ELSE IF (HU(N,IT).GT.HO(N,IT)) THEN
               HOW(1)=HU(N,IT)
               HUW(1)=HO(N,IT)
               VORZ=-1.
            END IF
            DMX=HS(N)
            FAKTOR=RMUE(N)

            IF (HOW(1).GE.XMIN.AND.HOW(1).LT.XMAX.AND.
     &         HUW(1).GE.YMIN.AND.HUW(1).LT.YMAX) THEN
C              PRINT '(T2,A)','Interpolation lÑuft !'
               CALL INDIUP(NKMAX,XK,QK,ZK,DMX,4,1.0,1,HOW,HUW,QAB)
               ABFL(IT) = QAB(1)*FAKTOR*VORZ
            ELSE
               IF (VORZ.GT.0.AND.HO(N,IT).GE.HOMIN(N)) THEN
                  PRINT '(T2,A,F10.3)','HO = ',HO(N,IT)
                  PRINT '(T2,A,F10.3)','HU = ',HU(N,IT)
                  PRINT '(T2,A)','WERTE NICHT IM HH-KENNFELD!'
               END IF
               IF (VORZ.LT.0.AND.HU(N,IT).GE.HOMIN(N)) THEN
                  PRINT '(T2,A,F10.3)','HO = ',HO(N,IT)
                  PRINT '(T2,A,F10.3)','HU = ',HU(N,IT)
                  PRINT '(T2,A)','WERTE NICHT IM HH-KENNFELD!'
               END IF
               ABFL(IT) = 0.0
            END IF
          END IF
C
C
C-----------------------------------------------------------------------
C
          ABFL(IT)=ABFL(IT)*(-1.)
C
C-----------------------------------------------------------------------
C
C
          QTEST=ABFL(IT)*(-1.)
          QABFL=QTEST
          PRINT '(T2,A,F8.4)','QABFL = ',QABFL
          IF (WEHR(N).EQ.'DARCY') QCHMOD='NULL'
          IF (WEHR(N).EQ.'AGATE') QCHMOD='NULL'
C
C         Test gegen AUSMIN(N)
C
          IF (QABFL.LT.AUSMIN(N)) THEN
             GEWIBAK=GEWI
             PRINT '(T2,A,F6.2)','736: QABFL < AUSMIN(N) = ',AUSMIN(N)
             QABFL=AUSMIN(N)
             GEWI=GEWI2
          END IF
C
C         mit GEWI gewogene Mittelung mit eingelesenem Abflu·
C
          ABFL(IT)=QABFL*(-1.)
          QNEU=ABFL(IT)
          ABFL(IT)=GEWI*ABFL(IT)+(1.-GEWI)*QALT(IT)
          GEWI=GEWIBAK
C
          QABFL=ABFL(IT)*(-1.)
C
C         Test gegen AUSMAX(N)
C
          IF (QABFL.GT.AUSMAX(N)) THEN
             PRINT '(T2,A,F6.2)','QABFL > AUSMAX(N) = ',AUSMAX(N)
             QABFL=AUSMAX(N)
          END IF
C
C         Test gegen AUSMAX1 infolge Mindestabflu· QMIN(N)
C
          IF (QCHMOD.NE.'NULL') THEN
             AUSMAX1=QO(N,IT)-QMIN(N)
             IF (QABFL.GT.AUSMAX1) THEN
                PRINT '(T2,A,F6.2)','QABFL > AUSMAX1 = ',AUSMAX1
                QABFL=AUSMAX1
C
C               eingeschlossner Test gegen AUSMIN(N)
C
                IF (QABFL.LT.AUSMIN(N)) THEN
                   PRINT '(T2,A,F6.2)','QABFL < AUSMIN(N) = ',AUSMIN(N)
                   QABFL=AUSMIN(N)
                   VORZ=0.0
                   ABFL(IT)=QABFL*(-1.)
                   QNEU=ABFL(IT)
                   GEWI=GEWIBAK
                   PRINT '(T2,A,F6.2)','GEWI = ',GEWI
                   ABFL(IT)=GEWI*ABFL(IT)+(1.-GEWI)*QALT(IT)
                END IF
             END IF
          END IF
C
          ABFL(IT)=QABFL*(-1.)
C
C-----------------------------------------------------------------------
C
          IF (WEHR(N)(2:5).NE.'GATE') THEN
             IF (VORZ.GE.0.AND.HO(N,IT).LE.HOMIN(N)) ABFL(IT)=0.0
             IF (VORZ.LT.0.AND.HU(N,IT).LT.HOMIN(N)) ABFL(IT)=0.0
          END IF
C
C-----------------------------------------------------------------------
C
          SUMABF=SUMABF+ABFL(IT)
C
          PRINT '(T2,2(A,F8.4),5(A,F8.4))','HO=',HO(N,IT),
     &    ' HU=',HU(N,IT),' QUOT=',QUOT,' C=',C,' QALT=',-QALT(IT),
     &    ' QNEU=',-QNEU,' QMIT=',-ABFL(IT)
C
   20    CONTINUE
C
         IF (ITMIT.GT.0) THEN
            PRINT '(T2,A,F10.1)','SUMABF = ',SUMABF
            PRINT '(T2,A,I5)','ITMIT  = ',ITMIT
            ALTMIT=SUMALT/REAL(ITMIT)
            PRINT '(T2,A,F10.5)','ALTMIT = ',ALTMIT
            ABFMIT=SUMABF/REAL(ITMIT)
            PRINT '(T2,A,F10.5)','NEUMIT = ',ABFMIT
         END IF
C
C        RNULL=AUSMIN(N)*(-1.)
C
         DO 25 IT=1,ITDEIN
            TIME=REAL(DTAUS*IT)
            EPS1=ABS(TIME-TMIN)
            EPS2=ABS(TIME-TMAX)
            IF (EPS1.LE.DTAUS.OR.EPS2.LE.DTAUS) THEN
               PRINT '(T2,A)','öBERGANG WIRD GEGLéTTET ! '
               IF (IT.GT.1) THEN
                  ABFINT(IT)=0.5*(ABFL(IT)+ABFL(IT-1))
               ELSE
                  ABFINT(IT)=ABFL(IT)
               END IF
            ELSE
               ABFINT(IT)=ABFL(IT)
            END IF
C
            IF (IT.LE.ITDEIN) THEN
c
c              Anfangswert nochmal und weitere Werte !!
c              letzter Wert wird weggelassen !!
c
               IF (MODUS.EQ.'STATIONéR') THEN
                  IF (TIME.GE.TMIN.AND.TIME.LE.TMAX) THEN
                     WRITE(22,'(F12.6)') ABFMIT
                  ELSE
c                    PRINT '(T2,A,F7.2,A)','TIME = ',TIME,' ABFLUSS = 0'
c                    WRITE(22,'(F12.6)') RNULL
                     WRITE(22,'(F12.6)') QALT(IT)
                  END IF
               ELSE IF (MODUS.EQ.'INSTATIONéR') THEN
                  IF (TIME.GE.TMIN.AND.TIME.LE.TMAX) THEN
                     IF (IT.GT.1.AND.ABFINT(IT-1).NE.0.0) THEN
                        DIFABF=ABFINT(IT)-ABFINT(IT-1)
                        RELDIF=ABS(DIFABF)/ABS(ABFINT(IT-1))
                        IF (RELDIF.GT.0.50) THEN
                           PRINT '(T2,A)','RELDIF > 0.50!'
                           ABFAKT=0.5*(ABFINT(IT)+ABFINT(IT-1))
                        ELSE
                           ABFAKT=ABFINT(IT)
                        END IF
                     ELSE
                        ABFAKT=ABFINT(IT)
                     END IF
                     WRITE(22,'(F12.6)') ABFAKT
                  ELSE
c                    PRINT '(T2,A,F7.2,A)','TIME = ',TIME,' ABFLUSS = 0'
c                    WRITE(22,'(F12.6)') RNULL
                     WRITE(22,'(F12.6)') QALT(IT)
                  END IF
               END IF
            END IF
   25    CONTINUE
         CLOSE(22,STATUS='KEEP')
   10 CONTINUE
C
   77 CONTINUE
C
      CLOSE(11,STATUS='KEEP')
      CLOSE(33,STATUS='KEEP')
C
C     PRINT '(T2,A)',' PROGRAMM "AUSEIN" FERTIG !'
C
      END
C-----------------------------------------------------------------------
      SUBROUTINE LESSUR (SURNAM,KOMENT,X,Y,Z,NMX,NMY,DX,DY,
     &           XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,NDIM)
C
      IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*30 SURNAM
      CHARACTER*4 KOMENT
      DIMENSION X(*),Y(*),Z(*),DX(*),DY(*)
      DIMENSION ZWERT(2001)
      DOUBLE PRECISION XMIND,XMAXD,YMIND,YMAXD,XX,YY
      DOUBLE PRECISION DXD(2001),DYD(2001)
      DOUBLE PRECISION DXMIN,DXMAX,DYMIN,DYMAX
      PRINT *
      PRINT '(T2,A,A,A)',' DATEI ',SURNAM,' WIRD GELESEN !'
      PRINT *
      OPEN(10,FILE=SURNAM,STATUS='UNKNOWN')
      READ(10,'(A4)') KOMENT
      READ(10,*) NMX,NMY
      DXMIN=999999.
      DYMIN=999999.
      DXMAX=-999999.
      DYMAX=-999999.
      IF (KOMENT.EQ.'DSAA'.OR.KOMENT.EQ.'MAAA') THEN
         READ(10,*) XMIND,XMAXD
         READ(10,*) YMIND,YMAXD
         DO 3 NX=1,NMX
            DXD(NX)=(XMAXD-XMIND)/DBLE(NMX-1)
            DX(NX)=REAL(DXD(NX))
    3    CONTINUE
         DO 4 NY=1,NMY
            DYD(NY)=(YMAXD-YMIND)/DBLE(NMY-1)
            DY(NY)=REAL(DYD(NY))
    4    CONTINUE
      ELSE IF (KOMENT.EQ.'DSFF'.OR.KOMENT.EQ.'MAFF') THEN
         READ(10,*) XMIND,XMAXD
         READ(10,*) (DXD(N),N=1,NMX)
         READ(10,*) YMIND,YMAXD
         READ(10,*) (DYD(N),N=1,NMY)
      END IF
      DO 5 NX=1,NMX
         IF (DXD(NX).GT.DXMAX) DXMAX=DXD(NX)
         IF (DXD(NX).LT.DXMIN) DXMIN=DXD(NX)
         DX(NX)=REAL(DXD(NX))
    5    CONTINUE
      DO 6 NY=1,NMY
         IF (DYD(NY).GT.DYMAX) DYMAX=DYD(NY)
         IF (DYD(NY).LT.DYMIN) DYMIN=DYD(NY)
         DY(NY)=REAL(DYD(NY))
    6    CONTINUE
      READ(10,*) ZMIN,ZMAX
      NMAX=NMX*NMY
      PRINT '(T2,A,I7)','NMX   = ',NMX
      PRINT '(T2,A,I7)','NMY   = ',NMY
      PRINT '(T2,A,I7)','NMAX  = ',NMAX
      PRINT '(T2,2(A,F12.5))','DXMIN = ',DXMIN,'  DXMAX = ',DXMAX
      PRINT '(T2,2(A,F12.5))','DYMIN = ',DYMIN,'  DYMAX = ',DYMAX
      IF (NMAX.GT.NDIM) THEN
         PRINT '(T2,A,I7)','NMAX = ',NMAX
         PRINT '(T2,A,I7)','NDIM = ',NDIM
         STOP 'UP LESSUR: NMAX > NDIM !'
      END IF
C
      IF (KOMENT(1:2).EQ.'DS') YY=YMIND
      IF (KOMENT(1:2).EQ.'MA') YY=YMAXD
      DO 1 NY=1,NMY
         IF (NY.GT.1) THEN
            IF (KOMENT(1:2).EQ.'DS') YY=YY+(DYD(NY-1)+DYD(NY))/2.
            IF (KOMENT(1:2).EQ.'MA') YY=YY-(DYD(NY-1)+DYD(NY))/2.
         END IF
         READ(10,*) (ZWERT(K), K=1,NMX)
         XX=XMIND
         DO 2 NX=1,NMX
            IF (NX.GT.1) THEN
               XX=XX+(DXD(NX-1)+DXD(NX))/2.
            END IF
C           N = (NX-1)*NMY + NY
            IF (KOMENT(1:2).EQ.'DS') N = (NX-1)*NMY + NY
            IF (KOMENT(1:2).EQ.'MA') N = NX*NMY - (NY-1)
            X(N)=REAL(XX)
            Y(N)=REAL(YY)
            Z(N)=ZWERT(NX)
    2    CONTINUE
    1 CONTINUE
      NMAX=N
      PRINT '(T2,A,I7)','NMAX = ',NMAX
      XMIN=REAL(XMIND)
      XMAX=REAL(XMAXD)
      YMIN=REAL(YMIND)
      YMAX=REAL(YMAXD)
      CLOSE (10,STATUS='KEEP')
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE WERTYP(NTYP,TYPE,WEHR,NTP)
C-----------------------------------------------------------------------
C     SUCHE DES WEHRTYPS
C-----------------------------------------------------------------------
      CHARACTER*5  TYPE(*),WEHR
C-----------------------------------------------------------------------
C     TYPE     C   A  WEHRTYP IN DER KENNLINIENDATEI
C     WEHR     C   A  WEHRTYP IM EINZELNEN BERECHNUNGSFALL
C-----------------------------------------------------------------------
         NTP=0
         DO 18 NT=1,NTYP
            IF (TYPE(NT).EQ.WEHR) THEN
               NTP=NT
            END IF
   18    CONTINUE
         IF (NTP.EQ.0) STOP 'WEHRTYP NICHT GEFUNDEN !'
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE INDIUP(NZF,XF,YF,ZF,DMX,MAXP,EXP,NZ,X,Y,Z)
      DIMENSION XF(*),YF(*),ZF(*)
      DIMENSION X(*),Y(*),Z(*)
      DIMENSION DIS(50),IHL(50),GI(50),GEWI(50)
C
C       NZF = Anzahl der gegebenen Punkte mit Z-Wert
C     XF(I) = 1D-Feld der X-Koordinaten mit zu interpolierenden Z-Werten
C     YF(I) = 1D-Feld der Y-Koordinaten mit zu interpolierenden Z-Werten
C     ZF(I) = 1D-Feld der zu interpolierenden Z-Werte
C
C       NZ = Anzahl der Punkte, auf die zu interpolieren ist
C     X(I) = 1D-Feld der X-Koordinaten, auf die zu interpolieren ist
C     Y(I) = 1D-Feld der Y-Koordinaten, auf die zu interpolieren ist
C     Z(I) = 1D-Feld der interpolierten Z-Werte (Ergebnisvektor !)
C
      ZMISS=0.1*10.**32
      ZFLAG=ZMISS/10.
      ZMIN=ZFLAG
      ZMAX=ZFLAG*(-1.)
C
      PRINT '(T2,A)','UNTERPROGRAMM INDIUP ARBEITET !'
C
      PALT=0.0
C
      DO 1 N=1,NZ
C
         PROZ=REAL(N)/REAL(NZ)*100.
         DPRO=PROZ-PALT
         IF (DPRO.GE.5.0) THEN
            PRINT '(T2,A,F4.0,A)','INDIUP STAND: ',PROZ,' %'
            PALT=PROZ
         END IF
C
         Z(N)=0.0
         MXP=MAXP
c        PRINT '(T2,A,I5)','UP INDIUP: MXP = ',MXP
c        PRINT '(T2,A,I5)','UP INDIUP: NZF = ',NZF
c        PRINT '(T2,A)','UP INDIUP: CALL SUCH....'
C
         CALL SUCH(X(N),Y(N),XF,YF,ZF,NZF,ZFLAG,DMX,MXP,DIS,IHL)
C
         IF (MXP.GT.0) THEN
C
C           PRINT '(T2,A,I5)','UP INDIUP: MXP = ',MXP
C
            CALL GEWI1(DIS,MXP,EXP,GI,GEWI)
C
            DO 2 M=1,MXP
               IF (ZF(IHL(M)).GT.ZFLAG) THEN
                  STOP 'UP INDIUP:  ZF(IHL(I)) > ZFLAG'
               END IF
               Z(N)=Z(N)+ZF(IHL(M))*GEWI(M)
    2       CONTINUE
         ELSE
c           PRINT '(T2,2F12.6)',X(N),Y(N)
c           PRINT '(T2,A,I5)','UP INDIUP: MXP = ',MXP
c           PRINT '(T2,A)','ZMISS wird gesetzt !'
            Z(N)=ZMISS
         END IF
         IF (Z(N).LT.ZMIN) ZMIN=Z(N)
         IF (Z(N).GT.ZMAX.AND.Z(N).LT.ZFLAG) ZMAX=Z(N)
         IF (Z(N).GE.ZFLAG) Z(N)=ZMISS
c        PRINT '(T2,2F10.1,F10.3)',X(N),Y(N),Z(N)
    1 CONTINUE
C
      END
C-----------------------------------------------------------------------
      SUBROUTINE SUCH(X,Y,XF,YF,ZF,NF,ZFLAG,DMX,NMX,DIST,IHOL)
C
C     DMX = MAXIMALER SUCHRADIUS
C     NMX = MAXIMALE ANZAHL DER NéCHSTEN PUNKTE
C
      DIMENSION XF(*),YF(*),ZF(*),DIST(*),IHOL(*)
C
C     PRINT '(T2,A,I5)','UP SUCH:  NMX = ',NMX
C
      DO 1 N=1,NMX
         DIST(N)=0.0
         IHOL(N)=0
    1 CONTINUE
C
      J=0
C
C     PRINT '(T2,A,I5)','UP SUCH:   NF = ',NF
C
      DO 9 I=1,NF
C
C       PRINT '(T2,2(A,I5))','UP SUCH:  I = ',I,' von ',NF
C
        DX=X-XF(I)
        DY=Y-YF(I)
        DIS=DX**2+DY**2
        IF (ZF(I).GT.ZFLAG) DIS=2.*DMX*DMX
        IF (DIS.LT.0.00001) THEN
           DIS=0.0
           NMX=1
        ELSE
           DIS=SQRT(DIS)
        END IF
        IF(DIS.GT.DMX) GO TO 9
        IF(J.GT.0) THEN
          IF(J.GE.NMX.AND.DIS.GT.DIST(J)) GO TO 9
        END IF
        J=J+1
        DIST(J)=DIS
C
C       PRINT '(T2,A,F10.1)',' DIST(J) = ',DIST(J)
C
        IHOL(J)=I
        IF(J.EQ.1.AND.NMX.GT.1) GOTO 9
        IF(J.EQ.1.AND.NMX.EQ.1) GOTO 10
C
        CALL DSORT(DIST,IHOL,J)
C
        IF(J.GT.NMX) J=NMX
    9 CONTINUE
      IF(NMX.GT.J) NMX=J
   10 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GEWI1(DIST,NMX,EXP,GI,GEWI)
C
C     GEWI(N) = GEWICHT AUS INVERSER DISTANZ ** EXP
C
      DIMENSION DIST(*),GI(*),GEWI(*)
C
      GSUM=0.0
C
      DO 1 N=1,NMX
         IF (DIST(N).EQ.0.0) THEN
            GI(N)=1.
            GSUM=GSUM+GI(N)
         ELSE
            GI(N)=(1./DIST(N))**EXP
            GSUM=GSUM+GI(N)
         END IF
    1 CONTINUE
C
      DO 2 N=1,NMX
         GEWI(N)=GI(N)/GSUM
    2 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
        SUBROUTINE DSORT(DS,IHOL,N)
C ***********************************************************************
C * DSORT szubrutin : A DS tomb elemeit novekvo sorrendben rendezi      *
C ***********************************************************************
        DIMENSION DS(*),IHOL(*)
        DNEW=DS(N)
        NEW=IHOL(N)
        N1=N-1
        DO 20 I=1,N1
        K=I
        IF(DNEW.LT.DS(I)) GO TO 30
 20     CONTINUE
        RETURN
 30     JK=0
        DO 40 I=K,N1
        J=N1-JK
        JK=JK+1
        DS(J+1)=DS(J)
        IHOL(J+1)=IHOL(J)
 40     CONTINUE
        DS(K)=DNEW
        IHOL(K)=NEW
 50     RETURN
        END
C-----------------------------------------------------------------------

