      SUBROUTINE WELLE (DATEI,NMAX,Q,DT)

      INCLUDE 'DIMENS.INC'
       
      COMMON /IO/           N5          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /QF/ QFAK,QBAS
      DIMENSION Q(IPHYD),QQ(2*IPHYD),TT(2*IPHYD)
      CHARACTER*30 DATEI,STRING
      CHARACTER*80 ZEILE
      CHARACTER*1 SIGNUM
      CHARACTER*2 TIMMOD
C     DT = Berechnungs-Zeitintervall [min]
C     NMAX  = Anzahl der Berechnungszeitschritte
      SIGNUM=DATEI(1:1)
      TIMMOD=DATEI(29:30)
      IF (TIMMOD.EQ.'  ') TIMMOD='IN'
      IF (SIGNUM.EQ.'-') THEN
         STRING=DATEI(2:28)
         DATEI=STRING
      END IF
      IF (QFAK.EQ.0.0) THEN
         QFAK=1.0
      END IF
C
      IF (QBAS.EQ.0.0) THEN
         QBAS=-10.**38
      END IF
C
      DATEI(29:30)='  '
      OPEN (21,FILE=DATEI,STATUS='OLD')
C
      READ (21,1) ZEILE
      IF (ZEILE(1:5).EQ.'WELLE') THEN
         READ (21,1) ZEILE
      END IF
    1 FORMAT(A)
C
C     JMAX = Anzahl der Werte
C     DTW  = Zeitintervall [h]
C
      READ (21,*) JMAX,DTW
C
      IF (TIMMOD.EQ.'ST') THEN
         IF (JMAX.GT.0) JMAX = 1
         IF (JMAX.LT.0) JMAX =-1
      END IF
C
      IF (JMAX.GT.0) THEN
C
C        EINLESEN EINER WELLE MIT KONSTANTEM ZEITSCHRITT
C
         IF(ABS(DTW).LT.0.0001) DTW=1.0
C        A1 = Zeitintervall [min]
         A1 = DTW*60.
         IF(A1.LT.DT) GOTO 100
         IF (JMAX.GT.NMAX) JMAX=NMAX
         A2 = ANINT(A1/DT)
         IF (ABS(A2-A1/DT).LT.0.0001) GOTO 10
    5 FORMAT(//1X,'UP WELLE ',A30/1X,'ZEITSCHRITT IST NICHT KOMPATIBEL'
     &//)
         STOP 5
C
   10    CONTINUE
         READ (21,*) (QQ(I),I=1,JMAX)
         QQMAX=-10.**38
         QQMIN= 10.**38
         JQMX=999999
         JQMI=999999
         DO 11 J=1,JMAX
            QQ(J)=QQ(J)*QFAK
            IF (QQ(J).GT.QQMAX) THEN
               QQMAX=QQ(J)
               JQMX=J
            END IF
            IF (QQ(J).LT.QQMIN) THEN
               QQMIN=QQ(J)
               JQMI=J
            END IF
   11    CONTINUE
         DO 12 J=1,JMAX
            IF (TIMMOD.EQ.'IM'.AND.J.GE.JQMX) QQ(J)=QQMAX
            IF (TIMMOD.EQ.'MA') QQ(J)=QQMAX
            IF (TIMMOD.EQ.'MI') QQ(J)=QQMIN
            IF (SIGNUM.EQ.'-') QQ(J)=(-1.)*QQ(J)
            IF (QQ(J).LT.QBAS) QQ(J)=QBAS
   12    CONTINUE
         IJ=0
         QQ(JMAX+1)=QQ(JMAX)
         IA=INT(A2)
         DO 15 I=1,JMAX
            DO 14 J=1,IA
               IJ=IJ+1
               Q(IJ)=QQ(I)+REAL(J-1)*(QQ(I+1)-QQ(I))/REAL(IA)
               IF (IJ.GE.NMAX) GOTO 17
  14        CONTINUE
  15     CONTINUE
         DO 16  I=IJ,NMAX-1
            IF(IJ.LT.NMAX) Q(I+1)=QQ(JMAX)
  16     CONTINUE
  17     CONTINUE
C
         CLOSE (21)
C
         QFAK=1.0
         QBAS=-10.**38
         RETURN
C
 100     A2 = ANINT(DT/A1)
         IF (ABS(A2-DT/A1).LT.0.0001) GOTO 110
         STOP 5
 110     CONTINUE
         READ (21,*) (QQ(I),I=1,JMAX)
         QQMAX=-10.**38
         QQMIN= 10.**38
         JQMX=999999
         JQMI=999999
         DO 13 J=1,JMAX
            QQ(J)=QQ(J)*QFAK
            IF (QQ(J).GT.QQMAX) THEN
               QQMAX=QQ(J)
               JQMX=J
            END IF
            IF (QQ(J).LT.QQMIN) THEN
               QQMIN=QQ(J)
               JQMI=J
            END IF
   13    CONTINUE
         DO 18 J=1,JMAX
            IF (TIMMOD.EQ.'IM'.AND.J.GE.JQMX) QQ(J)=QQMAX
            IF (TIMMOD.EQ.'MA') QQ(J)=QQMAX
            IF (TIMMOD.EQ.'MI') QQ(J)=QQMIN
            IF (SIGNUM.EQ.'-') QQ(J)=(-1.)*QQ(J)
            IF (QQ(J).LT.QBAS) QQ(J)=QBAS
   18    CONTINUE
         QQ(JMAX+1)=QQ(JMAX)
         IJ=0
         IA=INT(A2)
         DO 115 I=1,JMAX,IA
            IJ=IJ+1
            Q(IJ)=QQ(I)
            IF (IJ.GE.NMAX) GOTO 117
 115     CONTINUE
         DO 116  I=IJ,NMAX-1
            IF(IJ.LT.NMAX) Q(I+1)=QQ(JMAX)
 116     CONTINUE
 117     CONTINUE
C
         CLOSE(21)
C
         QFAK=1.0
         QBAS=-10.**38
         RETURN
C
      ELSE IF (JMAX.LT.0) THEN
C
C        EINLESEN EINER WELLE MIT VARIABLEM ZEITSCHRITT
         JMAX=JMAX*(-1.)
         DO 20 J=1,JMAX
            READ(21,*) TT(J),QQ(J)
C           UMRECHNUNG VON TT IN MINUTEN
            TT(J)=TT(J)*60.
   20    CONTINUE
         QQMAX=-10.**38
         QQMIN= 10.**38
         DO 19 J=1,JMAX
            QQ(J)=QQ(J)*QFAK
            IF (QQ(J).LT.QBAS) QQ(J)=QBAS
            IF (QQ(J).GT.QQMAX) QQMAX=QQ(J)
            IF (QQ(J).LT.QQMIN) QQMIN=QQ(J)
   19    CONTINUE
         DO 23 J=1,JMAX
            IF (TIMMOD.EQ.'MA') QQ(J)=QQMAX
            IF (TIMMOD.EQ.'MI') QQ(J)=QQMIN
            IF (SIGNUM.EQ.'-') QQ(J)=(-1.)*QQ(J)
            IF (QQ(J).LT.QBAS) QQ(J)=QBAS
   23    CONTINUE
         CLOSE(21)
         T=0
         J=1
         QU=QQ(J)
         QO=QQ(J)
         TU=0.0
         TO=TT(J)
         DO 21 N=1,NMAX
            T=T+DT
            IF (T.GE.TU.AND.T.LT.TO.AND.J.LE.JMAX) THEN
C              RICHTIGES INTERVALL, J <= JMAX
               Q(N)=QU+(QO-QU)/(TO-TU)*(T-TU)
            ELSE IF (T.GE.TU.AND.T.LT.TO.AND.J.GT.JMAX) THEN
C              RICHTIGES INTERVALL, J > JMAX
               Q(N)=QQ(JMAX)
            ELSE IF (T.GE.TU.AND.T.GE.TO.AND.J.LT.JMAX) THEN
C              INTERVALL GEGEBENER FLšSSE WEITERSCHIEBEN, J < JMAX
   22          J=J+1
               QU=QO
               TU=TO
               QO=QQ(J)
               TO=TT(J)
C              PRšFUNG, OB RICHTIGES INTERVALL, GGF. WEITERSCHIEBEN
               IF (T.GE.TU.AND.T.GE.TO.AND.J.LT.JMAX) GOTO 22
               IF (T.GE.TU.AND.T.LT.TO.AND.J.LE.JMAX) THEN
C                 RICHTIGES INTERVALL, J <= JMAX
                  Q(N)=QU+(QO-QU)/(TO-TU)*(T-TU)
               ELSE IF (T.GE.TU.AND.T.LT.TO.AND.J.GT.JMAX) THEN
C                 RICHTIGES INTERVALL, J > JMAX
                  Q(N)=QQ(JMAX)
               ELSE IF (T.GE.TU.AND.T.GE.TO.AND.J.GE.JMAX) THEN
C                 INTERVALL NICHT MEHR SCHIEBEN, DA J >= JMAX
                  Q(N)=QQ(JMAX)
               END IF
            ELSE IF (T.GE.TU.AND.T.GE.TO.AND.J.GE.JMAX) THEN
C              INTERVALL NICHT MEHR SCHIEBEN, DA J >= JMAX
               Q(N)=QQ(JMAX)
            END IF
   21    CONTINUE
         QFAK=1.0
         QBAS=-10.**38
         RETURN
      ELSE
         STOP
      END IF
      END
