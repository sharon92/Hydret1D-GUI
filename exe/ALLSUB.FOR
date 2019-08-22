C-----------------------------------------------------------------------
      SUBROUTINE BACJUN (NJ1,NJ2,NJ3,NJ4,I)
C
C      BACKWARD SWEEP THROUGH JUNCTIONS

      INCLUDE 'DIMENS.INC'

      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /CMS/          CM(IPJUN,9)
      COMMON /AB/           IAB(IPSEC)
      KLD1 = 2 * NJ1
      KLD2 = 2 * NJ2
      KLQ1 = KLD1 - 1
      KLQ2 = KLD2 - 1
      KLD3 = 2 * NJ3
      KLQ3 = KLD3 - 1
      XV(KLD1) = CM(I,1) + (CM(I,2) * XV(KLQ3)) - (CM(I,3) * XV(KLD3))
      XV(KLD2) = CM(I,4) + (CM(I,5) * XV(KLQ3)) - (CM(I,6) * XV(KLD3))
      XV(KLQ1) = (COZ(KLQ1) - COM(KLQ1,4) * XV(KLD1))/COM(KLQ1,3)
      XV(KLQ2) = (COZ(KLQ2) - COM(KLQ2,4) * XV(KLD2))/COM(KLQ2,3)
      IF (NJ4.EQ.0) GO TO 100
C
C      THIRD INFLOWING BRANCH
C
      KLD4 = 2 * NJ4
      KLQ4 = KLD4 - 1
      XV(KLD4) = CM(I,7) + (CM(I,8) * XV(KLQ3)) - (CM(I,9) * XV(KLD3))
      XV(KLQ4) = (COZ(KLQ4) - COM(KLQ4,4) * XV(KLD4))/COM(KLQ4,3)
  100 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE BAKGAT (ITIME,NU,ND)
C
C      THIS THE CALLING SUBROUTINE FOR THE BACKWARD SWEEP WHEN GATES
C      AND WEIRS ARE PRESENT

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /AB/           IAB(IPSEC)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH

C   GATES

c     write(n6,901) itime,ngates,nweirs,nu,iab(nu),nd,iab(nd)
c 901 format(1x,'itime= ',i5,'  ngates= ',i3,'  nweirs= ',i3,/,
c    1          '       ',5x,'  nu= ',i5,'/',i5,'  nd= ',i5,'/',i5,/)
      IF (NGATES.EQ.0) GO TO 110
      DO 100 LOT = 1,NGATES
         LOTTRY = NGATES - LOT + 1
         NEXT = IGATE(LOTTRY)
c     write(n6,902) ngates, lot, next,iab(next)
c 902 format(1x,'ngates= ',i3,' lot= ',i5,'  next= ',i5,'/',i5,/)
         IF ((NEXT.GT.ND).OR.(NEXT.LT.NU)) GO TO 100
         IF (NWEIRS.EQ.0) GO TO 95
         DO 90 LOTW = 1,NWEIRS
         LOTTRW = NWEIRS - LOTW + 1
         NEXTW = IWEIR(LOTTRW)
c     write(n6,903) nweirs, lotw, nextw,iab(nextw)
c 903 format(1x,'nweirs= ',i3,' lotw= ',i5,'  nextw= ',i5,'/',i5,/)
         IF ((NEXTW.GT.ND).OR.(NEXTW.LT.NU)) GO TO 90
         IF (NEXTW.LT.NEXT) GOTO 90
c     write(n6,904)
c 904 format(1x,'nextw > next ==> call bakwrd (nextw,nd)')
            CALL BAKWRD (NEXTW,ND)
            ND = NEXTW
c     write(n6,905) nd,iab(nd)
c 905 format(1x,'nd= ',i5,'/',i5)
   90    CONTINUE
c     write(n6,906)
c 906 format(1x,'call bakwrd (next,nd)')
   95       CALL BAKWRD (NEXT,ND)
C
C      SUBROUTINE GATE CHECKS FOR LINEARIZATION ERRORS DUE TO THE GATE
C
c     write(n6,907)
c 907 format(1x,'call gate (next,itime,lottry)')
         CALL GATE (NEXT,ITIME,LOTTRY,'BAKGAT')
         ND = NEXT - 1
         NDA = 2 * ND
         NDB = 2 * NEXT
c     write(n6,908) nd,iab(nd),nda,ndb
c 908 format(1x,'nd= ',i5,'/',i5,'  nda= ',i5,'  ndb= ',i5)
         XV(NDA)   = XV(NDB)
         XV(NDA-1) = XV(NDB-1)
c     write(n6,909) engsi(xv(nda),1),engsi(xv(nda-1),3)
c 909 format(1x,'xv(nda)= ',f7.2,'  xv(nda-1)= ',f7.2)

  100 CONTINUE
C
C   WEHRE

      IF (NWEIRS.EQ.0) GO TO 210
  110 DO 200 LOTW = 1,NWEIRS
         LOTTRW = NWEIRS - LOTW + 1
         NEXTW = IWEIR(LOTTRW)
c     write(n6,910) nweirs, lotw, nextw,iab(nextw)
c 910 format(1x,'DO200 nweirs= ',i3,' lotw= ',i5,' nextw= ',i5,'/',i5,/)
         IF ((NEXTW.GT.ND).OR.(NEXTW.LT.NU)) GO TO 200
c     write(n6,911)
c 911 format(1x,'nextw < nd .and. nextw > nu => call bakwrd (nextw,nd)')
         CALL BAKWRD (NEXTW,ND)
         ND = NEXTW
c     write(n6,905) nd,iab(nd)

  200 CONTINUE
  210 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE BAKWRD (N1,N2)
C
C *** THIS SUBROUTINE CALCULATES THE BACKWARD SWEEP
C      XV  IS ARRAY OF DISCHARGES AND STAGES

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AB/           IAB(IPSEC)
      NN = (N2 - 1) * 2 + 1
      NN1 = N2 * 2
      IF (N2.NE.NX) GO TO 100
      DENOM = CON(1) * COM(NN,4) - CON(2) * COM(NN,3)
      XV(NN) = (CON(3) * COM(NN,4) - CON(2) * COZ(NN))/DENOM
      XV(NN1) = (CON(1) * COZ(NN) - CON(3) * COM(NN,3))/DENOM
C#    WRITE(6,99)
C# 99 FORMAT(1X,'NN-CON1-CON2-CON3-COZ-DENOM-XVNN-XVNN1')
C#    WRITE(6,*) NN,CON(1),CON(2),CON(3),COZ(NN),DENOM,XV(NN),XV(NN1)
  100 KYOL = NN1
      KI = NN
      NLAST = N2 - 1
      DO 110 I = N1,NLAST

         KY = KI - 1
         KQ = KY - 1
C NEU EINGEFUEHRT BIS $$$$$
      II = INT(REAL(KY)/2.)
      DO 102 KW = 1,NWEIRS
      IF (II.NE.IWEIR(KW)) GO TO 102
      GO TO 104
 102  CONTINUE
C $$$$$
 103     YNUME = CO(KY,3) * XV(KI) + CO(KY,4) * XV(KYOL)
         XV(KY) = (COZ(KY) - YNUME)/COM(KY,2)
         XV(KQ) = (COZ(KQ) - COM(KQ,4) * XV(KY))/COM(KQ,3)
         KYOL = KY
         KI = KQ
         GO TO 110
  104 CONTINUE
      NI = (II - 1) * 2 + 1
      NI1 = II * 2
      DENOM = CO(NI1,1) * COM(NI,4) - CO(NI1,2) * COM(NI,3)
      XV(NI) = (E(NI1) * COM(NI,4) - CO(NI1,2) * COZ(NI))/DENOM
      XV(NI1) = (CO(NI1,1) * COZ(NI) - E(NI1) * COM(NI,3))/DENOM
  110 CONTINUE
      RETURN
C
      END

C-----------------------------------------------------------------------
      SUBROUTINE BILA
      INCLUDE 'DIMENS.INC'

C&&&  =================================================================
C
      CHARACTER*30 WHQDAT
C
      REAL  VRET, DUEB, ZRET
      REAL  KOR1, KOR2, KOR3
      INTEGER   IVR(IPSPE)

C   ipspe auf 6 gesetzt

      LOGICAL   SCHALT, RETKN, LOG, AUSBORD

      COMMON /RET1/    NSPE(IPSEC,IPSPE), NSP(IPSEC,IPSPE), NRET,
     1                 NSTORE, NWERTE(IPSPE), ZETA(IPSPE),DGRENZ(IPSPE),
     2                 NVERB(IPSEC), INDEX(IPSEC)
      COMMON / RET3 /  VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
      COMMON / RET4 /     KOR1(IPSEC), KOR2(IPSEC), KOR3(IPSEC)
      COMMON / RETLOG /   SCHALT(IPSEC), RETKN(IPSEC), AUSBORD(IPSEC)
      COMMON / SPGANG /   ZRGANG(IPHYD,IPSPE),VRGANG(IPHYD,IPSPE),JIGA
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
C&&&  =================================================================
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /STEP/         DTSAVE
      COMMON /ERR/          TOL
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /CREEP/        GINITL(IPGAT,IPPRC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /READ/         ITERAT
      COMMON /GATOLD/       GOLD(IPGAT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /TIB/          DT    , TIME , ITOTH, ITOTM,TOTH,JTIME
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /FLOWS/        NQIN   , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /WAVE/        HWELLE(IPOUT,IPWEL), QWELLE(IPOUT,IPWEL)
     1,             ITUN, IWEL, DTWEL, NWEL, KWEL(IPOUT), TOT, STIME
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /GWOUT/ XHGW(IPSEC),IS(IPSEC),IXGW(IPSEC)
      COMMON /BILAN/ QKNOTN(IPSEC),SQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      COMMON /REGEN/ SUMN
      COMMON /TEXT/  TITLE1,TITLE2
      COMMON /NUMB/  LEAD
C     REAL Q2(IPSEC), D2(IPSEC)
      COMMON /Q2D2/ Q2(IPSEC), D2(IPSEC)
C
c     REAL QORT(IPLAT), QBASIS(IPLAT), QWAVE(IPSEC)
      REAL QWAVE(IPSEC), SUMQG(4,IPSEC)
      REAL QWAVE2(IPINF),QUEBER(IPSEC)
      REAL QMAX(IPSEC), HMAX(IPSEC), VMAX(IPSEC)
      REAL HGWMAX(IPSEC)
      CHARACTER*80  TITLE1, TITLE2
C
C
C     AUSDRUCK DER KNOTEN -UND GESAMTBILANZEN
C
C     VARIABLEN:
C     LATRED  REDUZIERTE ANZAHL KNOTEN MIT SEITL. ZUFLUSS
C     QTOTAL  SUMME DER ZUFLUESSE AUS DEM GRUNDW.(INSTAT)
C     BASTOT  SUMME DER ZUFLUESSE (LATCOM = 0)
Calt! QORTOT  SUMME DER ZUFLUESSE AUS ORTSCHAFTEN (LUTZANSATZ)
C     QORTOT  SUMME DER SONSTIGEN SEITLICHEN ZUFLšSSE
C     QWATOT  SUMME DER ZUFLUSSWELLEN IN EINTRITTSKNOTEN
C     REGTOT  GESAMTNIEDERSCHLAG DER TEILFLAECHE
C     FLTOT   GESAMTFLAECHE
C     QBASIS  SEITLICHER ZUFLUSS , FEST VORGEGEBEN (LATCOM = 0)
C     SUMQG(1,I)  ZUFLUSS AUS ORTSCHAFTEN
C     SUMQG(2,I)  GRUNDWASSERZUFLUSS  1-D-MODELL GROUND
C     SUMQG(3,I)  ZUFLUSS BERECHNET UEBER UH
C     SUMQG(4,I)  GRUNDWASSERZUFLUSS BERECHNET MIT MODFLOW
Calt! QORT    ZUFLUSS AUS ORTSCHAFTEN
C     QORT    SONSTIGER SEITLICHER ZUFLUSS
C     QWAVE   ZUFLUSSWELLE IN EINTRITTSKNOTEN
C     FLAECH  AM KNOTEN EINGELEITETE TEILFLAECHE
C
C
C     ANFANGSWERTE
C
c     DO I = 1,KWEL(NWEL)
c       QORT(I) = 0.0
c       QBASIS(I) = 0.0
c     END DO

      DO I=1,IPINF
         QWAVE2(I)=0.0
      END DO
 
      UEBTOT=0.0
      QLATOT=0.0
      FLTOT =0.0
      QTOTAL=0.0
      QORTOT=0.0
      QWATOT=0.0
      QUEB  =0.0
      REGTOT=0.0
      VREGES=0.0
C
C     UMSORTIEREN DER LATERALEN ZUFLUSS-SUMMEN
C
      DO 900 IZ=1,IPSEC
         SUMQG(1,IZ)=0.0
         SUMQG(2,IZ)=0.0
         SUMQG(3,IZ)=0.0
         SUMQG(4,IZ)=0.0
  900 CONTINUE
      IZ=0
      DO 1000 J=1,LATINF
         DO 1100 KL=1,4
            IF (SQG(KL,J).NE.0.0) THEN
              CALL ZEIGER(J,L1E,IAB,IZ)
C
C             interne Nummer des Knotens !
              L1(J)=IZ
C
              IF (IZ.GT.0) THEN
                 IF (IZ.GT.IPSEC) THEN
                    PRINT '(T2,A,I5)','   IZ = ',IZ
                    PRINT '(T2,A,I5)','IPSEC = ',IPSEC
                    STOP 'UP BILA IZ > IPSEC ! '
                 END IF
                 SUMQG(KL,IZ)=SQG(KL,J)
              END IF
            END IF
 1100    CONTINUE
 1000 CONTINUE
C
C     ANZAHL KNOTEN MIT SEITL.ZUFLUSS VON LATINF AUF LATRED SETZEN
      DO 3008 J=1,LATINF
        IF (L1(J).GT.KWEL(NWEL)) THEN
          LATRED=J-1
          GO TO 3009
        ELSE
          LATRED=J
        END IF
 3008 CONTINUE
C
C
C     TITELZEILE
C
 3009 WRITE (N6,1991)
      WRITE (29,1991)
 1991 FORMAT (1H1,1X///,'   GESAMTBILANZ FUER DEN SIMULATIONS',
     1'LAUF'/3X,37('-')//)
      WRITE (N6,1387) TITLE1
      WRITE (29,1387) TITLE1
 1387 FORMAT (1X,A72)
      WRITE (N6,1386) TITLE2
      WRITE (29,1386) TITLE2
 1386 FORMAT (1X,A72)
      WRITE (N6,1209) IAB(KWEL(1))
      WRITE (29,1209) IAB(KWEL(1))
 1209 FORMAT (1X,'ANFANGSKNOTEN         ',12X,I10)
      IF (KWEL(NWEL).GT.0) THEN
         WRITE (N6,1389) IAB(KWEL(NWEL))
         WRITE (29,1389) IAB(KWEL(NWEL))
      END IF
 1389 FORMAT (1X,'AUFSUMMIERT BIS KNOTEN',12X,I10)
      WRITE (N6,1388) NINT(TOTH)
      WRITE (29,1388) NINT(TOTH)
 1388 FORMAT (1X,'SIMULATIONSDAUER      ',12X,I10,' H')
      WRITE (N6,1385) SUMN
      WRITE (29,1385) SUMN
 1385 FORMAT (1X,'NIEDERSCHLAGSMENGE    ',12X,F10.2,' MM'///)
C
C
C     TABELLENKOPF
      WRITE(N6,1992)
 1992 FORMAT (1X,'³',116('-'),'³')
      WRITE(N6,1993)
 1993 FORMAT (1X,'³   KNOTEN   ³   SUMME    ³   SUMME    ³',
     1 '   SUMME    ³   SUMME    ³   SUMME    ³   FLŽCHE   ³',
     2 '   SUMME    ³  ABFLUSS   ³')
      WRITE(N6,2011)
 2011 FORMAT (1X,'³   NUMMER   ³ LAT.ZUFLUSS³ LAT.ZUFLUSS³',
     1 ' LAT.ZUFLUSS³  ZUFLUSS   ³     N      ³            ³',
     2 ' UEBERLAUF  ³   FUELLE   ³')
      WRITE(N6,2012)
 2012 FORMAT (1X,'³            ³ GRUNDWASSER³  SONSTIGE  ³',
     1 '  LAND      ³  OBERHALB  ³            ³            ³',
     2 '            ³            ³')
      WRITE(N6,1994)
 1994 FORMAT (1X,'³            ³    M**3    ³    M**3    ³',
     1 '   M**3     ³    M**3    ³    M**3    ³    M**2    ³',
     2 '   M**3     ³    M**3    ³')
      WRITE(N6,1992)
C
C
C     ZUFLUSSWELLE IN EINTRITTSKNOTEN (BERECHNUNG)
      ZEITIN=INCRE*DT
      DO 1961 LZAEHL=1,NQIN
        DO 1962 MZAEHL=1,(LEAD-1)
          QQUER=(QUP(MZAEHL,LZAEHL)+QUP((MZAEHL+1),LZAEHL))*0.5
          QWAVE2(LZAEHL)=QWAVE2(LZAEHL)+QQUER*ZEITIN
 1962   CONTINUE
        QWAVE2(LZAEHL)=ENGSI(QWAVE2(LZAEHL),3)
 1961 CONTINUE
C
C
C     KONTROLLE DER AUSBORDUNGSWASSERMENGE
      DO 1967 MZAEHL=1,NT
        QUEB = QUEB + QOGES(MZAEHL)*DT
 1967 CONTINUE
C
C     TABELLENERSTELLUNG
      DO 1997 IZAEHL=KWEL(1),KWEL(NWEL)
C
C
C       NIEDERSCHLAG IN CBM + FLŽCHE IN M**2
        RAINKN=0.0
        FLAECH=0.0
        DO 1951 J=1,LATINF
          IF ((L1(J).EQ.IZAEHL).AND.(LATCOM(J).EQ.1)) THEN
            GESBRT=DGW(1,J)+DGW(2,J)
            FLAECH=GESBRT*ENGSI(DX(IZAEHL),1)
            RAINKN=FLAECH*SUMN*0.001
            REGTOT=REGTOT+RAINKN
            FLTOT =FLTOT+FLAECH
          END IF
 1951   CONTINUE
C
C       ABFRAGE AUF EINTRITTSKNOTEN
        DO 1964 NZAEHL=1,NQIN
          IF (IZAEHL.EQ.NUP(NZAEHL)) THEN
            QWAVE(IZAEHL)=QWAVE2(NZAEHL)
            GO TO 1965
          END IF
 1964   CONTINUE
        QWAVE(IZAEHL)=0.0
C
C       SUMME ALLER ZUFLUSSWELLEN
 1965   QWATOT=QWATOT+QWAVE(IZAEHL)
C
C       ABFRAGE AUF KNOTEN MIT SEITLICHEM ZUFLUSS
C       DO 1971 J=1,LATINF
C         IF (L1(J).EQ.IZAEHL) GO TO 1972
C1971   CONTINUE
C
C       GO TO 1973
C
C       KNOTEN MIT SEITLICHEM ZUFLUSS BZW. ZUFLUSSWELLE
C1972   IF (LATCOM(J).EQ.0) THEN
C         ZUFLUSSWELLE ALS WELLE EXTERN VORGEGEBEN
c         QBASIS(IZAEHL)=SUMQG(IZAEHL)
c         QORT(IZAEHL)=0.0
c         SUMQG(IZAEHL)=0.0
c         BASTOT=BASTOT+QBASIS(IZAEHL)
          QORTOT=QORTOT+SUMQG(1,IZAEHL)
C       ELSE IF (LATCOM(J).EQ.1 .OR. LATCOM(J).EQ.3) THEN
C         ZUFLUSSWELLE AUS GW ZUFLUSS
c         QBASIS(IZAEHL)=0.0
c         QORT(IZAEHL)=0.0
          QTOTAL=QTOTAL+SUMQG(2,IZAEHL)+SUMQG(4,IZAEHL)
C       ELSE IF (LATCOM(J).EQ.2) THEN
C         ZUFLUSSWELLE AUS ORTSCHAFTEN (LUTZANSATZ)
c         QBASIS(IZAEHL)=0.0
c         QORT(IZAEHL)=SUMQG(IZAEHL)
c         SUMQG(IZAEHL)=0.0
c         QORTOT=QORTOT+QORT(IZAEHL)
          QLATOT=QLATOT+SUMQG(3,IZAEHL)
C       ELSE
C         WRITE(N6,9999)
C9999     FORMAT('     WIE KOMMST DU HIERHER !?!')
C       END IF
 1973   CONTINUE
C!!     DO 2003 IZ1=1,NSTORE
C!!     DO 2003 IZ2=1,NVERB(IZ1)
C!!       IF(NSP(IZ2,IZ1).EQ.IZAEHL) THEN
C
C       AUSDRUCK DER TABELLE

C!!     WRITE(N6,2987) IAB(IZAEHL),SUMQG(IZAEHL),QORT(IZAEHL),
C!!  1  QBASIS(IZAEHL),QWAVE(IZAEHL),RAINKN,FLAECH,IZ1,
C!!  2  QKNOTN(IZAEHL)
C!!       XXX = ENGSI(XL(IZAEHL),1)
C!!     WRITE(29,5001) IAB(IZAEHL),XXX,SUMQG(IZAEHL),QORT(IZAEHL),
C!!  1  QBASIS(IZAEHL),QWAVE(IZAEHL),RAINKN,FLAECH,IZ1,
C!!  2  QKNOTN(IZAEHL)
C!!         GOTO 1996
C!!       END IF
C2003   CONTINUE
C
C       AUSDRUCK DER TABELLE
        QOSU = QOSUM(IZAEHL)*DT*(-1.)
        UEBTOT = UEBTOT + QOSU
        WRITE(N6,1987) IAB(IZAEHL),SUMQG(4,IZAEHL),SUMQG(1,IZAEHL),
     1  SUMQG(3,IZAEHL),QWAVE(IZAEHL),RAINKN,FLAECH,QOSU,
     2  QKNOTN(IZAEHL)
          XXX = ENGSI(XL(IZAEHL),1)
        WRITE(29,5000) IAB(IZAEHL),XXX,SUMQG(4,IZAEHL),
     1  SUMQG(1,IZAEHL),SUMQG(3,IZAEHL),QWAVE(IZAEHL),RAINKN,FLAECH,
     2  QOSU,QKNOTN(IZAEHL)
 5000   FORMAT (I5,1X,9(G11.5))
 5002   FORMAT (17X,7(G11.5))
 5001   FORMAT (I5,1X,7(G11.5),'RET.FL.: ',I2,G11.5)
 1987   FORMAT (1X,'³  ',I5,'     ³',G11.5,' ³',G11.5,' ³',G11.5,
     1  ' ³',G11.5,' ³',G11.5,' ³',G11.5,' ³',G11.5,' ³',G11.5,' ³')
 2987   FORMAT (1X,'³  ',I5,'     ³',G11.5,' ³',G11.5,' ³',G11.5,
     1  ' ³',G11.5,' ³',G11.5,' ³',G11.5,' ³ RET.FL.:',I2,' ³',G11.5,
     2  ' ³')
 1996   WRITE(N6,1984)
 1984   FORMAT (1X,'³',116('-'),'³')
 1997 CONTINUE
C
C     AUSDRUCK DER SUMMEN
C     SUMME DES SEITLICHEN ZUFLUSSES
      WRITE(N6,1983) QTOTAL,QORTOT,QLATOT,QWATOT,REGTOT,FLTOT,UEBTOT
      WRITE(29,5002) QTOTAL,QORTOT,QLATOT,QWATOT,REGTOT,FLTOT,UEBTOT
 1983 FORMAT (1X,'³   SUMME    ³',G11.5,' ³',G11.5,' ³',G11.5,' ³',
     1G11.5,' ³',G11.5,' ³',G11.5,' ³',G11.5,' ³  XXXXXXXX  ³')
      WRITE(N6,1982)
 1982 FORMAT (1X,'³',116('-'),'³'///)
C
C
C     BILANZ FUER GESAMTE SIMULATIONSDAUER
      WRITE (N6,1215)
      WRITE (29,5003)
 1215 FORMAT (1H1,'º',55('-'),'º')
 5003 FORMAT (1H ,'º',55('-'),'º')
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (29,1216)
 1216 FORMAT (1X,'º',55X,'º')
      WRITE (N6,1291)
      WRITE (29,1291)
 1291 FORMAT (1X,'º    GESAMTBILANZ FšR DEN SIMULATIONS',
     1'LAUF ',14X'º'/' º',4X,37('-'),14X,'º')
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1287) TITLE1
      WRITE (29,1216)
      WRITE (29,1287) TITLE1
 1287 FORMAT (1X,'º',4X,A45,6X,'º')
      WRITE (N6,1286) TITLE2
      WRITE (29,1286) TITLE2
 1286 FORMAT (1X,'º',4X,A45,6X,'º')
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1211) IAB(KWEL(1))
      WRITE (29,1216)
      WRITE (29,1211) IAB(KWEL(1))
 1211 FORMAT (1X,'º    ANFANGSKNOTEN         ',9X,I9,11X'º')
      IF (KWEL(NWEL).GT.0) THEN
         WRITE (N6,1289) IAB(KWEL(NWEL))
         WRITE (29,1289) IAB(KWEL(NWEL))
      END IF
 1289 FORMAT (1X,'º    AUFSUMMIERT BIS KNOTEN',9X,I9,11X'º')
      WRITE (N6,1288) NINT(TOTH)
      WRITE (29,1288) NINT(TOTH)
 1288 FORMAT (1X,'º    SIMULATIONSDAUER      ',9X,I9,'      H    º')
      WRITE (N6,1280) REGTOT
      WRITE (29,1280) REGTOT
 1280 FORMAT (1X,'º    GEBIETSNIEDERSCHLAG   ',9X,G11.5,' M**3    º')
      WRITE (N6,1299) FLTOT
      WRITE (29,1299) FLTOT
 1299 FORMAT (1X,'º    FLŽCHE                ',9X,G11.5,' M**2    º')
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1216)
      IF (KWEL(NWEL).GT.0) THEN
         WRITE (N6,1281) QKNOTN(KWEL(NWEL))
         WRITE (29,1216)
         WRITE (29,1281) QKNOTN(KWEL(NWEL))
      END IF
 1281 FORMAT (1X,'º    ABFLUSSFšLLE          ',9X,G11.5,' M**3    º')
C
C     ABFANGEN, FALLS DER TRENNUNGSKNOTEN NICHT DEN BEGINN EINES
C                    GEWAESSERS MARKIERT!
C
      LOG = .TRUE.
      DO 3333  I=1,NQIN
      IF(NUP(I).EQ.KWEL(1)) THEN
      LOG = .FALSE.
      END IF
 3333 CONTINUE
        IF (LOG) THEN
      WRITE (N6,3337) QWATOT
      WRITE (N6,3336) KWELE(1),QKNOTN(KWEL(1))
      WRITE (29,3337) QWATOT
      WRITE (29,3336) KWELE(1),QKNOTN(KWEL(1))
        QWATOT = QWATOT + QKNOTN(KWEL(1))
        END IF
      WRITE (N6,1285) QWATOT
      WRITE (29,1285) QWATOT
 1285 FORMAT (1X,'º    ZUFLUSS VON OBERHALB  ',8X,'-',G11.5,
     1' M**3    º')
 3336 FORMAT (1X,'º  ( ZUFLUSS AM KNOTEN',I5,9X,G11.5,
     1' M**3 )  º')
 3337 FORMAT (1X,'º  ( ZUFLUSS AN EINTRITTSKN.',8X,G11.5,
     1' M**3 )  º')
      WRITE (N6,1284) QLATOT
      WRITE (29,1284) QLATOT
 1284 FORMAT (1X,'º    SEITL. ZUFLUSS (LAND) ',8X,'-',G11.5,
     1' M**3    º')
      WRITE (N6,1270) QTOTAL
      WRITE (29,1270) QTOTAL
 1270 FORMAT (1X,'º    SEITL. ZUFLUSS (GW)   ',8X,'-',G11.5,
     1' M**3    º')
      WRITE (N6,1283) QORTOT
      WRITE (29,1283) QORTOT
 1283 FORMAT (1X,'º    SONSTIGE SEITL. ZUFL. ',8X,'-',G11.5,
     1' M**3    º')
      WRITE (N6,1282) UEBTOT
      WRITE (29,1282) UEBTOT
 1282 FORMAT (1X,'º    UEBERLAUFMENGE        ',8X,'+',G11.5,
     1' M**3    º')
C     WRITE (N6,1001) QUEB
 1001 FORMAT (1X,'º (KONTROLLE DER UEBERLAUFM. ',6X,' ',G11.5,
     1' M**3 )  º')
      WRITE (N6,1279)
      WRITE (29,1279)
 1279 FORMAT (1X,'º',4X,47('-'),4X,'º')
C     DIFSUM=QKNOTN(KWEL(NWEL))-BASTOT-QORTOT-QTOTAL-QWATOT+UEBTOT
      DIFSUM=QKNOTN(KWEL(NWEL))-QORTOT-QTOTAL-QWATOT+UEBTOT
      IF (DIFSUM.GT.0.) THEN
        WRITE (N6,1278) DIFSUM
        WRITE (29,1278) DIFSUM
 1278   FORMAT (1X,'º    ABFLUSS - ZUFLUSS',14X,G11.5,' M**3    º')
      ELSE
        WRITE (N6,1277) ABS(DIFSUM)
        WRITE (29,1277) ABS(DIFSUM)
 1277   FORMAT (1X,'º    ABFLUSS - ZUFLUSS',13X,'-',G11.5,' M**3    º')
      END IF
C
C         SPEICHERINHALT AM ENDE MUSS BERUECKSICHTIGT WERDEN!
      IF (NSTORE.GT.0) THEN
        WRITE (N6,1216)
        WRITE (N6,1216)
        WRITE (N6,1219)
        WRITE (N6,1216)
        WRITE (29,1216)
        WRITE (29,1219)
        WRITE (29,1216)
 1219 FORMAT (1X,'º       RETENTIONSFLAECHEN :          ',12X,
     1'      º')
      ELSE
      GOTO 3334
      END IF
      DO 2999  I = 1,NSTORE
        DO 2998 II = KWEL(1),KWEL(NWEL)
          DO 2998 III = 1,NVERB(I)
            IF (NSP(III,I).EQ.II) THEN
              VRE = ENGSI(VRET(I),3)
              WRITE (N6,1269) I,VRE
              WRITE (29,1269) I,VRE
 1269 FORMAT (1X,'º    RETENTIONSRAUM NR.',I2,'  ',8X,'+',G11.5,
     1' M**3    º')
             VREGES = VREGES + VRE
             GOTO 2999
            END IF
 2998   CONTINUE
 2999 CONTINUE
        WRITE (N6,1279)
        WRITE (N6,3335) VREGES
        WRITE (29,1279)
        WRITE (29,3335) VREGES
 3335   FORMAT (1X,'º    GESPEICHERTES VOLUMEN',10X,G11.5,' M**3    º')
        WRITE (N6,1216)
        WRITE (N6,1216)
        WRITE (N6,3342)
        WRITE (N6,1216)
        WRITE (29,1216)
        WRITE (29,3342)
        WRITE (29,1216)
        DIFSUM = DIFSUM*(-1.)
      IF (DIFSUM.GT.0.) THEN
        WRITE (N6,3350) DIFSUM
        WRITE (29,3350) DIFSUM
      ELSE
        WRITE (N6,3351) ABS(DIFSUM)
        WRITE (29,3351) ABS(DIFSUM)
      END IF
        WRITE (N6,3339) VREGES
        WRITE (N6,1279)
        WRITE (29,3339) VREGES
        WRITE (29,1279)
        DIF = DIFSUM - VREGES
      IF (DIF.GT.0.) THEN
        WRITE (N6,3340) DIF
        WRITE (29,3340) DIF
      ELSE
        WRITE (N6,3341) ABS(DIF)
        WRITE (29,3341) ABS(DIF)
      END IF
 3350   FORMAT (1X,'º    ZUFLUSS - ABFLUSS',13X,'+',G11.5,' M**3    º')
 3351   FORMAT (1X,'º    ZUFLUSS - ABFLUSS',13X,'-',G11.5,' M**3    º')
 3341 FORMAT (1X,'º    SUMME                ',9X,'-',G11.5,
     1' M**3    º')
 3340 FORMAT (1X,'º    SUMME                ',10X,G11.5,
     1' M**3    º')
 3339 FORMAT (1X,'º    GESPEICHERTES VOLUMEN',9X,'-',G11.5,
     1' M**3    º')
 3342 FORMAT (1X,'º       GESAMTAENDERUNG :             ',12X,
     1'      º')
C
 3334 CONTINUE
      WRITE (N6,1216)
      WRITE (N6,1216)
      WRITE (N6,1217)
      WRITE (29,1216)
      WRITE (29,1217)
 1217 FORMAT (1X,'º',55('-'),'º'///////)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CFNT (DT,ITIME)
C
C      THIS IS THE CALLING SUBROUTINE FOR SETTING THE MATRIX COEFFICIENT

      INCLUDE 'DIMENS.INC'

      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) , XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /CONV/         CONVEC
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /AB/           IAB(IPSEC)
      LOGICAL CHECK
c     CHECK = .TRUE.
      CHECK = .FALSE.
      DO 100 I = 1,NX
         IF (D(I).LE.CONVEC) CHECK = .TRUE.
  100 CONTINUE
      IF (NJUNC.EQ.0) GO TO 140
      DO 130 I = 1,NL
         DO 110 KK = 1,NJUNC
            IF (I.EQ.NXJ(KK,1)) GO TO 120
            IF (I.EQ.NXJ(KK,3)) GO TO 130
            IF (I.EQ.NXJ(KK,6)) GO TO 130
  110    CONTINUE
         CALL COEF (DT,ITIME,I,CHECK)
         GO TO 130
  120    CALL JUNCCO (KK)
  130 CONTINUE
      RETURN
  140 DO 150 I = 1,NL
  150 CALL COEF (DT,ITIME,I,CHECK)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE COEF (DT,ITIME,I,CHECK)
C
C     THIS SUBROUTINE CALCULATES THE COEFFICIENTS FOR MATRIX SOLUTION.
C     SUBROUTINE IS CALLED FOR EACH TIME PERIOD

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /GATOLD/       GOLD(IPGAT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC),DSR(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /WEHR2/ NWEHR,IWEHR(IPWEI),IWRAUS(IPWEI)
      LOGICAL CHECK
c     CHARACTER meth*4
C
C      GRAV IS GRAVITATIONAL CONSTANT; GRAV = 32.174
C
c     Meth='ja  '
c     IF(CHECK) meth='nein'
c     Write(25,991) Itime,meth
c991   format(1x,'Itime = ',I5,'  Konvekt. Terme beruecks.: ',a4)
c     Write(25,992)
c992   format(1x,' Knoten ','     C1    ','     C1P   ','     C2    ',
c     & 2x,'     C3    ','     C5    ','     C6    ','     C7    ')
      K = I + 1
      IF (K.GT.IPSEC) THEN
         WRITE(N6,'(A)') 'STOP IN COEF: K > IPSEC !'
         STOP 'COEF: K > IPSEC !'
      END IF
      VA = U(I)
      VAK = U(K)
C
C      GATE SECTIONS
C
C
C     DHEPS = 0.0001 m
      DHEPS = 0.0001
      DHEPS = SIENG(DHEPS,1)
C
      IF (NGATES.LE.0) GO TO 110
      DO 100 LOT = 1,NGATES
         IF (I.NE.IGATE(LOT)) GO TO 100
         ANEW = GATEL(LOT)
         DMAX = D(I)
         IF (D(K).GT.DMAX) DMAX=D(K)
C
         IF (DMAX.GT.HEIT(I)) DMAX=HEIT(I)
C
C      THIS STATEMENT CHECKS FOR GATE OPENINGS GREATER THAN THE DEPTH
C
C        IF (ANEW.GT.((D(I) + D(K))/2.)) GO TO 118
         IF (ANEW.GT.(0.95*DMAX)) GOTO 118
c        DELH = H(I) - H(K)
c        IF (ABS(DELH).LT.DHEPS) THEN
c           WRITE(25,'(T2,A,I5,A)')'COEF: GATE',IAB(I),' DELH < DHEPS !'
c           GOTO 118
c        END IF
         AOLD = GOLD(LOT)
         GO TO 120
  100 CONTINUE
  110 IF (NWEIRS.LE.0) GO TO 118
      DO 115 LOT = 1,NWEIRS
         IL = LOT
         IF (I.NE.IWEIR(LOT)) GO TO 115
         GO TO 125
  115 CONTINUE
  118 CONTINUE
C
C   COEFFICIENTS FOR EQUATIONS:
C     CONTINUITY: -(THE)*Q(I) + C1*Y(I) + THE*Q(I+1) + C1P*Y(I+1) = C2
C
C     MOMENTUM:     C3*Q(I)   + C5*Y(I) + C4*Q(I+1)  + C6*Y(I+1)  = C7
C
C         FORMULATION IS AS PRESENTED IN THESIS BY D. BOOK
C
C      THE IS TERM (DT/DX)
C
C&&&  =================================================================
C
C     SCHALT = .TRUE.  --->  ALTE BERECHNUNGSWEISE
C
C&&&  =================================================================
C
C!!   IF (SCHALT(I)) THEN
C
      THE = DT/DX(I)
      CS5 = 0.
      IF (ABS(QOFLOW(I)).LE.0.00001) GO TO 119
      IF (DSR(I).EQ.0.0) GO TO 119
C     IF (DSR(I).EQ.0.0) THEN
C        WRITE(N6,'(A)') 'STOP IN COEF: DIVIDING BY DSR = 0 !'
C        WRITE(N6,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',I,') = ',IAB(I)
C        WRITE(*,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',I,') = ',IAB(I)
C        STOP 'COEF: DIVIDING BY  Q = 0 !'
C     END IF
      CS5 = 0.750 * QOFLOW(I) / DSR(I)
 119  C1 = T(I) * 0.5 - CS5 * DT
      C1P = T(K) * 0.5 - CS5 * DT
      C2 = C1 * D(I) + C1P * D(K) + DT * (QLAT1(I) + QOFLOW(I))
C
C&&&  =================================================================
C
C     SCHALT = .FALSE.  --->  BERECHNUNG MIT RETENTIONSFLAECHEN
C
C&&&  =================================================================
C
C!!   ELSE
C!!   THE = DT/DX(I)
C!!   C1  = T(I) * 0.5 + KOR1(I) * DT
C!!   C1P = T(K) * 0.5 + KOR1(I) * DT
C!!   C2  = C1 * D(I) + C1P * D(K) + DT * (QLAT1(I) + KOR2(I) + KOR3(I))
C!!   END IF
C!!
C&&&  =================================================================
C
      CS0 = GRAV * DT
      CS2 = AREA(I) + AREA(K)
      CS3 = D(K) - D(I)
      CS4 = SF(I) + SF(K)
      CS7 = ZO(I) - ZO(K)
      IF (Q(I).EQ.0.0) THEN
         WRITE(N6,'(A)') 'STOP IN COEF: DIVIDING BY  Q = 0 !'
         WRITE(N6,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',I,') = ',IAB(I)
C        WRITE(*,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',I,') = ',IAB(I)
C        STOP 'COEF: DIVIDING BY  Q = 0 !'
         Q(I)=0.0001
      END IF
      IF (Q(K).EQ.0.0) THEN
         WRITE(N6,'(A)') 'STOP IN COEF: DIVIDING BY  Q = 0 !'
         WRITE(N6,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',K,') = ',IAB(K)
C        WRITE(*,'(T2,A,I3,A,I5)') 'KNOTEN IAB(',K,') = ',IAB(K)
C        STOP 'COEF: DIVIDING BY  Q = 0 !'
         Q(K)=0.0001
      END IF
      CQ1 = 2. * SF(I)/Q(I)
      CQ2 = 2. * SF(K)/Q(K)
      CK1 = SF(I) * ( - 3.33 * T(I)/AREA(I) + 1.33 * DPYH(I)/WP(I))
      CK2 = SF(K) * ( - 3.33 * T(K)/AREA(K) + 1.33 * DPYH(K)/WP(K))
      T1 = 0.5 * CS0 * CS2 * CQ1
      C3 = 1. - 4. * THE * VA + T1
C
C     IGNORING THE TERM DQV/DX IN THE MOMENTUM EQUN MAY INCREASE
C             STABILITY AT HIGH FROUDE NUMBERS.
C
C      IF CHECK IS TRUE IGNORE CONVECTIVE TERMS
C
      IF(CHECK) C3 = 1.+T1
      T1 = GRAV * THE * ((CS3 - CS7) * T(I) - CS2)
      T2 = 0.5 * CS0 * (CS4 * T(I) + CS2 * CK1)
      C5 = 2. * THE * U(I) * VA * T(I) + T1 + T2
      IF(CHECK) C5 = T1+T2
      T1 = 0.5 * CS0 * CS2 * CQ2
      C4 = 1. + 4. * THE * VAK + T1
      IF(CHECK) C4 = 1.+T1
      T1 = GRAV * THE * (CS2 + (CS3 - CS7) * T(K))
      T2 = 0.5 * CS0 * (CS4 * T(K) + CS2 * CK2)
      C6 =  - 2. * THE * U(K) * VAK * T(K) + T1 + T2
      IF(CHECK) C6 = T1+T2
      T1 =  - 2. * THE * (Q(K) * VAK - Q(I) * VA)
      T2 = GRAV * THE * CS2
      T3 = 0.
      T4 = C3 * Q(I) + C5 * D(I) + C4 * Q(K) + C6 * D(K)
      C7 = T1 + T2 * (CS7 - CS3) - .5 * CS0 * CS2 * CS4 + T3 + T4
      IF(CHECK) C7 = C7-T1
      GO TO 170
C
C     HERE WE ENTER CALCULATIONS FOR THE GATES.
C
  120 CONTINUE
      C1 = 0.0
      C1P = 0.
      C2 = 0.0
      C3 = 0.5
      C4 = 0.5
      DELH = H(I) - H(K)
      IF (ABS(DELH).LT.0.001) THEN
         WRITE(25,'(T2,A,I5,A)')'GATE',IAB(I),' ABS(DELH) < 0.001 !'
         IF (DELH.GE.0.0) DELH =  0.0001
         IF (DELH.LT.0.0) DELH = -0.0001
      END IF
      IF (DMAX.LE.0.0.AND.DELH.GT.0.0) DELH = 0.0001
      IF (DMAX.LE.0.0.AND.DELH.LE.0.0) DELH = -0.0001
      CALC = 0.5 * Q(I)/DELH
      C5 =  - CALC
      C6 = CALC
C
C     WIDTH OF GATE SECTION FACTORS OUT OF EQUN FOR RECTANGULAR CASE.
C
      EONE = 0.5 * Q(I) + (Q(I)/((ANEW + AOLD) * 0.5)) * (ANEW - AOLD)
      C7 = EONE + CALC * (ZO(I) - ZO(K))

      GO TO 170

C
C     HERE WE ENTER CALCULATIONS FOR THE WEIRS.     (NEU)
C
  125 CONTINUE

C   ABFANGEN DER UMKEHRSTR™MUNG (KEINE WEHRBERECHNUNG NACH POLENI !)

C?    IF (Q(I).GT.0.0) THEN
C
      STEM = D(I)
      HSTEM = ZO(I)+D(I)
C
      WRITE(25,'(A,I5)')    '  IAB =',IAB(I)
      WRITE(25,'(A,I5)')    ' WEHR =',IAB(IWEHR(IL))
      WRITE(25,'(A,F8.3)') '  STEM =',ENGSI(STEM,1)
      WRITE(25,'(A,F8.3)') ' HSTEM =',ENGSI(HSTEM,1)
C
Calt  HUW = H(K)
C     eingefügt am 06.06.2007
Calt  HUW = H(K) + U(K)*U(K)/(2.*GRAV)
C     verbessert am 14.06.2007
      HV2O = U(I)*U(I)/(2.*GRAV)
      HV2U = U(K)*U(K)/(2.*GRAV)
      IF (HV2U.GT.HV2O) THEN
         HUW = H(K) + V2CO(IL)*(HV2U - HV2O)
         WRITE (25,'(A)') '989: HUW = H(K) + V2CO(IL)*(HV2U - HV2O)'
      ELSE
         HUW = H(K)
         WRITE (25,'(A)') '992: HUW = H(K)'
      END IF
      HOW = H(I)
      WRITE (25,'(2(A,F8.3))') 'HOW=',ENGSI(HOW,1),' HUW=',ENGSI(HUW,1)
C??
C??
      IF (HOW.LT.HUW.AND.Q(I).LE.0.0) THEN
         WRITE(25,'(A)') 'COEF: FLIESSUMKEHR AM WEHR!'
C?       HUW = H(I)
C?       HOW = H(K)
C?       WRITE(25,'(2(A,F8.3))')'HOW=',ENGSI(HOW,1),' HUW=',ENGSI(HUW,1)
C??      STEM=HUW-ZO(I)
C        eingefügt am 04.10.2007
         WRITE(25,'(A)') 'COEF: HSTEM=(HOW+HUW)/2. gesetzt!'
         HSTEM=(HOW+HUW)/2.
         STEM = HSTEM-ZO(I)
C        Markierung Fliessumkehr! STEM negativ!
         STEM=-STEM
      ELSE IF (HOW.LT.HUW.AND.Q(I).GT.0.0) THEN
C        eingefügt am 04.10.2007
         WRITE(25,'(A)') 'COEF: HSTEM=(HOW+HUW)/2. gesetzt!'
         HSTEM=(HOW+HUW)/2.
         STEM = HSTEM-ZO(I)
      END IF
C
      IF (WCO(IL).GT.0.0) THEN
         HWS = ZO(I) + HW(IL)
      ELSE
         HW(IL) = 0.0
         HWS = ZO(I) + HW(IL)
      END IF
      QUO = (HUW-HWS)/(HOW-HWS)
      IF (HOW.LE.HWS.AND.HUW.LE.HOW) QUO=0.0
      IF (HOW.LE.HWS.AND.HUW.GT.HOW) QUO=1.0
      IF (QUO.GT.1.0) QUO=1.0
      IF (QUO.LT.0.0) QUO=0.0
      QUOOLD2=QUO
      QUOALT2=QUO
      QUOOLD=QUO
      QUOALT=QUO
      QUOVOR=QUO
      QUOBAK=QUO
      HOLD2=HOW
      HALT2=HOW
      HOLD=HOW
      HALT=HOW
      HVOR=HOW
      HBAK=HOW
C
      HWBAK=HW(IL)
      DHWTMP=0.0
      NITER=0
C       
 1160 CONTINUE
C       
      NITER=NITER+1
C       
      IF (NITER.GE.7) THEN
        QUOMI=(QUOOLD2+QUOALT2+QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/7.
      ELSE IF (NITER.EQ.6) THEN
        QUOMI=(QUOALT2+QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/6.
      ELSE IF (NITER.EQ.5) THEN
        QUOMI=(QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/5.
      ELSE IF (NITER.EQ.4) THEN
        QUOMI=(QUOALT+QUOVOR+QUOBAK+QUO)/4.
      ELSE IF (NITER.EQ.3) THEN
        QUOMI=(QUOVOR+QUOBAK+QUO)/3.
      ELSE IF (NITER.EQ.2) THEN
        QUOMI=(QUOBAK+QUO)/2.
      END IF
C       
C
      CALL POLENI (I,STEM,Q(I),HW(IL),BW(IL),WCO(IL),WNEI(IL),D(I),
     &             QUOMI,CCFAK(IL))
C
C     D(I) ist Ergebnis von POLENI incl. HW(IL)
C     STEM = alte Tiefe DALT
C     HWS = Wehrschwellenhöhe z.B. in m+NN
C
C     HWS = neue Wehrschwelle
      HWS = ZO(I) + HW(IL)
C     HHW = neuer Wasserstand!
      HHW = D(I) + ZO(I)
C
      WRITE (25,'(A,F8.3)') '1094  HHW =',ENGSI(HHW,1)
      WRITE (25,'(A,F8.3)') '1095  D(I)=',ENGSI(D(I),1)
      WRITE (25,'(A,F8.3)') '1096  HWS =',ENGSI(HWS,1)
      WRITE (25,'(A,F8.3)') '1097  HUW =',ENGSI(HUW,1)
C
C
C     Hier Nutzung von WH(IL,IPSTP) und WQ(IL,IPSTP) eingefügt!
C
      IF (WHQDAT(IL)(1:3).EQ.'H-Q') THEN
        DO ISTP=1,(IPSTP-1)
          WQU=WQ(IL,ISTP)
          WQO=WQ(IL,ISTP+1)
          WQI=Q(I)
          IF (WQI.GT.WQU.AND.WQI.LE.WQO) THEN
            WHU=WH(IL,ISTP)
            WHX=WHU
            WHO=WH(IL,ISTP+1)
            IF (WHO.GT.WHX) WHX=WHO
            IF (WHU.GT.0.AND.WHO.GT.0) THEN
              WRITE (25,'(A,F8.3)') ' WQO=',ENGSI(WQO,3)
              WRITE (25,'(A,F8.3)') ' WQI=',ENGSI(WQI,3)
              WRITE (25,'(A,F8.3)') ' WQU=',ENGSI(WQU,3)

              WHI=WHU+((WHO-WHU)/(WQO-WQU))*(WQI-WQU)
               
              WRITE (25,'(A,F8.3)') ' WHO=',ENGSI(WHO,1)
              WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
              WRITE (25,'(A,F8.3)') ' WHU=',ENGSI(WHU,1)
               
              IF (WHI.GT.WHX) THEN
                WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
                WRITE (25,'(A,F8.3)') ' WHX=',ENGSI(WHX,1)
                STOP
              END IF
            ELSE
              IF (HHW.LT.WH(IL,1)) THEN
                WHI=WH(IL,1)
              ELSE
                WHI=0.0
                GOTO 1165
              END IF
            END IF
C           Hier wird D(I) ggf. neu belegt!
            IF (WHI.GT.0) THEN
              IF (WHI.LT.HHW) THEN
                WRITE (25,'(A)') ' WHI < HHW !'
                WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
                WRITE (25,'(A,F8.3)') ' HHW=',ENGSI(HHW,1)
                WHI = (WHI + HHW)/2.
              END IF
              DHWTMP = WHI - HHW
              WRITE (25,'(A,F8.3)') 'DHWTMP =',ENGSI(DHWTMP,1)
              HHW = WHI
              D(I) = WHI - ZO(I)
              HBAK=HHW
              HW(IL) = HW(IL) + DHWTMP
              WRITE (25,'(A,F8.3)') 'HW(IL) =',ENGSI(HW(IL),1)
            ELSE
              HW(IL) = HWBAK
            END IF
          END IF
        END DO
 1165   CONTINUE
      END IF
C
C
      IF (STEM.GT.0.0.AND.HHW.LT.HUW) THEN
C        Korrektur für normale Fließrichtung bei massivem Rückstau
         HHW = HUW
      ELSE
         IF (NITER.GE.7) THEN
           HHW = (HOLD2+HALT2+HOLD+HALT+HVOR+HBAK+HHW)/7.
         ELSE IF (NITER.EQ.6) THEN
           HHW = (HALT2+HOLD+HALT+HVOR+HBAK+HHW)/6.
         ELSE IF (NITER.EQ.5) THEN
           HHW = (HOLD+HALT+HVOR+HBAK+HHW)/5.
         ELSE IF (NITER.EQ.4) THEN
           HHW = (HALT+HVOR+HBAK+HHW)/4.
         ELSE IF (NITER.EQ.3) THEN
           HHW = (HVOR+HBAK+HHW)/3.
         ELSE IF (NITER.EQ.2) THEN
           HHW = (HBAK+HHW)/2.
         END IF
      END IF

      WRITE (25,'(A,I5)')   '1176 NITER =',NITER
      WRITE (25,'(A,F8.3)') '1177   HHW =',ENGSI(HHW,1)

      QUO = (HUW-HWS)/(HHW-HWS)
      IF (HHW.LE.HWS.AND.HUW.LE.HOW) QUO=0.0
      IF (HHW.LE.HWS.AND.HUW.GT.HOW) QUO=1.0
      IF (QUO.GT.0.9999) THEN
         QUO=1.0
         HHW = HUW
      END IF

      IF (QUO.LT.0.0) QUO=0.0
      DQUO = ABS(QUOBAK-QUO)
      HOLD2=HALT2
      HALT2=HOLD
      HOLD=HALT
      HALT=HVOR
      HVOR=HBAK
      HBAK=HHW
      QUOOLD2=QUOALT2
      QUOALT2=QUOOLD
      QUOOLD=QUOALT
      QUOALT=QUOVOR
      QUOVOR=QUOBAK
      QUOBAK=QUO
      IF (NITER.LT.3) GOTO 1160
      IF (NITER.LT.30.AND.DQUO.GT.0.0005) GOTO 1160
      WRITE(25,'(A,F10.4)') 'QUOMIT = ',QUOMI

      D(I) = HHW - ZO(I)
C
C
C
      WRITE (25,'(2(A,F8.3))') 'HHW=',ENGSI(HHW,1),' HUW=',ENGSI(HUW,1)
C
C 149 IF (STEM.GE.D(I)) GOTO 160
  149 DIFSD = STEM-D(I)
      IF (DIFSD.GT.-0.03) GOTO 160
      WRITE (N6,150) IAB(I)
  150 FORMAT(/1X,'BERECHNETE TIEFE AM WEHR ',I5,' IST KLEINER ALS DIE PO
     &LENI-TIEFE; POLENI-WERT WIRD BENUTZT'/)
      ITI1 = ITIME - 1
      STEM = ENGSI(STEM,1)
      D(I) = ENGSI(D(I),1)
      DIFSD = STEM-D(I)
      WRITE (N6,*) ' DIFSD = ',DIFSD
      WRITE (N6,151) ITI1,STEM,D(I)
  151 FORMAT(1X,'ZEITSCHRITT =',I5,5X,'BERECHNETE TIEFE =',G10.4,5X,
     &'TIEFE NACH POLENI =',G10.4/)
      STEM = SIENG(STEM,1)
      D(I) = SIENG(D(I),1)
C     TIEFEN MITTELN...
      D(I) = (D(I)+STEM+STEM+STEM)/4.

C     TIEFEN MITTELN...
C     D(I) = (D(I)+STEM)/2.
 
 
      XV(I*2) = D(I)
C     WEHRBERECHNUNG NACH POLENI FERTIG !

      IF (Q(I).LE.0.0) THEN

       HI = D(I)+ZO(I)
       HI = ENGSI(HI,1)
       HK = ENGSI(H(K),1)
       WRITE(N6,'(T2,A,I5)')   'Q < 0 AM WEHR: IAB(I) = ',IAB(I)
       WRITE(N6,'(T2,A,F7.3)') '                 H(I) = ',HI
       WRITE(N6,'(T2,A,I5)') '                 IAB(K) = ',IAB(K)
       WRITE(N6,'(T2,A,F7.3)') '                 H(K) = ',HK

       IF (H(I).GT.H(K)) THEN
        WRITE(N6,'(T2,A)') 'WSP PASST NICHT ZU Q < 0 !'
        HMIT = 0.5*(H(I)+H(K))
        D(I) = HMIT - ZO(I)
       END IF
      END IF

  160 CONTINUE

      SCOEW = 1.5 * Q(I) / ( D(I)-HW(IL) )
      K = I * 2
      CO(K,1) = 1.
      CO(K,2) = -SCOEW
      CO(K,3) = 0.
      CO(K,4) = 0.
      E(K)    = Q(I) - SCOEW * D(I)
      K = K + 1
      CO(K,1) = 0.
      CO(K,2) = 0.
      CO(K,3) = 1.
      CO(K,4) = 0.
C     E(K)    =   SIEHE UNTERPROGRAMM FORWEI
      GO TO 200
  170 CONTINUE
      K = I * 2
      CO(K,1) =  - THE
      CO(K,2) = C1
      CO(K,3) = THE
      CO(K,4) = C1P
      E(K) = C2
      K = K + 1
      CO(K,1) = C3
      CO(K,2) = C5
      CO(K,3) = C4
      CO(K,4) = C6
      E(K) = C7
c200   Write(25,993) iab(i),C1,C1P,C2,C3,C5,C6,C7
c993   format(1x,I6,2x,3G11.3,2x,4G11.3)
200   RETURN
      END

C-----------------------------------------------------------------------
      subroutine date1 (datum,zeit,ha)
C**********************************************************************
C     Subroutine  D A T E 1         IHW  Universitaet Karlsruhe       *
C=====================================================================*
C     Abfrage des aktuellen Datums                                    *
C---------------------------------------------------------------------*
C     Aufruf:      CALL DATE1 (DATUM)                                 *
C---------------------------------------------------------------------*
C     EINGABE: keine                                                  *
C                                                                     *
C     AUSGABE: DATUM : Nummer des Tages, Character*3                  *
C              MON   : Monat, Character*3                             *
C              JAHR  : Jahr, Character*4                              *
C              ZEIT  : aktuelle Uhrzeit, Character*10                 *
C                                                                     *
C---------------------------------------------------------------------*
C     Beim Aufruf wird gleichzeitig die Startzeit gesetzt, mit der    *
C     ueber das Proghramm RUNTIM die verbrauchte Rechenzeit abgefragt *
C     werden kann.                                                    *
C---------------------------------------------------------------------*
C     erstellt am :   22.12.1986      Ihringer                        *
C     geaendert am:                                                   *
C**********************************************************************

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)

      CHARACTER     datum*12
      CHARACTER*3   MONAT(12)
      CHARACTER*10  zeit

      REAL NSTART
      INTEGER*2 YY, MO, DD, HH, MM, SS, HD

      DATA MONAT / 'JAN', 'FEB', 'MAE', 'APR', 'MAI', 'JUN',
     &             'JUL', 'AUG', 'SEP', 'OKT', 'NOV', 'DEZ' /

      CALL GETDAT (YY,MO,DD)
      CALL GETTIM (HH,MM,SS,HD)

      WRITE (datum,1) dd, monat(mo), YY
1     FORMAT (I2,'. ',a3,i5)

      WRITE (zeit,2) HH, MM, SS
2     FORMAT (1X,I2,':',I2,':',I2)

      nstart = REAL(hd)
      nstart = nstart + REAL(ss)*100.
      nstart = nstart + REAL(mm)*6000.
      nstart = nstart + REAL(hh)*360000.
      HA = nstart

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DOWN (NN,ITIME)
C
C      SUBROUTINE IS USED TO SET COEFFICIENTS FOR DOWNSTREAM BOUNDARY CO
C***  C12*Q+C13*D=C14 IS THE FORMULA USED HERE.

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /BNDRY/        FLOWER      , DHH         , DHT         ,
     1        ZSILL
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /TBLE/         DYDQ(IPRAT)
      COMMON /DOWNCO/       QDON(IPRAT) , YDON(IPRAT) , IPTS        ,
     1        IDOWN       , A           , B
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      DIMENSION             EDS(IPRAT)
      GO TO (100,130,180,190), IDOWN
C
C      RATING EQUATION IS USED
C
  100 CONTINUE
      IF (D(NX).GT.ZSILL) GO TO 110
C
C      DEPTH LESS THAN SILL HEIGHT
C     HERE THE FLOW IS LIMITED TO THE MINIMUM AMOUNT ALLOWED.
C
      FLOW = 10.000
      GO TO 200
  110 CONTINUE
C
C      FLOWER IS MAXIMUM DISCHARGE ALLOWED
C
      FLOW = FLOWER
      IF (Q(NL).GT.(FLOW - 0.10 * FLOW)) GO TO 200
C
C     CORRECT DEPTH FOR RATING CURVE EQUATION
C
      STEMP = D(NX)
      D(NX) = (ABS(Q(NX))/A) *  * (1./B) + ZSILL
C
C      CHECK FOR DEPTH WITHIN 10% OF RATING CURVE VALUE
C
      IF (ABS(STEMP - D(NX)).LE.(0.10 * D(NX))) GO TO 120
      WRITE (N6,220)
      ITI = ITIME - 1
      STEMP = ENGSI(STEMP,1)
      D(NX) = ENGSI(D(NX),1)
      WRITE (N6,230) ITI,STEMP,D(NX)
      STEMP = SIENG(STEMP,1)
      D(NX) = SIENG(D(NX),1)
  120 CONTINUE
C
C     SCOEF IS A COEFFICIENT DERIVED FROM LINEARIZATION OF THE BROAD
C        CRESTED WEIR FORMULA.
C
      SCOEF = B * Q(NX)/(D(NX) - ZSILL)
      C13 =  - SCOEF
      C12 = 1.0
      C14 = Q(NX) - SCOEF * D(NX)
      GO TO 210
C
C      RATING TABLE USED
C
  130 CONTINUE
      DO 140 I = 1,IPTS
  140 EDS(I) =  - DYDQ(I) * QDON(I + 1) + YDON(I + 1) - ZO(NX)
C     MAY ALSO USE Q(NX) AND D(NX) IN ABOVE EQUATION
C 111 EDS(I)=-DYDQ(I)*Q(NX) + D(NX)
      C13 = 1.0
C
C      FIND INTERVAL IN TABLE IN WHICH CURRENT DISCHARGE FALLS
C
      DO 160 I = 1,IPTS
         IF (Q(NX).GE.QDON(I).AND.Q(NX).LT.QDON(I + 1)) GO TO 150
         GO TO 160
  150    C12 =  - DYDQ(I)
         C14 = EDS(I)
         GO TO 210
  160 CONTINUE
      IF (Q(NX).LT.QDON(1)) GO TO 170
C
C      Q GREATER THAN MAXIMUM VALUE IN TABLE
C
      C12 =  - DYDQ(IPTS)
      C14 = EDS(IPTS)
      GO TO 210
C
C      Q LESS THAN MINIMUM VALUE IN TABLE
C
  170 C12 =  - DYDQ(1)
      C14 = EDS(1)
      GO TO 210
C
C      STAGE HYDROGRAPH USED
C
  180 C12 = 1.0E - 10
      C13 = 1.0
      C14 = HDOWN(ITIME) - ZO(NX)
      GO TO 210
C
C      DISCHARGE HYDROGRAPH USED
C
  190 FLOW = QDOWN(ITIME)
  200 CONTINUE
      C12 = 1.0
C
C  HERE THE BOUNDARY COND. IS BASED ON MAX. OR MIN. FLOW ALLOWED
C
      C13 = 1.0E - 10
      C14 = FLOW
  210 CONTINUE
      CO(NN,1) = C12
      CO(NN,2) = C13
      E(NN) = C14
      RETURN
C
  220 FORMAT (1X,'BERECHNETE TIEFE STIMMT NICHT MIT DER ABFLUSSKURVE AM
     1GEBIETSAUSLASS UEBEREIN; WERT DER ABFLUSSKURVE WIRD BENUTZT'/)
  230 FORMAT (1X,'ZEITSCHRITT :',I5,10X,'BERECHNETE TIEFE :',10X,G10.4,
     19X,'TIEFE AUS DER ABFLUSSKURVE :',G10.4/)
      END

C-----------------------------------------------------------------------
      SUBROUTINE DRAIN(HW,INOD,IL,IM1,IM2,IT)

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /PRECIP/       PE(IPPRC),NPREC,DTPREC
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /GWKOE1/ C1,C11,C2,C2S,C3,C3S,C4,C4S,C5,DXGW1,LDA
      COMMON /GWKOE2/ G1,G11,G2,G2S,G3,G3S,G4,G4S,G5,DXGW2
      COMMON /THOM/ A(IPGW,3),B(IPGW)
      REAL LDA


******************************************************
*     BERECHNUNG DER MATRIXELEMENTE ZUR LOESUNG      *
*     DES LINEAREN GLEICHUNGSSYSTEMS                 *
******************************************************

C
      PP1=PE(IT-1)*CLOSS(1,IL)/1000./DTGW
      PP2=PE(IT-1)*CLOSS(2,IL)/1000./DTGW

C     FUER KNOTEN 1  (Leakageansatz)
      IF(DGW(1,IL).EQ.0.) GOTO 40

      A(1,1)=  0.

      IRB=0
C     IF(IRB.EQ.1) GOTO 30

      IF(HGW(1,INOD).LT.GS(1,IL)) THEN
      A(1,3)= C5
      A(1,2)= -(C5+C4)
C     B(1)=QZ(IT)+DQZ+PP1*DXGW1/2.+C4*HGW(1,INOD)
      B(1)=-QSTAT(1,IL)-PP1*DXGW1/2.-C4*HGW(1,INOD)
      ELSE
      A(1,3)= C5
      A(1,2)= -(C5+C4S)
C     B(1)=QZ(IT)+DQZ+PP1*DXGW1/2.+C4S*HGW(1,INOD)
      B(1)=-QSTAT(1,IL)-PP1*DXGW1/2.-C4S*HGW(1,INOD)
      ENDIF
      GOTO 42

C 30  CONTINUE
C     IF(HGW(1,INOD).LT.GS(2,IL)) THEN
C     A(1,3)= C5
C     A(1,2)= -(C5+C4+LGW1)
C     B(1)=-LGW1*QZ(IT)-DQZ-PP1*DXGW1/2.-C4*(HGW(1,INOD)
C     ELSE
C     A(1,1)= C5
C     A(1,2)= -(C5+C4S+LGW1)
C     B(1)=-LGW1*QZ(IT)-DQZ-PP1*DXGW1/2.-C4S*(HGW(1,INOD)
C     ENDIF
C     GOTO 42

  40  CONTINUE
      A(1,1)=0.
      A(1,2)=1.
      A(1,3)=-1.
      B(1)=0.
  42  CONTINUE

C     FUER KNOTEN 2 BIS NGV-1
      DO 50 I=2,NGV-1
      IF(DGW(1,IL).EQ.0.) GOTO 45
      A(I,1)= C11
      IF(HGW(I,INOD).LT.GS(1,IL)) THEN
        A(I,2)=C3
        B(I)=-C2*HGW(I,INOD)-PP1
        ELSE
        A(I,2)=C3S
        B(I)=-C2S*HGW(I,INOD)-PP1
      ENDIF
      A(I,3)=C11
      GOTO 50

  45  CONTINUE
      A(I,1)=1.
      A(I,2)=-2.
      A(I,3)=1.
      B(I)=0.

  50  CONTINUE

C     FUER KNOTEN NGV (KNOTEN AM GEWAESSER)
      IF(DGW(1,IL).EQ.0.) GOTO 51
      IF(DGW(2,IL).EQ.0.) GOTO 52
      DXQ=.5*(DXGW1+DXGW2)
      GSW=.5*(GS(1,IL)+GS(2,IL))
      PPQ=.5*(PP1+PP2)
      A(NGV,1)=C5
      A(NGV,3)=G5
      IF(HGW(NGV,INOD).LT.GSW) THEN
      A(NGV,2)= -(C5+G5+LDA+.5*(C2+G2)*DXQ)
      B(NGV)=-HW*LDA-PPQ*DXQ-.5*(C2+G2)*HGW(NGV,INOD)*DXQ
      ELSE
      A(NGV,2)= -(C5+G5+LDA+.5*(C2S+G2S)*DXQ)
      B(NGV)=-HW*LDA-PPQ*DXQ-.5*(C2S+G2S)*HGW(NGV,INOD)*DXQ
      ENDIF
      GOTO 53

  51  CONTINUE
      DXQ=.5*DXGW2
      GSW=GS(2,IL)
      PPQ=PP2
      A(NGV,1)=0.
      A(NGV,3)=G5
      IF(HGW(NGV,INOD).LT.GSW) THEN
      A(NGV,2)= -(G5+LDA+G2*DXQ)
      B(NGV)=-QSTAT(1,IL)-HW*LDA-PPQ*DXQ-G2*HGW(NGV,INOD)*DXQ
      ELSE
      A(NGV,2)= -(G5+LDA+G2S*DXQ)
      B(NGV)=-QSTAT(1,IL)-HW*LDA-PPQ*DXQ-G2S*HGW(NGV,INOD)*DXQ
      ENDIF
      GOTO 53

  52  CONTINUE
      DXQ=.5*DXGW1
      GSW=GS(1,IL)
      PPQ=PP1
      A(NGV,3)=0.
      A(NGV,1)=C5
      IF(HGW(NGV,INOD).LT.GSW) THEN
      A(NGV,2)= -(C5+LDA+C2*DXQ)
      B(NGV)=-QSTAT(2,IL)-HW*LDA-PPQ*DXQ-C2*HGW(NGV,INOD)*DXQ
      ELSE
      A(NGV,2)= -(C5+LDA+C2S*DXQ)
      B(NGV)=-QSTAT(2,IL)-HW*LDA-PPQ*DXQ-C2S*HGW(NGV,INOD)*DXQ
      ENDIF
  53  CONTINUE

C     FUER KNOTEN NGV+1 BIS NGW-1
      DO 60 I=NGV+1,NGW-1
      IF(DGW(2,IL).EQ.0.) GOTO 55
      A(I,1)= G11
      IF(HGW(I,INOD).LT.GS(2,IL)) THEN
        A(I,2)=G3
        B(I)=-G2*HGW(I,INOD)-PP2
        ELSE
        A(I,2)=G3S
        B(I)=-G2S*HGW(I,INOD)-PP2
      ENDIF
      A(I,3)=G11
      GOTO 60

  55  CONTINUE
      A(I,1)=1.
      A(I,2)=-2.
      A(I,3)=1.
      B(I)=0.

  60  CONTINUE

C     FUER KNOTEN NGW
      IF(DGW(2,IL).EQ.0.) GOTO 80
      I=NGW
      A(I,3)=  0.

      IRB=0
C     IF(IRB.EQ.1) GOTO 70

      IF(HGW(I,INOD).LT.GS(2,IL)) THEN
      A(I,1)=-G5
      A(I,2)= G5+G4
C     B(I)=QZ(IT)+DQZ+PP2*DXGW2/2.+G4*HGW(I,INOD)
      B(I)=QSTAT(2,IL)+PP2*DXGW2/2.+G4*HGW(I,INOD)
      ELSE
      A(I,1)=-G5
      A(I,2)= G5+G4S
C     B(I)=QZ(IT)+DQZ+PP2*DXGW2/2.+G4S*HGW(I,INOD)
      B(I)=QSTAT(2,IL)+PP2*DXGW2/2.+G4S*HGW(I,INOD)
      ENDIF
      GOTO 85

C 70  IF(HGW(I,INOD).LT.GS(2,IL)) THEN
C     A(I,1)=-G5
C     A(I,2)= G5+G4+LGW2
C     B(I)=LGW2*QZ(IT)+DQZ+PP2*DXGW2/2.+G4*(HGW(I,INOD)
C     ELSE
C     A(I,1)=-G5+G4S
C     A(I,2)= G5+G4S+LGW2
C     B(I)=LGW2*QZ(IT)+DQZ+PP2*DXGW2/2.+G4S*(HGW(I,INOD)
C     ENDIF
C     GOTO 85

  80  CONTINUE
      A(I,3)=0.
      A(I,2)=1.
      A(I,1)=-1.
      B(I)=0.

******************************************************
*     LOESUNG DES LINEAREN GLEICHUNGSSYSTEMS         *
******************************************************
  85  CALL TRIDI(NGW)

      DO 90 I=1,NGV-1
      IF(HT(I).GT.GS(1,IL) .AND. HGW(I,INOD).LT.GS(1,IL)) THEN
      HT(I) = GS(1,IL)+(HT(I)-GS(1,IL))*POR(IM1)
      ENDIF
  90  CONTINUE

      IF(HT(NGV).GT.GSW .AND. HGW(NGV,INOD).LT.GSW) THEN
      HT(NGV) = GSW+(HT(NGV)-GSW)*0.5*(POR(IM1)+POR(IM2))
      ENDIF

      DO 95 I=NGV+1,NGW
      IF(HT(I).GT.GS(2,IL) .AND. HGW(I,INOD).LT.GS(2,IL)) THEN
      HT(I) = GS(2,IL)+(HT(I)-GS(2,IL))*POR(IM2)
      ENDIF
  95  CONTINUE

******************************************************
*     BERECHNUNG DER DRAINAGERATE                    *
******************************************************

      SOHLE=ENGSI(ZO(INOD),1)
      IF(HT(NGV).LT.SOHLE) THEN
      GWFLOW(IL)=(SOHLE-HW)*LDA

      WRITE(N6,201) L1E(IL),HW,SOHLE,LDA,GWFLOW(IL),QSTAT(1,IL),
     1              QSTAT(2,IL)
  201 FORMAT(1X,'KN-#     HW    HH     LDA      GWFLOW   ',
     1 '   QZU-LI       QZU-RE',/,
     2       1X,I5,2F7.2,F9.3,G12.4,2X,G12.4,2X,G12.4)
      ELSE
      GWFLOW(IL)=(HT(NGV)-HW)*LDA
      ENDIF
c     IF(IRB.EQ.1) THEN
c     QZ(IT)=(QZ(IT)-HT(N))*LGW
c     ENDIF


  500 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORJUN (ND,ND2,ND3,ND4,I)
C
C      FORWARD SWEEP THROUGH JUNCTIONS

      INCLUDE 'DIMENS.INC'

      COMMON /TIB/    DT      , TIME  , ITOTH, ITOTM,TOTH,JTIME
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /CMS/          CM(IPJUN,9)
      COMMON /EQUA/         E1(IPJUN)   , F1(IPJUN)   , F2(IPJUN)   ,
     1        F3(IPJUN)   , F4(IPJUN)   , E2(IPJUN)   , G1(IPJUN)   ,
     2        G2(IPJUN)   , G3(IPJUN)   , G4(IPJUN)   , E3(IPJUN)   ,
     3        P5(IPJUN)   , FG1(IPJUN)  , FG2(IPJUN)  , FG3(IPJUN)  ,
     4        FG4(IPJUN)  , E23(IPJUN)
      COMMON /AB/           IAB(IPSEC)
      IML = 2 * ND - 1
      INL = 2 * ND2 - 1
      IMK = 2 * ND3 - 1
      RP1 = 1.0
      RP2 = RP1
      RP3 =  - RP1
      RP5 =  - P5(I)
      RE1 = E1(I)
      RF1 = F1(I)
      RF2 = F2(I)
      RF3 = F3(I)
      RF4 = F4(I)
      RE2 = E2(I)
      RG1 = G1(I)
      RG2 = G2(I)
      RG3 = G3(I)
      RG4 = G4(I)
      RE3 = E3(I)
      RH1 = COM(IML,3)
      RH2 = COM(IML,4)
      RL1 = COM(INL,3)
      RL2 = COM(INL,4)
      RE4 = COZ(IML)
      RE5 = COZ(INL)
      RK1 = RE4/RH1
      RK2 = RH2/RH1
      RK3 = RE5/RL1
      RK4 = RL2/RL1
      RDE = RF2 - RF1 * RK2
      RM1 = (RE2 - RF1 * RK1)/RDE
      RM2 =  - RF3/RDE
      RM3 = RF4/RDE
      RDE = RG2 - RG1 * RK4
      RM4 = (RE3 - RG1 * RK3)/RDE
      RM5 =  - RG3/RDE
      RM6 = RG4/RDE
      COM(IMK,3) = (RP1 * RK2 * RM2) + (RP2 * RK4 * RM5) - RP3 - (RP5 *
     1RM2) - (RP5 * RM5)
      COZ(IMK) = (RP1 * RK1) - (RP1 * RK2 * RM1) + (RP2 * RK3) - (RP2 *
     1RK4 * RM4) - RE1 + RP5 * (RM1 + RM4)
      COM(IMK,4) =  - ((RP1 * RK2 * RM3) + (RP2 * RK4 * RM6) - RP5 * (RM
     13 + RM6) + RP5)
C
C      ARRAY  CM  IS SAVED FOR BACJUN
C
      CM(I,1) = RM1
      CM(I,2) = RM2
      CM(I,3) = RM3
      CM(I,4) = RM4
      CM(I,5) = RM5
      CM(I,6) = RM6
      IF (ND4.EQ.0) GO TO 100
C
C      THIRD INFLOWING BRANCH
C
      ILL = 2 * ND4 - 1
      RP4 = 1.
      RFG1 = FG1(I)
      RFG2 = FG2(I)
      RFG3 = FG3(I)
      RFG4 = FG4(I)
      RE23 = E23(I)
      RHL1 = COM(ILL,3)
      RHL2 = COM(ILL,4)
      RE45 = COZ(ILL)
      RK5 = RE45/RHL1
      RK6 = RHL2/RHL1
      RDE = RFG2 - RFG1 * RK6
      RM7 = (RE23 - RFG1 * RK5)/RDE
      RM8 =  - RFG3/RDE
      RM9 = RFG4/RDE
      COM(IMK,3) = COM(IMK,3) + (RP4 * RK6 * RM8) - (RP5 * RM8)
      COZ(IMK) = COZ(IMK) + (RP4 * RK5) - (RP4 * RK6 * RM7) + (RP5 * RM7
     1)
      COM(IMK,4) = COM(IMK,4) - (RP4 * RK6 * RM9) + (RP5 * RM9)
      CM(I,7) = RM7
      CM(I,8) = RM8
      CM(I,9) = RM9
  100 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORWEI (I)
C
C *** THIS SUBROUTINE CALCULATES THE FORWARD SWEEP OF THE WEIRS ('NEU')

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AB/           IAB(IPSEC)

      II = I * 2
      IJ = II + 1
      IK = II - 1
      COM(IJ,3) = 1.
      COM(IJ,4) = 0.
C   BACKWRD DIREKT OBERHALB WEHR
      DENO = CO(II,1) * COM(IK,4) - CO(II,2) * COM(IK,3)
      IF (DENO.EQ.0.0) THEN
         PRINT '(T2,A,I5)','IAB(I) = ',IAB(I)
         STOP 'UP FORWEI: DENO = 0.0 !'
      END IF
      XQ = ( E(II) * COM(IK,4) - CO(II,2) * COZ(IK) ) / DENO
CCCC  XY = ( CO(II,1) * COZ(IK) - E(II) * COM(IK,3) ) / DENO

      E(IJ)     = XQ
      COM(IJ,3) = 1.
      COM(IJ,4) = 0.
      COZ(IJ)   = XQ

      RETURN
C
      END


C-----------------------------------------------------------------------
      SUBROUTINE FORWRD (N1,N2)
C
C *** THIS SUBROUTINE CALCULATES THE FORWARD SWEEP

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AB/           IAB(IPSEC)
      NM = (N1 - 1) * 2
      NM1 = NM + 1
      I = N1 * 2
      IF (COM(NM1,3).EQ.0.0) THEN
         PRINT '(T2,4(A,I5))','N1= ',N1,'I= ',I,'J= ',J,'IAB= ',IAB(N1)
      END IF
      COM(I,2) = CO(I,2) - CO(I,1) * COM(NM1,4)/COM(NM1,3)
      COZ(I) = E(I) - CO(I,1) * COZ(NM1)/COM(NM1,3)
      J = I + 1
      COM(J,2) = CO(J,2) - CO(J,1) * COM(NM1,4)/COM(NM1,3)
      IF (COM(I,2).EQ.0.0) THEN
         PRINT '(T2,4(A,I5))','N1= ',N1,'I= ',I,'J= ',J,'IAB= ',IAB(N1)
      END IF
      COM(J,3) = CO(J,3) - CO(I,3) * COM(J,2)/COM(I,2)
      COM(J,4) = CO(J,4) - CO(I,4) * COM(J,2)/COM(I,2)
      COZ(J) = E(J) - CO(J,1) * COZ(NM1)/COM(NM1,3) - COM(J,2) * COZ(I)/
     1COM(I,2)
      IFIRST = J + 1
      ILAST = (N2 - 1) * 2 + 1
      IF (IFIRST.GE.ILAST) RETURN
      DO 100 I = IFIRST,ILAST,2
C#      IF (NWEIRS.EQ.0) GO TO 11
C#      DO 10 K=1,NWEIRS
C#      L = INT(REAL(I)/2.)
C#      IF ( L.EQ.IWEIR(K) ) GO TO 50
C 10    CONTINUE
C
C     P    FOR EVEN I
C# 11  CONTINUE
         COM(I,2) = CO(I,2) - CO(I,1) * COM(I - 1,4)/COM(I - 1,3)
         COZ(I) = E(I) - CO(I,1) * COZ(I - 1)/COM(I - 1,3)
         J = I + 1
C
C     P    FOR ODD I (=J)
C
         COM(J,2) = CO(J,2) - CO(J,1) * COM(I - 1,4)/COM(I - 1,3)
         COM(J,3) = CO(J,3) - CO(I,3) * COM(J,2)/COM(I,2)
         COM(J,4) = CO(J,4) - CO(I,4) * COM(J,2)/COM(I,2)
         COZ(J) = E(J) - CO(J,1) * COZ(I - 1)/COM(I - 1,3) - COM(J,2) *
     1   COZ(I)/COM(I,2)
      GO TO 100

  100 CONTINUE
      RETURN
C
      END


C-----------------------------------------------------------------------
      SUBROUTINE GATCO (K)
C
C      THIS SUBROUTINE SETS THE COEFFICIENTS AT GATES   ("NEU")
C      THE JUNCTION EQUATIONS ARE:
C            F1*Q1 + F3*Q3  = E2
C            Q1    - Q3     = E1

      INCLUDE 'DIMENS.INC'

      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /AB/           IAB(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) , XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /TIB/      DT     , TIME , ITOTH, ITOTM ,TOTH,JTIME
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /EQUA/         E1(IPJUN)   , F1(IPJUN)   , F2(IPJUN)   ,
     1        F3(IPJUN)   , F4(IPJUN)   , E2(IPJUN)   , G1(IPJUN)   ,
     2        G2(IPJUN)   , G3(IPJUN)   , G4(IPJUN)   , E3(IPJUN)   ,
     3        P5(IPJUN)   , FG1(IPJUN)  , FG2(IPJUN)  , FG3(IPJUN)  ,
     4        FG4(IPJUN)  , E23(IPJUN)
      I = K - 1
      U1 = U(I)
      U3 = U(K)
      UA1 = ABS(U1)
      UA3 = ABS(U3)
      Z1 = ZO(I)
      Z3 = ZO(K)
      DSDQ1 = DSFDQ(I)
      DSDQ3 = DSFDQ(K)
      DSDY1 = DSFDY(I)
      DSDY3 = DSFDY(K)
      A1 = AREA(I)
      A3 = AREA(K)
      T1 = T(I)
      T3 = T(K)
      D1 = D(I)
      D3 = D(K)
      SF1 = SF(I)
      SF3 = SF(K)
      Q1 = Q(I)
      Q3 = Q(K)
      XL1 = DX(I)
C
C   NOTE:  WHEN ARJUN IS SET TO ZERO, THE STORAGE EFFECT OF THE GATE
C          IS NOT CONSIDERED
C
C     ARJUN=T2*(XL2-XXL(LJ)) + (T1+T3)*XL1/2.
      ARJUN = 1.E - 10
      P5(K) = ARJUN/2./DT
      QL1 = (QOFLOW(I) + QLAT1(I)) * XL1
      E1(K) =  - QL1 - P5(K) * (D1 + D3)
      F1(K) =  1./GRAV * UA1/A1 - .5 * DSDQ1 * XL1
      F2(K) = 1. - 1./GRAV * UA1 * U1/A1 * T1 - .5 * DSDY1 * XL1
      F3(K) =  - 1./GRAV * UA3/A3 - .5 * DSDQ3 * XL1
      F4(K) =  - 1. + UA3 * U3/GRAV * T3/A3 - .5 * DSDY3 * XL1
      FGT1   =  1./GRAV * U1 * (.5 * UA1 - Q1/A1 + UA1/A1 * T1 * D1)
      FGT2 = U3/GRAV * (.5 * UA3 - Q3/A3 + UA3/A3 * T3 * D3)
      FGT4 = SF1 - DSDQ1 * Q1 - DSDY1 * D1
      FGT3 = SF3 - DSDQ3 * Q3 - DSDY3 * D3
      E2(K) =  - Z1 - FGT1 + Z3 + FGT2 + .5 * XL1 * (FGT4 + FGT3)
C
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION DSFDQ(I)

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      DSFDQ = 2. * SF(I)/Q(I)
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION DSFDY(I)

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      DSFDY =  - 2. * SF(I)/AREA(I) * (1.667 * T(I) - .667 * (AREA(I)/WP
     1(I)) * DPYH(I))
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE GATE (N,ITIME,M,CALLUP)

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      CHARACTER*6 CALLUP
      CHARACTER*1 GATMOD
      LOGICAL FLAG
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4), COZ(IPSED)  , CON(3)
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , ITE1        ,
     2        ITE2
      COMMON /AB/           IAB(IPSEC)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /QGAT/ AGA(IPGAT),QGAMIN(IPGAT),QGAMAX(IPGAT),
     1              GATTOL(IPGAT),RMUE(IPGAT),GATMIN(IPGAT),
     1              GATWSO(IPGAT)
      COMMON /MODGAT/ GATMOD(IPGAT)
      COMMON /CREEP/  GINITL(IPGAT,IPPRC)
C
C     SUBR GATE CHECKS AND CORRECTS GATE ERRORS GREATER THAN GATTOL %
C
      DERG = 0.0
      DELH = 0.0
      FLAG = .TRUE.
C
      GATBAK = GATEL(M)
C
      NTWO = N * 2
      IANSI = N + 1
      IIANS = IANSI * 2
      D = XV(NTWO)
      Q = XV(NTWO - 1)
C
      N1 = (N - 1) * 2
      IF (ITIME.LE.1) GO TO 110
C
      H1 = D + ZO(N)
      D1 = D
C
   50 CONTINUE
C
      IF (DERG.GE.0.0.AND.DELH.GE.0.0) THEN
C        DERG noch nicht ermittelt (DERG = 0.0);
C        Annahme, dass Vorzeichen von Q Str”mungsrichtung angibt !
         IF (Q.GE.0.0) THEN
            VORZ = 1.
            N2 = IANSI
            D2 = XV(IIANS)
            H2 = D2 + ZO(N2)
         ELSE IF (Q.LT.0.0) THEN
            VORZ = -1.
            N2 = N-1
            D2 = XV(N1)
            H2 = D2 + ZO(N2)
         END IF
      ELSE IF (DERG.LT.0.0.AND.DELH.GE.0.0) THEN
C        Annahme, dass Vorzeichen von Q Str”mungsrichtung angibt !
         IF (Q.GE.0.0) THEN
            VORZ = 1.
            N2 = IANSI
            D2 = XV(IIANS)
            H2 = D2 + ZO(N2)
         ELSE IF (Q.LT.0.0) THEN
            VORZ = -1.
            N2 = N-1
            D2 = XV(N1)
            H2 = D2 + ZO(N2)
         END IF
      ELSE IF (DERG.LT.0.0.AND.DELH.LT.0.0) THEN
C        DERG und DELH wurden negativ ermittelt;
C        Annahme, dass Strömungsrichtung sich umkehrt,
C        d.h. Vorzeichen von Q wechselt !
         IF (Q.GE.0.0) THEN
            VORZ = -1.
            N2 = N-1
            D2 = XV(N1)
            H2 = D2 + ZO(N2)
         ELSE IF (Q.LT.0.0) THEN
            VORZ = 1.
            N2 = IANSI
            D2 = XV(IIANS)
            H2 = D2 + ZO(N2)
         END IF
      END IF
C
      IF (D1.LE.0.0.OR.D2.LE.0.0) THEN
         WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
         WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': Tiefe < 0.0 !'
         WRITE(25,'(T2,A,F9.3)')'     Q = ',ENGSI(Q,3)
         WRITE(25,'(T2,A,F9.3)')'    D1 = ',ENGSI(D1,1)
         WRITE(25,'(T2,A,F9.3)')'    D2 = ',ENGSI(D2,1)
         WRITE(25,'(T2,A,F9.3)')'    H1 = ',ENGSI(H1,1)
         WRITE(25,'(T2,A,F9.3)')'    H2 = ',ENGSI(H2,1)
C!!!     STOP
      END IF
C
      DELH=H1-H2
C
C     DHEPS = 0.0001 m
      DHEPS = 0.00001
      DHEPS = SIENG(DHEPS,1)
C
      DMAX = D1
      DMIT = 0.5*(D1+D2)
      IF (D2.GT.DMAX) DMAX=D2
      IF (DMAX.LE.0.0.AND.DELH.GT.0.0) DELH = DHEPS
      IF (DMAX.LE.0.0.AND.DELH.LE.0.0) DELH = -1.0*DHEPS
C
C     CHECK FOR GATE OPENING GREATER THAN MAXIMUM-DEPTH
C
C     SICHERN DER VORGEGEBENEN ™FFNUNGSWEITE
C
      GATBAK = GATEL(M)
C
Calt  IF (GATEL(M).GT.((D + XV(IIANS))/2.)) GO TO 110
C
      IF (GATEL(M).LE.(0.95*DMAX).AND.GATEL(M).GT.DMIT) THEN
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
       WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),' NOCH KNAPP EINGETAUCHT !'
      END IF
C
      IF (GATEL(M).GT.(0.95*DMAX).AND.DMAX.GT.0.0) THEN
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
C      GATE WIRD NUR NACHGEFšHRT, WENN GATMOD = 'N' !
       IF (GATMOD(M).EQ.'N') THEN
         GATEL(M)=0.95*DMAX
         WRITE(25,'(T2,A,I5,A,A,F6.3,A)')'GATE',IAB(N),' NACHGEFšHRT !',
     &   ' GATEL = ',ENGSI(GATEL(M),1),' m'
       END IF
       IF (GATEL(M).LT.GATMIN(M)) GATEL(M)=GATMIN(M)
       IF (GATEL(M).GT.DMAX) THEN
          WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),' NICHT MEHR GETAUCHT !'
          WRITE(25,'(T2,A,F6.3,A)')' GATEL = ',ENGSI(GATEL(M),1),' m'
          GOTO 110
       END IF
      END IF
C
C      COMPUTE FLOW WITH GATE EQUATION:
C                  Q = K*A*(2*GRAV*DELH)**1/2
C                  WHERE K = RMUE(M)
      IF (ABS(DELH).LT.DHEPS) THEN
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
       WRITE(25,'(T2,A,I5,A)')'GATE: GATE',IAB(N),'DELH < DHEPS !'
       GOTO 110
      END IF
C
      RMUEGS = (GATEL(M)**2)/(D2**2) + 1./(RMUE(M)**2) - 1.
      RMUEGS = SQRT(RMUEGS)
      RMUEGS = 1./RMUEGS
C
Calt  QCHECK = RMUE(M)*WIDTH(N)*GATEL(M)*SQRT(2.0*GRAV*ABS(DELH)+V*V)
C???  QCHECK = RMUE(M)*WIDTH(N)*GATEL(M)*SQRT(ABS(DERG2G))
C
      QCHECK = RMUEGS*WIDTH(N)*GATEL(M)*SQRT(2.0*GRAV*ABS(DELH))
C
C???  V1=QCHECK/(WIDTH(N)*GATEL(M))
      V1=QCHECK/(WIDTH(N)*D1)
      V2=QCHECK/(WIDTH(N)*D2)
C
      DERG2G = 2.0*GRAV*DELH + V1*V1 - V2*V2
      DERG = VORZ*DERG2G/(2.*GRAV)
C
      WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
      WRITE(25,'(T2,A,F9.3)')'     Q = ',ENGSI(Q,3)
      WRITE(25,'(T2,A,F9.3)')'  VORZ = ',VORZ
      WRITE(25,'(T2,A,F9.3)')'    H1 = ',ENGSI(H1,1)
      WRITE(25,'(T2,A,F9.3)')'    H2 = ',ENGSI(H2,1)
      WRITE(25,'(T2,A,F9.3)')'  DELH = ',ENGSI(DELH,1)
      WRITE(25,'(T2,A,F9.3)')'    V1 = ',ENGSI(V1,1)
      WRITE(25,'(T2,A,F9.3)')'    V2 = ',ENGSI(V2,1)
      WRITE(25,'(T2,A,F9.3)')'  DERG = ',ENGSI(DERG,1)
C
      IF (DERG.LT.0.0.AND.FLAG.AND.DELH.LT.0.0) THEN
         FLAG = .FALSE.
         GOTO 50
      END IF
C
C     Versuch, GATEL(M) so zu beeinflussen, dass GATWSO im OW gilt
C
      IF (GATWSO(M).NE.0.0) THEN
         GATALT=GATEL(M)
         GA=ENGSI(GATEL(M),1)
         IF (H1.GT.H2.AND.GATWSO(M).GT.H2) THEN
            DH=GATWSO(M)-H2
C           DE2G = 2.0*GRAV*DH + V1*V1 - V2*V2
            DE2G = 2.0*GRAV*DH + V1*V1 
            DE = DE2G/(2.*GRAV)
            GATNEU = QCHECK/(RMUEGS*WIDTH(N)*SQRT(2*GRAV*DE))
         ELSE IF (H1.LE.H2.AND.GATWSO(M).GT.H2) THEN
            DH=GATWSO(M)-H1
C           DE2G = 2.0*GRAV*DH + V2*V2 - V1*V1
            DE2G = 2.0*GRAV*DH + V2*V2
            DE = DE2G/(2.*GRAV)
            IF (DE.LT.0.0) STOP 'GATE: DE < 0.0'
            GATNEU = ABS(QCHECK)/(RMUEGS*WIDTH(N)*SQRT(2*GRAV*DE))
         ELSE IF (GATWSO(M).LE.H2) THEN
            GATNEU=AGA(M)
         END IF
         IF (GATNEU.GT.AGA(M)) GATNEU=AGA(M)
         IF (GATNEU.LT.GATMIN(M)) GATNEU=GATMIN(M)
         GN=ENGSI(GATNEU,1)
         WRITE(25,'(T2,2(A,F8.5),A)') 'GA =',GA,'  GN =',GN,' m'
         GATEL(M)=(GATALT+GATNEU+GATNEU)/3.
         GINITL(M,ITIME)=GATEL(M)
         GINITL(M,ITIME+1)=GATNEU
         GN=ENGSI(GATEL(M),1)
         WRITE(25,'(T2,2(A,F8.5),A)') 'GA =',GA,'  GN =',GN,' m'
         QCHECK = RMUEGS*WIDTH(N)*GATEL(M)*SQRT(2.0*GRAV*ABS(DELH))
      END IF
C
C     IF (H1.LT.H2) QCHECK =  - QCHECK
      QCHECK = VORZ * QCHECK
      WRITE(25,'(T2,A,F9.3)')' GATEL = ',ENGSI(GATEL(M),1)
      WRITE(25,'(T2,A,F9.3)')'QCHECK = ',ENGSI(QCHECK,3)
C
C
c     WRITE (25,119) ENGSI(Q,3),ENGSI(QCHECK,3),ENGSI(H1,1),ENGSI(H2,1),
c    1 ENGSI(GATEL(M),1)
c 119 Format(1x,'Q= ',G10.4,2x,'QCHECK= ',G10.4,2x,'H1= ',F7.2,2x,
c    1 'H2= ',F7.2,2x,'GATEL= ',F7.2)
C
      QEPS = GATTOL(M) * ABS(Q)
      WRITE(25,'(T2,A,F9.3)')'  QEPS = ',ENGSI(QEPS,3)
C
      IF (QCHECK.GE.0.0) THEN
C        QCHECK positiv !
         QDIF=ABS(Q-QCHECK)
         IF (QDIF.LE.QEPS) GOTO 110
C???     IF (QCHECK.GE.(Q-QEPS)) GOTO 110
C        sonst Korrektur weiter unten
         WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
         WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': QCHECK positiv:'
         WRITE(25,'(T2,A,F9.3)')'     Q = ',ENGSI(Q,3)
         WRITE(25,'(T2,A,F9.3)')'  VORZ = ',VORZ
         WRITE(25,'(T2,A,F9.3)')'  DMAX = ',ENGSI(DMAX,1)
         WRITE(25,'(T2,A,F9.3)')' WIDTH = ',ENGSI(WIDTH(N),1)
         WRITE(25,'(T2,A,F9.3)')'    V1 = ',ENGSI(V1,1)
         WRITE(25,'(T2,A,F9.3)')'    V2 = ',ENGSI(V2,1)
         WRITE(25,'(T2,A,F9.3)')'    H1 = ',ENGSI(H1,1)
         WRITE(25,'(T2,A,F9.3)')'    H2 = ',ENGSI(H2,1)
         WRITE(25,'(T2,A,F9.3)')'RMUEGS = ',RMUEGS
         WRITE(25,'(T2,A,F9.3)')'  DELH = ',ENGSI(DELH,1)
         WRITE(25,'(T2,A,F9.3)')'  DERG = ',ENGSI(DERG,1)
         WRITE(25,'(T2,A,F9.3)')'QCHECK = ',ENGSI(QCHECK,3)
      ELSE IF (QCHECK.LT.0.0) THEN
C        QCHECK und Q negativ !
         QDIF=ABS(Q-QCHECK)
         IF (QDIF.LE.QEPS) GOTO 110
C???     IF (QCHECK.LE.(Q+QEPS)) GOTO 110
C        sonst Korrektur weiter unten
         WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
         WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': QCHECK negativ:'
         WRITE(25,'(T2,A,F9.3)')'     Q = ',ENGSI(Q,3)
         WRITE(25,'(T2,A,F9.3)')'  VORZ = ',VORZ
         WRITE(25,'(T2,A,F9.3)')'  DMAX = ',ENGSI(DMAX,1)
         WRITE(25,'(T2,A,F9.3)')' WIDTH = ',ENGSI(WIDTH(N),1)
         WRITE(25,'(T2,A,F9.3)')'    V1 = ',ENGSI(V1,1)
         WRITE(25,'(T2,A,F9.3)')'    V2 = ',ENGSI(V2,1)
         WRITE(25,'(T2,A,F9.3)')'    H1 = ',ENGSI(H1,1)
         WRITE(25,'(T2,A,F9.3)')'    H2 = ',ENGSI(H2,1)
         WRITE(25,'(T2,A,F9.3)')'RMUEGS = ',RMUEGS
         WRITE(25,'(T2,A,F9.3)')'  DELH = ',ENGSI(DELH,1)
         WRITE(25,'(T2,A,F9.3)')'  DERG = ',ENGSI(DERG,1)
         WRITE(25,'(T2,A,F9.3)')'QCHECK = ',ENGSI(QCHECK,3)
      END IF
C
C     Kontrolle durch QGAMAX oder QCHECK
      IF (Q.GT.QGAMAX(M).AND.QCHECK.GT.QGAMAX(M)) THEN
        QCHECK=QGAMAX(M)
        WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
        WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': OBERES Q-LIMIT GESETZT!'
      END IF
C
C     Kontrolle durch QCHECK, wenn größer als QGAMIN
      IF (Q.GT.QGAMIN(M).AND.QCHECK.LT.QGAMIN(M)) THEN
       QCHECK=QGAMIN(M)
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
       WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': UNTERES Q-LIMIT GESETZT!'
      END IF
C
C     keine Kontrolle durch QCHECK, wenn kleiner als QGAMIN
      IF (Q.LT.QGAMIN(M).OR.QCHECK.LT.QGAMIN(M)) THEN
       QCHECK=Q
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
       WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': Q selbst wieder gesetzt!'
      END IF
C
C     Mindeswert für QCHECK, wenn kleiner als QGAMIN (13.03.2018)
      IF (QCHECK.LT.QGAMIN(M)) THEN
       QCHECK=QGAMIN(M)
       WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
       WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': UNTERES Q-LIMIT GESETZT!'
      END IF
C
      QAEND = ABS(Q/QCHECK)
      IF (QAEND.GT.1.1.OR.QAEND.LT.0.9) THEN
         QCHECK = 0.5*(Q+QCHECK)
         WRITE(25,'(T2,A,F9.3)')'QCHECK gedämpft = ',ENGSI(QCHECK,3)
      END IF
C
C
C     IF ((ABS(ABS(QCHECK) - ABS(Q))).LE.(GATTOL(M) * ABS(Q))) THEN
C      WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': NICHT KORRIGIERT !'
C      GOTO 110
C     END IF
C
C
      WRITE(25,'(T2,A,I5,A,A)')'GATE',IAB(N),' CALLED BY ',CALLUP
      WRITE(25,120)
C     IF (LIST.GE.1) GO TO 100
      Q = ENGSI(Q,3)
      QCHECK = ENGSI(QCHECK,3)
      WRITE (25,130) IAB(N),Q,QCHECK
      Q = SIENG(Q,3)
      QCHECK = SIENG(QCHECK,3)
  100 CONTINUE
C
C      SET FLOWS TO COMPUTED DISCHARGE
C
      XV(NTWO - 1) = QCHECK
      XV(IIANS - 1) = QCHECK
C
C     THIS COMPLETES THE GATE CORRECTION SECTION OF THE PROGRAM.
C     CONTINUE THE DOUBLE SWEEP PROCESS OF SIMULTANEOUS EQUN SOLUTION.
C
  110 CONTINUE
      WRITE(25,'(T2,A,I5,A)')'GATE',IAB(N),': 110 CONTINUE !'
      CON(1) = CO(N1,1)
      CON(2) = CO(N1,2)
      CON(3) = E(N1) - CO(N1,3) * XV(NTWO - 1) - CO(N1,4) * XV(NTWO)
C
      GATEL(M) = GATBAK
C
      RETURN
C
  120 FORMAT (1X,'ABFLUSS AM GATE KORRIGIERT')
  130 FORMAT (1X,'KNOTEN IAB =',I5,' Q =',G10.4,'  QCHECK =',G10.4)
      END

C-----------------------------------------------------------------------
      SUBROUTINE GETDAT(YY,MO,DD)

      CHARACTER (LEN=8) C8DATE
      CHARACTER (LEN=10) C10TIME
      CHARACTER (LEN=5) C5ZONE
      INTEGER :: IT(8)

      INTEGER*2 YY, MO, DD

      C5ZONE='+0100'

      CALL DATE_AND_TIME (C8DATE,C10TIME,C5ZONE,IT)

      YY=IT(1)-2000
      MO=IT(2)
      DD=IT(3)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GETTIM(HH,MM,SS,HD)

      CHARACTER (LEN=8) C8DATE
      CHARACTER (LEN=10) C10TIME
      CHARACTER (LEN=5) C5ZONE
      INTEGER :: IT(8)

      INTEGER*2 HH, MM, SS, HD

      C5ZONE='+0100'

      CALL DATE_AND_TIME (C8DATE,C10TIME,C5ZONE,IT)

      HH=IT(5)
      MM=IT(6)
      SS=IT(7)
      HD=IT(8)
       
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GROUND(ITIME)
C
C
      INCLUDE 'DIMENS.INC'
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /TIB/          DT          , TIME, ITOTH ,ITOTM,TOTH,JTIME
      COMMON /AB/           IAB(IPSEC)
      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /PRECIP/       PE(IPPRC),NPREC,DTPREC
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /GWKOE1/ C1,C11,C2,C2S,C3,C3S,C4,C4S,C5,DXGW1,LDA
      COMMON /GWKOE2/ G1,G11,G2,G2S,G3,G3S,G4,G4S,G5,DXGW2
      COMMON /BILAN/ QKNOTN(IPSEC),SUMQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      REAL LSYS1, LSYS2, LDA
C

      IF (ITIME.EQ.1) THEN
      DTGW = DT/3600.
      NGW=IPGW
      NGV=(IPGW-1)/2+1
      DO 3 I=1,NX
      DO 3 J=1,NGW
      HGW(J,I)=0.
    3 CONTINUE
      WRITE(N6,701)
      ENDIF

      DO 40 IL=1,LATINF
      DO 30 INOD=1,NX

      IF (L1(IL).NE.INOD) GOTO 30
      IF (LATCOM(IL).NE.1) GOTO 30

      HW  = ENGSI(H(INOD),1)
      GWFLOW(IL)=0.

      IM1 = IAQUI(1,IL)
      IM2 = IAQUI(2,IL)
      LSYS1=DGW(1,IL)
      LSYS2=DGW(2,IL)
      HSOHL=H(INOD)-D(INOD)
C     WENN LSYS1 und LSYS2 = 0., WIRD NUR INFILTRATION INS GW
C     ANGENOMMEN UND BERECHNUNG NACH LEAKAGE-ANSATZ DURCHGEFUEHRT
      IF (LSYS1 .EQ. 0.  .AND. LSYS2 .EQ. 0.) GOTO 18
C
      TR1=TRANS(IM1)
      TR2=TRANS(IM2)
      HW2=SIENG(HGW(NGV,INOD),1)
      IF(ITIME.EQ.1) THEN
      LSYS=LSYS1+LSYS2
      IF(LSYS.GT.0.) THEN
      GHAV=GHA(1,IL)+(GHA(2,IL)-GHA(1,IL))*LSYS1/(LSYS1+LSYS2)
      ELSE
      GHAV=(GHA(1,IL)+GHA(2,IL))/2.
      ENDIF
      HW2=SIENG(GHAV,1)
      ENDIF
      IF(HW2.LE.HSOHL) THEN
      CALL SHAPE(INOD,H(INOD),AREA(INOD),WETPER,TOP)
C     LEAKAGE-FAKTOR LDA FUER BENETZTEN UMFANG
      LDA=(BETA(IM1)+BETA(IM2))/2.*ENGSI(WETPER,1)
      ELSE
      HW2=(HW2+H(INOD))/2.
      CALL SHAPE(INOD,HW2,AREA(INOD),WETPER,TOP)
C     LEAKAGE-FAKTOR LDA FUER BENETZTEN UMFANG
      LDA=(BETA(IM1)+BETA(IM2))/2.*ENGSI(WETPER,1)
      ENDIF
      DXGW1=LSYS1/REAL(NGV-1)
      DXGW2=LSYS2/REAL(NGV-1)

C     BEI INFILTRATION IN DAS GRUNDWASSER WIRD DIE DURCHLAESSIGKEIT
C     (DES GEWAESSERS UM DEN FAKTOR 10 REDUZIERT)
C     NEU: DES GEWAESSERS HALBIERT
C     (WIRD ABER NUR BEI DIREKTEM KONTAKT OBERFLAECHENGEWAESSER
C      MIT GRUNDWASSER DURCHGEFUEHRT)
C     IF(H(INOD).GT.HW2) LDA=LDA*0.10
      IF(H(INOD).GT.HW2) LDA=LDA*0.50

      INSTAT = 0
      IF (ITIME.GT.1) INSTAT=1
      IF (NPREC.EQ.0) INSTAT=0
      IF (INSTAT.EQ.1) GOTO 10

C     BELEGUNG DES GRUNDWASSER-AUSGANGSZUSTANDES

      IF(LSYS1.EQ.0. .OR. LSYS2.EQ.0.) GOTO 4
      W1=LDA+TR2/LSYS2+TR1/LSYS1
      W2=LDA*HW+TR2*GHA(2,IL)/LSYS2+TR1*GHA(1,IL)/LSYS1
      HH=W2/W1
      GOTO 6
    4 HH=GHA(1,IL)
      IF(LSYS2.EQ.0.) HH=GHA(2,IL)

    6 GWFLOW(IL)=LDA*(HH-HW)
c
      SOHLE=ENGSI(HSOHL,1)
      IF(HH.LT.SOHLE) THEN
      GWFLOW(IL)=(SOHLE-HW)*LDA

      WRITE(N6,201) L1E(IL),HW,SOHLE,LDA,GWFLOW(IL),QSTAT(1,IL),
     1              QSTAT(2,IL)
  201 FORMAT(1X,'KN-#     HW    HH     LDA      GWFLOW   ',
     1 '   QZU-LI       QZU-RE',/,
     2       1X,I5,2F7.2,F9.3,G12.4,2X,G12.4,2X,G12.4)
      ENDIF
c

      IF(LSYS2.GT.0.) THEN
      QSTAT(2,IL)=TR2*((GHA(2,IL)-HH)/LSYS2)
      QSTAT(1,IL)=GWFLOW(IL)-QSTAT(2,IL)
      ELSE
      QSTAT(1,IL)=TR1*((GHA(1,IL)-HH)/LSYS1)
      QSTAT(2,IL)=GWFLOW(IL)-QSTAT(1,IL)
      ENDIF

      HKNST1=QSTAT(1,IL)/TR1*DXGW1
      HKNST2=QSTAT(2,IL)/TR2*DXGW2
      DO 8 I=NGV,1,-1
   8  HGW(I,INOD)=HKNST1*REAL(NGV-I)+HH
      DO 9 I=NGV,NGW
   9  HGW(I,INOD)=HKNST2*REAL(I-NGV)+HH


c     WRITE(N6,702) L1E(IL),HW,HH,ENGSI(WETPER,1),LDA,GWFLOW(IL),
c    1              QSTAT(1,IL),QSTAT(2,IL)
      GOTO 20

   10 CONTINUE

C     BERECHNUNG DER BENOETIGTEN KONSTANTEN
C     FUER S=1 WERDEN DIESE MIT DEM ANHANG S1 GEKENNZEICHNET
C     LINKE SEITE
      IF(DXGW1.EQ.0.) DXGW1=.01
      C1=1./DXGW1
      C11=TR1*C1*C1
      C2=POR(IM1)/DTGW
         C2S=1./DTGW
      C3=-2.*C11-C2
         C3S=-2.*C11-C2S
      C4=C2*DXGW1/2.
         C4S=C2S*DXGW1/2.
      C5=TR1/DXGW1
C     RECHTE SEITE
      IF(DXGW2.EQ.0.) DXGW2=.01
      G1=1./DXGW2
      G11=TR2*G1*G1
      G2=POR(IM2)/DTGW
         G2S=1./DTGW
      G3=-2.*G11-G2
         G3S=-2.*G11-G2S
      G4=G2*DXGW2/2.
         G4S=G2S*DXGW2/2.
      G5=TR2/DXGW2
C     **********************************************************
C     DRAIN-MODELL
C
      CALL DRAIN (HW,INOD,IL,IM1,IM2,ITIME)

      DO 15 I=1,NGW
   15 HGW(I,INOD)=HT(I)

C
C     **********************************************************
C
      GOTO 20

   18 CALL SHAPE(INOD,H(INOD),AREA(INOD),WETPER,TOP)
C     LEAKAGE-FAKTOR LDA FUER BENETZTEN UMFANG
      LDA=(BETA(IM1)+BETA(IM2))/2.*ENGSI(WETPER,1)
      SOHLE=ENGSI(HSOHL,1)
      GWFLOW(IL)=(SOHLE-HW)*LDA
      IF(ENGSI(Q(INOD),3).LT.1.50) GWFLOW(IL)=0.

c     WRITE(N6,201) L1E(IL),HW,SOHLE,LDA,GWFLOW(IL),QSTAT(1,IL),
c    1              QSTAT(2,IL)

   20 CONTINUE

      GWFLOW(IL)=GWFLOW(IL)/3600.
      SUMQG(2,IL)=SUMQG(2,IL)+GWFLOW(IL)*3600.

      GOTO 40

   30 CONTINUE

   40 CONTINUE

  701 FORMAT(1X,'KN-#     HW     HH     WP      LDA      GWFLOW   ',
     1 '   QZU-LI       QZU-RE',/,
     2       1X,'       (m+NN) (m+NN)   (m)     (m/h)   (cbm/m/h) ',
     1 '  (cbm/m/h)   (cbm/m/h)',/)
  702 FORMAT(1X,I5,2F7.2,2F9.3,G12.4,2X,G12.4,2X,G12.4)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GWELLEN(DATEI,N,TINC,JLMAX)

      INCLUDE 'DIMENS.INC'
       
      PARAMETER (NDSTP=200,IBMAX=10)
      DIMENSION QQ(NDSTP,IPLAT), DTW(NDSTP), NK(IPSED), JLDIF(IPSED)
      COMMON /QQGWSTAT/     QQMIN(IPLAT),QQMAX(IPLAT)
      COMMON /QLH/          QLI(IPHYD,IPLAT)
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /BILAN/ QKNOTN(IPSEC),SQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      COMMON /WAVE/         HWELLE(IPOUT,IPWEL), QWELLE(IPOUT,IPWEL),
     1               ITUN, IWEL, DTWEL, NWEL, KWEL(IPOUT), TOT, STIME
      INTEGER ZE(IPSED),SP(IPSED),NKN(IPHYD),K(IPSED,5),IB(IPSED)
      DIMENSION QLEA(NDSTP,IBMAX)
      INTEGER IFLAG(IPLAT)
      INTEGER Z(IPSED)
      CHARACTER*30 DATEI
      CHARACTER*50 DATLEAK(10)
      CHARACTER*5 FORMDL(10)
      CHARACTER*5 MODVAR(10)
      COMMON /AB/ IAB(IPSEC)
C
      DO ID=1,5
         FORMDL(ID)='     '
         MODVAR(ID)='     '
      END DO
C
      DO IP=1,IPLAT
         QQMIN(IP)=999999.9
         QQMAX(IP)=-999999.9
      END DO
C
C     TMAX in Minuten
C
      TMAX=REAL(N)*TINC
C
C     Einlesen der GWE-Datei
C
      PRINT '(T2,A)','OPEN: GWE-Datei'
      OPEN (21,FILE=DATEI,STATUS='OLD')
      READ (21,*) NSTEPS, DTW(1)
      IF (NSTEPS.GT.IPHYD) STOP 'GWELLEN: NSTEPS > IPHYD !'
      READ (21,*) NKMAX
C
      PRINT '(T2,A,I5)','GWE-ZELLEN NKMAX = ',NKMAX
C
      IF (NKMAX.GT.IPSED) STOP 'GWELLEN: NKMAX > IPSED !'
      JLMAX=0
      JLAT=0
      JL=0
      DO 100 J=1,NKMAX
        READ(21,55,END=8)IB(J),K(J,1),ZE(J),SP(J),NK(J),(K(J,N1),N1=2,5)
   55   FORMAT(I2,I8,7I10)
CC 55   FORMAT(2X,I8,7I10)
        NK(J)=NK(J)+1
        IF (NK(J).GT.5) STOP 'GWELLEN: ANZAHL DER KNOTEN > 5 !'
C
C       PRINT '(T2,A,I5)','NK(J) = ',NK(J)
C
        DO 90 N1=1,NK(J)
C
C         Zuweisung des HYDRAULIK-Knotens
C
          JL=JL+1
          IF (JL.GT.IPSED) THEN
            PRINT '(T2,A,I5)','Gew„sserknoten = ',K(J,N1)
            PRINT '(T2,2(A,I5))','in Datensatz J = ',J,' von ',NKMAX
            PRINT '(T2,2(A,I5))','JL = ',JL,' > IPSED = ',IPSED
            STOP 'GWELLEN: Feldberschreitung!'
          END IF
C
C         Schleife ber alle bisher zugewiesenen Knoten
C
C         PRINT '(T2,A,I5)','JLAT = ',JLAT
C
          DO 95 JP=1,JLAT
            IF (L1E(JP).EQ.K(J,N1)) THEN
C             HYDRAULIK-Knoten wurde schon zugewiesen
              IFLAG(JP)=IFLAG(JP)+1
              JLDIF(JL)=JL-JLAT
              GOTO 90
            END IF
   95     CONTINUE
C
C         neue Zuweisung erfolgt
C
          JLAT=JLAT+1
          JLDIF(JL)=JL-JLAT
          IF (JLAT.GT.IPLAT) THEN
            PRINT '(T2,A,I3,A)','JLAT = ',JLAT,' > IPLAT !'
            STOP
          END IF
          L1E(JLAT)=K(J,N1)
          IFLAG(JLAT)=0
          LATCOM(JLAT)=3
          IF (JLAT.GT.JLMAX) JLMAX=JLAT
C
   90   CONTINUE
C
  100 CONTINUE
      PRINT '(T2,A,I5)','JLMAX = ',JLMAX
      JLMAX=0
C
      ID=0
  102 ID=ID+1
      READ(21,101,END=8) DATLEAK(ID),FORMDL(ID),MODVAR(ID)
      WRITE(*,'(T2,A50,A5,5X,A5)') DATLEAK(ID),FORMDL(ID),MODVAR(ID)
  101 FORMAT(A50,A5,5X,A5)
C
      GOTO 102
C
    8 CLOSE(21)
C
      IDMAX=ID-1
      QFAK=1./REAL(IDMAX)
      PRINT *
      PRINT'(T2,A,I2)','  ANZAHL DER LEA-DATEIEN  = ',IDMAX
      PRINT *
      PRINT *
      PRINT'(T2,A,F6.4)','  GEWICHTUNGSFAKTOR  QFAK = ',QFAK
      PRINT *
C
      DO 110 I=1,NDSTP
         DO 110 J=1,IPLAT
            QQ(I,J)=0.
  110 CONTINUE
C
      OPEN (22,FILE='GWLEABIL.PLT',STATUS='UNKNOWN')
      WRITE(22,'(A)') '"TSTD" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"'
C
C     Einlesen der LEA-Datei
C
      QQQSUM=0.0
      ID=0
  103 ID=ID+1
C
      IF (FORMDL(ID).EQ.'     ') FORMDL(ID)='ASCII'
      PRINT *
      PRINT '(T2,A,A)','  DATEIFORMAT = ',FORMDL(ID)
      PRINT *
      PRINT '(T2,A,A)','  MODFLOW_TYP = ',MODVAR(ID)
      PRINT *
C
      IF (FORMDL(ID)(1:3).EQ.'ASC') THEN
         OPEN (21,FILE=DATLEAK(ID),STATUS='OLD')
         WRITE(*,*) 'LEA-DATEI ALS ASCII-DATEI ER™FFNET !'
         READ(21,*) NSTEPS
C        READ(21,56) NSTEPS
   56    FORMAT(I4)
      ELSE IF (FORMDL(ID)(1:3).EQ.'BIN') THEN
         OPEN (21,FILE=DATLEAK(ID),STATUS='OLD',FORM='UNFORMATTED')
         WRITE(*,*) 'LEA-DATEI ALS BINŽR-DATEI ER™FFNET !'
         READ(21) NSTEPS
      ELSE IF (FORMDL(ID)(1:3).EQ.'TRA') THEN
         OPEN (21,FILE=DATLEAK(ID),STATUS='OLD',ACCESS='TRANSPARENT',
     &         FORM='FORMATTED')
         WRITE(*,*) 'LEA-DATEI ALS TRANS-DATEI ER™FFNET !'
         READ(21) NSTEPS
      END IF
      WRITE(*,'(T2,A,I5)') 'NSTEPS = ',NSTEPS
C
      JLAT=0
C
C     Schleife ber Zeitschritte in der LEA-Datei
C
      DO 500 I=1,NSTEPS
C
CCC      IF (TW.GT.TMAX) GOTO 501
C
         DO IBB=1,IBMAX
            QLEA(I,IBB)=0.0
         END DO
C
         PRINT'(T2,3(A,I4))','GWELLEN: STEP ',I,'(',NSTEPS,') DATEI ',ID
C
         IF (FORMDL(ID)(1:3).EQ.'ASC') THEN
            READ(21,57) DTW(I)
   57       FORMAT(85X,F9.2)
            PRINT '(T2,A,F9.2)',' DTW(I) = ',DTW(I)
            PRINT*
            READ(21,58) NKN(I)
   58       FORMAT(I6)
            PRINT '(T2,A,I6)',' NKN(I) = ',NKN(I)
            PRINT*
         ELSE IF (FORMDL(ID)(1:3).EQ.'BIN') THEN
            READ(21) DTW(I)
            PRINT '(T2,A,F9.2)',' DTW(I) = ',DTW(I)
            READ(21) NKN(I)
            PRINT '(T2,A,I6)',' NKN(I) = ',NKN(I)
         END IF
C        Berechnung der Obergrenze des Zeitintervalls in [min]
         TUMIN=TW
         TW=TW+DTW(I)/60.
         TOMIN=TW
C        Berechnung der Mitte des Zeitintervalls in [h]
         TSTD=0.5*(TUMIN+TOMIN)/60.
C        Stress-Periods DTW, Umrechnung von [sec] in [h]
         DTW(I)=DTW(I)/3600.
C
         DO 190 IJ=1,NKMAX
            Z(IJ) = ZE(IJ)
  190    CONTINUE
C
C        Schleife ber Zeilen in der LEA-Datei
C
         PALT=0.0
         DO 195 JJ=1,NKN(I)

            IR=0
            IL=0
            IZ=0
            IS=0
            QQQ=0.0

C           Einlesen der Zeile in der LEA-Datei
            IF (FORMDL(ID)(1:3).EQ.'ASC') THEN
               READ(21,59,END=191) IR,IL,IZ,IS,QQQ
   59          FORMAT(47X,I4,8X,I3,6X,I4,6X,I4,7X,G15.11)
            ELSE IF (FORMDL(ID)(1:3).EQ.'BIN') THEN
               READ(21,END=191) IR,IL,IZ,IS,QQQ
            END IF
C
  191       PROZ=REAL(JJ)/REAL(NKN(I))*100.
            DPRO=PROZ-PALT
            IF (DPRO.GE.5.0) THEN
C              PRINT '(T2,A,F4.0,A)','GWELLEN: ',PROZ,' %'
               PALT=PROZ
            END IF
C
            JLAT=0
            JL=0
C
C           Schleife ber prim„re Knoten
C
            DO 401 IJ=1,NKMAX
              IF (IZ.EQ.Z(IJ).AND.IS.EQ.SP(IJ)) THEN
C
C               Schleife ber sekund„re Knoten (maximal 5)
C
                DO N1=1,NK(IJ)
                  JL=JL+1
                  IF (K(IJ,N1).EQ.0.AND.IB(IJ).EQ.0) THEN
                     PRINT '(T2,A,I5)','      IJ = ',IJ
                     PRINT '(T2,A,I5)','  IB(IJ) = ',IB(IJ)
                     PRINT '(T2,A,I5)',' K(IJ,1) = ',K(IJ,1)
                     PRINT '(T2,A,I5)','  NK(IJ) = ',NK(IJ)
                     PRINT '(T2,A,I5)','      N1 = ',N1
                     PRINT '(T2,A,I5)','K(IJ,N1) = ',K(IJ,N1)
                     PRINT '(T2,A,I5)','      JL = ',JL
                     STOP 'GWELLEN: K(IJ,N1) = 0 !'
                     STOP 'GWELLEN:   IB(IJ) = 0 !'
                  END IF
C
C                 Zuweisung der Leakag-Rate zum HYDRAULIK-Knoten
C
C                 Schleife ber alle bisher zugewiesenen Knoten
C
                  DO JP=1,JLAT
                    IF (L1E(JP).EQ.K(IJ,N1)) THEN
C
C                     Überlagerung am bereits zugewiesenen Knoten
C
                      IF (IFLAG(JP).EQ.0) THEN
                        PRINT '(T2,A,I5)','NK(IJ) = ',NK(IJ)
                        PRINT '(T2,A,I5)','    JL = ',JL
                        PRINT '(T2,A,I5)','  JLAT = ',JLAT
                        PRINT '(T2,A,I5)',' JLDIF = ',JLDIF(JL)
                        PRINT '(T2,A,I5)','    JP = ',JP
                        PRINT '(T2,A,I5)','   L1E = ',L1E(JP)
                        STOP ' IFLAG(JP) máte > 0 sein !'
                      END IF
C
                      QQ(I,JP)=QQ(I,JP)-QFAK*QQQ/REAL(NK(IJ))
                      IF (QQ(I,JP).GT.QQMAX(JP)) QQMAX(JP)=QQ(I,JP)
                      IF (QQ(I,JP).LT.QQMIN(JP)) QQMIN(JP)=QQ(I,JP)
C
                      IF (IB(IJ).GT.0) THEN
                        QLEA(I,IB(IJ))=QLEA(I,IB(IJ))+QQQ
                      END IF
                      QQQSUM=QQQSUM-QFAK*QQQ/REAL(NK(IJ))
                      GOTO 550
C
                    ENDIF
                  END DO
C
C                 Wertzuweisung an neuem Knoten
C
                  JLAT=JLAT+1
                  IF (JLAT.GT.JLMAX) JLMAX=JLAT
                  IF (L1E(JLAT).NE.K(IJ,N1)) THEN
                    PRINT '(T2,A,I5)','    IJ = ',IJ
                    PRINT '(T2,A,I5)','NK(IJ) = ',NK(IJ)
                    PRINT '(T2,A,I5)','    N1 = ',N1
                    PRINT '(T2,A,I5)','    JL = ',JL
                    PRINT '(T2,A,I5)',' JLDIF = ',JLDIF(JL)
                    PRINT '(T2,A,I5)','  JLAT = ',JLAT
                    PRINT '(T2,A,I5)','   L1E = ',L1E(JLAT)
                    PRINT '(T2,A,I5)','     K = ',K(IJ,N1)
                    STOP 'GWELLEN: KNOTEN NICHT DECKUNGSGLEICH !'
                  END IF
C
                  QQ(I,JLAT)=QQ(I,JLAT)-QFAK*QQQ/REAL(NK(IJ))
                  IF (QQ(I,JLAT).GT.QQMAX(JLAT)) QQMAX(JLAT)=QQ(I,JLAT)
                  IF (QQ(I,JLAT).LT.QQMIN(JLAT)) QQMIN(JLAT)=QQ(I,JLAT)
C
                  IF (IB(IJ).GT.0) THEN
                    QLEA(I,IB(IJ))=QLEA(I,IB(IJ))+QQQ
                  END IF
                  QQQSUM=QQQSUM-QFAK*QQQ/REAL(NK(IJ))

  550             CONTINUE

                END DO

                Z(IJ) = 0

                GOTO 195
C
               ELSE
C
                  JL = JL + NK(IJ)
                  JLAT = JL - JLDIF(JL)
C
               END IF
C
  401       CONTINUE
C
  195    CONTINUE
C
         WRITE(22,'(11F10.3)') TSTD,(QLEA(I,IBB),IBB=1,10)
C
  500 CONTINUE
C
      CLOSE (21)
C
      DO JL=1,JLMAX
         IF (QQMIN(JL).GT.999999.0) QQMIN(JL)=0.0
         IF (QQMAX(JL).LT.-999999.0) QQMAX(JL)=0.0
      END DO
C
      IF (ID.LT.IDMAX) GOTO 103
      PRINT *
      PRINT '(T2,A,F12.6)','Kontrollsumme QQQSUM = ',QQQSUM
      PRINT *
C
C 501 NSTEPS=I
C
      PRINT *
      PRINT '(T2,A,I5)','JLMAX = ',JLMAX
      PRINT *
      DO 210 J=1,JLMAX
      QQ(NSTEPS+1,J)=QQ(NSTEPS,J)
  210 CONTINUE
C
C     Umrechnung auf Zeitintervalle von HYDRET
C     TINC = Sttzweite der Zufluáganglinien in [min]
C
      IF (NSTEPS.GT.N) NSTEPS=N
      IJ=0
      DO 600 I=1,NSTEPS
      IF (ABS(DTW(I)).LT.0.0001) DTW(I)=1.0
      A1 = DTW(I)*60.
      A2 = ANINT(A1/TINC)
      IF (ABS(A2-A1/TINC).LT.0.0001) GOTO 10
      WRITE(N6,4) I,DTW(I),TINC,A1,A2
    4 FORMAT(//1X,'I = ',I5,'  DTW(I)= ',F7.2,' TINC= ',F7.2,' A1= ',
     & G11.3,' A2= ',G11.3)
      WRITE(N6,5) DATEI
    5 FORMAT(//1X,'UP GWELLEN  ',A30/1X,'ZEITSCHRITT  NICHT KOMPATIBEL'
     &//)
      STOP 5
   10 CONTINUE
      IA=INT(A2)
      DO 14 J=1,IA
      IJ=IJ+1
      DO 300 JK=1,JLMAX
      QLI(IJ,JK)=QLI(IJ,JK)+QQ(I,JK)+REAL(J-1)*(QQ(I+1,JK)
     1           -QQ(I,JK))/REAL(IA)
  300 CONTINUE
  14  CONTINUE
  600 CONTINUE
      IF(IJ.LT.N) THEN
      DO 17  I=IJ,N-1
      DO 16 JK=1,JLMAX
      QLI(I+1,JK)=QLI(I,JK)
  16  CONTINUE
  17  CONTINUE
      ENDIF
c
C     SCHLEIFE šBER ZEIGER FšR LATERALE ZUFLšSSE
      DO 20 JK=1,JLMAX
C        AUFSUMMIERUNG AM AKTUELLEN KNOTENZEIGER FšR ALLE ZEITSCHRITTE
         DO 23 I=1,N-1
            SQG(4,JK)=SQG(4,JK)+QLI(I,JK)*TINC*60.
  23     CONTINUE
C        PRINT '(T2,A,I5,A,F9.1)','L1E = ',L1E(JK),'  SQG = ',SQG(4,JK)
  20  CONTINUE
C
      CLOSE (22)
C
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE HYD(IGW,MHYD,NZ,KNO,SOHLE)

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)

C     PARAMETER (NDIM=2050)
      DIMENSION KNO(5,*),SOHLE(5,*)
      CHARACTER DATEI*50, TEXT*70

  901 FORMAT(A)

      JGW=0

C     EINLESEN DER SIMULATIONSDATEN, NETZDATEN UND KENNGROESSEN

      READ (mhyd,901) TEXT
      READ (mhyd,901) TEXT
      READ (mhyd,901) TEXT
      READ (mhyd,901) TEXT
      READ (mhyd,*) STIME,TOTH,DT,LEAD,TINC,ITUN,IPRO
      READ (mhyd,*) NL,NJUNC,NQIN,LATINF,NWEIRS,NGATES,NRET,NSTORE
      READ (mhyd,*) LIST,NWEL,DTWEL,IDOWN,TOL,CONVEC
      READ (mhyd,*) (KWELE,J=1,NWEL)

      NX = NL+1

C NIEDERSCHLAGSDATEN EINLESEN

      READ(mhyd,901) DATEI                                             
      IF (DATEI.EQ.'NORAIN') THEN                                      
      JPREC = 0                                                        
      SUMN = 0.0                                                       
      GOTO 41                                                          
      END IF                                                           
   41 NPREC = 0                                                       

C     EINLESEN DER ZUFLUSSWELLEN AN DEN EINTRITTSKNOTEN
C
      READ (mhyd,780) (NUPE,J = 1,NQIN)
C
C
      DO 120 J = 1,NQIN
         READ (mhyd,901) DATEI
  120 CONTINUE

      IF (NJUNC.EQ.0) GO TO 180
C
C      READ AND PRINT OUT JUNCTION DATA
C
      DO 160 J = 1,NJUNC
      READ (mhyd,901) Text
      READ (mhyd,790) (NXJE,K = 1,7),DXL,(GAM,L = 1,3)
  160 CONTINUE
C
C      DOWNSTREAM BOUNDARY CONDITIONS ARE READ
C
  180 GO TO (190,200,230,250), IDOWN
C
C      RATING EQUATION
C
  190 READ (mhyd,680) WB,WMUE,ZSILL,FLOW
C
      GO TO 270
C
C      RATING TABLE - STAGE VS DISCHARGE
C
  200 READ (mhyd,670) IPTS
  205 READ (mhyd,680) (QDON,YDON,J = 1,IPTS)
      GO TO 270
C
C      STAGE HYDROGRAPH
C
  230 READ(mhyd,901) DATEI
      GO TO 270
C
C      DISCHARGE HYDROGRAPH
C
  250 READ(mhyd,901) DATEI
  270 CONTINUE
C
C      READ LATERAL INFLOW DATA
C
      IF (LATINF.EQ.0) GO TO 340
      JA=1
  290 READ(mhyd,*) NAQ
      IF (NAQ.LE.0) GOTO 291
      IF (NAQ.GT.1000) THEN
         READ(mhyd,901) DATEI
         PRINT '(T2,A)', DATEI
C
C        CALL GWELLEN (DATEI,LEAD,TINC,JGW)
         JGW=0
C
         NAQ=NAQ-1000
         JA=JGW+1
         LATINF=LATINF+JGW
      ENDIF
      READ(mhyd,605) (TRANS,POR,BETA,J=1,NAQ)
  291 Continue
      DO 300 J = JA,LATINF
         READ (mhyd,600) L1E,LATCOM,DGW,DGW,IML,IMR,
     1                GHA,GHA,GS,GS,CLOSS,CLOSS,XGW
C
         GOTO (297,300,298) LATCOM+1
         GOTO 300
  297    READ(mhyd,901) DATEI
         GOTO 300
  298    READ (mhyd,903) SPK,SPN,ARED,QBASA,QBASE,QMAX
  903    FORMAT (6F10.3)
  300 CONTINUE
C
C      READ IN WEIR DATA
C
  340 CONTINUE

      IF (NWEIRS.EQ.0) GO TO 344
      DO 343 KWEI = 1,NWEIRS
      READ (mhyd,629) IWEIRE,HW,BW,WCO,WNEI
  343 CONTINUE
C
C      READ IN GATE DATA
C
  344 IF (NGATES.EQ.0) GO TO 390
      DO 360 KNOCK = 1,NGATES
       READ (mhyd,'(5X,I5,F10.0)') IGATEE,AGA
       IF(AGA.GE.0.) THEN
         READ(mhyd,901) DATEI
         ENDIF
  360 CONTINUE
  390 CONTINUE
C
C     READ CHANNEL GEOMETRY.
C
      READ (mhyd,*) SLO
C
      DO 400 I=1,NX
         READ (MHYD,685) IAB,ITYPE,WIDTH,HEIT,ZTR,ZTL,
     &                   XL,ZO,RNI,DZERO,QZERO,ZS,CKM
C
         DO 420 J=1,NZ
            IF (IAB.EQ.KNO(IGW,J)) THEN
               IF (ZO.GT.SOHLE(IGW,J)) SOHLE(IGW,J)=ZO
               GOTO 420
            ENDIF
  420    CONTINUE
  400 CONTINUE
C
      RETURN
C
  600 FORMAT (2I4,2F8.0,2I4,4F8.2,F4.2,F4.2,F8.0)
  605 FORMAT (F8.2,F8.2,F8.5)
  629 FORMAT (I10,4F10.2)
  670 FORMAT (16I5)
  680 FORMAT (8F10.0)
C 685 FORMAT (I4,I2,4F7.2,F7.0,3F7.2,F7.3,2F5.2)
  685 FORMAT (I4,I2,4F7.2,F7.0,3F7.2,F7.0,2F5.2)
  780 FORMAT (8I10)
  790 FORMAT (7I5,5X,4F10.3)
      END

C-----------------------------------------------------------------------
      SUBROUTINE HYDPR (ITIME,TIME,DT)
C     SUBROUTINE HYDPR (D2,Q2,ITIME)

      INCLUDE 'DIMENS.INC'

      COMMON /ERR/          TOL
      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /GEOMTY/       ITYPE(IPSEC), WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /QDD/          QZERO(IPSEC) ,DZERO(IPSEC)
      COMMON /RND/          HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)
      COMMON /BAK/          CRNBAK(IPSEC)
c     REAL Q2(IPSEC), D2(IPSEC)
      COMMON /Q2D2/ Q2(IPSEC), D2(IPSEC)
C
      COMMON /RELAX/  IRELAXFOR, IRELAXBACK, DHZUL, DHRELZUL, DVRELZUL
C
      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
C
      DHSFPOSMX = 0.0
      DHSFNEGMI = 0.0
      KNSFPOSMX = 0
      KNSFNEGMI = 0
C
C     aktuelle Zeit in Stunden
      TAK = (TIME+DT/60.)/60.

c     DO 100 I = 1,NX
      DO 100 I = NX,1,-1
       
         D(I) = D2(I)
         Q(I) = Q2(I)
C
C      FIND AREA,PERIMETER,TOP WIDTH, FRICTION SLOPE, AND VELOCITY
C      FOR EACH SECTION
C
         H(I) = ZO(I) + D(I)
         IF (WP(I).LT.0.0001) WP(I)=0.0001
         IF (AREA(I).LT.0.0001) AREA(I)=0.0001
C
         AALT=AREA(I)
         WALT=WP(I)
         TALT=T(I)
         RALT=AALT/WALT
         CALT=AALT*RALT**(2./3)
C
         CALL SHAPE2 (I,H(I),AREA(I),WP(I),T(I),CH2,CHR,QGRENZ)
C
         IF (WP(I).LT.0.0001) WP(I)=0.0001
         IF (AREA(I).LT.0.0001) AREA(I)=0.0001
C
         IF (ITIME.GT.1) THEN
            RNEU=AREA(I)/WP(I)
            CNEU=AREA(I)*RNEU**(2./3)
            CMIT=0.5*(CNEU+CALT)
            CDPR=ABS(CNEU-CALT)/ABS(CMIT)
            IF (CDPR.GT.0.50) THEN
               WRITE(25,'(T2,A,I5)') 'HYDPR KNOTEN ',IAB(I)
               WRITE(25,'(T2,A)') 'DÄMPFUNG VON C = A*RHY**2./3.'
C              neue Werte in SI-Einheiten umrechnen
               ANEU=ENGSI(AREA(I),2)
               WNEU=ENGSI(WP(I),1)
               RNEU=ANEU/WNEU
               CNEU=ANEU*RNEU**(2./3)
C              korrigierte Werte in engl. Einheiten
               AREA(I)=0.5*(AALT+AREA(I))
               WP(I)=0.5*(WALT+WP(I))
               T(I)=0.5*(TALT+T(I))
C              alte Werte in SI-Einheiten umrechnen
               AALT=ENGSI(AALT,2)
               WALT=ENGSI(WALT,1)
               RALT=AALT/WALT
               CALT=AALT*RALT**(2./3)
C              korrigierte Werte in SI-Einheiten
               AKOR=ENGSI(AREA(I),2)
               WKOR=ENGSI(WP(I),1)
               RKOR=AKOR/WKOR
               CKOR=AKOR*RKOR**(2./3)
C              Testausgabe
               WRITE(25,'(T2,A,F10.3)') 'CALT = ',CALT
               WRITE(25,'(T2,A,F10.3)') 'CNEU = ',CNEU
               WRITE(25,'(T2,A,F10.3)') 'CKOR = ',CKOR
            END IF
         END IF
C
         IF (AREA(I).LE.0. .OR. WP(I).LE.0.) THEN
            WRITE(*,98) IAB(I),ENGSI(H(I),1),ENGSI(AREA(I),2),
     1      ENGSI(WP(I),1)
   98       FORMAT(1X,'QUERSCHNITTSFEHLER AM KNOTEN ',I5,'  H= ',
     1      F7.2,'  A= ',F9.2,'  WP= ',F7.2)
            STOP
         EN DIF
C
         U(I) = Q(I)/AREA(I)
c
c        RNID=RNIVAR(RNI(I),H(I),HR0(I),HR1(I),HR2(I),RN1(I),RN2(I))
c        Funktionsaufrauf
c
         IF (KRAUT(I).GT.0) THEN
           RNID=RNV7IT(I,ITIME)
         ELSE
           RNID=RNV7(I)
         END IF
c
         SFALT=SF(I)
c
         RHY=AREA(I)/WP(I)
c
c        Limitierung der Geschwindigkeit U(I) auf UGRENZ*TOL
c        mittels Manipulation von CH2, CHR oder RNID
c
         UGRENZ = QGRENZ/AREA(I)
         UGRTOL = UGRENZ*TOL
         QGRTOL = QGRENZ*TOL
c
         IF (CH2.GT.0.0) THEN
c          mit Conveyance CH2 im HEC-2-Stil
C?         IF (SFALT.NE.0.0.AND.ABS(U(I)).GT.UGRTOL) THEN
           IF (ABS(SFALT).GE.0.000001.AND.ABS(U(I)).GT.UGRTOL) THEN
             IF (UGRTOL.GT.0.10) THEN
               CH2QGR=QGRTOL/SQRT(ABS(SFALT))
               IF (U(I).LT.0.0) THEN
                  U(I)=-UGRTOL
               ELSE
                  U(I)=UGRTOL
               END IF
               IF(I.NE.NX.AND.CH2QGR.LT.CRNBAK(I).AND.CH2QGR.LT.CH2)THEN
                  WRITE(25,*) IAB(I),'  UGRTOL = ',UGRTOL
                  WRITE(25,*) IAB(I),' CH2ALT = ',CRNBAK(I)
                  WRITE(25,*) IAB(I),'    CH2 = ',CH2
                  CH2=CH2QGR
               END IF
             END IF
           END IF
           CRNBAK(I)=CH2
           SF(I)=(U(I)*ABS(U(I))*AREA(I)**2.)/(CH2**2.)
C?       ELSE IF (SFALT.NE.0.0.AND.CHR.GT.0.0) THEN
         ELSE IF (ABS(SFALT).GE.0.000001.AND.CHR.GT.0.0) THEN
c          mit Conveyance CHR im HEC-RAS-Default-Stil
           IF (ABS(U(I)).GT.UGRTOL) THEN
             IF (UGRTOL.GT.0.10) THEN
               CHRQGR=QGRTOL/SQRT(ABS(SFALT))
               IF (U(I).LT.0.0) THEN
                  U(I)=-UGRTOL
               ELSE
                  U(I)=UGRTOL
               END IF
               IF(I.NE.NX.AND.CHRQGR.LT.CRNBAK(I).AND.CHRQGR.LT.CHR)THEN
                  WRITE(25,*) IAB(I),'  UGRTOL = ',UGRTOL
                  WRITE(25,*) IAB(I),' CHRALT = ',CRNBAK(I)
                  WRITE(25,*) IAB(I),'    CHR = ',CHR
                  CHR=CHRQGR
               END IF
             END IF
           END IF
           CRNBAK(I)=CHR
           SF(I)=(U(I)*ABS(U(I))*AREA(I)**2.)/(CHR**2.)
         ELSE
c          mit wasserstandsabh„ngigem Mannings n (RNID)
C?         IF (SFALT.NE.0.0.AND.ABS(U(I)).GT.UGRTOL) THEN
           IF (ABS(SFALT).GE.0.000001.AND.ABS(U(I)).GT.UGRTOL) THEN
             IF (UGRTOL.GT.0.10) THEN
               RQGR=(SQRT(ABS(SFALT))*1.486*RHY**(2./3.))/(UGRTOL)
               IF (U(I).LT.0.0) THEN
                 U(I)=-UGRTOL
               ELSE
                 U(I)=UGRTOL
               END IF
               IF (I.NE.NX.AND.RQGR.GT.CRNBAK(I).AND.RQGR.GT.RNID) THEN
                 WRITE(25,*) IAB(I),'  UGRTOL = ',UGRTOL
                 WRITE(25,*) IAB(I),' RNIDALT = ',CRNBAK(I)
                 WRITE(25,*) IAB(I),'    RQGR = ',RQGR
                 RNID=RQGR
               END IF
             END IF
           END IF
           CRNBAK(I)=RNID
           SF(I)=((RNID/(1.486*RHY**(2./3.)))**2.)*U(I)*ABS(U(I))
         END IF
c
c        DAEMPFUNG VON SF(I) DURCH MITTELUNG MIT SFALT, WENN
c        PROZENTUALE AENDERUNG DES REIBUNGSGEFAELLES IM ZEITSCHRITT
c        GROESSER ALS 50%
c
         IF (I.NE.NX) THEN

           IF (D(I).GT.0.0) THEN
             DHSFZUL = DHRELZUL * ENGSI(D(I),1)
             IF (DHSFZUL.LT.DHZUL) DHSFZUL=DHZUL
           ELSE
             DHSFZUL=DHZUL
           END IF
          
           DHSF = (SF(I)-SFALT)*DX(I)
           DHSF = ENGSI(DHSF,1)
           SFMIT=0.5*(SFALT+SF(I))
           IF (ABS(SFMIT).GT.0.000001) THEN
c            SFDPR=ABS(SF(I)-SFALT)/ABS(SFMIT)
c            IF (SFDPR.GT.0.50) THEN
             IF (ABS(DHSF).GT.DHSFZUL) THEN
               WRITE(25,'(T2,A,I5)') 'HYDPR KNOTEN ',IAB(I)
               WRITE(25,'(T2,A,F10.6)') '          DHSFZUL = ',DHSFZUL
               WRITE(25,'(T2,A,F10.6)') '1. Mittelung DHSF = ',DHSF
               SF(I)=SFMIT
c              2. Mittelung
               SFMIT=0.5*(SFALT+SF(I))
               DHSF=(SF(I)-SFALT)*DX(I)
               DHSF = ENGSI(DHSF,1)
               IF (ABS(DHSF).GT.DHSFZUL) THEN
                 WRITE(25,'(T2,A,F10.6)') '2. Mittelung DHSF = ',DHSF
                 SF(I)=SFMIT
               END IF
c              3. Mittelung
               SFMIT=0.5*(SFALT+SF(I))
               DHSF=(SF(I)-SFALT)*DX(I)
               DHSF = ENGSI(DHSF,1)
               IF (ABS(DHSF).GT.DHSFZUL) THEN
                 WRITE(25,'(T2,A,F10.6)') '3. Mittelung DHSF = ',DHSF
                 SF(I)=SFMIT
               END IF
               DHSF = (SF(I)-SFALT)*DX(I)
               WRITE(25,'(T2,A,F10.6)')   '          DHSFNEU = ',DHSF
             END IF
           END IF

           IF (DHSF.GT.0.001.AND.DHSF.GT.DHSFPOSMX) THEN
             DHSFPOSMX = DHSF
             KNSFPOSMX = IAB(I)
           END IF
           IF (DHSF.LT.-0.001.AND.DHSF.LT.DHSFNEGMI) THEN
             DHSFNEGMI = DHSF
             KNSFNEGMI = IAB(I)
           END IF

         END IF
c
         HC = H(I) + DHH
         CALL SHAPE (I,HC,AXY1,WP1,TC)
         HC = H(I) - DHH
         CALL SHAPE (I,HC,AXY2,WP2,TC)
         IF (ITYPE(I).EQ.2) T(I) = (AXY1 - AXY2)/DHT
         IF (T(I).LT.0.1) T(I) = 0.10
         DPYH(I) = (WP1 - WP2)/DHT
c
  100 CONTINUE
c
C      Ausgabe auf DHSFSTAT.dat
       WRITE(52,522) TAK,KNSFPOSMX,DHSFPOSMX,KNSFNEGMI,DHSFNEGMI
  522  FORMAT (F10.3,2(I10,F10.3))
c
c
      RETURN
      END
c=======================================================================
c     REAL FUNCTION RNIVAR (RNI,H,HR0,HR1,HR2,RN1,RN2)
c
c     IF (HR1.EQ.0.0) THEN
c        RNIVAR = RNI
c        RETURN
c     END IF
c
c     IF (H.LE.HR0) THEN
c        RNIVAR = RNI
c     ELSE IF (H.GT.HR0.AND.H.LE.HR1) THEN
c        RNIVAR = RNI + (RN1-RNI)/(HR1-HR0) * (H-HR0)
c     ELSE IF (H.GT.HR1.AND.H.LE.HR2) THEN
c        RNIVAR = RN1 + (RN2-RN1)/(HR2-HR1) * (H-HR1)
c     ELSE IF (H.GT.HR2) THEN
c        RNIVAR = RN2
c     END IF
c
c     RETURN
c     END
c=======================================================================
      REAL FUNCTION RNV7(I)

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /RND/     HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)
c
      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
c
      RNV7=0.0000
c
      IF (HR0(I).EQ.0.0.OR.HR(I,1).EQ.0.0) THEN
         HU=0.0
         HO=0.0
         RNU=0.0
         RNO=0.0
         RNV7 = RNI(I)
         RETURN
      END IF
c
      IF (H(I).LE.HR0(I)) THEN
         HU=HR0(I)
         HO=HR0(I)
         RNU=RNI(I)
         RNO=RNI(I)
         RNV7=RNO
      ELSE IF (H(I).GT.HR0(I).AND.H(I).LE.HR(I,1)) THEN
         HU=HR0(I)
         HO=HR(I,1)
         RNU=RNI(I)
         RNO=RNV(I,1)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,1).AND.H(I).LE.HR(I,2)) THEN
         HU=HR(I,1)
         HO=HR(I,2)
         RNU=RNV(I,1)
         RNO=RNV(I,2)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,2).AND.H(I).LE.HR(I,3)) THEN
         HU=HR(I,2)
         HO=HR(I,3)
         RNU=RNV(I,2)
         RNO=RNV(I,3)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,3).AND.H(I).LE.HR(I,4)) THEN
         HU=HR(I,3)
         HO=HR(I,4)
         RNU=RNV(I,3)
         RNO=RNV(I,4)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,4).AND.H(I).LE.HR(I,5)) THEN
         HU=HR(I,4)
         HO=HR(I,5)
         RNU=RNV(I,4)
         RNO=RNV(I,5)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,5).AND.H(I).LE.HR(I,6)) THEN
         HU=HR(I,5)
         HO=HR(I,6)
         RNU=RNV(I,5)
         RNO=RNV(I,6)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,6).AND.H(I).LE.HR(I,7)) THEN
         HU=HR(I,6)
         HO=HR(I,7)
         RNU=RNV(I,6)
         RNO=RNV(I,7)
         RNV7=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,7)) THEN
         HU=SIENG(999.999,1)
         HO=SIENG(999.999,1)
         RNU=RNV(I,7)
         RNO=RNV(I,7)
         RNV7=RNV(I,7)
      ELSE
         STOP 'RNV7 IN HYDPR: KEIN FALL ERFšLLT!'
      END IF
c
      IF (RNV7.EQ.0.0) THEN
         RNV7=RNI(I)
         IF (RNV(I,1).GT.0.0) RNV7=RNV(I,1)
         IF (RNV(I,2).GT.0.0) RNV7=RNV(I,2)
         IF (RNV(I,3).GT.0.0) RNV7=RNV(I,3)
         IF (RNV(I,4).GT.0.0) RNV7=RNV(I,4)
         IF (RNV(I,5).GT.0.0) RNV7=RNV(I,5)
         IF (RNV(I,6).GT.0.0) RNV7=RNV(I,6)
         IF (RNV(I,7).GT.0.0) RNV7=RNV(I,7)
      END IF
c
  111 FORMAT(A,I5,3(F8.3,F8.4))
c
      RETURN
      END
c=======================================================================
      REAL FUNCTION RNV7IT(I,IT)

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /RND/     HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)
c
      COMMON /NUMB/  LEAD
c
      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
c
      RNV7IT=0.0000
c
      IF (HR0(I).EQ.0.0.OR.HR(I,1).EQ.0.0) THEN
         HU=0.0
         HO=0.0
         RNU=0.0
         RNO=0.0
         RNV7IT = RNI(I)
c        RETURN
         GOTO 110
      END IF
c
      IF (H(I).LE.HR0(I)) THEN
         HU=HR0(I)
         HO=HR0(I)
         RNU=RNI(I)
         RNO=RNI(I)
         RNV7IT=RNO
      ELSE IF (H(I).GT.HR0(I).AND.H(I).LE.HR(I,1)) THEN
         HU=HR0(I)
         HO=HR(I,1)
         RNU=RNI(I)
         RNO=RNV(I,1)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,1).AND.H(I).LE.HR(I,2)) THEN
         HU=HR(I,1)
         HO=HR(I,2)
         RNU=RNV(I,1)
         RNO=RNV(I,2)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,2).AND.H(I).LE.HR(I,3)) THEN
         HU=HR(I,2)
         HO=HR(I,3)
         RNU=RNV(I,2)
         RNO=RNV(I,3)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,3).AND.H(I).LE.HR(I,4)) THEN
         HU=HR(I,3)
         HO=HR(I,4)
         RNU=RNV(I,3)
         RNO=RNV(I,4)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,4).AND.H(I).LE.HR(I,5)) THEN
         HU=HR(I,4)
         HO=HR(I,5)
         RNU=RNV(I,4)
         RNO=RNV(I,5)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,5).AND.H(I).LE.HR(I,6)) THEN
         HU=HR(I,5)
         HO=HR(I,6)
         RNU=RNV(I,5)
         RNO=RNV(I,6)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,6).AND.H(I).LE.HR(I,7)) THEN
         HU=HR(I,6)
         HO=HR(I,7)
         RNU=RNV(I,6)
         RNO=RNV(I,7)
         RNV7IT=RNU+(RNO-RNU)/(HO-HU)*(H(I)-HU)
      ELSE IF (H(I).GT.HR(I,7)) THEN
         HU=SIENG(999.999,1)
         HO=SIENG(999.999,1)
         RNU=RNV(I,7)
         RNO=RNV(I,7)
         RNV7IT=RNV(I,7)
      ELSE
         STOP 'RNV7IT IN HYDPR: KEIN FALL ERFšLLT!'
      END IF
c
  110 CONTINUE
c
      IF (RNV7IT.EQ.0.0) THEN
         RNV7IT=RNI(I)
         IF (RNV(I,1).GT.0.0) RNV7IT=RNV(I,1)
         IF (RNV(I,2).GT.0.0) RNV7IT=RNV(I,2)
         IF (RNV(I,3).GT.0.0) RNV7IT=RNV(I,3)
         IF (RNV(I,4).GT.0.0) RNV7IT=RNV(I,4)
         IF (RNV(I,5).GT.0.0) RNV7IT=RNV(I,5)
         IF (RNV(I,6).GT.0.0) RNV7IT=RNV(I,6)
         IF (RNV(I,7).GT.0.0) RNV7IT=RNV(I,7)
      END IF
c
      IF (KRAUT(I).GT.0) THEN
         DO K = 1,LEAD
            FAK(K) = FAKTKS(K,KRAUT(I))
         END DO
         FAKIT=Y(IT,FAK)
         IF (FAKIT.GT.0.0) THEN
            FAKIT=FAKIT/100.
            RNV7IT=RNV7IT/FAKIT
         END IF
      END IF
c
  111 FORMAT(A,I5,3(F8.3,F8.4))
c
      RETURN
      END

C-----------------------------------------------------------------------
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
      PRINT'(T2,A)','M”gliche Eingaben in Zeile 22 auf Pos. 19-20:'
      PRINT *
      PRINT'(T2,A)','"VK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá erst wenn '
      PRINT'(T2,A)','       Wasserspiegel h”her als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá erst wenn '
      PRINT'(T2,A)','       Wasserspiegel h”her als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wird auf Bereich zwischen '
      PRINT'(T2,A)','       linkem und rechtem Hochpunkt reduziert !'
      PRINT *
      PRINT'(T2,A)','"VF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá von Anfang an !'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá von Anfang an !'
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

C-----------------------------------------------------------------------
      SUBROUTINE JUNCCO (LJ)
C
C      THIS SUBROUTINE SETS THE COEFFICIENTS AT JUNCTIONS
C      THE JUNCTION EQUATIONS ARE:
C            F1*Q1 + F2*Y1 + F3*Q3 + F4*Y3 = E2
C            G1*Q2 + G2*Y2 + G3*Q3 + G4*Y3 = E3
C            Q1 + Q2 - Q3 + P5*(Y1+Y2+Y3) = E1

      INCLUDE 'DIMENS.INC'

      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC),DSR(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /AB/           IAB(IPSEC)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) , XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /TIB/     DT      , TIME   , ITOTH, ITOTM, TOTH,JTIME
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /EQUA/         E1(IPJUN)   , F1(IPJUN)   , F2(IPJUN)   ,
     1        F3(IPJUN)   , F4(IPJUN)   , E2(IPJUN)   , G1(IPJUN)   ,
     2        G2(IPJUN)   , G3(IPJUN)   , G4(IPJUN)   , E3(IPJUN)   ,
     3        P5(IPJUN)   , FG1(IPJUN)  , FG2(IPJUN)  , FG3(IPJUN)  ,
     4        FG4(IPJUN)  , E23(IPJUN)
      I = NXJ(LJ,1)
      J = NXJ(LJ,3)
      K = NXJ(LJ,5)
      GAM1 = GAM(LJ,1)
      GAM2 = GAM(LJ,2)
      U1 = U(I)
      U2 = U(J)
      U3 = U(K)
      UA1 = ABS(U1)
      UA2 = ABS(U2)
      UA3 = ABS(U3)
      Z1 = ZO(I)
      Z2 = ZO(J)
      Z3 = ZO(K)
      DSDQ1 = DSFDQ(I)
      DSDQ2 = DSFDQ(J)
      DSDQ3 = DSFDQ(K)
      DSDY1 = DSFDY(I)
      DSDY2 = DSFDY(J)
      DSDY3 = DSFDY(K)
      A1 = AREA(I)
      A2 = AREA(J)
      A3 = AREA(K)
      T1 = T(I)
      T2 = T(J)
      T3 = T(K)
      D1 = D(I)
      D2 = D(J)
      D3 = D(K)
      SF1 = SF(I)
      SF2 = SF(J)
      SF3 = SF(K)
      Q1 = Q(I)
      Q2 = Q(J)
      Q3 = Q(K)
      XL1 = DX(I)
      XL2 = DX(J)
      FT4 = 0.0
C
C   NOTE:  WHEN ARJUN IS SET TO ZERO, THE STORAGE EFFECT OF THE JUNCTION
C          IS NOT CONSIDERED
C
C     ARJUN=T2*(XL2-XXL(LJ)) + (T1+T3)*XL1/2.
      ARJUN = 1.E - 10
      P5(LJ) = ARJUN/3./DT
      QL1 = (QOFLOW(I) + QLAT1(I)) * XL1
      QL2 = (QOFLOW(J) + QLAT1(J)) * XL2
      E1(LJ) =  - (QL1 + QL2) - P5(LJ) * (D1 + D2 + D3)
      F1(LJ) = (1. - GAM1)/GRAV * UA1/A1 - .5 * DSDQ1 * XL1
      F2(LJ) = 1. - (1. - GAM1)/GRAV * UA1 * U1/A1 * T1 - .5 * DSDY1 * X
     1L1
      F3(LJ) =  - 1./GRAV * UA3/A3 - .5 * DSDQ3 * XL1
      F4(LJ) =  - 1. + UA3 * U3/GRAV * T3/A3 - .5 * DSDY3 * XL1
      FGT1 = (1. - GAM1)/GRAV * U1 * (.5 * UA1 - Q1/A1 + UA1/A1 * T1 * D
     11)
      FGT2 = U3/GRAV * (.5 * UA3 - Q3/A3 + UA3/A3 * T3 * D3)
      FGT4 = SF1 - DSDQ1 * Q1 - DSDY1 * D1
      FGT3 = SF3 - DSDQ3 * Q3 - DSDY3 * D3
      E2(LJ) =  - Z1 - FGT1 + Z3 + FGT2 + .5 * XL1 * (FGT4 + FGT3)
C
      G1(LJ) = (1. - GAM2)/GRAV * UA2/A2 - .5 * DSDQ2 * XL2
      G2(LJ) = 1. - (1. - GAM2)/GRAV * UA2 * U2/A2 * T2 - .5 * DSDY2 * X
     1L2
      G3(LJ) =  - 1./GRAV * UA3/A3 - .5 * DSDQ3 * XL2
      G4(LJ) =  - 1. + UA3 * U3/GRAV * T3/A3 - .5 * DSDY3 * XL2
      GFT1 = (1. - GAM2)/GRAV * U2 * (.5 * UA2 - Q2/A2 + UA2/A2 * T2 * D
     12)
      GFT4 = SF2 - DSDQ2 * Q2 - DSDY2 * D2
      E3(LJ) =  - Z2 - GFT1 + Z3 + FGT2 + .5 * XL2 * (GFT4 + FGT3)
C
C     3RD INFLOWING BRANCH
C
      IJ = NXJ(LJ,6)
      IF (IJ.EQ.0) GO TO 100
      GAM4 = GAM(LJ,3)
      U4 = U(IJ)
      UA4 = ABS(U4)
      Z4 = ZO(IJ)
      DSDQ4 = DSFDQ(IJ)
      DSDY4 = DSFDY(IJ)
      A4 = AREA(IJ)
      T4 = T(IJ)
      D4 = D(IJ)
      SF4 = SF(IJ)
      Q4 = Q(IJ)
      XL4 = DX(IJ)
C     ARJUN=ARJUN+(T4*(XL4-XXL(LJ)))
      P5(LJ) = ARJUN/4./DT
      QL4 = (QOFLOW(IJ) + QLAT1(IJ)) * XL4
      E1(LJ) =  - (QL1 + QL2 + QL4) - P5(LJ) * (D1 + D2 + D3 + D4)
      FG1(LJ) = (1. - GAM4)/GRAV * UA4/A4 - .5 * DSDQ4 * XL4
      FG2(LJ) = 1. - (1. - GAM4)/GRAV * UA4 * U4/A4 * T4 - .5 * DSDY4 *
     1XL4
      FG3(LJ) =  - 1./GRAV * UA3/A3 - .5 * DSDQ3 * XL4
      FG4(LJ) =  - 1. + UA3 * U3/GRAV * T3/A3 - .5 * DSDY3 * XL4
      FT1 = (1. - GAM4)/GRAV * U4 * (.5 * UA4 - Q4/A4 + UA4/A4 * T4 * D4
     1)
      FT4 = SF4 - DSDQ4 * Q4 - DSDY4 * D4
      E23(LJ) =  - Z4 - FT1 + Z3 + FGT2 + .5 * XL4 * (FT4 + FGT3)
  100 CONTINUE
      IF(FT4.EQ.1245.4565) CALL GATCO
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE LATRL (HYDRO,NX,ITIME)
C
C      THIS SUBROUTINE DETERMINES THE LATERAL INFLOWS.
C      LATERAL INFLOW VALUE RETURNED (QLAT1) IS DISCHARGE PER FOOT OF
C      WIDTH AVERAGED OVER THE TIME PERIOD.
C      VALUE IN QINFLO IS TOTAL CFS AT END OF THE TIME PERIOD;
C      THIS VALUE IS PRINTED IN THE SUMMARY TABLE

      INCLUDE 'DIMENS.INC'

      COMMON /STEP/         DTSAVE
      COMMON /TIB/          DT     , TIME  , ITOTH, ITOTM, TOTH ,JTIME
      COMMON /AB/           IAB(IPSEC)
      COMMON /LIN/          QINFLO(IPSEC)
      COMMON /NUMB/         LEAD
      COMMON /QLH/          QLI(IPHYD,IPLAT)
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      DIMENSION             QL(IPHYD)
C
      CHARACTER*6 OVFMOD
      COMMON /OVFBIL/ OVFMOD,NOVF(IPSEC),IABOVF(IPSEC),IOVF(IPSEC),
     1                QOFLSN(100),NOVFMX,LIRE(IPSEC),OVFAN(IPSEC),
     2                OVFAUS(IPSEC),IOVFSTAT(IPSEC),QOMAX(IPSEC),
     3                QREGEL(IPSEC),TOVFAN(IPSEC),TOVFAUS(IPSEC)
C                     wird WSP > OVFAN wird QOFLOW() berechnet
C                     wird WSP wieder < OVFAUS wird QOFLOW() nicht berechnet
C-----------------------------------------------------------------------

      IF (LATINF.EQ.0.AND.OVFMOD(1:3).NE.'OVF') THEN
        RETURN
      ELSE IF (LATINF.EQ.0.AND.OVFMOD(1:3).EQ.'OVF') THEN
        DO N = 1,100
          QOFLSN(N) = 0.
        END DO
        DO I = 1,NX
          QLAT1(I) = 0.
          QINFLO(I) = 0.
        END DO
        DO I = 1,NX
          IF (IABOVF(I).GT.0) THEN
            QLAT1(IOVF(I))=QLAT1(IOVF(I))-QOFLOW(I)*DX(I)/DX(IOVF(I))
            QINFLO(IOVF(I))=QINFLO(IOVF(I))-QOFLOW(I)*DX(I)
          END IF
          QOFLSN(NOVF(I))=QOFLSN(NOVF(I))-QOFLOW(I)*DX(I)
        END DO
        TOVF=(TIME+DT/60.)/60.
        WRITE (50,'(F9.3,100F10.3)')TOVF,(ENGSI(QOFLSN(N),3),N=1,NOVFMX)
        RETURN
      ELSE IF (LATINF.NE.0.AND.OVFMOD(1:3).EQ.'OVF') THEN
        DO N = 1,100
          QOFLSN(N) = 0.
        END DO
        DO I = 1,NX
          QLAT1(I) = 0.
          QINFLO(I) = 0.
        END DO
        DO I = 1,NX
          IF (IABOVF(I).GT.0) THEN
            QLAT1(IOVF(I))=QLAT1(IOVF(I))-QOFLOW(I)*DX(I)/DX(IOVF(I))
            QINFLO(IOVF(I))=QINFLO(IOVF(I))-QOFLOW(I)*DX(I)
          END IF
          QOFLSN(NOVF(I))=QOFLSN(NOVF(I))-QOFLOW(I)*DX(I)
        END DO
        TOVF=(TIME+DT/60.)/60.
        WRITE (50,'(F9.3,100F10.3)')TOVF,(ENGSI(QOFLSN(N),3),N=1,NOVFMX)
      ELSE IF (LATINF.NE.0.AND.OVFMOD(1:3).NE.'OVF') THEN
        DO I = 1,NX
          QLAT1(I) = 0.
          QINFLO(I) = 0.
        END DO
      END IF
C
C      HYDRO IS UPSTREAM DISCHARGE FOR CURRENT TIME PERIOD
C
C     QLAST = HYDRO
C     IF (ITIME.GT.1) QLAST = C11(ITIME - 1,1)
C     FLOW = (HYDRO + QLAST) * 0.500
      DO 120 I = 1,NX
c        QLAT1(I) = 0.
c        QINFLO(I) = 0.
         DO 110 J = 1,LATINF
            IF (I.NE.L1(J)) GO TO 110
C
C      LATERAL INFLOW EQUAL CLOSS*UPSTREAM INFLOW
C
C           QLAT1(I) = (FLOW * CLOSS(J))/DX(I)
C           QINFLO(I) = HYDRO * CLOSS(J)
C           IF (CLOSS(J).NE.0.) GO TO 110
            IF (LATCOM(J).EQ.1) GO TO 105
C
C      SEITLICHE ZUFLUSSWELLE WIRD BERšCKSICHTIGT(KEINE BERECHNUNG)
C      IN QLI(K,J) STECKEN AUCH DIE LEAKAGE-RATEN AUS GWELLEN
C
            DO 100 K = 1,LEAD
  100       QL(K) = QLI(K,J)
            QLAT = Y(ITIME,QL)
            QLST = QLAT
            IF (ITIME.GT.1) QLST = Y(ITIME - 1,QL)
            DQLAT = QLAT - QLST
            QLAT1(I) = QLAT1(I) + (2*QLST+DQLAT*(DT/DTSAVE))*.5/DX(I)
            QINFLO(I)= QINFLO(I) + QLST+DQLAT*(DT/DTSAVE)
            GO TO 110
C
C     BERECHNUNG DES SEITLICHEN ZUFLUSSES UEBER EIN GRUNDWASSERMODELL
C     (ZU- BZW. ABFLUSS IST KONSTANT UEBER DAS GANZE ZEITINTERVALL)
C        QLAT1 IST IN CBM/SEC/M , DAHER UMRECHNUNG MIT SIENG(...,2)
C        QINFLO IST GESAMTE ZUFLUSSMENGE IN CBM/SEC
C
  105       QLAT1  (I) = SIENG(GWFLOW(J),2)
            QINFLO (I) = QINFLO(I)+QLAT1 (I) * DX (I)

  110    CONTINUE
  120 CONTINUE
      RETURN
C
            END
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

C
C      THIS FUNCTION SETS THE DOWNSTREAM DISCHARGES WHEN
C      USED AS A BOUNDARY CONDITION.
      REAL FUNCTION QDOWN(ITIME)

      INCLUDE 'DIMENS.INC'

      COMMON /FLOWS/        NQIN     , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        QNX(IPHYD)
      COMMON /AB/           IAB(IPSEC)
      QDOWN = Y(ITIME,QNX)
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION HDOWN(ITIME)
C
C      THIS FUNCTION SETS THE DOWNSTREAM STAGES WHEN USED AS
C      A BOUNDARY CONDITION

      INCLUDE 'DIMENS.INC'

      COMMON /STG/          DSTAGE(IPHYD)
      COMMON /AB/           IAB(IPSEC)
      HDOWN = Y(ITIME,DSTAGE)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE LICTEST
      LOGICAL EXLICC, EXLICY
      INQUIRE (FILE='C:\EXE\LICENCE.INF',EXIST=EXLICC)
      INQUIRE (FILE='Y:\EXE\LICENCE.INF',EXIST=EXLICY)
      IF (EXLICC) THEN
         RETURN
      ELSE IF (EXLICY) THEN
         RETURN
      ELSE
         STOP 'Sie haben keine Lizenz für HYDRET06!'
      END IF
      END

C-----------------------------------------------------------------------
      SUBROUTINE OUTPT (ITIME)
C
C      THIS SUBROUTINE CALCULATES OVERFLOWS AND PRINTS RESULTS FROM
C      THE ROUTING COMPUTATIONS

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /NUMB/         LEAD
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /AB/           IAB(IPSEC)
      COMMON /LIN/          QINFLO(IPSEC)
      COMMON /TIB/          DT    , TIME , ITOTH, ITOTM,TOTH,JTIME
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) , XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /GATOLD/       GOLD(IPGAT)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /NEWER/        QLAT1(IPSEC) , QDUM(IPSEC), DSR(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /FLOWS/        NQIN   , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /PRECIP/       PE(IPPRC),NPREC,DTPREC
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /GWOUT/ XHGW(IPSEC),IS(IPSEC),IXGW(IPSEC)
      COMMON /JUNC/         JUNNAM(IPJUN)
      COMMON /GANGLT/ TINC,TTEST,TPRIN
C
C&&&  =================================================================
      INTEGER   FALL(IPSEC)
      REAL DELTAZ(IPSPE), ZOLD(IPSPE), VOLD(IPSPE)
      REAL KOR1,KOR2,KOR3
      REAL DUEBFI(IPSEC), DISK(IPSEC)
      REAL Q1(IPHYD)
      LOGICAL   SCHALT, RETKN, AUSBORD

      LOGICAL   EXKSMINMAX
      REAL RKSMIN(IPSEC),RKSMAX(IPSEC),RKSAKT(IPSEC)
      INTEGER IABKS(IPSEC)

      COMMON / VORSCH / IPRO
      COMMON / RET1/    NSPE(IPSEC,IPSPE), NSP(IPSEC,IPSPE), NRET,
     1                  NSTORE, NWERTE(IPSPE),ZETA(IPSPE),DGRENZ(IPSPE),
     2                  NVERB(IPSEC), INDEX(IPSEC)
      COMMON / RET2 /  ZSP(IPRAT,IPSPE), SP(IPRAT,IPSPE) ,VSTART(IPSPE)
      COMMON / RET3 /  VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
      COMMON / RET4 /   KOR1(IPSEC), KOR2(IPSEC), KOR3(IPSEC)
      COMMON / RETLOG /   SCHALT(IPSEC), RETKN(IPSEC), AUSBORD(IPSEC)
      COMMON / QRET/ QOX(IPSPE,IPHYD),QEXT(IPHYD,IPSPE),
     &               MODEXT(IPSPE),IEXCL(IPSPE),QLEA(IPHYD,IPSPE),
     &               QUESP(IPSPE,IPHYD)
      COMMON / HALTU / DVHNEU(IPSEC),DVHALT(IPSEC)
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
C&&&  ==================================================================
C
      REAL QOFLOW(IPSEC)
      CHARACTER*40   JUNNAM
      CHARACTER*1 ZEI
      DATA OVF/0./
C
C-----------------------------------------------------------------------
C
      CHARACTER*6 OVFMOD
      COMMON /OVFBIL/ OVFMOD,NOVF(IPSEC),IABOVF(IPSEC),IOVF(IPSEC),
     1                QOFLSN(100),NOVFMX,LIRE(IPSEC),OVFAN(IPSEC),
     2                OVFAUS(IPSEC),IOVFSTAT(IPSEC),QOMAX(IPSEC),
     3                QREGEL(IPSEC),TOVFAN(IPSEC),TOVFAUS(IPSEC)
C                     wird WSP > OVFAN wird QOFLOW() berechnet
C                     wird WSP wieder < OVFAUS wird QOFLOW() nicht berechnet
C-----------------------------------------------------------------------
C
C
      COMMON /SOHLE/   ZSHIFT(IPSEC)

      DOUBLE PRECISION XK(IPSEC),YK(IPSEC)
      COMMON /XYKOORD/ XK,YK

      COMMON /GEWID/ ID(IPSEC)

      COMMON /RND/          HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)

      CHARACTER*300 C300
C     Hilfsstring für WRITECSV
      CHARACTER*4096 CL

      TTEST=TIME/TINC
      TPRIN=ANINT(TTEST)
      WRITE(25,'(A,F10.5)') 'TIME = ',TIME/60.
      WRITE(25,'(A,F10.5)') 'TTEST = ',TTEST
      WRITE(25,'(A,F10.5)') 'TPRIN = ',TPRIN
      IF (ABS(TTEST-TPRIN).LT.0.001) THEN
         WRITE(25,'(A,F8.2,A)') 'TIME = ',TIME/60.,' AUSGABE AKTIV !'
      END IF
C
      DTM = DT/60.
      DPM = DTPREC * 60.
C     AA=AMOD(DPM,DTM)
C     IF (IA.EQ.0) AA = 0.0
      AA=DPM/DTM
      IA=INT(DPM/DTM)
      REST=AA-REAL(IA)
C     WRITE(N6,'(A,F8.3)') '  AA = ',AA
C     WRITE(N6,'(A,I2)') '  IA = ',IA
C     WRITE(N6,'(A,F8.3)') 'REST = ',REST
C     IF(ABS(AA).LT.0.0001) GOTO 90
      IF(ABS(REST).LE.0.5.AND.ABS(REST).LT.0.0001) GOTO 90
      IF(ABS(REST).GT.0.5.AND.ABS(REST).GT.0.9999) GOTO 90
      WRITE(N6,91) DPM, DTPREC, DTM, DT, AA
 91   FORMAT(' ZEITSCHRITT DER NIEDERSCHLAGSWERTE  DPM =',F8.2,' MIN',
     1  ' ( DTPREC =',F8.2,' H )'/
     2       ' SIMULATIONSZEITSCHRITT              DTM =',F8.2,' MIN',
     3  ' ( DT     =',F8.2,' SEC )'/' DPM MUSS EIN GANZZAHLIGES VIELFACH
     4ES VON DTM SEIN !'/' DIES IST HIER NICHT DER FALL, DENN ES BLEIBT
     5EIN DIVISIONSREST VON AA =',F8.3)
      STOP 91
 90   CONTINUE
C
      DO 100 I = 1,NX
C        QOFLOW(I) = 0.0
C
C&&&  =================================================================
C
         FALL (I) = -1
         DSR(I)   = 0.0
         DUEBFI(I)= 0.0
         DISK(I)  = 0.0
         KOR1(I)  = 0.0
         KOR2(I)  = 0.0
         KOR3(I)  = 0.0

C&&&  =================================================================
C
  100 CONTINUE


      DO 125 J=1,NSTORE
C
C        externe Zuflsse zum Speicher
C
         DO 124 L=1,LEAD
            Q1(L) = QEXT(L,J)
  124    CONTINUE
         IF (ITIME.EQ.1) THEN
C           Anfangsfllung
            VRET(J)=VSTART(J)
C
            QX1= Y(ITIME,Q1)
            VRET(J)=VRET(J)+0.5*QX1*DT
            IF (VRET(J).LT.0.0) THEN
               VRET(J)=0.0
               QX1=0.0
            END IF
C           Speicherung fr Nettozufluáganglinie
            IF (MODEXT(J).EQ.0) QOX(J,ITIME)=-1.*QX1
         END IF
         IF (ITIME.GT.1) THEN
            QX1= Y(ITIME-1,Q1)
            QX2= Y(ITIME,Q1)
            VRET(J)=VRET(J)+0.5*(QX1+QX2)*DT
            IF (VRET(J).LT.0.0) THEN
               VRET(J)=0.0
               QX2=0.0
            END IF
C           Speicherung fr Nettozufluáganglinie
            IF (MODEXT(J).EQ.0) QOX(J,ITIME)=-1.*QX2
         END IF
C
C        Leakage-Rate in den Speicher
C
         DO 126 L=1,LEAD
            Q1(L) = QLEA(L,J)
  126    CONTINUE
         IF (ITIME.EQ.1) THEN
            QX1= Y(ITIME,Q1)
            VRET(J)=VRET(J)+0.5*QX1*DT
            IF (VRET(J).LT.0.0) THEN
               VRET(J)=0.0
               QX1=0.0
            END IF
C           Leakage-Raten in den Speicher werden
C           nicht zum Nettozufluá addiert, sie mssen
C           immer als separater Zufluá in SPI-Datei erscheinen,
C           da sie sonst doppelt gez„hlt werden !
         END IF
         IF (ITIME.GT.1) THEN
            QX1= Y(ITIME-1,Q1)
            QX2= Y(ITIME,Q1)
            VRET(J)=VRET(J)+0.5*(QX1+QX2)*DT
            IF (VRET(J).LT.0.0) THEN
               VRET(J)=0.0
               QX2=0.0
            END IF
C           Leakage-Raten in den Speicher werden
C           nicht zum Nettozufluá addiert, sie mssen
C           immer als separater Zufluá in SPI-Datei erscheinen,
C           da sie sonst doppelt gez„hlt werden !
         END IF
C
C        Wasserstand ZRET fr VRET
C        ZRET(J)   = WSTAND (VRET(J),J,DT,ITIME)
C
C        nur Wasserstandsermittlung
C        keine Korrektur von VRET(J) und QUESP()
C
         CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),0)
C
         IF (ITIME.EQ.1) THEN
            WRITE(25,4022)'SPEICHER ',J,'   VRET = ',ENGSI(VRET(J),3),
     &      '   ZRET = ',ENGSI(ZRET(J),1)
 4022       FORMAT(T2,A,I2,A,G11.5,A,G11.5)
         END IF
C
  125 CONTINUE
C
      COST = 0.00000
C
C *** GROSSE SCHLEIFE UEBER ALLE KNOTEN BEGINNT *******************
C
      DO 170 I = 1,NX

C        prüfen, ob absolut oder pro Breite
         QOFLOWVOR = QOFLOW(I)
         QOFLOW(I) = 0.0
C
C&&&  NACHFOLGENDE SPRUNGANWEISUNG VON 160 AUF 170 GEAENDERT!!
C
         IF (I.EQ.NX) GO TO 170
         IF (NJUNC.EQ.0) GO TO 130
         DO 120 JJ = 1,NJUNC
            IF (I.EQ.NXJ(JJ,1)) GO TO 110
            IF (I.EQ.NXJ(JJ,3)) GO TO 110
            IF (I.EQ.NXJ(JJ,6)) GO TO 110
            GO TO 120
  110       K = NXJ(JJ,5)
            GO TO 140
  120    CONTINUE
  130    K = I + 1
C
C####   DIE FOLGENDEN ZEILEN GEBEN DIE NEUE BERECHNUNG DES SEITLICHEN
C####   UEBERLAUFS WIEDER
C
  140    L = I - 1
         DO 141 JJ=1,NJUNC
            IF (I.NE.NXJ(JJ,5)) GO TO 141
            L = NXJ(JJ,1)
            GO TO 142
  141    CONTINUE
  142    DO 143 JJ=1,NQIN
            IF (I.EQ.NUP(JJ) ) L = I
  143    CONTINUE
C
C&&&     ==============================================================
C
         DXOB  = 0.0
         DXUN  = 0.0
         DSI   = 0.0
         DSK   = 0.0
C
C&&&     ===============================================================
C
C
         IF (RETKN(I))  THEN
            J = INDEX(I)
C           WASSERSTAND IM SPEICHER šBER GEWŽSSERSOHLE
c           DII = ZRET(J)-ZO(I)
c           DIK = ZRET(J)-ZO(K)
c           DIL = ZRET(J)-ZO(L)
            ZINP = 0.5 * (ZRET(J) + ZRETO(J))
            IF (ZINP.LT.(ZRET(J)-0.033)) ZINP = ZRET(J)
            DII = ZINP-ZO(I)
            DIK = ZINP-ZO(K)
            DIL = ZINP-ZO(L)
            IF (D(I).GT.DII) DII=D(I)
            IF (D(K).GT.DIK) DIK=D(K)
            IF (D(L).GT.DIL) DIL=D(L)
         ELSE
            DII=D(I)
            DIK=D(K)
            DIL=D(L)
         END IF
C
c        IF (D(I).LE.ZS(I)) GO TO 150
         IF (DII.LE.ZS(I)) GO TO 150
C
c        DSI = D(I) - ZS(I)
c        DSK = D(K) - ZS(K)
c        DSL = D(L) - ZS(L)
         DSI = DII - ZS(I)
         DSK = DIK - ZS(K)
         DSL = DIL - ZS(L)
         DSIK = DSI - DSK
         DSIL = DSI - DSL
         IF (ABS(DSIK).LE.1.E-10) DSIK = 1.E-10
         IF (ABS(DSIL).LE.1.E-10) DSIL = 1.E-10
C
C        DXUN : LAENGE DES UEBERSTROEMUNGSDREIECKS UNTERSTROM
C        DXOB : LAENGE DES UEBERSTROEMUNGSDREIECKE OBERSTROM
C
         DXUN = DSI * DX(I) / DSIK
         DXOB = DSI * DX(L) / DSIL
C
C  IN DEN FOLGENDEN VIER FAELLEN IST DIE UEBERFALLFLAECHE EIN TRAPEZ
C  - DIE UEBERFALLMENGE OBERHALB IST DANN BEREITS BERECHNET (KNOTEN L)
C  - DIE UEBERFALLMENGE UNTERHALB WIRD VOR ANW.NR 147 BERECHNET
C
         IF (DXUN.GT.DX(I)) DXUN = 0.
         IF (DXOB.GT.DX(L)) DXOB = 0.
         IF (DXUN.LT.0.) DXUN = 0.
         IF (DXOB.LT.0.) DXOB = 0.
         IF (L.EQ.I) DXOB = 0.

C     DSR(I) UND QOFLOW WERDEN NUR BERECHNET, WENN AUSBORDEN STATTFINDET
C     GOTO 150 ==> kein Ausborden!

       TIMEH = TIME/60.

       IF (OVFAN(I).GT.0.0001.AND.OVFAUS(I).GT.0.0001) THEN
          IF (TIMEH.GE.TOVFAN(I).AND.TIMEH.LE.TOVFAUS(I)) THEN
C            Steuerung z.B. auf Regelabfluss QREGEL möglich
             IF (IOVFSTAT(I).EQ.0) IOVFSTAT(I)=1
             DOVFAN = OVFAN(I) - ZO(I)
             DOVFAUS = OVFAUS(I) - ZO(I)
          ELSE
C            wegen Zeitfensterkriterium keine Steuerung möglich 
             IOVFSTAT(I)=0
             DOVFAUS = ZS(I)
             DOVFAN = 999999.9
          END IF
       ELSE
C         wegen Wasserstandskriterium keine Steuerung möglich 
          IOVFSTAT(I)=0
          DOVFAUS = ZS(I)
          DOVFAN = 999999.9
       END IF

       IF (IOVFSTAT(I).EQ.2.AND.D(I).LE.ZS(I)) THEN
          IOVFSTAT(I)=0
          WRITE(25,*) 'Knoten ',IAB(I),' QOFLOW gesteuert beendet!'
       ELSE IF (IOVFSTAT(I).EQ.2.AND.D(I).LE.DOVFAUS) THEN
          IOVFSTAT(I)=1
          QOFLOW(I) = 0.0
          QDUM(I) = QOFLOW(I)/DX(I)
          WRITE(25,*) 'Knoten ',IAB(I),' QOFLOW gesteuert beendet!'
          GO TO 170
       ELSE IF (IOVFSTAT(I).EQ.2.AND.D(I).GT.DOVFAUS) THEN
          IOVFSTAT(I)=2
          WRITE(25,*) 'Knoten ',IAB(I),' QOFLOW gesteuert geht weiter!'
       END IF
 
       IF (IOVFSTAT(I).EQ.1.AND.D(I).LE.ZS(I)) THEN
          IOVFSTAT(I)=0
       ELSE IF (IOVFSTAT(I).EQ.1.AND.D(I).LE.DOVFAN) THEN
          QOFLOW(I) = 0.0
          IOVFSTAT(I)=0
          QDUM(I) = QOFLOW(I)/DX(I)
          GO TO 170
       ELSE IF (IOVFSTAT(I).EQ.1.AND.D(I).GT.DOVFAN) THEN
          IOVFSTAT(I)=2
          WRITE(25,*) 'Knoten ',IAB(I),' QOFLOW gesteuert beginnt!'
       END IF

       IF (D(I).LE.ZS(I)) THEN
          QOFLOW(I) = 0.0
          QDUM(I) = QOFLOW(I)/DX(I)
          GO TO 150
       END IF

       IF (CKS(I).LT.0.0001) THEN
          QOFLOW(I) = 0.0
          QDUM(I) = QOFLOW(I)/DX(I)
          GO TO 150
       END IF
        

       IF (IOVF(I).GT.0) THEN
        HWS = ZO(I) + ZS(I)
        HUW = ZO(IOVF(I)) + D(IOVF(I))
        HOW = ZO(I) + D(I)
        QUO = (HUW-HWS)/(HOW-HWS)
        IF (QUO.LT.0.0) QUO = 0.0
        WRITE(25,*) 'Berechnung von QOFLOW(I)'
        WRITE(25,*) 'I,HWS,HOW,IOVF(I),HUW,QUO'
        WRITE(25,*) I,ENGSI(HWS,1),ENGSI(HOW,1),IOVF(I),ENGSI(HUW,1),QUO
C       BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
        XC=0.7
        IF (XC.GT.QUO) XC=QUO
        IF (XC.LT.0.4) XC=0.4
c       WRITE(25,*) 'XC = ',XC
        IF (QUO.LT.0.0) QUO=0.0
        IF (QUO.GT.1.0) QUO=1.0
        IF (QUO.GE.XC) THEN
           ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
           IF (ZC.GT.1.0) ZC=0.999
           CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
           IF (CC.LT.0.070) CC=0.070
        ELSE
           CC=1.0
        END IF
       ELSE
        QUO = 0.0
        CC = 1.0
       END IF

       QUOSIGN=1.0
       IF (IABOVF(I).LT.0) THEN
          J=-IABOVF(I)
          CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),0)
          WRITE(25,*) 'OUTPT: ZRET(J) = ',ENGSI(ZRET(J),1)
          HWS = ZO(I) + ZS(I)
          HUW = ZRET(J)
          HOW = ZO(I) + D(I)
          QUO = (HUW-HWS)/(HOW-HWS)
C         WRITE(25,*) 'OUTPT: QUO = ',QUO
C         BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
          XC=0.7
          IF (XC.GT.QUO) XC=QUO
          IF (XC.LT.0.4) XC=0.4
c         WRITE(25,*) 'XC = ',XC
          IF (QUO.LT.0.0) QUO=0.0
          IF (QUO.GT.1.0) THEN
             QUO=1.0
             QUOSIGN=-1.0
          END IF 
          IF (QUO.GE.XC) THEN
             ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
             IF (ZC.GT.1.0) ZC=0.999
             CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
             IF (CC.LT.0.070) CC=0.070
          ELSE
             CC=1.0
          END IF
       END IF


       DSR(I) = 0.5 * DSI

C???   QOFLOW(I) = - CKS(I) * CC * DSR(I)**1.5 * (DXUN+DXOB)

       QOFLOW(I) = - CKS(I) * CC * DSR(I)**1.5 * (DXUN+DXOB)*0.5

       QOFLOW(I) = QOFLOW(I) * QUOSIGN 

       IF (DXUN.GT.0.0) GO TO 147

       DSR(I) = 0.5 * (DSI + DSK)

       IF (DSR(I).LE.0.) THEN
          DSR(I) = 0.0
       ENDIF

       IF (DSR(I).GT.0.) THEN
          QOFLOW(I) = -CKS(I)*CC*DSR(I)**1.5*DX(I)*QUOSIGN + QOFLOW(I)
       END IF

       QOFLOW_L  = QOFLOW(L) / DX(L) 
       QOFLOW_I  = QOFLOW(I) / DX(I) 
       QOFLOW_M  = 0.5 * (QOFLOW_L + QOFLOW_I)
       QOFLOW(I) = QOFLOW_M * DX(I)

       IF (QOFLOWVOR.GT.0.0.OR.QOFLOW(I).GT.0.0) THEN
         WRITE(25,*) 'OUTPT:   IOVF(I) = ',IOVF(I)
         WRITE(25,*) 'OUTPT: IABOVF(I) = ',IABOVF(I)
         WRITE(25,*) 'OUTPT: Rückstaufaktor für QOFLOW(I) CC = ',CC
         WRITE(25,*) 'OUTPT: QOFLOWVOR = ',ENGSI(QOFLOWVOR,3)
         WRITE(25,*) 'OUTPT: QOFLOW(I) = ',ENGSI(QOFLOW(I),3)
       END IF

C      prüfen, ob QOFLOWVOR absolut oder pro Breite angegeben
C      vermutlich absolut wie QOFLOW an dieser Stelle
C      deshalb folgende Mittelung erlaubt

C???   QOFLOW(I) = 0.5 * (QOFLOWVOR + QOFLOW(I))
C???   IF (ABS(QOFLOW(I)).GT.ABS(QOFLOWVOR)) THEN
       IF (QOFLOW(I).GT.QOFLOWVOR) THEN
          QOFLOW(I) = 0.90 * QOFLOWVOR + 0.10 * QOFLOW(I)
       ELSE
          QOFLOW(I) = 0.5 * QOFLOWVOR + 0.5 * QOFLOW(I)
       END IF

C     DAS FOLGENDE DSR(I) STELLT EINE FIKTIVE UEBERFALLHOEHE FUER
C     KNOTEN I DAR; SIE WIRD IN COEF ZUR BERECHNUNG VON CS5 BENOETIGT

  147 IF (QOFLOW(I).GE.0.) GO TO 149

      ARG = ( - QOFLOW(I) / CKS(I) / DX(I) )
      IF (ARG.LT.0) THEN
         PRINT '(T2,A,I5)','   I = ',I
         PRINT '(T2,A,I5)',' IAB = ',IAB(I)
         PRINT '(T2,A,F10.3)',' CKS = ',CKS(I)
         PRINT '(T2,A,F10.3)','  DX = ',ENGSI(DX(I),1)
         STOP 'OUTPT LINE 277: ARG < 0 !'
      END IF
      DSR(I) = ARG ** (2./3.)
C --------------------------------------------------------------------
C    - DSI IST UEBERLAUFHOEHE BEI KNOTEN I (DSK BEIM UNTERSTROMIGEN,
C                                            DSL BEIM OBERSTROMIGEN)
C    - DSR(I) IST REPRAESENTATIVE UEBERLAUFHOEHE
C    - CKS IST KOEFFIZIENT FUER DIE UEBERLAEUFE
C    - QOFLOW IS A FLOW RATE OVER THE LENGTH OF THE SECTION.
C             OVERFLOWS ARE REPRESENTED AS NEGATIVE NUMBERS.
C --------------------------------------------------------------------
C     FOR PUMP SECTION, CALL THE PUMP FUNCTION.
  149    IF (ABS(ZS(I)).LE.0.00001) QOFLOW(I) = PUMP(DS,I)
  150    CONTINUE
  160    CONTINUE
C
C&&&  =================================================================
C
        SCHALT(I) = .TRUE.
C
        IF (RETKN(I))  THEN
C
C          Korrektur von VRET mit Žnderung des Haltungsvolumens,
C          wenn Haltung Bestandteil des Speichers und neuer
C          Wasserstand in Haltung nicht größer als Speicherwasserstand
C
           J = INDEX(I)
C
Ciex     IF (IEXCL(J).EQ.0) THEN
C
C          WENN IEXCL(J) = 1 LIEGT GEWŽSSER AUáERHALB DES SPEICHERS !
C          WENN IEXCL(J) = 0 LIEGT GEWŽSSER INNERHALB DES SPEICHERS !
C
Ciex       DVHNEU(I) = 0.0
Ciex       IF (I.LT.NX) THEN
C             zur Korrektur von VRET:
C             Volumen„nderung in der Haltung (echte Gr”áe !)
Ciex          IF (ITIME.GT.1) THEN
Ciex             DVHNEU(I) = 0.5*(T(I)+T(I+1))*DX(I)*(ZRET(J)-ZOLD(J))
Ciex          ELSE
Ciex             DVHNEU(I) = 0.0
Ciex          END IF
Ciex       ELSE
Ciex          DVHNEU(I) = 0.0
Ciex       END IF
c
Ciex       UFER = ZO(I) + ZS(I)
Ciex       VNEU = VRET(J)
c
Ciex       IF (ZRET(J).GE.UFER) THEN
Ciex          VNEU = VRET(J) + 0.5*(DVHALT(I)+DVHNEU(I))
Ciex          IF (VNEU.LT.0.0) VNEU = 0.0
c
c         WRITE(25,'(T2,A,I2)')'SPEICHER ',J
c         WRITE(25,'(T2,I5,A,F12.2)')IAB(I),'  ZOLD=',ENGSI(ZOLD(J),1)
c         WRITE(25,'(T2,I5,A,F12.2)')IAB(I),'  ZRET=',ENGSI(ZRET(J),1)
c         WRITE(25,'(T2,I5,A,F12.2)')IAB(I),'  VRET=',ENGSI(VRET(J),3)
c         WRITE(25,'(T2,I5,A,F12.2)')IAB(I),'DVHALT=',ENGSI(DVHNEU(I),3)
c         WRITE(25,'(T2,I5,A,F12.2)')IAB(I),'  VNEU=',ENGSI(VNEU,3)
c
Ciex       ELSE
Ciex          VNEU = VRET(J) + 0.5*DVHALT(I)
Ciex          DVHNEU(I) = 0.0
Ciex       END IF
C
Ciex       VRET(J)=VNEU
C
C          neuer Wert fr ZRET
Ciex       ZRET(J) = WSTAND (VRET(J),J,DT,ITIME)
C
Ciex     END IF
C
C          Berechnung der Ausbordungsrate QOFLOW absolut
C
           CALL RETENT (I,K,L,DXOB,DXUN,DSI,DSK,FALL(I),
     &                 QOFLOW(I),DUEBFI(I),DISK(I),QOFLOWVOR)
        END IF
C
c       WRITE(25,'(T2,I5,A,I3)')  IAB(I),'IOVFSTAT =',IOVFSTAT(I)
c       WRITE(25,'(T2,I5,A,F8.3)')IAB(I),' QOMAX =',ENGSI(QOMAX(I),3)
C
C       QOFLOW() auf maximal zulässige Ausleitung QOMAX() begrenzen
        IF (IOVFSTAT(I).GT.0.AND.QOMAX(I).GT.0.001) THEN
         QOMX = QOMAX(I)
         IF (QOFLOW(I).LT.0.0.AND.ABS(QOFLOW(I)).GT.QOMX) THEN
          QOFLOW(I) = -1.*QOMX
          WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'  QOMX =',ENGSI(-QOMX,3)
          WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'QOFLOW =',ENGSI(QOFLOW(I),3)
         END IF
        ELSE IF (IOVFSTAT(I).EQ.0.AND.QOMAX(I).GT.0.001) THEN
         QOFLOW(I) = 0.0
        END IF
C
c       WRITE(25,'(T2,I5,A,I3)')  IAB(I),'IOVFSTAT =',IOVFSTAT(I)
c       WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'QREGEL =',ENGSI(QREGEL(I+1),3)
C
C       QOFLOW() auf Regelanfluss unterhalb einstellen
C       DQREG = Betrag der seitlichen Ausleitung
        IF (IOVFSTAT(I).GT.0.AND.QREGEL(I+1).GT.0.001) THEN
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'  Q(I) =',ENGSI(Q(I),3)
         IF (QOFLOWVOR.LT.0.0) THEN
           DQVOR = -1.*QOFLOWVOR
         ELSE
           DQVOR = 0.0
         END IF
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),' DQVOR =',ENGSI(DQVOR,3)
         DQREG = 0.5 * (Q(I) - QREGEL(I+1) + DQVOR)
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),' DQREG =',ENGSI(DQREG,3)
         IF (QOMAX(I).GT.0.001.AND.DQREG.GT.QOMAX(I)) DQREG = QOMAX(I)
         IF (DQREG.GT.0.0.AND.QOFLOW(I).LT.0.0) THEN
C         Annahme: keine weiteren seitlichen Zuflüsse !
C         Regelabfluss unterhalb sicherstellen
          Q(I+1) = Q(I) - DQREG
          I2 = (I+1)*2
          XV(I2-1) = Q(I+1)
          WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'Q(I+1) =',ENGSI(Q(I+1),3)
C         QOFLOW(I) belegen
          DQREG = -1.*DQREG
C         hydraulisch mögliches QOFLOW wirkt als Limit
c         IF (ABS(QOFLOW(I)).LT.ABS(DQREG)) DQREG = QOFLOW(I)
C         hydrologisch vorgegebenes DQREG wirkt als Limit
c         IF (ABS(QOFLOW(I)).GT.ABS(DQREG)) QOFLOW(I) = DQREG
C         hydrologisch vorgegebenes DQREG wirkt zwangsweise !!
          QOFLOW(I) = DQREG
         ELSE
          DQREG = 0.0
         END IF
        ELSE
         DQREG = 0.0
        END IF
C
C       zeitliche Änderumng von QOFLOW(I) = DELTAQ bei Bedarf dämpfen
C
        DELTAQ = QOFLOW(I) - QDUM(I)*DX(I)
        QOFMIT = 0.5 * (QOFLOW(I) + QDUM(I)*DX(I))
        EPSDQ = 0.10*ABS(QOFMIT)
C       DQ-Genauigkeit = 100 l/s !!
        EPSDQMIN = SIENG(0.100,3)
        IF (EPSDQ.LT.EPSDQMIN) EPSDQ = EPSDQMIN
        IF (ABS(DELTAQ).GT.EPSDQ) THEN
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'QOFLOW =',ENGSI(QOFLOW(I),3)
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'QOFMIT =',ENGSI(QOFMIT,3)
         WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'DELTAQ =',ENGSI(DELTAQ,3)
         QOFLOW(I) = QOFMIT
        END IF
C
C       QOFLOW() ggf. nochmals auf maximal zulässige Ausleitung QOMAX() begrenzen
        IF (QOMAX(I).GT.0.001) THEN
         QOMX = QOMAX(I)
         IF (QOFLOW(I).LT.0.0.AND.ABS(QOFLOW(I)).GT.QOMX) THEN
          QOFLOW(I) = -1.*QOMX
          WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'  QOMX =',ENGSI(-QOMX,3)
          WRITE(25,'(T2,I5,A,F8.3)')IAB(I),'QOFLOW =',ENGSI(QOFLOW(I),3)
         END IF
        END IF
C
C
C       SEITLICHE ZUFLšSSE VON JUNCTIONS SUCHEN
C
        QJUN = 0.0
        DO JJ=1,NJUNC
           IF (I.EQ.NXJ(JJ,1)) THEN
              QJUN = Q(NXJ(JJ,3))
              IF (NXJ(JJ,6).GT.0) QJUN = QJUN + Q(NXJ(JJ,6))
           END IF
        END DO
C
C       FAKTOR, UM DEN DER ABFLUSS BEI AUSBORDEN VERRINGERT WERDEN DARF
        FAK = 0.80
C
        IF (I.LT.NX) THEN
           IF (Q(I).GT.0.0.AND.Q(I+1).GT.0.0) QOMX = Q(I)-FAK*Q(I+1)
           IF (Q(I).GT.0.0.AND.Q(I+1).LE.0.0) QOMX = Q(I)-Q(I+1)
           IF (Q(I).LE.0.0.AND.Q(I+1).LE.0.0) QOMX = FAK*Q(I)-Q(I+1)
           IF (Q(I).LE.0.0.AND.Q(I+1).GT.0.0) QOMX = 0.0
           QOMX = QOMX + QINFLO(I) + QJUN
C          maximal mögliche Volumenänderung pro dt
           DVDT = 0.5*(AREA(I)+AREA(I+1))*DX(I)/DT
C          Limitierung der Ausbordungsrate
           QOMX = QOMX + 0.1*DVDT
        END IF
        IF (QDUM(I).GT.0.0) QOMX = QOMX + QDUM(I)*DX(I)
        IF (QOMX.LT.0.0) QOMX = 0.0
        IF (QOFLOW(I).LT.0.0.AND.ABS(QOFLOW(I)).GT.QOMX) THEN
           QOFLOW(I) = -1.*QOMX
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),'   QI =',ENGSI(Q(I),3)
           IF (I.LT.NX) THEN
              WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' QI+1 =',ENGSI(Q(I+1),3)
           END IF
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' QJUN =',ENGSI(QJUN,3)
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' QINF =',ENGSI(QINFLO(I),3)
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' QDUM =',ENGSI(QDUM(I),3)
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' DVDT =',ENGSI(DVDT,3)
           WRITE(25,'(T2,I5,A,F7.2)')IAB(I),' QOMX =',ENGSI(QOMX,3)
        END IF
C&&&  =================================================================
C
C     COST IS THE OVERFLOW VOLUME SUMMED AT PARTICULAR TIME PERIOD
C        FOR *ALL* SECTIONS.
C     OVF IS THE RUNNING TOTAL OVER ALL TIME PERIODS UP TO THIS POINT.
C
C&&&     WEGFLIESSENDE (VERLORENE) WASSERMENGE WIRD AUFSUMMIERT
C
         IF (.NOT.RETKN(I))  THEN
C
           J = 0
           CUBFT = ( - (QOFLOW(I) + QDUM(I) * DX(I)) * .5) * DT
           COST = COST + CUBFT
C???       QDUM(I) = QOFLOW(I)/DX(I)
C
C&&&  =================================================================
C
C        SPEICHERZUFLUSS MIT POLENI-FORMEL
C
         ELSE IF (SCHALT(I))  THEN
           J = INDEX(I)
           VOLD(J) = VRET(J)
           VRET(J) = VRET(J) - 0.5 * (QOFLOW(I) + QDUM(I) * DX(I)) * DT
C
C        SPEICHERZUFLUSS MIT RETENTIONSGLEICHUNG
C
         ELSE
           J = INDEX(I)
           VOLD(J) = VRET(J)
C???       VRET(J) = VRET(J) - 0.5 * (QOFLOW(I) + QDUM(I)) * DX(I) * DT
           VRET(J) = VRET(J) - 0.5 * (QOFLOW(I) + QDUM(I) * DX(I)) * DT
C
C          LEERLAUFEN DES SPEICHERS AUFFANGEN
C
           IF (VRET(J).LE.0.0)  THEN
             QOFLOW(I) = 2.0 * VOLD(J)/(DX(I)*DT) - QDUM(I)
             VRET(J)   = 0.0
             KOR1(I)   = 0.0
             KOR2(I)   = 0.0
             KOR3(I)   = QOFLOW(I)
             DUEB(I)   = 0.0
             QOFLOW(I) = QOFLOW(I) * DX(I)
           END IF
C
C???       QDUM(I) = QOFLOW(I)
         END IF
C
C        NETTOZUFLUSSGANGLINIE ZUM SPEICHER
C
         IF (J.GT.0) THEN
            QOX(J,ITIME)=QOX(J,ITIME)+0.5*(QOFLOW(I)+QDUM(I)*DX(I))
         END IF
C
C        SPEICHERN DER AUSBORDUNGSRATE PRO šBERSTR™MBREITE QDUM(I)
C
C        QDUM(I) soll ist wohl den Zeitschritt vorher sichern !!?
C
C
         QDUM(I) = QOFLOW(I)/DX(I)

C        DVHALT(I) = DVHNEU(I)
C
C&&&  =================================================================
C
  170 CONTINUE
C
C&&&  *****   GROSSE SCHLEIFE BEENDET   ****************************
C
C&&&  =================================================================
C
      IF (NSTORE .EQ. 0)   GOTO 1730

C     Wasserstandsermittlung mit 2. Schätzung von QUESP(J,ITIME)
C     ohne Korrektur von VRET(J) gegen die Fließrichtung
c     DO J = 1,NSTORE
c       ZOLD(J)   = ZRET(J)
c
c       CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),0)
c        
c       DELTAZ(J) = ZRET(J) - ZOLD(J)
c     END DO

C     endgültige Wasserstandsermittlung
C     mit Korrektur von VRET(J) und Ermittlung von QUESP()

      DO J = NSTORE,1,-1

        ZOLD(J)   = ZRET(J)

        CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),1)
         
        IF (JSPUW(J).GT.0) THEN
C         Zufluss aus überlaufen von J in Speicher JJ = JSPUW einrechnen
          JJ = JSPUW(J)
          IF (ITIME.EQ.1) THEN
            QX1=QUESP(J,ITIME)
            VRET(JJ)=VRET(JJ)+DT*QX1
          END IF
          IF (ITIME.EQ.2) THEN
            QX1=QUESP(J,ITIME)
            QX2=QUESP(J,ITIME-1)
            VRET(JJ)=VRET(JJ)+DT*(QX1+QX2)/2.
          END IF
          IF (ITIME.GT.2) THEN
            QX1=QUESP(J,ITIME)
            QX2=QUESP(J,ITIME-1)
            QX3=QUESP(J,ITIME-2)
            VRET(JJ)=VRET(JJ)+DT*(QX1+QX2+QX3)/3.
          END IF
c          IF (ITIME.EQ.3) THEN
c            QX1=QUESP(J,ITIME)
c            QX2=QUESP(J,ITIME-1)
c            QX3=QUESP(J,ITIME-2)
c            VRET(JJ)=VRET(JJ)+DT*(QX1+QX2+QX3)/3.
c          END IF
c          IF (ITIME.EQ.4) THEN
c            QX1=QUESP(J,ITIME)
c            QX2=QUESP(J,ITIME-1)
c            QX3=QUESP(J,ITIME-2)
c            QX4=QUESP(J,ITIME-3)
c            VRET(JJ)=VRET(JJ)+DT*(QX1+QX2+QX3+QX4)/4.
c          END IF
c          IF (ITIME.GT.4) THEN
c            QX1=QUESP(J,ITIME)
c            QX2=QUESP(J,ITIME-1)
c            QX3=QUESP(J,ITIME-2)
c            QX4=QUESP(J,ITIME-3)
c            QX5=QUESP(J,ITIME-4)
c            VRET(JJ)=VRET(JJ)+DT*(QX1+QX2+QX3+QX4+QX5)/5.
c          END IF
          IF (VRET(JJ).LT.0.0) THEN
             VRET(JJ)=0.0
          END IF
        END IF
         
        DELTAZ(J) = ZRET(J) - ZOLD(J)
         
      END DO

C     abschließende Wasserstandswermittlung
C     mit Korrektur von VRET(J) infolge Überlaufens 
C
c      DO J = 1,NSTORE
c        ZOLD(J)   = ZRET(J)
c
c        CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),1)
c         
c        DELTAZ(J) = ZRET(J) - ZOLD(J)
c      END DO

      DO 1720   I = 1,NX
        IF (.NOT.RETKN(I))  GOTO 1720
        J = INDEX(I)
        KOR2(I) = KOR2(I) * DELTAZ(J)
 1720 CONTINUE
C
      TH=(TIME+DT/60.)/60.
      WRITE(53,1053) TH,(ENGSI(QUESP(J,ITIME),3),J=1,NSTORE)
      WRITE(54,1053) TH,(ENGSI(ZRET(J),1),ENGSI(ZRETO(J),1),J=1,NSTORE)
 1053 FORMAT (F9.3,100F10.3)
 1054 FORMAT (F9.3,200F10.3)
C
 1730 CONTINUE
C
C&&&  =================================================================
C
      OVF = OVF + COST

      IF (NRET.EQ.0) GOTO 1218
      IF (ITIME.EQ.1) THEN
Cdeb     WRITE(30,*) ((-1.*J),J,J=1,NSTORE)
         WRITE(30,1215) ((-J),J,J=1,20)
 1215    FORMAT ('99999.',20(4X,I4,4X,I4))
      ENDIF
         IF (ABS(TTEST-TPRIN).LT.0.001) THEN
Cdeb        WRITE (30,*) TIME/60.,(ENGSI(VRET(J),3)/1000.,
Cdeb &                   ENGSI(ZRET(J),1),J=1,NSTORE)
            WRITE (30,1217) TIME/60.,(ENGSI(VRET(J),3)/1000.,
     &                      ENGSI(ZRET(J),1),J=1,20)
 1217       FORMAT (F7.2,1X,20(F8.1,F8.3))
         END IF
 1218 CONTINUE

      IF (LIST.EQ.0) GO TO 180
      IF (ITIME.EQ.1.AND.LLIST.LE.1) RETURN
      IF (MOD((ITIME-1),LIST).NE.0) RETURN
      IF (IGRAPH.EQ.0) GO TO 180
C     CALL AGRAPH (XL,H)
C     HERE AN APPROPRIATE GRAPHING SUBROUTINE CAN BE INSERTED.
  180 CONTINUE
C
      JTIME = JTIME + 1
      IF (JTIME.EQ.1) WRITE (N6,270)
      IA = ITIME - 1
      PREP = 0.
      IDTM = INT(DTM)
      IF (IA.EQ.0) GO TO 171
      PREP = PE(IA)
  171 WRITE (N6,280) TIME,PREP,IDTM
C
C      PRINT OUT OVERFLOWS
C
      COST = ENGSI(COST,3)
      OVF = ENGSI(OVF,3)
      IF(OVF.GT.0.) WRITE (N6,220) COST,OVF
      COST = SIENG(COST,3)
      OVF = SIENG(OVF,3)
C
C      PRINT OUT GATE OPENINGS
C

      IF (NGATES.GT.0) GO TO 183
      GO TO 187
  183 DO 184 JIT=1,NGATES
  184 GATEL(JIT) = ENGSI(GATEL(JIT),1)
      WRITE (N6,230) (IGATEE(JIT),GATEL(JIT),JIT = 1,NGATES)
      DO 185 JIT=1,NGATES
  185 GATEL(JIT) = SIENG(GATEL(JIT),1)
  187 CONTINUE
c     IF (LATINF.GT.0) GO TO 188
      WRITE (N6,240)
      GO TO 189
c 188 WRITE (N6,241)
C
C      FIND FLOW PROPERTIES FOR EACH SECTION
C
  189 IF (JTIME.EQ.1) THEN
        OPEN (27,FILE='KSINVERS')
        OPEN (28,FILE='KSINVERS.dat',RECL=4096)
        IF (OVFMOD.EQ.'OVFBIL') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"KRAUT",'
            C300=C300(1:65)//'"DZERO",'
            C300=C300(1:73)//'"HZERO",'
            C300=C300(1:81)//'"QZERO",'
            C300=C300(1:89)//'"ZS",'
            C300=C300(1:94)//'"CKS",'
            C300=C300(1:100)//'"LIRE",'
            C300=C300(1:107)//'"NOVF",'
            C300=C300(1:114)//'"IABOVF",'
            C300=C300(1:123)//'"HR0",'
            C300=C300(1:129)//'"HR1",'
            C300=C300(1:135)//'"RNV1",'
            C300=C300(1:142)//'"HR2",'
            C300=C300(1:148)//'"RNV2",'
            C300=C300(1:155)//'"HR3",'
            C300=C300(1:161)//'"RNV3",'
            C300=C300(1:168)//'"HR4",'
            C300=C300(1:174)//'"RNV4",'
            C300=C300(1:181)//'"HR5",'
            C300=C300(1:187)//'"RNV5",'
            C300=C300(1:194)//'"HR6",'
            C300=C300(1:200)//'"RNV6",'
            C300=C300(1:207)//'"HR7",'
            C300=C300(1:213)//'"RNV7",'
            C300=C300(1:220)//'"SF",'
            C300=C300(1:225)//'"X",'
            C300=C300(1:229)//'"Y",'
            C300=C300(1:233)//'"ID",'
            C300=C300(1:238)//'"ZSHIFT"'
          ELSE
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"DZERO",'
            C300=C300(1:65)//'"HZERO",'
            C300=C300(1:73)//'"QZERO",'
            C300=C300(1:81)//'"ZS",'
            C300=C300(1:86)//'"CKS",'
            C300=C300(1:92)//'"LIRE",'
            C300=C300(1:99)//'"NOVF",'
            C300=C300(1:106)//'"IABOVF",'
            C300=C300(1:115)//'"HR0",'
            C300=C300(1:121)//'"HR1",'
            C300=C300(1:127)//'"RNV1",'
            C300=C300(1:134)//'"HR2",'
            C300=C300(1:140)//'"RNV2",'
            C300=C300(1:147)//'"HR3",'
            C300=C300(1:153)//'"RNV3",'
            C300=C300(1:160)//'"HR4",'
            C300=C300(1:166)//'"RNV4",'
            C300=C300(1:173)//'"HR5",'
            C300=C300(1:179)//'"RNV5",'
            C300=C300(1:186)//'"HR6",'
            C300=C300(1:192)//'"RNV6",'
            C300=C300(1:199)//'"HR7",'
            C300=C300(1:205)//'"RNV7",'
            C300=C300(1:212)//'"SF",'
            C300=C300(1:217)//'"X",'
            C300=C300(1:221)//'"Y",'
            C300=C300(1:225)//'"ID",'
            C300=C300(1:230)//'"ZSHIFT"'
          END IF
        ELSE IF (OVFMOD.EQ.'OVFREG') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"KRAUT",'
            C300=C300(1:65)//'"DZERO",'
            C300=C300(1:73)//'"HZERO",'
            C300=C300(1:81)//'"QZERO",'
            C300=C300(1:89)//'"ZS",'
            C300=C300(1:94)//'"CKS",'
            C300=C300(1:100)//'"LIRE",'
            C300=C300(1:107)//'"NOVF",'
            C300=C300(1:114)//'"IABOVF",'
            C300=C300(1:123)//'"OVFAN",'
            C300=C300(1:131)//'"OVFAUS",'
            C300=C300(1:140)//'"QOMAX",'
            C300=C300(1:148)//'"QREGEL",'
            C300=C300(1:157)//'"TOVFAN",'
            C300=C300(1:166)//'"TOVFAUS",'
            C300=C300(1:174)//'"HR0",'
            C300=C300(1:182)//'"HR1",'
            C300=C300(1:188)//'"RNV1",'
            C300=C300(1:195)//'"HR2",'
            C300=C300(1:201)//'"RNV2",'
            C300=C300(1:208)//'"HR3",'
            C300=C300(1:214)//'"RNV3",'
            C300=C300(1:221)//'"HR4",'
            C300=C300(1:227)//'"RNV4",'
            C300=C300(1:234)//'"HR5",'
            C300=C300(1:240)//'"RNV5",'
            C300=C300(1:248)//'"HR6",'
            C300=C300(1:253)//'"RNV6",'
            C300=C300(1:260)//'"HR7",'
            C300=C300(1:266)//'"RNV7",'
            C300=C300(1:273)//'"SF",'
            C300=C300(1:278)//'"X",'
            C300=C300(1:282)//'"Y",'
            C300=C300(1:286)//'"ID",'
            C300=C300(1:291)//'"ZSHIFT"'
          ELSE
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"DZERO",'
            C300=C300(1:65)//'"HZERO",'
            C300=C300(1:73)//'"QZERO",'
            C300=C300(1:81)//'"ZS",'
            C300=C300(1:86)//'"CKS",'
            C300=C300(1:92)//'"LIRE",'
            C300=C300(1:99)//'"NOVF",'
            C300=C300(1:106)//'"IABOVF",'
            C300=C300(1:115)//'"OVFAN",'
            C300=C300(1:123)//'"OVFAUS",'
            C300=C300(1:132)//'"QOMAX",'
            C300=C300(1:140)//'"QREGEL",'
            C300=C300(1:149)//'"TOVFAN",'
            C300=C300(1:158)//'"TOVFAUS",'
            C300=C300(1:168)//'"HR0",'
            C300=C300(1:174)//'"HR1",'
            C300=C300(1:180)//'"RNV1",'
            C300=C300(1:187)//'"HR2",'
            C300=C300(1:193)//'"RNV2",'
            C300=C300(1:200)//'"HR3",'
            C300=C300(1:206)//'"RNV3",'
            C300=C300(1:213)//'"HR4",'
            C300=C300(1:219)//'"RNV4",'
            C300=C300(1:226)//'"HR5",'
            C300=C300(1:232)//'"RNV5",'
            C300=C300(1:239)//'"HR6",'
            C300=C300(1:245)//'"RNV6",'
            C300=C300(1:252)//'"HR7",'
            C300=C300(1:258)//'"RNV7",'
            C300=C300(1:265)//'"SF",'
            C300=C300(1:270)//'"X",'
            C300=C300(1:274)//'"Y",'
            C300=C300(1:278)//'"ID",'
            C300=C300(1:283)//'"ZSHIFT"'
          END IF
        ELSE
          IF (KSTMOD.EQ.'KSTIME') THEN
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"KRAUT",'
            C300=C300(1:65)//'"DZERO",'
            C300=C300(1:73)//'"HZERO",'
            C300=C300(1:81)//'"QZERO",'
            C300=C300(1:89)//'"ZS",'
            C300=C300(1:94)//'"CKS",'
            C300=C300(1:100)//'"HR0",'
            C300=C300(1:106)//'"HR1",'
            C300=C300(1:112)//'"RNV1",'
            C300=C300(1:119)//'"HR2",'
            C300=C300(1:125)//'"RNV2",'
            C300=C300(1:132)//'"HR3",'
            C300=C300(1:138)//'"RNV3",'
            C300=C300(1:145)//'"HR4",'
            C300=C300(1:151)//'"RNV4",'
            C300=C300(1:158)//'"HR5",'
            C300=C300(1:164)//'"RNV5",'
            C300=C300(1:171)//'"HR6",'
            C300=C300(1:177)//'"RNV6",'
            C300=C300(1:184)//'"HR7",'
            C300=C300(1:190)//'"RNV7",'
            C300=C300(1:197)//'"SF",'
            C300=C300(1:202)//'"X",'
            C300=C300(1:206)//'"Y",'
            C300=C300(1:210)//'"ID",'
            C300=C300(1:215)//'"ZSHIFT"'
          ELSE
            C300='"IAB",'//'"ITYPE",'
            C300=C300(1:14)//'"WIDTH",'
            C300=C300(1:22)//'"HEIT",'
            C300=C300(1:29)//'"ZTR",'
            C300=C300(1:35)//'"ZTL",'
            C300=C300(1:41)//'"XL",'
            C300=C300(1:46)//'"ZO",'
            C300=C300(1:51)//'"RNI",'
            C300=C300(1:57)//'"DZERO",'
            C300=C300(1:65)//'"HZERO",'
            C300=C300(1:73)//'"QZERO",'
            C300=C300(1:81)//'"ZS",'
            C300=C300(1:86)//'"CKS",'
            C300=C300(1:92)//'"HR0",'
            C300=C300(1:98)//'"HR1",'
            C300=C300(1:104)//'"RNV1",'
            C300=C300(1:111)//'"HR2",'
            C300=C300(1:117)//'"RNV2",'
            C300=C300(1:124)//'"HR3",'
            C300=C300(1:130)//'"RNV3",'
            C300=C300(1:137)//'"HR4",'
            C300=C300(1:143)//'"RNV4",'
            C300=C300(1:150)//'"HR5",'
            C300=C300(1:156)//'"RNV5",'
            C300=C300(1:163)//'"HR6",'
            C300=C300(1:169)//'"RNV6",'
            C300=C300(1:176)//'"HR7",'
            C300=C300(1:182)//'"RNV7",'
            C300=C300(1:189)//'"SF",'
            C300=C300(1:194)//'"X",'
            C300=C300(1:198)//'"Y",'
            C300=C300(1:202)//'"ID",'
            C300=C300(1:207)//'"ZSHIFT"'
          END IF
        END IF

        WRITE(7,'(A)') 'Ausgabe von C300'
        WRITE(28,'(A)') C300

        EXKSMINMAX=.FALSE.
        INQUIRE (FILE='KSMINMAX.dat',EXIST=EXKSMINMAX)
        NKSMMX=0
        IF (EXKSMINMAX) THEN
          OPEN (67,FILE='KSMINMAX.dat')
          DO NKSMM=1,IPSEC
            READ(67,*,END=677) IABKS(NKSMM),RKSMIN(NKSMM),RKSMAX(NKSMM),
     1                         DZERO(NKSMM)
            DZERO(NKSMM) = SIENG (DZERO(NKSMM) ,1)
          END DO
  677     NKSMMX=NKSMM-1
          WRITE(*,*) 'KSMINMAX.dat: NKSMMX = ',NKSMMX
        END IF
      END IF
C
      IF (EXKSMINMAX) THEN
        REWIND 67
      END IF
C
      DO 210 I = 1,NX
         SFI = SF(I) * 1000.
         FROUDE = U(I)/SQRT(GRAV * AREA(I)/T(I))
         QOVF =  - QOFLOW(I)
         IF (NJUNC.EQ.0) GO TO 200
         DO 190 K = 1,NJUNC
  190    IF (I.EQ.NXJ(K,5)) WRITE (N6,250) JUNNAM(K)
  200    CONTINUE
         XL$     = ENGSI (XL    (I),1)
         Q$      = ENGSI (Q     (I),3)
         U$      = ENGSI (U     (I),1)
         AREA$   = ENGSI (AREA  (I),2)
         D$      = ENGSI (D     (I),1)
         T$      = ENGSI (T     (I),1)
         ZO$     = ENGSI (ZO    (I),1)
         H$      = ENGSI (H     (I),1)
         QINFL$  = ENGSI (QINFLO(I),3)
         QOVF$   = ENGSI (QOVF     ,3)
         ZEI = ' '
c
         IF (JTIME.EQ.1) THEN

            NSF=0

            IF (I.GT.1.AND.I.LT.NX) THEN
              H$O=ZO(I-1)+DZERO(I-1)
              H$M=ZO(I)+DZERO(I)
              H$U=ZO(I+1)+DZERO(I+1)
              IF (H$M.LT.H$U) THEN
                H$M=(H$O+H$M+H$U)/3.
                DZERO(I)=H$M-ZO(I)
              END IF
            END IF


            IF (I.LT.NX) THEN
              DX$=XL(I)-XL(I+1)
              DH$=ZO(I)+DZERO(I)-ZO(I+1)-DZERO(I+1)
              SF$=DH$/DX$
              IF (SF$.LT.0.00000001) SF$=0.00000001
              NSF=NSF+1
            ELSE
              SF$=0.00000001
            END IF

            IV=I-1
            IF (IV.GT.1.AND.IV.LT.NX) THEN
              DX$V=XL(IV)-XL(IV+1)
              DH$V=ZO(IV)+DZERO(IV)-ZO(IV+1)-DZERO(IV+1)
              SF$V=DH$V/DX$V
              IF (SF$V.LT.0.00000001) THEN
                SF$V=0.00000001
              ELSE
                NSF=NSF+1
                SF$=SF$+SF$V
              END IF
            ELSE
              SF$V=0.00000001
            END IF
             
            IN=I+1
            IF (IN.GT.1.AND.IN.LT.NX) THEN
              DX$N=XL(IN)-XL(IN+1)
              DH$N=ZO(IN)+DZERO(IN)-ZO(IN+1)-DZERO(IN+1)
              SF$N=DH$N/DX$N
              IF (SF$N.LT.0.00000001) THEN
                SF$N=0.00000001
              ELSE
                NSF=NSF+1
                SF$=SF$+SF$N
              END IF
            ELSE
              SF$N=0.00000001
            END IF

            IF (NSF.GT.0) THEN
              SF$=SF$/REAL(NSF)
            ELSE
              SF$=0.00000001
            END IF
             
         END IF
c
         QZERO$  = ENGSI (QZERO(I) ,3)
         DZERO$  = ENGSI (DZERO(I) ,1)
         WP$     = ENGSI (WP    (I),1)
         RHY$=AREA$/WP$
         IF (JTIME.EQ.1) THEN
            RKS$=(QZERO$/AREA$)/(RHY$**(2./3.)*ABS(SF$)**0.5)
         ELSE
            RKS$=(QZERO$/AREA$)/(RHY$**(2./3.)*ABS(SF(I))**0.5)
         END IF
c
         RHYENG=AREA(I)/WP(I)
         IF (JTIME.EQ.1) THEN
           RKSENG=(QZERO(I)/AREA(I))/(RHYENG**(2./3.)*ABS(SF$)**0.5)
         ELSE
           RKSENG=(QZERO(I)/AREA(I))/(RHYENG**(2./3.)*ABS(SF(I))**0.5)
         END IF
c
         IF (I.GT.1.AND.I.LT.NX) THEN
            QZERO$V = ENGSI (QZERO (I-1),3)
            WP$V    = ENGSI (WP    (I-1),1)
            AREA$V  = ENGSI (AREA  (I-1),2)
            RHY$V=AREA$V/WP$V
            RKS$V=(QZERO$V/AREA$V)/(RHY$V**(2./3.)*ABS(SF(I-1))**0.5)
c
            RHYENGV=AREA(I-1)/WP(I-1)
            RKSENGV=(QZERO(I-1)/AREA(I-1))/
     &      (RHYENGV**(2./3.)*ABS(SF(I-1))**0.5)
c
            QZERO$N = ENGSI (QZERO (I+1),3)
            WP$N    = ENGSI (WP    (I+1),1)
            AREA$N  = ENGSI (AREA  (I+1),2)
            RHY$N=AREA$N/WP$N
            RKS$N=(QZERO$N/AREA$N)/(RHY$N**(2./3.)*ABS(SF(I+1))**0.5)
c
            RHYENGN=AREA(I+1)/WP(I+1)
            RKSENGN=(QZERO(I+1)/AREA(I+1))/
     &      (RHYENGN**(2./3.)*ABS(SF(I+1))**0.5)
c
c           RKS$=(RKS$V+8.0*RKS$+RKS$N)/10.
c           RKSENG=(RKSENGV+8.0*RKSENG+RKSENGN)/10.
c
            RKS$=(RKS$V+2.0*RKS$+RKS$N)/4.
            RKSENG=(RKSENGV+2.0*RKSENG+RKSENGN)/4.
c
         ELSE IF (I.EQ.NX) THEN
            RKS$=RKS$N
            RKSENG=RKSENGN
         END IF
c
c
         IF (JTIME.EQ.1.AND.NKSMMX.GT.0) THEN
           DO NKSMM=1,NKSMMX
             IF (IABKS(NKSMM).EQ.IAB(I)) THEN
               IF (RKS$.LT.RKSMIN(NKSMM)) RKS$=RKSMIN(NKSMM)
               IF (RKS$.GT.RKSMAX(NKSMM)) RKS$=RKSMAX(NKSMM)
             END IF
           END DO
         END IF

         IF (RKS$.LT.5.0) RKS$=5.0
         IF (RKS$.GT.80.0) RKS$=80.0
c
         IF (RKS$.GT.0.0.AND.RKSENG.GT.0.0) THEN
            RMN$=1./RKS$
            RMNENG=1./RKSENG
         ELSE
            RKS$=25.0
            RMN$=1./RKS$
         END IF
c
c
  205    WRITE (N6,260) IAB(I),XL$,Q$,U$,AREA$,D$,T$,ZO$,
     1   H$,FROUDE,ZEI,SFI,QINFL$*1000.,QOVF$,RMN$,RKS$,RMNENG,RKSENG
c
         IF (JTIME.EQ.1) THEN
            CKS$     = 3./2. * CKS(I) / SQRT(2.*GRAV)
            WIDTH$   = ENGSI (WIDTH(I),1)
            HEIT$    = ENGSI (HEIT (I),1)
            ZS$      = ENGSI (ZS   (I),1)
            ZO$      = ENGSI (ZO   (I),1)
            HR$      = 0.0
            RNV$     = 0.0

            RKSAKT(I)=1./RMN$
c???        RNI(I)=RMN$

            WRITE (27,685) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1                     XL$,ZO$,RMN$,DZERO$,QZERO$,ZS$,CKS$,HR$,
     2                     HR$,RNV$,HR$,RNV$,HR$,RNV$,HR$,RNV$,
     3                     HR$,RNV$,HR$,RNV$,HR$,RNV$,SF$

            HZERO$=ZO$+DZERO$

c           XL$,ZO$,RMN$,DZERO$,HZERO$

         CALL CLEARCSV (CL,4096)

         IF (OVFMOD.EQ.'OVFBIL') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,693'
           WRITE(CL,693) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,KRAUT,DZERO$,HZERO$,
     2              QZERO$,ZS$,CKS$,LIRE(I),NOVF(I),IABOVF(I),
     3              HR$,(HR(I,N),RNV(I,N),N=1,7),SF$,XK(I),YK(I),
     4              ID(I),ZSHIFT(I)
          ELSE
           WRITE(CL,691) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,DZERO$,HZERO$,QZERO$,ZS$,CKS$,
     2              LIRE(I),NOVF(I),IABOVF(I),HR$,
     3              (HR(I,N),RNV(I,N),N=1,7),SF$,
     4              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (28,CL(1:LCL),LCL,',')
         ELSE IF (OVFMOD.EQ.'OVFREG') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,694'
           WRITE(CL,694) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,KRAUT,DZERO$,HZERO$,
     2              QZERO$,ZS$,CKS$,LIRE(I),NOVF(I),IABOVF(I),
     3              OVFAN(I),OVFAUS(I),QOMAX(I),QREGEL(I),TOVFAN(I),
     4              TOVFAUS(I),HR$,(HR(I,N),RNV(I,N),N=1,7),SF$,
     5              XK(I),YK(I),ID(I),ZSHIFT(I)
          ELSE
           WRITE(CL,692) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,DZERO$,HZERO$,QZERO$,ZS$,CKS$,
     2              LIRE(I),NOVF(I),IABOVF(I),OVFAN(I),OVFAUS(I),
     3              QOMAX(I),QREGEL(I),TOVFAN(I),TOVFAUS(I),
     4              HR$,(HR(I,N),RNV(I,N),N=1,7),SF$,
     5              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (28,CL(1:LCL),LCL,',')
         ELSE
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,689'
           WRITE(CL,689) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,KRAUT,DZERO$,HZERO$,
     2              QZERO$,ZS$,CKS$,HR$,
     3              (HR(I,N),RNV(I,N),N=1,7),SF$,XK(I),YK(I),ID(I),
     4              ZSHIFT(I)
          ELSE
           WRITE(CL,690) IAB(I),ITYPE(I),WIDTH$,HEIT$,ZTR(I),ZTL(I),
     1              XL$,ZO$,RMN$,DZERO$,HZERO$,QZERO$,ZS$,CKS$,
     2              HR$,(HR(I,N),RNV(I,N),N=1,7),SF$,
     3              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (28,CL(1:LCL),LCL,',')
         END IF

      
  685 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.4,F7.3,F7.2,F5.2,F5.3,
     &        F8.3,7(F8.3,F8.4),F12.8)
      
  689 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  690 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  691 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  693 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  692 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,6F8.2,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  694 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,6F8.2,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
c
         END IF
c
        IF (EXKSMINMAX) THEN
         WRITE (67,*) IABKS(I), RKSMIN(I), RKSMAX(I), ENGSI(DZERO(I),1),
     1                RKSAKT(I), ENGSI(XL(I),1)
        END IF
c
  210 CONTINUE
c
      IF (EXKSMINMAX) THEN
         CLOSE (67,STATUS='KEEP')
      END IF
c
      IF (JTIME.EQ.1) THEN
         CLOSE (27,STATUS='KEEP')
         CLOSE (28,STATUS='KEEP')
      END IF
C
C&&&  ==================================================================
C
      IF (NRET.EQ.0)  GOTO 1219
C
      WRITE (N6,211)
  211 FORMAT (//2X,'KNOTEN',2X,'FALL',2X,'QOFLOW {cbm/s}',3X,'DIFF {m}',
     1       3X,'DUEB {m}',2X,'RETKN',2X,'POLENI',3X,'DSR'/)
C
      DO 215  I =1,NX
         QOVF  = - QOFLOW(I)
         DREP  = DSR(I)
         DUEB1 = DUEB(I)
         DISK01 = DISK(I)
         DISK01 = ENGSI (DISK01,1)
         QOVF  = ENGSI (QOVF,3)
         DREP  = ENGSI (DREP,1)
         DUEB1 = ENGSI (DUEB1,1)
         WRITE (N6,212)  IAB(I), FALL(I), QOVF, DISK01, DUEB1,
     1                   RETKN(I), SCHALT(I), DREP
  212    FORMAT (2X,I5,3X,I3,3X,F10.3,3X,F8.3,6X,F8.3,4X,
     1          L1,7X,L1,5X,F7.3,1X)
  215 CONTINUE

  213 CONTINUE
C
      WRITE (N6,216)
  216 FORMAT (//2X,'SPEICHERFLAECHE',5X,'VRET {M**3}',5X,
     1       'ZRET {M+NN}'/)
      DO 219   J = 1,NSTORE
        VRET(J) = ENGSI (VRET(J),3)
        ZRET(J) = ENGSI (ZRET(J),1)
        WRITE (N6,217)  J, VRET(J), ZRET(J)
  217   FORMAT (7X,I5,10X,F10.1,6X,F10.3)
        VRET(J) = SIENG (VRET(J),3)
        ZRET(J) = SIENG (ZRET(J),1)
  219 CONTINUE
C
C&&&  =================================================================
C
 1219 WRITE (N6,270)
      RETURN
C
  220 FORMAT (//1X,'UEBERLAUFMENGE FUER DIESES ZEITINTERVALL :',G10.4,
     1   ' cbm'/1X,'GESAMTE BISHERIGE UEBERLAUFMENGE :        ',G10.4,
     2   'cbm'/)
  230 FORMAT (1X,'DURCHLASSOEFFNUNGEN',5(4(I5,F8.2,' M')/20X))
  240 FORMAT (/1X,
     1  ' KNO-   STATION   ABFLUSS   FLIESS-    FLAECHE  WASSER-   WASSE
     2R    SOHL-    WASSER-    FROUDE-  REIBUNGS- SEITLICHER UEBERLAUF-'
     3/'  TEN                         GESCHW.             TIEFE    SPIEG
     4EL-  HOEHE    SPIEGEL     ZAHL    GEFAELLE   ZUFLUSS     MENGE'/
     5 '                                                            BREI
     6TE             HOEHE'//
     7 '            {m}   {cbm/sec}   {m/sec}     {qm      {m}       {m}
     8     {m+NN}    {m+NN}      {-}    {X 1000}   {l/sec}   {cbm/sec}'/
     9   /133('=')/)
  241 FORMAT (/1X,
     1  ' KNO-   STATION   ABFLUSS   FLIESS-  REIBUNGS-  WASSER-   WASSE
     2R-   SOHL-    WASSER-     GW-   x= ABSTAND  SEITLICHER UEBERLAUF-'
     3/'  TEN                         GESCHW. GEFAELLE    TIEFE    SPIEG
     4EL-  HOEHE    SPIEGEL-  SPIEGEL    VOM       ZUFLUSS     MENGE'/
     5 '                                                            BREI
     6TE             HOEHE    BEI x   VORFLUTER '//
     7 '            {m}   {cbm/sec}   {m/sec}   {*1000}    {m}       {m}
     8     {m+NN}    {m+NN}    {m+NN}    {m}        {l/sec}   {cbm/sec}'/
     9   /133('=')/)
  250 FORMAT (1X,'--------------     ',A40,5X,68('-'))
  260 FORMAT (1X,I4,F10.0,F11.3,F10.2,F10.2,F9.2,
     1        F10.1,F10.2,F10.2,F10.3,A,F9.2,F10.2,F15.3,2(F10.4,F10.2))
  270 FORMAT (1X,132('*')/1H1)
  280 FORMAT (///133('*')/5X,'ZEIT =',F8.2,' min    ',30X,
     1' NIEDERSCHLAG = ',F6.2,' mm/',I3,' min'/132('*')/)
      END

C-----------------------------------------------------------------------
      SUBROUTINE POLENI (I,DALT,Q,HW,BW,WCO,WNEI,DNEU,QUO,CCFAK)
C
C     THIS SUBROUTINE CALCULATES THE POLENI-DEPTH FOR A WEIR
C
C     DNEU = neue Wassertiefe aus Poleni-Berechnung
C
C
      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
C
C======================================================================
C
C
      GRAV = 32.174
C
C   BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
      XC=0.7
      IF (XC.GT.QUO) XC=QUO
      IF (XC.LT.0.4) XC=0.4
      IF (QUO.LT.0.0) QUO=0.0
      IF (QUO.GT.1.0) QUO=1.0
      IF (QUO.GE.XC.AND.CCFAK.GT.0.0) THEN
         ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
         IF (ZC.GT.1.0) ZC=0.999
         CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
         CC=CC*CCFAK
         IF (CC.LT.0.070) CC=0.070
      ELSE
         CC=1.0
      END IF
      WRITE(25,*) 'UP POLENI: IAB = ',IAB(I),'  CC = ',CC
      WRITE(25,*) 'UP POLENI: IAB = ',IAB(I),' WCO = ',WCO
      WRITE(25,'(T2,A,F10.4)') 'UP POLENI: RUECKSTAUFAKTOR CC = ',CC
C
C     IF (WCO.LE.0.0.AND.CC.GT.0.001) THEN
      IF (WCO.LE.0.0.AND.CC.GT.0.070) THEN
         WRITE(25,'(T2,A)') 'UP POLENI: DNEU aus YGRENZ4'
         DMUEW = 0.57755775
         FAK=-WCO/DMUEW
         FAK=CC*FAK
         QQ=Q/FAK

         CALL YGRENZ4(I,QQ,DNEU,HMIN)
         DNEU = DNEU + HW
          
         WRITE(25,*) IAB(I),'WCO = ',WCO
         WRITE(25,*) IAB(I),'FAK = ',FAK
         IF (DNEU.LT.0) THEN
            WRITE(25,*) IAB(I),'DALT in  YGRENZ4 = ',ENGSI(DALT,1)
            WRITE(25,*) IAB(I),'DNEU aus YGRENZ4 = ',ENGSI(DNEU,1)
            DNEU=DALT
            WRITE(25,*) IAB(I),'DNEU aus YGRENZ4 = ',ENGSI(DNEU,1)
         END IF
         IF (DALT.LT.0.0) THEN
            DALT=-DALT
C           Fliessumkehr!
            WRITE(25,*) IAB(I),'DALT in  YGRENZ4 = ',ENGSI(DALT,1)
            IF (DNEU.LT.DALT) THEN
               DNEU=DALT
               WRITE(25,*) IAB(I),'DNEU aus YGRENZ4 = ',ENGSI(DNEU,1)
            END IF
         END IF
         GOTO 149
      ELSE
         WRITE(25,'(T2,A)') 'UP POLENI: DNEU = ABS(DALT)'
         DNEU=ABS(DALT)
         WRITE(25,*) 'UP POLENI: IAB = ',IAB(I),'  ITYP = ',ITYPE(I)
         IF (ITYPE(I).NE.3) GOTO 149
      END IF

      IF (DALT.LT.0.0) DALT=-DALT
C
C   KOEFFIZIENTEN FUER WEHRSECTION (VORERST NUR FREIER UEBERFALL)
      A1 = 2./3. * CC * WCO * SQRT(2.*GRAV) * BW
C   ERMITTLUNG DES POLENI-WERTES
      DNEU = ABS( Q/A1 ) ** (2./3.) + HW
      IF (DNEU.LE.0.000001) THEN
           WRITE(N6,'(T2,A,I5)') ' IAB(I) = ',IAB(I)
         WRITE(N6,'(T2,A,F7.3)') '     HW = ',HW
         WRITE(N6,'(T2,A,F7.3)') '   DNEU = ',DNEU
         STOP 'UP COEF: POLENI-TIEFE <= 0 !'
      END IF
C   ERWEITERUNG FUER TRAPEZQUERSCHNITTE AM 14.11.1990 EINGEFUEGT
      IF(WNEI.EQ.0.) GOTO 149
      DH=DNEU/50.
      HH=DNEU-DH
      A1 = 2./3. * CC * WCO * SQRT(2.*GRAV) * BW
  130 IF (HH.LE.0.0) THEN
           WRITE(N6,'(T2,A,I5)') ' IAB(I) = ',IAB(I)
         WRITE(N6,'(T2,A,F7.3)') '     HW = ',HW
         WRITE(N6,'(T2,A,F7.3)') '   DALT = ',DALT
         WRITE(N6,'(T2,A,F7.3)') '   DNEU = ',DNEU
         WRITE(N6,'(T2,A,F7.3)') '      Q = ',Q
         WRITE(N6,'(T2,A,F7.3)') '     QQ = ',QQ
         WRITE(N6,'(T2,A,F7.3)') '     HH = ',HH
         WRITE(N6,'(T2,A,F7.3)') '     DH = ',DH
         STOP 'UP POLENI:   HH <= 0 !'
      END IF
      QQ=A1*HH**(3./2.)+A1*0.8*(HH**(5./2.))/BW*WNEI
      DELQ=QQ-ABS(Q)
      QEPS=ABS(Q)/1000.
      IF(DELQ.LT.QEPS) GOTO 140
      IF(QQ.GT.ABS(Q)) HH=HH-DH
      IF(QQ.LT.ABS(Q)) THEN
        HH=HH+DH
        DH=DH/2.
        HH=HH-DH
      ENDIF
      GOTO 130
  140 DNEU=HH
C
C
  149 RETURN
C
      END

C-----------------------------------------------------------------------
      SUBROUTINE PRANTL(RNI,D)

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
       
      RLAMB=2.*LOG10(D/(2.*RNI))+1.74
      RLAMB=1./RLAMB
      RLAMB=RLAMB*RLAMB
      RHY=D/4.
      RNI=SQRT(8.*9.81/RLAMB)/RHY**(1./6.)
      RNI=1./RNI
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE RAIN(DATEI,ZEILE,J)

      INCLUDE 'DIMENS.INC'
       
      PARAMETER (IPMX=12)
      COMMON /PRECIP/  PE(IPPRC),NPREC,DTPREC
      COMMON /TIB/     DT,TIME,ITOTH,ITOTM,TOTH,JTIME
      REAL PREP(IPMX)
      CHARACTER*30 DATEI
      CHARACTER*80 ZEILE
      OPEN (21,FILE=DATEI,STATUS='OLD')
      READ (21,1) ZEILE
    1 FORMAT(A)
      READ (21,*) NPREC,DTPREC
      IF (NPREC .EQ. 0) GOTO 50
C
C     IF (DTPREC.LE.0.0001) DTPREC=1.
C     PREC = DTPREC*REAL(IPPRC)
      A = DTPREC/(DT/60.)
      IA = NINT(A)
C     PRINT '(T2,A,F10.5)','DTPREC = ',DTPREC
C     PRINT '(T2,A,F10.5)','    DT = ',DT
C     PRINT '(T2,A,F10.5)','     A = ',A
C     PRINT '(T2,A,I5)','    IA = ',IA
      J = 0
      IZ = 0
   41 READ (21,*,END=444) (PREP(I),I=1,IPMX)
C  41 READ (21,42) (PREP(I),I=1,12)
C  42 FORMAT (12F5.1)
C
      GOTO 44
C
  444 IA=I
      DO 45 I=IA,IPMX
         PREP(I)=0.0
   45 CONTINUE
C
   44 DO 43 I=1,IPMX
      IZ = IZ+1
        DO 43 IJ=1,IA
        J=J+1
        PE(J) = PREP(I)/REAL(IA)
   43 CONTINUE
C
      IF (IZ.LT.NPREC) GO TO 41
   50 CLOSE (21)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE REGRES (N,X,Y,XM,YM,A,B,R,SQ,UCL,OCL,SIGNI)
C***********************************************************************
C     LINEARE REGRESSION
C     N= ANZAHL DER  DATEN
C     XM,YM= MITTELWERTE
C     XR,YR= SCHNITTPUNKT DER BEIDEN  REGRESSIONSGERADEN
C     A=\-B*X
C     B=REGRESSIONSKOEFFIZIENT
C     SQ=SUMME DER FEHLERQUADRATE=(YBEOBACHTET-YGERECHNET)**2
C     UCL=UNTERE SIGNIFIKANZGRENZE
C     OCL=OBERE SIGNIFIKANTGRENZE
C***********************************************************************

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
 
      DIMENSION X(*),Y(*)
      R=0.
      SQ=0.
      V1=0.
      V2=0.
      V12=0.
      SUMX=0.
      SUMY=0.
      SUMXX=0.
      SUMXY=0.
      XM=0.
      YM=0.
      DO 1 I=1,N
         SUMX=SUMX+X(I)
         SUMY=SUMY+Y(I)
         SUMXY=SUMXY+X(I)*Y(I)
         SUMXX=SUMXX+X(I)*X(I)
    1 CONTINUE
      B=(SUMXY-SUMX*SUMY/N)/(SUMXX-SUMX*SUMX/N)
      A=SUMY/N-B*SUMX/N
      XM=SUMX/N
      YM=SUMY/N
      DO 2 I=1,N
         YB=A+B*X(I)
         SQ=SQ+(Y(I)-YB)**2
    2 CONTINUE
      DO 3 I=1,N
         R=R+(X(I)-XM)*(Y(I)-YM)
         V1=V1+(X(I)-XM)**2
         V2=V2+(Y(I)-YM)**2
    3 CONTINUE
      V12=SQRT(V1*V2)
      IF (R.EQ.0.0.OR.V12.EQ.0.0) then
         R=0.0
      ELSE
         R=R/V12
      END IF
C     CALL KONFID (UCL,OCL,N,SIGNI)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE RETENT (I,K,L,DXOB,DXUN,DSI,DSK,FALL,QOFLOW,
     1                   DUEBFI,DISK,QOFLOWVOR)
C
C     SUBROUTINE RETENT BERECHNET DIE FUELLUNG UND ENTLEERUNG VON
C     RETENTIONSFLAECHEN, WENN DIE POLENI-FORMEL NICHT MEHR ANGEWEN-
C     DET WERDEN KANN. DES WEITEREN WERDEN DIE KORREKTURFAKTOREN FUER
C     DIE KONTINUITAETSGLEICHUNG BESTIMMT.
C
      INCLUDE 'DIMENS.INC'
C
      REAL  DUEB, KOR1, KOR2, KOR3, EPS1
      INTEGER    FALL
      LOGICAL   SCHALT, RETKN, AUSBORD
C
      COMMON /AB/           IAB(IPSEC)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
C
      COMMON /RET1/    NSPE(IPSEC,IPSPE), NSP(IPSEC,IPSPE), NRET,
     1                  NSTORE, NWERTE(IPSPE),ZETA(IPSPE),DGRENZ(IPSPE),
     2                  NVERB(IPSEC), INDEX(IPSEC)
      COMMON / RET3 /  VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
      COMMON / RET4 /   KOR1(IPSEC), KOR2(IPSEC), KOR3(IPSEC)
      COMMON / RETLOG /   SCHALT(IPSEC), RETKN(IPSEC), AUSBORD(IPSEC)
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
      PARAMETER   (EPS1 = 0.03)
C
C     EPS1 = MINDESTWASSERSPIEGELDIFFERENZ  ---> DISK {FT}
C
      ADIFF   = 0.0
      AMEAN   = 0.0
      HMEAN   = 0.0
      ZSMEAN  = 0.0
      DISK    = 0.0
      DISK1   = 0.0
C
C     WERT VOM ZEITSCHRITT VORHER
C     AUF HALTUNGSLÄNGE VERTEILEN
C    

      QOFLOWVOR = QOFLOWVOR / DX(I)

C
C     ZUERST WERDEN MITTELWERTE GEBILDET:
C            HMEAN  = MITTLERER WASSERSTAND IM FLUSS    {FT+NN}
C            ZSMEAN = MITTLERE HOEHE DER UEBERLAUFKANTE {FT+NN}
C
c     DII = ZRET(INDEX(I))-ZO(I)
      ZINP = 0.5 * (ZRET(INDEX(I)) + ZRETO(INDEX(I)))
      IF (ZINP.LT.ZRET(INDEX(I))-0.033)  ZINP = ZRET(INDEX(I))
      DII = ZINP-ZO(I)
      IF (D(I).GT.DII) DII=D(I)
C
C     FALL 0 : WASSERSTAND TIEFER ALS UEBERLAUFKANTE
C
C     IF (D(I) .LE. ZS(I))   THEN
      IF (DII .LE. ZS(I))   THEN
        FALL   = 0
        HMEAN  = H(I)
        ZSMEAN = ZO(I) + ZS(I)
C
C     FALL 1 : ZWEI DREIECKE ALS UEBERLAUFFLAECHE
C
      ELSE IF ((DXUN.GT.0.0) .AND. (DXOB.GT.0.0))   THEN
        FALL    = 1
        H1      = H(I) + (H(K) - H(I)) * DXUN/DX(I)
        H2      = H(I) + (H(L) - H(I)) * DXOB/DX(L)
        AMEAN   = (H2 + H(I)) * 0.5 * DXOB + (H(I) + H1) * 0.5 *DXUN
        HMEAN   = AMEAN/(DXUN + DXOB)
        HMAX=-999999.
        HMIN= 999999.
        IF (H(K).GT.HMAX) HMAX=H(K)
        IF (H(L).GT.HMAX) HMAX=H(L)
        IF (H(K).LT.HMIN) HMIN=H(K)
        IF (H(L).LT.HMIN) HMIN=H(L)
        IF (HMEAN.GT.HMAX.OR.HMEAN.LT.HMIN) HMEAN=0.5*(HMAX+HMIN)
        ZSMEAN  = HMEAN - 0.5 * DSI
        IF (ZSMEAN.GT.HMEAN) ZSMEAN=HMEAN
        IF (ZSMEAN.LT.ZS(I)) ZSMEAN=ZS(I)
C
C     FALL 2 : DREIECK UND TRAPEZ
C
      ELSE IF (DXOB .GT. 0.0)   THEN
        FALL   = 2
        H2     = H(I) + (H(L) - H(I)) * DXOB/DX(L)
        AMEAN  = (H2 + H(I)) * 0.5 * DXOB + (H(I) + H(K)) * 0.5 * DX(I)
        HMEAN  = AMEAN/(DXOB + DX(I))
        ADIFF  = 0.5 * DXOB * DSI + 0.5 * (DSI + DSK) * DX(I)
        AMEAN  = AMEAN - ADIFF
        ZSMEAN = AMEAN/(DXOB + DX(I))
C
C     FALL 3 : DREIECK UNTERSTOM
C
      ELSE IF (DXUN .GT. 0.0)   THEN
        FALL   = 3
        H1     = H(I) + (H(K) - H(I)) * DXUN/DX(I)
        HMEAN  = 0.5 * (H(I) + H1)
        ZSMEAN = 0.5 * (H1 + ZO(I) + ZS(I))
C
C     FALL 4 : UEBERFALLFLAECHE IST TRAPEZ (NORMALFALL)
C
      ELSE
        FALL   = 4
        HMEAN  = 0.5 * (H(I) + H(K))
        ZSMEAN = 0.5 * (ZO(I) + ZS(I) + ZO(K) + ZS(K))
c       WRITE(25,'(T2,I5,A,F7.2)') I,'  ZO(I) = ',ENGSI(ZO(I),1)
c       WRITE(25,'(T2,I5,A,F7.2)') I,'  ZS(I) = ',ENGSI(ZS(I),1)
c       WRITE(25,'(T2,I5,A,F7.2)') I,'  ZO(K) = ',ENGSI(ZO(K),1)
c       WRITE(25,'(T2,I5,A,F7.2)') I,'  ZS(K) = ',ENGSI(ZS(K),1)
      END IF
C
C     DISK = WASSERSPIEGELDIFFERENZ ZWISCHEN FLUSS U. RETENTIONSFLAECHE
C     DUEB = ZUSTROEMTIEFE
C
      ZINP = 0.5 * (ZRET(INDEX(I)) + ZRETO(INDEX(I)))
      IF (ZINP.LT.ZRET(INDEX(I))-0.033)  ZINP = ZRET(INDEX(I))
C
      IF (HMEAN.LE.ZSMEAN) THEN
c        DISK = ZSMEAN - ZRET(INDEX(I))
         DISK = ZSMEAN - ZINP
         IF (DISK.GT.0.0) DISK=0.0
      ELSE
c        DISK = HMEAN - ZRET(INDEX(I))
         DISK = HMEAN - ZINP
      END IF
c     DUEB(I) = ZRET(INDEX(I)) - ZSMEAN
      DUEB(I) = ZINP - ZSMEAN
      IF (DUEB(I) .LT. 0.0)   DUEB(I) = 0.0
C
C     BEI AUSBORDEN UND ZU KLEINEM DUEB WIRD DER OVERFLOW
C     MIT DER FORMEL VON POLENI BESTIMMT ( IN OUTPT)
C
      IF ((DISK .GE. 0.0) .AND. (DUEB(I) .LT. DGRENZ(INDEX(I)))) THEN
c      WRITE(25,'(T2,I5,A,I5)')   I,'   FALL = ',FALL
c      WRITE(25,'(T2,I5,A,F7.2)') I,'  HMEAN = ',ENGSI(HMEAN,1)
c      WRITE(25,'(T2,I5,A,F7.2)') I,'   ZRET = ',ENGSI(ZRET(INDEX(I)),1)
c      WRITE(25,'(T2,I5,A,F7.2)') I,'   ZRET = ',ENGSI(ZINP,1)
c      WRITE(25,'(T2,I5,A,F7.2)') I,' ZSMEAN = ',ENGSI(ZSMEAN,1)
c      WRITE(25,'(T2,I5,A,F7.2)') I,'   DISK = ',ENGSI(DISK,1)
c      WRITE(25,'(T2,I5,A,F7.2)') I,'   DUEB = ',ENGSI(DUEB(I),1)
       RETURN
      END IF
C
      SCHALT(I) = .FALSE.
C
C     **********    NEUER BERECHNUNGSMODUS :       **********
C
C     ZUERST WIRD EINE ZU GERINGE WASSERSPIEGELDIFFERENZ ODER EIN
C     NULLWERDEN DER UEBERSTROEMUNGSTIEFE ABGEFANGEN:
C
      IF ((ABS(DISK) .LT. EPS1) .OR. (DUEB(I) .LT. EPS1))   THEN

        QOFLOW  = 0.0
        KOR1(I) = 0.0
        KOR2(I) = 0.0
        KOR3(I) = 0.0
C
      ELSE
C
C       hier QOFLOWCC berecchnen!
C
        HWS = ZSMEAN
        IF (DISK .GT. 0.0)  THEN
C         DISK POSITIV ---> AUSBORDEN
          HOW = HMEAN
          HUW = HMEAN - DISK
        ELSE
C         DISK NEGATIV ---> RUECKSTROEMEN
          HUW = HMEAN
          HOW = HMEAN - DISK
        END IF

        QUO = (HUW-HWS)/(HOW-HWS)
        IF (QUO.LT.0.0) QUO = 0.0
c       WRITE(25,*) 'RETENT: Berechnung von QOFLOW(I)'
c       WRITE(25,*) 'I,HWS,HOW,HUW,QUO'
c       WRITE(25,*) I,ENGSI(HWS,1),ENGSI(HOW,1),ENGSI(HUW,1),QUO
C       BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
        XC=0.7
        IF (XC.GT.QUO) XC=QUO
        IF (XC.LT.0.4) XC=0.4
c       WRITE(25,*) 'XC = ',XC
        IF (QUO.LT.0.0) QUO=0.0
        IF (QUO.GT.1.0) QUO=1.0
        IF (QUO.GE.XC) THEN
           ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
           IF (ZC.GT.1.0) ZC=0.999
           CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
           IF (CC.LT.0.070) CC=0.070
        ELSE
           CC=1.0
        END IF
C
        WRITE(25,*) 'RETENT: Rückstaufaktor für QOFLOWCC = ',CC
C
C       neue Berechnung pro Breite!
        QOFLOWCC = CKS(I) * CC * DUEB(I)**1.5
C
C       alte Berechnung pro Breite!
        QOFLOW = ZETA(INDEX(I)) * SQRT(2.0*GRAV) * DUEB(I)
C
C
C
C       DISK POSITIV ---> AUSBORDEN
C       DISK NEGATIV ---> RUECKSTROEMEN
C
        IF (DISK .GT. 0.0)  THEN
          QOFLOW = - QOFLOW * SQRT(DISK)
        ELSE
          DISK1 = - DISK
          QOFLOW = QOFLOW * SQRT(DISK1)
        END IF
C
C       Faktor alt/neu
        IF (QOFLOWCC.GT.0.0) THEN
          FAKALTNEU = ABS(QOFLOW)/QOFLOWCC
          WRITE(25,*) 'RETENT: Faktor alt/neu FAKALTNEU = ',FAKALTNEU
        END IF
C
C       OVERFLOW MIT ZUGEHOERIGER UEBERFLUSSLAENGE MULTIPLIZIERT:
C
C
C!!!    QOFLOW = QOFLOW * (DXUN + DXOB)
C
        IF (DXUN .GT. 0.0)  THEN
          WRITE(25,'(T2,I5,A,F7.2)') IAB(I),'  DXUN = ',ENGSI(DXUN,1)
          QOFLOW = QOFLOW * (DXUN + DXOB)
        ELSE IF (DXOB .GT. 0.0)  THEN
          WRITE(25,'(T2,I5,A,F7.2)') IAB(I),'  DXOB = ',ENGSI(DXOB,1)
          QOFLOW = QOFLOW * (DXOB + DX(I))
        ELSE
          WRITE(25,'(T2,I5,A,F7.2)') IAB(I),' DX(I) = ',ENGSI(DX(I),1)
          QOFLOW = QOFLOW * DX(I)
        END IF
C
C       OVERFLOW AUF DIE UNTERE HALTUNGSLAENGE VERTEILEN
C       UND MIT ZEITSCHRITT VORHER MITTELN
C
        QOFLOW = QOFLOW/DX(I)
C
C       MIT ZEITSCHRITT VORHER MITTELN
C
C
C???    QOFLOW = 0.5 * (QOFLOW + QOFLOWVOR)
C???    IF (ABS(QOFLOW).GT.ABS(QOFLOWVOR)) THEN
        IF (QOFLOW.GT.QOFLOWVOR) THEN
           QOFLOW = 0.90 * QOFLOWVOR + 0.10 * QOFLOW
        ELSE
           QOFLOW = 0.5 * QOFLOWVOR + 0.5 * QOFLOW
        END IF
C
C       DUEBFI = FIKTIVE ZUSTROEMTIEFE
C
        DUEBFI = QOFLOW/(ZETA(INDEX(I)) * SQRT(2.0*GRAV))
C
        IF (DISK .GT. 0.0)  THEN
          DUEBFI = DUEBFI/SQRT(DISK)
        ELSE
          DUEBFI = DUEBFI/SQRT(DISK1)
        END IF
C
        DUEBFI = ABS(DUEBFI)
C
C       KORREKTURFAKTOREN
C
        KOR1(I) = ZETA(INDEX(I)) * SQRT(2.0*GRAV) * DUEBFI
        KOR2(I) = 0.5 * ZETA(INDEX(I)) * SQRT(2.0*GRAV)
        KOR3(I) = QOFLOW
C
        IF (DISK .GT. 0.0)  THEN
          KOR1(I) = KOR1(I)/(8.0 * SQRT(DISK))
          KOR2(I) = - KOR2(I) * (SQRT(DISK) - 0.5 * DUEBFI/(SQRT(DISK)))
        ELSE
          KOR1(I) = KOR1(I)/(8.0 * SQRT(DISK1))
          KOR2(I) = KOR2(I) * (SQRT(DISK1) + 0.5 * DUEBFI/SQRT(DISK1))
        END IF
C
      END IF
C
C     UMRECHNUNG AUF GESAMTRATE in L**3/T
C
      QOFLOW = QOFLOW * DX(I)

      QOFLOWVOR = QOFLOWVOR * DX(I)

      WRITE(25,*) 'RETENT: KNOTEN IAB = ',IAB(I)
      WRITE(25,*) 'RETENT: SPEICHER J = ',INDEX(I)
      WRITE(25,*) 'RETENT:  QOFLOWVOR = ',ENGSI(QOFLOWVOR,3)
      WRITE(25,*) 'RETENT:     QOFLOW = ',ENGSI(QOFLOW,3)
C
      RETURN
      END
C
C&&&  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
      SUBROUTINE WSTAND (INHALT,NUMMER,DT,ITIME,WST,WSTO,IVKORR)
C
C     WSTAND BERECHNET DEN WASSERSTAND IM SPEICHER IN
C     ABHAENGIGKEIT VOM SPEICHERVOLUMEN
C     INHALT = GESPEICHERTES VOLUMEN IN {FT**3}
C     NUMMER = NUMMER DER UEBERFLUTUNGSFLAECHE
C
C
      INCLUDE 'DIMENS.INC'
C
      REAL INHALT, STEIG, ZSP, SP, INHVOR
      REAL ZETA
      INTEGER   NUMMER, NSPE, NSP, NRET
      INTEGER   NWERTE
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /RET1/   NSPE(IPSEC,IPSPE), NSP(IPSEC,IPSPE), NRET,
     1                NSTORE, NWERTE(IPSPE), ZETA(IPSPE),DGRENZ(IPSPE),
     2                NVERB(IPSEC), INDEX(IPSEC)
      COMMON /RET2 /  ZSP(IPRAT,IPSPE), SP(IPRAT,IPSPE) ,VSTART(IPSPE)
      COMMON /RET3 /  VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
C
      COMMON /QRET / QOX(IPSPE,IPHYD),QEXT(IPHYD,IPSPE),
     &               MODEXT(IPSPE),IEXCL(IPSPE),QLEA(IPHYD,IPSPE),
     &               QUESP(IPSPE,IPHYD)
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
      IF (ITIME.GT.1) THEN
        ITVOR = ITIME -1
        QUESPVOR = QUESP(NUMMER,ITVOR)
      END IF
C
      IRUECK = 0
      INHVOR = INHALT
      DVUEBVOR = 0.0
      DVUEB = 0.0
      QUEBVOR = QUESPVOR
      QUEB = QUESPVOR
      DHUEVOR = 0.0
      DHUE = 0.0
      ITER=0
      WSTVOR = WST
      CCVOR=0.0
      CC=0.0
      DELTAQMIN=999999.9
C
      IF (JSPUW(NUMMER).GT.0) THEN
        WSTUW = ZRETO(JSPUW(NUMMER))
      ELSE
        WSTUW = 0.0
      END IF
C
      IF (ZKANTE(NUMMER).GT.0.0) THEN
        ZTEST = ZKANTE(NUMMER) + 0.015
        DO II = 2,NWERTE(NUMMER)
          ZUN = ZSP((II-1),NUMMER)
          ZOB = ZSP(II,NUMMER)
          IF (ZTEST.GE.ZUN.AND.ZTEST.LT.ZOB) THEN
            SPUN = SP((II-1),NUMMER)
            SPOB = SP(II,NUMMER)
            SPTEST = SPUN + (ZTEST-ZUN)*(SPOB-SPUN)/(ZOB-ZUN)
          END IF
        END DO
        DVPOT = INHVOR - SPTEST - 0.5 * QUESPVOR * DT
        QPOT = DVPOT / DT
        IF (QPOT.LT.0.0037) THEN
          QPOT = 0.0
          DVPOT = 0.0
        END IF
      ELSE
        ZTEST = 0.0
        SPTEST = 0.0
        QPOT = 0.0
        DVPOT = 0.0
      END IF
C
      IF (NUMMER .LE. NSTORE)   GOTO 10
      WRITE (N6,9) NUMMER
    9 FORMAT (//2X,' FEHLER IM UNTERPROGRAMM WSTAND BEI UEBERFLUTUNGS',
     1       'FLAECHE NUMMER',I5//)
      STOP 9
C
   10 CONTINUE
C
      DO 20  I = 2,NWERTE(NUMMER)
         IF ( SP(I,NUMMER) .GT. INHALT) GOTO 30
   20 CONTINUE
      WRITE (N6,21) NUMMER, INHALT
   21 FORMAT (//2X,' VORSICHT: SPEICHER  NUMMER',I5,' VRET= ', F10.0,
     1 /,2X,'  LAEUFT UEBER ! FEHLER IM UNTERPROGRAMM WSTAND!'//)
      STOP 21
   30 CONTINUE
      I = I - 1
      K = I + 1
C     IF (I.GT.0) THEN
       STEIG = ZSP(K,NUMMER) - ZSP(I,NUMMER)
       STEIGO = ZSPO(K,NUMMER) - ZSPO(I,NUMMER)
       SPDIFF= SP(K,NUMMER) - SP(I,NUMMER)
       IF (SPDIFF.EQ.0.0) THEN
        PRINT '(T2,A,I3)','SPEICHER NUMMER ',NUMMER
        PRINT '(T2,A,I2,A,F11.3)','K=',K,'  SP=',ENGSI(SP(K,NUMMER),3)
        PRINT '(T2,A,I2,A,F11.3)','K=',K,' ZSP=',ENGSI(ZSP(K,NUMMER),1)
        PRINT '(T2,A,I2,A,F11.3)','I=',I,'  SP=',ENGSI(SP(I,NUMMER),3)
        PRINT '(T2,A,I2,A,F11.3)','I=',I,' ZSP=',ENGSI(ZSP(I,NUMMER),1)
        STOP 'FUNCTION WSTAND: BEI VERSCHIEDENEM ZSP GLEICHES SP !'
       END IF

       STEIG = STEIG/SPDIFF
       STEIGO = STEIGO/SPDIFF

       WSTVOR = WST
       WST = ZSP(I,NUMMER) + STEIG * (INHALT - SP(I,NUMMER))
       WSTO = ZSPO(I,NUMMER) + STEIGO * (INHALT - SP(I,NUMMER))

       IF (IRUECK.EQ.0.AND.WSTUW.GT.ZTEST.AND.WST.LT.WSTUW) THEN
c        Rückstausituation
         IF (ITER.EQ.0) THEN
c          Rückstau wird nur vor der Iteration einmal ausgewertet
           WRITE(25,*) 'Speicher im Rückstau! Volumenausgleich!'
           WRITE(25,*) '                 ITIME = ',ITIME
           WRITE(25,*) '                NUMMER = ',NUMMER
           WRITE(25,*) ' vor Ausgleich     WST = ',ENGSI(WST,1)
           WRITE(25,*) ' vor Ausgleich   WSTUW = ',ENGSI(WSTUW,1)
c          Steigung DWDV der aktuellen Speicherinhaltslinine
           DWDV = STEIG
c          Steigung DWDVUW der Speicherinhaltslinine unterhalb
           DO II = 2,NWERTE(JSPUW(NUMMER))
             ZUN = ZSPO((II-1),JSPUW(NUMMER))
             ZOB = ZSPO(II,JSPUW(NUMMER))
             IF (WSTUW.GE.ZUN.AND.WSTUW.LT.ZOB) THEN
               SPUN = SP((II-1),JSPUW(NUMMER))
               SPOB = SP(II,JSPUW(NUMMER))
               DWDVUW = (ZOB-ZUN)/(SPOB-SPUN)
             END IF
           END DO
c          Volumenausgleich DVRUECK infolge Rückstau
           DVRUECK = (WSTUW - WST)/(DWDV + DWDVUW)
           WRITE(25,*) 'Ausgleichsvol. DVRUECK = ',ENGSI(DVRUECK,3)
           QPOTRUECK = DVRUECK / DT
           IF (IVKORR.EQ.1) THEN
             VRET(JSPUW(NUMMER)) = VRET(JSPUW(NUMMER)) - DVRUECK
           END IF
c          Wasserstandsänderung im aktuellen Speicher
           DW = DWDV * DVRUECK
           WST = WST + DW
c          Wasserstandsänderung im Speicher unterhalb
           DWUW = DWDVUW * DVRUECK
           WSTUW = WSTUW - DWUW
c          Kontrollausgabe
           WRITE(25,*) 'nach Ausgleich     WST = ',ENGSI(WST,1)
           WRITE(25,*) 'nach Ausgleich   WSTUW = ',ENGSI(WSTUW,1)
c          Mittelung
           WST = 0.5 * (WST + WSTUW)
           IF (WSTO.LT.WST) THEN
             WSTO = 0.5 * (WSTO + WST)
           END IF
           IRUECK = 1
           WSTVOR = WST
         END IF
       END IF

C      Dämpfung von WST mit Iterationsschritt vorher
       IF (WSTVOR.GT.0.0) WST = (WST + 2.0*WSTVOR)/3.0
       IF (WSTUW.GT.0.0.AND.WST.GT.WSTUW) IRUECK = 0
       IF (WSTUW.GT.ZTEST.AND.WST.LT.WSTUW) IRUECK = 1
        
       IF (QPOT.GT.0.0.AND.WST.GT.ZTEST) THEN
C         Speicher läuft über 
          WRITE(25,*) 'Speicher läuft über!'
          WRITE(25,*) '    ITIME = ',ITIME
          WRITE(25,*) '       DT = ',DT
          WRITE(25,*) '   NUMMER = ',NUMMER
          WRITE(25,*) '   INHVOR = ',ENGSI(INHVOR,3)
          WRITE(25,*) '   WSTVOR = ',ENGSI(WSTVOR,1)
          WRITE(25,*) ' QUESPVOR = ',ENGSI(QUESPVOR,3)
          WRITE(25,*) '   SPTEST = ',ENGSI(SPTEST,3)
          WRITE(25,*) '    DVPOT = ',ENGSI(DVPOT,3)
          WRITE(25,*) '   INHALT = ',ENGSI(INHALT,3)
          WRITE(25,*) '      WST = ',ENGSI(WST,1)
          WRITE(25,*) '   ZKANTE = ',ENGSI(ZKANTE(NUMMER),1)
          WRITE(25,*) '   BKANTE = ',ENGSI(BKANTE(NUMMER),1)
          DHUEVOR = DHUE
          IF (IRUECK.EQ.0) THEN
            DHUE = WST - ZKANTE(NUMMER)
          ELSE IF (IRUECK.EQ.1) THEN
            DHUE = WSTUW - ZKANTE(NUMMER)
          END IF
C         Dämpfung von angenommenem DHUE mit Iterationsschritt vorher
          IF (DHUEVOR.GT.0.0) DHUE = 0.5 * (DHUE + DHUEVOR) 
          WRITE(25,*) '     DHUE = ',ENGSI(DHUE,1)
          QUEBVOR = QUEB
          CCVOR=CC
C         Wasserstand im Unterwasser ggf. berücksichtigen
          IF (JSPUW(NUMMER).GT.0) THEN
            WRITE(25,*) '    WSTUW = ',ENGSI(WSTUW,1)
            IF (IRUECK.EQ.0) THEN
              QUO = (WSTUW-ZKANTE(NUMMER))/(WST-ZKANTE(NUMMER))
            ELSE IF (IRUECK.EQ.1) THEN
              QUO = (WST-ZKANTE(NUMMER))/(WSTUW-ZKANTE(NUMMER))
            END IF
            IF (QUO.LT.0.0) QUO = 0.0
C           BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
            XC=0.7
            IF (XC.GT.QUO) XC=QUO
            IF (XC.LT.0.4) XC=0.4
            IF (QUO.LT.0.0) QUO=0.0
            IF (QUO.GT.1.0) QUO=1.0
            IF (QUO.GE.XC) THEN
              ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
              IF (ZC.GT.1.0) ZC=0.999
              CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
              IF (CC.LT.0.070) CC=0.070
            ELSE
              CC=1.0
            END IF
          ELSE
            CC=1.0
          END IF
          IF (CCVOR.GT.0.0) CC = (CC + 2.0*CCVOR)/3.0
          WRITE(25,*) '     CC = ',CC
          IF (IEXCL(NUMMER).LT.0) THEN
C           Überlaufmenge aus Interpolation der QGRENZ-Tabelle
            ZHUE = ZKANTE(NUMMER) + DHUE
            DO II = 2,NWERTE(NUMMER)
              ZUN = ZSP((II-1),NUMMER)
              ZOB = ZSP(II,NUMMER)
              QUN = QGRKANTE((II-1),NUMMER)
              QOB = QGRKANTE(II,NUMMER)
              IF (ZUN.LE.ZTEST) THEN
                ZUN = ZTEST
                QUN = 0.0
              END IF
              IF (ZHUE.GE.ZUN.AND.ZHUE.LT.ZOB) THEN
C               Interpolation der QGRENZ-Tabelle
                QIN = QUN + (ZHUE-ZUN)*(QOB-QUN)/(ZOB-ZUN)
              END IF
            END DO
C           Überlaufmenge 
            QUEB = CC * QIN / 0.557
          ELSE
C           Überlaufmenge mittels Wehrberechnung über Poleni-Formel
            QUEB = CC * CMUEKANTE(NUMMER) * BKANTE(NUMMER) * DHUE**1.5
          END IF
          IF (QUEB.LT.0.0037) QUEB = 0.0
          IF (IRUECK.EQ.1) THEN
            IF (QUEB.GE.QPOTRUECK) THEN
              QUEB = -QPOTRUECK
            ELSE IF (QUEB.LT.QPOTRUECK) THEN
              QUEB = -QUEB
            END IF
          END IF
          WRITE(25,*) '      QUEB = ',ENGSI(QUEB,3)
C         Dämpfung von QUEB mit Iterationsschritt vorher
          QUEB = (QUEB + 2.0*QUEBVOR)/3.0
          WRITE(25,*) '      QUEB = ',ENGSI(QUEB,3)
          ITPOT = 0
   40     CONTINUE
C         wenn zu groß, Mittelung mit 0.5*QPOT
          IF (QUEB.GT.QPOT) THEN
             ITPOT = ITPOT +1
             QUEB = 0.5 * (QUEB + 0.5*QPOT)
             WRITE(25,*) '      QPOT = ',ENGSI(QPOT,3)
             WRITE(25,*) '      QUEB = ',ENGSI(QUEB,3)
             IF (ITPOT.GT.50) THEN
               WRITE(25,*) 'WSTAND: ITPOT > 50!'
               STOP 'WSTAND: ITPOT > 50!'
             END IF
             GOTO 40
          END IF
          DVUEBVOR = DVUEB
C         QUESPVOR ist die bereits fertige Überlaufwassermenge aus dem Zeitschritt vorher
C         damit wir die Volumenänderung DVUEB berechnet
          DVUEB = 0.5 * (QUEB + QUESPVOR) * DT
          INHALT = INHVOR - DVUEB
          IF (INHALT.LT.SPTEST) THEN
             WRITE(25,*) '    DVUEB = ',ENGSI(DVUEB,3)
             WRITE(25,*) '  WSTAND: INHALT < SPTEST !'
             STOP 'WSTAND: INHALT < SPTEST !'
          END IF
          EPSV = INHVOR * 0.00005
C         Q-Genauigkeit = 10 l/s !!
          EPSQ = SIENG(0.010,3)
          EPSVMIN = DT * EPSQ
          IF (EPSV.LT.EPSVMIN) EPSV = EPSVMIN
C         EPSW = 0.0825 Fuss sind etwa 0.25 cm!
C         W-Genauigkeit = 0.25 cm !!
          EPSW = 0.0825
C         Abflusszänderung im Rahmen der Iteration
          DELTAQ = ABS(QUEBVOR-QUEB)
          IF (DELTAQ.LT.DELTAQMIN) DELTAQMIN=DELTAQ
C         Volumendifferenzänderung im Rahmen der Iteration
          DELTAV = ABS(DVUEBVOR-DVUEB)
C         Wasserstandsänderung im Rahmen der Iteration
          DELTAW = ABS(WSTVOR-WST)
          WRITE(25,*) '    DVUEB = ',ENGSI(DVUEB,3)
          WRITE(25,*) '   INHALT = ',ENGSI(INHALT,3)
          ITER = ITER + 1
          IF (ITER.GT.50) THEN
             WRITE(25,*) 'DELTAQMIN = ',ENGSI(DELTAQMIN,3)
             WRITE(25,*) '   DELTAQ = ',ENGSI(DELTAQ,3)
             WRITE(25,*) '     EPSQ = ',ENGSI(EPSQ,3)
             WRITE(25,*) 'WSTAND: ITER > 50!'
             IF (DELTAW.LT.EPSW) GOTO 50
             STOP 'WSTAND: ITER > 50!'
          END IF
          IF (DELTAQ.GT.DELTAQMIN) THEN
             WRITE(25,*) 'DELTAQMIN = ',ENGSI(DELTAQMIN,3)
             WRITE(25,*) '   DELTAQ = ',ENGSI(DELTAQ,3)
             WRITE(25,*) '     EPSQ = ',ENGSI(EPSQ,3)
             WRITE(25,*) 'WSTAND: DELTAQ > DELTAQMIN!'
             GOTO 50
          END IF
          WRITE(25,*) '   DELTAQ = ',ENGSI(DELTAQ,3)
          WRITE(25,*) '     EPSQ = ',ENGSI(EPSQ,3)
          IF (DELTAQ.GT.EPSQ) GOTO 10
          WRITE(25,*) '   DELTAV = ',ENGSI(DELTAV,3)
          WRITE(25,*) '     EPSV = ',ENGSI(EPSV,3)
          IF (DELTAV.GT.EPSV) GOTO 10
          WRITE(25,*) '   DELTAW = ',ENGSI(DELTAW,1)
          WRITE(25,*) '     EPSW = ',ENGSI(EPSW,1)
          IF (DELTAW.GT.EPSW) GOTO 10
       ELSE
          IF (IRUECK.EQ.1) THEN
             INHALT = INHVOR + DVRUECK
             QUEB = -QPOTRUECK
          ELSE
             QUEB = 0.0
          END IF
       END IF 

   50  CONTINUE

       IF (ITIME.GT.0.AND.IVKORR.EQ.1) QUESP(NUMMER,ITIME) = QUEB
C      Zurückstezen des Volumens wenn IVKORR = 0
       IF (IVKORR.EQ.0) INHALT = INHVOR
        
       IF (WST.LT.ZSP(1,NUMMER)) THEN
           WRITE(25,*) '   NUMMER = ',NUMMER
           WRITE(25,*) '   INHVOR = ',ENGSI(INHVOR,3)
           WRITE(25,*) '   WSTVOR = ',ENGSI(WSTVOR,1)
           WRITE(25,*) '   INHALT = ',ENGSI(INHALT,3)
           WRITE(25,*) '      WST = ',ENGSI(WST,1)
           WRITE (N6,22) NUMMER, INHALT
   22      FORMAT (//2X,' VORSICHT: SPEICHER  NUMMER',I5,' VRET= ',
     1     F12.2,/,2X,'  LŽUFT LEER ! FEHLER IM UNTERPROGRAMM WSTAND!'/)
           STOP 22
           WST = ZSP(1,NUMMER)
       END IF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE RETINP (J,DATEI,DATOUT,LEAD,TINC)
C
C     SUBROUTINE RETINP LIEST DIE SPEICHERINHALTSLINIE EIN.
C
C
      INCLUDE 'DIMENS.INC'
C
C
      COMMON /QF/ QFAK,QBAS
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /RET1/  NSPE(IPSEC,IPSPE),NSP(IPSEC,IPSPE),NRET,NSTORE,
     1               NWERTE(IPSPE),ZETA(IPSPE),
     2               DGRENZ(IPSPE),NVERB(IPSEC),INDEX(IPSEC)
      COMMON /RET2 / ZSP(IPRAT,IPSPE), SP(IPRAT,IPSPE) ,VSTART(IPSPE)
      COMMON /RET3 / VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
      COMMON /QRET / QOX(IPSPE,IPHYD),QEXT(IPHYD,IPSPE),
     &               MODEXT(IPSPE),IEXCL(IPSPE),QLEA(IPHYD,IPSPE),
     &               QUESP(IPSPE,IPHYD)
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
      CHARACTER*30    DATEI, DATOUT, DATEXT, SPIDAT
      CHARACTER*80    ZEILE, LEER
      REAL Q1(IPHYD)
C
      PRINT *
      PRINT '(T2,A,A)',' SPEICHERDATEN VON ',DATEI
C
      OPEN (31,FILE=DATEI,STATUS='OLD')
C
C&&&  SPEICHERKENNLINIE EINLESEN
C
        READ(31,1003)  ZEILE
        READ(31,1010)  NWERTE(J), ZETA(J), DGRENZ(J), ZSTART, IEXCL(J),
     &                 ZKANTE(J), BKANTE(J), CMUEKANTE(J), JSPUW(J)

        ZKANTE(J) = SIENG(ZKANTE(J),1)
        BKANTE(J) = SIENG(BKANTE(J),1)
C
C       WENN NWERTE(J) > 0  ALTMODISCHE SPI-DATEI WIRD GELESEN!
C       WENN NWERTE(J) = -1  MODERNE SPI-DATEI WIRD GELESEN (GRAPHER-DAT-DATEI)!
C
C       WENN IEXCL(J) = 1  SPFAK WIRD NICHT VERWENDTET (Z.B. GEWÄSSER AUSSERHALB DES SPEICHERS !)
C       WENN IEXCL(J) = 0  SPFAK WIRD VERWENDTET (Z.B. GEWÄSSER INNERHALB DES SPEICHERS !)
C       WENN IEXCL(J) = -1 SPFAK WIRD VERWENDET; ÜBERLAUFEN MIT QGRENZ-SPALTE UND KONSTANTEM BEIWERT!
C       WENN IEXCL(J) = -2 SPFAK VERWENDET; ÜBERLAUFEN MIT QGRENZ- UND BEIWERT-SPALTE
C                          DABEI WIRD AUCH SPALTE ZSPO() MIT GELESEN
C
        IF (NWERTE(J).GT.0) THEN
          PRINT '(T2,A,I5)','                  NWERTE(J) = ',NWERTE(J)
          PRINT '(T2,A,I5)','SPEICHER UNTERHALB JSPUW(J) = ',JSPUW(J)
          N = NWERTE(J)
          IF (N.GT.IPRAT) THEN
            WRITE (N6,1008) N,IPRAT
            STOP 'NWERTE DER SPEICHERTABELLE > IPRAT FÜR SPEICHER!'
          END IF
C         Einlesen der Parametertabelle im alten Stil
          DO I = 1,N
            IF (IEXCL(J).EQ.1) THEN
              READ(31,*,END=4030) ZSP(I,J), SP(I,J)
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.0) THEN
              READ(31,*,END=4030) ZSP(I,J), SP(I,J), SPFAK
              SP(I,J) = SP(I,J) * SPFAK
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.-1) THEN
              READ(31,*,END=4030) ZSP(I,J), SP(I,J), SPFAK,
     &                            QGRKANTE(I,J)
              QGRKANTE(I,J) = QGRKANTE(I,J) * CMUEKANTE(J)
              SP(I,J) = SP(I,J) * SPFAK
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.-2) THEN
              READ(31,*,END=4030) ZSP(I,J), SP(I,J), SPFAK,
     &                  QGRKANTE(I,J),CMUEKANTE(J),ZSPO(I,J)
              QGRKANTE(I,J) = QGRKANTE(I,J) * CMUEKANTE(J)
              SP(I,J) = SP(I,J) * SPFAK
            END IF
          END DO
        ELSE IF (NWERTE(J).EQ.-1) THEN
          N = IPRAT
          READ(31,'(A)') SPIDAT
          PRINT '(T2,A,A)','       TABELLE VON ',SPIDAT
          OPEN (32,FILE=SPIDAT,STATUS='OLD')
          READ(32,'(A)') LEER
C         Einlesen der Parametertabelle im neuen Stil
          DO I = 1,N
            IF (IEXCL(J).EQ.1) THEN
              READ(32,*,END=4025) ZSP(I,J), SP(I,J)
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.0) THEN
              READ(32,*,END=4025) ZSP(I,J), SP(I,J), SPFAK
              SP(I,J) = SP(I,J) * SPFAK
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.-1) THEN
              READ(32,*,END=4025) ZSP(I,J), SP(I,J), SPFAK,
     &                            QGRKANTE(I,J)
              QGRKANTE(I,J) = QGRKANTE(I,J) * CMUEKANTE(J)
              SP(I,J) = SP(I,J) * SPFAK
              ZSPO(I,J) = ZSP(I,J)
            ELSE IF (IEXCL(J).EQ.-2) THEN
              READ(32,*,END=4025) ZSP(I,J), SP(I,J), SPFAK,
     &                  QGRKANTE(I,J),CMUEKANTE(J),ZSPO(I,J)
              QGRKANTE(I,J) = QGRKANTE(I,J) * CMUEKANTE(J)
              SP(I,J) = SP(I,J) * SPFAK
            END IF
          END DO
 4025     CLOSE(32,STATUS='KEEP')
          NWERTE(J)=I-1
          N = NWERTE(J)
          PRINT '(T2,A,I5)','                  NWERTE(J) = ',NWERTE(J)
          PRINT '(T2,A,I5)','SPEICHER UNTERHALB JSPUW(J) = ',JSPUW(J)
        END IF
C
        VSTART(J) = 0.0
C
C       Suche des Startvolumens
        DO I = 2,NWERTE(J)
           IF (I.GT.1.AND.ZSTART.GT.0.0.AND.ZSTART.GT.ZSP(1,J)) THEN
              IF (ZSTART.GE.ZSP(I-1,J).AND.ZSTART.LT.ZSP(I,J)) THEN
                 ZU=ZSP(I-1,J)
                 ZO=ZSP(I,J)
                 VU=SP(I-1,J)
                 VO=SP(I,J)
                 VSTART(J) = VU + (VO-VU)/(ZO-ZU)*(ZSTART-ZU)
                 VSTART(J) = SIENG (VSTART(J),3)
                 ZRET(J)=SIENG(ZSTART,1)
C
                 ZRETO(J)=SIENG(ZSTART,1)
C
                 VRET(J)=VSTART(J)
                 PRINT*
              END IF
           END IF
        END DO
C
        WRITE(25,*) 'SPEICHER ',J,' ZSTART = ',ZSTART
        WRITE(25,*) 'SPEICHER ',J,' VSTART = ',ENGSI(VSTART(J),3)
        WRITE(25,*) 'SPEICHER ',J,'  IEXCL = ',IEXCL(J)
        WRITE(25,*) 'SPEICHER ',J,'  JSPUW = ',JSPUW(J)
C
        WRITE (N6,1006)  ZEILE
        WRITE (N6,1007)  (ZSP(I,J), SP(I,J), I = 1,N)
        WRITE (N6,1009)  ZETA(J), DGRENZ(J)
C
        DO 4029  I = 1,N
          ZSP(I,J) = SIENG (ZSP(I,J),1)
          SP(I,J)  = SIENG (SP(I,J),3)
          ZSPO(I,J) = SIENG (ZSPO(I,J),1)
          QGRKANTE(I,J) = SIENG (QGRKANTE(I,J),3)
 4029   CONTINUE
        DGRENZ(J) =  SIENG (DGRENZ(J),1)
C
        READ(31,1003,END=4030)  DATOUT
        READ(31,'(2I5)',END=4032) NEXT, MODEXT(J)
        DO 2 N=1,NEXT
Calt       READ(31,1003,END=4032)  DATEXT
           READ (31,'(A,3F10.0)',END=4032) DATEXT,QFAK,QSTAT,QBAS
           PRINT *
           PRINT *
           PRINT '(T2,A,A)',' EXTERNER SPEICHERZUFLUSS VON ',DATEXT
           PRINT *
           PRINT *
C
           CALL WELLE(DATEXT,LEAD,Q1,TINC)
C
           WRITE(N6,'(A)') ' '
           WRITE(N6,'(A,A)') ' EXTERNER SPEICHERZUFLUSS VON ',DATEXT
           WRITE(N6,'(A)') ' '
           WRITE(N6,'(8F9.3)') (Q1(L),L=1,LEAD)
           WRITE(N6,'(A)') ' '
C
           DO 1 L=1,LEAD
              QEXT(L,J) = QEXT(L,J) + SIENG (Q1(L),3)
    1      CONTINUE
    2   CONTINUE
C
Calt    READ (31,1003,END=4032)  DATEXT
        READ (31,'(A,3F10.0)',END=4032) DATEXT,QFAK,QSTAT,QBAS
        PRINT *
        PRINT *
        PRINT '(T2,A,A)','EXTERNE SPEICHERLEAKAGE VON ',DATEXT
        PRINT *
        PRINT *
C
        CALL WELLE(DATEXT,LEAD,Q1,TINC)
C
        WRITE(N6,'(A)') ' '
        WRITE(N6,'(A,A)') ' EXTERNE SPEICHERLEAKAGE VON ',DATEXT
        WRITE(N6,'(A)') ' '
        WRITE(N6,'(8F9.3)') (Q1(L),L=1,LEAD)
        WRITE(N6,'(A)') ' '
C
        DO 3 L=1,LEAD
              QLEA(L,J) = SIENG (Q1(L),3)
    3   CONTINUE
C
        GOTO 4032
 4030 CLOSE (31)
      write(*,*) 'DATEI fr ber. Zuflussganglinie zum Speicher angeben '
      STOP 4030
 4032 CLOSE (31)
C
 1003 FORMAT (A)
 1004 FORMAT (I5,2F10.0)
 1005 FORMAT (2F10.0)
 1006 FORMAT (/2X,A/)
 1007 FORMAT (2X,F10.3,' {M+NN} ',5X,E10.3,' {M**3} ')
 1008 FORMAT (///2X,' ZU VIELE WERTEPAARE DER SPEICHERINHALTSLINIE ',
     1' N =',I5,5X,' MAXIMAL MOEGLICH SIND  IPRAT =',I5)
 1009 FORMAT (/2X,' VERLUSTBEIWERT              ZETA   =',F10.3/
     1         2X,' GRENZE FUER POLENI-FORMEL  DGRENZ =',F10.3,'{M}'/)
 1010 FORMAT (2(I5,3F10.0),I5)
C
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SHAPE (I,H,A,W,T)
C
C      THIS SUBROUTINE CALCULATES THE FLOW AREA, WETTED PERIMETER,
C      AND TOP WIDTH, GIVEN THE DEPTH OF FLOW

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)

      D = H - ZO(I)
       
      IF (D.LE.0.0) THEN
         WRITE(25,'(A,I5)') ' IAB = ',IAB(I)
         WRITE(25,'(A,I5,A)') ' IAB = ',IAB(I),' UP SHAPE D <= 0 !'
         WRITE(N6,'(A,I5,A)') ' IAB = ',IAB(I),' UP SHAPE D <= 0 !'
C        STOP 'UP SHAPE TIEFE D <= 0 !'
         DO J=1,ISEC
           IF (LAB(J).EQ.IAB(I)) JAC=J
         END DO
         ASEC(JAC,1) = ASEC(JAC,2)/10.
         WSEC(JAC,1) = WSEC(JAC,2)/10.
         TSEC(JAC,1) = TSEC(JAC,2)/10.
         A = ASEC(JAC,2)/10.
         W = WSEC(JAC,2)/10.
         T = TSEC(JAC,2)/10.
         GOTO 150
      END IF
       
Calt  IF (ITYPE(I).EQ.4) GO TO 120
      IF (ITYPE(I).GT.3) GO TO 120
      IF (ITYPE(I).EQ.3) GO TO 110
      T = WIDTH(I)
      IF (ITYPE(I).EQ.1) GO TO 100
C
C     RECTANGULAR SECTION.
C
      IF (D.GT.HEIT(I)) D = HEIT(I)
      A = T * D
      W = T + D + D
      IF (D.LT.HEIT(I)) RETURN
C WENN D >= HEIT  ==> GESCHLOSSENER QUERSCHNITT
      W = W + T
      T = 0.10
      RETURN
  100 CONTINUE
      IF (HEIT(I).EQ.0.0) HEIT(I)=WIDTH(I)
      IF (D.GT.HEIT(I)) D = HEIT(I)
C
C     CIRCULAR CROSS-SECTION.
C***  MAX FLOW IS AT 0.94*DIA OF THE PIPE.
C
C     NORMAL CASE---PIPE FLOWING LESS THAN HALF FULL.
C
      WRITE(N6,*) ' line 7659 ACOS'
      WRITE(N6,*) ' D = ',D,'  T = ',T 
      ARG = 1.0 - (2.0 * D/T)
      WRITE(N6,*) ' ARG = ',ARG
      THETA = 2.0 * ACOS(1.0 - (2.0 * D/T))
C
C     TEST FOR A THETA IN SECOND QUADRANT (NEGATIVE NUMBER).
C     TEST FOR SUBTENDED ANGLE (PIPE OVER HALF FULL)
C
      IF (THETA.LT.0.0) THETA = 6.28318 + THETA
      A = T * T * (THETA - SIN(THETA)) * 0.125
      W = T * THETA * 0.500
      T = T * SIN(THETA * 0.5)
      IF(T.LE.0.) T=WIDTH(I)
C     WRITE(N6,101) ENGSI(A,2),ENGSI(W,1),ENGSI(T,1)
C 101 FORMAT(1X,'UP SHAPE: PIPE: A W T ',3F10.3)
      RETURN
C
C      TRAPEZOIDAL CROSS SECTION
C
  110 CONTINUE
      A = WIDTH(I) * D + D * D/2. * (ZTR(I) + ZTL(I))
      ZSQR = SQRT(ZTR(I) *  * 2 + 1)
      ZSQL = SQRT(ZTL(I) *  * 2 + 1)
      W = WIDTH(I) + D * (ZSQR + ZSQL)
      T = WIDTH(I)+D*(ZTR(I) + ZTL(I))
C  TRAPEZQUERSCNITT UEBERLAEUFT FUER D >= HEIT
      IF (D.LE.HEIT(I)) RETURN
      UEB = D - HEIT(I)
      A = A + T * UEB
      RETURN
C
C     FREE SHAPED CROSS SECTION
C
  120 DO 121 J=1,ISEC
      JAC = J
      IF ( LAB(J).EQ.IAB(I) ) GO TO 125
  121 CONTINUE
      WRITE (N6,122) IAB(I)
  122 FORMAT(//1X,'FUER KNOTEN',I5,' KEINE QUERSCHNITTSDATEN DA')
      STOP 122
  125 DH= HEIT(I) / REAL(IPSTP-1)
      D1 = DH
      D2 = 0.
      IF (D.GE.HEIT(I)) GO TO 145
      DO 140 N = 2,IPSTP
      IF ((D-D1) .GT. 0.001)  GO TO 130

      FAK = (D-D2) / DH
      A = FAK * ( ASEC(JAC,N) - ASEC(JAC,N-1) ) + ASEC(JAC,N-1)
      W = FAK * ( WSEC(JAC,N) - WSEC(JAC,N-1) ) + WSEC(JAC,N-1)
      T = FAK * ( TSEC(JAC,N) - TSEC(JAC,N-1) ) + TSEC(JAC,N-1)
      IF (A.LE.0.) THEN
         WRITE(N6,126) IAB(I),ENGSI(H,1), N
 126     FORMAT(1X,'UP SHAPE 126 KNOTEN ',I5,' WSP = ',F7.2,' N= ',I3)
         WRITE(N6,*) '  D = ',D
         WRITE(N6,*) ' D2 = ',D2
         WRITE(N6,*) ' DH = ',DH
         WRITE(N6,*) 'FAK = ',FAK
         STOP 126
      ENDIF
      IF (T.LE.0.) THEN
         WRITE(N6,127) IAB(I),ENGSI(T,1), N
 127     FORMAT(1X,'UP SHAPE 127 KNOTEN ',I5,' T  = ',F7.2,' N= ',I3)
         WRITE(N6,*) '  D = ',D
         WRITE(N6,*) ' D2 = ',D2
         WRITE(N6,*) ' DH = ',DH
         WRITE(N6,*) 'FAK = ',FAK
         STOP 127
      ENDIF
      GO TO 150

  130 D2 = D1
      D1 = D1 + DH
  140 CONTINUE
      WRITE(N6,149)
  149 FORMAT(1X,'WIE KOMMST DU HIERHER ? '/1X,'***SHAPE***********'//)
      STOP 149
C WENN D >= HEIT, FLIESST WASSER SEITLICH UEBER; DIE QUERSCHNITTSWERTE
C          WERDEN DANN WIE FOLGT BERECHNET:
  145 UEB = D - HEIT(I)
      A = ASEC(JAC,IPSTP) + UEB * TSEC(JAC,IPSTP)
      T = TSEC(JAC,IPSTP)
      W = WSEC(JAC,IPSTP)
      IF(A.LE.0.) THEN
      WRITE(N6,146) IAB(I),ENGSI(H,1), ENGSI(UEB,1)
 146  FORMAT(1x,'UP SHAPE  KNOTEN ',I5,' WSP = ',F7.2,' UEB= ',F7.2)
      WRITE(N6,147) ENGSI(ASEC(JAC,IPSTP),2),ENGSI(TSEC(JAC,IPSTP),1)
 147  FORMAT(1x,' ASEC(JAC,IPSTP) ',F7.2,' TSEC(JAC,IPSTP) ',F7.2)
      DO 3250  JJ=1,IPSTP
      AWR = ENGSI ( ASEC(JAC,JJ),2)
      TWR = ENGSI ( TSEC(JAC,JJ),1)
      WWR = ENGSI ( WSEC(JAC,JJ),1)
      WRITE(N6,3210) IAB(I),JJ,AWR,TWR,WWR
 3210 FORMAT(1X,2I9,4F12.3)
 3250 CONTINUE
      STOP 146
      ENDIF
c     IF(T.LE.0.) THEN
c     WRITE(N6,147) IAB(I),ENGSI(T,1), ENGSI(UEB,1)
c147  FORMAT(1x,'UP SHAPE  KNOTEN ',I5,' T  = ',F7.2,' UEB=',F7.2)
c     STOP 147
c     ENDIF

  150 RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SHAPE2 (I,H,A,W,T,CH2,CHR,QGR)
C
C      THIS SUBROUTINE CALCULATES THE FLOW AREA, WETTED PERIMETER,
C      AND TOP WIDTH, GIVEN THE DEPTH OF FLOW

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)
C
C     STARTWERTE SETZEN FšR CH2 UND CHR
C
      CH2=0.0
      CHR=0.0
      QGR=0.0
      GRAV = 32.174
C
      D = H - ZO(I)
C
C     nachfolgende Zeilen eingefügt am 08.10.2008 als Test
C
      IF (D.LE.0.0) THEN
         WRITE(25,'(A,I5)') ' IAB = ',IAB(I)
         WRITE(25,'(A,I5,A)') ' IAB = ',IAB(I),' UP SHAPE2 D <= 0 !'
         WRITE(N6,'(A,I5,A)') ' IAB = ',IAB(I),' UP SHAPE2 D <= 0 !'
C        STOP 'UP SHAPE TIEFE D <= 0 !'
         DO J=1,ISEC
           IF (LAB(J).EQ.IAB(I)) JAC=J
         END DO
         ASEC(JAC,1) = ASEC(JAC,2)/10.
         WSEC(JAC,1) = WSEC(JAC,2)/10.
         TSEC(JAC,1) = TSEC(JAC,2)/10.
         CH2SEC(JAC,1) = CH2SEC(JAC,2)/10.
         CHRSEC(JAC,1) = CHRSEC(JAC,2)/10.
         QGRSEC(JAC,1) = QGRSEC(JAC,2)/10.
         A = ASEC(JAC,2)/10.
         W = WSEC(JAC,2)/10.
         T = TSEC(JAC,2)/10.
         CH2 = CH2SEC(JAC,2)/10.
         CHR = CHRSEC(JAC,2)/10.
         QGR = QGRSEC(JAC,2)/10.
         GOTO 150
      END IF
C
Calt  IF (ITYPE(I).EQ.4) GO TO 120
      IF (ITYPE(I).GT.3) GO TO 120
      IF (ITYPE(I).EQ.3) GO TO 110
      T = WIDTH(I)
      IF (ITYPE(I).EQ.1) GO TO 100
C
C     RECTANGULAR SECTION.
C
      IF (D.GT.HEIT(I)) D = HEIT(I)
      A = T * D
      W = T + D + D
      QGR=0.57735*A*SQRT(2.0*GRAV*1.5*D)
c      WRITE(N6,*) 'SHAPE2:  H = ',ENGSI(H,1)
c      WRITE(N6,*) 'SHAPE2:  D = ',ENGSI(D,1),'  QGR = ',ENGSI(QGR,3)
      IF (D.LT.HEIT(I)) RETURN

C     WENN D >= HEIT  ==> GESCHLOSSENER QUERSCHNITT
      W = W + T
      T = 0.10
      DS = H - ZO(I)
      QGR=0.700*A*SQRT(2.0*GRAV*DS)
c      WRITE(N6,*) 'SHAPE2:  H = ',ENGSI(H,1)
c      WRITE(N6,*) 'SHAPE2: DS = ',ENGSI(D,1),'  QGR = ',ENGSI(QGR,3)
      RETURN
  100 CONTINUE
      IF (HEIT(I).EQ.0.0) HEIT(I)=WIDTH(I)
      IF (D.GT.HEIT(I)) D = HEIT(I)
C
C     CIRCULAR CROSS-SECTION.
C***  MAX FLOW IS AT 0.94*DIA OF THE PIPE.
C
C     NORMAL CASE---PIPE FLOWING LESS THAN HALF FULL.
C
      WRITE(N6,*) ' line 7853 ACOS'
      WRITE(N6,*) ' D = ',D,'  T = ',T 
      ARG = 1.0 - (2.0 * D/T)
      WRITE(N6,*) ' ARG = ',ARG
      THETA = 2.0 * ACOS(1.0 - (2.0 * D/T))
C
C     TEST FOR A THETA IN SECOND QUADRANT (NEGATIVE NUMBER).
C     TEST FOR SUBTENDED ANGLE (PIPE OVER HALF FULL)
C
      IF (THETA.LT.0.0) THETA = 6.28318 + THETA
      A = T * T * (THETA - SIN(THETA)) * 0.125
      W = T * THETA * 0.500
      T = T * SIN(THETA * 0.5)
      IF(T.LE.0.) T=WIDTH(I)
C     WRITE(N6,101) ENGSI(A,2),ENGSI(W,1),ENGSI(T,1)
C 101 FORMAT(1X,'UP SHAPE: PIPE: A W T ',3F10.3)
      RETURN
C
C      TRAPEZOIDAL CROSS SECTION
C
  110 CONTINUE
      A = WIDTH(I) * D + D * D/2. * (ZTR(I) + ZTL(I))
      ZSQR = SQRT(ZTR(I) *  * 2 + 1)
      ZSQL = SQRT(ZTL(I) *  * 2 + 1)
      W = WIDTH(I) + D * (ZSQR + ZSQL)
      T = WIDTH(I)+D*(ZTR(I) + ZTL(I))
C  TRAPEZQUERSCNITT UEBERLAEUFT FUER D >= HEIT
      IF (D.LE.HEIT(I)) RETURN
      UEB = D - HEIT(I)
      A = A + T * UEB
      RETURN
C
C     FREE SHAPED CROSS SECTION ( ITYPE > 3)
C
  120 DO 121 J=1,ISEC
      JAC = J
      IF ( LAB(J).EQ.IAB(I) ) GO TO 125
  121 CONTINUE
      WRITE (N6,122) IAB(I)
  122 FORMAT(//1X,'FUER KNOTEN',I5,' KEINE QUERSCHNITTSDATEN DA')
      STOP 122
  125 DH= HEIT(I) / REAL(IPSTP-1)
      D1 = DH
      D2 = 0.
      IF (D.GT.HEIT(I)) GO TO 145

      DO 140 N = 2,IPSTP

      IF ((D-D1) .GT. 0.001)  GO TO 130

      FAK = (D-D2) / DH
      A = FAK * ( ASEC(JAC,N) - ASEC(JAC,N-1) ) + ASEC(JAC,N-1)
      W = FAK * ( WSEC(JAC,N) - WSEC(JAC,N-1) ) + WSEC(JAC,N-1)
      T = FAK * ( TSEC(JAC,N) - TSEC(JAC,N-1) ) + TSEC(JAC,N-1)
      CH2 = FAK * ( CH2SEC(JAC,N) - CH2SEC(JAC,N-1) ) + CH2SEC(JAC,N-1)
      CHR = FAK * ( CHRSEC(JAC,N) - CHRSEC(JAC,N-1) ) + CHRSEC(JAC,N-1)
      QGR = FAK * ( QGRSEC(JAC,N) - QGRSEC(JAC,N-1) ) + QGRSEC(JAC,N-1)
      IF (A.LE.0.) THEN
         WRITE(N6,126) IAB(I),ENGSI(H,1), N
 126     FORMAT(1X,'UP SHAPE2 126 KNOTEN ',I5,' WSP = ',F7.2,' N= ',I3)
         WRITE(N6,*) '  D = ',D
         WRITE(N6,*) ' D2 = ',D2
         WRITE(N6,*) ' DH = ',DH
         WRITE(N6,*) 'FAK = ',FAK
         STOP 126
      ENDIF
      IF (T.LE.0.) THEN
         WRITE(N6,127) IAB(I),ENGSI(T,1), N
 127     FORMAT(1X,'UP SHAPE2 127 KNOTEN ',I5,' T  = ',F7.2,' N= ',I3)
         WRITE(N6,*) '  D = ',D
         WRITE(N6,*) ' D2 = ',D2
         WRITE(N6,*) ' DH = ',DH
         WRITE(N6,*) 'FAK = ',FAK
         STOP 127
      ENDIF
      IF (QGR.LE.0.) THEN
         WRITE(N6,129) IAB(I),ENGSI(QGR,3), N
 129     FORMAT(1X,'UP SHAPE2 129 KNOTEN ',I5,' QGR = ',F7.2,' N= ',I3)
         WRITE(N6,*) '  D = ',D
         WRITE(N6,*) ' D2 = ',D2
         WRITE(N6,*) ' DH = ',DH
         WRITE(N6,*) 'FAK = ',FAK
         STOP 129
      ENDIF
      GO TO 150

  130 D2 = D1
      D1 = D1 + DH
  140 CONTINUE

      WRITE(N6,149)
  149 FORMAT(1X,'WIE KOMMST DU HIERHER ? '/1X,'***SHAPE***********'//)
      STOP 149

C WENN D >= HEIT, FLIESST WASSER SEITLICH UEBER; DIE QUERSCHNITTSWERTE
C          WERDEN DANN WIE FOLGT BERECHNET:

  145 UEB = D - HEIT(I)
      A = ASEC(JAC,IPSTP)
      T = TSEC(JAC,IPSTP)
      W = WSEC(JAC,IPSTP)
      RHY=A/W
      RNH2=1.486/CH2SEC(JAC,IPSTP)*A*RHY**(2./3.)
      RNHR=1.486/CHRSEC(JAC,IPSTP)*A*RHY**(2./3.)
      A = A + UEB * TSEC(JAC,IPSTP)
      RHY=A/W
      CH2=1.486/RNH2*A*RHY**(2./3.)
      CHR=1.486/RNHR*A*RHY**(2./3.)
      QGR=QGRSEC(JAC,IPSTP)

      IF(A.LE.0.) THEN
      WRITE(N6,146) IAB(I),ENGSI(H,1), ENGSI(UEB,1)
 146  FORMAT(1x,'UP SHAPE  KNOTEN ',I5,' WSP = ',F7.2,' UEB= ',F7.2)
      WRITE(N6,147) ENGSI(ASEC(JAC,IPSTP),2),ENGSI(TSEC(JAC,IPSTP),1)
 147  FORMAT(1x,' ASEC(JAC,IPSTP) ',F7.2,' TSEC(JAC,IPSTP) ',F7.2)
      DO 3250  JJ=1,IPSTP
      AWR = ENGSI ( ASEC(JAC,JJ),2)
      TWR = ENGSI ( TSEC(JAC,JJ),1)
      WWR = ENGSI ( WSEC(JAC,JJ),1)
      WRITE(N6,3210) IAB(I),JJ,AWR,TWR,WWR
 3210 FORMAT(1X,2I9,4F12.3)
 3250 CONTINUE
      STOP 146
      ENDIF
c     IF(T.LE.0.) THEN
c     WRITE(N6,147) IAB(I),ENGSI(T,1), ENGSI(UEB,1)
c147  FORMAT(1x,'UP SHAPE  KNOTEN ',I5,' T  = ',F7.2,' UEB=',F7.2)
c     STOP 147
c     ENDIF

  150 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE STATER (ITIME,GATEL)
C
C     THIS SUBROUTINE CALCULATES THE GATE TRAJECTORIES SPATIALLY.

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /CREEP/        GINITL(IPGAT,IPPRC)
      REAL      GATEL(IPGAT)
C
C     LOWER BOUNDS ON GATE OPENING ARE SET TO GMIN.
C
C     GMIN=0.05 GEŽNDERT AM 26.11.1997 (GERD)
      GMIN=0.0005
      DO 100 K = 1,NGATES
         GATEL(K) = GINITL(K,ITIME)
         IF (GATEL(K).LT.GMIN) GATEL(K) = GMIN
  100 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SURFAC(NJ,LEAD,JPREC,Q,DT,TINC,KNOTEN)
**********************************************************************
*     BERECHNUNG DER ABFLUSSGANGLINIE BEI BEKANNTER UH UND REGEN     *
*     MODELL: LINEARE SPEICHERKASKADE (MIT VORGESCHALTETEM SPEICHER) *
**********************************************************************

      INCLUDE 'DIMENS.INC'
 
      PARAMETER (IPNH=100)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /PRECIP/ PE(IPPRC),NPREC,DTPREC
      COMMON /URBAN/  SPK(IPURB),SPN(IPURB),ARED(IPURB),QBASA(IPURB),
     1QBASE(IPURB),QMAX(IPURB)
      DIMENSION Q(IPHYD),QBAS(IPHYD),QD(IPHYD),P(IPHYD)
      DIMENSION H(IPNH)
      NH=IPNH
      CALL SYFU (SPK(NJ),SPN(NJ),TINC,NH,H)
      SUM=0.
      DO 100 I=1,NH
  100 SUM=SUM+H(I)
      DO 101 I=1,NH
  101 H(I) = H(I)/SUM
C*    ANPASSEN DER ZEITINTERVALLE DT DER DURCH SUBROUTINE "RAIN"
C*    ANGEPASSTEN NIEDERSCHLAGSGANGLINIE (DT IST IDENTISCH DEM
C*    RECHENZEITSCHRITT) AN DIE ZEITINTERVALLE TINC DER ABFLUSSGANGLINIE
C*    (TINC DARF NICHT KLEINER ALS DT SEIN UND TINC/DT = GANZE ZAHL!)
      A1=ANINT(TINC/DT)
      NA1=INT(A1)
      IF(TINC.LT.DT) THEN
      WRITE(N6,901) KNOTEN
      STOP 5
      ENDIF
      IF(ABS(A1-TINC/DT).GE.0.0001) THEN
      WRITE(N6,902) KNOTEN
      STOP 5
      ENDIF
C*    ANPASSEN DER NIEDERSCHLAGSGANGLINIE
      FAK = ARED(NJ)/(6.*DT)
      DO 102 I=1,JPREC,NA1
      II=I
      IF(NA1.GT.1) II=I/NA1+1
      IF (II.GT.IPHYD) THEN
         PRINT '(T2,A)','FELDšBERSCHREITUNG IN UP SURFAC:'
         PRINT '(T2,2(A,I5))','II = ',II,'  IPHYD = ',IPHYD
         STOP
      END IF
      P(II)=0.
      DO 103 JJ=I,I+NA1-1
  103 P(II)=P(II)+PE(JJ)
  102 P(II)=P(II)*FAK/A1
C*    BERECHNUNG DES BASIS ABFLUSSES QBAS
      QBAS(1)=QBASA(NJ)
      QBAS(LEAD)=QBASE(NJ)
      SS=QBAS(LEAD)-QBAS(1)
      SS = SS / REAL(LEAD-1)
      DO 104 I=2,LEAD
  104 QBAS(I) = QBAS(1) + REAL(I-1)*SS
C*    BERECHNUNG DES DIREKTABFLUSSES
      NE=JPREC+NH-1
      CALL FALTS(NH,JPREC,NE,H,P,QD)
C*    BERECHNEN DES GESAMTABFLUSSES
      DO 105 I=1,LEAD
  105 Q(I)=QD(I)+QBAS(I)
C*    ZUSCHALTEN EINES SPEICHERS FUER Q > QMAX
      DO 106 I=1,LEAD-1
      IF(Q(I).GT.QMAX(NJ)) THEN
      Q(I+1)=Q(I+1)+Q(I)-QMAX(NJ)
      Q(I)=QMAX(NJ)
      ENDIF
      IF(I.EQ.(LEAD-1).AND.Q(I+1).GT.QMAX(NJ)) Q(I+1)=QMAX(NJ)
  106 CONTINUE
  901 FORMAT(//1X,'URBANER ABFLUSS NACH KNOTEN:',I4/1X,'ZEITINTERVALL',
     1'DER ABFLUSSGANGLINIE IST KLEINER ALS RECHENZEITSCHRITT',//)
  902 FORMAT(//1X,'URBANER ABFLUSS NACH KNOTEN:',I4/1X,'ZEITINTERVALL',
     1'DER AFLUSSGANGLINIE UND DER RECHENZEITSCHRITT',
     2' SIND NICHT KOMPATIBEL',//)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SYFU (AK,AN,DT,NH,H)

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
       
      DIMENSION H(NH)
      IF (AK.EQ.0..OR.AN.EQ.0.) GOTO 101
      NH1 = NH
      SUM = 0.
      DO 102 I=1,NH
      ZEIT = (REAL(I)-0.5) * DT
      SP1 = AN - 1.
      AA = ZEIT ** SP1
      ZZ = -ZEIT/AK
      IF (ZZ.GT.70.) ZZ = 70.
      IF (ZZ.LT.-70.) ZZ = -70.
      BB = EXP (ZZ)
      CC = AK ** AN
      H(I) = AA * BB / CC
      SUM = SUM + H(I)
  102 IF (H(I).LE.0.0001) GOTO 103
  103 NH = I
      SS = 0.
      DO 104 I=1,NH
      H(I) = H(I) / SUM
      SS = SS + H(I)
  104 IF (SS.GT.0.9995) GOTO 105
      GOTO 106
  105 NH = I
      I= I+1
      DO 107 J=I,NH1
  107 H(I) = 0.
  106 NH = NH + 1
      IF (NH.GT.NH1) NH = NH1
      DO 108 I=NH,2,-1
  108 H(I) = H(I-1)
      NH = NH + 1
      H(NH) = 0.
      H(1) = 0.
      RETURN

  101 NH = 3
      DO 109 I=1,NH
  109 H(I) = 0.
      H(2) = 1.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FALTS (NH,N,NQ,H,X,Y)
*****************************************************
*     AUSFUEHRUNG DES FALTUNGSINTEGRALES            *
*     NH  ANZAHL H- ORDINATEN                       *
*     N  ANZAHL X- ORDINATEN                        *
*****************************************************

      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
 
      DIMENSION H(*),X(*),Y(*)
      DO 101 I=1,NQ
      Y(I) = 0.
      DO 101 J=1,I
      K = I - J + 1
      IF (K.GT.NH) GOTO 101
      IF (J.GT.N) GOTO 101
      Y(I) = Y(I) + X(J) * H(K)
  101 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE TEST (TOL,ITIME)
C     SUBROUTINE TEST (TOL,ITIME,Q2,D2)
C
C      THIS SUBROUTINE CALLS THE ROUTINES FOR THE FORWARD AND
C      BACKWARD SWEEPS.  RESULTS ARE CHECKED FOR CRITICAL FLOW
C      AND, IF NECESSARY, THE ROUTING PROCEDURE IS REPEATED FOR
C      THIS TIME PERIOD.

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /STEP/         DTSAVE
      COMMON /TIB/          DT   , TIME  , ITOTH, ITOTM,TOTH,JTIME
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /DOWNCO/       QDON(IPRAT) , YDON(IPRAT) , IPTS        ,
     1        IDOWN       , AC          , B
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /BNDRY/        FLOWER      , DHH         , DHT         ,
     1        ZSILL
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /WEHR2/ NWEHR,IWEHR(IPWEI),IWRAUS(IPWEI)
      COMMON /JUN/          NJUNC       , NXJ(IPJUN,7) , XXL(IPJUN) ,
     1        GAM(IPJUN,3)
      COMMON /FLOWS/        NQIN    , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /GATOLD/       GOLD(IPGAT)
      COMMON /AB/           IAB(IPSEC)
C
C     REAL Q2(IPSEC), D2(IPSEC)
      COMMON /Q2D2/ Q2(IPSEC), D2(IPSEC)
C
      LOGICAL DTIME,FLAG,FLAG120
      DATA MTIME/0/
C
      COMMON /NEWER/ QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /GEWID/ ID(IPSEC)
C
      COMMON /QDD/    QZERO(IPSEC) , DZERO(IPSEC)
C
      COMMON /RELAX/  IRELAXFOR, IRELAXBACK, DHZUL, DHRELZUL, DVRELZUL
C
C
C***  DT IS IN SECONDS AND TOTAL T IS IN MINUTES.
C
      NNN = NX * 2
      DTSAVE = DT
      DTIME = .TRUE.
      IF (NGATES.LE.0) GO TO 180
      IF (ITIME.GE.2) CALL STATER (ITIME - 1,GOLD)
      CALL STATER (ITIME,GATEL)
C
C      ON THE FIRST TIME THROUGH SKIP TO ROUTING PROCEDURE
C
      GO TO 180
C
C      THIS SECTION IS USED TO REDEFINE THE BOUNDARY CONDITIONS FOR
C      THE REDUCED TIME STEP SO THE ROUTING CAN BE REPEATED
C
  100 CONTINUE
      WRITE (N6,440) IAB(I)
      IF (LIST.GE.1) GO TO 110
      Q2$ = ENGSI(Q2(I),3)
      D2$ = ENGSI(D2(I),1)
      WRITE (IOUT,460) IAB(I),Q2$,D2$,FROUDE,DT
  110 CONTINUE
  120 DT = DT * 0.5
C
C      LIMIT FOR DECREASING TIME STEP AND REPEATING COMPUTATIONS
C
      IF (DT.GE..10) GO TO 125
      WRITE (N6,121)
  121 FORMAT (///1X,'ABBRUCH, WEIL ZEITSCHRITT KUERZER ALS 0.1 SEC')
      STOP 121
  125 IF(DTIME) DT = (DTSAVE*2.)/3.
      DTIME = .FALSE.
C
C      RESET UPSTREAM INFLOWS
C
      DO 130 I = 1,NQIN
         NSEC = NUP(I)
         JJ = 2 * NSEC - 1
         QLAST = C11(ITIME,I)
         IF (ITIME.GT.1) QLAST = C11(ITIME - 1,I)
         DELQ = C11(ITIME,I) - QLAST
         QSET = QLAST + DELQ * (DT/DTSAVE)
         E(JJ) = QSET
  130 CONTINUE
C
C     RESET LATERAL INFLOWS.
C
      QSET = E(1)
      CALL LATRL (QSET,NX,ITIME)
C
C      RESET GATE OPENINGS
C
      IF (NGATES.LE.0) GO TO 150
      DO 140 I = 1,NGATES
         DELGA = GATEL(I) - GOLD(I)
         GATEL(I) = GOLD(I) + DELGA * (DT/DTSAVE)
  140 CONTINUE
C
C     RESET DOWNSTREAM BOUNDARY CONDITION.
C     FOR RATING CURVE OR TABLE NO CHANGES ARE NECESSARY
C
  150 GO TO (180,180,160,170), IDOWN
C
C      STAGE HYDROGRAPH
C
  160 HDLAST = HDOWN(ITIME)
      IF (ITIME.GT.1) HDLAST = HDOWN(ITIME - 1)
      DELHD = HDOWN(ITIME) - HDLAST
      HSETD = HDLAST + DELHD * (DT/DTSAVE)
      E(NNN) = HSETD - ZO(NX)
      GO TO 180
C
C      DISCHARGE HYDROGRAPH
C
  170 QDLAST = QDOWN(ITIME)
      IF (ITIME.GT.1) QDLAST = QDOWN(ITIME - 1)
      DELQD = QDOWN(ITIME) - QDLAST
      QSETD = QDLAST + DELQD * (DT/DTSAVE)
      E(NNN) = QSETD
  180 CONTINUE
C
C      SET THE COEFFICIENTS FOR MATRIX SOLUTION
C
      CALL CFNT (DT,ITIME)
C
C      SET COEFFICIENTS FOR INFLOW HYDROGRAPHS AT UPSTREAM SECTIONS
      DO 190 I = 1,NQIN
         NSEC = NUP(I)
         JJ = 2 * NSEC - 1
         COM(JJ,3) = CO(JJ,3)
         COM(JJ,4) = CO(JJ,4)
         COZ(JJ) = E(JJ)
  190 CONTINUE

C
C      FORWARD SWEEP - INTERRUPTED FOR JUNCTIONS
C
      NUU = 1
      IF (NJUNC.EQ.0) GO TO 205
      DO 200 I = 1,NJUNC
         NU = NXJ(I,2)
         ND = NXJ(I,1)
         NUU = NU
         DO 195 K=1,NWEIRS
         IWK = IWEIR(K)
         IF(IWK.LT.NUU .OR. IWK.GT.ND) GO TO 195
         NDD = IWK
         CALL FORWRD (NUU,NDD)
         CALL FORWEI (IWK)
         NUU = IWK + 1
  195    CONTINUE
         CALL FORWRD (NUU,ND)
         NU2 = NXJ(I,4)
         ND2 = NXJ(I,3)
         NUU = NU2
         DO 196 K=1,NWEIRS
         IWK = IWEIR(K)
         IF(IWK.LT.NUU .OR. IWK.GT.ND2) GO TO 196
CCCCC    IF (H(IWK+1).GT.(ZO(IWK)+HW(K)) ) GO TO 196
         NDD = IWK
         CALL FORWRD (NUU,NDD)
         CALL FORWEI (IWK)
         NUU = IWK + 1
  196    CONTINUE
         CALL FORWRD (NUU,ND2)
         NU4 = NXJ(I,7)
         ND4 = NXJ(I,6)
         IF (ND4.LE.0) GO TO 198
         NUU = NU4
         DO 197 K=1,NWEIRS
         IWK = IWEIR(K)
         IF(IWK.LT.NUU .OR. IWK.GT.ND4) GO TO 197
CCCCC    IF (H(IWK+1).GT.(ZO(IWK)+HW(K)) ) GO TO 197
         NDD = IWK
         CALL FORWRD (NUU,NDD)
         CALL FORWEI (IWK)
         NUU = IWK + 1
  197    CONTINUE
         CALL FORWRD (NUU,ND4)
  198    ND3 = NXJ(I,5)
         CALL FORJUN (ND,ND2,ND3,ND4,I)
  200 CONTINUE
      NU = NXJ(NJUNC,5)
         NUU = NU
         DO 201 K=1,NWEIRS
         IWK = IWEIR(K)
         IF(IWK.LT.NUU .OR. IWK.GT.NX) GO TO 201
CCCCC    IF (H(IWK+1).GT.(ZO(IWK)+HW(K)) ) GO TO 201
         NDD = IWK
         CALL FORWRD (NUU,NDD)
         CALL FORWEI (IWK)
         NUU = IWK + 1
  201    CONTINUE
         GOTO 210
  205    IF (NWEIRS.EQ.0) GOTO 210
         DO 206 K=1,NWEIRS
         IWK = IWEIR(K)
         IF(IWK.LT.NUU .OR. IWK.GT.NX) GO TO 206
         NDD = IWK
         CALL FORWRD (NUU,NDD)
         CALL FORWEI (IWK)
         NUU = IWK + 1
  206    CONTINUE

  210 CALL FORWRD (NUU,NX)
      NNN = NX * 2
      CON(1) = CO(NNN,1)
      CON(2) = CO(NNN,2)
      CON(3) = E(NNN)
C
C     TESTAUSGABE NACH FORWARD SWEEP
C
       IRELAXFOR=0
C
       DHPOSMX = 0.0
       DHNEGMI = 0.0
       DQPOSMX = 0.0
       DQNEGMI = 0.0
       KNPOSMX = 0
       KNNEGMI = 0
       DVPOSMX = 0.0
       DVNEGMI = 0.0
       KNDVPOSMX = 0
       KNDVNEGMI = 0
C
C      aktuelle Zeit in Stunden
       TAK = (TIME+DT/60.)/60.
C
       IF (TAK.GT.0) THEN
         DO I = 2,NX-1
           JQX = I * 2
           JQXU = (I+1) * 2
           IF (XL(I+1).LT.XL(I)) THEN
C            maximal mögliche Volumenänderung pro dt
             DVPOT = 0.5*(AREA(I)+AREA(I+1))*DX(I)
C            aktuelle Volumenänderung pro dt
             QZU = 0.5 * (XV(JQX-1) + Q2(I))
             QAB = 0.5 * (XV(JQXU-1) + Q2(I+1))
C            SEITLICHE ZUFLšSSE VON JUNCTIONS SUCHEN
             QJUN = 0.0
             DO JJ=1,NJUNC
               IF (I.EQ.NXJ(JJ,1)) THEN
                 JQXJUN = NXJ(JJ,3) * 2
                 QJUN = 0.5*(XV(JQXJUN-1) + Q2(NXJ(JJ,3)))
                 IF (NXJ(JJ,6).GT.0) THEN
                   JQXJUN = NXJ(JJ,6) * 2
                   QJUN = QJUN + 0.5*(XV(JQXJUN - 1) + Q2(NXJ(JJ,6)))
                 END IF
               END IF
             END DO

             QLAT = DX(I)*(QLAT1(I)+QOFLOW(I))

             DQAKT = QZU + QJUN - QAB + QLAT
C            
             IF (ENGSI(DQAKT,3).GT.0.001) THEN
               DVAKT = DQAKT * DT
               DVREL = DVAKT/DVPOT
             ELSE
               DQAKT = 0.0
               DVAKT = 0.0
               DVREL = 0.0
             END IF

             IF (DVREL.GT.0.01.AND.DVREL.GT.DVPOSMX) THEN
               DVPOSMX = DVREL
               KNDVPOSMX = IAB(I)
             END IF
             IF (DVREL.LT.-0.01.AND.DVREL.LT.DVNEGMI) THEN
               DVNEGMI = DVREL
               KNDVNEGMI = IAB(I)
             END IF

             IF (ABS(DVREL).GT.DVRELZUL) THEN
               WRITE (25,*) 'DV/DT  ZU GROSS NACH FORWARD SWEEP!'
               WRITE (25,*)    '  IAB = ', IAB(I)
               WRITE (25,*) 'DVPOT = ', ENGSI(DVPOT,3)
               WRITE (25,*) 'DVAKT = ', ENGSI(DVAKT,3)
               WRITE (25,*) 'DVREL = ', DVREL
               WRITE (25,*) '  QZU = ', ENGSI(QZU,3)
               WRITE (25,*) ' QJUN = ', ENGSI(QJUN,3)
               WRITE (25,*) ' QLAT = ', ENGSI(QLAT,3)
               WRITE (25,*) '  QAB = ', ENGSI(QAB,3)
               WRITE (25,*) 'DQAKT = ', ENGSI(DQAKT,3)
             END IF

           END IF
           DVOR = ENGSI(D2(I),1)
           QVOR = ENGSI(Q2(I),3)
           DAKT = ENGSI(XV(JQX),1)
           QAKT = ENGSI(XV(JQX-1),3)
           DBAS = ENGSI(DZERO(I),1)
           QBAS = ENGSI(QZERO(I),3)
           DH = DAKT - DVOR
           IF (DH.GT.0.001.AND.DH.GT.DHPOSMX) THEN
              DHPOSMX = DH
              QMIT = (QAKT+QVOR)/2.
              DQPOSMX = (QAKT-QVOR)/QMIT
              KNPOSMX = IAB(I)
           END IF
           IF (DH.LT.-0.001.AND.DH.LT.DHNEGMI) THEN
              DHNEGMI = DH
              QMIT = (QAKT+QVOR)/2.
              DQNEGMI = (QAKT-QVOR)/QMIT
              KNNEGMI = IAB(I)
           END IF

           IF (DAKT.GT.0.0) THEN
             DHAKTZUL = DHRELZUL * DAKT
             IF (DHAKTZUL.LT.DHZUL) DHAKTZUL=DHZUL
           ELSE
             DHAKTZUL=DHZUL
           END IF
          
           IF (ABS(DVOR-DAKT).GT.DHAKTZUL) THEN
             WRITE (25,'(A)') 'WSP INSTABIL NACH FORWARD SWEEP!'
             WRITE(25,'(T2,A,F10.6)') '    DHAKTZUL = ',DHAKTZUL
             WRITE (25,481) 'IAB=',IAB(I),' DVOR=',DVOR,' QVOR =',QVOR
             WRITE (25,481) 'IAB=',IAB(I),' DAKT=',DAKT,' QAKT =',QAKT
             WRITE (25,481) 'IAB=',IAB(I),' DBAS=',DBAS,' QBAS =',QBAS
C            IRELAXFOR=1
C            Dämpfung am aktuellen Knoten
C???         IF (DAKT.LT.DBAS) DAKT=DBAS
             IF (DAKT.LT.0.05) DAKT=0.05
             DNEU = 0.5*(DVOR+DAKT)
             QNEU = 0.5*(QVOR+QAKT)
             WRITE (25,481) 'IAB=',IAB(I),' DNEU=',DNEU,' QNEU =',QNEU
             XV(JQX) = SIENG(DNEU,1)
             XV(JQX-1) = SIENG(QNEU,3)
C            Dämpfung am Knoten oberhalb
             I1 = I-1
             JQX1 = I1 * 2
             IF (XL(I1).GT.XL(I)) THEN
               DVOR = ENGSI(D2(I1),1)
               QVOR = ENGSI(Q2(I1),3)
               DAKT = ENGSI(XV(JQX1),1)
               QAKT = ENGSI(XV(JQX1 - 1),3)
               DBAS = ENGSI(DZERO(I1),1)
               QBAS = ENGSI(QZERO(I1),3)
C???           IF (DAKT.LT.DBAS) DAKT=DBAS
               IF (DAKT.LT.0.05) DAKT=0.05
               DNEU = 0.5*(DVOR+DAKT)
               QNEU = 0.5*(QVOR+QAKT)
               WRITE (25,481)'IAB=',IAB(I1),' DNEU=',DNEU,' QNEU =',QNEU
               XV(JQX1) = SIENG(DNEU,1)
               XV(JQX1-1) = SIENG(QNEU,3)
             END IF
C            Dämpfung am Knoten unterhalb
             I1 = I+1
             JQX1 = I1 * 2
             IF (XL(I1).LT.XL(I)) THEN
               DVOR = ENGSI(D2(I1),1)
               QVOR = ENGSI(Q2(I1),3)
               DAKT = ENGSI(XV(JQX1),1)
               QAKT = ENGSI(XV(JQX1 - 1),3)
               DBAS = ENGSI(DZERO(I1),1)
               QBAS = ENGSI(QZERO(I1),3)
C???           IF (DAKT.LT.DBAS) DAKT=DBAS
               IF (DAKT.LT.0.05) DAKT=0.05
               DNEU = 0.5*(DVOR+DAKT)
               QNEU = 0.5*(QVOR+QAKT)
               WRITE (25,481)'IAB=',IAB(I1),' DNEU=',DNEU,' QNEU =',QNEU
               XV(JQX1) = SIENG(DNEU,1)
               XV(JQX1-1) = SIENG(QNEU,3)
             END IF
           END IF
         END DO
       END IF
  481  FORMAT (A,I6,2(A,F10.3))
C
C      BACKWARD SWEEP - INTERRUPTED FOR GATES AND JUNCTIONS
C
      ND = NX
      IF (NJUNC.EQ.0) GO TO 270
      IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 220
      CALL BAKGAT (ITIME,NU,ND)
  220 CALL BAKWRD (NU,ND)
      DO 260 J = 1,NJUNC
         LM = NJUNC - J + 1
         NJ1 = NXJ(LM,1)
         NJ2 = NXJ(LM,3)
         NJ3 = NXJ(LM,5)
         NJ4 = NXJ(LM,6)
         CALL BACJUN (NJ1,NJ2,NJ3,NJ4,LM)
         ND = NJ1
         NU = NXJ(LM,2)
         IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 230
         CALL BAKGAT (ITIME,NU,ND)
  230    CALL BAKWRD (NU,ND)
         ND = NJ2
         NU = NXJ(LM,4)
         IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 240
         CALL BAKGAT (ITIME,NU,ND)
  240    CALL BAKWRD (NU,ND)
         IF (NJ4.EQ.0) GO TO 260
         ND = NJ4
         NU = NXJ(LM,7)
         IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 250
         CALL BAKGAT (ITIME,NU,ND)
  250    CALL BAKWRD (NU,ND)
  260 CONTINUE
C#     WRITE(6,188)
C#     WRITE(6,189) (IP,XV(IP),IP=1,NNN)
      GO TO 290
  270 NU = 1
      IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 280
      CALL BAKGAT (ITIME,NU,ND)
  280 CALL BAKWRD (NU,ND)
  290 CONTINUE
C#    WRITE(6,192)
C#     WRITE(6,191) (IY,COM(IY,2),IY=1,NLL)
C
C     TESTAUSGABE NACH BACKWARD SWEEP
C
       IRELAXBACK=0
C
       IF (TIME.GT.0) THEN
         DO I = 2,NX-1
           JQX = I * 2
           DVOR = ENGSI(D2(I),1)
           QVOR = ENGSI(Q2(I),3)
           DAKT = ENGSI(XV(JQX),1)
           QAKT = ENGSI(XV(JQX - 1),3)
           DBAS = ENGSI(DZERO(I),1)
           QBAS = ENGSI(QZERO(I),3)
           DH = DAKT - DVOR
           IF (DH.GT.0.001.AND.DH.GT.DHPOSMX) THEN
              DHPOSMX = DH
              QMIT = (QAKT+QVOR)/2.
              DQPOSMX = (QAKT-QVOR)/QMIT
              KNPOSMX = IAB(I)
           END IF
           IF (DH.LT.-0.001.AND.DH.LT.DHNEGMI) THEN
              DHNEGMI = DH
              QMIT = (QAKT+QVOR)/2.
              DQNEGMI = (QAKT-QVOR)/QMIT
              KNNEGMI = IAB(I)
           END IF

           IF (DAKT.GT.0.0) THEN
             DHAKTZUL = DHRELZUL * DAKT
             IF (DHAKTZUL.LT.DHZUL) DHAKTZUL=DHZUL
           ELSE
             DHAKTZUL=DHZUL
           END IF

           IF (ABS(DVOR-DAKT).GT.DHAKTZUL) THEN
             WRITE (25,'(A)') 'WSP INSTABIL NACH BACKWARD SWEEP:'
             WRITE(25,'(T2,A,F10.6)') '    DHAKTZUL = ',DHAKTZUL
             WRITE (25,481) 'IAB=',IAB(I),' DVOR=',DVOR,' QVOR =',QVOR
             WRITE (25,481) 'IAB=',IAB(I),' DAKT=',DAKT,' QAKT =',QAKT
             WRITE (25,481) 'IAB=',IAB(I),' DBAS=',DBAS,' QBAS =',QBAS
C            IRELAXBACK=1
C            Dämpfung am aktuellen Knoten
C???         IF (DAKT.LT.DBAS) DAKT=DBAS
             IF (DAKT.LT.0.05) DAKT=0.05
             DNEU = 0.5*(DVOR+DAKT)
             QNEU = 0.5*(QVOR+QAKT)
             WRITE (25,481) 'IAB=',IAB(I),' DNEU=',DNEU,' QNEU =',QNEU
             XV(JQX) = SIENG(DNEU,1)
             XV(JQX-1) = SIENG(QNEU,3)
C            Dämpfung am Knoten oberhalb
             I1 = I-1
             JQX1 = I1 * 2
             IF (XL(I1).GT.XL(I)) THEN
               DVOR = ENGSI(D2(I1),1)
               QVOR = ENGSI(Q2(I1),3)
               DAKT = ENGSI(XV(JQX1),1)
               QAKT = ENGSI(XV(JQX1 - 1),3)
               DBAS = ENGSI(DZERO(I1),1)
               QBAS = ENGSI(QZERO(I1),3)
C???           IF (DAKT.LT.DBAS) DAKT=DBAS
               IF (DAKT.LT.0.05) DAKT=0.05
               DNEU = 0.5*(DVOR+DAKT)
               QNEU = 0.5*(QVOR+QAKT)
               WRITE (25,481)'IAB=',IAB(I1),' DNEU=',DNEU,' QNEU =',QNEU
               XV(JQX1) = SIENG(DNEU,1)
               XV(JQX1-1) = SIENG(QNEU,3)
             END IF
C            Dämpfung am Knoten unterhalb
             I1 = I+1
             JQX1 = I1 * 2
             IF (XL(I1).LT.XL(I)) THEN
               DVOR = ENGSI(D2(I1),1)
               QVOR = ENGSI(Q2(I1),3)
               DAKT = ENGSI(XV(JQX1),1)
               QAKT = ENGSI(XV(JQX1 - 1),3)
               DBAS = ENGSI(DZERO(I1),1)
               QBAS = ENGSI(QZERO(I1),3)
C???           IF (DAKT.LT.DBAS) DAKT=DBAS
               IF (DAKT.LT.0.05) DAKT=0.05
               DNEU = 0.5*(DVOR+DAKT)
               QNEU = 0.5*(QVOR+QAKT)
               WRITE (25,481)'IAB=',IAB(I1),' DNEU=',DNEU,' QNEU =',QNEU
               XV(JQX1) = SIENG(DNEU,1)
               XV(JQX1-1) = SIENG(QNEU,3)
             END IF
           END IF
         END DO
       END IF

C      Ausgabe auf DHDQSTAT.dat
       WRITE(51,511)TAK,KNPOSMX,DHPOSMX,DQPOSMX,KNNEGMI,DHNEGMI,DQNEGMI, 
     &              KNDVPOSMX,DVPOSMX,KNDVNEGMI,DVNEGMI
  511  FORMAT (F10.3,2(I10,2F10.3),2(I10,F10.3))

C      Dämpfung über alle Knoten
C      derzeit abgeschaltet
       IF (IRELAXFOR.EQ.1.OR.IRELAXBACK.EQ.1) THEN
         DO I = 1,NX-1
           JQX = I * 2
           DVOR = ENGSI(D2(I),1)
           QVOR = ENGSI(Q2(I),3)
           DAKT = ENGSI(XV(JQX),1)
           QAKT = ENGSI(XV(JQX-1),3)
           DBAS = ENGSI(DZERO(I),1)
C???       IF (DAKT.LT.DBAS) DAKT=DBAS
           IF (DAKT.LT.0.05) DAKT=0.05
           DNEU = 0.5*(DVOR+DAKT)
           QNEU = 0.5*(QVOR+QAKT)
           XV(JQX) = SIENG(DNEU,1)
           XV(JQX-1) = SIENG(QNEU,3)
         END DO
       END IF
C
C      CHECK FOR INFEASIBLE FLOW CONDITIONS AFTER BACHWARD SWEEP
C
      FLAG120 = .FALSE.

      DO 410 I = 1,NX

         QMIT = 0.0
         FLAG = .FALSE.
         JQX = I * 2
         Q2(I) = XV(JQX - 1)
         D2(I) = XV(JQX)
c        QMIT=QMIT+Q2(I)
c        NMIT=1
         NMIT=0
         IF (I.GT.1.AND.XL(I-1).GT.XL(I)) THEN
            IABOW = IAB(I-1)
            ZOW = ZO(I-1)
            JQX = (I-1) * 2
            QOW = XV(JQX - 1)
            QMIT=QMIT+QOW
            NMIT=NMIT+1
            QOW = ENGSI(QOW,3)
            DOW = XV(JQX)
            HOW = DOW + ZOW
            HOW = ENGSI(HOW,1)
            DOW = ENGSI(DOW,1)
         ELSE
            KNOW = 0
            QOW = 0.0
            DOW = 0.0
            HOW = 0.0
            ZUW = ZO(I+1)
            DUW = XV(JQX)
            HUW = DUW + ZUW
            HMIT=HUW
         END IF
         IF (I.LT.NX.AND.XL(I+1).LT.XL(I)) THEN
            IABUW = IAB(I+1)
            ZUW = ZO(I+1)
            JQX = (I+1) * 2
            QUW = XV(JQX - 1)
            QMIT=QMIT+QUW
            NMIT=NMIT+1
            QUW = ENGSI(QUW,3)
            DUW = XV(JQX)
            HUW = DUW + ZUW
            HUW = ENGSI(HUW,1)
            DUW = ENGSI(DUW,1)
            HMIT=0.5*(HOW+HUW)
         ELSE
            KNUW = 0
            QUW = 0.0
            DUW = 0.0
            HUW = 0.0
            HMIT=HOW
         END IF
         HMIT=SIENG(HMIT,1)
         DMIT=HMIT-ZO(I)
         IF (DMIT.LT.0.05) DMIT=0.05
         QMIT=ABS(QMIT/REAL(NMIT))

C     CHECK FOR DEPTH LESS THAN 0.1 FT
C      IF DEPTH OUT OF RANGE, REDUCE TIME STEP
C
         IF (D2(I).GT.0.10) GOTO 300
C
C        Testsprung zu 310 eingefügt am 08.10.2008
C        damit Zeitschrittreduzierung vermieden wird
C
C        GOTO 310   rückgängig gemacht !!
C
         WRITE (N6,'(A)') 'TIEFE ZU KLEIN NACH BACKWARD SWEEP'

         OPEN (56,FILE='REDUZ.dat')
         WRITE (56,'(A)') '"ID" "IAB" "XL" "ZO" "D" "WSP" "Q" "QL" "QO"'

         DO I1= 1,NX
           JQX1= I1* 2
           D2$ = ENGSI(XV(JQX1),1)
           Q2$ = ENGSI(XV(JQX1 - 1),3)
           XL$ = ENGSI(XL(I1),1)
           ZO$ = ENGSI(ZO(I1),1)
           WS$ = ZO$ + D2$
           QL$ = QLAT1(I1)*DX(I1)
           QL$ = ENGSI(QL$,3)
           QO$ = QOFLOW(I1)*DX(I1)
           QO$ = ENGSI(QO$,3)
           WRITE (56,561) ID(I1),IAB(I1),XL$,ZO$,D2$,WS$,Q2$,QL$,QO$
  561      FORMAT (2I6,F10.1,6F10.2)
         END DO

         CLOSE (56,STATUS='KEEP')

C
         WRITE (N6,470)
         D2$ = ENGSI(D2(I),1)
         Q2$ = ENGSI(Q2(I),3)
         WRITE (N6,480) IABOW,DOW,QOW,DT
         WRITE (N6,480) IAB(I),D2$,Q2$,DT
         WRITE (N6,480) IABUW,DUW,QUW,DT

         WRITE (N6,*) '  HOW = ',HOW
         WRITE (N6,*) '  HUW = ',HUW
         WRITE (N6,*) '  ZOW = ',ENGSI(ZOW,1)
         WRITE (N6,*) '  ZUW = ',ENGSI(ZUW,1)
         WRITE (N6,*) ' HMIT = ',ENGSI(HMIT,1)
         WRITE (N6,*) ' DMIT = ',ENGSI(DMIT,1)

         IF (ITYPE(I).GE.4) THEN
            CALL YGRENZ4(I,QMIT,YGRZ,HMIN)
            IF (YGRZ.GT.DMIT) THEN
               DMIT=YGRZ
               WRITE (N6,*) ' YGRZ gesetzt = ',ENGSI(YGRZ,1)
            ELSE
               WRITE (N6,*) ' DMIT bleibt = ',ENGSI(DMIT,1)
            END IF
            D2(I) = DMIT
            XV(JQX) = D2(I)
            XV(JQX-1) = QMIT
         END IF

         STOP 'TIEFE ZU KLEIN, GESAMTAUSGABE AUF REDUZ.DAT BEACHTEN !!!'

         FLAG120=.TRUE.
c        GO TO 120

  300    CONTINUE
         IF (D2(I).GT.0.05) GO TO 310
         WRITE (N6,*) 'ITIME = ',ITIME
         WRITE (N6,*) '  DOW = ',DOW
         WRITE (N6,*) '  DUW = ',DUW
         WRITE (N6,*) ' YGRZ = ',ENGSI(YGRZ,1)
         WRITE (N6,*) ' QMIT = ',ENGSI(QMIT,3)
         WRITE (N6,301) IAB(I),D2(I)
  301    FORMAT (////1X,'WASSERTIEFE WIRD KLEINER ALS 0.05 FUSS BEI ABSC
     1HNITT',I5,'     TIEFE IST',F8.4,' FUSS')
         STOP 301

  310    CONTINUE

         H = D2(I) + ZO(I)

C        CALL SHAPE (I,H,A,XWP,TOP)

         CALL SHAPE2 (I,H,A,XWP,TOP,CH2,CHR,QGRENZ)
          
         if ( a .lt. 1e-5 .or. top .lt.1e-5 ) then
         write(*,*) ' TEST: Knoten ',iab(i),' A= ',a,'  TOP =',top,
     1   ' Q = ',Engsi(Q2(I),3)
         endif

         FROUDE = ABS(Q2(I))/(A * SQRT(GRAV * ABS(A/TOP)))

         IF (ABS(Q2(I)).GT.QGRENZ) THEN
            FROUDE2 = ABS(Q2(I))/QGRENZ
c           IF (I.NE.NX.AND.FROUDE2.GT.FROUDE) FROUDE=FROUDE2
            IF (I.NE.NX.AND.ITYPE(I).GE.4.AND.FROUDE2.GT.TOL) THEN
               WRITE(N6,*) 'IAB = ',IAB(I),'     Q2 > QGRENZ !'
               WRITE(N6,*) ENGSI(Q2(I),3),' > ',ENGSI(QGRENZ,3)
               WRITE(N6,*) 'IAB = ',IAB(I),' FROUDE2 = ',FROUDE2
               CALL YGRENZ4(I,ABS(Q2(I)),YGRZ,HMIN)
               D2(I) = YGRZ
               XV(JQX) = D2(I)
            END IF
         END IF

C        Umgehen des FROUDE-Kriteriums fr vollen Kreisquerschnitt
         IF (ITYPE(I).EQ.1.AND.D2(I).GE.HEIT(I)) THEN
c           write(n6,*)' VOLLES KREISROHR, FROUDE-Kriterium umgangen !'
            FROUDE=1.0
         END IF
         IF (ITYPE(I).EQ.2.AND.D2(I).GE.HEIT(I)) THEN
c           write(n6,*)' VOLLER DURCHLASS, FROUDE-Kriterium umgangen !'
            FROUDE=1.0
         END IF
C        Umgehen des FROUDE-Kriteriums fr Wehre
         DO NW=1,NWEIRS
            IF (I.EQ.IWEIR(NW)) FROUDE=1.0
         END DO
C
C*** FOR FROUDE NUMBERS .GE. TOL,  REDUCE DT.
C
         GOTO 410

C???     IF (FROUDE.LT.TOL) GOTO 410

         DO 700 JWEI=1,NWEHR
            IF (IWEHR(JWEI).EQ.I) GOTO 410
  700    CONTINUE

         WRITE(25,1311) IAB(I),ENGSI(H,1),ENGSI(A,2),ENGSI(TOP,1),
     1                  FROUDE,DT
 1311    FORMAT(1X,'I/H/A/TOP/FROUDE/ ',I4,5F8.2)

         DO 320 KK = 1,NQIN
            IF (I.EQ.NUP(KK)) GO TO 100
  320    CONTINUE

         IF (I.EQ.NX) GO TO 100
c  
         IF (FLAG) GO TO 100
C
C      Ersetze d2(i) durch ygrenz
C
         IF (LIST.GE.1) GO TO 330
         D2$ = ENGSI(D2(I),1)
         Q2$ = ENGSI(Q2(I),3)
         WRITE (IOUT,460) IAB(I),Q2$,D2$,FROUDE,DT
         WRITE (25,460) IAB(I),Q2$,D2$,FROUDE,DT
  330    CONTINUE
c
c?         IF (ITYPE(I).GE.4) THEN
c?            CALL YGRENZ4(I,Q2(I),YGRZ,HMIN)
c?            d2(i) = ygrz
c?         ELSE IF (ITYPE(I).EQ.2.OR.ITYPE(I).EQ.3) THEN
c?            call ygrenz(i,q2(i),ygrz,hmin)
c?            d2(i) = ygrz
c?         ELSE IF (ITYPE(I).EQ.1.AND.D2(I).LT.HEIT(I)) THEN
c?            call ygrenz(i,q2(i),ygrz,hmin)
c?            d2(i) = ygrz
c?         ELSE IF (ITYPE(I).EQ.1.AND.D2(I).GE.HEIT(I)) THEN
c?            write(n6,*)' VOLLES ROHR, YGRENZ nicht gesetzt !'
c?         END IF

  390    FLAG = .TRUE.
 
         XV(JQX) = D2(I)
 
         IF (NGATES.EQ.0.AND.NWEIRS.EQ.0) GO TO 300
C
C     RETURN FOR CORRECTION BEFORE CHECKING FROUDE NO.
C
C     GATES
C
         IF (ITIME.LE.1) GO TO 300
C
         IF(NGATES.EQ.0) GO TO 401
c        N = 99999
c        N2 = 999999
         N = 10000
         N2 = 20000
         DO 400 LOT = 1,NGATES
            IG1 = IGATE(LOT)
            IG2 = IG1 + 1
            IF ((I.NE.IG1).AND.(I.NE.IG2)) GO TO 400
            N = IG1
            N2 = IG2
            ILOT = LOT
  400    CONTINUE
         IF ((I.NE.N).AND.(I.NE.N2)) GO TO 401
         CALL GATE (N,ITIME,ILOT,'TEST  ')
         Q2(N) = XV(2 * N - 1)
         Q2(N + 1) = XV(2 * N + 1)
C
C     WEHRE
C
  401    IF(NWEIRS.EQ.0) GO TO 300
c        N = 99999
c        N2 = 999999
         N = 10000
         N2 = 20000
         DO 402 LOT = 1,NWEIRS
            IG1 = IWEIR(LOT)
            IG2 = IG1 + 1
            IF ((I.NE.IG1).AND.(I.NE.IG2)) GO TO 402
            N = IG1
            N2 = IG2
            ILOT = LOT
  402    CONTINUE
         IF ((I.NE.N).AND.(I.NE.N2)) GO TO 300
         CALL WEIR (N,ITIME,ILOT)
         Q2(N) = XV(2 * N - 1)
         Q2(N + 1) = XV(2 * N + 1)
         GO TO 300
C
  410 CONTINUE

      IF (FLAG120) GOTO 120

C
C     SUCCESSFUL RETURN OF NEW TIME STEP DEPTH AND DISCHARGE
C        HAS BEEN ACCOMPLISHED.
C      DTSAVE IS ORIGINAL TIME STEP;
C      DT IS CURRENT TIME STEP;
C      IF DT IS LESS THAN 1/2 OF DTSAVE, THE RESULTS ARE CONSIDERED
C      APPLICABLE FOR THE PREVIOUS TIME PERIOD AND COMPUTATIONS
C      ARE REPEATED FOR THE CURRENT TIME PERIOD.
C
      IF (DTIME) GO TO 420
      IF (DT.LT.(DTSAVE * 0.500)) ITIME = ITIME - 1
      IF (ITIME.LE.0) ITIME = 1
  420 CONTINUE
      IF (MTIME.EQ.ITIME) GO TO 430
      TIME = TIME + (DTSAVE/60.)
  430 CONTINUE
      DT = DTSAVE
      MTIME = ITIME
      RETURN
C
  440 FORMAT (1X,'BEI',I10,' FROUDE > TOL; ZEITSCHRITT NICHT REDUZIERT')
  450 FORMAT (1X,'BEI',I10,' FROUDE > TOL; GRENZTIEFE ANGESETZT')
  460 FORMAT (1X,'I=',I5,2X,'Q=',G10.4,2X,'D=',G10.4,2X,'FR=',G10.4,2X,'
     1DT=',F6.2,' SEC')
  470 FORMAT (1X,'ZEITSCHRITT REDUZIERT; TIEFE IST AUSSERHALB DES ZULAES
     1SIGEN BEREICHS')
  480 FORMAT (1X,'I=',I5,2X,'D=',G10.4,2X,'Q=',G10.4,'DT=',G10.4,' SEC')
      END

C      THIS FUNCTION SETS THE UPSTREAM INFLOWS

      REAL FUNCTION C11(ITIME,I)

      INCLUDE 'DIMENS.INC'

      COMMON /FLOWS/        NQIN    , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /NUMB/         LEAD
      COMMON /AB/           IAB(IPSEC)
      REAL      Q1(IPHYD)
      DO 100 K = 1,LEAD
  100 Q1(K) = QUP(K,I)
      C11 = Y(ITIME,Q1)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE TRA
C
C      COMPUTES NORMAL DEPTH CURVE FOR DOWNSTREAM SECTION
C      LIST OF DISCHARGES AND DIMENSIONS ARE BROUGHT IN
C      YO  IS INITIAL ESTIMATE OF DEPTH
C      DISCHARGES MUST BE ENTERED IN ASCENDING ORDER
C      A BISECTION SEARCH METHOD IS USED

      INCLUDE 'DIMENS.INC'

      COMMON /DOWNCO/       QQ(IPRAT)   , YY(IPRAT)   , NQ          ,
     1        IDOWN       , AAA         , BBB
      COMMON /DATA/         B           , Z1          , Z2          ,
     1        D           , RN          , S           , YO
      COMMON /AB/           IAB(IPSEC)
C
C      TERM  ACC  SPECIFIES TOLERANCE FOR AGREEMENT BETWEEN
C      COMPUTED AND ACTUAL VALUE OF A*R**.667
C
      ACC = 0.001
      IN = 15
      IO = 16
C     WRITE(IO,92) RN,S,B,Z1,Z2,D
C
C      D  IS DIAMETER FOR CIRCULAR PIPE
C
C      MUST CHECK FOR PIPE CAPACITY IF CLOSED CONDUIT
      IF (D.EQ.0.) GO TO 100
      QMAX = (1.486/RN) * ARTT(D) * SQRT(S)
  100 AR23 = (QQ(1) * RN/1.486)/SQRT(S)
      YL = YO
  110 IF (ARTT(YL).LT.AR23) GO TO 120
      YL = YL/2.
      GO TO 110
  120 YM = 2. * YL
      DO 170 I = 1,NQ
      IKON = 0
         IF (D.EQ.0.0) GO TO 130
         YH = D
         IF (QQ(I).GT.QMAX) GO TO 180
  130    AR23 = (QQ(I) * RN/1.486)/SQRT(S)
  140    A1 = ABS(ARTT(YM) - AR23)
         A2 = ACC * AR23
         IF (A1.LT.A2) GO TO 160
C
C      TRY NEW DEPTH
C
         ARV = ARTT(YM)/AR23
         IF (ARV.LT.1.) GO TO 150
         YH = YM
         YM = (YH + YL) * .5
         IKON = IKON + 1
         IF (IKON.GT.2000) STOP 9877
         GO TO 140
  150    YL = YM
         YM = YM / ARV
         IF (D.GT.0.) YM = (YH + YL) * 0.5
         GO TO 140
  160    CONTINUE
C
C      CONVERGENCE
C
         YY(I) = YM
         YL = YM
         YM = 2. * YM
C     WRITE(IO,93) QQ(I),YY(I)
  170 CONTINUE
      RETURN
  180 WRITE (IO,190)
      STOP 180
C
  190 FORMAT (1X,'ABFLUSS UEBERSTEIGT KAPAZITAET IN SUBROUTINE TRA')
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION ARTT(Y)
C
C      COMPUTATION OF A*R**2/3

      INCLUDE 'DIMENS.INC'

      COMMON /AB/           IAB(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /DATA/         B           , Z1          , Z2          ,
     1        D           , RN          , S           , YO
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH

C     IF (D.EQ.0.0) GO TO 100
      IF (ITYPE(NX).NE.1) GOTO 100
C
C      CIRCULAR SECTION
C
      IF (Y.GT.D) Y = D

      WRITE(N6,*) ' line 9139 ACOS'
      WRITE(N6,*) ' Y = ',Y,'  D = ',D 
      ARG = 1.0 - (2.0 * Y/D)
      WRITE(N6,*) ' ARG = ',ARG

      THETA = 2. * ACOS(1. - (2. * Y/D))
      IF (THETA.LT.0.0) THETA = 6.28318 + THETA
      A1 = D * D * (THETA - SIN(THETA)) * .125
      P1 = 0.5 * THETA * D
      GO TO 150

  100 IF (ITYPE(NX).NE.2) GO TO 105
C 100 IF (ITYPE(NX).EQ.4) GO TO 110
C
C      RECTANGULAR SECTION  ITYPE = 2
C
      A1 = B * Y
      P1 = B + Y * 2.
      GO TO 150

  105 IF (ITYPE(NX).NE.3) GO TO 110
C
C      TRAPEZOIDAL SECTION  ITYPE = 3
C
      A1 = B * Y + (Z1 + Z2) * 0.5 * Y * Y
      P1 = B + Y * (SQRT(Z1 *  * 2 + 1.) + SQRT(Z2 *  * 2 + 1.))
      GO TO 150
C
C      FREE SHAPED SECTION  ITYPE = 4 OR 5 OR 9
C
  110 IF (Y.GT.HEIT(NX)) Y = HEIT(NX)
      DH = HEIT(NX) / REAL(IPSTP-1)
      D1 = DH
      D2 = 0.
           DO 120 II=1,NX
           KS = II
           IF (LAB(KS).EQ.IAB(NX)) GO TO 130
  120      CONTINUE
      WRITE(N6,121) KS,LAB(KS),NX,IAB(NX)
  121 FORMAT (//1X,I3,'.UNREGELMAESSIGER QUERSCHNITT (NUMMER',I5,')'/
     1 1X,'STIMMT NICHT MIT BEZEICHNUNG DES UNTERSTEN KNOTENS UEBEREIN'/
     2 1X,'NX =',I5,'      IAB(NX) = ',I5,'  ---> A R T T')
      STOP 120
  130 DO 140 N = 2,IPSTP
      IF ((Y-D1) .GT. 0.001) GO TO 135

      FAK = (Y-D2) / DH
      A1 = FAK * ( ASEC(KS,N) - ASEC(KS,N-1) ) + ASEC(KS,N-1)
      P1 = FAK * ( WSEC(KS,N) - WSEC(KS,N-1) ) + WSEC(KS,N-1)
      GO TO 150

  135 D2 = D1
      D1 = D1 + DH
  140 CONTINUE

  150 R1 = A1/P1
      ARTT = A1 * (R1 *  * .667)
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION SIENG(W,I)

C DIESE FUNCTION WANDELT SI-EINHEITEN IN ANGELSAECHSISCHE EINHEITEN UM
C      W  =  UMZUWANDELNDE GROESSE
C      I  =  POTENZ DER EINHEIT FOOT
!
      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
!
      FT = 0.30479
!
      RM1 = 3.28095
      RM2 = 10.76463
      RM3 = 35.31822
!
c     IF (I.EQ.1) SIENG = W / FT
c     IF (I.EQ.2) SIENG = W * RM2
c     IF (I.EQ.3) SIENG = W * RM3
!
      IF (I.EQ.1) SIENG = W / FT
      IF (I.EQ.2) SIENG = W / FT / FT
      IF (I.EQ.3) SIENG = W / FT / FT / FT
!
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION ENGSI(W,I)

C DIESE FUNCTION WANDELT ANGELSAECHSISCHE EINHEITEN IN SI-EINHEITEN UM
C      W  =  UMZUWANDELNDE GROESSE
C      I  =  POTENZ DER EINHEIT FOOT
!
      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
!
      FT = 0.30479
!
      RFT1 = 0.3048
      RFT2 = 0.092896944
      RFT3 = 0.028314059
!
c     IF (I.EQ.1) ENGSI = W * RFT1
c     IF (I.EQ.2) ENGSI = W * RFT2
c     IF (I.EQ.3) ENGSI = W * RFT3
!
      IF (I.EQ.1) ENGSI = W * FT
      IF (I.EQ.2) ENGSI = W * FT * FT
      IF (I.EQ.3) ENGSI = W * FT * FT * FT
!
      RETURN
      END

C-----------------------------------------------------------------------
****************************************************************
*  UNTERPROGRAMM T R I D I                                     *
*  LOEST TRIDIAGONALMATRIX MIT THOMAS ALGORITHMUS              *
****************************************************************
      SUBROUTINE TRIDI(N)

      INCLUDE 'DIMENS.INC'
       
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /THOM/ A(IPGW,3),B(IPGW)
      A(1,3)=A(1,3)/A(1,2)
      B(1)=B(1)/A(1,2)
      DO 1 I=2,N
         I1=I-1
         A(I,2)=A(I,2)-A(I1,3)*A(I,1)
         A(I,3)=A(I,3)/A(I,2)
         B(I)=(B(I)-B(I1)*A(I,1))/A(I,2)
  1   CONTINUE
      N1=N-1
      DO 10 L=1,N1
         I=N-L
         I1=I+1
         B(I)=B(I)-A(I,3)*B(I1)
 10   CONTINUE
      DO 20 I=1,N
         HT(I)=B(I)
 20   CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE UNSTDY(VERSION,TITLE,DATIN,PLOT,FORM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC
CCCCC     THIS SUBROUTINE MAY BE CALLED FROM PROGRAM MAIN OR FROM AN
CCCCC     OPTIMIZATION PROGRAM WHICH SPECIFIES GATE TRAJECTORIES AND
CCCCC     LATERAL INFLOWS
CCCCC     THIS PROGRAM IS DESIGNED TO ROUTE HYDROGRAPHS THROUGH A SEWER
CCCCC     USING THE COMPLETE, DYNAMIC EQUATIONS OF UNSTEADY FLOW.  A FUL
CCCCC     IMPLICIT FINITE DIFFERENCE SCHEME IS USED TO SOLVE THE ST. VEN
CCCCC     EQUATIONS.  REQUIREMENTS FOR INPUT INCLUDE UPSTREAM, DOWNSTREA
CCCCC     INITIAL BOUNDARY CONDITIONS; CROSS SECTIONAL DATA FOR SEWER SY
CCCCC     LATERAL INFLOWS OR OUTFLOWS;  AND GATE LOCATIONS AND TRAJECTOR
CCCCC     THE PROGRAM WILL ALSO ROUTE FLOW THROUGH JUNCTIONS, ACCOUNTING
CCCCC     FOR STORAGE IN THE JUNCTION AND LOSSES IF NECESSARY.  INFLOW
CCCCC     HYDROGRAPHS AND LATERAL INFLOWS CAN BE READ FROM A FILE
CCCCC     PRODUCED BY KINEROS OR THE SAN FRANCISCO RUNOFF MODEL.
CCCCC     PRESENT DIMENS.INCIONS:
CCCCC                    IPSEC  SECTIONS
CCCCC                    IPHYD  HYDROGRAPH POINTS
CCCCC                             NOTE:  MAY INTERPOLATE BETWEEN THESE
CCCCC                    IPRAT  NUMBER OF POINTS FOR RATING CURVE
CCCCC                    IPGAT  GATE SECTIONS
CCCCC                    IPLAT  LATERAL INFLOW SECTIONS
CCCCC                    IPINF  INFLOW HYDROGRAPHS
CCCCC                    IPJUN  JUNCTIONS
CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

C&&&  =================================================================
      REAL  VRET, DUEB, ZRET
      REAL  KOR1, KOR2, KOR3
      LOGICAL   SCHALT, RETKN, AUSBORD
C
      COMMON /RET1/    NSPE(IPSEC,IPSPE), NSP(IPSEC,IPSPE), NRET,
     1                 NSTORE, NWERTE(IPSPE), ZETA(IPSPE),DGRENZ(IPSPE),
     2                 NVERB(IPSEC), INDEX(IPSEC)
      COMMON / RET3 /  VRET(IPSPE),ZRET(IPSPE),ZRETO(IPSPE),DUEB(IPSEC)
      COMMON / RET4 /     KOR1(IPSEC), KOR2(IPSEC), KOR3(IPSEC)
      COMMON / RETLOG /   SCHALT(IPSEC), RETKN(IPSEC), AUSBORD(IPSEC)
      COMMON / QRET/ QOX(IPSPE,IPHYD),QEXT(IPHYD,IPSPE),
     &               MODEXT(IPSPE),IEXCL(IPSPE),QLEA(IPHYD,IPSPE),
     &               QUESP(IPSPE,IPHYD)
      COMMON /RETO/ RETOUT(IPSPE)
c     COMMON /RND/     HR0(IPSEC),HR1(IPSEC),HR2(IPSEC),
c    &                            RN1(IPSEC),RN2(IPSEC)
      COMMON /RND/     HR0(IPSEC),HR(IPSEC,7),RNV(IPSEC,7)
      COMMON /QPARTS/ AHR0(IPSEC),AHR(IPSEC,7),WHR0(IPSEC),WHR(IPSEC,7),
     &                THR0(IPSEC),THR(IPSEC,7),QHR0(IPSEC),QHR(IPSEC,7)
C
      COMMON /KANTE / ZKANTE(IPSPE), BKANTE(IPSPE), CMUEKANTE(IPSPE),
     &                QGRKANTE(IPRAT,IPSPE),ZSPO(IPRAT,IPSPE),
     &                JSPUW(IPSPE)
C                     JSPUW(J) = ZEIGER AUF SPEICHER UNTERHALB; DORT WIRD
C                     DER OBERWASSERSTAND ZRETO(JSPUW) BEI RÜCKSTAU BERÜCKSICHTIGT
C
C&&&  =================================================================
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /HGR/          HGR(IPSEC)
      COMMON /AB/           IAB(IPSEC)
      COMMON /STEP/         DTSAVE
      COMMON /ERR/          TOL
      COMMON /LATER/        LATINF      , L1(IPLAT)   , CLOSS(2,IPLAT),
     1                      LATCOM(IPLAT), GWFLOW(IPLAT)
      COMMON /LIN/          QINFLO(IPSEC)
      COMMON /CREEP/        GINITL(IPGAT,IPPRC)
      COMMON /GATES/        IGATE(IPGAT) , NGATES
      COMMON /READ/         ITERAT
      COMMON /GATOLD/       GOLD(IPGAT)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /TIB/          DT    , TIME , ITOTH, ITOTM,TOTH,JTIME
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /NEWER/        QLAT1(IPSEC) , QOFLOW(IPSEC), DSR(IPSEC)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)
      COMMON /BNDRY/        FLOW        , DHH         , DHT         ,
     1        ZSILL
      COMMON /FLOWS/        NQIN   , NUP(IPINF)  , QUP(IPHYD,IPINF) ,
     1        CDOWN(IPHYD)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
      COMMON /WAVE/        HWELLE(IPOUT,IPWEL), QWELLE(IPOUT,IPWEL)
     1,             ITUN, IWEL, DTWEL, NWEL, KWEL(IPOUT), TOT, STIME
      COMMON /NAMES/        IWEIRE(IPWEI),IGATEE(IPGAT),L1E(IPLAT),
     1                      NXJE(IPJUN,7),NUPE(IPINF),KWELE(IPOUT)
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)

      COMMON /WEHR2/ NWEHR,IWEHR(IPWEI),IWRAUS(IPWEI)
      COMMON /AQUI/ DGW(2,IPLAT),IAQUI(2,IPLAT),NAQ,NGW,DTGW,NGV,
     1              GHA(2,IPLAT),GS(2,IPLAT),TRANS(IPAQU),POR(IPAQU),
     2              BETA(IPAQU), XGW(IPLAT)
      COMMON /PRECIP/       PE(IPPRC),NPREC,DTPREC
      COMMON /GWAT/ HGW(IPGW,IPSEC), HT(IPGW), QSTAT(2,IPLAT)
      COMMON /GWOUT/ XHGW(IPSEC),IS(IPSEC),IXGW(IPSEC)
      COMMON /BILAN/ QKNOTN(IPSEC),SQG(4,IPLAT),QOGES(IPPRC),
     1               QOSUM(IPSEC)
      COMMON /REGEN/         SUMN                                     
      COMMON /GANGLT/ TINC,TTEST,TPRIN
C
C
      COMMON /MODUSOF/ ZOF(IPSEC)
C
      COMMON /MODUSDF/ ZDF(IPSEC)
C
      COMMON /SOHLE/   ZSHIFT(IPSEC)
C
      DOUBLE PRECISION XK(IPSEC),YK(IPSEC)
      COMMON /XYKOORD/ XK,YK
C
      COMMON /GEWID/ ID(IPSEC)
C
      CHARACTER*6 OVFMOD
C
      COMMON /OVFBIL/ OVFMOD,NOVF(IPSEC),IABOVF(IPSEC),IOVF(IPSEC),
     1                QOFLSN(100),NOVFMX,LIRE(IPSEC),OVFAN(IPSEC),
     2                OVFAUS(IPSEC),IOVFSTAT(IPSEC),QOMAX(IPSEC),
     3                QREGEL(IPSEC),TOVFAN(IPSEC),TOVFAUS(IPSEC)
C                     wird WSP > OVFAN wird QOFLOW() berechnet
C                     wird WSP wieder < OVFAUS wird QOFLOW() nicht berechnet
C
C     REAL Q2(IPSEC), D2(IPSEC)
      COMMON /Q2D2/ Q2(IPSEC), D2(IPSEC)
C
      CHARACTER*6 KSTMOD
      COMMON /KSTIME/ KSTMOD,KRAUT(IPSEC),TIMHKS(50),FAKTKS(IPHYD,3),
     1                FAK(IPHYD)
C
C----------------------------------------------------------------------
      REAL QMAX(IPSEC),HMAX(IPSEC),VMAX(IPSEC),SFMAX(IPSEC)
      REAL QMXHR0(IPSEC),QMXHR(IPSEC,7)
      INTEGER       ITQMAX(IPSEC),STFLAG(100)
      REAL THQMAX(IPSEC)
      REAL HGWMAX(IPSEC)
      REAL DBAK(IPSEC,5),YD(5),XIT(5),RMSDA(IPSEC)
      REAL RMSDN(IPSEC)
      DIMENSION HWEHR(IPWEI), BWEHR(IPWEI), WEHRCO(IPWEI)
      DIMENSION HWUW(IPWEI), WEHRNEI(IPWEI), WEHRCCFAK(IPWEI)
      CHARACTER*80 TITLE
      CHARACTER*30 RETOUT
      CHARACTER*30 VERSION,DATIN
      CHARACTER*1  CH1,CH2,PLOT,FORM
      CHARACTER*300 C300
C     Hilfsstring für WRITECSV
      CHARACTER*4096 CL
      DATA QMXHR0 /IPSEC*0./
      DATA QMAX /IPSEC*0./
      DATA HMAX /IPSEC*0./
      DATA HGWMAX /IPSEC*0./

      DO IP=1,IPSEC
         DBAK(IP,1)=0.0
         DBAK(IP,2)=0.0
         DBAK(IP,3)=0.0
         DBAK(IP,4)=0.0
         DBAK(IP,5)=0.0
         QMXHR(IP,1)=0.0
         QMXHR(IP,2)=0.0
         QMXHR(IP,3)=0.0
         QMXHR(IP,4)=0.0
         QMXHR(IP,5)=0.0
         QMXHR(IP,6)=0.0
         QMXHR(IP,7)=0.0
      END DO

      IPLOT = 0
      DO 9 I=1,IPSEC
        DO 9 J=1,IPGW
        HGW(J,I)=0.
    9 CONTINUE
      IN = 15
      IOUT=16
      NGW=IPGW
      NGV=(NGW-1)/2+1
C   PLOTDATEIEN
      N77  = 77
      N88  =  88
      N99  =  99
      N10 = 10
      N11 = 11
      N12 = 12
      N20 = 20
      N21 = 21
      N25 = 25
c     WRITE (*,11)
c  11 FORMAT(/,1X,'SOLLEN PLOTDATEIEN ERSTELLT WERDEN (J/N)? ')
c     READ(*,12) PLOT
c     READ(17,12) PLOT
c     READ(17,12,END=13) FORM
c  12 FORMAT(A)
c  13 CLOSE(17,STATUS='KEEP')
c
      IF (PLOT.EQ.'j') PLOT='J'
      IF (PLOT.EQ.'J') THEN
       IF (FORM.EQ.'A') THEN
        OPEN (55,FILE='QOF.PLT')
        OPEN (N77,FILE='QCH.PLT')
        OPEN (N88,FILE='HCH.PLT')
CSCHI   OPEN (N99,FILE='QDR.PLT')
        OPEN (N99,FILE='QL1.PLT')
        OPEN (N10,FILE='WPE.PLT')
        OPEN (13,FILE='HGW.PLT')
        WRITE(*,*) 'QCH,HCH,QDR,WPE ALS ASCII-DATEIEN ER™FFNET !'
       ELSE IF (FORM.EQ.'B') THEN
        OPEN (55,FILE='QOF.PLT',FORM='UNFORMATTED')
        OPEN (N77,FILE='QCH.PLT',FORM='UNFORMATTED')
        OPEN (N88,FILE='HCH.PLT',FORM='UNFORMATTED')
CSCHI   OPEN (N99,FILE='QDR.PLT',FORM='UNFORMATTED')
        OPEN (N99,FILE='QL1.PLT',FORM='UNFORMATTED')
        OPEN (N10,FILE='WPE.PLT',FORM='UNFORMATTED')
        OPEN (13,FILE='HGW.PLT',FORM='UNFORMATTED')
        WRITE(*,*) 'QCH,HCH,QDR,WPE ALS BINŽR-DATEIEN ER™FFNET !'
       ELSE IF (FORM.EQ.'T') THEN
        OPEN (55,FILE='QOF.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        OPEN (N77,FILE='QCH.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        OPEN (N88,FILE='HCH.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
CSCHI   OPEN (N99,FILE='QDR.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        OPEN (N99,FILE='QL1.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        OPEN (N10,FILE='WPE.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        OPEN (13,FILE='HGW.PLT',ACCESS='TRANSPARENT',FORM='FORMATTED')
        WRITE(*,*) 'QCH,HCH,QDR,WPE ALS TRANS-DATEIEN ER™FFNET !'
       ELSE
          STOP 'AUSGABE ASCII ODER BINŽR ODER TRANS ?'
       ENDIF
      ENDIF

      OPEN (51,FILE='DHDQSTAT.dat',STATUS='UNKNOWN')
      WRITE(51,*)'"TH" "KPOS" "DHPOS" "DQPREL" "KNEG" "DHNEG" "DQNREL"',
     &           ' "KDVDTPOS" "DVDTPOS" "KDVDTNEG" "DVDTNEG"'

      OPEN (52,FILE='DHSFSTAT.dat',STATUS='UNKNOWN')
      WRITE(52,*)'"TIM" "KPOS" "DHSFPOS" "KNEG" "DHSFNEG"'

      IF (OVFMOD(1:3).EQ.'OVF') THEN
        OPEN (50,FILE='QOFLOW.dat',STATUS='UNKNOWN')
      END IF
       
      OPEN (N11,FILE='RAIN.PLT')
      OPEN (N12,FILE='QOVF.PLT')
      OPEN (N25,FILE='SYST.PLT')
      OPEN (29,FILE='BILA.OUT')
      OPEN (30,FILE='RETENT.PLT')
Calt  OPEN (43,FILE='PRODAT.PLT')
C
      ITERAT = ITERAT + 1
      IF (ITERAT.GT.1) GO TO 100
C
C     CALL SUBROUTINE TO READ IN INPUT
C
C
      WRITE(*,9001)
 9001 FORMAT(6(/),' Programm  H Y D R E T 0 6  arbeitet',/)
      WRITE(*,9002)
 9002 FORMAT(' Eingabedaten werden gelesen   ',/)

      CALL INPUT

      IF (PLOT.EQ.'J') THEN
         IF (FORM.EQ.'A') THEN
            WRITE(55,*) NX
            WRITE(N77,*) NX
            WRITE(N88,*) NX
            WRITE(N99,*) LATINF
            WRITE(N10,*) NX
            WRITE(13,*) NX
         ELSE IF (FORM.EQ.'B') THEN
            WRITE(55) NX
            WRITE(N77) NX
            WRITE(N88) NX
            WRITE(N99) LATINF
            WRITE(N10) NX
            WRITE(13) NX
         ELSE IF (FORM.EQ.'T') THEN
c            WRITE(55,'(A4)') NX
c            WRITE(N77,'(A4)') NX
c            WRITE(N88,'(A4)') NX
c            WRITE(N99,'(A4)') LATINF
c            WRITE(N10,'(A4)') NX
c            WRITE(13,'(A4)') NX
            WRITE(55) NX
            WRITE(N77) NX
            WRITE(N88) NX
            WRITE(N99) LATINF
            WRITE(N10) NX
            WRITE(13) NX
         ENDIF
      ENDIF

      DO 50 I=1,NX
      IS(I)=2
      IXGW(I)=1
      XHGW(I)=0.
         IF (LATINF.EQ.0) GO TO 50
         DO 49 J=1,LATINF
         IF (L1(J).NE.I) GOTO 49
         IF (LATCOM(J).NE.1) GOTO 50
         IF(XGW(J).LT.0.) IS(I)=1
         XX1=DGW(IS(I),J)/REAL(NGV-1)
         IF(XX1.GT.0.) THEN
         XX2=XGW(J)/XX1
         ELSE
         XX2=0.
         ENDIF
         IXGW(I)=INT(ABS(XX2)+0.5)+1
         XHGW(I)=REAL(IXGW(I)-1)*XX1
         IF(XGW(J).GE.0.) THEN
         IXGW(I)=IXGW(I)+NGV-1
         ELSE
         XHGW(I)=-XHGW(I)
         IXGW(I)=NGV-IXGW(I)+1
         ENDIF
         GOTO 50
   49    CONTINUE
   50 CONTINUE
      JTIME = 0

  100 CONTINUE

C
C      INITIALIZE ALL VALUES BEFORE ROUTING BEGINS
C
      DO 110 I = 1,NX
         QLAT1(I) = 0.0
         QOFLOW(I) = 0.
         Q2(I) = QZERO(I)
         D2(I) = DZERO(I)
C
C&&&  =================================================================
C
         VRET(I) = 0.0
         ZRET(I) = 0.0
         ZRETO(I) = 0.0
         DUEB(I) = 0.0
         SCHALT(I) = .TRUE.
         RETKN (I) = .FALSE.
         KOR1(I) = 0.0
         KOR2(I) = 0.0
         KOR3(I) = 0.0
         INDEX(I)= 0
C
         IF (NSTORE .EQ. 0)   GOTO 106
C
         DO 105   J = 1,NSTORE
C
C???       CALL WSTAND(VRET(J),J,DT,ITIME,ZRET(J),ZRETO(J),0)
C
           DO 104   K = 1,NVERB(J)
             IF (I .EQ. NSP(K,J))  THEN
               RETKN(I) = .TRUE.
               INDEX(I) = J
C
C alt          ZRET(J)  = WSTAND(VRET(J),J)
C
               GOTO 106
             END IF
  104      CONTINUE
  105    CONTINUE
  106    CONTINUE
C
C&&&  =================================================================
C
  110 CONTINUE

      ITIME = 0
C     CALL HYDPR (D2,Q2,ITIME)
      CALL HYDPR (ITIME,TIME,DT)
   
C     TIME =  - (DT/60.)
      TIME =  - (DT/60.) + STIME
      NN = NX * 2
      DTSAVE = DT
C     COEFFICIENTS IN THE MATRIX SOLUTION THAT STAY THE SAME. . .
C
      DO 120 I = 1,NQIN
         NSEC = NUP(I)
         JJ = 2 * NSEC - 1
         CO(JJ,1) = 0.
         CO(JJ,2) = 0.
         CO(JJ,3) = 1.
  120 CO(JJ,4) = 0.
      CO(NN,3) = 0.
      CO(NN,4) = 0.

      DO 130 I = 1,NL
         IF (ABS(ZS(I)).LE.0.0001) GO TO 129
         IF (D(I).LE.ZS(I)) GO TO 130
         DINT = ENGSI ( D(I),1 )
         ZSINT = ENGSI (ZS(I),1 )
         WRITE (N6,128) IAB(I),DINT,ZSINT
 128     FORMAT(//1X,'ANFANGSWASSERTIEFE BEI KNOTEN',I5,' ZU HOCH'/
     1            1X,'D(I) =',F8.3,' M      ZS(I) =',F8.3,' M')
C
C&&&  NACHFOLGENDE STOP-ANWEISUNG WURDE IN EINE SPRUNGANWEISUNG
C     GEAENDERT, UM DAS EICHEN DES BASISABFLUSSES ZU ERLEICHTERN
C     ANFANGSWASSERTIEFE WIRD GLEICH HOEHE DER UEBERLAUFKANTE
C     GESETZT
C                                                           15.3.1989
C     D(I) = ZS(I) - 0.01
C     STOP 128
      GOTO 130
C
  129    QOFLOW(I) = PUMP(DS,I)/DX(I)
  130 CONTINUE

      ITIME = 0
      IF (NGATES.GT.0) CALL STATER (ITIME + 1,GOLD)
C
C     SICHERN DER WEHRDATEN  ZU BEGINN DER SIMULATION
C
      NWEHR = NWEIRS
      WRITE(25,1240)
1240  FORMAT(1X,'TIME  WEHR-NR  KNOTEN    SCHWELLE  BREITE  WST-UW',
     1 '  WST-OW  H-SCHWELLE')
1241  FORMAT(1X,I4,4X,I4,3X,I6,4X,4F8.2,2X,F8.2)
      DO 1141 JWEI=1,NWEHR
      IWEHR(JWEI) = IWEIR(JWEI)
      HWEHR(JWEI) = HW(JWEI)
      BWEHR(JWEI) = BW(JWEI)
      WEHRCO(JWEI) = WCO(JWEI)
      WEHRNEI(JWEI) = WNEI(JWEI)
      IWRAUS(JWEI) = IRAUS(JWEI)
      WEHRCCFAK(JWEI) = CCFAK(JWEI)
      HWUW(JWEI) = 0.
      HWS = ZO(IWEIR(JWEI))+HW(JWEI)
      WRITE(25,1241) ITIME,JWEI,IAB(IWEIR(JWEI)),ENGSI(HW(JWEI),1),
     1 ENGSI(BW(JWEI),1),ENGSI(H(IWEIR(JWEI)+1),1),
     2 ENGSI(H(IWEIR(JWEI)),1),ENGSI(HWS,1)
1141  CONTINUE
C
C     THIS CODE INITIATES THE ROUTING ROUTINE
C     LOOP BACK THRU HERE FOR EACH TIME STEP.
C
      PRINT *
      WRITE(*,'(T2,2A)')'        HYDRET06-Version: ',VERSION
      PRINT *
      WRITE(*,'(T2,2A)')'    Simulation mit Datei: ',DATIN
      PRINT *
      WRITE(*,'(T2,A,I6)')'     Anzahl Zeitschritte: ',NT
      PRINT *
C

      ISTART=ITIME
      ISTVOR=0
      
C     BEGINN ZEITSCHLEIFE 
      
  140 CONTINUE

      ITIME = ITIME + 1

      ISTVOR=ISTAGE
      STAGE=100.*REAL(ITIME-ISTART)/REAL(NT-ISTART) + 0.5
      ISTAGE=INT(STAGE)
      IF (ISTAGE.LT.1) ISTAGE=1

      IF (ISTAGE.GT.ISTVOR) THEN
        WRITE(*,133) ISTAGE
 133    FORMAT(1X,'STAND = ',I3,' % VON 100 %')
      END IF

      ITIM = ITIME
C
C      WEHRE
C
       IF (NWEHR .EQ. 0) GOTO 1150

       DO 1148 JWEI = 1,NWEHR

       IJ = IWEHR(JWEI)+1
       WRITE(25,'(A,I4)') 'WEHR ',IAB(IJ-1)
       HUW = H(IJ)
       HOW = H(IJ-1)
       DALT = D(IJ-1)
       IF (HOW.LT.HUW.AND.Q(IJ-1).LE.0.0) THEN
          WRITE(25,'(A,I4)') '7865: FLIESSUMKEHR AM WEHR ',IAB(IJ-1)
C?        HUW = H(IJ-1)
C?        HOW = H(IJ)
          DALT = -D(IJ)
       END IF
       HWS = ZO(IJ-1)+HW(JWEI)
       QUO = (HUW-HWS)/(HOW-HWS)
C?     IF (QUO.GT.1.0) QUO=1.0
       IF (QUO.LT.0.0) QUO=0.0

C      UEBERPRUEFEN OB UNTERWASSERSTAND > WASSERSTAND BEI DEM
C      WEHR EINGESTAUT WURDE / FUER WEHRE, DIE NICHT EINGESTAUT
C      SIND GILT HWUW = 0.

       IF (H(IJ) .GE. HWUW(JWEI)) GOTO 1146

C      WEHR IST EINGESTAUT / UEBERPRUEFEN OB WASSERSTAND NACH
C      POLENI > BERECHNETEM WASSERSTAND
C
C
       QUOOLD2=QUO
       QUOALT2=QUO
       QUOOLD=QUO
       QUOALT=QUO
       QUOVOR=QUO
       QUOBAK=QUO
       HOLD2=HOW
       HALT2=HOW
       HOLD=HOW
       HALT=HOW
       HVOR=HOW
       HBAK=HOW
C
       HWBAK=HWEHR(JWEI)
       NITER=0
C       
 1160  CONTINUE

C       
       NITER=NITER+1
C       
       IF (NITER.GE.7) THEN
         QUOMI=(QUOOLD2+QUOALT2+QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/7.
       ELSE IF (NITER.EQ.6) THEN
         QUOMI=(QUOALT2+QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/6.
       ELSE IF (NITER.EQ.5) THEN
         QUOMI=(QUOOLD+QUOALT+QUOVOR+QUOBAK+QUO)/5.
       ELSE IF (NITER.EQ.4) THEN
         QUOMI=(QUOALT+QUOVOR+QUOBAK+QUO)/4.
       ELSE IF (NITER.EQ.3) THEN
         QUOMI=(QUOVOR+QUOBAK+QUO)/3.
       ELSE IF (NITER.EQ.2) THEN
         QUOMI=(QUOBAK+QUO)/2.
       END IF
C
       CALL POLENI ((IJ-1),DALT,Q(IJ-1),HWEHR(JWEI),BWEHR(JWEI),
     &             WEHRCO(JWEI),WEHRNEI(JWEI),DPOL,QUOMIT,
     &             WEHRCCFAK(JWEI))
C
C
C??    HHW = DPOL + ZO(IJ-1)
       HHW = DPOL + HWS
C
C      DPOL ist Ergebnis von POLENI incl. HWEHR(JWEI)
C      DALT ist die alte Tiefe
C      HWS = Wehrschwellenhöhe z.B. in m+NN
C
C      HWS = neue Wehrschwelle
       HWS = ZO(I) + HWEHR(JWEI)
C      HHW = neuer Wasserstand!
       HHW = DPOL + ZO(I)
C
C
C      Hier Nutzung von WH(JWEI,IPSTP) und WQ(JWEI,IPSTP) eingefügt!
C
       IF (WHQDAT(JWEI)(1:3).EQ.'H-Q') THEN
         DO ISTP=1,(IPSTP-1)
           WQU=WQ(JWEI,ISTP)
           WQO=WQ(JWEI,ISTP+1)
           WQI=Q(I)
           IF (WQI.GT.WQU.AND.WQI.LE.WQO) THEN
             WHU=WH(JWEI,ISTP)
             WHX=WHU
             WHO=WH(JWEI,ISTP+1)
             IF (WHO.GT.WHX) WHX=WHO
             IF (WHU.GT.0.AND.WHO.GT.0) THEN
               WRITE (25,'(A,F8.3)') ' WQO=',ENGSI(WQO,3)
               WRITE (25,'(A,F8.3)') ' WQI=',ENGSI(WQI,3)
               WRITE (25,'(A,F8.3)') ' WQU=',ENGSI(WQU,3)

               WHI=WHU+((WHO-WHU)/(WQO-WQU))*(WQI-WQU)

               WRITE (25,'(A,F8.3)') ' WHO=',ENGSI(WHO,1)
               WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
               WRITE (25,'(A,F8.3)') ' WHU=',ENGSI(WHU,1)

               IF (WHI.GT.WHX) THEN
                 WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
                 WRITE (25,'(A,F8.3)') ' WHX=',ENGSI(WHX,1)
                 STOP
               END IF
             ELSE
               IF (HHW.LT.WH(JWEI,1)) THEN
                 WHI=WH(JWEI,1)
               ELSE
                 WHI=0.0
                 GOTO 1165
               END IF
             END IF

C            Hier wird D(I) ggf. neu belegt!
             IF (WHI.GT.0) THEN
               IF (WHI.LT.HHW) THEN
                 WRITE (25,'(A)') ' WHI < HHW !'
                 WRITE (25,'(A,F8.3)') ' WHI=',ENGSI(WHI,1)
                 WRITE (25,'(A,F8.3)') ' HHW=',ENGSI(HHW,1)
                 WHI = (WHI + HHW)/2.
               END IF
               DHWTMP = WHI - HHW
               WRITE (25,'(A,F8.3)') 'DHWTMP =',ENGSI(DHWTMP,1)
               HHW = WHI
               D(I) = WHI - ZO(I)
               HBAK=HHW
               HWEHR(JWEI) = HWEHR(JWEI) + DHWTMP
               WRITE (25,'(A,F8.3)')'HWEHR(JWEI) =',ENGSI(HWEHR(JWEI),1)
             ELSE
               HWEHR(JWEI) = HWBAK
             END IF
           END IF
         END DO
 1165    CONTINUE
       END IF
C
C
       IF (DALT.GT.0.0.AND.HHW.LT.HUW) THEN
C         Korrektur für normale Fließrichtung bei massivem Rückstau
          HHW = HUW
       ELSE
          IF (NITER.GE.7) THEN
            HHW = (HOLD2+HALT2+HOLD+HALT+HVOR+HBAK+HHW)/7.
          ELSE IF (NITER.EQ.6) THEN
            HHW = (HALT2+HOLD+HALT+HVOR+HBAK+HHW)/6.
          ELSE IF (NITER.EQ.5) THEN
            HHW = (HOLD+HALT+HVOR+HBAK+HHW)/5.
          ELSE IF (NITER.EQ.4) THEN
            HHW = (HALT+HVOR+HBAK+HHW)/4.
          ELSE IF (NITER.EQ.3) THEN
            HHW = (HVOR+HBAK+HHW)/3.
          ELSE IF (NITER.EQ.2) THEN
            HHW = (HBAK+HHW)/2.
          END IF
       END IF
       QUO = (HUW-HWS)/(HHW-HWS)
       IF (QUO.GT.1.0) THEN
C?        QUO=1.0
          HHW = HUW
       END IF
       IF (QUO.LT.0.0) QUO=0.0
       DQUO = ABS(QUOBAK-QUO)
       HOLD2=HALT2
       HALT2=HOLD
       HOLD=HALT
       HALT=HVOR
       HVOR=HBAK
       HBAK=HHW
       QUOOLD2=QUOALT2
       QUOALT2=QUOOLD
       QUOOLD=QUOALT
       QUOALT=QUOVOR
       QUOVOR=QUOBAK
       QUOBAK=QUO

       IF (NITER.LT.3) GOTO 1160
       IF (NITER.LT.30.AND.DQUO.GT.0.0005) GOTO 1160
       WRITE(25,'(A,F10.4)') 'QUOMIT = ',QUOMI
C
C
C
Calt   IF (H(IJ).GE. HHW) GOTO 1146
       IF (H(IJ-1).GE. HHW) GOTO 1146

C      WASSERSTAND NACH POLENI > BERECHNETER WASSERSTAND
C      ==> WEHR WIRKSAM, WENN NICHT EINGESTAUT !!!

       IF (HUW.GE.HWS) THEN
C         UNTERWASSER šBER SCHWELLE
C         EINSTAUGRAD WIRD BERECHNET

          QUO = (HUW-HWS)/(HHW-HWS)
          WRITE(25,1246) IAB(IJ-1),ENGSI(HHW,1),ENGSI(HUW,1),
     1    ENGSI(Q(IJ-1),3),QUO
 1246     FORMAT(1X,'WEHR ',I4,'/ HPOL = ',F6.2,'/ H-UW = ',F6.2,
     1    '/  Q  = ',F6.2,'/ QUO = ',F5.2)

C         QUO = (HOW-HWS)/(HHW-HWS)
C         WRITE(25,1246) IAB(IJ-1),ENGSI(HHW,1),ENGSI(HOW,1),
C    1    ENGSI(Q(IJ-1),3),QUO
C1246     FORMAT(1X,'1246: WEHR ',I4,'/ HPOL = ',F6.2,'/ H-OW = ',F6.2,
C    1    '/  Q  = ',F6.2,'/ QUO = ',F5.2)

C??       IF (QUO.GE.0.95) GOTO 1146
C?        IF (QUO.GT.1.0) QUO=1.0
          IF (QUO.LT.0.0) QUO=0.0
          IF (QUO.GT.1.00) GOTO 1146

       END IF

       NWEIRS = NWEIRS+1

       WRITE(25,1243) IAB(IWEHR(JWEI))
1243   FORMAT(2X,'1243: WEHR ',I5,'  WIEDER RUECKSTAUFREI')
       HWUW(JWEI) = 0.
       JJ = 0
1143   JJ = JJ+1
       IF (JJ.GT.IPWEI) THEN
          PRINT '(T2,A,I5)','NWEIRS = ',NWEIRS
          PRINT '(T2,A,I5)','IPWEI = ',IPWEI
          PRINT '(T2,A,I5)','   JJ = ',JJ
          STOP 'UNSTDY: JJ > IPWEI !'
       END IF
       IF (NWEIRS .EQ. 1) GOTO 1145
       IF (IWEIR(JJ).GT.0.AND.IWEIR(JJ).LT.IWEHR(JWEI)) THEN
          WRITE(25,'(T2,A,I5)') '     NWEIRS = ',NWEIRS
          WRITE(25,'(T2,A,I5)') '  IWEIR(JJ) = ',IWEIR(JJ)
          WRITE(25,'(T2,A,I5)') 'IWEHR(JWEI) = ',IWEHR(JWEI)
c         PRINT '(T2,A,I5)','  IWEIR(JJ) = ',IWEIR(JJ)
c         PRINT '(T2,A,I5)','IWEHR(JWEI) = ',IWEHR(JWEI)
          GOTO 1143
       END IF
       DO 1144 JJJ=NWEIRS,JJ+1,-1
          HW(JJJ) = HW(JJJ-1)
          BW(JJJ) = BW(JJJ-1)
          WCO(JJJ) = WCO(JJJ-1)
          WNEI(JJJ) = WNEI(JJJ-1)
          IRAUS(JJJ) = IRAUS(JJJ-1)
1144      IWEIR(JJJ)=IWEIR(JJJ-1)
1145   CONTINUE
       IWEIR(JJ)=IWEHR(JWEI)
       HW(JJ) = HWEHR(JWEI)
       BW(JJ) = BWEHR(JWEI)
       WCO(JJ) = WEHRCO(JWEI)
       WNEI(JJ) = WEHRNEI(JWEI)
       IRAUS(JJ) = IWRAUS(JWEI)
       CCFAK(JJ) = WEHRCCFAK(JWEI)
       GOTO 1148

1146   CONTINUE

C      UEBERPRUEFEN OB WEHR EINGESTAUT WIRD
C      WENN HWUW .NE. 0. ==> WEHR IST BEREITS EINGESTAUT

       IF (HWUW(JWEI).NE.0.) GOTO 1148

       IF (HUW .LT. HWS ) GOTO 1148

C      UNTERWASSERSTAND > SCHWELLENHOEHE DES WEHRES ==>
C      EINSTAUGRAD UEBERPRUEFEN

       QUO = (HUW-HWS)/(HOW-HWS)
C?     IF (QUO.GT.1.0) QUO=1.0
       IF (QUO.LT.0.0) QUO=0.0
       IF (HOW.LE.HWS.AND.HUW.LE.HOW) QUO=0.0
       IF (HOW.LE.HWS.AND.HUW.GT.HOW) QUO=1.0

      WRITE(25,1247) IAB(IJ-1),ENGSI(HOW,1),ENGSI(HUW,1),
     1               ENGSI(Q(IJ-1),3),QUO
1247  FORMAT(1X,'1247: WEHR ',I4,'/ H-OW = ',F6.2,'/ H-UW = ',F6.2,
     1 '/  Q  = ',F6.2,'/ QUO = ',F5.2)

C???   IF (QUO.LT.0.95) GOTO 1148
C      IF (QUO.LE.1.00) GOTO 1148
C      am 05.10.2007 geändert:
       IF (QUO.LE.0.99.AND.Q(IJ-1).GT.0.0) GOTO 1148

       IF (IWRAUS(JWEI).EQ.0) GOTO 1148
C         RAUSSCHMISS NICHT ZULŽSSIG ==> 1148

C      EINSTAUGRAD >= 99 %
C      WEHR IST EINGESTAUT==> WIRD UNWIRKSAM

       NWEIRS = NWEIRS-1
       HWUW(JWEI) = H(IJ)

       WRITE(25,1248) IAB(IWEHR(JWEI))
1248   FORMAT(2X,'1248: WEHR ',I5,'  IM RUECKSTAU ==> RAUSSCHMISS! ')

       IF (NWEIRS .EQ. 0) GOTO 1148

       DO 1147 JJ=1,NWEIRS
          IF (IWEIR(JJ).LT.IWEHR(JWEI)) GOTO 1147
          HW(JJ) = HW(JJ+1)
          BW(JJ) = BW(JJ+1)
          WCO(JJ) = WCO(JJ+1)
          WNEI(JJ) = WNEI(JJ+1)
          IRAUS(JJ) = IRAUS(JJ+1)
          IWEIR(JJ) = IWEIR(JJ+1)
1147   CONTINUE

1148   CONTINUE

1150   CONTINUE

C      AUSDRUCKEN DER WIRKSAMEN WEHRE

       DO 1151 JWEI=1,NWEIRS
          HWS = ZO(IWEIR(JWEI))+HW(JWEI)
          WRITE(25,1241) ITIME,JWEI,IAB(IWEIR(JWEI)),ENGSI(HW(JWEI),1),
     1    ENGSI(BW(JWEI),1),ENGSI(H(IWEIR(JWEI)+1),1),
     2    ENGSI(H(IWEIR(JWEI)),1),ENGSI(HWS,1)
          IF (IAB(IWEIR(JWEI)).EQ.0) THEN
             WRITE(25,*) 'Das JWEI-te Wehr fehlt in der Knotentabelle!'
             WRITE(25,*) '  JWEI = ',JWEI
             WRITE(25,*) ' IWEIR = ',IWEIR(JWEI)
             STOP 'Es fehlt ein Wehr in der Knotentabelle!'
          END IF
1151   CONTINUE

C
C      SET UPSTREAM FLOWS
C
      DO 150 I = 1,NQIN
         NSEC = NUP(I)
         JJ = 2 * NSEC - 1
  150 E(JJ) = C11(ITIME,I)
C
C     SET LATERAL INFLOWS FOR THE MODEL.
C
C
      IF (ITIME.GT.1) GOTO 155
      IF (LATINF.EQ.0) GOTO 155
      IF (NAQ.EQ.0) GOTO 155
C
      CALL GROUND(ITIME)
C
  155 HYDRO = C11(ITIME,1)
      CALL LATRL (HYDRO,NX,ITIME)
C
C      OBTAIN DOWNSTREAM BOUNDARY CONDITIONS
C
      CALL DOWN (NN,ITIME)
C
C      TEST IS ROUTING ROUTINE; ALSO CHECKS FOR CORRECTIONS
C      WHICH NEED TO BE MADE
C
C     CALL TEST (TOL,ITIME,Q2,D2)
      CALL TEST (TOL,ITIME)
C
C     FILL COMMON BLOCK  DM  WITH VALUES FROM CURRENT TIME PERIOD

C     CALL HYDPR (D2,Q2,ITIME)
      CALL HYDPR (ITIME,TIME,DT)
C
      IF (ITIM .NE. ITIME ) GOTO 140
C
C     ABSPEICHERN DER ABFLUSS- UND WASSERSTANDSWERTE FUER SPAETEREN
C     GANGLINIENAUSDRUCK
C
      IF(MOD(ITIME-1,IWEL).NE.0)  GO TO 170
      IW = (ITIME-1) / IWEL + 1
      DO 160 JW = 1,NWEL
      KW = KWEL(JW)
      IF (JW.GT.IPOUT) THEN
         PRINT '(T2,A,I5,A)',' JW = ',JW,' > IPOUT !'
         STOP
      END IF
      IF (IW.GT.IPWEL) THEN
         PRINT '(T2,A,I5,A)',' IW = ',IW,' > IPWEL !'
         STOP
      END IF
      IF (KW.GT.IPSEC) THEN
         PRINT '(T2,A,I5,A)',' KW = ',KW,' > IPSEC !'
         STOP
      END IF
      IF (KW.GT.0) THEN
         HWELLE(JW,IW) = ENGSI (H(KW),1)
         QWELLE(JW,IW) = ENGSI (Q(KW),3)
      END IF
  160 CONTINUE
C
C
C      PRINTOUT RESULTS FOR CURRENT TIME PERIOD
  170 CALL OUTPT (ITIME)
C
      IF (LATINF.EQ.0) GOTO 179
      IF (NAQ.EQ.0) GOTO 179
      IF (ITIME.EQ.1) GOTO 179
C
      CALL GROUND(ITIME)
C
C     RAUSSCHREIBEN AUF DATEIEN FUER PLOTS
C      N 7  - ABFLUESSE IM GEWAESSER (CBM/SEC)
C      N 8  - WASSERSTAENDE IM GEWAESSER (M+NN)
C      N 9  - ZUFLUESSE AUS DEM GRUNDWASSER (CBM/SEC/M)
C      N10  - GRUNDWASSERSTAENDE (M+NN)
C
  174 FORMAT(I5)
  175 FORMAT(5(I4,F8.2,' ³'))
  176 FORMAT(5(I4,F8.3,' ³'))
  177 FORMAT(4(I4,E11.3,' ³'))
  179 KNGW=21
C
C     ERMITTELN DER "QPARTS"
C
      DO 201 I=1,NX
         IF (HR0(I).GT.0.0) THEN
            DHI=H(I)-HR0(I)
            IF (DHI.GT.0.0) THEN
               AI=AHR0(I)+DHI*THR0(I)
               RKSI=1/RNI(I)
               IF (WHR0(I).GT.0) THEN
                  RHYI=AI/WHR0(I)
               ELSE
c                 PRINT '(T2,A,I5)',' IAB(I) = ',IAB(I)
c                 PRINT '(T2,A)',' WHR0(I) < 0 !'
C                 STOP
               END IF
               IF (RHYI.LT.0) THEN
                  STOP 'UNSTDY LINE 619: RHYI < 0 !'
               END IF
               IF (SF(I).LT.0) THEN
                  QHR0(I)=-1.*AI*RKSI*RHYI**(2./3.)*ABS(SF(I))**(1./2)
               ELSE
                  QHR0(I)=AI*RKSI*RHYI**(2./3.)*SF(I)**(1./2)
               END IF
               IF (QHR0(I).GT.Q(I)) QHR0(I)=Q(I)
            ELSE
               QHR0(I)=Q(I)
            END IF
         ELSE
            QHR0(I)=0.0
         END IF
         IF (QHR0(I).GT.QMXHR0(I)) QMXHR0(I)=QHR0(I)
         DO 202 N=1,7
            IF (HR(I,N).GT.0.0) THEN
               DHI=H(I)-HR(I,N)
               IF (DHI.GT.0.0) THEN
                 AI=AHR(I,N)+DHI*THR(I,N)
                 RKSI=1/RNV(I,N)
                 IF (WHR(I,N).GT.0) THEN
                    RHYI=AI/WHR(I,N)
                 ELSE
c                   PRINT '(T2,A,I5)',' IAB(I) = ',IAB(I)
c                   PRINT '(T2,A)',' WHR(I,N) < 0 !'
C                   STOP
                 END IF
                 IF (RHYI.LT.0) THEN
                    STOP 'UNSTDY LINE 648: RHYI < 0 !'
                 END IF
                 IF (SF(I).LT.0) THEN
                   QHR(I,N)=-1.*AI*RKSI*RHYI**(2./3.)*ABS(SF(I))**(1./2)
                 ELSE
                   QHR(I,N)=AI*RKSI*RHYI**(2./3.)*SF(I)**(1./2)
                 END IF
                 IF (QHR(I,N).GT.Q(I)) QHR(I,N)=Q(I)
               ELSE
                 QHR(I,N)=Q(I)
               END IF
            ELSE
               QHR(I,N)=0.0
            END IF
            IF (QHR(I,N).GT.QMXHR(I,N)) QMXHR(I,N)=QHR(I,N)
  202    CONTINUE
  201 CONTINUE
C
      IF (PLOT.EQ.'J'.AND.ABS(TTEST-TPRIN).LT.0.001) THEN
C
         IPLOT = IPLOT +1
         WRITE(25,'(A,I5,A)') 'IPLOT = ',IPLOT,' AUSGABE AKTIV !'
         IF (FORM.EQ.'A') THEN
            WRITE(55,174) IPLOT
            WRITE(55,175) (IAB(LW),ENGSI((DX(LW)*QOFLOW(LW)),3),LW=1,NX)
            WRITE(N77,174) IPLOT
            WRITE(N77,175) (IAB(LW),ENGSI(Q(LW),3),LW=1,NX)
            WRITE(N88,174) IPLOT
            WRITE(N88,176) (IAB(LW),ENGSI(H(LW),1),LW=1,NX)
            WRITE(N99,174) IPLOT
            WRITE(N99,177) (L1E(LW),GWFLOW(LW),LW=1,LATINF)
            WRITE(N10,174) IPLOT
            WRITE(N10,176) (IAB(LW),ENGSI(WP(LW),1),LW=1,NX)
            WRITE(13,174) IPLOT
            WRITE(13,176) (IAB(LW),HGW(IXGW(LW),LW),LW=1,NX)
         ELSE IF (FORM.EQ.'B') THEN
            WRITE(55) IPLOT
            WRITE(55) (IAB(LW),ENGSI((DX(LW)*Q(LW)),3),LW=1,NX)
            WRITE(N77) IPLOT
            WRITE(N77) (IAB(LW),ENGSI(Q(LW),3),LW=1,NX)
            WRITE(N88) IPLOT
            WRITE(N88) (IAB(LW),ENGSI(H(LW),1),LW=1,NX)
            WRITE(N99) IPLOT
            WRITE(N99) (L1E(LW),GWFLOW(LW),LW=1,LATINF)
            WRITE(N10) IPLOT
            WRITE(N10) (IAB(LW),ENGSI(WP(LW),1),LW=1,NX)
            WRITE(13) IPLOT
            WRITE(13) (IAB(LW),HGW(IXGW(LW),LW),LW=1,NX)
         ELSE IF (FORM.EQ.'T') THEN
            WRITE(55) IPLOT
            WRITE(55) (IAB(LW),ENGSI((DX(LW)*Q(LW)),3),LW=1,NX)
            WRITE(N77) IPLOT
            WRITE(N77) (IAB(LW),ENGSI(Q(LW),3),LW=1,NX)
            WRITE(N88) IPLOT
            WRITE(N88) (IAB(LW),ENGSI(H(LW),1),LW=1,NX)
            WRITE(N99) IPLOT
            WRITE(N99) (L1E(LW),GWFLOW(LW),LW=1,LATINF)
            WRITE(N10) IPLOT
            WRITE(N10) (IAB(LW),ENGSI(WP(LW),1),LW=1,NX)
            WRITE(13) IPLOT
            WRITE(13) (IAB(LW),HGW(IXGW(LW),LW),LW=1,NX)
         ENDIF
      ENDIF
C
C
C     BESTIMMEN DES MAXIMALEN WASSERSTANDES , DES MAXIMALEN ABFLUSSES,
C     DES MAXIMALEN GW-STANDES UND DER UEBERLAUFSUMME AN DEN KNOTEN
      QOGEB = 0.
C
      DO 205 I=1,NX
         QOHALT=QOFLOW(I)*DX(I)
         IF (I.GE.KWEL(1) .AND. I.LE.KWEL(NWEL)) THEN
            QOGEB = QOGEB+ENGSI(QOHALT,3)
         ENDIF
         QOSUM(I)=QOSUM(I)+ENGSI(QOHALT,3)
         HMNN=ENGSI(H(I),1)
         QCBM=ENGSI(Q(I),3)
         IF (HMNN.GT.HMAX(I)) HMAX(I)=HMNN
         IF (HGW(IXGW(I),I).GT.HGWMAX(I)) THEN
            HGWMAX(I)=HGW(IXGW(I),I)
         ENDIF
         IF (ABS(QCBM).GT.QMAX(I)) THEN
            QMAX(I)=QCBM
            VMAX(I)=ENGSI(U(I),1)
            SFMAX(I)=SF(I)*1000.
            ITQMAX(I)=ITIME
            THQMAX(I)=ITIME*DT/3600.
         ENDIF
  205 CONTINUE
C
      QOGES(ITIME)=-QOGEB
C
C     ABSPEICHERN DES WASSERTIEFEN UND DURCHFLÜSSE
C
C
C     ABSPEICHERN DER WASSERTIEFEN AUS DEN LETZTEN 5 ZEITSCHRITTEN
C
      NWIN=5
C
      DO 206 I=1,NX

c        JQX = I * 2

         DMET=ENGSI(D(I),1)
         DO 207 NW=2,NWIN
            DBAK(I,NW-1)=DBAK(I,NW)
  207    CONTINUE
         DBAK(I,NWIN)=DMET

  206 CONTINUE
C
C     LINEARE REGRESSIONSBERECHNUNG FšR STABILITŽTSTEST
C
      IF (ITIME.LT.5) THEN
         DO I=1,NX
            RMSDN(I)=0.0
            RMSDA(I)=RMSDN(I)
         END DO
      ELSE IF (ITIME.GE.5) THEN
         DO I=1,NX
            IF (I.GT.1) RMSDA(I)=RMSDN(I)
            DO 209 NW=1,NWIN
               XIT(NW)=REAL(NW)
               YD(NW)=DBAK(I,NW)
  209       CONTINUE
            CALL REGRES (NWIN,XIT,YD,XITM,YDM,A,B,R,SQ,UCL,OCL,SIGNI)
            RMSDN(I)=SQRT(SQ/NWIN)
            IF (ITIME.GE.6) THEN
               RMSDD=RMSDN(I)-RMSDA(I)
               RMSDDR=100.*RMSDD/YDM
               IF (RMSDDR.GT.5.0) THEN
C                AUSGABE AUF DATEI 'SYST.PLT'
                 STFLAG(ISTAGE)=1
                 WRITE (N25,5050) ITIME,IAB(I),YDM,B,R,RMSDDR
 5050            FORMAT('STABIL',I5,2X,I5,F9.3,2X,F9.4,2X,F6.3,2X,F9.3)
               END IF
            END IF
         END DO
      END IF
C                                                                       
C      ABFLUSSFUELLE AM KNOTEN / SEITLICHER ZUFLUSS                     
C                                                                       
      DO 2005 IZAEHL = KWEL(1),KWEL(NWEL)                               
c       DO 2006 KZAEHL = 1,LATINF                                       
c         IF(L1(KZAEHL).EQ.IZAEHL) THEN                                 
c           SUMQG(IZAEHL) = SUMQG(IZAEHL) + ENGSI(QINFLO(IZAEHL),3)*DT  
c           GOTO 2005                                                   
c         END IF                                                        
c2006   CONTINUE
        QKNOTN(IZAEHL) = QKNOTN(IZAEHL)+ENGSI(Q(IZAEHL),3)*DT           
 2005  CONTINUE
C
C     GO TO NEXT TIME STEP.
C
      IF (ITIME.LT.NT) GO TO 140
C
C     AUSDRUCK DER BILANZEN
C     ZUVOR ABZIEHEN DER FLšSSE DES LETZTEN ZEITSCHRITTS, DAMIT DIE
C     BILANZ FšR DIE SIMULATIONSDAUER STIMMT !
C
      DO 2007 IZAEHL = KWEL(1),KWEL(NWEL)                               
         QKNOTN(IZAEHL) = QKNOTN(IZAEHL)-ENGSI(Q(IZAEHL),3)*DT
 2007 CONTINUE
C
      CALL BILA
C
C     AUSDRUCK DER MAXIMALEN WASSERSTAENDE UND ABFLUESSE SOWIE
C      GESCHWINDIGKEIT UND UEBERLAUFMENGE FUER ALLE KNOTEN
C
      WRITE(N6,330)
      WRITE(N6,291)
      WRITE(N6,292) TITLE
      WRITE(N6,341)
      WRITE(N6,301)
      WRITE(N6,302)
      WRITE(N6,341)

Calt  REWIND 43
C
      OPEN (N20,FILE='WSP.PLT')

      OPEN (47,FILE='Wsp.dat')

      C300='"ID","KNO","TYP","STAT","SOHL","UF_L","UF_R","WSP","Q","V"'
      C300=C300(1:58)//',"AVOL","I1000","HEL","HGR","HWSPORI","HDECKDF"'
      C300=C300(1:105)//',"HSOHLOF","KS","B_WSP","FROUDE","IT","TIME"'
      C300=C300(1:149)//',"X","Y"'
      WRITE(47,'(A)') C300(1:157)

C
      WRITE(20,'(I10)') NX
      DO 215 I=1,NX
      QOSUM(I)=QOSUM(I)*DT/1000.
      WRITE(N6,321) IAB(I),HMAX(I),QMAX(I),VMAX(I),HGWMAX(I),XHGW(I),
     1              QOSUM(I)

      LIRE(I)=0
      IF(ITYPE(I).GT.3) THEN
        HRANDL=ENGSI(ZL(I),1)
        HRANDR=ENGSI(ZR(I),1)
        IF (ZL(I).LT.ZR(I)) LIRE(I)=1
        IF (ZR(I).LT.ZL(I)) LIRE(I)=2
      ELSE
        HRANDL=ZO(I)+ZS(I)
        HRANDL=ENGSI(HRANDL,1)
        HRANDR=HRANDL
      ENDIF

      HSOHL=ENGSI(ZO(I),1)
      TIEFE=ENGSI(D(I),1)
      HSOHLOF=ENGSI(ZOF(I),1)
      HDECKDF=ENGSI(ZDF(I),1)
       
      STATN=ENGSI(XL(I),1)
      IF (I.GT.1.AND.I.LT.NX) THEN
         IF (ITYPE(I).GT.6.OR.ITYPE(I+1).GT.6) THEN
            BETAS=0.0
         ELSE
            DSUW=ZO(I)-ZO(I+1)
            DXUW=XL(I)-XL(I+1)
            BETAS=ATAN(DSUW/DXUW)
         ENDIF
      ELSE
         BETAS=0.0
      ENDIF

      IF (LIST.EQ. NT ) THEN
         QOHALT=QOFLOW(I)*DX(I)
         QOHALT=ENGSI(QOHALT,3)
         TITIME=ITIME*DT/3600.

         IF (I.GT.1) THEN
            V2=0.5*(ENGSI(U(I-1),1)+ENGSI(U(I),1))
            V2=V2**2.
         ELSE IF (I.EQ.1) THEN
            V2=ENGSI(U(I),1)**2.
         END IF
         HEL=V2/(2.*9.81)
         IF (I.GT.1.AND.I.LT.NX) THEN
            DHUW=H(I)-H(I+1)
            BETAH=ATAN(DHUW/DXUW)
         ELSE
            BETAH=0.0
         ENDIF
         IF (BETAS.GT.0.0.AND.BETAH.GT.0.0) THEN
            BETASH=0.5*(BETAS+BETAH)
         ELSE IF (BETAS.LE.0.0.AND.BETAH.GT.0.0) THEN
            BETASH=BETAH*0.5
         ELSE IF (BETAS.LE.0.0.AND.BETAH.LE.0.0) THEN
            BETASH=0.0
         END IF
         COSBETA=COS(BETASH)

         HCOSBETA=ENGSI((H(I)-ZO(I)),1)*COSBETA
         WRITE(25,'(T2,I5,A,F10.3)') IAB(I),'  HCOSBETA = ',HCOSBETA
         HEL=HEL+HSOHL+HCOSBETA

         QQ=Q(I)
         CALL YGRENZ4(I,QQ,DQGR,HMIN)
         HGR(I)=HSOHL+ENGSI(DQGR,1)
         IF (ITYPE(I).LT.4) HGR(I)=0

         HTEST=-1.0
         DO NW=1,NWEIRS
            IF (I.EQ.IWEIR(NW)) HTEST=ENGSI(DQGR,1)
         END DO
          
         IF (HTEST.GT.0.0.AND.HTEST.GT.HCOSBETA) THEN
            HEL=HEL-HCOSBETA+HTEST
         END IF
          
         IF (HEL.GT.HELVOR.AND.HTEST.GT.0.0.AND.HTEST.LT.HCOSBETA) THEN
            WRITE(25,'(T2,A,I5)')    '     IAB = ',IAB(I)
            WRITE(25,'(T2,A,F10.3)') '  HELVOR = ',HELVOR
            WRITE(25,'(T2,A,F10.3)') '     HEL = ',HEL
            WRITE(25,'(T2,A,F10.3)') 'HCOSBETA = ',HCOSBETA
            WRITE(25,'(T2,A,F10.3)') '   HTEST = ',HTEST
            HEL=HEL-HCOSBETA+HTEST
            WRITE(25,'(T2,A,F10.3)') '  HELNEU = ',HEL
         END IF

         IF (I.GT.1.AND.HEL.GT.HELVOR) HEL=HELVOR
         IF (I.LT.NX) THEN
            V2=0.5*(ENGSI(U(I),1)+ENGSI(U(I+1),1))
            V2=V2**2.
            HELUW=V2/(2.*9.81)+ENGSI(ZO(I+1),1)
            HCOSBETA=ENGSI((H(I+1)-ZO(I+1)),1)*COSBETA
            HELUW=HELUW+HCOSBETA
            IF (HEL.LT.HELUW) HEL=HELUW
         END IF

         HWSP=ENGSI(H(I),1)
         HWSPORI=HWSP

         IF (ITYPE(I).GT.3) THEN
            IF (HSOHLOF.GT.HDECKDF) THEN
C              Überströmung über HDECKDF
               IF ((HWSP-HSOHLOF).LT.0.10) HWSP=HSOHLOF+0.10
            ELSE
               IF (HSOHLOF.GT.HSOHL) THEN
C                 Überströmung über HSOHLOF
                  IF ((HWSP-HSOHLOF).LT.0.10) HWSP=HSOHLOF+0.10
               ELSE
C                 Druckabfluss
                  IF (HWSP.GT.HDECKDF) HWSP=HDECKDF
               END IF
            END IF
         END IF

         IF (ITYPE(I).EQ.1) THEN
            RKSI=RNI(I)
         ELSE
            RKSI=1/RNV7(I)
         END IF

         FROUDE = U(I)/SQRT(GRAV * (Q(I)/U(I))/T(I))
         BREITE = ENGSI(T(I),1)

         WRITE(20,327)IAB(I),STATN,HSOHL,HRANDL,HRANDR,HWSP,
     &                ENGSI(Q(I),3),ENGSI(U(I),1),QOHALT,(SF(I)*1000.),
     &                ITIME,TITIME,HEL,HGR(I),HWSPORI,HDECKDF,HSOHLOF,
     &                RKSI,ITYPE(I),BREITE,FROUDE


         CALL CLEARCSV (CL,4096)
         WRITE (CL,471) ID(I),IAB(I),ITYPE(I),STATN,HSOHL,HRANDL,HRANDR,
     &         HWSP,ENGSI(Q(I),3),ENGSI(U(I),1),QOHALT,
     &         (SF(I)*1000.),HEL,HGR(I),HWSPORI,HDECKDF,HSOHLOF,
     &         RKSI,BREITE,FROUDE,ITIME,TITIME,XK(I),YK(I)
  471    FORMAT (2I6,I3,F9.1,5F9.2,F6.2,F9.1,F9.3,8F9.2,I5,F9.3,2F11.2)
         LCL=LEN_TRIM(CL)+1
         CALL WRITECSV (47,CL(1:LCL),LCL,',')


         HELVOR=HEL
         
         IF (I.EQ.NX) THEN
            IF (HGR(I).LT.ENGSI(H(I),1)) HGR(I)=HGR(I)+1.0
         END IF
         
      ELSE

         IF (I.GT.1) THEN
            V2=0.5*(VMAX(I-1)+VMAX(I))
            V2=V2**2.
         ELSE IF (I.EQ.1) THEN
            V2=VMAX(I)**2.
         END IF
         HEL=V2/(2.*9.81)
         IF (I.GT.1.AND.I.LT.NX) THEN
            DHUW=HMAX(I)-HMAX(I+1)
            BETAH=ATAN(DHUW/DXUW)
         ELSE
            BETAH=0.0
         ENDIF
         IF (BETAS.GT.0.0.AND.BETAH.GT.0.0) THEN
            BETASH=0.5*(BETAS+BETAH)
         ELSE IF (BETAS.LE.0.0.AND.BETAH.GT.0.0) THEN
            BETASH=BETAH*0.5
         ELSE IF (BETAS.LE.0.0.AND.BETAH.LE.0.0) THEN
            BETASH=0.0
         END IF
         COSBETA=COS(BETASH)

         HCOSBETA=(HMAX(I)-HSOHL)*COSBETA
         WRITE(25,'(T2,I5,A,F10.3)') IAB(I),'  HCOSBETA = ',HCOSBETA
         HEL=HEL+HSOHL+HCOSBETA

         QQ=SIENG(QMAX(I),3)
         CALL YGRENZ4(I,QQ,DQGR,HMIN)
         HGR(I)=HSOHL+ENGSI(DQGR,1)
         IF (ITYPE(I).LT.4) HGR(I)=0

         HTEST=-1.0
         DO NW=1,NWEIRS
            IF (I.EQ.IWEIR(NW)) HTEST=ENGSI(DQGR,1)
         END DO

         IF (HTEST.GT.0.0.AND.HTEST.GT.HCOSBETA) THEN
            HEL=HEL-HCOSBETA+HTEST
         END IF

         IF (HEL.GT.HELVOR.AND.HTEST.GT.0.0.AND.HTEST.LT.HCOSBETA) THEN
            WRITE(25,'(T2,A,I5)')    '     IAB = ',IAB(I)
            WRITE(25,'(T2,A,F10.3)') '  HELVOR = ',HELVOR
            WRITE(25,'(T2,A,F10.3)') '     HEL = ',HEL
            WRITE(25,'(T2,A,F10.3)') 'HCOSBETA = ',HCOSBETA
            WRITE(25,'(T2,A,F10.3)') '   HTEST = ',HTEST
            HEL=HEL-HCOSBETA+HTEST
            WRITE(25,'(T2,A,F10.3)') '  HELNEU = ',HEL
         END IF
 
         IF (I.GT.1.AND.HEL.GT.HELVOR) HEL=HELVOR
         IF (I.LT.NX) THEN
            V2=0.5*(VMAX(I)+VMAX(I+1))
            V2=V2**2.
            HELUW=V2/(2.*9.81)+ENGSI(ZO(I+1),1)
            HCOSBETA=ENGSI((HMAX(I+1)-ZO(I+1)),1)*COSBETA
            HELUW=HELUW+HCOSBETA
            IF (HEL.LT.HELUW) HEL=HELUW
         END IF

         HWSP=HMAX(I)
         HWSPORI=HWSP

         IF (ITYPE(I).GT.3) THEN
            IF (HSOHLOF.GT.HDECKDF) THEN
C              Überströmung
               IF ((HWSP-HSOHLOF).LT.0.10) HWSP=HSOHLOF+0.10
            ELSE
               IF (HSOHLOF.GT.HSOHL) THEN
C                 Überströmung über HSOHLOF
                  IF ((HWSP-HSOHLOF).LT.0.10) HWSP=HSOHLOF+0.10
               ELSE
C                 Druckabfluss
                  IF (HWSP.GT.HDECKDF) HWSP=HDECKDF
               END IF
C              Druckabfluss
               IF (HWSP.GT.HDECKDF) HWSP=HDECKDF
            END IF
         END IF

         IF (ITYPE(I).EQ.1) THEN 
            RKSI=RNI(I)
         ELSE 
            RKSI=1/RNV7(I)
         END IF

C???     FROUDE = U(I)/SQRT(GRAV * AREA(I)/T(I))
         FROUDE = 0.0
         BREITE = 0.0

         WRITE(20,327)IAB(I),STATN,HSOHL,HRANDL,HRANDR,HWSP,QMAX(I),
     &                VMAX(I),QOSUM(I),SFMAX(I),ITQMAX(I),THQMAX(I),HEL,
     &                HGR(I),HWSPORI,HDECKDF,HSOHLOF,RKSI,ITYPE(I),
     &                BREITE,FROUDE
         

         CALL CLEARCSV (CL,4096)
         WRITE (CL,*) ID(I),IAB(I),ITYPE(I),STATN,HSOHL,HRANDL,HRANDR,
     &                HWSP,QMAX(I),VMAX(I),QOSUM(I),SFMAX(I),
     &                HEL,HGR(I),HWSPORI,HDECKDF,HSOHLOF,
     &                RKSI,BREITE,FROUDE,ITQMAX(I),THQMAX(I)
         LCL=LEN_TRIM(CL)+1
         CALL WRITECSV (47,CL(1:LCL),LCL,',')


         HELVOR=HEL
         
      ENDIF
  215 CONTINUE
C
      CLOSE (N20)
      CLOSE (47)
C
      IF (LIST.EQ.NT) THEN
         DO I=NX,2,-1
            IF (HGR(I-1).LT.HGR(I)) HGR(I-1)=HGR(I)
         END DO
      END IF
C
      OPEN (N21,FILE='QPARTS.PLT')
C
      WRITE(21,'(A)')'"KNOT"  "STAT[KM]"  "QGES"    "QHR0"    "QHR1"
     &"QHR2"    "QHR3"    "QHR4"    "QHR5"    "QHR6"    "QHR7"'
C
      DO 216 I=1,NX
      STATN=ENGSI(XL(I),1)/1000.
      IF ( LIST .EQ. NT ) THEN
         WRITE(21,326) IAB(I),STATN,ENGSI(Q(I),3),ENGSI(QHR0(I),3),
     &            ENGSI(QHR(I,1),3),ENGSI(QHR(I,2),3),ENGSI(QHR(I,3),3),
     &            ENGSI(QHR(I,4),3),ENGSI(QHR(I,5),3),ENGSI(QHR(I,6),3),
     &            ENGSI(QHR(I,7),3)
      ELSE
         WRITE(21,326) IAB(I),STATN,QMAX(I),ENGSI(QMXHR0(I),3),
     &      ENGSI(QMXHR(I,1),3),ENGSI(QMXHR(I,2),3),ENGSI(QMXHR(I,3),3),
     &      ENGSI(QMXHR(I,4),3),ENGSI(QMXHR(I,5),3),ENGSI(QMXHR(I,6),3),
     &      ENGSI(QMXHR(I,7),3)
      ENDIF
  216 CONTINUE
C
      CLOSE (N21)
C
      WRITE(N6,341)
      WRITE(N6,330)
C
C     AUSDRUCK DER AUSGEBORDETEN MENGEN FUER JEDEN ZEITSCHRITT
C      (AUF UNIT = 12)
C
        TIMO=0
        NTT=-NT
        WRITE(12,323) NTT,DT/3600.
        DO 350 I=1,NT
          TIMO=TIMO+DT/3600.
          WRITE(12,'(2F12.5)') TIMO,QOGES(I)
  350   CONTINUE
C       WRITE(12,324) (-QOGES(I),I=1,NT)
C
C
C     AUSDRUCK DER BERECHNETEN ZUFLUSSGANGLINIE IN JEDEN SPEICHER
C
      DO 178 J=1,NSTORE
       IOT=30+J
       OPEN(IOT,FILE=RETOUT(J),STATUS='UNKNOWN')
       PRINT *
       WRITE(*,'(T2,A,A)')'SPEICHERZUFLUSS UND ÜBERLAUF AUF ',RETOUT(J)
       PRINT *
       WRITE(IOT,*) 'ZUFLUSS UND ÜBERLAUF SPEICHER ',J
       NT1=INT(NT/(TINC/(DT/60.)))+1
       IF (NT1.GT.NT) NT1=NT
C      WRITE(IOT,325) NT1,TINC/60.
       WRITE(IOT,325) -NT1,TINC/60.
       TIMIN=-(DT/60.)
       SUMQOX=0.0
       SUMQUE=0.0
       ISUM=0
       DO 1781 I=1,NT
         TIMIN=TIMIN+(DT/60.)
         TIMH=TIMIN/60.
         TIMTES=TIMIN/TINC
         TPR=ANINT(TIMTES)
         IF (ABS(TIMTES-TPR).LT.0.001) THEN
            ISUM=ISUM+1
            SUMQOX=SUMQOX+ENGSI(QOX(J,I),3)*(-1.)
            SUMQUE=SUMQUE+ENGSI(QUESP(J,I),3)
            RISUM=REAL(ISUM)
            WRITE(IOT,'(3F12.3)') TIMH,SUMQOX/RISUM,SUMQUE/RISUM
            SUMQOX=0.0
            SUMQUE=0.0
            ISUM=0
         ELSE
            ISUM=ISUM+1
            SUMQOX=SUMQOX+ENGSI(QOX(J,I),3)*(-1.)
            SUMQUE=SUMQUE+ENGSI(QUESP(J,I),3)
         END IF
 1781  CONTINUE
       CLOSE(IOT)
  178 CONTINUE
C
C     AUSDRUCK DER GANGLINIEN VON ABFLUSS Q UND WASSERSTAND H FUER
C      AUSGEWAEHLTE QUERSCHNITTE
C
      CH1 = 'Q'
      CH2 = '-'
      NA = 1
      NE = 6
  180 IF(NE.GT.NWEL) NE = NWEL
      IF (NA.NE.1) WRITE (N6,330)
      WRITE (N6,290)
      WRITE(N6,340) (CH2,I=NA,NE)
      WRITE(N6,300) (KWELE(I),I=NA,NE)
      WRITE(N6,340) (CH2,I=NA,NE)
      WRITE(N6,310) (CH1,I=NA,NE)
      WRITE(N6,340) (CH2,I=NA,NE)
      DO 200 J = 1,IPWEL
      TWEL = DTWEL * REAL(J) - DTWEL
      RTIME = STIME + TWEL
      CALL ZEIT (RTIME,ITH,ITM,ITS)
      IF (ITUN.GT.0) GO TO 190
      IF (ITS.GT.30) ITM = ITM + 1
      ITS = 0
  190 WRITE (N6,320) ITH,ITM,ITS,(HWELLE(I,J),QWELLE(I,J),I=NA,NE)
      IF (TWEL.GT.(TOT-DTWEL)) GO TO 210
  200 CONTINUE
  210 WRITE(N6,340) (CH2,I=NA,NE)
      IF (NE.EQ.NWEL) GO TO 220
      NA = NA + 6
      NE = NE + 6
      GO TO 180
  220 CONTINUE

CANF  IF(LIST.EQ.NT) THEN

      OPEN (26,FILE='ANFANG')

      OPEN (46,FILE='Start.dat',RECL=4096)
      IF (OVFMOD.EQ.'OVFBIL') THEN
        IF (KSTMOD.EQ.'KSTIME') THEN
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"KRAUT",'
          C300=C300(1:65)//'"DZERO",'
          C300=C300(1:73)//'"HZERO",'
          C300=C300(1:81)//'"QZERO",'
          C300=C300(1:89)//'"ZS",'
          C300=C300(1:94)//'"CKS",'
          C300=C300(1:100)//'"LIRE",'
          C300=C300(1:107)//'"NOVF",'
          C300=C300(1:114)//'"IABOVF",'
          C300=C300(1:123)//'"HR0",'
          C300=C300(1:129)//'"HR1",'
          C300=C300(1:135)//'"RNV1",'
          C300=C300(1:142)//'"HR2",'
          C300=C300(1:148)//'"RNV2",'
          C300=C300(1:155)//'"HR3",'
          C300=C300(1:161)//'"RNV3",'
          C300=C300(1:168)//'"HR4",'
          C300=C300(1:174)//'"RNV4",'
          C300=C300(1:181)//'"HR5",'
          C300=C300(1:187)//'"RNV5",'
          C300=C300(1:194)//'"HR6",'
          C300=C300(1:200)//'"RNV6",'
          C300=C300(1:207)//'"HR7",'
          C300=C300(1:213)//'"RNV7",'
          C300=C300(1:220)//'"SF",'
          C300=C300(1:225)//'"X",'
          C300=C300(1:229)//'"Y",'
          C300=C300(1:233)//'"ID",'
          C300=C300(1:238)//'"ZSHIFT"'
        ELSE
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"DZERO",'
          C300=C300(1:65)//'"HZERO",'
          C300=C300(1:73)//'"QZERO",'
          C300=C300(1:81)//'"ZS",'
          C300=C300(1:86)//'"CKS",'
          C300=C300(1:92)//'"LIRE",'
          C300=C300(1:99)//'"NOVF",'
          C300=C300(1:106)//'"IABOVF",'
          C300=C300(1:115)//'"HR0",'
          C300=C300(1:121)//'"HR1",'
          C300=C300(1:127)//'"RNV1",'
          C300=C300(1:134)//'"HR2",'
          C300=C300(1:140)//'"RNV2",'
          C300=C300(1:147)//'"HR3",'
          C300=C300(1:153)//'"RNV3",'
          C300=C300(1:160)//'"HR4",'
          C300=C300(1:166)//'"RNV4",'
          C300=C300(1:173)//'"HR5",'
          C300=C300(1:179)//'"RNV5",'
          C300=C300(1:186)//'"HR6",'
          C300=C300(1:192)//'"RNV6",'
          C300=C300(1:199)//'"HR7",'
          C300=C300(1:205)//'"RNV7",'
          C300=C300(1:212)//'"SF",'
          C300=C300(1:217)//'"X",'
          C300=C300(1:221)//'"Y",'
          C300=C300(1:225)//'"ID",'
          C300=C300(1:230)//'"ZSHIFT"'
        END IF
      ELSE IF (OVFMOD.EQ.'OVFREG') THEN
        IF (KSTMOD.EQ.'KSTIME') THEN
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"KRAUT",'
          C300=C300(1:65)//'"DZERO",'
          C300=C300(1:73)//'"HZERO",'
          C300=C300(1:81)//'"QZERO",'
          C300=C300(1:89)//'"ZS",'
          C300=C300(1:94)//'"CKS",'
          C300=C300(1:100)//'"LIRE",'
          C300=C300(1:107)//'"NOVF",'
          C300=C300(1:114)//'"IABOVF",'
          C300=C300(1:123)//'"OVFAN",'
          C300=C300(1:131)//'"OVFAUS",'
          C300=C300(1:140)//'"QOMAX",'
          C300=C300(1:148)//'"QREGEL",'
          C300=C300(1:157)//'"TOVFAN",'
          C300=C300(1:166)//'"TOVFAUS",'
          C300=C300(1:174)//'"HR0",'
          C300=C300(1:182)//'"HR1",'
          C300=C300(1:188)//'"RNV1",'
          C300=C300(1:195)//'"HR2",'
          C300=C300(1:201)//'"RNV2",'
          C300=C300(1:208)//'"HR3",'
          C300=C300(1:214)//'"RNV3",'
          C300=C300(1:221)//'"HR4",'
          C300=C300(1:227)//'"RNV4",'
          C300=C300(1:234)//'"HR5",'
          C300=C300(1:240)//'"RNV5",'
          C300=C300(1:248)//'"HR6",'
          C300=C300(1:253)//'"RNV6",'
          C300=C300(1:260)//'"HR7",'
          C300=C300(1:266)//'"RNV7",'
          C300=C300(1:273)//'"SF",'
          C300=C300(1:278)//'"X",'
          C300=C300(1:282)//'"Y",'
          C300=C300(1:286)//'"ID",'
          C300=C300(1:291)//'"ZSHIFT"'
        ELSE 
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"DZERO",'
          C300=C300(1:65)//'"HZERO",'
          C300=C300(1:73)//'"QZERO",'
          C300=C300(1:81)//'"ZS",'
          C300=C300(1:86)//'"CKS",'
          C300=C300(1:92)//'"LIRE",'
          C300=C300(1:99)//'"NOVF",'
          C300=C300(1:106)//'"IABOVF",'
          C300=C300(1:115)//'"OVFAN",'
          C300=C300(1:123)//'"OVFAUS",'
          C300=C300(1:132)//'"QOMAX",'
          C300=C300(1:140)//'"QREGEL",'
          C300=C300(1:149)//'"TOVFAN",'
          C300=C300(1:158)//'"TOVFAUS",'
          C300=C300(1:168)//'"HR0",'
          C300=C300(1:174)//'"HR1",'
          C300=C300(1:180)//'"RNV1",'
          C300=C300(1:187)//'"HR2",'
          C300=C300(1:193)//'"RNV2",'
          C300=C300(1:200)//'"HR3",'
          C300=C300(1:206)//'"RNV3",'
          C300=C300(1:213)//'"HR4",'
          C300=C300(1:219)//'"RNV4",'
          C300=C300(1:226)//'"HR5",'
          C300=C300(1:232)//'"RNV5",'
          C300=C300(1:239)//'"HR6",'
          C300=C300(1:245)//'"RNV6",'
          C300=C300(1:252)//'"HR7",'
          C300=C300(1:258)//'"RNV7",'
          C300=C300(1:265)//'"SF",'
          C300=C300(1:270)//'"X",'
          C300=C300(1:274)//'"Y",'
          C300=C300(1:278)//'"ID",'
          C300=C300(1:283)//'"ZSHIFT"'
        END IF
      ELSE
        IF (KSTMOD.EQ.'KSTIME') THEN
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"KRAUT",'
          C300=C300(1:65)//'"DZERO",'
          C300=C300(1:73)//'"HZERO",'
          C300=C300(1:81)//'"QZERO",'
          C300=C300(1:89)//'"ZS",'
          C300=C300(1:94)//'"CKS",'
          C300=C300(1:100)//'"HR0",'
          C300=C300(1:106)//'"HR1",'
          C300=C300(1:112)//'"RNV1",'
          C300=C300(1:119)//'"HR2",'
          C300=C300(1:125)//'"RNV2",'
          C300=C300(1:132)//'"HR3",'
          C300=C300(1:138)//'"RNV3",'
          C300=C300(1:145)//'"HR4",'
          C300=C300(1:151)//'"RNV4",'
          C300=C300(1:158)//'"HR5",'
          C300=C300(1:164)//'"RNV5",'
          C300=C300(1:171)//'"HR6",'
          C300=C300(1:177)//'"RNV6",'
          C300=C300(1:184)//'"HR7",'
          C300=C300(1:190)//'"RNV7",'
          C300=C300(1:197)//'"SF",'
          C300=C300(1:202)//'"X",'
          C300=C300(1:206)//'"Y",'
          C300=C300(1:210)//'"ID",'
          C300=C300(1:215)//'"ZSHIFT"'
        ELSE
          C300='"IAB",'//'"ITYPE",'
          C300=C300(1:14)//'"WIDTH",'
          C300=C300(1:22)//'"HEIT",'
          C300=C300(1:29)//'"ZTR",'
          C300=C300(1:35)//'"ZTL",'
          C300=C300(1:41)//'"XL",'
          C300=C300(1:46)//'"ZO",'
          C300=C300(1:51)//'"RNI",'
          C300=C300(1:57)//'"DZERO",'
          C300=C300(1:65)//'"HZERO",'
          C300=C300(1:73)//'"QZERO",'
          C300=C300(1:81)//'"ZS",'
          C300=C300(1:86)//'"CKS",'
          C300=C300(1:92)//'"HR0",'
          C300=C300(1:98)//'"HR1",'
          C300=C300(1:104)//'"RNV1",'
          C300=C300(1:111)//'"HR2",'
          C300=C300(1:117)//'"RNV2",'
          C300=C300(1:124)//'"HR3",'
          C300=C300(1:130)//'"RNV3",'
          C300=C300(1:137)//'"HR4",'
          C300=C300(1:143)//'"RNV4",'
          C300=C300(1:150)//'"HR5",'
          C300=C300(1:156)//'"RNV5",'
          C300=C300(1:163)//'"HR6",'
          C300=C300(1:169)//'"RNV6",'
          C300=C300(1:176)//'"HR7",'
          C300=C300(1:182)//'"RNV7",'
          C300=C300(1:189)//'"SF",'
          C300=C300(1:194)//'"X",'
          C300=C300(1:198)//'"Y",'
          C300=C300(1:202)//'"ID",'
          C300=C300(1:207)//'"ZSHIFT"'
        END IF
      END IF

      WRITE(7,'(A)') 'Ausgabe von C300'
      WRITE(46,'(A)') C300
       
      IF (LIST.EQ.NT) THEN
         OPEN (36,FILE='ANFANG.HGR')
      END IF

      DO 250 I=1,NX
         CKS(I)   = 3./2. * CKS(I) / SQRT(2.*GRAV)
         WIDTH(I) = ENGSI (WIDTH(I),1)
         HEIT (I) = ENGSI (HEIT (I),1)
         XL   (I) = ENGSI (XL   (I),1)
         ZS   (I) = ENGSI (ZS   (I),1)
         ZO   (I) = ENGSI (ZO   (I),1)
         DZERO(I) = ENGSI (D    (I),1)
         QZERO(I) = ENGSI (Q    (I),3)
         OVFAN(I) = ENGSI (OVFAN(I),1)
         OVFAUS(I)= ENGSI (OVFAUS(I),1)
         QOMAX(I) = ENGSI (QOMAX(I),3)
         QREGEL(I)= ENGSI (QREGEL(I),3) 
         HR0  (I) = ENGSI (HR0  (I),1)
         HR(I,1)  = ENGSI (HR(I,1),1)
         HR(I,2)  = ENGSI (HR(I,2),1)
         HR(I,3)  = ENGSI (HR(I,3),1)
         HR(I,4)  = ENGSI (HR(I,4),1)
         HR(I,5)  = ENGSI (HR(I,5),1)
         HR(I,6)  = ENGSI (HR(I,6),1)
         HR(I,7)  = ENGSI (HR(I,7),1)
         ZSHIFT(I)= ENGSI (ZSHIFT(I),1)
         HZERO=ZO(I)+DZERO(I)

         WRITE (26,685) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1               XL(I),ZO(I),RNIBAK(I),DZERO(I),QZERO(I),ZS(I),
     2               CKS(I),HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)

         CALL CLEARCSV (CL,4096)

         IF (OVFMOD.EQ.'OVFBIL') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,693'
           WRITE(CL,693) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),KRAUT(I),DZERO(I),HZERO,
     2              QZERO(I),ZS(I),CKS(I),LIRE(I),NOVF(I),IABOVF(I),
     3              HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I),XK(I),YK(I),
     4              ID(I),ZSHIFT(I)
          ELSE
           WRITE(CL,691) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2              CKS(I),LIRE(I),NOVF(I),IABOVF(I),HR0(I),
     3              (HR(I,N),RNV(I,N),N=1,7),SF(I),
     4              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (46,CL(1:LCL),LCL,',')
         ELSE IF (OVFMOD.EQ.'OVFREG') THEN
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,694'
           WRITE(CL,694) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),KRAUT(I),DZERO(I),HZERO,
     2              QZERO(I),ZS(I),CKS(I),LIRE(I),NOVF(I),IABOVF(I),
     3              OVFAN(I),OVFAUS(I),QOMAX(I),QREGEL(I),TOVFAN(I),
     4              TOVFAUS(I),HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I),
     5              XK(I),YK(I),ID(I),ZSHIFT(I)
          ELSE
           WRITE(CL,692) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2              CKS(I),LIRE(I),NOVF(I),IABOVF(I),OVFAN(I),OVFAUS(I),
     3              QOMAX(I),QREGEL(I),TOVFAN(I),TOVFAUS(I),
     4              HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I),
     5              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (46,CL(1:LCL),LCL,',')
         ELSE
          IF (KSTMOD.EQ.'KSTIME') THEN
           WRITE(7,'(A)') 'Ausgabe von CL,689'
           WRITE(CL,689) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),KRAUT(I),DZERO(I),HZERO,
     2              QZERO(I),ZS(I),CKS(I),HR0(I),
     3              (HR(I,N),RNV(I,N),N=1,7),SF(I),XK(I),YK(I),ID(I),
     4              ZSHIFT(I)
          ELSE
           WRITE(CL,690) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1              XL(I),ZO(I),RNIBAK(I),DZERO(I),HZERO,QZERO(I),ZS(I),
     2              CKS(I),HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I),
     3              XK(I),YK(I),ID(I),ZSHIFT(I)
          END IF
          LCL=LEN_TRIM(CL)+1
          CALL WRITECSV (46,CL(1:LCL),LCL,',')
         END IF

         IF (LIST.EQ.NT) THEN
           DHGR=HGR(I)-ZO(I)
           WRITE(36,685) IAB(I),ITYPE(I),WIDTH(I),HEIT(I),ZTR(I),ZTL(I),
     1                   XL(I),ZO(I),RNIBAK(I),DHGR,QZERO(I),ZS(I),
     2                   CKS(I),HR0(I),(HR(I,N),RNV(I,N),N=1,7),SF(I)
         END IF
      
  685 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.4,F7.3,F7.2,F5.2,F5.3,
     &        F8.3,7(F8.3,F8.4),F12.8)
      
  689 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  690 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  691 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  693 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  692 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,6F8.2,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  694 FORMAT (I4,I3,4F7.2,F10.1,F7.2,F7.4,I5,F8.3,2F9.3,F6.2,F6.3,
     &        I5,I5,I5,6F8.2,F8.3,7(F8.3,F8.4),F12.8,2F12.2,I6,F7.2)
      
  250 CONTINUE
      CLOSE (26)
      CLOSE (46)
      IF (LIST.EQ.NT) THEN
         CLOSE (36)
      END IF
CANF  ENDIF

      IF (PLOT.EQ.'J') THEN
         WRITE (*,*) ' FORM VOR CLOSE = ',FORM
         IF (FORM.EQ.'T') THEN
            NX0=0
            WRITE(55) NX0
            WRITE(N77) NX0
            WRITE(N88) NX0
            WRITE(N99) NX0
            WRITE(N10) NX0
         ENDIF
         CLOSE (55)
         CLOSE (N77)
         CLOSE (N88)
         CLOSE (N99)
         CLOSE (N10)
      ENDIF
      CLOSE (N11)
      CLOSE (N12)
      IF (FORM.EQ.'T') WRITE(13) NX0
      CLOSE (13)
      CLOSE (N25)
      CLOSE (30)
Calt  CLOSE (43)
C
      RETURN
C
  290 FORMAT (1X,'   WASSERSTANDS- UND ABFLUSSGANGLINIEN FUER AUSGEWAEHL
     1TE QUERSCHNITTE'/4X,66('-')//)
  291 FORMAT (1X,'   MAXIMALE WASSERSTAENDE UND ABFLUESSE WAEHREND DER S
     1IMULATION     '/4X,60('-')//)
  292 FORMAT(5X,A//)
  300 FORMAT (1X,'³',4X,'KNOTEN    ³',6(I10,7X,'³',:))
  301 FORMAT (1X,'³',4X,'KNOTEN    ³     HMAX     ³     QMAX     ³
     1V       ³    HGWMAX ==>  ENTFERNUNG   ³ S-UEBERLAUF  ³',/,
     2        1X,'³',4X,'          ³              ³              ³
     3        ³               VOM VORFLUTER ³              ³')
  302 FORMAT (1X,'³',4X,'          ³    (m+NN)    ³  (cbm/sec)   ³   (m/
     1sec)    ³    (m+NN)          (m)      ³  (1000 cbm)  ³')
  310 FORMAT (1X,'³',5X,'ZEIT',5X,'³',6(4X,'h',8X,A1,3X,'³',:))
  320 FORMAT (1X,'³',3X,I2,':',I2,':',I2,1X,6(2X,'³',F7.2,F8.2),'  ³')
  321 FORMAT (1X,'³',5X,I4,5X,5('³',3X,F7.2,4X),'³',3X,F10.2,' ³')
  322 FORMAT (1X,I5,F10.1,3F8.2,F8.3,2F8.2,F12.2,1X,F8.3,1X,I5)
C 327 FORMAT (I6,F10.1,3F8.2,F8.3,2F8.2,F12.2,1X,F8.3,1X,I5,7F10.4,I3)
  327 FORMAT (I6,F10.1,3F8.2,F8.3,2F8.2,F12.2,1X,F8.3,1X,I5,7F10.4,I3,
     &        F8.2,F8.2)
  323 FORMAT (1X,'AUSBORDUNGSWELLE',/,I5,F16.12)
  324 FORMAT (6F12.2)
  325 FORMAT (I5,F8.2)
  326 FORMAT (1X,I5,10F10.4)
  330 FORMAT (1H1/)
  340 FORMAT (1X,'³',15('-'),6(17('-'),A1,:))
  341 FORMAT (1X,7('³--------------'),'³')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WEIR (N,ITIME,M)

      INCLUDE 'DIMENS.INC'

      CHARACTER*30 WHQDAT

      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
     1        COM(IPSED,4), COZ(IPSED)  , CON(3)
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , ITE1        ,
     2        ITE2
      COMMON /AB/           IAB(IPSEC)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /WEHR/  NWEIRS,IWEIR(IPWEI),HW(IPWEI),BW(IPWEI),WCO(IPWEI),
     1               WNEI(IPWEI),WNEIL(IPWEI),WNEIR(IPWEI),IRAUS(IPWEI),
     2               V2CO(IPWEI),CCFAK(IPWEI),WHQDAT(IPWEI),
     3               WH(IPWEI,IPSTP),WQ(IPWEI,IPSTP)
C
C     SUBR WEIR CHECKS AND CORRECTS WEIR ERRORS GREATER THAN 1%
C
      NTWO = N * 2
      IANSI = N + 1
      IIANS = IANSI * 2
      D = XV(NTWO)
      Q = XV(NTWO - 1)
      N1 = (N - 1) * 2
      WRITE(N6,1)
      WRITE(N6,*) ITIME,N1,D,Q
    1 FORMAT(1X,'ITIME-N1-D-Q')
      IF (ITIME.LE.1) GO TO 110
      H2 = XV(IIANS) + ZO(IANSI)
      H1 = D + ZO(N)
      IF (WCO(M).GT.0.0) THEN
         HKR = HW(M) + ZO(N)
      ELSE
         HKR = ZO(N)
      END IF
      HUE = H1 - HKR
      HU  = H2 - HKR
      WRITE(N6,*) HKR,HUE,HU,HW(M)
C
C     BERECHNUNG NACH SCHROEDER TECHN. HYDRAULIK S.39
      IF (HUE.GE.HU) QUO=HU/HUE
      IF (HUE.LT.HU) QUO=HUE/HU
      XC=0.7
      IF (XC.GT.QUO) XC=QUO
      IF (XC.LT.0.4) XC=0.4
      IF (QUO.LT.0.0) QUO=0.0
      IF (QUO.GT.1.0) QUO=1.0
      IF (QUO.GE.XC) THEN
         ZC=(2.-3.*XC+QUO)/(3.*(1.-XC))
         IF (ZC.GT.1.0) ZC=0.999
         CC=(3./2.)*SQRT(3.)*ZC*SQRT(1.-ZC)
      ELSE
         CC=1.0
      END IF
      IF (HUE.LT.HU) THEN CC=-1.*CC
      IF (CC.GE.0.0.AND.CC.LT.0.070) CC=0.070
      IF (CC.LT.0.0.AND.CC.GT.-0.070) CC=-0.070
      WRITE(25,'(T2,A,F10.4)') 'UP WEIR: RUECKSTAUFAKTOR CC = ',CC
C
C      COMPUTE FLOW WITH WEIR EQUATION:
C         Q = 2/3 * CC* MUE * SQRT(( 2 * GRAV * H )) * H * L
C
      IF (WCO(M).GT.0.0) THEN
         IF (WNEI(M) .EQ. 0.) THEN
            QCHECK = 2./3.*CC*WCO(M)*SQRT(2.*GRAV*ABS(HUE))*HUE*BW(M)
         ELSE
            A1 = 2./3.*CC*WCO(M)*SQRT(2.*GRAV)*BW(M)
            QCHECK = A1*(HUE**(3./2.))+A1*WNEI(M)/BW(M)*(HUE**(5./2.))
         ENDIF
      ELSE
C
         CALL SHAPE2 (N,H1,ADUM,WPDUM,TDUM,CH2,CHR,QGRENZ)
C
         QCHECK = QGRENZ
      END IF
C
   90 IF ((ABS(QCHECK-Q)).LE.(0.010 * ABS(Q))) GO TO 110
      WRITE (N6,120)
      IF (LIST.GE.1) GO TO 100
      Q = ENGSI(Q,3)
      QCHECK = ENGSI(QCHECK,3)
      WRITE (N6,130) IAB(N),Q,QCHECK
      Q = SIENG(Q,3)
      QCHECK = SIENG(QCHECK,3)
  100 CONTINUE
C
C      SET FLOWS TO COMPUTED DISCHARGE
C
      XV(NTWO - 1) = (QCHECK+Q+Q)/3.
      XV(IIANS - 1) = (QCHECK+Q+Q)/3.
C
C     THIS COMPLETES THE GATE CORRECTION SECTION OF THE PROGRAM.
C     CONTINUE THE DOUBLE SWEEP PROCESS OF SIMULTANEOUS EQUN SOLUTION.
C
  110 CONTINUE
      WRITE(N6,88)
C     WRITE(N6,*) IAB(N),N,M,HUE,VIL,ENGSI(QCHECK,3),ENGSI(Q,3)
C 88  FORMAT(1X,'IAB(N)-N-M-HUE-VIL-QCHECK-Q')
      WRITE(N6,*) IAB(N),N,M,HUE,ENGSI(QCHECK,3),ENGSI(Q,3)
  88  FORMAT(1X,'IAB(N)-N-M-HUE-QCHECK-Q')
      CON(1) = CO(N1,1)
      CON(2) = CO(N1,2)
      CON(3) = E(N1) - CO(N1,3) * XV(NTWO - 1) - CO(N1,4) * XV(NTWO)
      RETURN
C
  120 FORMAT (1X,'ABFLUSS AM WEHR KORRIGIERT')
  130 FORMAT (1X,'KNOTEN IAB=',I5,' Q=',G10.4,'  Q VERBESSERT=',G10.4)
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION PUMP(DK,I)
C
C      PUMP IS CALLED FOR SECTIONS WITH ZERO WEIR HEIGHT
C      DISHARGE LEAVING THE SYSTEM IS RETURNED.
C      DK IS THE DEPTH OF WATER

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /GATDIM/       GATEL(IPGAT)
      COMMON /AB/           IAB(IPSEC)
      DATA GMIN/0.05/,ANTICP/-1.00/
C     TO ANTICIPATE   THE PUMP CURVE, SET ANTICP TO 0.5 OR 1.0 FEET.
C
C --------------------------------------------------------------------
C   !!!!    DIESE FUNCTION IST VERMUTLICH FUER EINEN SPEZIFISCHEN
C           FALL ANGEWANDT WORDEN; WENN BENOETIGT, MUSS SIE VORHER
C           UEBERPRUEFT WERDEN.
C    ---->  Z.ZT. ERFOLGT EIN PROGRAMMABBRUCH BEIM AUFRUF VON "PUMP"
C
      WRITE (N6,*) 'PUMP-Knoten I = ',I,'  IAB(I) = ',IAB(I)
      WRITE (N6,99)
   99 FORMAT(////100('#')/5X,'PUMP WURDE AUFGERUFEN !  --> DATENFEHLER
     1BEI WEHRHOEHEN: ZS MUSS GROESSER 0 SEIN')
      ISTOP=1
      IF (ISTOP.EQ.1) STOP 99
C --------------------------------------------------------------------
      DS = DK + ANTICP
      QOVF = 0.0
      IF (DS.GE.0.0) QOVF = 10.829
      IF (DS.GE.1.500) QOVF = 30.94
      IF (GATEL(IPGAT).LE.GMIN) GO TO 100
      IF (DS.GE.5.57) QOVF = 55.692
      IF (DS.GE.6.91) QOVF = 116.025
      IF (DS.GE.8.16) QOVF = 232.05
C
C     CONVERSION . . . . 100MGD=154.7CFS.  QOVF NUMBERS HERE ARE IN CFS.
C
  100 CONTINUE
      PUMP =  - QOVF
      RETURN
      END

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      REAL FUNCTION  Y (ITIME,Q)
C
C      THIS SUBROUTINE IS USED TO INTERPOLATE BETWEEN GIVEN HYDROGRAPH
C      POINTS  -  ALSO USED FOR GATE TRAJECTORIES

      INCLUDE 'DIMENS.INC'

      REAL      Q(IPHYD)
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /STUFF/        NT          , NX          , LIST,LLIST  ,
     1        GRAV        , CKS(IPSEC)  , NL          , INCRE       ,
     2        IGATE2
      COMMON /TIB/      DT     , TIME , ITOTH, ITOTM,TOTH ,JTIME
      COMMON /AB/           IAB(IPSEC)
      DELINC = DT/60.
      DELX = REAL(INCRE) * DELINC
C
C     HERE THE PARAMETER COMING IN FROM COMMON IS THE NUMBER OF
C        TIME STEPS BETWEEN HYDROGRAPH POINTS.
C         DELX IS TIME BETWEEN POINTS
C
      Y = Q(1)
      IF (ITIME.LE.1) RETURN
      MRE = MOD(ITIME - 1,INCRE)
      IF (ITIME.LE.INCRE) MRE = ITIME - 1
      IF (MRE.NE.0) GO TO 100
      ISUB = ((ITIME - 1)/INCRE) + 1
      Y = Q(ISUB)
      RETURN
  100 CONTINUE
      DELINC = REAL(MRE) * DELINC
      ISUB = ((ITIME - 1)/INCRE) + 1
      P1 = Q(ISUB)
      ISUB = ISUB + 1
      P2 = Q(ISUB)
C     WRITE(N6,10) ISUB,MRE,ITIME,DELINC,DELX,P1,P2
      SLOPE = (P2 - P1)/DELX
      Y = SLOPE * DELINC + P1
      RETURN
C
      END

C-----------------------------------------------------------------------
      SUBROUTINE WELLE (DATEI,NMAX,Q,DT)

      INCLUDE 'DIMENS.INC'
       
      COMMON /IO/           N5          , N6          , IN          ,
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
         PRINT '(T2,3A)','WELLE ',DATEI,' MIT VORZEICHENWECHSEL !'
      END IF
      IF (TIMMOD.EQ.'ST') THEN
      PRINT'(T2,3A)','WELLE ',DATEI(1:28),' STATIONŽR MIT ANFANGSWERT !'
      END IF
      IF (TIMMOD.EQ.'MA') THEN
      PRINT'(T2,3A)','WELLE ',DATEI(1:28),' STATIONŽR MIT MAXIMUM !'
      END IF
      IF (TIMMOD.EQ.'MI') THEN
      PRINT'(T2,3A)','WELLE ',DATEI(1:28),' STATIONŽR MIT MINIMUM !'
      END IF
      IF (TIMMOD.EQ.'IM') THEN
      PRINT'(T2,3A)','WELLE ',DATEI(1:28),' INSTATIONŽR BIS MAXIMUM !'
      END IF
C
      IF (QFAK.EQ.0.0) THEN
         QFAK=1.0
      ELSE IF (QFAK.NE.0.0) THEN
         PRINT'(T2,3A,F8.2)','WELLE ',DATEI(1:28),' MIT FAKTOR ',QFAK
      END IF
C
      IF (QBAS.EQ.0.0) THEN
         QBAS=-10.**38
      ELSE IF (QBAS.NE.0.0) THEN
         PRINT'(T2,3A,F9.3)','WELLE ',DATEI(1:28),' MIT BASIS-Q ',QBAS
      END IF
C
      DATEI(29:30)='  '
      PRINT'(T2,A)',DATEI
      OPEN (21,FILE=DATEI,STATUS='OLD')
C
      READ (21,1) ZEILE
      IF (ZEILE(1:5).EQ.'WELLE') THEN
         PRINT'(T2,A)','FGM-WELLE!'
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
         WRITE(N6,5) DATEI
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
         WRITE(N6,5) DATEI
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
         PRINT *
         PRINT '(T2,3A)','WELLE ',DATEI,' MIT VARIABLEM ZEITSCHRITT !'
         PRINT *
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
         PRINT *
         PRINT '(T2,A)','UP WELLE: JMAX = 0 !'
         PRINT *
         STOP
      END IF
      END

C-----------------------------------------------------------------------
      SUBROUTINE XSECT (NX,IPRO,XSECMO,PRODAT,IAB4A)
C**********************************************************************
C           X S E C T
C**********************************************************************
C
C        THIS ROUTINE CALCULATES AREA, HYDRAULIC PERIMETER AND
C        TOP WIDTH FOR IRREGULAR X-SECTIONS.
C
C       DIESER PROGRAMMTEIL STAMMT AUS DEM INSTAT-PAKET UND WURDE
C       FUER UNSTDY MODIFIZIERT
C
C
      INCLUDE 'DIMENS.INC'
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /DM/           U(IPSEC)    , D(IPSEC)    , H(IPSEC)    ,
     1        Q(IPSEC)    , SF(IPSEC)   , XL(IPSEC)   , T(IPSEC)    ,
     2        AREA(IPSEC) , WP(IPSEC)   , DX(IPSEC)   , DPYH(IPSEC)
      COMMON /GEOWP/        HWP(IPSEC,IPSTP),IWP(IPSEC,IPSTP),
     1                      RNFAK(IPSEC,IPSTP),NNWP(IPSEC)
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
      COMMON /BAK/          CRNBAK(IPSEC)
C
      COMMON /MODUSOF/      ZOF(IPSEC)
C
      COMMON /MODUSDF/      ZDF(IPSEC)
C
      COMMON /SOHLE/        ZSHIFT(IPSEC)
C
      DIMENSION      X(IPCOO),Y(IPCOO)
C     KS-Wert-Profil (Strickler-Beiwert)
      DIMENSION      XKS(IPCOO),RKS(IPCOO)
C     ZS-Wert-Profil (Schwellenhöhe)
      DIMENSION      XZS(IPCOO),YZS(IPCOO)

      DIMENSION      JTYPE(IPSEC)
      CHARACTER*2 XSECMO,MODUS,KSMOD
      CHARACTER*1 CTAB
      CHARACTER*30 PRODAT
C
      DATA IDB /0/
C
      NSTOP = 0
      NSTP  = IPSTP
C
      KSEC = 0
      DO I=1,NX
         ZOF(I)=ZO(I)
         ZDF(I)=9999.9
         JTYPE(I) = ITYPE(I)
         CRNBAK(I) = 0.0
      END DO
C
C........ READ X-SECTION DATA
C
      IF(ISEC.LE.0) GOTO 2500
      IF(IPRO.NE.1) THEN
      WRITE(N6,535)
  535 FORMAT(1H1//10X,'KOORDINATEN DER UNREGELMAESSIGEN QUERSCHNITTE'/
     1      10X,46('-')//)
      ENDIF
 2500 CONTINUE

      IF (PRODAT(1:1).NE.' ') THEN
         CLOSE(N5,STATUS='KEEP')
         OPEN(N5,FILE=PRODAT,STATUS='OLD')
         PRINT*
         PRINT'(T2,A,A,A)','DATEI ',PRODAT,' GE™FFNET!'
         PRINT*
         PRINT*
         PRINT'(T2,A,I5)','SUCHE VON PROFIL ',IAB4A
         PRINT*
         NSECV=0
C
         DO 3400 KS=1,IPSEC
            IF (KS.GT.1.AND.NSEC.GT.0) NSECV=NSEC
            READ (N5,5000)  NSEC,NPTS,CTAB,MODUS,YBKMAX,STATION
            READ (N5,*)  (DUM,DUM,NS=1,NPTS)
            IF (NSEC.EQ.IAB4A.OR.NSEC.EQ.(-IAB4A)) GOTO 3410
 3400    CONTINUE
C
 3410    REWIND N5
         PRINT*
         PRINT'(T2,A,I5,A)','PROFIL ', IAB4A,' GEFUNDEN! REWIND!'
         PRINT*
C
         IF (NSECV.NE.0) THEN
            DO 3420 KS=1,IPSEC
               READ (N5,5000)  NSEC,NPTS,CTAB,MODUS,YBKMAX,STATION
               READ (N5,*)  (DUM,DUM,NS=1,NPTS)
               IF (NSEC.EQ.NSECV) GOTO 3430
 3420       CONTINUE
         END IF
C
      END IF
 3430 CONTINUE
      PRINT*
      PRINT'(T2,A,I5,A)','PROFIL ', NSECV,' GEFUNDEN! SCHLEIFE 3500!'
      PRINT*

C     Groáe Schleife ber den Querprofilzeiger (maximal alle X-Sections)
C     zun„chst Bearbeitung von ITYPE = 4

      OPEN(37,FILE='PSlot.plt',STATUS='UNKNOWN')
      WRITE(37,*) '"KNO","STAT","ZSLOT"'

      DO 3500 KS=1,NX

      NPKS=0
      NPZS=0

 3000 READ (N5,5000,END=4000)  NSEC,NPTS,CTAB,MODUS,YBKMAX,STATION
 5000 FORMAT (2I5,1X,A1,1X,A2,F5.0,9X,F10.2)

      IF (NSEC.LT.0) THEN

         IF (MODUS.EQ.'H2') THEN
            NPKS=NPTS

         WRITE(N6,*)' NSEC = ',-NSEC,'  MODUS = ',MODUS,'  NPKS = ',NPKS
          
            KSMOD=MODUS
          
            READ (N5,*)  (XKS(NK),RKS(NK),NK=1,NPKS)
            DO NK=1,NPKS
               XKS(NK)=SIENG(XKS(NK),1)
            END DO

             
         END IF

         IF (MODUS.EQ.'ZS') THEN
            NPZS = NPTS
             
         WRITE(N6,*)' NSEC = ',-NSEC,'  MODUS = ',MODUS,'  NPZS = ',NPZS
          
            READ (N5,*)  (XZS(NK),YZS(NK),NK=1,NPZS)
            DO NK=1,NPZS
               XZS(NK)=SIENG(XZS(NK),1)
               YZS(NK)=SIENG(YZS(NK),1)
            END DO
         END IF

         GOTO 3000

      ELSE

         IF (MODUS.EQ.'  ') NPKS=0
         IF (MODUS.EQ.'VK') NPKS=0
         IF (MODUS.EQ.'RK') NPKS=0
         IF (MODUS.EQ.'VF') NPKS=0
         IF (MODUS.EQ.'RF') NPKS=0
         IF (MODUS.EQ.'DF') NPKS=0
         IF (MODUS.EQ.'OF') NPKS=0

         WRITE(N6,*) ' NSEC = ',NSEC,'  MODUS = ',MODUS,'  NPKS = ',NPKS

         IF (MODUS.EQ.'  ') NPZS=0
         IF (MODUS.EQ.'VK') NPZS=0
         IF (MODUS.EQ.'RK') NPZS=0
         IF (MODUS.EQ.'VF') NPZS=0
         IF (MODUS.EQ.'RF') NPZS=0
         IF (MODUS.EQ.'DF') NPZS=0
         IF (MODUS.EQ.'OF') NPZS=0

         WRITE(N6,*) ' NSEC = ',NSEC,'  MODUS = ',MODUS,'  NPZS = ',NPZS

      END IF
       
      IF (CTAB.EQ.' ') ITAB=0
      IF (CTAB.EQ.'1') ITAB=1
      IF (MODUS.EQ.'  ') THEN
c        PRINT'(T2,A,A,A)','MODUS = ',XSECMO,' (XSECMO) WIRD GESETZT!'
         MODUS=XSECMO
      END IF
      IF (MODUS.EQ.' F'.OR.MODUS.EQ.' K') THEN
      PRINT *
      PRINT'(T2,A)','Berechnungsmodus im Querprofildatensatz nicht voll'
     &,'st„ndig definiert !'
      PRINT'(T2,A)','M”gliche Eingaben auf Pos. 14-15 der Kopfzeile:'
      PRINT *
      PRINT'(T2,A)','"VK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá erst wenn '
      PRINT'(T2,A)','       Wasserspiegel h”her als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RK" = Querschnittsparameter im Kernquerschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá erst wenn '
      PRINT'(T2,A)','       Wasserspiegel h”her als Kernquerschnitt;'
      PRINT'(T2,A)','       Querprofil wird auf Bereich zwischen '
      PRINT'(T2,A)','       linkem und rechtem Hochpunkt reduziert !'
      PRINT *
      PRINT'(T2,A)','"VF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá von Anfang an !'
      PRINT'(T2,A)','       Querprofil wie eingegeben angesetzt !'
      PRINT *
      PRINT'(T2,A)','"RF" = Querschnittsparameter im vollen Querschnitt'
      PRINT'(T2,A)','       d.h. breitfl„chiger Abfluá von Anfang an !'
      PRINT'(T2,A)','       Querprofil wird auf Bereich zwischen '
      PRINT'(T2,A)','       linkem und rechtem Hochpunkt reduziert !'
      PRINT *
      STOP 'STANDARD-MODUS ist "RK" !'
      END IF
C
      IF (MODUS.EQ.'RK'.OR.MODUS.EQ.'VK') THEN
         CONTINUE
      ELSE IF (MODUS.EQ.'RF'.OR.MODUS.EQ.'VF') THEN
         CONTINUE
      ELSE IF (MODUS.EQ.'DF'.OR.MODUS.EQ.'OF') THEN
         CONTINUE
      ELSE
c        PRINT'(T2,A,A,A)','MODUS = ',XSECMO,' (XSECMO) WIRD GESETZT!'
         MODUS=XSECMO
      END IF

      WRITE(N6,*) ' NSEC = ',NSEC,'  MODUS = ',MODUS,'  NPKS = ',NPKS
C
CCC   WRITE(*,5001) NSEC,NPTS,ITAB,MODUS,YBKMAX,STATION
 5001 FORMAT (2I5,1X,I1,1X,A2,1X,F5.1,3X,F10.2)
      NCO(KS) = NPTS
C
C .......NAC IST DIE LFD.NUMMER DES KNOTENS IM GESAMTSYSTEM
C ......NSEC IST DIE BEZEICHNUNG DES KNOTENS ( = IAB() )
C
      DO 3020 N=1,NX
         LAB(KS) = IAB(N)
         NAC = N
         IF (IAB(N)-NSEC)    3020,3040,3020
 3020 CONTINUE
      NSTOP = 1
      WRITE (N6,6040)  NSEC
 6040 FORMAT (//11H **********,' DATA ERROR -- CHANNEL NUMBER',I6,
     1        ' FOR VARIABLE CROSS-SECTION DOES NOT EXIST IN',
     2        ' THE SYSTEM',11H **********)
C     READ (N5,5020)  (DUM,DUM,NS=1,NPTS)
      READ (N5,*)  (DUM,DUM,NS=1,NPTS)
C     Sprung zum Einlesen der n„chsten Querprofilkopfzeile
      GO TO 3000
C     Hier wird die Anzahl der unregelm„áigen Querprofile hochgez„hlt
 3040 KSEC = KSEC + 1
      IF ( KSEC .LE. ISEC )  GO TO 3050
      WRITE (N6,6060)  ISEC
 6060 FORMAT (//11H **********,' DATA ERROR -- MORE VARIABLE',
     1        ' CROSS-SECTION DATA GIVEN AS THE',I6,
     2        ' CHANNELS SPECIFIED',11H **********)
C     Sprung zur Stop-Anweisung am Ende
      GO TO 7001
 3050 JTYPE(NAC) = 0
C     Abfrage, ob Dimensionierung ausreicht
      IF ( NPTS .LE. IPCOO )   GO TO 3055
      WRITE (N6,6080)  NSEC, NPTS, IPCOO
 6080 FORMAT (//11H **********,' DATA ERROR -- CHANNEL NUMBER',I6,
     1        ' FOR VARIABLE CROSS-SECTION'/
     2        T27,I4,' COORDINATE POINTS',
     3        ' SPECIFIED EXCEEDS PROGRAM DIMENSIONS OF',I3,
     4        11H **********)
C     Sprung zur Stop-Anweisung am Ende
      GO TO 7001
C
 3055 IF (ITYPE(NAC).GT.3.AND.ITYPE(NAC).NE.5) THEN
      XLKM=ENGSI(XL(NAC),1)/1000.
      XLM=XLKM*1000.
C
C     Einlesen der X,Y-Wertepaare des Querprofils im freien Format
      READ (N5,*)  (X(NS),Y(NS),NS=1,NPTS)
C
C     WRITE IRREGULAR CROSS SECTION DATA
C
      IF (IPRO.EQ.1) GO TO 540
      WRITE(N6,536) KSEC,NSEC,(X(NS),Y(NS),NS=1,NPTS)
  536 FORMAT(/5X,I3,'. UNREGELMAESSIGER QUERSCHNITT    (KNOTEN',I5,')'
     1 //10(5X,4(2F8.2,8X)/)/)
  540 CONTINUE
C
      DO 3057 NS = 1,NPTS
      X(NS) = SIENG( X(NS),1 )
      Y(NS) = SIENG( Y(NS),1 )
 3057 CONTINUE
C
C........ ESTABLISH X-SECTION LIMITS
C
      N1 = 1
      YBK = AMIN1 ( Y(1), Y(NPTS) )
 3075 ZBOT = 1.0E10
      DO 3080 NS=N1,NPTS
         IF ( Y(NS) .GT. ZBOT )  GO TO 3080
         ZBOT = Y(NS)
         NBOT = NS
 3080 CONTINUE

C     ABFRAGE OB TIEFSTER PUNKT LINKER ODER RECHTER RAND

      IF (NBOT.NE.N1) GOTO 3085
      WRITE(N6,'(T2,I5,A)')LAB(KS),': LINKER RAND IST TIEFSTER PUNKT !'
      WRITE(N6,'(T2,A)')'       PROFILPUNKT WIRD ABGESCHNITTEN !'
      N1 = N1+1
      GOTO 3075
 3085 IF (NBOT.NE.NPTS) GOTO 3087
      WRITE(N6,'(T2,I5,A)')LAB(KS),': RECHTER RAND IST TIEFSTER PUNKT !'
      WRITE(N6,'(T2,A)')'       PROFILPUNKT WIRD ABGESCHNITTEN !'
      NPTS=NPTS-1
      GOTO 3075
 3087 CONTINUE

C     LINKE UND RECHTE UEBERLAUFHOEHE BESTIMMEN

      YBKL=YBK
      YBKR=YBK
      NULI=N1
      NURE=NPTS
C     LINKES MAXIMUM
      DO 8001 NS=NBOT-1,1,-1
         IF (Y(NS).GT.YBKL) THEN
            YBKL=Y(NS)
            NULI=NS
         ENDIF
 8001 CONTINUE
C     RECHTES MAXIMUM
      DO 8003 NS=NBOT+1,NPTS
         IF (Y(NS).GT.YBKR) THEN
            YBKR=Y(NS)
            NURE=NS
         ENDIF
 8003 CONTINUE
C
      IF (IPRO.NE.1) THEN
      WRITE(N6,'(T2,I5,2(A,I5))')LAB(KS),':  N1 =',N1,'   NULI =',NULI
      WRITE(N6,'(T2,I5,2(A,I5))')LAB(KS),':NPTS =',NPTS,'   NURE =',NURE
      END IF
C
      IF (MODUS.EQ.'RK'.OR.MODUS.EQ.'RF') THEN
         IF (NULI.NE.N1) THEN
            WRITE(N6,'(T2,I5,A)')LAB(KS),': LINKER RAND NEU FESTGELEGT'
            WRITE(N6,'(T2,I5,A)')LAB(KS),': PROFILDATEN NEU SORTIERT'
         END IF
         IF (NURE.NE.NPTS) THEN
            WRITE(N6,'(T2,I5,A)')LAB(KS),': RECHTER RAND NEU FESTGELEGT'
            WRITE(N6,'(T2,I5,A)')LAB(KS),': PROFILDATEN NEU SORTIERT'
         END IF
C
         J=0
C        SCHLEIFE VOM LINKEN ZUM RECHTEN MAXIMUM
         DO 8005 NS=NULI,NURE
            J=J+1
            IF (NS.EQ.NBOT) NBOT=J
            X(J)=X(NS)
            Y(J)=Y(NS)
 8005    CONTINUE
         NPTS=J
      END IF
C
      NCO(KS)=NPTS
C     YBK=NIEDRIGSTER DER BEIDEN HOCHPUNKTE YBKL UND YBKR
      YBK=YBKR
      IF (YBKL.LT.YBKR) THEN
         YBK=YBKL
      END IF

      WRITE(N6,5022)  ENGSI(YBK,1),ENGSI(ZBOT,1),NSTP
5022  FORMAT(1X,'YBK= ',F7.2,'  ZBOT= ',F7.2,'  NSTP= ',I5)
C
C     AUSGABE AUF PRODAT.PLT, WIRD BEN™TIGT FšR WSP.PLT
C
      ZL(NAC)=Y(1)
      ZR(NAC)=Y(NPTS)
C
C
      HEIT(NAC) = YBK - ZBOT
C
C     Festlegung der Ausbordungstiefe ZS(NAC)
C
C     Ge„ndert am 02.02.93. Gerd.
C     ursprnglich war es so:
C     ZS(NAC) = HEIT(NAC)
C
      IF (ZS(NAC).GT.HEIT(NAC)) ZS(NAC) = HEIT(NAC)
C
C
C     Festlegung des Tiefenbereichs fr die Querschnittsparameter,
C     zuvor gegebenenfalls "knstliche" Erh”hung der R„nder des
C     Querprofils auf H”he YBKMAX
C
      IF (YBKMAX.GT.0.0) THEN
C        YBKMAX AUS PROFILDATEN WIRD VERWENDET
         YBKMAX = SIENG(YBKMAX,1)
      ELSE IF (YBKMAX.LT.0.0) THEN
C        YBKMAX=MAXIMUM DER BEIDEN HOCHPUNKTE YBKL UND YBKR
         YBKMAX=YBKL
         IF (YBKR.GT.YBKL) YBKMAX=YBKR
         YBKMAX=YBKMAX+0.1
      END IF
C
      HZERO=ZO(NAC)+DZERO(NAC)
C
      IF (HZERO.GT.YBKMAX) YBKMAX=HZERO+SIENG(1.0,1)
C
      DBKMAX=YBKMAX-ZO(NAC)
C
      DBKMAX=ENGSI(DBKMAX,1)
      IF (DBKMAX.LT.5.0) DBKMAX=5.0
C
c     IF (DBKMAX.LT.4.0) THEN
c       DBKMAX=DBKMAX+1.0
c     ELSE IF (DBKMAX.GE.4.0.AND.DBKMAX.LT.5.0) THEN
c        DBKMAX=5.0
c     END IF
C
      DBKMAX=SIENG(DBKMAX,1)
C
      YBKMAX=ZO(NAC)+DBKMAX
C
C     Deckenhöhe für MODUS = DF wird zunächst auf YBKMAX gesetzt
C
      IF (MODUS.EQ.'DF') ZDF(NAC)=YBKMAX
C
C
      IF (ITAB.EQ.1) THEN
         WRITE(N6,'(T2,A)')' '
         WRITE(N6,'(T2,I5)')LAB(KS)
         WRITE(N6,'(T2,A,F10.2)')'  YBKL = ',ENGSI(YBKL,1)
         WRITE(N6,'(T2,A,F10.2)')'  YBKR = ',ENGSI(YBKR,1)
         WRITE(N6,'(T2,A,F10.2)')'YBKMAX = ',ENGSI(YBKMAX,1)
         WRITE(N6,'(T2,A)')' '
      END IF
C
      IF (YBKMAX.GT.YBKL.AND.YBKMAX.GT.YBKR) THEN
C        Beide R„nder werden erh”ht
         WRITE(N6,'(T2,I5,A)')LAB(KS),': Beide R„nder werden erh”ht !'
         YBK=YBKMAX
         NPTS=NPTS+2
         NBOT=NBOT+1
         DO 5024 NS=NPTS-1,2,-1
            X(NS)=X(NS-1)
            Y(NS)=Y(NS-1)
5024     CONTINUE
         X(NPTS)=X(NPTS-1)
         Y(NPTS)=YBK
         X(1)=X(2)
         Y(1)=YBK
      ELSE IF (YBKMAX.GT.YBKL.AND.YBKMAX.LE.YBKR) THEN
C        linker Rand wird erh”ht
         WRITE(N6,'(T2,I5,A)')LAB(KS),': Linker Rand wird erh”ht !'
         YBK=YBKMAX
         NPTS=NPTS+1
         NBOT=NBOT+1
         DO 5025 NS=NPTS,2,-1
            X(NS)=X(NS-1)
            Y(NS)=Y(NS-1)
5025     CONTINUE
         X(1)=X(2)
         Y(1)=YBK
      ELSE IF (YBKMAX.LE.YBKL.AND.YBKMAX.GT.YBKR) THEN
C        rechter Rand wird erh”ht
         WRITE(N6,'(T2,I5,A)')LAB(KS),': Rechter Rand wird erh”ht !'
         YBK=YBKMAX
         NPTS=NPTS+1
         X(NPTS)=X(NPTS-1)
         Y(NPTS)=YBK
      ELSE IF (YBKMAX.LE.YBKL.AND.YBKMAX.LE.YBKR.AND.YBKMAX.NE.0.0) THEN
C        YBK=YBKMAX wird gesetzt
         WRITE(N6,'(T2,I5,A)')LAB(KS),': YBK=YBKMAX wird gesetzt !'
         YBK=YBKMAX
      END IF
C
C     gesamte Querschnittsh”he
C
      HEIT(NAC) = YBK - ZBOT
C
C     Schrittweite fr Parametertabelle
C
      DELY = ( YBK - ZBOT ) / ( NSTP - 1 )
C
C....... DETERMINE AREA, TOP WIDTH AND HYDRAULIC PERIMETER
C
C
      OPEN(60,FILE='XHATWRKN.DAT',STATUS='UNKNOWN')
C     IF (IKPF60.EQ.0) THEN
         WRITE(60,'(A,A)')'"  XL[KM]" "H[M+NN]"   "A[M2]"  "TWI[M]"',
     &                    '  "DTW/DH"  "WPE[M]"  "RHY[M]"  "KNOTEN"'
C        IKPF60=1
C     END IF
C
      OPEN(61,FILE='QGRENZ.DAT',STATUS='UNKNOWN')
C
      IF (MODUS.EQ.'RK'.OR.MODUS.EQ.'VK') THEN
         CALL XSK (NSEC,X,Y,NPTS,DELY,ITAB,NSTP,KS,ZBOT,NBOT,XLKM)
      ELSE IF (MODUS.EQ.'RF'.OR.MODUS.EQ.'VF') THEN
         CALL XSF (NSEC,X,Y,NPTS,DELY,ITAB,NSTP,KS,ZBOT,XLKM,
     &             NPKS,XKS,RKS,NPZS,XZS,YZS,MODUS,NAC,YBKMAX)
      ELSE IF (MODUS.EQ.'DF'.OR.MODUS.EQ.'OF') THEN
         CALL XSF (NSEC,X,Y,NPTS,DELY,ITAB,NSTP,KS,ZBOT,XLKM,
     &             NPKS,XKS,RKS,NPZS,XZS,YZS,MODUS,NAC,YBKMAX)
      ELSE
         PRINT'(T2,A,A)','MODUS = ',MODUS
         STOP 'UP XSECT: KEIN MODUS DEFINIERT !'
      END IF
C
C........ DEBUG OUTPUT
C

C
C....... CORRECTION WHEN BOTTOM OF SECTION I USED
C         IS NOT EQUAL TO BED ELEVATION AT SECTION I
C
C     ZO(NAC) = SOHLE AUS KNOTENTABELLE
C     ZBOT    = TIEFSTER PUNKT AUS QUERPROFIL
C
      HCORR = ZO(NAC) - ZBOT

      ZSHIFT(NAC)=HCORR

      IF (ABS(HCORR).GE.0.01) THEN

        WRITE(N6,*) '  KNO = ',LAB(KS)
        WRITE(N6,*) '   ZO = SOHLE KNOTENTABELLE  = ',ENGSI(ZO(NAC),1)
        WRITE(N6,*) ' ZBOT = TIEFPUNKT QUERPROFIL = ',ENGSI(ZBOT,1)
        WRITE(N6,*) 'HCORR = ZO - ZBOT            = ',ENGSI(HCORR,1)
        ZBOT = ZBOT + HCORR
        YBK  = YBK  + HCORR
        ZL(NAC)=ZL(NAC) + HCORR
        ZR(NAC)=ZR(NAC) + HCORR

        DO II=1,NPTS
          Y (II) = Y (II) + HCORR
        END DO

      ELSE

        ZSHIFT(NAC)=0.0

      END IF

C  UEBERPRUEFUNG GROBER QUERSCHNITTSFEHLER
      YMAX = ENGSI(Y(NPTS),1) - ENGSI(Y(1),1)
      IF (YMAX.LE.5.) GO TO 3500
      WRITE (N6,3490) IAB(KS),YMAX
 3490 FORMAT(////' WARNUNG : QUERSCHNITT',I5,' IST',F8.2,' M HOCH')
C
      END IF
C
 3500 CONTINUE

      PRINT '(T2,A)','XSECT SCHLEIFE 3500 ENDE '

C
 4000 KSI=KS-1
      NSTOP=0
C

      PRINT '(T2,A)','XSECT SCHLEIFE 3600 ANFANG '
      PRINT'(T2,A,I5)','KSI = ',KSI

      DO 3600 NI=1,NX
C
       IF (ITYPE(NI).EQ.5) THEN
C
C        PRINT'(T2,A,I5,A)','Knoten ',IAB(NI),' mit ITYPE = 5 gefunden!'
         KSI=KSI+1
         IF (KSI.GT.NX) THEN
            PRINT'(T2,A,I5)',' NI = ',NI
            PRINT'(T2,A,I5)','IAB = ',IAB(NI)
            PRINT'(T2,A,I5)','KSI = ',KSI
            PRINT'(T2,A,I5)',' NX = ',NX
            STOP 'UP XSECT: KSI > NX !'
         END IF
         LAB(KSI)=IAB(NI)
C
         NACI=NI
C
C        Knoten oberhalb und unterhalb suchen
C
         DO 3650 N=NI,1,-1
           ITYP=ITYPE(N)
           IF (ITYP.EQ.4.OR.ITYP.EQ.6.OR.ITYP.EQ.8.OR.ITYP.EQ.18) THEN
             NO=N
             GOTO 3651
           END IF
 3650    CONTINUE
 3651    CONTINUE
C        PRINT'(T2,A,I5,A)','Knoten ',IAB(NO),' oberhalb!'
C
         DO 3660 N=NI,NX,1
           ITYP=ITYPE(N)
           IF (ITYP.EQ.4.OR.ITYP.EQ.6.OR.ITYP.EQ.8.OR.ITYP.EQ.18) THEN
             NU=N
             GOTO 3661
           END IF
 3660    CONTINUE
 3661    CONTINUE
C3661    PRINT'(T2,A,I5,A)','Knoten ',IAB(NU),' unterhalb!'
C
C        Suchen von KSO und KSU
C
         DO 3610 KS=1,KSEC
            IF (LAB(KS).EQ.IAB(NO)) THEN
               KSO=KS
               NACO=NO
C              PRINT'(T2,A,I5)','KSO  = ',KSO
C              PRINT'(T2,A,I5)','NACO = ',NACO
            END IF
            IF (LAB(KS).EQ.IAB(NU)) THEN
               KSU=KS
               NACU=NU
C              PRINT'(T2,A,I5)','KSU  = ',KSU
C              PRINT'(T2,A,I5)','NACU = ',NACU
            END IF
 3610    CONTINUE
C
         WRITE(N6,3090) LAB(KSO)
 3090    FORMAT(///5X,'QUERSCHNITT',I5,' OBERHALB DER INTERPOLATION',//
     &   5X,' KNOTEN   LFD.NR      H      FLAECHE   WSP.BREITE ',
     &   ' BEN.UMFANG'/)
C
C        Schrittweite fr Parametertabelle Knoten oberhalb
C
C???     am 23.5.95 richtigen Zeiger NACO eingebaut !
C???     DELY = HEIT(KSO)/REAL(NSTP-1)
         DELY = HEIT(NACO)/REAL(NSTP-1)
C
C        Ausgabe der Parametertabelle Knoten oberhalb
C
         DO 3630 I=1,NSTP
            HH = ZO(NO) + REAL(I-1)*DELY
            HWR = ENGSI (HH,1)
            AWR = ENGSI (ASEC(KSO,I),2)
            TWR = ENGSI (TSEC(KSO,I),1)
            WWR = ENGSI (WSEC(KSO,I),1)
            QGR = ENGSI (QGRSEC(KSO,I),3)
            WRITE(N6,'(1X,2I9,6F12.3)') LAB(KSO),I,HWR,AWR,TWR,WWR,QGR
 3630    CONTINUE
C
         XO=XL(NO)
         XI=XL(NI)
         XU=XL(NU)
c        WRITE(N6,'(1X,A,I5)') 'NO = ',NO
c        WRITE(N6,'(1X,A,I5)') 'NI = ',NI
c        WRITE(N6,'(1X,A,I5)') 'NU = ',NU
c        WRITE(N6,'(1X,A,F12.3)') 'XO = ',ENGSI(XO,1)
c        WRITE(N6,'(1X,A,F12.3)') 'XI = ',ENGSI(XI,1)
c        WRITE(N6,'(1X,A,F12.3)') 'XU = ',ENGSI(XU,1)
C        Interpolation der H”he
C???     am 23.5.95 richtige Zeiger NACI,NACU,NACO eingebaut !
C???     HEIT(KSI)=HEIT(KSU)+(XI-XU)*(HEIT(KSO)-HEIT(KSU))/(XO-XU)
         HEIT(NACI)=HEIT(NACU)+(XI-XU)*(HEIT(NACO)-HEIT(NACU))/(XO-XU)
C        Interpolation der Sohle
         ZO(NI)=ZO(NU)+(XI-XU)*(ZO(NO)-ZO(NU))/(XO-XU)
C        Interpolation der fitiven Sohle für MODUS = OF
         ZOF(NI)=ZOF(NU)+(XI-XU)*(ZOF(NO)-ZOF(NU))/(XO-XU)
C        Interpolation der Deckenhöhe für MODUS = DF
         ZDF(NI)=ZDF(NU)+(XI-XU)*(ZDF(NO)-ZDF(NU))/(XO-XU)
C        Interpolation linke Randh”he
         ZL(NI)=ZL(NU)+(XI-XU)*(ZL(NO)-ZL(NU))/(XO-XU)
C        Interpolation rechte Randh”he
         ZR(NI)=ZR(NU)+(XI-XU)*(ZR(NO)-ZR(NU))/(XO-XU)
C        Interpolation der šberfallh”hendifferenz
C        ZS(NI)=ZS(NU)+(XI-XU)*(ZS(NO)-ZS(NU))/(XO-XU)
         ZSINP=ZS(NU)+(XI-XU)*(ZS(NO)-ZS(NU))/(XO-XU)
         IF (ZS(NI).GT.ZSINP) ZS(NI)=ZSINP
C
         WRITE(N6,'(A,I5)') 'KSI = ',KSI
         WRITE(N6,3091) LAB(KSI)
 3091    FORMAT(///5X,'QUERSCHNITT',I5,' AN DER INTERPOLATIONSTELLE',//
     &   5X,' KNOTEN   LFD.NR      H      FLAECHE   WSP.BREITE ',
     &   ' BEN.UMFANG  HYDR.RADIUS   CARHY   QGRENZ'/)
C
C        Schrittweite fr Parametertabelle an Interpolationsstelle
C
C???     am 23.5.95 richtigen Zeiger NACI eingebaut!
C???     DELY = HEIT(KSI)/REAL(NSTP-1)
         DELY = HEIT(NACI)/REAL(NSTP-1)
C
C        WASSERSTAND HZERO
C
c        WRITE(*,'(A,F8.3)') '    ZO = ', ENGSI(ZO(NI),1)
c        WRITE(*,'(A,F8.3)') ' DZERO = ', ENGSI(DZERO(NI),1)
c        WRITE(*,'(A,F8.3)') ' HZERO = ', ENGSI(HZERO,1)
         HZERO=ZO(NI)+DZERO(NI)
         HV=ZO(NI)
         AWRV=0.0
         WWRV=0.0
         RHYV=0.0
         CARHYV=0.0
         RHYVV=0.0
         NWP=0
C
         DO 3631 I=1,NSTP
C          Bestimmung des Wasserstandes
           HH = ZO(NI) + REAL(I-1)*DELY
C          Interpolation der Fl„che
           ASEC(KSI,I)=(ASEC(KSO,I)-ASEC(KSU,I))/(XO-XU)
           ASEC(KSI,I)=ASEC(KSU,I)+(XI-XU)*ASEC(KSI,I)
C          Interpolation der Breite
           TSEC(KSI,I)=(TSEC(KSO,I)-TSEC(KSU,I))/(XO-XU)
           TSEC(KSI,I)=TSEC(KSU,I)+(XI-XU)*TSEC(KSI,I)
C          Interpolation des ben. Umfangs
           WSEC(KSI,I)=(WSEC(KSO,I)-WSEC(KSU,I))/(XO-XU)
           WSEC(KSI,I)=WSEC(KSU,I)+(XI-XU)*WSEC(KSI,I)
C          Interpolation der Conveyance im HEC2-Stil
           CH2SEC(KSI,I)=(CH2SEC(KSO,I)-CH2SEC(KSU,I))/(XO-XU)
           CH2SEC(KSI,I)=CH2SEC(KSU,I)+(XI-XU)*CH2SEC(KSI,I)
C          Interpolation der Conveyance im HEC-RAS-Stil
           CHRSEC(KSI,I)=(CHRSEC(KSO,I)-CHRSEC(KSU,I))/(XO-XU)
           CHRSEC(KSI,I)=CHRSEC(KSU,I)+(XI-XU)*CHRSEC(KSI,I)
C          Interpolation des Grenzabflusses
           QGRSEC(KSI,I)=(QGRSEC(KSO,I)-QGRSEC(KSU,I))/(XO-XU)
           QGRSEC(KSI,I)=QGRSEC(KSU,I)+(XI-XU)*QGRSEC(KSI,I)
C          Korrektur für MODUS OF
           IF (I.GT.1) THEN
              IF (ZOF(NI).GT.ZO(NI).AND.HH.LE.ZOF(NI)) THEN
                 ASEC0=ASEC(KSI,I)
                 RHY0=ASEC(KSI,I)/WSEC(KSI,I)
                 QGRSEC0=QGRSEC(KSI,I)
                 ASEC(KSI,I)=0.3
                 TSEC(KSI,I)=0.15
                 WSEC(KSI,I)=30.0
                 QGRSEC(KSI,I)=0.003
              ELSE IF (ZOF(NI).GT.ZO(NI).AND.HH.GT.ZOF(NI)) THEN
                 RHY=ASEC(KSI,I)/WSEC(KSI,I)-RHY0+0.03
                 ASEC(KSI,I)=ASEC(KSI,I)-ASEC0+0.3
                 WSEC(KSI,I)=ASEC(KSI,I)/RHY
                 QGRSEC(KSI,I)=QGRSEC(KSI,I)-QGRSEC0+0.003
                 IF (QGRSEC(KSI,I).LT.0.0) QGRSEC(KSI,I)=0.0
              END IF
           END IF
C          Ausgabe
           HWR = ENGSI (HH,1)
           AWR = ENGSI (ASEC(KSI,I),2)
           TWR = ENGSI (TSEC(KSI,I),1)
           WWR = ENGSI (WSEC(KSI,I),1)
           QGR = ENGSI (QGRSEC(KSI,I),3)
           IF (I.GT.1) THEN
             RHY = AWR/WWR
             IF (RHY.LT.RHYV) THEN
c              WRITE(N6,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
c              WRITE(*,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
               WRITE(N6,'(A)') 'TYP5 PROBLEM: RHY WIRD KLEINER!'
c              WRITE(*,'(A)') 'TYP5 PROBLEM: RHY WIRD KLEINER!'
             END IF
             IF (AWR.LT.AWRV) THEN
c              WRITE(N6,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
c              WRITE(*,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
               WRITE(N6,'(A)') 'TYP5 FEHLER: FLŽCHE WIRD KLEINER!'
c              WRITE(*,'(A)') 'TYP5 FEHLER: FLŽCHE WIRD KLEINER!'
             END IF
             IF (WWR.LT.WWRV) THEN
c              WRITE(N6,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
c              WRITE(*,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
               WRITE(N6,'(A)') 'TYP5 FEHLER: UMFANG WIRD KLEINER!'
c              WRITE(*,'(A)') 'TYP5 FEHLER: UMFANG WIRD KLEINER!'
             END IF
           ELSE
             RHY=0.0
           END IF

           GEO = RHY**(2./3.)
           CARHY = AWR*GEO

           IF (CARHY.LT.CARHYV) THEN
              WRITE(N6,*) '   1. RHY alt = ',RHY,'    WWR alt = ',WWR
              GEO = CARHYV/AWR
              RHY = GEO**(3./2.)
              WWR = AWR/RHY
              WSEC(KSI,I) = SIENG(WWR,1)
              CARHY=CARHYV
              WRITE(N6,*) '   1. RHY neu = ',RHY,'    WWR neu = ',WWR
           END IF

c           IF (RHY.LT.RHYV) THEN
c             WRITE(N6,*) '   2. RHY alt = ',RHY,'    WWR alt = ',WWR
c             RHY=0.5*(RHY+RHYV)
c             RHY=RHYV
c             WWR = AWR/RHY
c              WSEC(KSI,I) = SIENG(WWR,1)
c              GEO = RHY**(2./3.)
c              CARHY = AWR*GEO
c              WRITE(N6,*) '   2. RHY neu = ',RHY,'    WWR neu = ',WWR
c           END IF

           IF (TWR.GT.WWR) THEN
              TWR=WWR
              TSEC(KSI,I) = SIENG(TWR,1)
           END IF
 
           IF (NWP.EQ.0) THEN
C             1. FORMALER RHY-WENDEPUNKT = WASSERSPIEGEL
              NWP=NWP+1
              NNWP(KSI)=NWP
              RNFAK(KSI,NWP)=1.0
              HWP(KSI,NWP)=HZERO
              IWP(KSI,NWP)=I
C          WRITE(N6,3213) NWP,ENGSI(HZERO,1),IWP(KSI,NWP),RNFAK(KSI,NWP)
           END IF
           WRITE(N6,'(1X,2I9,7F12.3)') LAB(KSI),I,HWR,AWR,TWR,WWR,RHY,
     &                                 CARHY,QGR
           IF (RHYV.GE.RHYVV) THEN
C            RHY STEIGEND
             IF (RHY.LT.RHYV.AND.HV.GE.HZERO) THEN
C              ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
               NWP=NWP+1
               IF (NWP.EQ.2) GEO1=GEOV
               RNFAK(KSI,NWP)=GEOV/GEO1
               HWP(KSI,NWP)=HV
               IWP(KSI,NWP)=I-1
C             WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KSI,NWP),RNFAK(KSI,NWP)
             END IF
           ELSE IF (RHYV.LT.RHYVV) THEN
C            RHY FALLEND
             IF (RHY.GT.RHYV.AND.HV.GE.HZERO) THEN
C              ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
               NWP=NWP+1
               IF (NWP.EQ.2) GEO1=GEOV
               RNFAK(KSI,NWP)=GEOV/GEO1
               HWP(KSI,NWP)=HV
               IWP(KSI,NWP)=I-1
C             WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KSI,NWP),RNFAK(KSI,NWP)
 3213 FORMAT(I5,'. RHY-WENDEP.: ',F8.3,' I = ',I2,' RNFAK = ',F8.3)
             END IF
           END IF
           IF (I.EQ.NSTP) THEN
C            LETZTER WERT IN PROFILTABELLE
             NWP=NWP+1
             NNWP(KSI)=NWP
             IF (NWP.EQ.2) GEO1=GEO
             RNFAK(KSI,NWP)=GEO/GEO1
             HWP(KSI,NWP)=HH
             IWP(KSI,NWP)=I
C            WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KSI,NWP),RNFAK(KSI,NWP)
           END IF
           AWRV=AWR
           WWRV=WWR
           RHYVV=RHYV
           RHYV=RHY
           CARHYV=CARHY
           GEOV=GEO
           HV=HH
 3631    CONTINUE
         NNWP(KSI)=NWP
C
         WRITE(N6,3092) LAB(KSU)
 3092    FORMAT(///5X,'QUERSCHNITT',I5,' UNTERHALB DER INTERPOLATION',//
     &   5X,' KNOTEN   LFD.NR      H      FLAECHE   WSP.BREITE ',
     &   ' BEN.UMFANG'/)
C
C        Schrittweite fr Parametertabelle Knoten unterhalb
C
C???     am 23.5.95 richtigen Zeiger NACU eingebaut!
C???     DELY = HEIT(KSU)/REAL(NSTP-1)
         DELY = HEIT(NACU)/REAL(NSTP-1)
C
C        Ausgabe der Parametertabelle Knoten unterhalb
C
         DO 3632 I=1,NSTP
            HH = ZO(NU) + REAL(I-1)*DELY
            HWR = ENGSI (HH,1)
            AWR = ENGSI (ASEC(KSU,I),2)
            TWR = ENGSI (TSEC(KSU,I),1)
            WWR = ENGSI (WSEC(KSU,I),1)
            QGR = ENGSI (QGRSEC(KSU,I),3)
            WRITE(N6,'(1X,2I9,6F12.3)') LAB(KSU),I,HWR,AWR,TWR,WWR,QGR
 3632    CONTINUE
C
         JTYPE(NI)=0
C
       END IF
C
 3600 CONTINUE

      PRINT '(T2,A)','XSECT SCHLEIFE 3600 ENDE '

C
      IF(IPRO.NE.1) WRITE(N6,803)
  803 FORMAT (1H1)
      DO 4020 N = 1, NX
      IF ( JTYPE(N) .LE. 3 )   GO TO 4020
      WRITE (N6,6180)  IAB(N), N
 6180 FORMAT (//11H **********,' DATA ERROR -- CHANNEL',
     1        ' NO.',I6,' ( N = ',I5,' ) LACKS X-SECTION',
     2        ' DATA',11H **********)
      NSTOP = 1
 4020 CONTINUE
C
      CLOSE(60,STATUS='KEEP')
      CLOSE(61,STATUS='KEEP')
      CLOSE(37,STATUS='KEEP')
C
      IF (NSTOP.EQ.0) GO TO 8000
 7001 PRINT '(T2,A,I5)',' ISEC = ',ISEC
      PRINT '(T2,A,I5)','   NX = ',NX
      PRINT '(T2,A,I5)','   KS = ',KS
      PRINT '(T2,A,I5)','    N = ',N
      PRINT '(T2,A,I5)',' NSEC = ',NSEC
      PRINT '(T2,A,I5)',' KSEC = ',KSEC
      PRINT '(T2,A,I5)','NSTOP = ',NSTOP
      STOP 7001
 8000 RETURN
 3210 FORMAT(1X,2I9,6F12.3)
      END

C-----------------------------------------------------------------------
      SUBROUTINE XSF (NSEC,X,Y,NPTS,DELY,ITAB,NSTP,KS,ZBOT,XLKM,
     &                NPKS,XKS,RKS,NPZS,XZS,YZS,MODUS,NAC,YBKMAX)
C**********************************************************************
C     X S F
C**********************************************************************
C
C     BERECHNUNG DER QUERSCHNITTSPARAMETER AUSGEHEND VOM VOLLQUERSCHNITT
C
C     Voraussetzung: im Programm XSECT  MODUS = 'RF' oder 'VF'
C                    im Programm XSECT  MODUS = 'DF' oder 'OF'
C
C**********************************************************************

      INCLUDE 'DIMENS.INC'
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
C
C     CH2SEC = Conveyance im HEC-2-Stil
C     CHRSEC = Conveyance im HEC-RAS-Default-Stil
C
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC),
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /GEOWP/        HWP(IPSEC,IPSTP),IWP(IPSEC,IPSTP),
     1                      RNFAK(IPSEC,IPSTP),NNWP(IPSEC)
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
C
      COMMON /MODUSOF/      ZOF(IPSEC)
C
      COMMON /MODUSDF/      ZDF(IPSEC)
C
      DIMENSION      X(IPCOO),Y(IPCOO), XP(0:310), YP(0:310)
      REAL  SUM1, SUM2
      CHARACTER*2 MODUS
C
C     VEREINBARUNGEN FšR UNTERTEILUNGEN ZUNŽCHST IM HEC-2-STIL
C
      REAL WTS(IPCOO,IPSTP),AS(IPCOO,IPSTP)
      REAL CONV(IPCOO,IPSTP)
      REAL XKS(IPCOO),RKS(IPCOO)
      REAL XZS(IPCOO),YZS(IPCOO)
C
C     WST(JS,I)=BENETZTER UMFANG IN SEGMENT JS UND LAMELLE I
C     AS(JS,I)=FLŽCHENANTEIL IN SEGMENT JS UND LAMELLE I (I=max26)
C
C     VEREINBARUNGEN FÜR MODUS 'OF':
C
      REAL AWRDF(IPSTP), TWRDF(IPSTP), WWRDF(IPSTP), QGRDF(IPSTP)
      REAL AWROF(IPSTP), TWROF(IPSTP), WWROF(IPSTP), QGROF(IPSTP)
C
      XLM=XLKM*1000.

      DO IPC=1,IPCOO
         DO IPS=1,IPSTP
            AS(IPC,IPS)=0.0
            WTS(IPC,IPS)=0.0
            CONV(IPC,IPS)=0.0
         END DO
      END DO
C
C     SUCHEN DES WASSERSTANDES HZERO
C
      DO 3088 I=1,IPSEC
         IF (LAB(KS).EQ.IAB(I)) THEN
c           WRITE(*,'(A,F8.3)') '    ZO = ', ENGSI(ZO(I),1)
c           WRITE(*,'(A,F8.3)') ' DZERO = ', ENGSI(DZERO(I),1)
c           WRITE(*,'(A,F8.3)') ' HZERO = ', ENGSI(HZERO,1)
            HZERO=ZO(I)+DZERO(I)
            ITYP=ITYPE(I)
         END IF
 3088 CONTINUE
C
C     ANFANGSWERTE
C
      AREA=999999999.
      ASEC(KS,1) = 0.0
      TSEC(KS,1) = 0.0
      WSEC(KS,1) = 0.0
      CH2SEC(KS,1) = 0.0
      CHRSEC(KS,1) = 0.0
      QGRSEC(KS,1) = 0.0
      AWRDF(1) = 0.0
      TWRDF(1) = 0.0
      WWRDF(1) = 0.0
      QGRDF(1) = 0.0
      AWROF(1) = 0.0
      TWROF(1) = 0.0
      WWROF(1) = 0.0
      QGROF(1) = 0.0
      XPSUKL=0.0
      XPSOKL=0.0
      XPSUKR=0.0
      XPSOKR=0.0
      YPSUKL=0.0
      YPSOKL=0.0
      YPSUKR=0.0
      YPSOKR=0.0
      XPSUK=0.0
      XPSOK=0.0
      YPSUK=0.0
      YPSOK=0.0

      AT = 0.0
      WT = 0.0
      H = ZBOT
      WDEL = ENGSI(DELY,1)
      IF (ITAB.GT.0) THEN
         WRITE(N6,3089) LAB(KS),WDEL
 3089    FORMAT(///5X,'QUERSCHNITT',I5,'    H-SCHRITTWEITE',F10.3,' M'//
     &   5X,' LFD.NR   KNOTEN      H      FLAECHE   WSP.BREITE ',
     &   ' BEN.UMFANG  HYDR.RADIUS      QGRENZ      CGRENZ   ',
     &   '    CARHY       ASSUM       WTSUM       COSUM   '/)
      END IF
C
C     Versuch, OK und UK von Preissmann-Slots zu erkennen
C
C     von links nach rechts
      DO N=1,NPTS-2,1
         IF (N.EQ.1) THEN
            IF (X(N).EQ.X(N+1).AND.Y(N).GT.Y(N+1)) THEN
               IF (Y(N).LT.YBKMAX) THEN
                  XPSOKL=X(N)
                  YPSOKL=Y(N)
                  XPSUKL=X(N+1)
                  YPSUKL=Y(N+1)
               END IF
            END IF
         ELSE IF (N.GT.1) THEN
            IF (X(N).EQ.X(N+1).AND.Y(N).GE.Y(N+1)) THEN
               IF (X(N).GE.X(N-1).AND.X(N+1).GT.X(N+2)) THEN
                  IF (Y(N).GE.Y(N+1)) THEN
                     XPSOKL=X(N)
                     YPSOKL=Y(N)
                     XPSUKL=X(N+1)
                     YPSUKL=Y(N+1)
                  END IF
               END IF
            END IF
         END IF
      END DO
C
C     von rechts nach links
      DO N=NPTS,3,-1
         IF (N.EQ.NPTS) THEN
            IF (X(N).EQ.X(N-1).AND.Y(N).GT.Y(N-1)) THEN
               IF (Y(N).LT.YBKMAX) THEN
                  XPSOKR=X(N)
                  YPSOKR=Y(N)
                  XPSUKR=X(N-1)
                  YPSUKR=Y(N-1)
               END IF
            END IF
         ELSE IF (N.LT.NPTS) THEN
            IF (X(N).EQ.X(N-1).AND.Y(N).GE.Y(N-1)) THEN
               IF (X(N).LE.X(N+1).AND.X(N-1).LT.X(N-2)) THEN
                  IF (Y(N).GE.Y(N-1)) THEN
                     XPSOKR=X(N)
                     YPSOKR=Y(N)
                     XPSUKR=X(N-1)
                     YPSUKR=Y(N-1)
                  END IF
               END IF
            END IF
         END IF
      END DO
C
c      IF (ITAB.GT.0) THEN
         WRITE(N6,'(A,F6.2)') '  XPSOKL = ',ENGSI(XPSOKL,1)
         WRITE(N6,'(A,F6.2)') '  YPSOKL = ',ENGSI(YPSOKL,1)
         WRITE(N6,'(A,F6.2)') '  XPSOKR = ',ENGSI(XPSOKR,1)
         WRITE(N6,'(A,F6.2)') '  YPSOKR = ',ENGSI(YPSOKR,1)
         WRITE(N6,'(A,F6.2)') '  XPSUKL = ',ENGSI(XPSUKL,1)
         WRITE(N6,'(A,F6.2)') '  YPSUKL = ',ENGSI(YPSUKL,1)
         WRITE(N6,'(A,F6.2)') '  XPSUKR = ',ENGSI(XPSUKR,1)
         WRITE(N6,'(A,F6.2)') '  YPSUKR = ',ENGSI(YPSUKR,1)
c      END IF
C
      DXOK=XPSOKR-XPSOKL
      DXUK=XPSUKR-XPSUKL
C
c      IF (ITAB.GT.0) THEN
c         WRITE(N6,'(A,F6.2)') '  DXOK = ',ENGSI(DXOK,1)
c         WRITE(N6,'(A,F6.2)') '  DXUK = ',ENGSI(DXUK,1)
c      END IF
C
      IF (ENGSI(DXOK,1).GT.0.20.OR.ENGSI(DXUK,1).GT.0.20) THEN
         XPSUKL=0.0
         XPSOKL=0.0
         XPSUKR=0.0
         XPSOKR=0.0
         YPSUKL=0.0
         YPSOKL=0.0
         YPSUKR=0.0
         YPSOKR=0.0
      ELSE
         IF (YPSUKL.GT.0.0.AND.YPSUKR.GT.0.0) THEN
            XPSUK=0.5*(XPSUKL+XPSUKR)
            YPSUK=0.5*(YPSUKL+YPSUKR)
         ELSE IF (YPSUKL.GT.0.0.AND.YPSUKR.EQ.0.0) THEN
            XPSUK=XPSUKL
            YPSUK=YPSUKL
         ELSE IF (YPSUKL.EQ.0.0.AND.YPSUKR.GT.0.0) THEN
            XPSUK=XPSUKR
            YPSUK=YPSUKR
         END IF
         IF (YPSOKL.GT.0.0.AND.YPSOKR.GT.0.0) THEN
            XPSOK=0.5*(XPSOKL+XPSOKR)
            YPSOK=0.5*(YPSOKL+YPSOKR)
         ELSE IF (YPSOKL.GT.0.0.AND.YPSOKR.EQ.0.0) THEN
            XPSOK=XPSOKL
            YPSOK=YPSOKL
         ELSE IF (YPSOKL.EQ.0.0.AND.YPSOKR.GT.0.0) THEN
            XPSOK=XPSOKR
            YPSOK=YPSOKR
         END IF    
         XPSUK=ENGSI(XPSUK,1)
         YPSUK=ENGSI(YPSUK,1)
         XPSOK=ENGSI(XPSOK,1)
         YPSOK=ENGSI(YPSOK,1)
      END IF
C
c      IF (ITAB.GT.0) THEN
         WRITE(N6,'(A,F6.2)') '  XPSOKL = ',ENGSI(XPSOKL,1)
         WRITE(N6,'(A,F6.2)') '  YPSOKL = ',ENGSI(YPSOKL,1)
         WRITE(N6,'(A,F6.2)') '  XPSOKR = ',ENGSI(XPSOKR,1)
         WRITE(N6,'(A,F6.2)') '  YPSOKR = ',ENGSI(YPSOKR,1)
         WRITE(N6,'(A,F6.2)') '  XPSUKL = ',ENGSI(XPSUKL,1)
         WRITE(N6,'(A,F6.2)') '  YPSUKL = ',ENGSI(YPSUKL,1)
         WRITE(N6,'(A,F6.2)') '  XPSUKR = ',ENGSI(XPSUKR,1)
         WRITE(N6,'(A,F6.2)') '  YPSUKR = ',ENGSI(YPSUKR,1)
          WRITE(N6,'(A,F6.2)') '   XPSOK = ',XPSOK
          WRITE(N6,'(A,F6.2)') '   YPSOK = ',YPSOK
          WRITE(N6,'(A,F6.2)') '   XPSUK = ',XPSUK
          WRITE(N6,'(A,F6.2)') '   YPSUK = ',YPSUK
c      END IF
C
      XORG=X(1)
      DO J=1,NPTS
         X(J)=X(J)-XORG
      END DO
C
      IF (NPKS.GT.0) THEN
         do nk=1,npks
            xks(nk)=xks(nk)-XORG
         end do
      END IF
C
      IF (NPZS.GT.0) THEN
         do nk=1,npzs
            xzs(nk)=xzs(nk)-XORG
         end do
      END IF
C
      SUM1 = 0.
      SUM2 = 0.
C
      DO 3091 J=1,NPTS-1
      SUM1 = SUM1 + X(J)*Y(J+1)
      SUM2 = SUM2 + Y(J)*X(J+1)
 3091 CONTINUE
      SUM1 = SUM1 + X(NPTS)*Y(1)
      SUM2 = SUM2 + X(1)*Y(NPTS)
      ARMAX=0.5*(SUM1-SUM2)
c     WRITE(N6,5001) ENGSI(ARMAX,2)
 5001 FORMAT(1X,'ARMAX = ',F12.2)
C
      QGRENZ=0.0
      QGRENZV=0.0
      DQGRENZ=0.0
C
      HV=H
      AWRV=0.0
      WWRV=0.0
      RHYV=0.0
      RHYVV=0.0
      NWP=0
C
      ABW=0.0
      HGR=0.0
      H37=0.0
      I37=0
      HGRV=0.0
      CARHYV=0.0
      CARHY=0.0
      CGRENZV=0.0
      CGRENZ=0.0
      DCARHYV=0.0
      DCARHY=0.0
C
c     WRITE(N6,*) ' YPSUK = ',YPSUK
c     WRITE(N6,*) ' YPSOK = ',YPSOK
C
      ITYP37=ITYP
      IF (YPSUK.EQ.0.0.AND.YPSOK.EQ.0.0) THEN
        ITYP37=4
        IF (ITYP.EQ.8.OR.ITYP.EQ.9.OR.ITYP.EQ.18.OR.ITYP.EQ.19) THEN
          WRITE(37,*) 'Preissmann-Slot am Bauwerk prüfen!'
        END IF
      END IF
      WRITE(N6,*) 'ITYP37 = ',ITYP37
      WRITE(37,'(3I5)') LAB(KS),ITYP37,ITYP
C
      DO 3220 I=2,NSTP
C
      QGRDF(I)=0.0
      QGROF(I)=0.0
C
      H = H + DELY
CC    WRITE(N6,4900) I,ENGSI(H,1)
C4900 FORMAT(' I= ',I5,' H= ',F8.3)
C
C . . . . Bestimmung der Schnittpunkte mit Profil
C
      jp=0
      ts=0.
      wt=0.
      if(y(1).gt.h) then
        iblock=1
CC      WRITE(N6,'(T2,A)') 'IFBLOCK 1'
        jp=jp+1
        xp(jp)=x(1)
        yp(jp)=y(1)
      endif
      if(abs(y(1)-h).lt.0.01) then
        iblock=2
CC      WRITE(N6,'(T2,A)') 'IFBLOCK 2'
        jp=jp+1
        xp(jp)=x(1)
        yp(jp)=y(1)

        xl=xp(jp)
        yl=yp(jp)
         
      endif
c
      dxsum=0.0
c
      do 3100 j=2,npts-1

        if (npzs.gt.0) then
           xili = x(j-1)
           ximi = x(j)
           xire = x(j+1)
           zsli = 0.0
           zsmi = 0.0
           zsre = 0.0
           do nk=1,npzs-1
              xzsu=xzs(nk)
              xzso=xzs(nk+1)
              dxk=xzso-xzsu
              if (abs(dxk).lt.0.01) then
                 xzsu=xzsu-0.005
                 xzso=xzso+0.005
              end if
              yzsu=yzs(nk)
              yzso=yzs(nk+1)
              if (zsli.lt.1.and.xili.ge.xzsu.and.xili.le.xzso) then
                 zsli=yzsu+(xili-xzsu)*(yzso-yzsu)/(xzso-xzsu)
              end if
              if (zsmi.lt.1.and.ximi.ge.xzsu.and.ximi.le.xzso) then
                 zsmi=yzsu+(ximi-xzsu)*(yzso-yzsu)/(xzso-xzsu)
              end if
              if (zsre.lt.1.and.xire.ge.xzsu.and.xire.le.xzso) then
                 zsre=yzsu+(xire-xzsu)*(yzso-yzsu)/(xzso-xzsu)
              end if
           end do
        else
           zsli = 0.0
           zsmi = 0.0
           zsre = 0.0
        end if
c
c     WRITE(N6,'(T2,A,I2)') 'J = ',J
c
      if(y(j).le.h .and. y(j-1).gt.h) then
        iblock=3
CC      WRITE(N6,'(T2,A)') 'IFBLOCK 3'
        jp=jp+1
        dx=x(j)-x(j-1)
        dy=y(j)-y(j-1)
        dyy=0.0
        if (h.ge.zsmi) dyy=y(j)-h
        dxx=dx/dy*dyy
        xp(jp)=x(j)-dxx
        yp(jp)=h
c       Testgröße dxsum für Wasserspiegelbreite wird aufsummiert
        dxsum=dxsum+dx+dxx
c       benetzter Umfang wird aufsummiert
        wt=wt+sqrt(dxx*dxx+dyy*dyy)
c       benetzter Umfang der Unterteilung wird gespeichert
        WTS(J-1,I)=sqrt(dxx*dxx+dyy*dyy)
c       Fl„che der Unterteilung wird gespeichert
        daa=0.0
        if (h.ge.zsmi) daa=0.5*(h-y(j))*dxx
        AS(J-1,I)=daa
c       Conveyance berechnen
        xint=0.5*(x(j)+x(j-1))
c       WRITE(N6,'(A,I2,A,F6.2)') ' J = ',J,' XINT = ',ENGSI(XINT,1)
        if (npks.gt.1) then
           rksint=0.0
           do nk=1,npks-1
              xksu=xks(nk)
              xkso=xks(nk+1)
              dxk=xkso-xksu
              if (abs(dxk).lt.0.01) then
                 xksu=xksu-0.005
                 xkso=xkso+0.005
              end if
              rksu=rks(nk)
              rkso=rks(nk+1)
              if (rksint.lt.1.and.xint.ge.xksu.and.xint.le.xkso) then
                 rksint=rksu+(xint-xksu)*(rkso-rksu)/(xkso-xksu)
              end if
           end do
c          WRITE(N6,'(12X,A,F6.2)') '13795: RKSINT = ',RKSINT
           RHY=AS(J-1,I)/WTS(J-1,I)
           CONV(J-1,I)=1.486*RKSINT*AS(J-1,I)*RHY**(2./3.)
        else
           CONV(J-1,I)=0.0
        end if
c       xl und yl speichern

        xl=xp(jp)
        yl=yp(jp)
 
        if(y(j).le.h .and. y(j+1).gt.h) goto 3099
        goto 3100
      endif

      if(y(j).gt.h) then
        iblock=4
CC      WRITE(N6,'(T2,A)') 'IFBLOCK 4'
        jp=jp+1
        xp(jp)=x(j)
        yp(jp)=y(j)
        goto 3100
      endif

 3099 if(y(j).le.h .and. y(j+1).gt.h) then
        iblock=5
CC      WRITE(N6,'(T2,A)') 'IFBLOCK 5'
        jp=jp+1
        if(y(j-1).ge.h) goto 4001
c       zsdd=0.5*(zsli+zsmi)
        zsmin=zsli
        if (zsmi.lt.zsmin) zsmin=zsmi
        dx=0.0
        dy=0.0
c       if (h.ge.zsdd) then
        if (h.ge.zsmin) then
           dx=x(j)-x(j-1)
           dy=y(j)-y(j-1)
        end if
c       Testgröße dxsum für Wasserspiegelbreite wird aufsummiert
        dxsum=dxsum+dx
c       benetzter Umfang wird aufsummiert
        wt=wt+sqrt(dx*dx+dy*dy)
c       benetzter Umfang der Unterteilung wird gespeichert
        WTS(J-1,I)=sqrt(dx*dx+dy*dy)
c       Fl„che der Unterteilung wird gespeichert
c alt   daa=0.5*(h-y(j)+h-y(j-1))*dx
        daa=0.0
        if (h.ge.zsmi) daa=(h-y(j))*dx
        if (h.ge.zsli) daa=daa+(h-y(j-1))*dx
        daa=0.5*daa
        AS(J-1,I)=daa
c       Conveyance berechnen
        xint=0.5*(x(j)+x(j-1))
c       WRITE(N6,'(A,I2,A,F6.2)') ' J = ',J,' XINT = ',ENGSI(XINT,1)
        if (npks.gt.1) then
           rksint=0.0
           do nk=1,npks-1
              xksu=xks(nk)
              xkso=xks(nk+1)
              dxk=xkso-xksu
              if (abs(dxk).lt.0.01) then
                 xksu=xksu-0.005
                 xkso=xkso+0.005
              end if
              rksu=rks(nk)
              rkso=rks(nk+1)
              if (rksint.lt.1.and.xint.ge.xksu.and.xint.le.xkso) then
                 rksint=rksu+(xint-xksu)*(rkso-rksu)/(xkso-xksu)
              end if
           end do
c          WRITE(N6,'(12X,A,F6.2)') '13850: RKSINT = ',RKSINT
           RHY=AS(J-1,I)/WTS(J-1,I)
           CONV(J-1,I)=1.486*RKSINT*AS(J-1,I)*RHY**(2./3.)
        else
           CONV(J-1,I)=0.0
        end if
 4001   continue

c       zsdd=0.5*(zsmi+zsre)
        zsmin=zsre
        if (zsmi.lt.zsmin) zsmin=zsmi
        dx=0.0
c       if (h.ge.zsdd) dx=x(j+1)-x(j)
        if (h.ge.zsmin) dx=x(j+1)-x(j)
        dy=y(j+1)-y(j)
        dyy=0.0
        if (h.ge.zsmi) dyy=h-y(j)
        dxx=dx/dy*dyy
        xp(jp)=x(j)+dxx
        yp(jp)=h

        xr=xp(jp)
        yr=yp(jp)
         
c       Testgröße dxsum für Wasserspiegelbreite wird aufsummiert
        dxsum=dxsum+dxx
c       benetzter Umfang wird aufsummiert
        wt=wt+sqrt(dxx*dxx+dyy*dyy)
c       benetzter Umfang der Unterteilung wird gespeichert
        WTS(J,I)=sqrt(dxx*dxx+dyy*dyy)
c       Fl„che der Unterteilung wird gespeichert
c alt   daa=0.5*dyy*dxx
        daa=0.0
        if (h.ge.zsmi) daa=(h-y(j))*dxx
        daa=0.5*daa
        AS(J,I)=daa
c       Conveyance berechnen
        xint=0.5*(x(j+1)+x(j))
c       WRITE(N6,'(A,I2,A,F6.2)') ' J = ',J,' XINT = ',ENGSI(XINT,1)
        if (npks.gt.1) then
           rksint=0.0
           do nk=1,npks-1
              xksu=xks(nk)
              xkso=xks(nk+1)
              dxk=xkso-xksu
              if (abs(dxk).lt.0.01) then
                 xksu=xksu-0.005
                 xkso=xkso+0.005
              end if
              rksu=rks(nk)
              rkso=rks(nk+1)
              if (rksint.lt.1.and.xint.ge.xksu.and.xint.le.xkso) then
                 rksint=rksu+(xint-xksu)*(rkso-rksu)/(xkso-xksu)
              end if
           end do
c          WRITE(N6,'(12X,A,F6.2)') '13891: RKSINT = ',RKSINT
           RHY=AS(J,I)/WTS(J,I)
           CONV(J,I)=1.486*RKSINT*AS(J,I)*RHY**(2./3.)
        else
           CONV(J,I)=0.0
        end if

c       hier: Wasserspiegelbreite wird aufsummiert
         
        ts=ts+xr-xl
         
        goto 3100
      endif

      iblock=0
c     write(n6,4902)
c4902 format(2x,'Punkte liegen unterhalb h')
c     zsdd=0.5*(zsmi+zsli)
      zsmin=zsli
      if (zsmi.lt.zsmin) zsmin=zsmi
      dxx=0.0
      dyy=0.0
c     if (h.ge.zsdd) then
      if (h.ge.zsmin) then
         dxx=x(j)-x(j-1)
         dyy=y(j)-y(j-1)
      end if
c     Testgröße dxsum für Wasserspiegelbreite wird aufsummiert
      dxsum=dxsum+dxx
c     benetzter Umfang wird aufsummiert
      wt=wt+sqrt(dxx*dxx+dyy*dyy)
c     benetzter Umfang der Unterteilung wird gespeichert
      WTS(J-1,I)=sqrt(dxx*dxx+dyy*dyy)
c     Fl„che der Unterteilung wird gespeichert
c alt daa=0.5*(h-y(j)+h-y(j-1))*dxx
      daa=0.0
      if (h.ge.zsmi) daa=(h-y(j))*dxx
      if (h.ge.zsli) daa=daa+(h-y(j-1))*dxx
      daa=0.5*daa
      AS(J-1,I)=daa
c     Conveyance berechnen
      xint=0.5*(x(j)+x(j-1))
c     WRITE(N6,'(A,I2,A,F6.2)') ' J = ',J,' XINT = ',ENGSI(XINT,1)
      if (npks.gt.1) then
         rksint=0.0
         do nk=1,npks-1
            xksu=xks(nk)
            xkso=xks(nk+1)
            dxk=xkso-xksu
            if (abs(dxk).lt.0.01) then
               xksu=xksu-0.005
               xkso=xkso+0.005
            end if
            rksu=rks(nk)
            rkso=rks(nk+1)
            if (rksint.lt.1.and.xint.ge.xksu.and.xint.le.xkso) then
               rksint=rksu+(xint-xksu)*(rkso-rksu)/(xkso-xksu)
            end if
         end do
c        WRITE(N6,'(12X,A,F6.2)') '13933: RKSINT = ',RKSINT
         RHY=AS(J-1,I)/WTS(J-1,I)
         CONV(J-1,I)=1.486*RKSINT*AS(J-1,I)*RHY**(2./3.)
      else
         CONV(J-1,I)=0.0
      end if

 3100 continue

      if(abs(y(npts)-h).lt.0.01) then
         jp=jp+1
         xp(jp)=x(npts)
         yp(jp)=y(npts)
c
         if (npzs.gt.0) then
           xili = x(npts-1)
           ximi = x(npts)
           zsli = 0.0
           zsmi = 0.0
           do nk=1,npzs-1
              xzsu=xzs(nk)
              xzso=xzs(nk+1)
              dxk=xzso-xzsu
              if (abs(dxk).lt.0.01) then
                 xzsu=xzsu-0.005
                 xzso=xzso+0.005
              end if
              yzsu=yzs(nk)
              yzso=yzs(nk+1)
              if (zsli.lt.1.and.xili.ge.xzsu.and.xili.le.xzso) then
                 zsli=xzsu+(xili-xzsu)*(yzso-yzsu)/(xzso-xzsu)
              end if
              if (zsmi.lt.1.and.ximi.ge.xzsu.and.ximi.le.xzso) then
                 zsmi=xzsu+(ximi-xzsu)*(yzso-yzsu)/(xzso-xzsu)
              end if
           end do
         end if
c
         if (iblock.eq.0) then
            xp(jp-1)=x(npts-1)
            yp(jp-1)=y(npts-1)
         end if
c
c        zsdd=0.5*(zsmi+zsli)
         zsmin=zsli
         if (zsmi.lt.zsmin) zsmin=zsmi
         dxx=0.0
         dyy=0.0
c        if (h.ge.zsdd) then
         if (h.ge.zsmin) then
            dxx=xp(jp)-xp(jp-1)
            dyy=yp(jp)-yp(jp-1)
         end if
c
c        Fl„che der Unterteilung wird gespeichert
c alt    daa=0.5*(h-yp(jp)+h-yp(jp-1))*dxx
         daa=0.0
         if (h.ge.zsli) daa=(h-yp(jp-1))*dxx
         if (h.ge.zsmi) daa=daa+(h-yp(jp))*dxx 
         daa=0.5*daa
         AS(NPTS-1,I)=daa
c
         dwt=sqrt(dxx*dxx+dyy*dyy)
         if (I.GT.0.and.dwt.gt.(0.5*WSEC(KS,I-1))) THEN
c           WRITE(N6,'(T2,A,I5,A)')'KNOTEN ',LAB(KS),': WT SEHR GROSS!'
         else
c           Testgröße dxsum für Wasserspiegelbreite wird aufsummiert
            dxsum=dxsum+dxx
c           benetzter Umfang wird aufsummiert
            wt=wt+dwt
c           benetzter Umfang der Unterteilung wird gespeichert
            WTS(NPTS-1,I)=dwt
         end if

c        hier: Wasserspiegelbreite
          
         ts=xp(jp)-xl
 
 
c        Conveyance berechnen
         xint=0.5*(x(npts)+x(npts-1))
c        WRITE(N6,'(A,I2,A,F6.2)') ' J = ',J,' XINT = ',ENGSI(XINT,1)
         if (npks.gt.1) then
            rksint=0.0
            do nk=1,npks-1
               xksu=xks(nk)
               xkso=xks(nk+1)
               dxk=xkso-xksu
               if (abs(dxk).lt.0.01) then
                  xksu=xksu-0.005
                  xkso=xkso+0.005
               end if
               rksu=rks(nk)
               rkso=rks(nk+1)
               if (rksint.lt.1.and.xint.ge.xksu.and.xint.le.xkso) then
                  rksint=rksu+(xint-xksu)*(rkso-rksu)/(xkso-xksu)
               end if
            end do
c           WRITE(N6,'(12X,A,F6.2)') '13988: RKSINT = ',RKSINT
            RHY=AS(NPTS-1,I)/WTS(NPTS-1,I)
            CONV(NPTS-1,I)=1.486*RKSINT*AS(NPTS-1,I)*RHY**(2./3.)
         else
            CONV(NPTS-1,I)=0.0
         end if
      endif

      if (y(npts).gt.h) then
         jp=jp+1
         xp(jp)=x(npts)
         yp(jp)=y(npts)
      endif

      jpmax=jp

      if (jpmax.gt.300) then
      Write(N6,5002) jpmax,(engsi(xp(j),1),engsi(yp(j),1),j=1,jpmax)
 5002 format(' jpmax = ',i8,/,10(5x,4(2f8.2,8X)/)/)
      stop 'jpmax > 300'
      end if
c
C     Test mit DXSUMM im Vergleich zu TS
c
c     IF (TS.GT.DXSUM) THEN
      WRITE(N6,'(2(A,F9.2))')'DXSUM=',ENGSI(DXSUM,1),'  TS=',ENGSI(TS,1)
c     END IF
c
C
C     Flaechenberechnung nach Gauss-Elling eingefuegt:
c
      sum1=0.
      sum2=0.
      avor=area

      do 3215 j=1,jpmax-1
         sum1=sum1+xp(j)*yp(j+1)
         sum2=sum2+yp(j)*xp(j+1)
 3215 continue
      if (jpmax.gt.300) then
         print '(t2,A,I5)',' jpmax = ',jpmax
      stop 'jpmax > 300'
      end if
      sum1=sum1+xp(jpmax)*yp(1)
      sum2=sum2+yp(jpmax)*xp(1)
      area=0.5*(sum1-sum2)

      ASEC(KS,I) = ARMAX-AREA
      TSEC(KS,I) = TS
      WSEC(KS,I) = WT

      ASSUM=0.0
      WTSUM=0.0
      COSUM=0.0

      DO JS=1,NPTS
         ASSUM=ASSUM+ENGSI(AS(JS,I),2)
         WTSUM=WTSUM+ENGSI(WTS(JS,I),1)
         COSUM=COSUM+ENGSI(CONV(JS,I),3)
      END DO

C     ASEC wird auf ASSUM aus neuer Berechnugsmethode gesetzt
      ASEC(KS,I)=SIENG(ASSUM,2)

      IF (NPKS.GT.0) THEN
         CH2SEC(KS,I) = SIENG(COSUM,3)
      ELSE
         CH2SEC(KS,I) = 0.0
      END IF

      HWR = ENGSI (H,1)
      AWR = ENGSI (ASEC(KS,I),2)
      TWR = ENGSI (TSEC(KS,I),1)
      WWR = ENGSI (WSEC(KS,I),1)

      IF (I.GT.2) THEN
         HWRVOR1=HWRVOR
         HWRVOR=HWR
         DIFTWR=TWR-TWRVOR
c        WRITE(N6,*) 'TWRVOR1 = ',TWRVOR1
c        WRITE(N6,*) ' TWRVOR = ',TWRVOR
c        WRITE(N6,*) '    TWR = ',TWR
c        WRITE(N6,*) ' DIFTWR = ',DIFTWR
         IF (DIFTWR.LT.0.0.AND.TWR.LT.0.11) DIFTWR=0.0
         TWRVOR1=TWRVOR
         TWRVOR=TWR
c        bei Bauwerken wie in HYDRET95 keine Korrektur von RHY und WWR
         IF (ITYP.EQ.8.OR.ITYP.EQ.9) DIFTWR=-1.0
c        WRITE(N6,*) ' DIFTWR = ',DIFTWR
      END IF


      IF (I.GT.1) THEN
         RHY = AWR/WWR
         GEO = RHY**(2./3.)
         IF (RHY.LT.RHYV) THEN
            WRITE(N6,*) '12774:  RHY     = ',RHY,'  RHYV = ',RHYV
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(N6,'(A)') 'XSF: PROBLEM: HYDR. RADIUS WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSF: PROBLEM: HYDR. RADIUS WIRD KLEINER!'
         END IF
         IF (AWR.LT.AWRV) THEN
            WRITE(N6,*) 'AWR = ',AWR,'  AWRV = ',AWRV
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(N6,'(A)') 'XSF: PROFILFEHLER: FLŽCHE WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSF: PROFILFEHLER: FLŽCHE WIRD KLEINER!'
         END IF
         IF (WWR.LT.WWRV) THEN
            WRITE(N6,*) 'WWR = ',WWR,'  WWRV = ',WWRV
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(N6,'(A)') 'XSF: PROFILFEHLER: UMFANG WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSF: PROFILFEHLER: UMFANG WIRD KLEINER!'
         END IF

         RHYALT=RHY
         WWRALT=WWR

         CARHY = AWR*GEO
         IF (ITYP.LT.8.OR.ITYP.GT.10) THEN
           IF (CARHY.LT.CARHYV) THEN
             WRITE(N6,*) '12804:  RHY alt = ',RHY,'    WWR alt = ',WWR
             GEO = CARHYV/AWR
             RHY = GEO**(3./2.)
             WWR = AWR/RHY
             WSEC(KS,I) = SIENG(WWR,1)
             CARHY=CARHYV
             WRITE(N6,*) '12810:  RHY neu = ',RHY,'    WWR neu = ',WWR
           END IF
         END IF

c         IF (RHY.LT.RHYV) THEN
c            RHY=0.5*(RHY+RHYV)
c            RHY=RHYV
c            WWR = AWR/RHY
c            WSEC(KS,I) = SIENG(WWR,1)
c            GEO = RHY**(2./3.)
c            CARHY = AWR*GEO
c            WRITE(N6,*) 'RHY neu = ',RHY,'    WWR neu = ',WWR
c         END IF

         AWRV=AWR
         WWRV=WWR
      ELSE
         GEO = 0.0
      END IF

      IF (TWR.GT.WWR) THEN
         TWR=WWR
         TSEC(KS,I) = SIENG(TWR,1)
      END IF
       
      IF (NWP.EQ.0) THEN
C        1. FORMALER RHY-WENDEPUNKT = WASSERSPIEGEL
         NWP=NWP+1
         NNWP(KS)=NWP
         RNFAK(KS,NWP)=1.0
         HWP(KS,NWP)=HZERO
         IWP(KS,NWP)=I
      END IF
C
      IF (I.GT.2) THEN
         DTWDH=(TWR-TWRVOR1)/(HWR-HWRVOR1)
      ELSE
         DTWDH=0
      END IF
C
      WRITE(60,3209) XLKM,HWR,AWR,TWR,DTWDH,WWR,RHY,LAB(KS)
 3209 FORMAT(7F10.3,I10)
C
C     
C     Berechnung der potentiellen Querschnittsleistungen
C     QPOTW nach Wehranalogie, QPOTS nach Schützanalogie (TYP 8 und 9)
C     Wehr-Beiwert DMUEW = 0.57755775 (Querriegel i.d.R. breitkronig)
C     Schütz-Beiwert DMUES = 0.60 
C
      DMUEW=0.57755775
      DMUES=0.60

C     YBR=Sohllage, HS=gesamte Wassertiefe
C     ABW=unterer Querschnittsanteil (z.B. Durchlass), am Anfang = 0.0
      YBR=ENGSI(ZBOT,1)
      HS=HWR-YBR

C     Querschnittsleistung nach Schützanalogie (Toricelli)
      IF ((MODUS.EQ.'DF'.OR.MODUS.EQ.'OF').AND.AWRDF(I).GT.0.01) THEN
C        MODUS='DF' ==> nur Durchlass bzw. Verdolung wird betrachet!
C        MODUS='OF' ==> nur Overflow von Durchlass/Verdolung betrachet!
         QPOTS=DMUES*AWRDF(I)*SQRT(2.0*9.81*HS)
      ELSE
         IF (ABW.LT.0.01) THEN
            QPOTS=DMUES*AWR*SQRT(2.0*9.81*HS)
         ELSE
            QPOTS=DMUES*ABW*SQRT(2.0*9.81*HS)
         END IF
      END IF
      HGRV=HGR
      IF (QGRENZ.GT.QGRENZV) QGRENZV=QGRENZ


C     Querschnittsleistung nach Wehranalogie
c     WRITE(N6,*) 'ABW =',ABW
      IF (ABW.LT.0.01) THEN
C        Bauwerksquerschnitt noch nicht erkannt
         HGR=AWR/TWR
         H37=HGR
         HUE=1.5*HGR
         QPOTW=DMUEW*AWR*SQRT(2.0*9.81*HUE)
         FRGRENZ=(QPOTW/AWR)/SQRT(9.81*HGR)
c        WRITE(N6,*) 'ITYP =',ITYP
         IF (ITYP.EQ.8.OR.ITYP.EQ.9.OR.ITYP.EQ.18.OR.ITYP.EQ.19) THEN
            AWRDF(I)=AWR
            TWRDF(I)=TWR
            WWRDF(I)=WWR
            RHYDF=AWRDF(I)/WWRDF(I)
c           WRITE(N6,*) 'HGR =',HGR,' 4*HS =',(4*HS),' DIFTWR =',DIFTWR
            IF (HGR.GT.(4*HS).AND.ABS(DIFTWR).LT.0.002) THEN
               WRITE(N6,*) 'UP XSF Line 10955:'
               WRITE(N6,*) ' HGR = ',HGR,'  4*HS = ',(4*HS)
               ABW=AWR
               QPOTW=QPOTS
               FRGRENZ=1.0
               H37=HS
            ELSE IF (HGR.LT.HGRV) THEN
               WRITE(N6,*) ' HGR = ',HGR,'  HGRV = ',HGRV
               WRITE(N6,*) 'UP XSF Line 10962: HGR < HGRV !'
               ABW=AWR
               QPOTW=QPOTS
               FRGRENZ=1.0
               H37=HS
            END IF
c           Tests mit YPSUK (Unkterkante Preissmann-Slot) ob zulässig
            YU=YPSUK-WDEL*0.5-0.001
            YO=YPSUK+WDEL*0.5+0.001
            IF (ABW.GT.0.01) THEN
               IF (HWR.LE.YU.OR.HWR.GE.YO) THEN
                  WRITE(N6,*) 'UP XSF Line 10972: außerhalb PSlot!'
                  WRITE(N6,*) ' YO = ',YO
                  WRITE(N6,*) 'HWR = ',HWR
                  WRITE(N6,*) ' YU = ',YU
                  ABW=0.0
                  QPOTW=DMUEW*AWR*SQRT(2.0*9.81*HUE)
                  FRGRENZ=(QPOTW/AWR)/SQRT(9.81*HGR)
                  H37=HGR
               END IF
            ELSE
               IF (HWR.GE.YU.AND.HWR.LE.YO) THEN
                  WRITE(N6,*) 'UP XSF Line 10979: innerhalb PSlot!'
                  ABW=AWR
                  QPOTW=QPOTS
                  FRGRENZ=1.0
                  H37=HS
               END IF
            END IF
            IF (ABW.GT.0.01) THEN
C              Bauwerksquerschnitt erstmalig erkannt
               DBW=SQRT(4.0*ABW/3.14)
               WRITE(N6,*) 'UP XSF Line 10855:'
               WRITE(N6,*) ' ABW = ',ABW,'  DBW = ',DBW
               GEODF=RHYDF**(2./3.)
               CARHYDF=AWRDF(I)*GEODF
C              Bauwerksdecke speichern
               IF (MODUS.EQ.'DF'.OR.MODUS.EQ.'OF') ZDF(NAC)=SIENG(HWR,1)
            END IF
            AWROF(I)=0.0
            TWROF(I)=0.0
            WWROF(I)=0.0
            RHYOF=0.0
            CARHYOF=0.0
         END IF
      ELSE
C        Bauwerksquerschnitt erkannt QPOTW für Überströmung
         HGR=(AWR-ABW)/TWR
         H37=AWR/TWR
         HUE=1.5*HGR
c        IF (HGR.GT.HS) STOP 'HGR > HS!'
         IF (HGR.LE.0.0) THEN
            WRITE (25,*) ' AWR = ', AWR
            WRITE (25,*) ' ABW = ', ABW
            WRITE (25,*) ' TWR = ', TWR
c           STOP 'HGR < 0.0!'
            HGR=0.0
            QPOTW=0.0
            FRGRENZ=0.0
         ELSE
            QPOTW=DMUEW*(AWR-ABW)*SQRT(2.0*9.81*HUE)
            QGROF(I)=QPOTW
            FRGRENZ=(QPOTW/(AWR-ABW))/SQRT(9.81*HGR)
         END IF
         QPOTW=QPOTW+QPOTS
         AWRDF(I)=AWRDF(I-1)
         TWRDF(I)=TWRDF(I-1)
         WWRDF(I)=WWRDF(I-1)
         AWROF(I)=AWR-AWRDF(I)
         TWROF(I)=TWR
         WWROF(I)=WWR-WWRDF(I)
         RHYOF=AWROF(I)/WWROF(I)
         GEOOF=RHYOF**(2./3.)
         CARHYOF=AWROF(I)*GEOOF
      END IF

C     Querschnittsleistung nach Schützanalogie (Toricelli) nochmals
      IF ((MODUS.EQ.'DF'.OR.MODUS.EQ.'OF').AND.AWRDF(I).GT.0.01) THEN
C        MODUS='DF' ==> nur Durchlass bzw. Verdolung wird betrachet!
C        MODUS='OF' ==> nur Overflow von Durchlass/Verdolung betrachet!
         QPOTS=DMUES*AWRDF(I)*SQRT(2.0*9.81*HS)
      ELSE
         IF (ABW.LT.0.01) THEN
            QPOTS=DMUES*AWR*SQRT(2.0*9.81*HS)
         ELSE
            QPOTS=DMUES*ABW*SQRT(2.0*9.81*HS)
         END IF
      END IF
      HGRV=HGR
      IF (QGRENZ.GT.QGRENZV) QGRENZV=QGRENZ

      IF (QPOTW.LE.QGRENZV) THEN
         QGREXT=QGRENZV+DQGRENZ
         QPOTW1=0.5*(QGREXT+QPOTW) 
         IF (QPOTW1.LE.QGRENZV) QPOTW1=(QGREXT+QGREXT+QPOTW)/3.
         IF (QPOTW1.LE.QGRENZV) QPOTW1=(QGREXT+QGREXT+QGREXT+QPOTW)/4.
         IF (QPOTW1.LE.QGRENZV) QPOTW1=QGRENZV
      ELSE
         DQGRENZ=QPOTW-QGRENZV
         QPOTW1=QPOTW
      END IF

      IF (QPOTW1.LT.QGRENZV) THEN
         WRITE(N6,*) 'QGRENZV = ',QGRENZV,'  QPOTW1 = ',QPOTW1
         WRITE(N6,*) 'UP XSF: QPOTW1 < QGRENZV!'
      END IF

      CGRENZV=CGRENZ
      IF ((MODUS.EQ.'DF'.OR.MODUS.EQ.'OF').AND.ABW.GT.0.01) THEN
c        WRITE(N6,*) 'UP XSF: Line 10911: QGRENZ aus QPOTS'
C        MODUS='DF' ==> nur Durchlass bzw. Verdolung wird betrachet!
C        MODUS='OF' ==> nur Overflow von Verdolung wird betrachet!
         QGRDF(I)=QPOTS
         QGRENZ=QGRDF(I)
         CGRENZ=QGRENZ/(AWRDF(I)*RHYDF**(2./3.))
         CGRENZDF=QGRDF(I)/(AWRDF(I)*RHYDF**(2./3.))
         CGRENZOF=QGROF(I)/(AWROF(I)*RHYOF**(2./3.))
      ELSE
c        WRITE(N6,*) 'UP XSF: Line 10920: QGRENZ aus QPOTW1'
         QGRENZ=QPOTW1
         CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
         QGRDF(I)=QPOTW1
         CGRENZDF=CGRENZ
         CGRENZOF=0.0
      END IF

      DCARHY=CARHY-CARHYV

      IF (DIFTWR.GE.0.0) THEN
        IF (CARHY.LE.CARHYV.AND.CGRENZ.GT.CGRENZV) THEN
c         WRITE(N6,*) 'CGRENZV=',CGRENZV,' CGRENZ=',CGRENZ
c         WRITE(N6,*) ' CARHYV=',CARHYV, '  CARHY=',CARHY
          CARHY=QGRENZ/CGRENZV
          GEO = CARHY/AWR
          RHY = GEO**(3./2.)
          WWR = AWR/RHY
          WRITE(N6,*) '13043:  WWR korrigiert = ',WWR
          WSEC(KS,I) = SIENG(WWR,1)
          IF (TWR.GT.WWR) THEN
            TWR=WWR
            WRITE(N6,*) '13047:  TWR korrigiert = ',TWR
            TSEC(KS,I) = SIENG(TWR,1)
          END IF
          CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
          TWROF(I)=TWR
          WWROF(I)=WWR-WWRDF(I)
        ELSE IF (DCARHY.LT.DCARHYV) THEN
          CARHY=0.5*(CARHYV+DCARHYV+CARHY)
          GEO = CARHY/AWR
          RHY = GEO**(3./2.)
          WWR = AWR/RHY
c         WRITE(N6,*) '13058: WWR korrigiert = ',WWR
          WSEC(KS,I) = SIENG(WWR,1)
          IF (TWR.GT.WWR) THEN
            TWR=WWR
c           WRITE(N6,*) '13062: TWR korrigiert = ',TWR
            TSEC(KS,I) = SIENG(TWR,1)
          END IF
          CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
          TWROF(I)=TWR
          WWROF(I)=WWR-WWRDF(I)
        END IF
      END IF

      IF (WWROF(I).LT.0.0) THEN
         WWROF(I)=0.0
         WWROF(I-1)=0.0
      END IF

      DCARHYV=CARHY-CARHYV
      CARHYV=CARHY

      QGRSEC(KS,I)=SIENG(QGRENZ,3)

c     IF (ABW.GT.0.01) THEN
c        WRITE(N6,*) 'H37 =',H37,' HS =',HS,' RHY =',RHY
c     END IF

      IF (ABW.GT.0.01.AND.HWR.GT.YPSOK.AND.I37.EQ.0) THEN
         XLM=XLKM*1000.
         I37=1
         WRITE(37,'(I5,F10.1,F8.2,I2,F8.2)') LAB(KS),XLM,HWR,I37,YPSOK
         AWROF(I-1)=0.0
         WWROF(I-1)=0.0
         TWROF(I-1)=0.0
         QGROF(I-1)=0.0
      ELSE IF (ABW.GT.0.01.AND.H37.GE.HS.AND.I37.EQ.0) THEN
         XLM=XLKM*1000.
         WRITE(37,'(I5,F10.1,F8.2,I2,F8.2)') LAB(KS),XLM,HWR,I37,YPSUK
         AWROF(I-1)=0.0
         WWROF(I-1)=0.0
         TWROF(I-1)=0.0
         QGROF(I-1)=0.0
         AWROF0=AWROF(I)
         WWROF0=WWROF(I)
         AWROF(I)=0.0
         WWROF(I)=0.0
         TWROF(I)=0.0
         QGROF(I)=0.0
         IF (MODUS.EQ.'OF') ZOF(NAC)=SIENG(HWR,1)
      ELSE IF (ABW.GT.0.01.AND.RHY.LT.RHYV.AND.I37.EQ.0) THEN
        IF (HWR.GT.YPSOK) THEN
          XLM=XLKM*1000.
          I37=1
          WRITE(37,'(I5,F10.1,F8.2,I2,F8.2)') LAB(KS),XLM,HWR,I37,YPSOK
          AWROF(I-1)=0.0
          WWROF(I-1)=0.0
          TWROF(I-1)=0.0
          QGROF(I-1)=0.0
        END IF
      ELSE IF (ABW.LT.0.01) THEN
         AWROF(I)=0.0
         WWROF(I)=0.0
         TWROF(I)=0.0
         QGROF(I)=0.0
      END IF

      AWROF(I)=AWROF(I)-AWROF0
      IF (AWROF(I).LT.0.10) AWROF(I)=0.10
      WWROF(I)=WWROF(I)-WWROF0
      IF (WWROF(I).LT.TWROF(I)) WWROF(I)=TWROF(I)
      IF (TWROF(I).GT.0.05) THEN
         HGR=AWROF(I)/TWROF(I)
         HUE=1.5*HGR
         QGROF(I)=DMUEW*AWROF(I)*SQRT(2.0*9.81*HUE)
      ELSE
         QGROF(I)=0.001
         TWROF(I)=0.05
      END IF
      IF (WWROF(I).GT.0.0) THEN
         RHYOF=AWROF(I)/WWROF(I)
         GEOOF=RHYOF**(2./3.)
         CGRENZOF=QGROF(I)/(AWROF(I)*GEOOF)
         CARHYOF=AWROF(I)*GEOOF
      ELSE
         RHYOF=0.01
         CGRENZOF=0.0
         CARHYOF=0.0
         WWROF(I)=AWROF(I)/RHYOF
      END IF


      IF (MODUS.EQ.'DF'.AND.ABW.GT.0.01) THEN
C        MODUS='DF' ==> nur Durchlass bzw. Verdolung wird betrachet!
C        Querschnittsparameter bleiben konstant

         ASEC(KS,I)   = SIENG(AWRDF(I),2)
         WSEC(KS,I)   = SIENG(WWRDF(I),1)
         TSEC(KS,I)   = SIENG(TWRDF(I),1)
         QGRSEC(KS,I) = SIENG(QGRDF(I),3)
          
         WRITE(61,6161) LAB(KS),XLM,HWR,AWR,TWR,WWR,RHY,CARHYDF,QGRDF(I)
          
      ELSE IF (MODUS.EQ.'OF') THEN
C        MODUS='OF' ==> nur der Overflow wird betrachet!

         ASEC(KS,I)   = SIENG(AWROF(I),2)
         WSEC(KS,I)   = SIENG(WWROF(I),1)
         TSEC(KS,I)   = SIENG(TWROF(I),1)
         QGRSEC(KS,I) = SIENG(QGROF(I),3)
          
         WRITE(61,6161) LAB(KS),XLM,HWR,AWR,TWR,WWR,RHY,CARHYOF,QGROF(I)
          
      ELSE
          
         QGRSEC(KS,I)=SIENG(QGRENZ,3)
          
         WRITE(61,6161) LAB(KS),XLM,HWR,AWR,TWR,WWR,RHY,CARHY,QGRENZ
 6161    FORMAT(I5,F10.2,5F10.2,2F12.3)
          
      END IF

      IF (ITAB.GT.0) THEN
         IF (MODUS.EQ.'DF') THEN
            WRITE(N6,3210) LAB(KS),I,HWR,AWRDF(I),TWRDF(I),WWRDF(I),
     &                     RHYDF,QGRDF(I),CGRENZ,CARHYDF
         ELSE IF (MODUS.EQ.'OF') THEN
            WRITE(N6,3210) LAB(KS),I,HWR,AWROF(I),TWROF(I),WWROF(I),
     &                     RHYOF,QGROF(I),CGRENZOF,CARHYOF
         ELSE
            WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY,QGRENZ,CGRENZ,
     &                     CARHY,ASSUM,WTSUM,COSUM
c    &                     CARHY,ASSUM,WTSUM,COSUM,
c    &                     ((ENGSI(AS(JS,I),2)),JS=1,NPTS),
c    &                     ((ENGSI(WTS(JS,I),1)),JS=1,NPTS)
         END IF
 3210       FORMAT(1X,2I9,311F12.3)
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c3210       FORMAT(1X,2I9,6F12.3)
      END IF
      IF (RHYV.GE.RHYVV) THEN
C        RHY STEIGEND
         IF (RHY.LT.RHYV.AND.HV.GE.HZERO) THEN
C           ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
            NWP=NWP+1
            NNWP(KS)=NWP
            IF (NWP.EQ.2) GEO1=GEOV
            RNFAK(KS,NWP)=GEOV/GEO1
            HWP(KS,NWP)=HV
            IWP(KS,NWP)=I-1
C           WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
         END IF
      ELSE IF (RHYV.LT.RHYVV) THEN
C        RHY FALLEND
         IF (RHY.GT.RHYV.AND.HV.GE.HZERO) THEN
C           ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
            NWP=NWP+1
            NNWP(KS)=NWP
            IF (NWP.EQ.2) GEO1=GEOV
            RNFAK(KS,NWP)=GEOV/GEO1
            HWP(KS,NWP)=HV
            IWP(KS,NWP)=I-1
C           WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
 3213 FORMAT(I5,'. RHY-WENDEP.: ',F8.3,' I = ',I2,' RNFAK = ',F8.3)
         END IF
      END IF
      IF (I.EQ.NSTP) THEN
C        LETZTER WERT IN PROFILTABELLE
         NWP=NWP+1
         NNWP(KS)=NWP
         IF (NWP.EQ.2) GEO1=GEO
         RNFAK(KS,NWP)=GEO/GEO1
         HWP(KS,NWP)=H
         IWP(KS,NWP)=I
C        WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
      END IF
      RHYVV=RHYV
      RHYV=RHY
      GEOV=GEO
      HV=H

      IF (ASEC(KS,I).LE.0.) THEN
         HWSP=ENGSI(H,1)
         WRITE(N6,3211) LAB(KS), HWSP, ENGSI(ASEC(KS,I),2)
 3211    FORMAT(1x,' UP XSF KNOTEN ',I5,' WSP= ',F7.2,' A= ',F7.2)
         STOP 3211
      ENDIF
      IF(TSEC(KS,I).LT.0.) THEN
      HWSP=ENGSI(H,1)
c     HWSP=H+ZBOT
c     HWSP=ENGSI(HWSP,1)
      WRITE(N6,3212) LAB(KS), HWSP, ENGSI(TSEC(KS,I),1)
 3212 FORMAT(1x,'UP XSF FEHLER AN KNOTEN ',I5,' WSP= ',F7.2,' T= ',F7.2)
c     STOP 3212
      ENDIF

 3220 CONTINUE
c
      IF (MODUS.EQ.'OF') THEN
         WRITE (N6,*) ' ZOF(NAC) = ',ENGSI(ZOF(NAC),1)
      END IF
c

      return
      end

C-----------------------------------------------------------------------
      SUBROUTINE XSK(NSEC,X,Y,NPTS,DELY,ITAB,NSTP,KS,ZBOT,NBOT,XLKM)
C**********************************************************************
C     X S K
C**********************************************************************
C
C     BERECHNUNG DER QUERSCHNITTSPARAMETER AUSGEHEND VOM KERNQUERSCHNITT
C
C     Voraussetzung: im Programm XSECT  MODUS = 'RK' oder 'VK'
C
C**********************************************************************

      INCLUDE 'DIMENS.INC'
C
      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /GEOWP/        HWP(IPSEC,IPSTP),IWP(IPSEC,IPSTP),
     1                      RNFAK(IPSEC,IPSTP),NNWP(IPSEC)
      COMMON /QDD/          QZERO(IPSEC) , DZERO(IPSEC)
C
      DIMENSION      X(IPCOO),Y(IPCOO)
C
C     SUCHEN DES WASSERSTANDES HZERO
C
      DO 3088 I=1,IPSEC
         IF (LAB(KS).EQ.IAB(I)) THEN
c           WRITE(*,'(A,F8.3)') '    ZO = ', ENGSI(ZO(I),1)
c           WRITE(*,'(A,F8.3)') ' DZERO = ', ENGSI(DZERO(I),1)
c           WRITE(*,'(A,F8.3)') ' HZERO = ', ENGSI(HZERO,1)
            HZERO=ZO(I)+DZERO(I)
            ITYP=ITYPE(I)
         END IF
 3088 CONTINUE
C
C     ANFANGSWERTE
C
      XLM=XLKM*1000.
      ASEC(KS,1) = 0.0
      TSEC(KS,1) = 0.0
      WSEC(KS,1) = 0.0
      AT = 0.0
      WT = 0.0
      NR = NBOT + 1
      NL = NBOT - 1
      YL = Y(NBOT)
      YR = YL
      XM = X(NBOT)
      XR = XM
      XL = XR
      H = ZBOT
      WDEL = ENGSI(DELY,1)
      IF (ITAB.GT.0) THEN
      WRITE(N6,3089) LAB(KS),WDEL
 3089    FORMAT(///5X,'QUERSCHNITT',I5,'    H-SCHRITTWEITE',F10.3,' M'//
     &   5X,' LFD.NR   KNOTEN      H      FLAECHE   WSP.BREITE ',
     &   ' BEN.UMFANG  HYDR.RADIUS      QGRENZ      CGRENZ   ',
     &   '    CARHY       ASSUM       WTSUM       COSUM   '/)
      END IF
C
C
      QGRENZ=0.0
      QGRENZV=0.0
      DQGRENZ=0.0
C
      HV=H
      AWRV=0.0
      WWRV=0.0
      RHYV=0.0
      RHYVV=0.0
      NWP=0
C
      ABW=0.0
      HGR=0.0
      H37=0.0
      I37=0
      HGRV=0.0
      CARHYV=0.0
      CARHY=0.0
C
      CGRENZV=0.0
      CGRENZ=0.0
      DCARHYV=0.0
      DCARHY=0.0
C
      WRITE(37,'(2I5)') LAB(KS),ITYP
C
      DO 3220 I=2,NSTP
      H = H + DELY
C
C . . . . RIGHT SIDE
C
 3100 DELTAYR=Y(NR)-H
c     IF (ITAB.GT.0) THEN
c        WRITE(N6,3215) NR,ENGSI(XR,1),ENGSI(DELTAYR,1)
c3215    FORMAT(1x,' NR= ',I5,' XR= ',F7.2,' DELTAYR= ',F7.3)
c     END IF
      IF (DELTAYR+0.001)  3120,3120,3140
 3120 DY = Y(NR) - YR
      DX = X(NR) - XR
      AT = AT + ( XR - XM ) * DY + 0.5 * DX * DY
      WT = WT + SQRT ( DX * DX + DY * DY )
      YR = Y(NR)
      XR = X(NR)
      NR = NR + 1
      IF ( NR .GT. NPTS )  GO TO 3160
      GO TO 3100
 3140 DY = H - YR
      DX = ( X(NR) - XR ) * ( DY / ( Y(NR) - YR ) )
      AT = AT + ( XR - XM ) * DY + 0.5 * DX * DY
      WT = WT + SQRT ( DX * DX + DY * DY )
      YR = YR + DY
      XR = XR + DX
C
C . . . . LEFT SIDE
C
 3160 DELTAYL=Y(NL)-H
c     IF (ITAB.GT.0) THEN
c        WRITE(N6,3216) NL,ENGSI(XL,1),ENGSI(DELTAYL,1)
c3216    FORMAT(1x,' NL= ',I5,' XL= ',F7.2,' DELTAYL= ',F7.3)
c     END IF
      IF (DELTAYL+.001 )  3180,3200,3200
 3180 DY = Y(NL) - YL
      DX = XL - X(NL)
      ATB=AT
      AT = AT + ( XM - XL ) * DY + 0.5 * DX * DY
      WT = WT + SQRT ( DX * DX + DY * DY )
      IF ( NL .LT. 1 )  GO TO 3220
      IF ( NL .EQ. 1 .AND. DELTAYL.LE.DY) GOTO 3200
      YL = Y(NL)
      XL = X(NL)
      NL = NL - 1
      GO TO 3160
 3200 DY = H - YL
C
      DYYL = Y(NL) - YL
      IF (DYYL.EQ.0.0) THEN
         PRINT '(T2,A,I4)',' NSEC = ',NSEC
         PRINT '(T2,A,I4)','   NL = ',NL
         PRINT '(T2,A,F8.3)','Y(NL) = ',ENGSI(Y(NL),1)
         PRINT '(T2,A,F8.3)',' YL = ',ENGSI(YL,1)
         PRINT '(T2,A,F8.3)','  H = ',ENGSI(H,1)
         STOP 'DYYL = 0.0 !'
      END IF
      DX = ( XL - X(NL) ) * ( DY / ( Y(NL) - YL ) )
      ATB=AT
      AT = AT + ( XM - XL ) * DY + 0.5 * DX * DY
      IF (AT.LT.0.0) THEN
         WRITE(N6,3201) ENGSI(H,1),ENGSI(XM,1),ENGSI(XL,1),
     1   ENGSI(DX,1), ENGSI(DY,1)
 3201    FORMAT(1x,' H= ',F7.2,' XM= ',F7.2,' XL= ',F7.2,' DX= ',
     1   F7.2,' DY= ',F7.2)
         PRINT '(T2,2(A,F7.2))','ATB=',ENGSI(ATB,1),'  AT=',ENGSI(AT,2)
         STOP 3201
      ENDIF
      WT = WT + SQRT ( DX * DX + DY * DY )
      YL = YL + DY
      XL = XL - DX
c     Zeile 232 am 5.6.91 eingefuegt
      IF (AT.LT.ASEC(KS,I-1)) AT=ASEC(KS,I-1)
      ASEC(KS,I) = AT
      TSEC(KS,I) = XR - XL
C     am 22.07.95 abgeschaltet
C     IF(TSEC(KS,I).LE.TSEC(KS,I-1)) TSEC(KS,I)=TSEC(KS,I-1)
c
      WSEC(KS,I) = WT

      HWR = ENGSI ( H,1)
      AWR = ENGSI ( ASEC(KS,I),2)
      TWR = ENGSI ( TSEC(KS,I),1)
      WWR = ENGSI ( WSEC(KS,I),1)
      RHY = AWR/WWR

      IF (I.GT.2) THEN
         HWRVOR=HWR
         DIFTWR=TWR-TWRVOR
         IF (DIFTWR.LT.0.0.AND.TWR.LT.0.11) DIFTWR=0.0
c        bei Bauwerken wie in HYDRET95 keine Korrektur von RHY und WWR
         IF (ITYP.EQ.8.OR.ITYP.EQ.9) DIFTWR=-1.0
c        WRITE(N6,*) ' DIFTWR = ',DIFTWR
         TWRVOR=TWR
         RHYVOR=RHY
      END IF

      IF (I.GT.2.AND.RHY.LT.RHYVOR) THEN
c        GLAETTUNG DER PARAMETER ZUR STABILISIERUNG 01.09.2000
c        ASEC(KS,I)=(ASEC(KS,I)+ASEC(KS,I-1))/2.
c        TSEC(KS,I)=(TSEC(KS,I)+TSEC(KS,I-1))/2.
c        WSEC(KS,I)=(WSEC(KS,I)+WSEC(KS,I-1))/2.
         ASEC(KS,I)=SQRT(ASEC(KS,I)*ASEC(KS,I-1))
         TSEC(KS,I)=SQRT(TSEC(KS,I)*TSEC(KS,I-1))
         WSEC(KS,I)=SQRT(WSEC(KS,I)*WSEC(KS,I-1))
         AWR = ENGSI ( ASEC(KS,I),2)
         TWR = ENGSI ( TSEC(KS,I),1)
         WWR = ENGSI ( WSEC(KS,I),1)
         RHY = AWR/WWR
      END IF
      IF (I.GT.1) THEN
         RHY = AWR/WWR
         GEO = RHY**(2./3.)
         IF (RHY.LT.RHYV) THEN
c           WRITE(N6,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KSI),I,HWR,AWR,TWR,WWR,RHY
            WRITE(N6,'(A)') 'XSK: PROBLEM: HYDR. RADIUS WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSK: PROBLEM: HYDR. RADIUS WIRD KLEINER!'
         END IF
         IF (AWR.LT.AWRV) THEN
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
            WRITE(N6,'(A)') 'XSK: PROFILFEHLER: FLŽCHE WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSK: PROFILFEHLER: FLŽCHE WIRD KLEINER!'
         END IF
         IF (WWR.LT.WWRV) THEN
c           WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
c           WRITE(*,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY
            WRITE(N6,'(A)') 'XSK: PROFILFEHLER: UMFANG WIRD KLEINER!'
c           WRITE(*,'(A)') 'XSK: PROFILFEHLER: UMFANG WIRD KLEINER!'
         END IF

         RHYALT=RHY
         WWRALT=WWR

         CARHY = AWR*GEO
         IF (CARHY.LT.CARHYV) THEN
            WRITE(N6,*) 'RHY alt = ',RHY,'    WWR alt = ',WWR
            GEO = CARHYV/AWR
            RHY = GEO**(3./2.)
            WWR = AWR/RHY
            WSEC(KS,I) = SIENG(WWR,1)
            CARHY=CARHYV
            WRITE(N6,*) 'RHY neu = ',RHY,'    WWR neu = ',WWR
         END IF

c         IF (RHY.LT.RHYV) THEN
c            RHY=0.5*(RHY+RHYV)
c            RHY=RHYV
c            WWR = AWR/RHY
c            WSEC(KS,I) = SIENG(WWR,1)
c            GEO = RHY**(2./3.)
c            CARHY = AWR*GEO
c            WRITE(N6,*) 'RHY neu = ',RHY,'    WWR neu = ',WWR
c         END IF

         AWRV=AWR
         WWRV=WWR
      ELSE
         GEO = 0.0
      END IF

      IF (TWR.GT.WWR) THEN
         TWR=WWR
         TSEC(KS,I) = SIENG(TWR,1)
      END IF
       
      IF (NWP.EQ.0) THEN
C        1. FORMALER RHY-WENDEPUNKT = WASSERSPIEGEL
         NWP=NWP+1
         NNWP(KS)=NWP
         RNFAK(KS,NWP)=1.0
         HWP(KS,NWP)=HZERO
         IWP(KS,NWP)=I
C        WRITE(N6,3213) NWP,ENGSI(HZERO,1),IWP(KS,NWP),RNFAK(KS,NWP)
      END IF
C
      IF (I.GT.2) THEN
         DTWDH=(TWR-TWRVOR)/(HWR-HWRVOR)
      ELSE
         DTWDH=0
      END IF
C
      WRITE(60,3209) XLKM,HWR,AWR,TWR,DTWDH,WWR,RHY,LAB(KS)
 3209 FORMAT(7F10.3,I10)
C
C
C     Berechnung der potentiellen Querschnittsleistungen
C     QPOTW nach Wehranalogie, QPOTS nach Schützanalogie (TYP 8 und 9)
C     Wehr-Beiwert DMUEW = 0.57755775 (Querriegel i.d.R. breitkronig)
C     Schütz-Beiwert DMUES = 0.70 
C
      DMUEW=0.57755775
      DMUES=0.60

C     YBR=Sohllage, HS=gesamte Wassertiefe
C     ABW=unterer Querschnittsanteil (z.B. Durchlass), am Anfang = 0.0
      YBR=ENGSI(ZBOT,1)
      HS=HWR-YBR

C     Querschnittsleistung nach Schützanalogie
      IF (ABW.LT.0.01) THEN
         QPOTS=DMUES*AWR*SQRT(2.0*9.81*HS)
      ELSE
         QPOTS=DMUES*ABW*SQRT(2.0*9.81*HS)
      END IF

C     Querschnittsleistung nach Wehranalogie
      IF (ABW.LT.0.01) THEN
         HGR=AWR/TWR
         H37=HGR
         HUE=1.5*HGR
         QPOTW=DMUEW*AWR*SQRT(2.0*9.81*HUE)
         FRGRENZ=(QPOTW/AWR)/SQRT(9.81*HGR)
         IF (ITYP.EQ.8.OR.ITYP.EQ.9.OR.ITYP.EQ.18.OR.ITYP.EQ.19) THEN
            IF (HGR.GT.(4*HS).AND.ABS(DIFTWR).LT.0.002) THEN
               WRITE(N6,*) ' HGR = ',HGR,'  4*HS = ',(4*HS)
               ABW=AWR
               QPOTW=QPOTS
               FRGRENZ=1.0
            ELSE IF (HGR.LT.HGRV) THEN
               WRITE(N6,*) ' HGR = ',HGR,'  HGRV = ',HGRV
               WRITE(N6,*) 'UP XSK: Line 11425: HGR < HGRV !'
               ABW=AWR
               QPOTW=QPOTS
               FRGRENZ=1.0
            END IF
            IF (ABW.GT.0.01) THEN
               DBW=SQRT(4.0*ABW/3.14)
               WRITE(N6,*) ' ABW = ',ABW,'  DBW = ',DBW
            END IF
         END IF
      ELSE
         HGR=(AWR-ABW)/TWR
         H37=AWR/TWR
         HUE=1.5*HGR
c        IF (HGR.GT.HS) STOP 'HGR > HS!'
         IF (HGR.LE.0.0) THEN
            WRITE (25,*) ' AWR = ', AWR
            WRITE (25,*) ' ABW = ', ABW
            WRITE (25,*) ' TWR = ', TWR
c           STOP 'HGR < 0.0!'
            HGR=0.0
            QPOTW=0.0
            FRGRENZ=0.0
         ELSE
            QPOTW=DMUEW*(AWR-ABW)*SQRT(2.0*9.81*HUE)
            FRGRENZ=(QPOTW/(AWR-ABW))/SQRT(9.81*HGR)
         END IF
         QPOTW=QPOTW+QPOTS
      END IF

      IF (ABW.GT.0.01.AND.H37.GT.HS) THEN
         XLM=XLKM*1000.
         WRITE(37,'(I5,F10.1,F8.2)') LAB(KS),XLM,HWR
      ELSE IF (ABW.GT.0.01.AND.RHY.LT.RHYV.AND.I37.EQ.0) THEN
         XLM=XLKM*1000.
         WRITE(37,'(I5,F10.1,F8.2)') LAB(KS),XLM,HWR
         I37=1
      END IF

      HGRV=HGR
      IF (QGRENZ.GT.QGRENZV) QGRENZV=QGRENZ

      IF (QPOTW.LE.QGRENZV) THEN
         QGREXT=QGRENZV+DQGRENZ
         QPOTW1=0.5*(QGREXT+QPOTW) 
         IF (QPOTW1.LE.QGRENZV) QPOTW1=(QGREXT+QGREXT+QPOTW)/3.
         IF (QPOTW1.LE.QGRENZV) QPOTW1=(QGREXT+QGREXT+QGREXT+QPOTW)/4.
         IF (QPOTW1.LE.QGRENZV) QPOTW1=QGRENZV
      ELSE
         DQGRENZ=QPOTW-QGRENZV
         QPOTW1=QPOTW
      END IF

      IF (QPOTW1.LT.QGRENZV) THEN
         WRITE(N6,*) 'QGRENZV = ',QGRENZV,'  QPOTW1 = ',QPOTW1
         WRITE(N6,*) 'UP XSK: QPOTW1 < QGRENZV!'
      END IF

      QGRENZ=QPOTW1
      CGRENZV=CGRENZ
      CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
       
      DCARHY=CARHY-CARHYV
       
      IF (DIFTWR.GE.0.0) THEN
        IF (CARHY.LE.CARHYV.AND.CGRENZ.GT.CGRENZV) THEN
c         WRITE(N6,*) 'CGRENZV=',CGRENZV,' CGRENZ=',CGRENZ
c         WRITE(N6,*) ' CARHYV=',CARHYV, '  CARHY=',CARHY
          CARHY=QGRENZ/CGRENZV
          GEO = CARHY/AWR
          RHY = GEO**(3./2.)
          WWR = AWR/RHY
          WSEC(KS,I) = SIENG(WWR,1)
          CARHYV=CARHY
          IF (TWR.GT.WWR) THEN
            TWR=WWR
            TSEC(KS,I) = SIENG(TWR,1)
          END IF
          CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
        ELSE IF (DCARHY.LT.DCARHYV) THEN
          CARHY=CARHYV+DCARHYV
c         WRITE(N6,*) 'DCARHY < DCARHYV!'
          CARHY=0.5*(CARHYV+DCARHYV+CARHY)
          GEO = CARHY/AWR
          RHY = GEO**(3./2.)
          WWR = AWR/RHY
          WSEC(KS,I) = SIENG(WWR,1)
          IF (TWR.GT.WWR) THEN
            TWR=WWR
            TSEC(KS,I) = SIENG(TWR,1)
          END IF
          CGRENZ=QGRENZ/(AWR*RHY**(2./3.))
        END IF
      END IF

      WWRV=WWR
      DCARHYV=CARHY-CARHYV
      CARHYV=CARHY
 
      QGRSEC(KS,I)=SIENG(QGRENZ,3)
 
      WRITE(61,6161) LAB(KS),XLM,HWR,AWR,TWR,WWR,RHY,CARHY,QGRENZ
 6161 FORMAT(I5,F10.2,5F10.2,2F12.3)
C
      IF (ITAB.GT.0) THEN
         WRITE(N6,3210) LAB(KS),I,HWR,AWR,TWR,WWR,RHY,QGRENZ,CGRENZ,
     &                  CARHY
 3210    FORMAT(1X,2I9,9F12.3)
      END IF
      IF (RHYV.GE.RHYVV) THEN
C        RHY STEIGEND
         IF (RHY.LT.RHYV.AND.HV.GE.HZERO) THEN
C           ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
            NWP=NWP+1
            NNWP(KS)=NWP
            IF (NWP.EQ.2) GEO1=GEOV
            RNFAK(KS,NWP)=GEOV/GEO1
            HWP(KS,NWP)=HV
            IWP(KS,NWP)=I-1
C           WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
         END IF
      ELSE IF (RHYV.LT.RHYVV) THEN
C        RHY FALLEND
         IF (RHY.GT.RHYV.AND.HV.GE.HZERO) THEN
C           ECHTER RHY-WENDEPUNKT > WASSERSPIEGEL
            NWP=NWP+1
            NNWP(KS)=NWP
            IF (NWP.EQ.2) GEO1=GEOV
            RNFAK(KS,NWP)=GEOV/GEO1
            HWP(KS,NWP)=HV
            IWP(KS,NWP)=I-1
C           WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
 3213 FORMAT(I5,'. RHY-WENDEP.: ',F8.3,' I = ',I2,' RNFAK = ',F8.3)
         END IF
      END IF
      IF (I.EQ.NSTP) THEN
C        LETZTER WERT IN PROFILTABELLE
         NWP=NWP+1
         NNWP(KS)=NWP
         IF (NWP.EQ.2) GEO1=GEO
         RNFAK(KS,NWP)=GEO/GEO1
         HWP(KS,NWP)=H
         IWP(KS,NWP)=I
C        WRITE(N6,3213) NWP,ENGSI(HV,1),IWP(KS,NWP),RNFAK(KS,NWP)
      END IF
      RHYVV=RHYV
      RHYV=RHY
      GEOV=GEO
      HV=H

      IF (ASEC(KS,I).LE.0.) THEN
         HWSP=ENGSI(H,1)
         WRITE(N6,3211) LAB(KS), HWSP, ENGSI(ASEC(KS,I),2)
 3211    FORMAT(1x,' UP XSK KNOTEN ',I5,' WSP= ',F7.2,' A= ',F7.2)
         STOP 3211
      ENDIF
      IF (TSEC(KS,I).LT.0.) THEN
         HWSP=ENGSI(H,1)
c        HWSP=H+ZBOT
c        HWSP=ENGSI(HWSP,1)
         WRITE(N6,3212) LAB(KS), HWSP, ENGSI(TSEC(KS,I),1)
 3212 FORMAT(1x,'UP XSK FEHLER AN KNOTEN ',I5,' WSP= ',F7.2,' T= ',F7.2)
         WRITE(N6,3214) I,ENGSI(XR,1),ENGSI(XL,1),ENGSI(TSEC(KS,I),1)
 3214 FORMAT(1x,' I= ',I5,' XR= ',F7.2,' XL= ',F7.2,' T=',F7.2)
         STOP 3212
      ENDIF

 3220 CONTINUE
c
      return
      end

C-----------------------------------------------------------------------
      SUBROUTINE ygrenz(i,q,ygr,hmin)
C
C      BERECHNUGNG DER GRENZTIEFE YGR

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
      COMMON /TRAP/         ZTR(IPSEC)  , ZTL(IPSEC)
      dimension ho(1500),v(1500),y(1500)
c     dy=sieng(0.01,1)
      dy=0.01
      ks=int(zs(i)/dy)
      if (ks.gt.1500) then
        dy=zs(i)/REAL(1500)
        ks=1500
      endif
      g2=32.174*2.
      hit=zo(i)
      do 1000 k=1,ks
      hit=hit+dy
      call shape(i,hit,a,xwp,top)
      IF(A.LE.0.) THEN
      WRITE(N6,112) IAB(I),ENGSI(HIT,1)
  112 FORMAT(1x,'UP YGRENZ  FEHLER A<=0 AM KNOTEN ',I5,' WSP = ',F7.2)
      STOP 112
      ENDIF
      v(k)=q/a
      y(k)=hit-zo(i)
      ho(k)=v(k)*v(k)/g2 + y(k)
c     write(n6,111)k,q,v(k),y(k),a,top
c 111 format(' k=',i4,' Q= ',f7.2,' v= ',f7.2,' y= ',f7.2,' A= ',f7.2,
c    &' Breite = ',f7.2)
 1000 continue
      hmin=ho(1)
      klabel=1
      do 1100 k=2,ks
      if ( ho(k) .lt. hmin ) then
      hmin = ho(k)
      klabel=k
      else
      goto 1100
      endif
 1100 continue
      ygr=y(klabel)
      write(n6,*)' IAB = ',iab(i)
      hm=engsi(hmin,1)
      yg=engsi(ygr,1)
      abfl=engsi(q,3)
      write(n6,*)' homin = ',hm,' ygrenz = ',yg,' Q = ',abfl
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE YGRENZ4(I,Q,YQGR,HMIN)
C
C      BERECHNUGNG DER GRENZTIEFE YGR

      INCLUDE 'DIMENS.INC'

      COMMON /IO/           N5          , N6          , IN          ,
     1        IOUT        , IGRAPH
      COMMON /AB/           IAB(IPSEC)
      COMMON /IRRSEC/       ISEC,
     1                      NCO(IPSEC),LAB(IPSEC),ASEC(IPSEC,IPSTP),
     2                      TSEC(IPSEC,IPSTP),WSEC(IPSEC,IPSTP),
     3                      CH2SEC(IPSEC,IPSTP),CHRSEC(IPSEC,IPSTP),
     4                      QGRSEC(IPSEC,IPSTP)
      COMMON /GEOMTY/       ITYPE(IPSEC) , WIDTH(IPSEC) , HEIT(IPSEC) ,
     1        ZO(IPSEC)   , ZS(IPSEC)   , RNI(IPSEC) , RNIBAK(IPSEC),
     2        ZL(IPSEC)   , ZR(IPSEC)
C
      GRAV = 32.174
C
      IF (ITYPE(I).GE.4) THEN
         DH=HEIT(I)/REAL(IPSTP-1)
      ELSE
         DH=ZS(I)/REAL(IPSTP-1)
         WRITE(N6,*)'UP YGRENZ4: ',IAB(I),' ZS = ',ENGSI(ZS(I),1)
         WRITE(N6,*)'UP YGRENZ4: ',IAB(I),' DH = ',ENGSI(DH,1)
      END IF

      YQGR=-999.99
      Q=ABS(Q)

      DO IP=1,IPSTP-1

         D1=DH*REAL(IP-1)
         D2=D1+DH
         H1=ZO(I)+D1
         H2=ZO(I)+D2

         IF (IP.GT.1) THEN
            CALL SHAPE2(I,H1,A1,WP1,B1,CH2,CHR,QGR1)
         ELSE
            H1=ZO(I)
            A1=0.0
            WP1=0.0
            B1=0.0
            QGR1=0.0
         END If

         CALL SHAPE2(I,H2,A2,WP2,B2,CH2,CHR,QGR2)
       
         IF (Q.GE.QGR1.AND.Q.LE.QGR2) THEN
       
c           WRITE(N6,*)'UP YGRENZ4: ',IAB(I),' QGR1 = ',ENGSI(QGR1,3)
c           WRITE(N6,*)'UP YGRENZ4: ',IAB(I),'    Q = ',ENGSI(Q,3)
c           WRITE(N6,*)'UP YGRENZ4: ',IAB(I),' QGR2 = ',ENGSI(QGR2,3)
       
            HQGR=H1+((H2-H1)/(QGR2-QGR1))*(Q-QGR1)
            AQGR=A1+((A2-A1)/(QGR2-QGR1))*(Q-QGR1)
            BQGR=B1+((B2-B1)/(QGR2-QGR1))*(Q-QGR1)
            YQGR=HQGR-ZO(I)
            YGR=AQGR/BQGR
            VGR=Q/AQGR
            HMIN=(VGR*VGR)/(2*GRAV)+YGR
            FRGR=VGR/SQRT(GRAV*YGR)
       
         END IF

      END DO

      IF (YQGR.LE.-999.0) THEN
         WRITE(25,*)'YGRENZ4: ',IAB(I),'    Q = ',ENGSI(Q,3)
         WRITE(25,*)'YGRENZ4: YQGR nicht aus Tabelle bestimmbar!'
         WRITE(25,*)'YGRENZ4: YQGR wird extrapoliert!'
         WRITE(25,*)'UP YGRENZ4: ',IAB(I),' QGR1 = ',ENGSI(QGR1,3)
         WRITE(25,*)'UP YGRENZ4: ',IAB(I),'    Q = ',ENGSI(Q,3)
         WRITE(25,*)'UP YGRENZ4: ',IAB(I),' QGR2 = ',ENGSI(QGR2,3)

         HQGR=H1+((H2-H1)/(QGR2-QGR1))*(Q-QGR1)
         AQGR=A1+((A2-A1)/(QGR2-QGR1))*(Q-QGR1)
         BQGR=B1+((B2-B1)/(QGR2-QGR1))*(Q-QGR1)
         YQGR=HQGR-ZO(I)
         YGR=AQGR/BQGR
         VGR=Q/AQGR
         HMIN=(VGR*VGR)/(2*GRAV)+YGR
         FRGR=VGR/SQRT(GRAV*YGR)

      END IF
       
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ZEIGER(J,L1E,IAB,IZ)

      INCLUDE 'DIMENS.INC'
       
      INTEGER L1E(IPLAT),IAB(IPSEC)
C
C     SUCHE DES PASSENDEN KNOTENZEIGERS
      DO 21 IPS=1,IPSEC
         IF (L1E(J).EQ.IAB(IPS)) THEN
            IZ=IPS
            IF (IZ.GT.IPSEC) THEN
               PRINT '(T2,A,I5)','   IZ = ',IZ
               PRINT '(T2,A,I5)','IPSEC = ',IPSEC
               STOP 'UP ZEIGER IZ > IPSEC ! '
            END IF
            GOTO 22
         END IF
  21  CONTINUE
  22  RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ZEIT (TIM,ITH,ITM,ITS)

C     EINGABE :   TIM  = ZEIT IN MINUTEN
C     AUSGABE :   ITH  = STUNDEN
C                 ITM  = MINUTEN
C                 ITS  = SEKUNDEN
C
      IMPLICIT REAL (A-H)
      IMPLICIT INTEGER (I-N)
      IMPLICIT REAL (O-Z)
C
      TM =  MOD (TIM,60.)
      TH =  TIM / 60.
      ITH = INT(TH)
      ITH = MOD(ITH,24)
      ITM = INT (TM)
      TS  = (TM - REAL(ITM) ) * 60.
      ITS = INT (TS)
      RETURN
      END
C-----------------------------------------------------------------------
