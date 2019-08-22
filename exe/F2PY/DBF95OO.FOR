!-----------------------------------------------------------------------
!     neue Version von READ_DBF-Unterprogrammen,
!     die auch f¸r DBF-Files aus OpenOffice funktionoieren
!-----------------------------------------------------------------------
      SUBROUTINE AVGINP (HDRDAT,FLTDAT,NCOLS,NROWS,XLLCORNER,YLLCORNER,
     &           XLLCENTER,YLLCENTER,CELLSIZE,ZNODATA,BYTEORDER,ZARCV,
     &           FAKTOR,OFFSET,ZSURF,XMIND,XMAXD,YMIND,YMAXD,ZMIN,ZMAX)
!     UNTERPROGRAMM ZUM EINLESEN DES BINéREN ARCVIEW-GRID-FORMATS
!     BESTEHEND AUS DER HDR-DATEI UND DER FLT-DATEI
!-----------------------------------------------------------------------
!     VEREINBARUNGEN HDR-FILE
      CHARACTER*255 HDRDAT
      INTEGER NCOLS,NROWS
      DOUBLE PRECISION XLLCORNER,YLLCORNER,XLLCENTER,YLLCENTER,CELLSIZE
      REAL ZNODATA
      CHARACTER*8 BYTEORDER
!     VEREINBARUNGEN FLT-FILE
      CHARACTER*255 FLTDAT
      CHARACTER*4 FOURBYTE
      REAL ZARCV(*)
!     VEREINBARUNGEN FUER SURFER-KOMPATIBLEN VEKTOR ZSURF (GEOEAS!)
      REAL ZSURF(*)
      DOUBLE PRECISION XMIND,XMAXD,YMIND,YMAXD
      ZBLANK=1.70141e+038

!     EINLESEN HDR-FILE
!
      CALL LESHDR (HDRDAT,NCOLS,NROWS,NMAX,XLLCORNER,YLLCORNER,
     &            XLLCENTER,YLLCENTER,CELLSIZE,ZNODATA,BYTEORDER,
     &            XMIND,XMAXD,YMIND,YMAXD)
!
!     EINLESEN FLT-FILE
      WRITE(7,'(T2,A,A)') FLTDAT,' OPEN'
      OPEN(99,FILE=FLTDAT,ACCESS='TRANSPARENT',STATUS='OLD')
      DO 8 N=1,NMAX
         READ(99,'(A4)') FOURBYTE
!        WRITE(7,'(T2,A4)') FOURBYTE
         READ(FOURBYTE,'(A4)') ZARCV(N)
!        WRITE(7,'(T2,F10.3)') ZARCV(N)
    8 CONTINUE
      CLOSE (99,STATUS='KEEP')
      WRITE(7,'(T2,A,A)') FLTDAT,' CLOSED'
!
      ZNODATA=ZNODATA+1
!     WRITE(7,'(T2,A,F10.2)') 'ZNODATA = ',ZNODATA
      ZMIN=999999.
      ZMAX=-999999.
      DO 9 NY=1,NROWS
         DO 10 NX=1,NCOLS
            NSURF = (NX-1)*NROWS + NY
            NARCV = (NROWS-NY)*NCOLS+NX
            IF (ZARCV(NARCV).LT.ZNODATA) THEN
               ZSURF(NSURF)=ZBLANK
            ELSE
               ZSURF(NSURF)=ZARCV(NARCV)/FAKTOR+OFFSET
               IF (ZSURF(NSURF).GT.ZMAX) ZMAX=ZSURF(NSURF)
               IF (ZSURF(NSURF).LT.ZMIN) ZMIN=ZSURF(NSURF)
            END IF
   10    CONTINUE
    9 CONTINUE
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE LESHDR (HDRDAT,NCOLS,NROWS,NMAX,XLLCORNER,YLLCORNER,
     &                  XLLCENTER,YLLCENTER,CELLSIZE,ZNODATA,BYTEORDER,
     &                  XMIND,XMAXD,YMIND,YMAXD)
!     UNTERPROGRAMM ZUM EINLESEN DER HDR-DATEI
!-----------------------------------------------------------------------
!     VEREINBARUNGEN HDR-FILE
      CHARACTER*255 HDRDAT
      CHARACTER*30 LINE,FORM
      INTEGER NCOLS,NROWS
      DOUBLE PRECISION XLLCORNER,YLLCORNER,XLLCENTER,YLLCENTER,CELLSIZE
      REAL ZNODATA
      CHARACTER*8 BYTEORDER
      DOUBLE PRECISION XMIND,XMAXD,YMIND,YMAXD

!     EINLESEN HDR-FILE
      WRITE(7,'(T2,A,A)') HDRDAT,' OPEN'
      OPEN(88,FILE=HDRDAT,STATUS='OLD')
!     NCOLS LESEN
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 1 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    1 CONTINUE
      IF (IL.EQ.1) FORM='(I1)'
      IF (IL.EQ.2) FORM='(I2)'
      IF (IL.EQ.3) FORM='(I3)'
      IF (IL.EQ.4) FORM='(I4)'
      IF (IL.EQ.5) FORM='(I5)'
      IF (IL.EQ.6) FORM='(I6)'
      READ(LINE(IA:IE),FORM) NCOLS
!     NROWS LESEN
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 2 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    2 CONTINUE
      IF (IL.EQ.1) FORM='(I1)'
      IF (IL.EQ.2) FORM='(I2)'
      IF (IL.EQ.3) FORM='(I3)'
      IF (IL.EQ.4) FORM='(I4)'
      IF (IL.EQ.5) FORM='(I5)'
      IF (IL.EQ.6) FORM='(I6)'
      READ(LINE(IA:IE),FORM) NROWS
!     XLLCORNER BZW. XLLCENTER LESEN
      XLLCORNER=0.0
      XLLCENTER=0.0
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 3 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    3 CONTINUE
      IF (IL.EQ.2) FORM='(F2.0)'
      IF (IL.EQ.3) FORM='(F3.0)'
      IF (IL.EQ.4) FORM='(F4.0)'
      IF (IL.EQ.5) FORM='(F5.0)'
      IF (IL.EQ.6) FORM='(F6.0)'
      IF (IL.EQ.7) FORM='(F7.0)'
      IF (IL.EQ.8) FORM='(F8.0)'
      IF (IL.EQ.9) FORM='(F9.0)'
      IF (IL.EQ.10) FORM='(F10.0)'
      IF (IL.EQ.11) FORM='(F11.0)'
      IF (IL.EQ.12) FORM='(F12.0)'
      IF (IL.EQ.13) FORM='(F13.0)'
      IF (IL.EQ.14) FORM='(F14.0)'
      IF (IL.EQ.15) FORM='(F15.0)'
      IF (LINE(1:9).EQ.'xllcorner') READ(LINE(IA:IE),FORM) XLLCORNER
      IF (LINE(1:9).EQ.'xllcenter') READ(LINE(IA:IE),FORM) XLLCENTER
!     YLLCORNER BZW. YLLCENTER LESEN
      YLLCORNER=0.0
      YLLCENTER=0.0
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 4 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    4 CONTINUE
      IF (IL.EQ.2) FORM='(F2.0)'
      IF (IL.EQ.3) FORM='(F3.0)'
      IF (IL.EQ.4) FORM='(F4.0)'
      IF (IL.EQ.5) FORM='(F5.0)'
      IF (IL.EQ.6) FORM='(F6.0)'
      IF (IL.EQ.7) FORM='(F7.0)'
      IF (IL.EQ.8) FORM='(F8.0)'
      IF (IL.EQ.9) FORM='(F9.0)'
      IF (IL.EQ.10) FORM='(F10.0)'
      IF (IL.EQ.11) FORM='(F11.0)'
      IF (IL.EQ.12) FORM='(F12.0)'
      IF (IL.EQ.13) FORM='(F13.0)'
      IF (IL.EQ.14) FORM='(F14.0)'
      IF (IL.EQ.15) FORM='(F15.0)'
      IF (LINE(1:9).EQ.'yllcorner') READ(LINE(IA:IE),FORM) YLLCORNER
      IF (LINE(1:9).EQ.'yllcenter') READ(LINE(IA:IE),FORM) YLLCENTER
!     CELLSIZE LESEN
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 5 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    5 CONTINUE
      IF (IL.EQ.2) FORM='(F2.0)'
      IF (IL.EQ.3) FORM='(F3.0)'
      IF (IL.EQ.4) FORM='(F4.0)'
      IF (IL.EQ.5) FORM='(F5.0)'
      IF (IL.EQ.6) FORM='(F6.0)'
      IF (IL.EQ.7) FORM='(F7.0)'
      IF (IL.EQ.8) FORM='(F8.0)'
      IF (IL.EQ.9) FORM='(F9.0)'
      IF (IL.EQ.10) FORM='(F10.0)'
      IF (IL.EQ.11) FORM='(F11.0)'
      IF (IL.EQ.12) FORM='(F12.0)'
      IF (IL.EQ.13) FORM='(F13.0)'
      IF (IL.EQ.14) FORM='(F14.0)'
      IF (IL.EQ.15) FORM='(F15.0)'
      READ(LINE(IA:IE),FORM) CELLSIZE
!     NODATA_VALUE LESEN
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 6 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    6 CONTINUE
      IF (IL.EQ.2) FORM='(F2.0)'
      IF (IL.EQ.3) FORM='(F3.0)'
      IF (IL.EQ.4) FORM='(F4.0)'
      IF (IL.EQ.5) FORM='(F5.0)'
      IF (IL.EQ.6) FORM='(F6.0)'
      IF (IL.EQ.7) FORM='(F7.0)'
      IF (IL.EQ.8) FORM='(F8.0)'
      IF (IL.EQ.9) FORM='(F9.0)'
      IF (IL.EQ.10) FORM='(F10.0)'
      IF (IL.EQ.11) FORM='(F11.0)'
      IF (IL.EQ.12) FORM='(F12.0)'
      IF (IL.EQ.13) FORM='(F13.0)'
      IF (IL.EQ.14) FORM='(F14.0)'
      IF (IL.EQ.15) FORM='(F15.0)'
      READ(LINE(IA:IE),FORM) ZNODATA
!     BYTEORDER LESEN
      READ(88,'(A)') LINE
      IA=30
      IE=30
      DO 7 I=2,30
         IF (LINE(I:I).NE.' '.AND.LINE((I-1):(I-1)).EQ.' ') IA=I
         IF (I.GT.IA) THEN
            IF (LINE(I:I).EQ.' '.AND.LINE((I-1):(I-1)).NE.' ') IE=I-1
         END IF
         IL=IE-IA+1
    7 CONTINUE
      READ(LINE(IA:IE),'(A8)') BYTEORDER
      CLOSE (88,STATUS='KEEP')
      WRITE(7,'(T2,A,A)') HDRDAT,' CLOSED'
!
!     WRITE(7,'(T2,A,I5)')    '    NCOLS = ',NCOLS
!     WRITE(7,'(T2,A,I5)')    '    NROWS = ',NROWS
!     WRITE(7,'(T2,A,F14.6)') 'XLLCORNER = ',XLLCORNER
!     WRITE(7,'(T2,A,F14.6)') 'YLLCORNER = ',YLLCORNER
!     WRITE(7,'(T2,A,F14.6)') 'XLLCENTER = ',XLLCENTER
!     WRITE(7,'(T2,A,F14.6)') 'YLLCENTER = ',YLLCENTER
!     WRITE(7,'(T2,A,F14.6)') ' CELLSIZE = ',CELLSIZE
!     WRITE(7,'(T2,A,F14.6)') '  ZNODATA = ',ZNODATA
!     WRITE(7,'(T2,A,A8)')    'BYTEORDER = ',BYTEORDER
      NMAX=NCOLS*NROWS
!     WRITE(7,'(T2,A,I9)')    '     NMAX = ',NMAX
      XMIND=XLLCORNER+CELLSIZE/2.
      YMIND=YLLCORNER+CELLSIZE/2.
      XMAXD=XMIND+CELLSIZE*(NCOLS-1)
      YMAXD=YMIND+CELLSIZE*(NROWS-1)
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ZVONXY(X,Y,ZSURF,NMX,NMY,DX,DY,XMIN,XMAX,YMIN,YMAX,
     &                  ZMIN,ZMAX,ZXY)
      REAL ZSURF(*)
      DOUBLE PRECISION X,Y,DX,DY,XMIN,XMAX,YMIN,YMAX,X1,Y1
      DOUBLE PRECISION DNXNY,DNX1NY,DNXNY1
      DOUBLE PRECISION GNXNY,GNX1NY,GNXNY1,SUMGEW
!
!     MITTELZELLE FINDEN
!
      NX=DNINT((X-XMIN)/DX)+1
      NY=DNINT((Y-YMIN)/DY)+1
!
!     NéCHSTER NACHBAR IN X-RICHTUNG
!
      X1=X+DX/2.
      NX1=DNINT((X1-XMIN)/DX)+1
      IF (NX1.EQ.NX) THEN
         X1=X-DX/2.
         NX1=DNINT((X1-XMIN)/DX)+1
      END IF
      IF (NX1.GT.NMX) NX1=NMX
      IF (NX1.LT.1) NX1=1
!
!     NéCHSTER NACHBAR IN Y-RICHTUNG
!
      Y1=Y+DY/2.
      NY1=DNINT((Y1-YMIN)/DY)+1
      IF (NY1.EQ.NY) THEN
         Y1=Y-DY/2.
         NY1=DNINT((Y1-YMIN)/DY)+1
      END IF
      IF (NY1.GT.NMY) NY1=NMY
      IF (NY1.LT.1) NY1=1
!
!     ZWERTE BELEGEN
!
      N=(NX-1)*NMY+NY
      IF (ZSURF(N).GT.ZMIN.AND.ZSURF(N).LT.ZMAX) THEN
         ZNXNY=ZSURF(N)
      ELSE
         ZNXNY=-9999
      END IF
      N=(NX1-1)*NMY+NY
      IF (ZSURF(N).GE.ZMIN.AND.ZSURF(N).LE.ZMAX) THEN
         ZNX1NY=ZSURF(N)
      ELSE
         ZNX1NY=-9999
         ZNXNY=-9999
      END IF
      N=(NX-1)*NMY+NY1
      IF (ZSURF(N).GE.ZMIN.AND.ZSURF(N).LE.ZMAX) THEN
         ZNXNY1=ZSURF(N)
      ELSE
         ZNXNY1=-9999
         ZNXNY=-9999
      END IF
!
!     MITTELWERT UND VARIANZ BERECHNEN
!
      ZMIT=(ZNXNY+ZNX1NY+ZNXNY1)/3
      D2NXNY=(ZNXNY-ZMIT)**2
      D2NX1NY=(ZNX1NY-ZMIT)**2
      D2NXNY1=(ZNXNY1-ZMIT)**2
      SZ2=(D2NXNY+D2NX1NY+D2NXNY1)/2
      SZ=SQRT(SZ2)
!
!     FALLS STANDARDABWEICHUNG SZ ZU GROSS, PROTOKOLL AUSGEBEN
!     FALLS STANDARDABWEICHUNG SZ ZU GROSS, ZXY=-9999
!
      SZUL=0.05*ZMIT
      IF (SZ.GT.SZUL) THEN
!        WRITE(7,110)'NMX = ',NMX,'  NMY = ',NMY
!        WRITE(7,111)' NX = ',NX, '   NY = ',NY, '   ZNXNY = ',ZNXNY
!        WRITE(7,111)'NX1 = ',NX1,'   NY = ',NY, '  ZNX1NY = ',ZNX1NY
!        WRITE(7,111)' NX = ',NX, '  NY1 = ',NY1,'  ZNXNY1 = ',ZNXNY1
!        WRITE(7,112)'XMIN = ',XMIN,'  X = ',X,'  XMAX = ',XMAX
!        WRITE(7,112)'YMIN = ',YMIN,'  Y = ',Y,'  YMAX = ',YMAX
  110    FORMAT(T2,2(A,I4))
  111    FORMAT(T2,2(A,I4),A,F10.3)
  112    FORMAT(T2,3(A,F12.3))
         ZXY=-9999.
      ELSE
!
!        ABSTéNDE BERECHNEN
!
         XNXNY=XMIN+DX*(NX-1)
         YNXNY=YMIN+DY*(NY-1)
         DNXNY=(X-XNXNY)*(X-XNXNY)+(Y-YNXNY)*(Y-YNXNY)
         IF (DNXNY.LT.0.000001) DNXNY=0.000001
         DNXNY=DSQRT(DNXNY)

         XNX1NY=XMIN+DX*(NX1-1)
         DNX1NY=(X-XNX1NY)*(X-XNX1NY)+(Y-YNXNY)*(Y-YNXNY)
         IF (DNX1NY.LT.0.000001) DNX1NY=0.000001
         DNX1NY=DSQRT(DNX1NY)

         YNXNY1=YMIN+DY*(NY1-1)
         DNXNY1=(X-XNXNY)*(X-XNXNY)+(Y-YNXNY1)*(Y-YNXNY1)
         IF (DNXNY1.LT.0.000001) DNXNY1=0.000001
         DNXNY1=DSQRT(DNXNY1)
!
!     GEWICHTE BERECHNEN (INVERSE DISTANZ ** 2)
!
         GNXNY=(1/DNXNY)**2.
         GNX1NY=(1/DNX1NY)**2.
         GNXNY1=(1/DNXNY1)**2.
         SUMGEW=GNXNY+GNX1NY+GNXNY1
         GNXNY=GNXNY/SUMGEW
         GNX1NY=GNX1NY/SUMGEW
         GNXNY1=GNXNY1/SUMGEW
!
!        ZWERT ZXY BERECHNEN
!
         ZXY=ZNXNY*GNXNY+ZNX1NY*GNX1NY+ZNXNY1*GNXNY1
      END IF
!
      IF (ZXY.GT.ZMIN.AND.ZXY.LT.ZMAX) THEN
         ZXY=ZXY
      ELSE
         ZXY=-9999
      END IF
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE LESBNA (BNADAT,NBNA,CPID,LPID,CSID,LSID,NXY,X,Y,NDIM)
      CHARACTER*255 BNADAT
      CHARACTER*30 CPID(*),CSID(*)
      CHARACTER*1 CTEST1
      INTEGER NXY(*),LPID(*),LSID(*)
      INTEGER IP1(4),IP2(2)
      DOUBLE PRECISION X(NDIM,*),Y(NDIM,*)
      CHARACTER*80 BNAHED
!
      WRITE(7,'(T2,A,A)') BNADAT,' OPEN'
      OPEN(99,FILE=BNADAT,STATUS='OLD')
!
      NH=0
    1 NH=NH+1
      READ (99,'(A)',END=999) BNAHED
      I1=0
      I2=0
      DO 2 I=1,80
         CTEST1=BNAHED(I:I)
         IF (CTEST1.EQ.'"') THEN
            I1=I1+1
            IP1(I1)=I
         END IF
         IF (CTEST1.EQ.',') THEN
            I2=I2+1
            IP2(I2)=I
         END IF
         IF (CTEST1.EQ.' ') THEN
            IP3=I
         END IF
    2 CONTINUE
      CPID(NH)=BNAHED((IP1(1)+1):(IP1(2)-1))
      LPID(NH)=IP1(2)-1-IP1(1)
      CSID(NH)=BNAHED((IP1(3)+1):(IP1(4)-1))
      LSID(NH)=IP1(4)-1-IP1(3)
!     WRITE(7,'(T2,A)') BNAHED
!     WRITE(7,'(T2,A,A,A,I3)') '  PID = ',CPID(NH),'  LPID = ',LPID(NH)
!     WRITE(7,'(T2,A,A,A,I3)') '  SID = ',CSID(NH),'  LSID = ',LSID(NH)
      READ(BNAHED((IP2(2)+1):(IP3-1)),*,END=999) NXY(NH)
      IF (NXY(NH).LT.0) NXY(NH)=NXY(NH)*(-1)
!     WRITE(7,'(A,I6)')   ' NXY = ',NXY(NH)
!
      READ (99,*) (X(N,NH),Y(N,NH),N=1,NXY(NH))
!
      GOTO 1
!
  999 NBNA=NH-1
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE CLEARCSV (CL,LCL)
      CHARACTER (LEN=LCL) CL
      DO LC=1,LCL-1
         CL(LC:(LC+1))=' '
      END DO
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITECSV (LU,CL,LCL,C1TRENN)
!     CHARACTER*4096 CL
      CHARACTER (LEN=LCL) CL
!     CHARACTER*4096 CSV
      CHARACTER (LEN=LCL+1) CSV,CSVRED
      CHARACTER (LEN=1) C1TRENN
      INTEGER :: IA(256) = 0
      INTEGER :: IE(256) = 0
      INTEGER :: ITX(256) = 0
!
!     IA=STARTPOSITION EINER VARIABLE
!     IE=ENDPOSITION EINER VARIABLE
!
      WRITE(7,'(T2,A,I4)') 'LCL = ',LCL
      WRITE(7,'(T2,A)') CL(1:LCL)
!
!     ERSTE ZEICHEN ANALYSIEREN
      I=0
    1 I=I+1
      IF (CL(I:I).EQ.'"') THEN
         ITEXT=1
         IV=1
         IA(IV)=I
c        WRITE(7,*) 'ANF TEXT: IV = ',IV,'  IA = ',IA(IV)
      ELSE
         ITEXT=0
      END IF
      ITX(1)=ITEXT
      IF (CL(I:I).NE.' '.AND.ITEXT.EQ.0) THEN
         IV=1
         IA(IV)=I
c        WRITE(7,*) 'ANF VARI: IV = ',IV,'  IA = ',IA(IV)
      ELSE IF (CL(I:I).EQ.' ') THEN
         IV=0
         GOTO 1
      END IF

!     SCHLEIFE öBER DIE ZELLEN DES STRINGS CL
      LC=LEN_TRIM(CL)
      WRITE(7,'(T2,A,I4)') 'LC = ',LC
      WRITE(7,'(T2,A)') CL(1:LC)
      DO I=1,LC
         J=I+1
         IF (ITEXT.EQ.0) THEN
!           WRITE(7,'(T2,A,I1)') 'ITEXT = ',ITEXT
            IF (CL(I:I).EQ.' '.AND.CL(J:J).NE.' ') THEN
               IF (CL(J:J).EQ.'"') THEN
!                 NEUER TEXT
!                 WRITE(7,'(T2,A)') 'NEUER TEXT'
                  ITEXT=1
                  IV=IV+1
                  ITX(IV)=ITEXT
                  IA(IV)=J+1
c                 WRITE(7,*) 'NEU TEXT: IV = ',IV,'  IA = ',IA(IV)
               ELSE
!                 NEUE VARIABLE (KEIN TEXT => ITEXT=0)
!                 WRITE(7,'(T2,A)') 'NEUE VARIABLE'
                  ITEXT=0
                  IF (J.GT.IA(IV)) IV=IV+1
                  ITX(IV)=ITEXT
                  IA(IV)=J
c                 WRITE(7,*) 'NEU VARI: IV = ',IV,'  IA = ',IA(IV)
               END IF
            END IF
            IF (CL(I:I).NE.' '.AND.CL(J:J).EQ.' ') THEN
!              WRITE(7,'(T2,A,I4)') 'J = ',J
!              ENDE DER VARIABLE
!              WRITE(7,'(T2,A)') 'ENDE DER VARIABLE'
               IE(IV)=I
c              WRITE(7,*) 'END VARI: IV = ',IV,'  IE = ',IE(IV)
            END IF
         ELSE IF (ITEXT.EQ.1) THEN
!           WRITE(7,'(T2,A,I1)') 'ITEXT = ',ITEXT
!           WRITE(7,'(T2,A,I4)') 'J = ',J
            IF (CL(I:I).EQ.'"'.AND.CL(J:J).EQ.' ') THEN
!              ENDE DES TEXTES
!              WRITE(7,'(T2,A)') 'ENDE DES TEXTES'
               ITEXT=0
               IE(IV)=I-1
c              WRITE(7,*) 'END TEXT: IV = ',IV,'  IE = ',IE(IV)
            END IF
            IF (CL(I:I).EQ.'"'.AND.CL(J:J).EQ.',') THEN
!              ENDE DES TEXTES
!              WRITE(7,'(T2,A)') 'ENDE DES TEXTES'
               ITEXT=0
               IE(IV)=I-1
c              WRITE(7,*) 'END TEXT: IV = ',IV,'  IE = ',IE(IV)
            END IF
         END IF
      END DO

      IVMAX=IV
      WRITE(7,*) ' IVMAX = ',IVMAX
       
!     BELEGEN DES AUSGABESTRINGS CSV
      ICA=1
      DO IV=1,IVMAX
         L=IE(IV)-IA(IV)+1
         ICE=ICA+L-1
         IF (ICE.GT.LC) THEN

            WRITE(7,*) '    IK = ',IK
            WRITE(7,*) '   ICA = ',ICA
            WRITE(7,*) '    IV = ',IV
            WRITE(7,*) '    IA = ',IA(IV),' IE = ',IE(IV)
            WRITE(7,*) '     L = ',L
            WRITE(7,*) '   ICE = ',ICE
            WRITE(7,*) '   ITX = ',ITX(IV)

            CLOSE(7,STATUS='KEEP')

            STOP 'ICE.GT.LC!'

         END IF
         IK=ICE+1
         CSV(ICA:ICE)=CL(IA(IV):IE(IV))
!        IF (IV.LT.IVMAX) CSV(IK:IK)=','
         IF (IV.LT.IVMAX) CSV(IK:IK)=C1TRENN
         IF (IV.EQ.IVMAX.AND.ITX(IV).EQ.1) THEN
           IF (CSV((IK-1):(IK-1)).NE.'"') THEN
             CSV(IK:IK)='"'
             ICE=IK
           END IF
         END IF
         ICA=IK+1
      END DO
!     WRITE(LU,'(A)') CSV(1:ICE)
!     CSV reduziern
      ITEXT=0
      IR=0
      IG=0
!     WRITE(7,*) 'ICE = ',ICE
      DO I=1,ICE
         IF (CSV(I:I).EQ.'"') IG=IG+1
         IF (ITEXT.EQ.0.AND.CSV(I:I).EQ.'"') THEN
            ITEXT=1
         ELSE IF (ITEXT.EQ.1.AND.CSV(I:I).EQ.'"') THEN
            ITEXT=0
         END IF
         IF (ITEXT.EQ.1) THEN
            IR=IR+1
            CSVRED(IR:IR)=CSV(I:I)
         ELSE
            IF (CSV(I:I).NE.' ') THEN
               IR=IR+1
               CSVRED(IR:IR)=CSV(I:I)
            END IF
         END IF
      END DO
!     WRITE(7,*) ' IG = ',IG
      IRE=IR
      WRITE(LU,'(A)') CSVRED(1:IRE)
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE DIMTES (N,NDIM,LINE)
      IF (N.GT.NDIM) THEN
         WRITE(7,*) 'NDIM = ',NDIM
         WRITE(7,*) ' N = ',N
         WRITE(7,*) ' N > NDIM'
         WRITE(7,*) 'LINE = ',LINE
         STOP ' FELDöBERSCHREITUNG ! '
      ELSE IF (N.LE.0) THEN
         WRITE(7,*) 'NDIM = ',NDIM
         WRITE(7,*) ' N = ',N
         WRITE(7,*) ' N <= 0'
         WRITE(7,*) 'LINE = ',LINE
         STOP ' FELDöBERSCHREITUNG ! '
      END IF
      RETURN
      END
!-----------------------------------------------------------------------
!     ROUTINEN ZUM LESEN VON DBF-FORMAT von G.R.Schiffler April 2000
!-----------------------------------------------------------------------
      SUBROUTINE READ_DBF_REAL (DBFILE,LU,C11ID,LI,ZAHL,LC,NVAL)
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
      CHARACTER*255 DBFILE
      REAL ZAHL(*)
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
      CHARACTER*8 FORM3
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM),C0
      CHARACTER*32767 C32KB
      INTEGER*2 NFELD,NF
      INTEGER*2 NBREC
      INTEGER*2 IA(NFDIM),IE(NFDIM)
      INTEGER*1 IC(NFDIM),LN(NFDIM),I0
      LOGICAL EXDBF
!     IA = Feldanfang im String C32KB (*32)
!     IE = Feldende im String C32KB (*32)
!     IC = Anzahl Dezimalstellen des Feldes

      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF) THEN
         WRITE(7,'(T2,A,A)') 'READ_DBF_REAL VON ',DBFILE
      ELSE
         WRITE(7,'(T2,A,A)') DBFILE,' EXISTIERT NICHT!'
         STOP
      END IF
!
      OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',FORM='UNFORMATTED')
!
      CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
!     WRITE(7,'(T2,A)') 'INITIALISIERUNG FERTIG!'
!
      I0=0
      WRITE(C0,'(A)') I0
!
      NFA=0
      NV=0
!
      DO 1 NF=1,NFELD
         IF (LI.EQ.LN(NF).AND.C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
    1 CONTINUE
!
      IF (NFA.EQ.0) GOTO 3
!
      L=IE(NFA)-IA(NFA)+1
      IF (L.GT.LC) THEN
         WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,' < ',L,' zu kurz!'
         L=LC
         IE(NFA)=L+IA(NFA)-1
      END IF
!
c     FORM1='(FX.0)'
c     FORM2='(FXX.0)'
c     FORM3='(FXXX.0)'
!
c     IF (L.GE.1.AND.L.LT.10) THEN
c        WRITE(FORM1(3:3),'(I1)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM1
c     ELSE IF (L.GE.10.AND.L.LT.100) THEN
c        WRITE(FORM2(3:4),'(I2)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM2
c     ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c        WRITE(FORM3(3:5),'(I3)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM3
c     END IF
!
      NV=0
!
      DO 2 NR=1,NREC
         READ(LU) C32KB(1:NBREC)
!        Typ=N (Number)
         IF (C32KB(1:1).NE.'*') THEN
            NV=NV+1
c           WRITE(7,*) IA(NFA),IE(NFA)
            DO IP=IA(NFA),IE(NFA)
               IF (C32KB(IP:IP).EQ.C0) C32KB(IP:IP)=' '
            END DO
            IEAKT=LEN_TRIM(C32KB(IA(NFA):IE(NFA))) + IA(NFA) -1
c           WRITE(7,*) IEAKT
c           WRITE(7,*) C32KB(IA(NFA):IEAKT)
            READ(C32KB(IA(NFA):IEAKT),*) ZAHL(NV)
c           WRITE(7,*) ZAHL(NV)
c           IF (L.GE.1.AND.L.LT.10) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM1) ZAHL(NV)
c           ELSE IF (L.GE.10.AND.L.LT.100) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM2) ZAHL(NV)
c           ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM3) ZAHL(NV)
c           END IF
         END IF
    2 CONTINUE
!
    3 NVAL=NV
!
      CLOSE(LU,STATUS='KEEP')
!
c     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE READ_DBF_DBLE (DBFILE,LU,C11ID,LI,DZAHL,LC,NVAL)
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
      CHARACTER*255 DBFILE
      DOUBLE PRECISION DZAHL(*)
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
      CHARACTER*8 FORM3
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM),C0
      CHARACTER*32767 C32KB
      INTEGER*2 NFELD,NF
      INTEGER*2 NBREC
      INTEGER*2 IA(NFDIM),IE(NFDIM)
      INTEGER*1 IC(NFDIM),LN(NFDIM),I0
      LOGICAL EXDBF
!     IA = Feldanfang im String C32KB (*32)
!     IE = Feldende im String C32KB (*32)
!     IC = Anzahl Dezimalstellen des Feldes
!
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF) THEN
         WRITE(7,'(T2,A,A)') 'READ_DBF_DBLE VON ',DBFILE
      ELSE
         WRITE(7,'(T2,A,A)') DBFILE,' EXISTIERT NICHT!'
         STOP
      END IF
!
      OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',FORM='UNFORMATTED')
!
      CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
!     WRITE(7,'(T2,A)') 'INITIALISIERUNG FERTIG!'
!
      I0=0
      WRITE(C0,'(A)') I0
!
      NFA=0
      NV=0
!
      DO 1 NF=1,NFELD
         IF (LI.EQ.LN(NF).AND.C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
    1 CONTINUE
!
      IF (NFA.EQ.0) GOTO 3  
!
      L=IE(NFA)-IA(NFA)+1
      IF (L.GT.LC) THEN
         WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,' < ',L,' zu kurz!'
         L=LC
         IE(NFA)=L+IA(NFA)-1
      END IF
!
c     FORM1='(FX.0)'
c     FORM2='(FXX.0)'
c     FORM3='(FXXX.0)'
!
c     IF (L.LT.10) THEN
c        WRITE(FORM1(3:3),'(I1)') L
c        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM1
c     ELSE IF (L.GE.10.AND.L.LT.100) THEN
c        WRITE(FORM2(3:4),'(I2)') L
c        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM2
c     ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c        WRITE(FORM3(3:5),'(I3)') L
c        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM3
c     END IF
!
      NV=0
!
      DO 2 NR=1,NREC
         READ(LU) C32KB(1:NBREC)
!        Typ=N (Double)
         IF (C32KB(1:1).NE.'*') THEN
            NV=NV+1
c           WRITE(7,*) IA(NFA),IE(NFA)
            DO IP=IA(NFA),IE(NFA)
               IF (C32KB(IP:IP).EQ.C0) C32KB(IP:IP)=' '
            END DO
            IEAKT=LEN_TRIM(C32KB(IA(NFA):IE(NFA))) + IA(NFA) -1
c           WRITE(7,*) IEAKT
c           WRITE(7,*) C32KB(IA(NFA):IEAKT)
            READ(C32KB(IA(NFA):IEAKT),*) DZAHL(NV)
c           WRITE(7,*) DZAHL(NV)
c           IF (L.GE.1.AND.L.LT.10) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM1) DZAHL(NV)
c           ELSE IF (L.GE.10.AND.L.LT.100) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM2) DZAHL(NV)
c           ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM3) DZAHL(NV)
c           END IF
         END IF
    2 CONTINUE
!
    3 NVAL=NV
!
      CLOSE(LU,STATUS='KEEP')
!
c     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE READ_DBF_INTE (DBFILE,LU,C11ID,LI,NUM,LC,NVAL)
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
      CHARACTER*255 DBFILE
      INTEGER*4 NUM(*)
      CHARACTER*4 FORM1
      CHARACTER*5 FORM2
      CHARACTER*6 FORM3
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM),C0
      CHARACTER*32767 C32KB
      INTEGER*2 NFELD,NF
      INTEGER*2 NBREC
      INTEGER*2 IA(NFDIM),IE(NFDIM)
      INTEGER*1 IC(NFDIM),LN(NFDIM),I0
      LOGICAL EXDBF
!     IA = Feldanfang im String C32KB (*32)
!     IE = Feldende im String C32KB (*32)
!     IC = Anzahl Dezimalstellen des Feldes
!
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF) THEN
         WRITE(7,'(T2,A,A)') 'READ_DBF_INTE VON ',DBFILE
      ELSE
         WRITE(7,'(T2,A,A)') DBFILE,' EXISTIERT NICHT!'
         STOP
      END IF
!
      OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',FORM='UNFORMATTED')
!
      CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
!     WRITE(7,'(T2,A)') 'INITIALISIERUNG FERTIG!'
!
      I0=0
      WRITE(C0,'(A)') I0
!
      NFA=0
      NV=0
!
      DO 1 NF=1,NFELD
         IF (LI.EQ.LN(NF).AND.C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
    1 CONTINUE
!
      IF (NFA.EQ.0) GOTO 3  
!
!     WRITE(7,'(T2,A)') C11ID(1:LI)
!     WRITE(7,'(T2,A,I3)') ' NFA = ',NFA
!
      L=IE(NFA)-IA(NFA)+1
      IF (L.GT.LC) THEN
         WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,' < ',L,' zu kurz!'
         L=LC
         IE(NFA)=L+IA(NFA)-1
      END IF
!
c     FORM1='(IX)'
c     FORM2='(IXX)'
c     FORM3='(IXXX)'
!
c     IF (L.GE.1.AND.L.LT.10) THEN
c        WRITE(FORM1(3:3),'(I1)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM1
c     ELSE IF (L.GE.10.AND.L.LT.100) THEN
c        WRITE(FORM2(3:4),'(I2)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM2
c     ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c        WRITE(FORM3(3:5),'(I3)') L
!        WRITE(7,'(T2,A,A)') ' FORMAT = ',FORM3
c     END IF
!
c     WRITE(7,'(T2,A,I11)') 'NREC = ',NREC
c     WRITE(7,'(T2,2(A,I6))') 'VON ',IA(NFA),' BIS ',IE(NFA)
!
      NV=0
!
      DO 2 NR=1,NREC
         READ(LU) C32KB(1:NBREC)
!        Typ=N (Integer)
         IF (C32KB(1:1).NE.'*') THEN
            NV=NV+1
c           WRITE(7,*) IA(NFA),IE(NFA)
            DO IP=IA(NFA),IE(NFA)
               IF (C32KB(IP:IP).EQ.C0) C32KB(IP:IP)=' '
            END DO
            IEAKT=LEN_TRIM(C32KB(IA(NFA):IE(NFA))) + IA(NFA) -1
c           WRITE(7,*) IEAKT
c           WRITE(7,*) C32KB(IA(NFA):IEAKT)
            READ(C32KB(IA(NFA):IEAKT),*) NUM(NV)
c           WRITE(7,*) NUM(NV)
c           IF (L.GE.1.AND.L.LT.10) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM1) NUM(NV)
c           ELSE IF (L.GE.10.AND.L.LT.100) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM2) NUM(NV)
c           ELSE IF (L.GE.100.AND.L.LT.1000) THEN
c              READ(C32KB(IA(NFA):IE(NFA)),FORM3) NUM(NV)
c           END IF
         END IF
    2 CONTINUE
!
    3 NVAL=NV
!
      CLOSE(LU,STATUS='KEEP')
!
c     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE READ_DBF_CHAR (DBFILE,LU,C11ID,LI,CX,LC,NVAL)
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
      CHARACTER*255 DBFILE
      CHARACTER (LEN=*) :: CX(*)
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM)
      CHARACTER*32767 C32KB
      INTEGER*2 NFELD,NF
      INTEGER*2 NBREC
      INTEGER*2 IA(NFDIM),IE(NFDIM)
      INTEGER*1 IC(NFDIM),LN(NFDIM)
      LOGICAL EXDBF
!     IA = Feldanfang im String C32KB (*32)
!     IE = Feldende im String C32KB (*32)
!     IC = Anzahl Dezimalstellen des Feldes

      INQUIRE(FILE=DBFILE,EXIST=EXDBF)

      IF (EXDBF) THEN
         WRITE(7,'(T2,A,A)') 'READ_DBF_REAL VON ',DBFILE
      ELSE
         WRITE(7,'(T2,A,A)') DBFILE,' EXISTIERT NICHT!'
         STOP
      END IF
!
      OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',FORM='UNFORMATTED')
!
      CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
!     WRITE(7,'(T2,A)') 'INIDBF FERTIG!'
!
      NFA=0
      NV=0
!
      DO 1 NF=1,NFELD
         IF (LI.EQ.LN(NF).AND.C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
    1 CONTINUE
!
      IF (NFA.EQ.0) GOTO 3  
!
      L=IE(NFA)-IA(NFA)+1
!
      IF (C1TYPE(NFA).EQ.'D') THEN
         WRITE(7,'(T2,A)')'Datentyp D (DATE) als C (CHARACTER) gelesen!'
         IF (LC.GT.L) THEN
            WRITE(7,'(T2,4A,I5)')'Feld ',C11ID,' mit Laenge ',LC
            WRITE(7,'(T2,A,I5,A)')'wird nur mit Laenge ',L,' belegt!'
         END IF
      END IF
!
      IF (L.GT.LC) THEN
         WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,' < ',L,' zu kurz!'
         L=LC
         IE(NFA)=L+IA(NFA)-1
      END IF
!
      NV=0
!
      DO 2 NR=1,NREC
         READ(LU) C32KB(1:NBREC)
         IF (C32KB(1:1).NE.'*') THEN
            NV=NV+1
!           Typ=C
            CX(NV)(1:L)=C32KB(IA(NFA):IE(NFA))
         END IF
    2 CONTINUE
!
    3 NVAL=NV
!
      CLOSE(LU,STATUS='KEEP')
!
c     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
!     DBF-DATENBANK BEIM EINLESEN INITIALISIEREN
!     D.H. DATENBANK-HEADER IM DBASE-FORMAT LESEN
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
!     VEREINBARUNGEN DATEIKOPF
      CHARACTER*32 C32BYTE
      CHARACTER*12 C12RESMU
      CHARACTER*1 C1VALID,C1FLAG,C1KEYDB4,C1MDX1,C1LDID,C1ENDHDR
      CHARACTER*1 C1TFLAG,C1EFLAG,C1NULL,C1TEST
      INTEGER*1 I1ENDHDR,I1NULL,I1TEST,I1VALID,I1VERS,I1TFLAG,I1EFLAG
      INTEGER*1 I1RESMU(12),I1MDXFLAG,I1LDID
      CHARACTER*2 C2ZERO
      INTEGER*1 JJ,MM,TT
      INTEGER*2 NBHEAD,NBREC
      INTEGER*2 NFELD,NF
      INTEGER*4 NBFILE
!     NREC   = Anzahl der DatensÑtze (*4)
!     NBHEAD = Anzahl der Bytes im Dateikopf (*2)
!     NBREC  = Datensatzgrî· in Bytes (*2)
!     NFELD = Anzahl der Felder (Spalten) (*2)
!
!     VEREINBARUNGEN FELDBESCHREIBER
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM),C1MDX2(NFDIM),C2RES(NFDIM),C1IDW(NFDIM)
      INTEGER*1 IC(NFDIM),I1LF,LN(NFDIM)
      INTEGER*2 LF(NFDIM)
!     LF = FeldlÑnge (*2)
!     IC = Dezimalstellen (*1)
!     LN = L‰nge des Feldnamens
!
!     VEREINBARUNGEN DATEN-RECORDS
      INTEGER*2 IA(NFDIM),IE(NFDIM)
!
      I1NULL=0
!
!     WRITE(*,*) I1NULL
!
!     Byte 0
      READ(LU) I1VERS
!     WRITE(*,*) I1VERS
!
!     Byte 1-3
      READ(LU) JJ
      READ(LU) MM
      READ(LU) TT
!     WRITE(*,*) JJ,MM,TT
!
!     Byte 4-7
      READ(LU) NREC
!     WRITE(*,*) NREC
!     WRITE(7,'(T2,A,I11)') 'INIDBF:    NREC=',NREC
!
!     Byte 8-9
      READ(LU) NBHEAD
!     WRITE(*,*) NBHEAD
!     WRITE(7,'(T2,A,I11)') 'INIDBF:  NBHEAD=',NBHEAD
      NFELD=NBHEAD-1-32
      NFELD=NFELD/32
!     WRITE(7,'(T2,A,I11)') 'INIDBF:   NFELD=',NFELD
!
!     Byte 10-11
      READ(LU) NBREC
!     WRITE(*,*) NBREC
!     WRITE(7,'(T2,A,I11)') 'INIDBF:   NBREC=',NBREC
!
      NBFILE=NBHEAD+NREC*NBREC+1
      WRITE(7,'(T2,A,I12)') 'INIDBF:  NBFILE=',NBFILE
!
!     Byte 12-13
      READ(LU) I1NULL
!     WRITE(*,*) I1NULL
      READ(LU) I1NULL
!     WRITE(*,*) I1NULL
!
!     Byte 14 TRANSACTION FLAG
      READ(LU) I1TFLAG
!     WRITE(*,*) I1TFLAG
!
!     Byte 15 ENCRYPTION FLAG
      READ(LU) I1EFLAG
!     WRITE(*,*) I1EFLAG
!
!     Bytes 16-27 RESERVED FOR MULTI USER
      READ(LU) (I1RESMU(I),I=1,12)
!     WRITE(*,*) (I1RESMU(I),I=1,12) 
!
!     Byte 28 MDX FILE FLAG
      READ(LU) I1MDXFLAG
!     WRITE(*,*) I1MDXFLAG
!
!     Byte 29 LANGUAGE DRIVER ID
      READ(LU) I1LDID
!     WRITE(*,*) I1LDID
!
!     Bytes 30-31
      READ(LU) I1NULL
!     WRITE(*,*) I1NULL
      READ(LU) I1NULL
!     WRITE(*,*) I1NULL
!
      IA(1)=2
!
      DO 1 NF=1,NFELD
!        WRITE(*,*) NF
c        WRITE(7,'(T2,A,I11)') 'INIDBF:      NF=',NF
         READ(LU) C32BYTE
!        WRITE(*,*) C32BYTE
c        WRITE(7,'(T2,2A)') 'INIDBF: C32BYTE=',C32BYTE
         C11NAME(NF)=C32BYTE(1:11)
!        WRITE(*,*) C11NAME(NF)
         WRITE(7,'(T2,A,A)') 'C11NAME = ',C11NAME(NF)
         LN(NF)=0
         DO IP=1,11
            C1TEST=C11NAME(NF)(IP:IP)
!           WRITE(*,*) 'C1TEST = ',C1TEST
            I1TEST=ICHAR(C1TEST)
!           WRITE(*,*) 'I1TEST = ',I1TEST
            C1TEST=CHAR(I1TEST)
!           WRITE(*,*) 'C1TEST = ',C1TEST
            IF (I1TEST.EQ.0.AND.LN(NF).EQ.0) LN(NF)=IP-1
         END DO
!        WRITE(7,'(T2,A,I2)') 'LN = ',LN(NF)
         C1TYPE(NF)=C32BYTE(12:12)
!        WRITE(7,'(T2,A)') C1TYPE(NF)
         I1LF = ICHAR(C32BYTE(17:17))
         IF (I1LF.GE.0) LF(NF)=I1LF
         IF (I1LF.LT.0) LF(NF)=256+I1LF
         IF (LF(NF).GT.255.OR.LF(NF).LE.0) THEN
            WRITE(7,'(T2,A,I11)') 'INIDBF:      NF=',NF
            WRITE(7,'(T2,A,A)') 'C11NAME = ',C11NAME(NF)
            WRITE(7,'(T2,A,I11)') '     FeldlÑnge:',LF(NF)
            STOP 'Feldl‰nge falsch!'
         END IF
!        Berechnung von Feldanfang- und Feldende-Zeigern
         IE(NF)=IA(NF)+LF(NF)-1
         IF (NF.LT.NFELD) IA(NF+1)=IE(NF)+1
!        WRITE(7,'(T2,2(A,I6))') 'IA =',IA(NF),'  IE =',IE(NF)
         IC(NF) = ICHAR(C32BYTE(18:18))
!        WRITE(7,'(T2,A,I11)') 'Dezimalstellen:',IC(NF)
         C2RES(NF)=C32BYTE(19:20)
!        WRITE(7,'(T2,2A)') ' C2RES(NF)=',C2RES(NF)
         C1IDW(NF)=C32BYTE(21:21)
!        WRITE(7,'(T2,2A)') ' C1IDW(NF)=',C1IDW(NF)
         C1MDX2(NF)=C32BYTE(32:32)
!        WRITE(7,'(T2,2A)') 'C1MDX2(NF)=',C1MDX2(NF)
    1 CONTINUE
!
!     Markierung fÅr Feldende (Dateikopf)
      READ(LU) C1ENDHDR
!     WRITE(7,'(T2,2A)') 'C1ENDHDR=',C1ENDHDR
      I1ENDHDR = ICHAR(C1ENDHDR)
!     WRITE(7,'(T2,A,I4)') 'I1ENDHDR=',I1ENDHDR
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_DBF_CHAR (DBFILE,LU,C11ID,LI,CX,LC,NVAL,MOD)
!
!     C11ID = maximal 11-stelliger Feldname
!        LI = reale LÑnge des Feldnamens (max=11)
!        CX = 1D-Feld fÅr Feldvariable (hier Character-String)
!        LC = Anzahl der Stellen in der Feldvariable (hier Zeichenzahl)
!      NVAL = Anzahl der gÅltigen Werte (Records)
!       MOD = Modus wie folgt:
!       MOD = 'UPD' => Feldinhalt wird aktualisiert (update),
!                      wobei Feldeigenschaften erhalten bleiben;
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'ADD' => Feld wird zur Datenbank hinzugefÅgt (add).
!                      Voraussetzung: Datenbank muss schon existieren!
!       MOD = 'DEL' => Feld wird gelîscht (delete),
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'NEW' => Feld wird in neue Datenbank geschrieben (new).
!
!     Die Ausgabe erfolgt zunÑchst auf temporÑre Datenbank TEMP.DBF,
!     damit Lese- und SchreibvorgÑnge sich nicht stîren.
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
      CHARACTER (LEN=3) MOD
      CHARACTER*255 DBFILE
      CHARACTER (LEN=*) :: CX(*)
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM),C11NAMT(NFDIM),C11NULL
      CHARACTER*1 C1TYPE(NFDIM),C1TYPT(NFDIM)
      CHARACTER*32767 C32KB,C32KBT
      INTEGER*2 NFELD,NF,NFELDT,NFT,NFDEL
      INTEGER*2 NBREC,NBRECT
      INTEGER*2 IA(NFDIM),IE(NFDIM),NBLE,NBRA
      INTEGER*1 IC(NFDIM),LN(NFDIM)
!     IA  = Feldanfang im String C32KB (*32)
!     IE  = Feldende im String C32KB (*32)
!     IC  = Anzahl Dezimalstellen des Feldes
      INTEGER*2 IAT(NFDIM),IET(NFDIM)
      INTEGER*1 ICT(NFDIM)
!     IAT = Feldanfang im String C32KBT (*32)
!     IET = Feldende im String C32KBT (*32)
!     ICT = Anzahl Dezimalstellen des Feldes
      LOGICAL EXDBF
      INTEGER*1 I1ENDDBF
      CHARACTER*1 C1ENDDBF
      INTEGER*4 NBFILT
      CHARACTER*1 C1BYTE
      INTEGER*1 I1NULL
!
      WRITE(7,*) 'WRITE_DBF_CHAR ',DBFILE
      WRITE(7,*) '            LU ',LU
      WRITE(7,*) '         C11ID ',C11ID
      WRITE(7,*) '            LI ',LI
      WRITE(7,*) '            LC ',LC
      WRITE(7,*) '          NVAL ',NVAL
      WRITE(7,*) '           MOD ',MOD
!
      I1NULL=0
      WRITE(C11NULL,'(11A)') (I1NULL,I=1,11)
!
      LUT=LU+10
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
         WRITE(7,'(T2,A,A)') 'LINE 1166   MOD ',MOD
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
C        OPEN(LU,FILE=DBFILE,STATUS='OLD',FORM='BINARY')
!
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
C        OPEN(LUT,FILE='TEMP.DBF',STATUS='UNKNOWN',FORM='BINARY')
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
         WRITE(7,'(T2,A,A)') 'LINE 1172   MOD ',MOD
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
         WRITE(7,'(T2,A,A)') 'LINE 1178   MOD ',MOD
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'NEW') THEN
         WRITE(7,'(T2,A,A)') 'LINE 1184   MOD ',MOD
         MOD='ADD'
         WRITE(7,'(T2,A,A)') 'LINE 1186   MOD ',MOD
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
         WRITE(7,'(T2,A,A)') 'LINE 1192   MOD ',MOD
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='NEW',
     &        FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE
         STOP ' 1040: ES LIEGT KEINE SINNVOLLE KOMBINATION VOR!'
      END IF
!
!
      IF (EXDBF) THEN
!
         CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
         CALL CHKMOD (NFELD,C11NAME,NREC,C11ID,LI,NVAL,MOD)
!
      END IF
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
!        Feld aktualisieren, signifikante Stellen bleiben gleich
         NFELDT=NFELD
         NRECT=NREC
         NBRECT=NBREC
         DO NFT=1,NFELDT
            NF=NFT
            C11NAMT(NFT)=C11NAME(NF)
            C1TYPT(NFT)=C1TYPE(NF)
            IAT(NFT)=IA(NF)
            IET(NFT)=IE(NF)
            ICT(NFT)=IC(NF)
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
!        Feld hinzufÅgen
         NFELDT=NFELD+1
         NRECT=NREC
         NBRECT=NBREC+LC
         DO NFT=1,NFELDT
            IF (NFT.LT.NFELDT) THEN
!              vorhandene Felder...
               NF=NFT
               C11NAMT(NFT)=C11NAME(NF)
               C1TYPT(NFT)=C1TYPE(NF)
               IAT(NFT)=IA(NF)
               IET(NFT)=IE(NF)
               ICT(NFT)=IC(NF)
            ELSE IF (NFT.EQ.NFELDT) THEN
!              neues Feld hinten anhÑngen...
               NF=NFT-1
               C11NAMT(NFT)=C11NULL
               C11NAMT(NFT)(1:LI)=C11ID
!              Datentyp = C, weil String
               C1TYPT(NFT)='C'
               IAT(NFT)=IE(NF)+1
               IET(NFT)=IAT(NFT)+LC-1
!              Anzahl Dezimalstellen=0, weil String
               ICT(NFT)=0
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
!        Feld lîschen
         NFELDT=NFELD-1
         NRECT=NREC
         NBRECT=NBREC-LC
         NFT=0
         DO NF=1,NFELD
            IF (C11NAME(NF)(1:LI).NE.C11ID(1:LI)) THEN
!              verbleibende Felder...
               NFT=NFT+1
               IF (NFT.EQ.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)
                  IET(NFT)=IE(NF)
                  ICT(NFT)=IC(NF)
               ELSE IF (NFT.LT.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)-LC
                  IET(NFT)=IE(NF)-LC
                  ICT(NFT)=IC(NF)
               END IF
            ELSE
               NFDEL=NF
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
!        erstes Feld in neuer Datenbank anlegen
         NFELDT=1
         NREC=NVAL
         NRECT=NREC
         NBRECT=LC+1
!        neues Feld anlegen...
         C11NAMT(1)=C11NULL
         C11NAMT(1)(1:LI)=C11ID
!        Datentyp = C, weil String
         C1TYPT(1)='C'
         IAT(1)=2
         IET(1)=LC+1
!        Anzahl Dezimalstellen=0, weil String
         ICT(1)=0
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
!        hier erstes Feld der neuen Datenbank schreiben ...

         DO NV=1,NVAL
            C32KBT(1:1)=' '
            C32KBT(2:NBRECT)=CX(NV)
!           WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
            WRITE(LUT) C32KBT(1:NBRECT)
         END DO
!
      END IF
!
      IF (EXDBF.AND.MOD.NE.'NEW') THEN
!
         NV=0
!
         DO 2 NR=1,NREC
!           alten Record lesen
            READ(LU) C32KB(1:NBREC)
!           WRITE(7,'(T2,A,A)') 'C32KB =',C32KB(1:NBREC)
            IF (C32KB(1:1).NE.'*') THEN
               NV=NV+1
!              Typ=C
               IF (MOD.EQ.'UPD') THEN
!                 aktuelles Feld suchen...
                  DO NF=1,NFELD
                     IF (C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
                  END DO
!                 aktuelle FeldlÑnge bestimmem und LC testen...
                  L=IE(NFA)-IA(NFA)+1
                  IF (L.GT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_CHAR:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist zu kurz!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,'<',L,' !'
                  ELSE IF (L.LT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_CHAR:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist lÑnger!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),L,'<',LC,' !'
                  END IF
!                 aktualisierten Feldinhalt in Record schreiben...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  C32KBT(IA(NFA):IE(NFA))=CX(NV)(1:L)
               ELSE IF (MOD.EQ.'ADD') THEN
!                 neuen Feldinhalt an Record anhÑngen...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  C32KBT((NBREC+1):NBRECT)=CX(NV)
               ELSE IF (MOD.EQ.'DEL') THEN
!                 Feldinhalt aus Record lîschen...
                  IF (NFDEL.EQ.1) THEN
                     NBRA=IA(NFDEL+1)
                     C32KBT(1:NBRECT)=C32KB(NBRA:NBREC)
                  ELSE IF (NFDEL.EQ.NFELD) THEN
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBRECT)=C32KB(1:NBLE)
                  ELSE
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBLE)=C32KB(1:NBLE)
                     NBRA=IA(NFDEL+1)
                     C32KBT((NBLE+1):NBRECT)=C32KB(NBRA:NBREC)
                  END IF
               END IF
!              Ausgabe auf Datei TEMP.DBF
!              WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
               WRITE(LUT) C32KBT(1:NBRECT)
            ELSE
               WRITE (7,'(T2,A)') 'SUBROUTINE WRITE_DBF_CHAR'
               WRITE (7,'(T2,A)') 'PROGRAMMERWEITERUNG ERFORDERLICH'
               WRITE (7,'(T2,A)') 'FöR MIT "*" GELôSCHTE RECORDS!'
               STOP
            END IF
    2    CONTINUE
!
         IF (NV.NE.NVAL) THEN
            WRITE(7,'(T2,A)') DBFILE
            WRITE(7,'(T2,A)') C11NAME(NFA)
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
            WRITE(7,'(T2,A,I9)') 'FELDLAENGE NVAL = ',NVAL
            WRITE(7,'(T2,A,I9)') 'AKTIVE RECORDS  = ',NV
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
         END IF
!
      END IF
!
      IF (MOD.NE.'   ') THEN
!
!        Markierung fÅr Dateiende
         I1ENDDBF=  26
         WRITE(C1ENDDBF,'(A)') I1ENDDBF
         WRITE(LUT) C1ENDDBF
!
!        KOPIEREN ALLER BYTES VON TEMP.DBF (LUT) AUF DBFILE(LU)
!
c         REWIND(LU)
c         REWIND(LUT)
!
c         DO NB=1,NBFILT
c            READ(LUT) C1BYTE
c            WRITE(LU) C1BYTE
c         END DO
!
      END IF
!
      CLOSE(LUT,STATUS='KEEP')
!     WRITE(7,'(T2,A)') 'TEMP.DBF GESCHLOSSEN!'
!
      CLOSE(LU,STATUS='KEEP')
!     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      DO IP=4,255
         IF (DBFILE((IP-3):(IP-3)).EQ.'.') LDBFILE=IP
      END DO
      CALL SYSTEM ("ECHO OFF")
      CALL DOSCOPY ('TEMP.DBF',8,DBFILE,LDBFILE)
      CALL SYSTEM ("DEL TEMP.DBF")
      CALL SYSTEM ("ECHO ON")
c     CALL SYSTEM ("DEL TEMP.DBF > screen")
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_DBF_INTE (DBFILE,LU,C11ID,LI,NUM,LC,NVAL,MOD)
!
!     C11ID = maximal 11-stelliger Feldname
!        LI = reale LÑnge des Feldnamens (max=11)
!       NUM = 1D-Feld fÅr Feldvariable (hier Integer-Nummer)
!        LC = Anzahl der Stellen in der Feldvariable (hier Zeichenzahl)
!      NVAL = Anzahl der gÅltigen Werte (Records)
!       MOD = Modus wie folgt:
!       MOD = 'UPD' => Feldinhalt wird aktualisiert (update),
!                      wobei Feldeigenschaften erhalten bleiben;
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'ADD' => Feld wird zur Datenbank hinzugefÅgt (add).
!                      Voraussetzung: Datenbank muss schon existieren!
!       MOD = 'DEL' => Feld wird gelîscht (delete),
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'NEW' => Feld wird in neue Datenbank geschrieben (new).
!
!     Die Ausgabe erfolgt zunÑchst auf temporÑre Datenbank TEMP.DBF,
!     damit Lese- und SchreibvorgÑnge sich nicht stîren.
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
      CHARACTER*3 MOD
      CHARACTER*255 DBFILE
      INTEGER*4 NUM(*)
      CHARACTER*4 FORM1
      CHARACTER*5 FORM2
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM),C11NAMT(NFDIM),C11NULL
      CHARACTER*1 C1TYPE(NFDIM),C1TYPT(NFDIM)
      CHARACTER*32767 C32KB,C32KBT
      INTEGER*2 NFELD,NF,NFELDT,NFT,NFDEL
      INTEGER*2 NBREC,NBRECT
      INTEGER*2 IA(NFDIM),IE(NFDIM),NBLE,NBRA
      INTEGER*1 IC(NFDIM),LN(NFDIM)
!     IA  = Feldanfang im String C32KB (*32)
!     IE  = Feldende im String C32KB (*32)
!     IC  = Anzahl Dezimalstellen des Feldes
      INTEGER*2 IAT(NFDIM),IET(NFDIM)
      INTEGER*1 ICT(NFDIM)
!     IAT = Feldanfang im String C32KBT (*32)
!     IET = Feldende im String C32KBT (*32)
!     ICT = Anzahl Dezimalstellen des Feldes
      LOGICAL EXDBF
      INTEGER*1 I1ENDDBF
      CHARACTER*1 C1ENDDBF
      INTEGER*4 NBFILT
      CHARACTER*1 C1BYTE
      INTEGER*1 I1NULL
!
c     WRITE(7,'(T2,A,A)') 'WRITE_DBF_INTE ',DBFILE
!
      FORM1='(IX)'
      FORM2='(IXX)'
!
      IF (LC.GE.1.AND.LC.LT.10) THEN
         WRITE(FORM1(3:3),'(I1)') LC
      ELSE IF (LC.GE.10.AND.LC.LT.100) THEN
         WRITE(FORM2(3:4),'(I2)') LC
      END IF
!
      I1NULL=0
      WRITE(C11NULL,'(11A)') (I1NULL,I=1,11)
!
      LUT=LU+10
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'NEW') THEN
         MOD='ADD'
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='NEW',
     &        FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE
         STOP ' 1344: ES LIEGT KEINE SINNVOLLE KOMBINATION VOR!'
      END IF
!
!
      IF (EXDBF) THEN
!
         CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
         CALL CHKMOD (NFELD,C11NAME,NREC,C11ID,LI,NVAL,MOD)
!
      END IF
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
!        Feld aktualisieren
         NFELDT=NFELD
         NRECT=NREC
         NBRECT=NBREC
         DO NFT=1,NFELDT
            NF=NFT
            C11NAMT(NFT)=C11NAME(NF)
            C1TYPT(NFT)=C1TYPE(NF)
            IAT(NFT)=IA(NF)
            IET(NFT)=IE(NF)
            ICT(NFT)=IC(NF)
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
!        Feld hinzufÅgen
         NFELDT=NFELD+1
         NRECT=NREC
         NBRECT=NBREC+LC
         DO NFT=1,NFELDT
            IF (NFT.LT.NFELDT) THEN
!              vorhandene Felder...
               NF=NFT
               C11NAMT(NFT)=C11NAME(NF)
               C1TYPT(NFT)=C1TYPE(NF)
               IAT(NFT)=IA(NF)
               IET(NFT)=IE(NF)
               ICT(NFT)=IC(NF)
            ELSE IF (NFT.EQ.NFELDT) THEN
!              neues Feld hinten anhÑngen...
               NF=NFT-1
               C11NAMT(NFT)=C11NULL
               C11NAMT(NFT)(1:LI)=C11ID
!              Datentyp = N, weil Integer
               C1TYPT(NFT)='N'
               IAT(NFT)=IE(NF)+1
               IET(NFT)=IAT(NFT)+LC-1
!              Anzahl Dezimalstellen=0, weil Integer
               ICT(NFT)=0
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
!        Feld lîschen
         NFELDT=NFELD-1
         NRECT=NREC
         NBRECT=NBREC-LC
         NFT=0
         DO NF=1,NFELD
            IF (C11NAME(NF)(1:LI).NE.C11ID(1:LI)) THEN
!              verbleibende Felder...
               NFT=NFT+1
               IF (NFT.EQ.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)
                  IET(NFT)=IE(NF)
                  ICT(NFT)=IC(NF)
               ELSE IF (NFT.LT.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)-LC
                  IET(NFT)=IE(NF)-LC
                  ICT(NFT)=IC(NF)
               END IF
            ELSE
               NFDEL=NF
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
!        erstes Feld in neuer Datenbank anlegen
         NFELDT=1
         NREC=NVAL
         NRECT=NREC
         NBRECT=LC+1
!        neues Feld anlegen...
         C11NAMT(1)=C11NULL
         C11NAMT(1)(1:LI)=C11ID
!        Datentyp = N, weil Integer (Numerisch)
         C1TYPT(1)='N'
         IAT(1)=2
         IET(1)=LC+1
!        Anzahl Dezimalstellen=0, weil Integer
         ICT(1)=0
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
!        hier erstes Feld der neuen Datenbank schreiben ...

         DO NV=1,NVAL
            C32KBT(1:1)=' '
            IF (LC.GE.1.AND.LC.LT.10) THEN
               WRITE(C32KBT(2:NBRECT),FORM1) NUM(NV)
            ELSE IF (LC.GE.10.AND.LC.LT.100) THEN
               WRITE(C32KBT(2:NBRECT),FORM2) NUM(NV)
            END IF
!           WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
            WRITE(LUT) C32KBT(1:NBRECT)
         END DO
!
      END IF
!
      IF (EXDBF.AND.MOD.NE.'NEW') THEN
!
         NV=0
!
         DO 2 NR=1,NREC
!           alten Record lesen
            READ(LU) C32KB(1:NBREC)
!           WRITE(7,'(T2,A,A)') 'C32KB =',C32KB(1:NBREC)
            IF (C32KB(1:1).NE.'*') THEN
               NV=NV+1
               IF (MOD.EQ.'UPD') THEN
!                 aktuelles Feld suchen...
                  DO NF=1,NFELD
                     IF (C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
                  END DO
!                 aktuelle FeldlÑnge bestimmem und LC testen...
                  L=IE(NFA)-IA(NFA)+1
                  IF (L.GT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_INTE:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist zu kurz!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,'<',L,' !'
                  ELSE IF (L.LT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_INTE:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist lÑnger!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),L,'<',LC,' !'
                  END IF
!                 aktualisierten Feldinhalt in Record schreiben...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (L.GE.1.AND.L.LT.10) THEN
                     WRITE(FORM1(3:3),'(I1)') L
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM1) NUM(NV)
                  ELSE IF (L.GE.10.AND.L.LT.100) THEN
                     WRITE(FORM2(3:4),'(I2)') L
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM2) NUM(NV)
                  END IF
               ELSE IF (MOD.EQ.'ADD') THEN
!                 neuen Feldinhalt an Record anhÑngen...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (LC.GE.1.AND.LC.LT.10) THEN
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM1) NUM(NV)
                  ELSE IF (LC.GE.10.AND.LC.LT.100) THEN
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM2) NUM(NV)
                  END IF
               ELSE IF (MOD.EQ.'DEL') THEN
!                 Feldinhalt aus Record lîschen...
                  IF (NFDEL.EQ.1) THEN
                     NBRA=IA(NFDEL+1)
                     C32KBT(1:NBRECT)=C32KB(NBRA:NBREC)
                  ELSE IF (NFDEL.EQ.NFELD) THEN
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBRECT)=C32KB(1:NBLE)
                  ELSE
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBLE)=C32KB(1:NBLE)
                     NBRA=IA(NFDEL+1)
                     C32KBT((NBLE+1):NBRECT)=C32KB(NBRA:NBREC)
                  END IF
               END IF
!              Ausgabe auf Datei TEMP.DBF
!              WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
               WRITE(LUT) C32KBT(1:NBRECT)
            ELSE
               WRITE (7,'(T2,A)') 'SUBROUTINE WRITE_DBF_INTE'
               WRITE (7,'(T2,A)') 'PROGRAMMERWEITERUNG ERFORDERLICH'
               WRITE (7,'(T2,A)') 'FöR MIT "*" GELôSCHTE RECORDS!'
               STOP
            END IF
    2    CONTINUE
!
         IF (NV.NE.NVAL) THEN
            WRITE(7,'(T2,A)') DBFILE
            WRITE(7,'(T2,A)') C11NAME(NFA)
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
            WRITE(7,'(T2,A,I9)') 'FELDLAENGE NVAL = ',NVAL
            WRITE(7,'(T2,A,I9)') 'AKTIVE RECORDS  = ',NV
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
         END IF
!
      END IF
!
      IF (MOD.NE.'   ') THEN
!
!        Markierung fÅr Dateiende
         I1ENDDBF=  26
         WRITE(C1ENDDBF,'(A)') I1ENDDBF
         WRITE(LUT) C1ENDDBF
!
!        KOPIEREN ALLER BYTES VON TEMP.DBF (LUT) AUF DBFILE(LU)
!
c         REWIND(LU)
c         REWIND(LUT)
!
c         DO NB=1,NBFILT
c            READ(LUT) C1BYTE
c            WRITE(LU) C1BYTE
c         END DO
!
      END IF
!
      CLOSE(LUT,STATUS='KEEP')
!     WRITE(7,'(T2,A)') 'TEMP.DBF GESCHLOSSEN!'
!
      CLOSE(LU,STATUS='KEEP')
!     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      DO IP=4,255 
         IF (DBFILE((IP-3):(IP-3)).EQ.'.') LDBFILE=IP
      END DO
!
      CALL SYSTEM ("ECHO OFF")
      CALL DOSCOPY ('TEMP.DBF',8,DBFILE,LDBFILE)
      CALL SYSTEM ("DEL TEMP.DBF")
      CALL SYSTEM ("ECHO ON")
c     CALL SYSTEM ("DEL TEMP.DBF > screen")
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_DBF_REAL (DBFILE,LU,C11ID,LI,ZAHL,LC,LD,NVAL,MOD)
!
!     C11ID = maximal 11-stelliger Feldname
!        LI = reale LÑnge des Feldnamens (max=11)
!      ZAHL = 1D-Feld fÅr Feldvariable
!        LC = Anzahl der Stellen in der Feldvariable
!        LD = Anzahl der Dezimalstellen
!      NVAL = Anzahl der gÅltigen Werte (Records)
!       MOD = Modus wie folgt:
!       MOD = 'UPD' => Feldinhalt wird aktualisiert (update),
!                      wobei Feldeigenschaften erhalten bleiben;
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'ADD' => Feld wird zur Datenbank hinzugefÅgt (add).
!                      Voraussetzung: Datenbank muss schon existieren!
!       MOD = 'DEL' => Feld wird gelîscht (delete),
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'NEW' => Feld wird in neue Datenbank geschrieben (new).
!
!     Die Ausgabe erfolgt zunÑchst auf temporÑre Datenbank TEMP.DBF,
!     damit Lese- und SchreibvorgÑnge sich nicht stîren.
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
      CHARACTER*3 MOD
      CHARACTER*255 DBFILE
      REAL ZAHL(*)
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
      CHARACTER*8 FORM3
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM),C11NAMT(NFDIM),C11NULL
      CHARACTER*1 C1TYPE(NFDIM),C1TYPT(NFDIM)
      CHARACTER*32767 C32KB,C32KBT
      INTEGER*2 NFELD,NF,NFELDT,NFT,NFDEL
      INTEGER*2 NBREC,NBRECT
      INTEGER*2 IA(NFDIM),IE(NFDIM),NBLE,NBRA
      INTEGER*1 IC(NFDIM),LN(NFDIM)
!     IA  = Feldanfang im String C32KB (*32)
!     IE  = Feldende im String C32KB (*32)
!     IC  = Anzahl Dezimalstellen des Feldes
      INTEGER*2 IAT(NFDIM),IET(NFDIM)
      INTEGER*1 ICT(NFDIM)
!     IAT = Feldanfang im String C32KBT (*32)
!     IET = Feldende im String C32KBT (*32)
!     ICT = Anzahl Dezimalstellen des Feldes
      LOGICAL EXDBF
      INTEGER*1 I1ENDDBF
      CHARACTER*1 C1ENDDBF
      INTEGER*4 NBFILT
      CHARACTER*1 C1BYTE
      INTEGER*1 I1NULL
!
      WRITE(7,'(T2,A,A)') 'WRITE_DBF_REAL ',DBFILE
      WRITE(7,'(T2,A,A)') '           MOD ',MOD
!
      FORM1='(FX.Y)'
      FORM2='(FXX.Y)'
      FORM3='(FXX.YY)'
!
      IF (LC.GE.1.AND.LC.LT.10.AND.LD.GE.1.AND.LD.LT.10) THEN
         WRITE(FORM1(3:3),'(I1)') LC
         WRITE(FORM1(5:5),'(I1)') LD
      ELSE IF (LC.GE.10.AND.LC.LT.100.AND.LD.GE.1.AND.LD.LT.10) THEN
         WRITE(FORM2(3:4),'(I2)') LC
         WRITE(FORM2(6:6),'(I1)') LD
      ELSE IF (LC.GE.10.AND.LC.LT.100.AND.LD.GE.10.AND.LD.LT.100) THEN
         WRITE(FORM3(3:4),'(I2)') LC
         WRITE(FORM3(6:7),'(I2)') LD
      END IF
!
      I1NULL=0
      WRITE(C11NULL,'(11A)') (I1NULL,I=1,11)
!
      LUT=LU+10
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'NEW') THEN
         MOD='ADD'
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='NEW',
     &        FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE
         STOP ' 1669: ES LIEGT KEINE SINNVOLLE KOMBINATION VOR!'
      END IF
!
!
      IF (EXDBF) THEN
!
         CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
         CALL CHKMOD (NFELD,C11NAME,NREC,C11ID,LI,NVAL,MOD)
!
      END IF
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
!        Feld aktualisieren
         NFELDT=NFELD
         NRECT=NREC
         NBRECT=NBREC
         DO NFT=1,NFELDT
            NF=NFT
            C11NAMT(NFT)=C11NAME(NF)
            C1TYPT(NFT)=C1TYPE(NF)
            IAT(NFT)=IA(NF)
            IET(NFT)=IE(NF)
            ICT(NFT)=IC(NF)
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
!        Feld hinzufÅgen
         NFELDT=NFELD+1
         NRECT=NREC
         NBRECT=NBREC+LC
         DO NFT=1,NFELDT
            IF (NFT.LT.NFELDT) THEN
!              vorhandene Felder...
               NF=NFT
               C11NAMT(NFT)=C11NAME(NF)
               C1TYPT(NFT)=C1TYPE(NF)
               IAT(NFT)=IA(NF)
               IET(NFT)=IE(NF)
               ICT(NFT)=IC(NF)
            ELSE IF (NFT.EQ.NFELDT) THEN
!              neues Feld hinten anhÑngen...
               NF=NFT-1
               C11NAMT(NFT)=C11NULL
               C11NAMT(NFT)(1:LI)=C11ID
!              Datentyp = N, weil Real (Numerisch)
               C1TYPT(NFT)='N'
               IAT(NFT)=IE(NF)+1
               IET(NFT)=IAT(NFT)+LC-1
!              Anzahl Dezimalstellen=LD
               ICT(NFT)=LD
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
!        Feld lîschen
         NFELDT=NFELD-1
         NRECT=NREC
         NBRECT=NBREC-LC
         NFT=0
         DO NF=1,NFELD
            IF (C11NAME(NF)(1:LI).NE.C11ID(1:LI)) THEN
!              verbleibende Felder...
               NFT=NFT+1
               IF (NFT.EQ.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)
                  IET(NFT)=IE(NF)
                  ICT(NFT)=IC(NF)
               ELSE IF (NFT.LT.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)-LC
                  IET(NFT)=IE(NF)-LC
                  ICT(NFT)=IC(NF)
               END IF
            ELSE
               NFDEL=NF
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
!        erstes Feld in neuer Datenbank anlegen
         NFELDT=1
         NREC=NVAL
         NRECT=NREC
         NBRECT=LC+1
!        neues Feld anlegen...
         C11NAMT(1)=C11NULL
         C11NAMT(1)(1:LI)=C11ID
!        Datentyp = N, weil Real (Numerisch)
         C1TYPT(1)='N'
         IAT(1)=2
         IET(1)=LC+1
!        Anzahl Dezimalstellen=LD
         ICT(1)=LD
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
!        hier erstes Feld der neuen Datenbank schreiben ...

         DO NV=1,NVAL
           C32KBT(1:1)=' '
           IF (LC.GE.1.AND.LC.LT.10.AND.LD.GE.1.AND.LD.LT.10) THEN
              WRITE(C32KBT(2:NBRECT),FORM1) ZAHL(NV)
           ELSEIF(LC.GE.10.AND.LC.LT.100.AND.LD.GE.1.AND.LD.LT.10)THEN
              WRITE(C32KBT(2:NBRECT),FORM2) ZAHL(NV)
           ELSEIF(LC.GE.10.AND.LC.LT.100.AND.LD.GE.10.AND.LD.LT.100)THEN
              WRITE(C32KBT(2:NBRECT),FORM3) ZAHL(NV)
           END IF
!          WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
           WRITE(LUT) C32KBT(1:NBRECT)
         END DO
!
      END IF
!
      IF (EXDBF.AND.MOD.NE.'NEW') THEN
!
         NV=0
!
         DO 2 NR=1,NREC
!           alten Record lesen
            READ(LU) C32KB(1:NBREC)
!           WRITE(7,'(T2,A,A)') 'C32KB =',C32KB(1:NBREC)
            IF (C32KB(1:1).NE.'*') THEN
               NV=NV+1
               IF (MOD.EQ.'UPD') THEN
!                 aktuelles Feld suchen...
                  DO NF=1,NFELD
                     IF (C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
                  END DO
!                 aktuelle FeldlÑnge bestimmem und LC testen...
                  L=IE(NFA)-IA(NFA)+1
                  IF (L.GT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist zu kurz!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,'<',L,' !'
                  ELSE IF (L.LT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist lÑnger!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),L,'<',LC,' !'
                  END IF
!                 aktualisierten Feldinhalt in Record schreiben...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (L.GE.1.AND.L.LT.10.AND.
     &               LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM1(3:3),'(I1)') L
                     WRITE(FORM1(5:5),'(I1)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM1) ZAHL(NV)
                  ELSE IF (L.GE.10.AND.L.LT.100.AND.
     &                    LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM2(3:4),'(I2)') L
                     WRITE(FORM2(6:6),'(I1)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM2) ZAHL(NV)
                  ELSE IF (L.GE.10.AND.L.LT.100.AND.
     &                    LD.GE.10.AND.LD.LT.100) THEN
                     WRITE(FORM3(3:4),'(I2)') L
                     WRITE(FORM3(6:7),'(I2)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM3) ZAHL(NV)
                  END IF
               ELSE IF (MOD.EQ.'ADD') THEN
!                 neuen Feldinhalt an Record anhÑngen...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (LC.GE.1.AND.LC.LT.10.AND.
     &               LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM1(3:3),'(I1)') LC
                     WRITE(FORM1(5:5),'(I1)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM1) ZAHL(NV)
                  ELSE IF (LC.GE.10.AND.LC.LT.100.AND.
     &                    LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM2(3:4),'(I2)') LC
                     WRITE(FORM2(6:6),'(I1)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM2) ZAHL(NV)
                  ELSE IF (LC.GE.10.AND.LC.LT.100.AND.
     &                    LD.GE.10.AND.LD.LT.100) THEN
                     WRITE(FORM3(3:4),'(I2)') LC
                     WRITE(FORM3(6:7),'(I2)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM3) ZAHL(NV)
                  END IF
               ELSE IF (MOD.EQ.'DEL') THEN
!                 Feldinhalt aus Record lîschen...
                  IF (NFDEL.EQ.1) THEN
                     NBRA=IA(NFDEL+1)
                     C32KBT(1:NBRECT)=C32KB(NBRA:NBREC)
                  ELSE IF (NFDEL.EQ.NFELD) THEN
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBRECT)=C32KB(1:NBLE)
                  ELSE
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBLE)=C32KB(1:NBLE)
                     NBRA=IA(NFDEL+1)
                     C32KBT((NBLE+1):NBRECT)=C32KB(NBRA:NBREC)
                  END IF
               END IF
!              Ausgabe auf Datei TEMP.DBF
!              WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
               WRITE(LUT) C32KBT(1:NBRECT)
            ELSE
               WRITE (7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL'
               WRITE (7,'(T2,A)') 'PROGRAMMERWEITERUNG ERFORDERLICH'
               WRITE (7,'(T2,A)') 'FöR MIT "*" GELôSCHTE RECORDS!'
               STOP
            END IF
    2    CONTINUE
!
         IF (NV.NE.NVAL) THEN
            WRITE(7,'(T2,A)') DBFILE
            WRITE(7,'(T2,A)') C11NAME(NFA)
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
            WRITE(7,'(T2,A,I9)') 'FELDLAENGE NVAL = ',NVAL
            WRITE(7,'(T2,A,I9)') 'AKTIVE RECORDS  = ',NV
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
         END IF
!
      END IF
!
      IF (MOD.NE.'   ') THEN
!
!        Markierung fÅr Dateiende
         I1ENDDBF=  26
         WRITE(C1ENDDBF,'(A)') I1ENDDBF
         WRITE(LUT) C1ENDDBF
!
!        KOPIEREN ALLER BYTES VON TEMP.DBF (LUT) AUF DBFILE(LU)
!
c         REWIND(LU)
c         REWIND(LUT)
!
c         DO NB=1,NBFILT
c            READ(LUT) C1BYTE
c            WRITE(LU) C1BYTE
c         END DO
!
      END IF
!
      CLOSE(LUT,STATUS='KEEP')
!     WRITE(7,'(T2,A)') 'TEMP.DBF GESCHLOSSEN!'
!
      CLOSE(LU,STATUS='KEEP')
!     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      DO IP=4,255 
         IF (DBFILE((IP-3):(IP-3)).EQ.'.') LDBFILE=IP
      END DO
      CALL SYSTEM ("ECHO OFF")
      CALL DOSCOPY ('TEMP.DBF',8,DBFILE,LDBFILE)
      CALL SYSTEM ("DEL TEMP.DBF")
      CALL SYSTEM ("ECHO ON")
c     CALL SYSTEM ("DEL TEMP.DBF > screen")
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_DBF_DBLE (DBFILE,LU,C11ID,LI,ZAHL,LC,LD,NVAL,MOD)
!
!     C11ID = maximal 11-stelliger Feldname
!        LI = reale LÑnge des Feldnamens (max=11)
!      ZAHL = 1D-Feld fÅr Feldvariable
!        LC = Anzahl der Stellen in der Feldvariable
!        LD = Anzahl der Dezimalstellen
!      NVAL = Anzahl der gÅltigen Werte (Records)
!       MOD = Modus wie folgt:
!       MOD = 'UPD' => Feldinhalt wird aktualisiert (update),
!                      wobei Feldeigenschaften erhalten bleiben;
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'ADD' => Feld wird zur Datenbank hinzugefÅgt (add).
!                      Voraussetzung: Datenbank muss schon existieren!
!       MOD = 'DEL' => Feld wird gelîscht (delete),
!                      Voraussetzung: Feld muss schon existieren!
!       MOD = 'NEW' => Feld wird in neue Datenbank geschrieben (new).
!
!     Die Ausgabe erfolgt zunÑchst auf temporÑre Datenbank TEMP.DBF,
!     damit Lese- und SchreibvorgÑnge sich nicht stîren.
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
      CHARACTER*3 MOD
      CHARACTER*255 DBFILE
      DOUBLE PRECISION ZAHL(*)
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
      CHARACTER*8 FORM3
      CHARACTER (LEN=LI) C11ID
      CHARACTER*11 C11NAME(NFDIM),C11NAMT(NFDIM),C11NULL
      CHARACTER*1 C1TYPE(NFDIM),C1TYPT(NFDIM)
      CHARACTER*32767 C32KB,C32KBT
      INTEGER*2 NFELD,NF,NFELDT,NFT,NFDEL
      INTEGER*2 NBREC,NBRECT
      INTEGER*2 IA(NFDIM),IE(NFDIM),NBLE,NBRA
      INTEGER*1 IC(NFDIM),LN(NFDIM)
!     IA  = Feldanfang im String C32KB (*32)
!     IE  = Feldende im String C32KB (*32)
!     IC  = Anzahl Dezimalstellen des Feldes
      INTEGER*2 IAT(NFDIM),IET(NFDIM)
      INTEGER*1 ICT(NFDIM)
!     IAT = Feldanfang im String C32KBT (*32)
!     IET = Feldende im String C32KBT (*32)
!     ICT = Anzahl Dezimalstellen des Feldes
      LOGICAL EXDBF
      INTEGER*1 I1ENDDBF
      CHARACTER*1 C1ENDDBF
      INTEGER*4 NBFILT
      CHARACTER*1 C1BYTE
      INTEGER*1 I1NULL
!
c     WRITE(7,'(T2,A,A)') 'WRITE_DBF_DBLE ',DBFILE
!
      FORM1='(FX.Y)'
      FORM2='(FXX.Y)'
      FORM3='(FXX.YY)'
!
      IF (LC.GE.1.AND.LC.LT.10.AND.LD.GE.1.AND.LD.LT.10) THEN
         WRITE(FORM1(3:3),'(I1)') LC
         WRITE(FORM1(5:5),'(I1)') LD
      ELSE IF (LC.GE.10.AND.LC.LT.100.AND.LD.GE.1.AND.LD.LT.10) THEN
         WRITE(FORM2(3:4),'(I2)') LC
         WRITE(FORM2(6:6),'(I1)') LD
      ELSE IF (LC.GE.10.AND.LC.LT.100.AND.LD.GE.10.AND.LD.LT.100) THEN
         WRITE(FORM3(3:4),'(I2)') LC
         WRITE(FORM3(6:7),'(I2)') LD
      END IF
!
      I1NULL=0
      WRITE(C11NULL,'(11A)') (I1NULL,I=1,11)
!
      LUT=LU+10
      INQUIRE(FILE=DBFILE,EXIST=EXDBF)
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (EXDBF.AND.MOD.EQ.'NEW') THEN
         MOD='ADD'
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='OLD',
     &       FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
         OPEN(LU,FILE=DBFILE,ACCESS='TRANSPARENT',STATUS='NEW',
     &        FORM='UNFORMATTED')
         OPEN(LUT,FILE='TEMP.DBF',ACCESS='TRANSPARENT',STATUS='UNKNOWN',
     &       FORM='UNFORMATTED')
      ELSE
         STOP ' 2016: ES LIEGT KEINE SINNVOLLE KOMBINATION VOR!'
      END IF
!
!
      IF (EXDBF) THEN
!
         CALL INIDBF (LU,NFELD,C11NAME,LN,C1TYPE,NREC,NBREC,IA,IE,IC)
!
         CALL CHKMOD (NFELD,C11NAME,NREC,C11ID,LI,NVAL,MOD)
!
      END IF
!
      IF (EXDBF.AND.MOD.EQ.'UPD') THEN
!        Feld aktualisieren
         NFELDT=NFELD
         NRECT=NREC
         NBRECT=NBREC
         DO NFT=1,NFELDT
            NF=NFT
            C11NAMT(NFT)=C11NAME(NF)
            C1TYPT(NFT)=C1TYPE(NF)
            IAT(NFT)=IA(NF)
            IET(NFT)=IE(NF)
            ICT(NFT)=IC(NF)
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'ADD') THEN
!        Feld hinzufÅgen
         NFELDT=NFELD+1
         NRECT=NREC
         NBRECT=NBREC+LC
         DO NFT=1,NFELDT
            IF (NFT.LT.NFELDT) THEN
!              vorhandene Felder...
               NF=NFT
               C11NAMT(NFT)=C11NAME(NF)
               C1TYPT(NFT)=C1TYPE(NF)
               IAT(NFT)=IA(NF)
               IET(NFT)=IE(NF)
               ICT(NFT)=IC(NF)
            ELSE IF (NFT.EQ.NFELDT) THEN
!              neues Feld hinten anhÑngen...
               NF=NFT-1
               C11NAMT(NFT)=C11NULL
               C11NAMT(NFT)(1:LI)=C11ID
!              Datentyp = N, weil Real (Numerisch)
               C1TYPT(NFT)='N'
               IAT(NFT)=IE(NF)+1
               IET(NFT)=IAT(NFT)+LC-1
!              Anzahl Dezimalstellen=LD
               ICT(NFT)=LD
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (EXDBF.AND.MOD.EQ.'DEL') THEN
!        Feld lîschen
         NFELDT=NFELD-1
         NRECT=NREC
         NBRECT=NBREC-LC
         NFT=0
         DO NF=1,NFELD
            IF (C11NAME(NF)(1:LI).NE.C11ID(1:LI)) THEN
!              verbleibende Felder...
               NFT=NFT+1
               IF (NFT.EQ.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)
                  IET(NFT)=IE(NF)
                  ICT(NFT)=IC(NF)
               ELSE IF (NFT.LT.NF) THEN
                  C11NAMT(NFT)=C11NAME(NF)
                  C1TYPT(NFT)=C1TYPE(NF)
                  IAT(NFT)=IA(NF)-LC
                  IET(NFT)=IE(NF)-LC
                  ICT(NFT)=IC(NF)
               END IF
            ELSE
               NFDEL=NF
            END IF
         END DO
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
      ELSE IF (.NOT.EXDBF.AND.MOD.EQ.'NEW') THEN
!        erstes Feld in neuer Datenbank anlegen
         NFELDT=1
         NREC=NVAL
         NRECT=NREC
         NBRECT=LC+1
!        neues Feld anlegen...
         C11NAMT(1)=C11NULL
         C11NAMT(1)(1:LI)=C11ID
!        Datentyp = N, weil Real (Numerisch)
         C1TYPT(1)='N'
         IAT(1)=2
         IET(1)=LC+1
!        Anzahl Dezimalstellen=LD
         ICT(1)=LD
!
         CALL INIDBT (LUT,NFELDT,C11NAMT,C1TYPT,NRECT,NBRECT,
     &                IAT,IET,ICT,NBFILT)
!
!        hier erstes Feld der neuen Datenbank schreiben ...

         DO NV=1,NVAL
           C32KBT(1:1)=' '
           IF (LC.GE.1.AND.LC.LT.10.AND.LD.GE.1.AND.LD.LT.10) THEN
              WRITE(C32KBT(2:NBRECT),FORM1) ZAHL(NV)
           ELSEIF(LC.GE.10.AND.LC.LT.100.AND.LD.GE.1.AND.LD.LT.10)THEN
              WRITE(C32KBT(2:NBRECT),FORM2) ZAHL(NV)
           ELSEIF(LC.GE.10.AND.LC.LT.100.AND.LD.GE.10.AND.LD.LT.100)THEN
              WRITE(C32KBT(2:NBRECT),FORM3) ZAHL(NV)
           END IF
!          WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
           WRITE(LUT) C32KBT(1:NBRECT)
         END DO
!
      END IF
!
      IF (EXDBF.AND.MOD.NE.'NEW') THEN
!
         NV=0
!
         DO 2 NR=1,NREC
!           alten Record lesen
            READ(LU) C32KB(1:NBREC)
!           WRITE(7,'(T2,A,A)') 'C32KB =',C32KB(1:NBREC)
            IF (C32KB(1:1).NE.'*') THEN
               NV=NV+1
               IF (MOD.EQ.'UPD') THEN
!                 aktuelles Feld suchen...
                  DO NF=1,NFELD
                     IF (C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) NFA=NF
                  END DO
!                 aktuelle FeldlÑnge bestimmem und LC testen...
                  L=IE(NFA)-IA(NFA)+1
                  IF (L.GT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist zu kurz!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),LC,'<',L,' !'
                  ELSE IF (L.LT.LC) THEN
                    WRITE(7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL:'
                    WRITE(7,'(T2,A)') 'vorhandenes Feld ist lÑnger!'
                    WRITE(7,'(T2,A,2(I3,A))') C11NAME(NFA),L,'<',LC,' !'
                  END IF
!                 aktualisierten Feldinhalt in Record schreiben...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (L.GE.1.AND.L.LT.10.AND.
     &               LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM1(3:3),'(I1)') L
                     WRITE(FORM1(5:5),'(I1)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM1) ZAHL(NV)
                  ELSE IF (L.GE.10.AND.L.LT.100.AND.
     &                    LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM2(3:4),'(I2)') L
                     WRITE(FORM2(6:6),'(I1)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM2) ZAHL(NV)
                  ELSE IF (L.GE.10.AND.L.LT.100.AND.
     &                    LD.GE.10.AND.LD.LT.100) THEN
                     WRITE(FORM3(3:4),'(I2)') L
                     WRITE(FORM3(6:7),'(I2)') LD
                     WRITE(C32KBT(IA(NFA):IE(NFA)),FORM3) ZAHL(NV)
                  END IF
               ELSE IF (MOD.EQ.'ADD') THEN
!                 neuen Feldinhalt an Record anhÑngen...
                  C32KBT(1:NBREC)=C32KB(1:NBREC)
                  IF (LC.GE.1.AND.LC.LT.10.AND.
     &               LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM1(3:3),'(I1)') LC
                     WRITE(FORM1(5:5),'(I1)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM1) ZAHL(NV)
                  ELSE IF (LC.GE.10.AND.LC.LT.100.AND.
     &                    LD.GE.1.AND.LD.LT.10) THEN
                     WRITE(FORM2(3:4),'(I2)') LC
                     WRITE(FORM2(6:6),'(I1)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM2) ZAHL(NV)
                  ELSE IF (LC.GE.10.AND.LC.LT.100.AND.
     &                    LD.GE.10.AND.LD.LT.100) THEN
                     WRITE(FORM3(3:4),'(I2)') LC
                     WRITE(FORM3(6:7),'(I2)') LD
                     WRITE(C32KBT((NBREC+1):NBRECT),FORM3) ZAHL(NV)
                  END IF
               ELSE IF (MOD.EQ.'DEL') THEN
!                 Feldinhalt aus Record lîschen...
                  IF (NFDEL.EQ.1) THEN
                     NBRA=IA(NFDEL+1)
                     C32KBT(1:NBRECT)=C32KB(NBRA:NBREC)
                  ELSE IF (NFDEL.EQ.NFELD) THEN
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBRECT)=C32KB(1:NBLE)
                  ELSE
                     NBLE=IE(NFDEL-1)
                     C32KBT(1:NBLE)=C32KB(1:NBLE)
                     NBRA=IA(NFDEL+1)
                     C32KBT((NBLE+1):NBRECT)=C32KB(NBRA:NBREC)
                  END IF
               END IF
!              Ausgabe auf Datei TEMP.DBF
!              WRITE(7,'(T2,A,A)') 'C32KBT=',C32KBT(1:NBRECT)
               WRITE(LUT) C32KBT(1:NBRECT)
            ELSE
               WRITE (7,'(T2,A)') 'SUBROUTINE WRITE_DBF_REAL'
               WRITE (7,'(T2,A)') 'PROGRAMMERWEITERUNG ERFORDERLICH'
               WRITE (7,'(T2,A)') 'FöR MIT "*" GELôSCHTE RECORDS!'
               STOP
            END IF
    2    CONTINUE
!
         IF (NV.NE.NVAL) THEN
            WRITE(7,'(T2,A)') DBFILE
            WRITE(7,'(T2,A)') C11NAME(NFA)
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
            WRITE(7,'(T2,A,I9)') 'FELDLAENGE NVAL = ',NVAL
            WRITE(7,'(T2,A,I9)') 'AKTIVE RECORDS  = ',NV
            WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
         END IF
!
      END IF
!
      IF (MOD.NE.'   ') THEN
!
!        Markierung fÅr Dateiende
         I1ENDDBF=  26
         WRITE(C1ENDDBF,'(A)') I1ENDDBF
         WRITE(LUT) C1ENDDBF
!
!        KOPIEREN ALLER BYTES VON TEMP.DBF (LUT) AUF DBFILE(LU)
!
c         REWIND(LU)
c         REWIND(LUT)
!
c         DO NB=1,NBFILT
c            READ(LUT) C1BYTE
c            WRITE(LU) C1BYTE
c         END DO
!
      END IF
!
      CLOSE(LUT,STATUS='KEEP')
!     WRITE(7,'(T2,A)') 'TEMP.DBF GESCHLOSSEN!'
!
      CLOSE(LU,STATUS='KEEP')
!     WRITE(7,'(T2,A,A)') DBFILE,' GESCHLOSSEN!'
!
      DO IP=4,255 
         IF (DBFILE((IP-3):(IP-3)).EQ.'.') LDBFILE=IP
      END DO
      CALL SYSTEM ("ECHO OFF")
      CALL DOSCOPY ('TEMP.DBF',8,DBFILE,LDBFILE)
      CALL SYSTEM ("DEL TEMP.DBF")
      CALL SYSTEM ("ECHO ON")
c     CALL SYSTEM ("DEL TEMP.DBF > screen")
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE INIDBT (LUT,NFELD,C11NAME,C1TYPE,NREC,NBREC,IA,IE,IC,
     &                   NBFILE)
!
!     TEMPORéRE DATENBANK BEI AUSGABE INITIALISIEREN
!     D.H. DATENBANK-HEADER IM DBASE-FORMAT SCHREIBEN
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
!     VEREINBARUNGEN DATEIKOPF
      CHARACTER*32 C32BYTE
      CHARACTER*12 C12RESMU,C12NULL
      CHARACTER*1 C1VALID,C1TFLAG,C1EFLAG,C1MDX1,C1LDID,C1ENDHDR,C1NULL
      INTEGER*1 I1ENDHDR,I1NULL
      CHARACTER*2 C2ZERO,C2NULL
      INTEGER*1 JJ,MM,TT
      INTEGER*2 NBHEAD,NBREC
      INTEGER*2 NFELD,NF
      INTEGER*4 NBFILE
!     NREC   = Anzahl der DatensÑtze (*4)
!     NBHEAD = Anzahl der Bytes im Dateikopf (*2)
!     NBREC  = Datensatzgrî· in Bytes (*2)
!     NFELD = Anzahl der Felder (Spalten) (*2)
!
!     VEREINBARUNGEN FELDBESCHREIBER
      CHARACTER*11 C11NAME(NFDIM)
      CHARACTER*1 C1TYPE(NFDIM),C1MDX2(NFDIM),C2RES(NFDIM),C1IDW(NFDIM)
      INTEGER*1 IC(NFDIM),I1LF
      INTEGER*2 LF(NFDIM)
!     LF = FeldlÑnge (*1)
!     IC = Dezimalstellen (*1)
!
!     VEREINBARUNGEN DATEN-RECORDS
      INTEGER*2 IA(NFDIM),IE(NFDIM)
!
      I1NULL=0
      WRITE(C1NULL,'(A)') I1NULL
      C2NULL(1:1)=C1NULL
      C2NULL(2:2)=C1NULL
      C12NULL(1:2)=C2NULL
      C12NULL(3:4)=C2NULL
      C12NULL(5:6)=C2NULL
      C12NULL(7:8)=C2NULL
      C12NULL(9:10)=C2NULL
      C12NULL(11:12)=C2NULL
!
      C1VALID=''
      C32BYTE(1:1)=C1VALID
c     JJ=2001
      JJ=01
      MM=06
      TT=04
      WRITE(C32BYTE(2:2),'(A)') JJ
      WRITE(C32BYTE(3:3),'(A)') MM
      WRITE(C32BYTE(4:4),'(A)') TT
c     WRITE(7,'(T2,A,I11)') 'INIDBT:   NREC=',NREC
      WRITE(C32BYTE(5:8),'(A)') NREC
c     WRITE(7,'(T2,A,I11)') 'INIDBT:  NFELD=',NFELD
      NBHEAD=NFELD*32
      NBHEAD=NBHEAD+1+32
c     WRITE(7,'(T2,A,I11)') 'INIDBT: NBHEAD=',NBHEAD
c     WRITE(7,'(T2,A,I11)') 'INIDBT:  NBREC=',NBREC
      NBFILE=NBHEAD+NREC*NBREC+1
c     WRITE(7,'(T2,A,I11)') 'INIDBT: NBFILE=',NBFILE
      WRITE(C32BYTE(9:10),'(A)') NBHEAD
      WRITE(C32BYTE(11:12),'(A)') NBREC
!     C2ZERO='  '
      C2ZERO=C2NULL
      C32BYTE(13:14)=C2ZERO
!     TRANSACTION FLAG
!     C1TFLAG=' '
      C1TFLAG=C1NULL
      C32BYTE(15:15)=C1TFLAG
!     ENCRYPTION FLAG
!     C1EFLAG=' '
      C1EFLAG=C1NULL
      C32BYTE(16:16)=C1EFLAG
!     RESERVED FOR MULTI USER
!     C12RESMU='            '
      C12RESMU=C12NULL
      C32BYTE(17:28)=C12RESMU
!     MDX FILE FLAG
!     C1MDX1=' '
      C1MDX1=C1NULL
      C32BYTE(29:29)=C1MDX1
!     LANGUAGE DRIVER ID
!     C1LDID=' '
      C1LDID=C1NULL
      C32BYTE(30:30)=C1LDID
!     C2ZERO='  '
      C2ZERO=C2NULL
      C32BYTE(31:32)=C2ZERO
!
!     Ausgabe auf Datei
!     WRITE(7,'(T2,2A)') 'INIDBT: C32BYTE=',C32BYTE
      WRITE(LUT) C32BYTE
!
      DO 1 NF=1,NFELD
!        Feldname
         C32BYTE(1:11)=C11NAME(NF)
!        Feldtyp
         C32BYTE(12:12)=C1TYPE(NF)
!        FeldlÑnge
         LF(NF)=IE(NF)-IA(NF)+1
         IF (LF(NF).GE.0.AND.LF(NF).LE.127) I1LF=LF(NF)
         IF (LF(NF).GT.127.AND.LF(NF).LE.255) I1LF=LF(NF)-256
         WRITE(C32BYTE(17:17),'(A)') I1LF
!        Dezimalstellen
         WRITE(C32BYTE(18:18),'(A)') IC(NF)
!        sonstige
!        C2RES(NF)='  '
         C2RES(NF)=C2NULL
         C32BYTE(19:20)=C2RES(NF)
!        C1IDW(NF)=' '
         C1IDW(NF)=C1NULL
         C32BYTE(21:21)=C1IDW(NF)
!        C1MDX2(NF)=' '
         C1MDX2(NF)=C1NULL
         C32BYTE(32:32)=C1MDX2(NF)
!        Ausgabe auf Datei
!        WRITE(7,'(T2,2A)') 'INIDBT: C32BYTE=',C32BYTE
         WRITE(LUT) C32BYTE
    1 CONTINUE
!
!     Markierung fÅr Feldende (Dateikopf)
      I1ENDHDR=  13
      WRITE(C1ENDHDR,'(A)') I1ENDHDR
c     WRITE(7,'(T2,2A)') 'INIDBT: C1ENDHDR=',C1ENDHDR
      WRITE(LUT) C1ENDHDR
!
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE CHKMOD (NFELD,C11NAME,NREC,C11ID,LI,NVAL,MOD)
!
      PARAMETER (NFDIM=256)
!     PARAMETER (NFDIM=512)
!
!     VEREINBARUNGEN FELDBESCHREIBER
      CHARACTER*11 C11NAME(NFDIM),C11NULL
      CHARACTER (LEN=LI) C11ID
      INTEGER*2 NFELD,NF
      CHARACTER*3 MOD
      INTEGER*1 I1NULL
!
      I1NULL=0
      WRITE(C11NULL,'(11A)') (I1NULL,I=1,11)

      IEX=0
!
      DO NF=1,NFELD
         IF (C11NAME(NF)(1:LI).EQ.C11ID(1:LI)) THEN
            IF (LI.LT.11) THEN
               IF (C11NAME(NF)((LI+1):11).EQ.C11NULL((LI+1):11)) THEN
c                 WRITE(7,'(T2,A,A,A)')'Feld ',C11ID(1:LI),' existiert!'
                  IEX=1
                  IF (MOD.EQ.'ADD'.OR.MOD.EQ.'add') THEN
                     C11NAME(NF)((LI+1):(LI+1))='?'
c                    WRITE(7,'(T2,A,A,A)')'Feld ',C11ID(1:LI),' backup!'
c                    WRITE(7,'(T2,A)') C11NAME(NF)
                  END IF
               END IF
            ELSE IF (LI.EQ.11) THEN
c              WRITE(7,'(T2,A,A,A)') 'Feld ',C11ID(1:LI),' existiert!'
               IEX=1
               IF (MOD.EQ.'ADD'.OR.MOD.EQ.'add') THEN
                  C11NAME(NF)(11:11)='?'
c                 WRITE(7,'(T2,A,A,A)') 'Feld ',C11ID(1:LI),' backup!'
c                 WRITE(7,'(T2,A)') C11NAME(NF)
               END IF
            END IF
         END IF
      END DO
!
      IF (IEX.EQ.0) THEN
c        WRITE(7,'(T2,A,A,A)') 'Feld ',C11ID(1:LI),' existiert nicht!'
!
         IF (MOD.EQ.'UPD'.OR.MOD.EQ.'upd') THEN
            MOD='ADD'
c           WRITE(7,'(T2,A)') 'Feld nicht vorhanden, kein UPD, ADD!'
         END IF
!
         IF (MOD.EQ.'DEL'.OR.MOD.EQ.'del') THEN
            MOD='   '
c           WRITE(7,'(T2,A)') 'Feld nicht vorhanden, kein DEL!'
         END IF
      END IF
!
      IF (NVAL.NE.NREC) THEN
         WRITE(7,'(T2,A)') C11ID
         WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
         WRITE(7,'(T2,A,I9)') 'FELDLAENGE NVAL = ',NVAL
         WRITE(7,'(T2,A,I9)') 'RECORDZAHL NREC = ',NREC
         WRITE(7,'(T2,A)')    '--------ACHTUNG!--------'
      END IF
!
      RETURN
      END
!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE DOSCOPY (CVON,LVON,CNACH,LNACH)
C-----------------------------------------------------------------------
      CHARACTER(LEN=LVON) CVON
      CHARACTER(LEN=LNACH) CNACH
      CHARACTER(LEN=122) :: DOSCMD = ' '

      DOSCMD(1:9)='"copy /Y '
      DOSCMD(10:(LVON+9))=CVON(1:LVON)
      DOSCMD((LVON+10):(LVON+10))=' '
      DOSCMD((LVON+11):(LVON+10+LNACH))=CNACH(1:LNACH)
      DOSCMD((LVON+11+LNACH):(LVON+11+LNACH))='"'
      LCMD=LVON+11+LNACH

      WRITE(7,'(A,A)') 'DOSCMD = ',DOSCMD(1:LCMD)
       
      DOSCMD((LCMD):(LCMD+9))=' > Screen"'
      LCMD=LCMD+9

      WRITE(7,'(A,A)') 'DOSCMD = ',DOSCMD(1:LCMD)

      IF (LCMD.GT.122) STOP 'DOSCOPY: LCMD > 122 !'

      CALL SYSTEM(DOSCMD(1:LCMD))

      RETURN
      END
C-----------------------------------------------------------------------
