C-----------------------------------------------------------------------
      SUBROUTINE READ_SHP (LU,SHP,ISHPTYP,XMIN,XMAX,YMIN,YMAX,
     &           ZMIN,ZMAX,MMIN,MMAX,NUMRECS,ISUMPARTS,ISUMPOINTS,
     &           IPARTS,XPOINT,YPOINT,ZPOINT,MPOINT,NPMAX,IMOPT)
C=======================================================================
C
C     Unterprogramm zu lesen der ESRI-SHP-Datei
C
C     *.shp = MAIN FILE
C     *.shx = INDEX FILE
C     *.dbf = DBASE FILE
C
C=======================================================================
C     MAIN FILE HEADER - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld         Wert       Typ  Order  Variablen
C-----------------------------------------------------------------------
C     Byte 0    FileCode     9994        I    Big   CFILCODBIG,IFILCOD
C     Byte 4    Unused       0           I    Big
C     Byte 8    Unused       0           I    Big
C     Byte 12   Unused       0           I    Big
C     Byte 16   Unused       0           I    Big
C     Byte 20   Unused       0           I    Big
C     Byte 24   FileLength   FileLength  I    Big   CFILLENBIG,IFILLEN
C     Byte 28   Version      1000        I    Lit   IVERSION
C     Byte 32   ShapeType    ShapeType   I    Lit   ISHPTYP
C     Byte 36   BoundingBox  Xmin        D    Lit   XMIN
C     Byte 44   BoundingBox  Ymin        D    Lit   XMIN
C     Byte 52   BoundingBox  Xmax        D    Lit   YMIN
C     Byte 60   BoundingBox  Ymax        D    Lit   YMAX
C     Byte 68*  BoundingBox  Zmin        D    Lit   ZMIN
C     Byte 76*  BoundingBox  Zmax        D    Lit   ZMAX
C     Byte 84*  BoundingBox  Mmin        D    Lit   MMIN
C     Byte 92*  BoundingBox  Mmax        D    Lit   MMAX
C     (* = unused, with value 0.0, if not Measured or Z type)
C-----------------------------------------------------------------------
C     Measure M optional in Shape-File-Typen 11,13,15,18,21,23,25,28
C     IMOPT=0   kein Measure M in Shape-Datei
C     IMOPT=1   Measure M in Shape-Datei vorbelegt
C=======================================================================
C     MAIN FILE HEADER - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=255) SHP
      CHARACTER (LEN=100) CFILHED
      CHARACTER (LEN=4) CFILCODBIG,CFILLENBIG,C4
      CHARACTER (LEN=8) C8
      CHARACTER (LEN=16) C16
      CHARACTER (LEN=40) C40
      CHARACTER (LEN=36) C36
      INTEGER*4 IFILCOD,IFILLEN,IVERSION,ISHPTYP,ITYP
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX
      DOUBLE PRECISION XMINT,YMINT,XMAXT,YMAXT,ZMINT,ZMAXT,MMINT,MMAXT
C-----------------------------------------------------------------------
C     MAIN FILE RECORD HEADER
C-----------------------------------------------------------------------
      CHARACTER (LEN=8) CRECHED
      CHARACTER (LEN=4) CRECNUMBIG,CCONLENBIG
      INTEGER*4 IRECNUM,ICONLEN
C=======================================================================
C     MAIN FILE RECORD CONTENTS - DESCRIPTION
C=======================================================================
C     Point Record Content - Description (ShapeType = 1)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  1        I   Lit  1        ISHPTYP
C     Byte 4    XPoint   X        D   Lit  1        XPOINT()
C     Byte 12   YPoint   Y        D   Lit  1        YPOINT()
C=======================================================================
C     PointZ Record Content - Description (ShapeType = 11)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  11       I   Lit  1        ISHPTYP
C     Byte 4    XPoint   X        D   Lit  1        XPOINT()
C     Byte 12   YPoint   Y        D   Lit  1        YPOINT()
C     Byte 20   ZPoint   Z        D   Lit  1        ZPOINT()
C     Byte 28   MPoint   M        D   Lit  1        MPOINT()
C=======================================================================
C     PolyLine Record Content - Description (ShapeType = 3)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  3        I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     (X = 44 + 4 * NumParts)
C=======================================================================
C     PolyLineZ Record Content - Description (ShapeType = 13)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  13       I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     Byte Y    Zmin     Zmin     D   Lit  1        ZBLO
C     Byte Y+8  Zmax     Zmax     D   Lit  1        ZBHI
C     Byte Y+16 Zarray   Zarray   D   Lit  NPoints  ZPOINT()
C    *Byte Z    Mmin     Mmin     D   Lit  1        MBLO
C    *Byte Z+8  Mmax     Mmax     D   Lit  1        MBHI
C    *Byte Z+16 Marray   Marray   D   Lit  NPoints  MPOINT()
C     (X = 44 + (4 * NumParts)
C     (Y = X + (16 * NumPoints))
C     (Z = Y + 16 + (8 * NumPoints))  ( * optional )
C=======================================================================
C     Polygon Record Content - Description (ShapeType = 5)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  5        I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     (X = 44 + 4 * NumParts)
C=======================================================================
C     PolygonZ Record Content - Description (ShapeType = 15)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  15       I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     Byte Y    Zmin     Zmin     D   Lit  1        ZBLO
C     Byte Y+8  Zmax     Zmax     D   Lit  1        ZBHI
C     Byte Y+16 Zarray   Zarray   D   Lit  NPoints  ZPOINT()
C    *Byte Z    Mmin     Mmin     D   Lit  1        MBLO
C    *Byte Z+8  Mmax     Mmax     D   Lit  1        MBHI
C    *Byte Z+16 Marray   Marray   D   Lit  NPoints  MPOINT()
C     (X = 44 + (4 * NumParts)
C     (Y = X + (16 * NumPoints))
C     (Z = Y + 16 + (8 * NumPoints))  ( * optional )
C=======================================================================
C     MAIN FILE RECORD CONTENTS - VEREINBARUNGEN
C
C     NUMRECS = Anzahl der Records im Shape-file = Anzahl Objekte
C
C     ISUMPARTS() = Summe der NUMPARTS incl. des aktuellen Records
C     ISUMPOINTS() = Summe der NUMPOINTS incl. des aktuellen Records
C-----------------------------------------------------------------------
      DOUBLE PRECISION XBLO,YBLO,XBHI,YBHI,ZBLO,ZBHI,MBLO,MBHI
C     (Variablen sind nur Dummys; evtl. Dimensionieren f¸r Records)
      INTEGER*4 NUMPARTS,NUMPOINTS
      INTEGER*4 ISUMPARTS(*),ISUMPOINTS(*)
      INTEGER*4 IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
      DOUBLE PRECISION ZPOINT(*),MPOINT(*)
      NPMAX=0
      XMINT=1.0*10.**38
      YMINT=1.0*10.**38
      ZMINT=1.0*10.**38
      MMINT=1.0*10.**38
      XMAXT=-1.0*10.**38
      YMAXT=-1.0*10.**38
      ZMAXT=-1.0*10.**38
      MMAXT=-1.0*10.**38
      NZB=0
      NMB=0
C
      WRITE(7,'(T2,A,A)') 'OPEN ',SHP
      OPEN(LU,FILE=SHP,ACCESS='TRANSPARENT',STATUS='OLD')
C
      READ(LU) CFILHED
C     Eingelesene Bytes
      NREADBYTES=100
      CFILCODBIG=CFILHED(1:4)
      CFILLENBIG=CFILHED(25:28)
      CALL CBIG_TO_ILIT (CFILCODBIG,IFILCOD)
      CALL CBIG_TO_ILIT (CFILLENBIG,IFILLEN)
      NFILEBYTES=IFILLEN*2
      READ(CFILHED(29:32),'(A4)') IVERSION
      READ(CFILHED(33:36),'(A4)') ISHPTYP
      READ(CFILHED(37:44),'(A8)') XMIN
      READ(CFILHED(45:52),'(A8)') YMIN
      READ(CFILHED(53:60),'(A8)') XMAX
      READ(CFILHED(61:68),'(A8)') YMAX
      ITYP=ISHPTYP
      IF (ITYP.GT.10.AND.ITYP.LT.30) THEN
         READ(CFILHED(69:76),'(A8)') ZMIN
         READ(CFILHED(77:84),'(A8)') ZMAX
      ELSE
         ZMIN=0.0
         ZMAX=0.0
      END IF
      IF (ITYP.GT.10) THEN
         READ(CFILHED(85:92),'(A8)') MMIN
         READ(CFILHED(93:100),'(A8)') MMAX
      ELSE
         MMIN=0.0
         MMAX=0.0
      END IF
c
      WRITE (7,'(T2,A)      ') '---- SHP-FILE-HEADER ----'
      WRITE (7,'(T2,A,I10)  ') 'File Code = ',IFILCOD
      WRITE (7,'(T2,A,I10)  ')' FileBytes = ',NFILEBYTES
      WRITE (7,'(T2,A,I10)  ')'   Version = ',IVERSION
      WRITE (7,'(T2,A,I10)  ')'      ITYP = ',ITYP
      WRITE (7,'(T2,A,I10)  ')' Shapetype = ',ISHPTYP
      WRITE (7,'(T2,A,F15.5)')'      XMIN = ',XMIN
      WRITE (7,'(T2,A,F15.5)')'      YMIN = ',YMIN
      WRITE (7,'(T2,A,F15.5)')'      XMAX = ',XMAX
      WRITE (7,'(T2,A,F15.5)')'      YMAX = ',YMAX
      WRITE (7,'(T2,A,F15.5)')'      ZMIN = ',ZMIN
      WRITE (7,'(T2,A,F15.5)')'      ZMAX = ',ZMAX
      WRITE (7,'(T2,A,F15.5)')'      MMIN = ',MMIN
      WRITE (7,'(T2,A,F15.5)')'      MMAX = ',MMAX
      WRITE (7,'(T2,A,I20)  ')'NREADBYTES = ',NREADBYTES
      WRITE (7,'(T2,A)')'---- SHP-FILE-RECORDS ----'
C
      IMOPT=0
C
    1 READ(LU) CRECHED
C     WRITE (7,'(T2,A,A)') 'CRECHED = ',CRECHED
C     Eingelesene Bytes
      NREADBYTES=NREADBYTES+8
      CRECNUMBIG=CRECHED(1:4)
      CCONLENBIG=CRECHED(5:8)
      CALL CBIG_TO_ILIT (CRECNUMBIG,IRECNUM)
      CALL CBIG_TO_ILIT (CCONLENBIG,ICONLEN)
      NCONTENTBYTES=ICONLEN*2
c     WRITE (7,'(T2,A,I10)') 'Record Number = ',IRECNUM
c     WRITE (7,'(T2,A,I10)') 'Content Bytes = ',NCONTENTBYTES
C
      IF (ISHPTYP.EQ.1.OR.ISHPTYP.EQ.11.OR.ISHPTYP.EQ.21.OR.
     &    ISHPTYP.EQ.8.OR.ISHPTYP.EQ.18.OR.ISHPTYP.EQ.28) THEN
C        ISHPTYP = 1   ShapeType = Point
C        ISHPTYP = 11  ShapeType = PointZ
C        ISHPTYP = 21  ShapeType = PointM
C        ISHPTYP = 8   ShapeType = MultiPoint
C        ISHPTYP = 18  ShapeType = MultiPointZ
C        ISHPTYP = 28  ShapeType = MultiPointM
         ITYP=ISHPTYP
         READ(LU) C4
C        Eingelesene Bytes
         NREADBYTES=NREADBYTES+4
         READ(C4,'(A4)') ISHPTEST
         IF (ITYP.NE.ISHPTEST) THEN
            WRITE (7,'(T2,A,I10)') '    Record Number = ',IRECNUM
            WRITE (7,'(T2,A,I10)') '    Content Bytes = ',NCONTENTBYTES
            WRITE (7,'(T2,A,I20)')'        NREADBYTES = ',NREADBYTES
            WRITE (7,'(T2,A,I10)')' ISHPTYP IM HEADER = ',ITYP
            WRITE (7,'(T2,A,I10)')' ISHPTYP IM RECORD = ',ISHPTEST
            IF (NCONTENTBYTES.EQ.4.AND.ISHPTEST.EQ.0) THEN
C              NULLSHAPE!
               WRITE (7,*) 'Nullshape!'
               ISUMPOINTS(IRECNUM)=ISUMPOINTS(IRECNUM-1)
               ISUMPARTS(IRECNUM)=ISUMPARTS(IRECNUM-1)
               GOTO 1
            END IF
            STOP 'ShapeTyp (1/11/21/8/18/28) falsch!'
         END IF
         NUMPARTS=0
         ISUMPARTS(IRECNUM)=0
         IF (ISHPTYP.EQ.1.OR.ISHPTYP.EQ.11.OR.ISHPTYP.EQ.21) THEN
            NUMPOINTS=1
         ELSE IF (ISHPTYP.EQ.8.OR.ISHPTYP.EQ.18.OR.ISHPTYP.EQ.28) THEN
            READ(LU) C36
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+36
            READ(C36(1:8),'(A8)') XBLO
            READ(C36(8:16),'(A8)') YBLO
            READ(C36(17:24),'(A8)') XBHI
            READ(C36(25:32),'(A8)') YBHI
            READ(C36(33:36),'(A4)') NUMPOINTS
         END IF
         IF (NUMPOINTS.GT.NPMAX) NPMAX=NUMPOINTS
         IF (IRECNUM.GT.1) THEN
            ISUMPOINTS(IRECNUM)=NUMPOINTS+ISUMPOINTS(IRECNUM-1)
         ELSE
            ISUMPOINTS(IRECNUM)=NUMPOINTS
         END IF
c        PRINT '(T2,A,I10)',' SumPoints = ',ISUMPOINTS(IRECNUM)
         DO NPO=1,NUMPOINTS
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
            READ(LU) C16
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+16
            READ(C16(1:8),'(A8)') XPOINT(ISPO)
            READ(C16(9:16),'(A8)') YPOINT(ISPO)
            IF (ISHPTYP.EQ.11) THEN
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') ZPOINT(ISPO)
               IF (NCONTENTBYTES.GT.28) THEN
C                 Measure M in SHP vorhanden
                  IMOPT=1
                  READ(LU) C8
C                 Eingelesene Bytes
                  NREADBYTES=NREADBYTES+8
                  READ(C8,'(A8)') MPOINT(ISPO)
               END IF
            ELSE IF (ISHPTYP.EQ.21) THEN
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') MPOINT(ISPO)
            END IF
         END DO
         IF (ISHPTYP.EQ.18) THEN
            READ(LU) C16
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+16
            READ(C16(1:8),'(A8)') ZMIN
            READ(C16(9:16),'(A8)') ZMAX
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') ZPOINT(ISPO)
            END DO
C           zu lesende Bytes wenn nur Z vorhanden
            NZB=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
C           optionale zu lesende Bytes wenn M vorhanden
            NMB=NZB+16+(8*NUMPOINTS)
            IF (NCONTENTBYTES.GT.NZB.AND.NCONTENTBYTES.EQ.NMB) THEN
C              Measure M in SHP vorhanden
               IMOPT=1
               READ(LU) C16
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+16
               READ(C16(1:8),'(A8)') MMIN
               READ(C16(9:16),'(A8)') MMAX
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
                  READ(LU) C8
C                 Eingelesene Bytes
                  NREADBYTES=NREADBYTES+8
                  READ(C8,'(A8)') MPOINT(ISPO)
               END DO
            END IF
         ELSE IF (ISHPTYP.EQ.28) THEN
C           optional zu lesende Bytes wenn M vorhanden
            NMB=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
            IF (NCONTENTBYTES.EQ.NMB) THEN
C              Measure M in SHP vorhanden
               IMOPT=1
               READ(LU) C16
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+16
               READ(C16(1:8),'(A8)') MMIN
               READ(C16(9:16),'(A8)') MMAX
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
                  READ(LU) C8
C                 Eingelesene Bytes
                  NREADBYTES=NREADBYTES+8
                  READ(C8,'(A8)') MPOINT(ISPO)
               END DO
            END IF
         END IF
      END IF
C
      IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23.OR.
     &    ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
C        ISHPTYP = 3  ShapeType = PolyLine
C        ISHPTYP = 13 ShapeType = PolyLineZ
C        ISHPTYP = 23 ShapeType = PolyLineM
C        ISHPTYP = 5  ShapeType = Polygon
C        ISHPTYP = 15 ShapeType = PolygonZ
C        ISHPTYP = 25 ShapeType = PolygonM
         ITYP=ISHPTYP
         READ(LU) C4
C        Eingelesene Bytes
         NREADBYTES=NREADBYTES+4
         READ(C4,'(A4)') ISHPTEST
         IF (ITYP.NE.ISHPTEST) THEN
            WRITE (7,'(T2,A,I10)') '    Record Number = ',IRECNUM
            WRITE (7,'(T2,A,I10)') '    Content Bytes = ',NCONTENTBYTES
            WRITE (7,'(T2,A,I20)')'        NREADBYTES = ',NREADBYTES
            WRITE (7,'(T2,A,I10)')' ISHPTYP IM HEADER = ',ITYP
            WRITE (7,'(T2,A,I10)')' ISHPTYP IM RECORD = ',ISHPTEST
            IF (NCONTENTBYTES.EQ.4.AND.ISHPTEST.EQ.0) THEN
C              NULLSHAPE!
               WRITE (7,*) 'Nullshape!'
               ISUMPOINTS(IRECNUM)=ISUMPOINTS(IRECNUM-1)
               ISUMPARTS(IRECNUM)=ISUMPARTS(IRECNUM-1)
               GOTO 1
            END IF
            STOP 'ShapeTyp (3/5/13/15/23/25) falsch!'
         END IF
         READ(LU) C40
C        Eingelesene Bytes
         NREADBYTES=NREADBYTES+40
         READ(C40(1:8),'(A8)') XBLO
         READ(C40(9:16),'(A8)') YBLO
         READ(C40(17:24),'(A8)') XBHI
         READ(C40(25:32),'(A8)') YBHI
         READ(C40(33:36),'(A4)') NUMPARTS
         READ(C40(37:40),'(A4)') NUMPOINTS
         IF (NUMPOINTS.GT.NPMAX) NPMAX=NUMPOINTS
c        WRITE (7,'(T2,A,F15.5)') '     XBLO = ',XBLO
c        WRITE (7,'(T2,A,F15.5)') '     YBLO = ',YBLO
c        WRITE (7,'(T2,A,F15.5)') '     XBHI = ',XBHI
c        WRITE (7,'(T2,A,F15.5)') '     YBHI = ',YBHI
c        WRITE (7,'(T2,A,I10)')   ' NumParts = ',NUMPARTS
c        WRITE (7,'(T2,A,I10)')   'NumPoints = ',NUMPOINTS
         IF (IRECNUM.GT.1) THEN
            ISUMPARTS(IRECNUM)=NUMPARTS+ISUMPARTS(IRECNUM-1)
            ISUMPOINTS(IRECNUM)=NUMPOINTS+ISUMPOINTS(IRECNUM-1)
         ELSE
            ISUMPARTS(IRECNUM)=NUMPARTS
            ISUMPOINTS(IRECNUM)=NUMPOINTS
         END IF
c        PRINT '(T2,A,I10)','  SumParts = ',ISUMPARTS(IRECNUM)
c        PRINT '(T2,A,I10)',' SumPoints = ',ISUMPOINTS(IRECNUM)
         DO NPA=1,NUMPARTS
            IF (IRECNUM.GT.1) THEN
               ISPA=ISUMPARTS(IRECNUM-1)+NPA
            ELSE
               ISPA=NPA
            END IF
            READ(LU) C4
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+4
            READ(C4,'(A4)') IPARTS(ISPA)
            IPARTS(ISPA)=IPARTS(ISPA)+1
c           WRITE(7,'(T2,A,I10)') '   ISPA = ',ISPA
c           WRITE(7,'(T2,A,I10)') ' IParts = ',IPARTS(ISPA)
         END DO
         DO NPO=1,NUMPOINTS
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
            READ(LU) C16
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+16
            READ(C16(1:8),'(A8)') XPOINT(ISPO)
            READ(C16(9:16),'(A8)') YPOINT(ISPO)
         END DO
         IF (ISHPTYP.EQ.13.OR.ISHPTYP.EQ.15) THEN
C           ISHPTYP = 13 ShapeType = PolyLineZ
C           ISHPTYP = 15 ShapeType = PolygonZ
            READ(LU) C8
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+8
            READ(C8,'(A8)') ZBLO
            READ(LU) C8
C           Eingelesene Bytes
            NREADBYTES=NREADBYTES+8
            READ(C8,'(A8)') ZBHI
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') ZPOINT(ISPO)
            END DO
C           zu lesende Bytes wenn nur Z vorhanden
            NZB=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
C           optionale zu lesende Bytes wenn M vorhanden
            NMB=NZB+16+(8*NUMPOINTS)
            IF (NCONTENTBYTES.GT.NZB.AND.NCONTENTBYTES.EQ.NMB) THEN
C              Measure M in SHP vorhanden
               IMOPT=1
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') MBLO
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') MBHI
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
                  READ(LU) C8
C                 Eingelesene Bytes
                  NREADBYTES=NREADBYTES+8
                  READ(C8,'(A8)') MPOINT(ISPO)
               END DO
            END IF
         END IF
         IF (ISHPTYP.EQ.23.OR.ISHPTYP.EQ.25) THEN
C           ISHPTYP = 23 ShapeType = PolyLineM
C           ISHPTYP = 25 ShapeType = PolygonM
C           zu lesende Bytes wenn M (optional) vorhanden
            NMB=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
            IF (NCONTENTBYTES.EQ.NMB) THEN
C              Measure M in SHP vorhanden
               IMOPT=1
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') MBLO
               READ(LU) C8
C              Eingelesene Bytes
               NREADBYTES=NREADBYTES+8
               READ(C8,'(A8)') MBHI
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IRECNUM,NPO)
                  READ(LU) C8
C                 Eingelesene Bytes
                  NREADBYTES=NREADBYTES+8
                  READ(C8,'(A8)') MPOINT(ISPO)
               END DO
            ELSE
               PRINT *
               PRINT '(T2,A)','kein Measure!'
               PRINT *
            END IF
         END IF
      END IF
C
c     WRITE (7,'(T2,A,I10)') '       IRECNUM = ',IRECNUM
c     WRITE (7,'(T2,A,I10)') ' NCONTENTBYTES = ',NCONTENTBYTES
c     WRITE (7,'(T2,A,I10)') '       NZBytes = ',NZB
c     WRITE (7,'(T2,A,I10)') '       NMBytes = ',NMB
c     WRITE (7,'(T2,A,I10)') '    NREADBYTES = ',NREADBYTES
c     WRITE (7,'(T2,A,I10)') '    NFILEBYTES = ',NFILEBYTES

      IF (NREADBYTES.LT.NFILEBYTES) GOTO 1
C
      IF (IMOPT.EQ.0) THEN
         MMIN=0.0
         MMAX=0.0
         WRITE (7,'(T2,A,F15.5)')'      MMIN = ',MMIN
         WRITE (7,'(T2,A,F15.5)')'      MMAX = ',MMAX
      END IF
C
      NUMRECS=IRECNUM
      NPOINTS=ISUMPOINTS(NUMRECS)
      DO NP=1,NPOINTS
         IF (XPOINT(NP).LT.XMINT) XMINT=XPOINT(NP)
         IF (YPOINT(NP).LT.YMINT) YMINT=YPOINT(NP)
         IF (ZPOINT(NP).LT.ZMINT) ZMINT=ZPOINT(NP)
         IF (MPOINT(NP).LT.MMINT) MMINT=MPOINT(NP)
         IF (XPOINT(NP).GT.XMAXT) XMAXT=XPOINT(NP)
         IF (YPOINT(NP).GT.YMAXT) YMAXT=YPOINT(NP)
         IF (ZPOINT(NP).GT.ZMAXT) ZMAXT=ZPOINT(NP)
         IF (MPOINT(NP).GT.MMAXT) MMAXT=MPOINT(NP)
      END DO
C
      WRITE (7,'(T2,A,F15.5)')'    XMINT = ',XMINT
      WRITE (7,'(T2,A,F15.5)')'    YMINT = ',YMINT
      WRITE (7,'(T2,A,F15.5)')'    XMAXT = ',XMAXT
      WRITE (7,'(T2,A,F15.5)')'    YMAXT = ',YMAXT
      WRITE (7,'(T2,A,F15.5)')'    ZMINT = ',ZMINT
      WRITE (7,'(T2,A,F15.5)')'    ZMAXT = ',ZMAXT
      WRITE (7,'(T2,A,F15.5)')'    MMINT = ',MMINT
      WRITE (7,'(T2,A,F15.5)')'    MMAXT = ',MMAXT
C
      CLOSE (LU,STATUS='KEEP')
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_SHP_TO_BNA (LU,ISHPTYP,NUMRECS,ISUMPARTS,
     &                             ISUMPOINTS,IPARTS,XPOINT,YPOINT,
     &                             C127PID,LPID,C127SID,LSID)
      CHARACTER*127 C127PID(*),C127SID(*)
      CHARACTER*127 C127PIT,C127SIT
      INTEGER*4 ISHPTYP,ISUMPARTS(*),ISUMPOINTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
C     Schleife Åber alle Records
      DO IREC=1,NUMRECS
        IF (IREC.GT.1) THEN
          NUMPARTS=ISUMPARTS(IREC)-ISUMPARTS(IREC-1)
          NUMPOINTS=ISUMPOINTS(IREC)-ISUMPOINTS(IREC-1)
        ELSE
          NUMPARTS=ISUMPARTS(IREC)
          NUMPOINTS=ISUMPOINTS(IREC)
        END IF

C       WRITE(7,'(T2,A,I10)') 'SHP_TO_BNA: NUMPOINTS = ',NUMPOINTS
C
        C127PIT=C127PID(IREC)
        C127SIT=C127SID(IREC)
C
        IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23.OR.
     &      ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
C         Schleife Åber Polygonteile (NUMPARTS)
          DO NPA=1,NUMPARTS
            IF (IREC.GT.1) THEN
              ISPA=ISUMPARTS(IREC-1)+NPA
            ELSE
              ISPA=NPA
            END IF
Calt        NPOANF=IPARTS(ISPA)+1
            NPOANF=IPARTS(ISPA)
            IF (NPA.EQ.NUMPARTS) THEN
              NPOEND=NUMPOINTS
            ELSE
              NPOEND=IPARTS(ISPA+1)+1-1
            END IF
            NV=NPOEND-NPOANF+1
            IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23) THEN
              WRITE(LU,'(5A,I10)')'"',C127PIT(1:LPID),'","',
     &                                C127SIT(1:LSID),'",',-NV
            ELSE IF(ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25)THEN
              WRITE(LU,'(5A,I10)')'"',C127PIT(1:LPID),'","',
     &                                C127SIT(1:LSID),'",',NV
            END IF
            DO NPO=NPOANF,NPOEND
              ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
              WRITE(LU,*) XPOINT(ISPO),YPOINT(ISPO)
            END DO
          END DO
        ELSE IF (ISHPTYP.EQ.1.OR.ISHPTYP.EQ.11.OR.
     &           ISHPTYP.EQ.8.OR.ISHPTYP.EQ.18) THEN
C         Schleife Åber Einzelpunkte (NUMPOINTS)
          DO NPO=1,NUMPOINTS
            NV=NPO
C           WRITE(LU,'(A,I6,A,I6,A,I10)')'"',NPO,'","',IREC,'",',NV
            WRITE(LU,'(5A,I10)')'"',C127PIT(1:LPID),'","',
     &                              C127SIT(1:LSID),'",',NV
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
            WRITE(LU,*) XPOINT(ISPO),YPOINT(ISPO)
          END DO
        END IF
C
      END DO
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_SHP_TO_DAT (LU,ISHPTYP,NUMRECS,ISUMPARTS,
     &                             ISUMPOINTS,IPARTS,XPOINT,YPOINT,
     &                             ZPOINT,MPOINT,SPOINT)
C     Vereinbarungen
      INTEGER*4 ISHPTYP,ISUMPARTS(*),ISUMPOINTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
      DOUBLE PRECISION ZPOINT(*),MPOINT(*)
      DOUBLE PRECISION SPOINT(*)
C     Dateikopf
      WRITE(LU,'(A)') '"IREC" "NPO" "X" "Y" "Z" "M" "S"'
C     Schleife Åber alle Records
      DO IREC=1,NUMRECS
        IF (IREC.GT.1) THEN
          NUMPARTS=ISUMPARTS(IREC)-ISUMPARTS(IREC-1)
          NUMPOINTS=ISUMPOINTS(IREC)-ISUMPOINTS(IREC-1)
        ELSE
          NUMPARTS=ISUMPARTS(IREC)
          NUMPOINTS=ISUMPOINTS(IREC)
        END IF

C       WRITE(7,'(T2,A,I10)') 'SHP_TO_BNA: NUMPOINTS = ',NUMPOINTS

        IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23.OR.
     &      ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
C         Schleife Åber Polygonteile (NUMPARTS)
          DO NPA=1,NUMPARTS
            IF (IREC.GT.1) THEN
              ISPA=ISUMPARTS(IREC-1)+NPA
            ELSE
              ISPA=NPA
            END IF
Calt        NPOANF=IPARTS(ISPA)+1
            NPOANF=IPARTS(ISPA)
            IF (NPA.EQ.NUMPARTS) THEN
              NPOEND=NUMPOINTS
            ELSE
              NPOEND=IPARTS(ISPA+1)+1-1
            END IF
            NV=NPOEND-NPOANF+1
            DO NPO=NPOANF,NPOEND
              ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
              WRITE(LU,*) IREC,NPO,XPOINT(ISPO),YPOINT(ISPO),
     &                    ZPOINT(ISPO),MPOINT(ISPO),SPOINT(ISPO)
            END DO
          END DO
        ELSE IF (ISHPTYP.EQ.1.OR.ISHPTYP.EQ.11.OR.
     &           ISHPTYP.EQ.8.OR.ISHPTYP.EQ.18) THEN
C         Schleife Åber Einzelpunkte (NUMPOINTS)
          DO NPO=1,NUMPOINTS
            NV=NPO
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
            WRITE(LU,*) IREC,NPO,XPOINT(ISPO),YPOINT(ISPO),
     &                  ZPOINT(ISPO),MPOINT(ISPO),SPOINT(ISPO)
          END DO
        END IF
C
      END DO
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SIZES_OF_SHP (LU,ISHPTYP,NUMRECS,ISUMPARTS,ISUMPOINTS,
     &           IPARTS,XPOINT,YPOINT,SPOINT,NPMAX,DLEN,AREA,APOS,ANEG,
     &           IRECOFP,IPARTOFP,IMOPT,C127PID,LPID,C127SID,LSID,NDIM)
      CHARACTER*127 C127PID(*),C127SID(*),C127
      CHARACTER*127 C127PIT,C127SIT
      INTEGER*4 ISHPTYP,ISUMPARTS(*),ISUMPOINTS(*),IPARTS(*)
      INTEGER*4 IRECOFP(*),IPARTOFP(*),NCOMP(NUMRECS)
      DOUBLE PRECISION XPOINT(*),YPOINT(*),SPOINT(*),XPANF,YPANF
      DOUBLE PRECISION XTEMP(NPMAX),YTEMP(NDIM),APART(NDIM)
      DOUBLE PRECISION DX,DY,DLEN(*),AREA(*),APOS(*),ANEG(*)
      DOUBLE PRECISION DLENMAX,AREAMAX

C     LPID ermitteln
      IF (NUMRECS.GE.0.AND.NUMRECS.LT.10) LPID=1
      IF (NUMRECS.GE.10.AND.NUMRECS.LT.100) LPID=2
      IF (NUMRECS.GE.100.AND.NUMRECS.LT.1000) LPID=3
      IF (NUMRECS.GE.1000.AND.NUMRECS.LT.10000) LPID=4
      IF (NUMRECS.GE.10000.AND.NUMRECS.LT.100000) LPID=5
      IF (NUMRECS.GE.100000.AND.NUMRECS.LT.1000000) LPID=6
      IF (NUMRECS.GE.1000000.AND.NUMRECS.LT.10000000) LPID=7
      IF (NUMRECS.GE.10000000.AND.NUMRECS.LT.100000000) LPID=8

      DLENMAX=0.0
      AREAMAX=0.0

C     Schleife Åber alle Records zur Ermittlung von DLEN und AREA

      WRITE(7,*) 'SIZES_OF_SHP: NUMRECS = ',NUMRECS

      DO IREC=1,NUMRECS
         NCOMP(IREC)=0

C        C127PID aus IREC ermitteln
         IF (LPID.EQ.1) WRITE(C127(1:LPID),'(I1)') IREC
         IF (LPID.EQ.2) WRITE(C127(1:LPID),'(I2)') IREC
         IF (LPID.EQ.3) WRITE(C127(1:LPID),'(I3)') IREC
         IF (LPID.EQ.4) WRITE(C127(1:LPID),'(I4)') IREC
         IF (LPID.EQ.5) WRITE(C127(1:LPID),'(I5)') IREC
         IF (LPID.EQ.6) WRITE(C127(1:LPID),'(I6)') IREC
         IF (LPID.EQ.7) WRITE(C127(1:LPID),'(I7)') IREC
         IF (LPID.EQ.8) WRITE(C127(1:LPID),'(I8)') IREC
         C127PID(IREC)=C127

         IF (IREC.GT.1) THEN
            NUMPARTS=ISUMPARTS(IREC)-ISUMPARTS(IREC-1)
            NUMPOINTS=ISUMPOINTS(IREC)-ISUMPOINTS(IREC-1)
         ELSE
            NUMPARTS=ISUMPARTS(IREC)
            NUMPOINTS=ISUMPOINTS(IREC)
         END IF

C        LÑnge der Polygone in einem Record zu Null setzen
         DLEN(IREC)=0.0
C        FlÑchen in einem Record zu Null setzen
         AREA(IREC)=0.0
         APOS(IREC)=0.0
         ANEG(IREC)=0.0
C        Schleife Åber Polygonteile
         DO NPA=1,NUMPARTS
            APART(NPA)=0.0
            IF (IREC.GT.1) THEN
               ISPA=ISUMPARTS(IREC-1)+NPA
            ELSE
               ISPA=NPA
            END IF
Calt        NPOANF=IPARTS(ISPA)+1
            NPOANF=IPARTS(ISPA)
            IF (NPA.EQ.NUMPARTS) THEN
               NPOEND=NUMPOINTS
            ELSE
               NPOEND=IPARTS(ISPA+1)+1-1
            END IF
            ISPOV=0
            IT=0
            DO NPO=NPOANF,NPOEND
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
               IRECOFP(ISPO)=IREC
               IPARTOFP(ISPO)=NPA
               IF (NPO.EQ.NPOANF) SPOINT(ISPO)=0.0
               IF (ISPOV.GT.0) THEN
                  DX=XPOINT(ISPO)-XPOINT(ISPOV)
                  DY=YPOINT(ISPO)-YPOINT(ISPOV)
                  SPOINT(ISPO)=SPOINT(ISPOV)+DSQRT(DX*DX+DY*DY)
                  DLEN(IREC)=DSQRT(DX*DX+DY*DY)+DLEN(IREC)
               END IF
               ISPOV=ISPO
               IT=IT+1
               XTEMP(IT)=XPOINT(ISPO)
               YTEMP(IT)=YPOINT(ISPO)
            END DO
            IF (ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
               ITMAX=IT
               CALL AREAXY (ITMAX,XTEMP,YTEMP,APART(NPA))
               AREA(IREC)=AREA(IREC)+APART(NPA)
               IF (APART(NPA).LT.0.0) THEN
                  ANEG(IREC)=ANEG(IREC)+APART(NPA)
                  NCOMP(IREC)=NCOMP(IREC)+ITMAX+1
               END IF
               IF (APART(NPA).GT.0.0) THEN
                  APOS(IREC)=APOS(IREC)+APART(NPA)
                  NCOMP(IREC)=NCOMP(IREC)+ITMAX
               END IF
            ELSE
               AREA(IREC)=0.0
               APOS(IREC)=0.0
               ANEG(IREC)=0.0
            END IF
         END DO

         IF (DLEN(IREC).GT.DLENMAX) DLENMAX=DLEN(IREC)
         IF (AREA(IREC).GT.AREAMAX) AREAMAX=AREA(IREC)

      END DO
C
      IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23) THEN
         IF (DLENMAX.EQ.0.0) LSID=3
         IF (DLENMAX.GE.0.AND.DLENMAX.LT.10) LSID=1+3
         IF (DLENMAX.GE.10.AND.DLENMAX.LT.100) LSID=2+3
         IF (DLENMAX.GE.100.AND.DLENMAX.LT.1000) LSID=3+3
         IF (DLENMAX.GE.1000.AND.DLENMAX.LT.10000) LSID=4+3
         IF (DLENMAX.GE.10000.AND.DLENMAX.LT.100000) LSID=5+3
         IF (DLENMAX.GE.100000.AND.DLENMAX.LT.1000000) LSID=6+3
         IF (DLENMAX.GE.1000000.AND.DLENMAX.LT.10000000) LSID=7+3
         IF (DLENMAX.GE.10000000.AND.DLENMAX.LT.100000000) LSID=8+3
         IF (DLENMAX.GE.100000000.AND.DLENMAX.LT.1000000000) LSID=9+3
         DO IREC=1,NUMRECS
            IF (LSID.EQ.3) C127(1:LSID)='0.0'
            IF (LSID.EQ.4) WRITE(C127(1:LSID),'(F4.2)') DLEN(IREC)
            IF (LSID.EQ.5) WRITE(C127(1:LSID),'(F5.2)') DLEN(IREC)
            IF (LSID.EQ.6) WRITE(C127(1:LSID),'(F6.2)') DLEN(IREC)
            IF (LSID.EQ.7) WRITE(C127(1:LSID),'(F7.2)') DLEN(IREC)
            IF (LSID.EQ.8) WRITE(C127(1:LSID),'(F8.2)') DLEN(IREC)
            IF (LSID.EQ.9) WRITE(C127(1:LSID),'(F9.2)') DLEN(IREC)
            IF (LSID.EQ.10) WRITE(C127(1:LSID),'(F10.2)') DLEN(IREC)
            IF (LSID.EQ.11) WRITE(C127(1:LSID),'(F11.2)') DLEN(IREC)
            IF (LSID.EQ.12) WRITE(C127(1:LSID),'(F12.2)') DLEN(IREC)
            C127SID(IREC)=C127
         END DO
      ELSE IF (ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
         IF (AREAMAX.EQ.0.0) LSID=3
         IF (AREAMAX.GE.0.AND.AREAMAX.LT.10) LSID=1+3
         IF (AREAMAX.GE.10.AND.AREAMAX.LT.100) LSID=2+3
         IF (AREAMAX.GE.100.AND.AREAMAX.LT.1000) LSID=3+3
         IF (AREAMAX.GE.1000.AND.AREAMAX.LT.10000) LSID=4+3
         IF (AREAMAX.GE.10000.AND.AREAMAX.LT.100000) LSID=5+3
         IF (AREAMAX.GE.100000.AND.AREAMAX.LT.1000000) LSID=6+3
         IF (AREAMAX.GE.1000000.AND.AREAMAX.LT.10000000) LSID=7+3
         IF (AREAMAX.GE.10000000.AND.AREAMAX.LT.100000000) LSID=8+3
         IF (AREAMAX.GE.100000000.AND.AREAMAX.LT.1000000000) LSID=9+3
         DO IREC=1,NUMRECS
            IF (LSID.EQ.3) C127(1:LSID)='0.0'
            IF (LSID.EQ.4) WRITE(C127(1:LSID),'(F4.2)') AREA(IREC)
            IF (LSID.EQ.5) WRITE(C127(1:LSID),'(F5.2)') AREA(IREC)
            IF (LSID.EQ.6) WRITE(C127(1:LSID),'(F6.2)') AREA(IREC)
            IF (LSID.EQ.7) WRITE(C127(1:LSID),'(F7.2)') AREA(IREC)
            IF (LSID.EQ.8) WRITE(C127(1:LSID),'(F8.2)') AREA(IREC)
            IF (LSID.EQ.9) WRITE(C127(1:LSID),'(F9.2)') AREA(IREC)
            IF (LSID.EQ.10) WRITE(C127(1:LSID),'(F10.2)') AREA(IREC)
            IF (LSID.EQ.11) WRITE(C127(1:LSID),'(F11.2)') AREA(IREC)
            IF (LSID.EQ.12) WRITE(C127(1:LSID),'(F12.2)') AREA(IREC)
            C127SID(IREC)=C127
         END DO
      END IF

C     Schleife alle Records zur BNA-Ausgabe Polylinien und Polygone

      DO IREC=1,NUMRECS
c        WRITE(7,'(T2,A,I10)') ' ISUMPARTS(IREC) = ',ISUMPARTS(IREC)
c        WRITE(7,'(T2,A,I10)') 'ISUMPOINTS(IREC) = ',ISUMPOINTS(IREC)
         IF (IREC.GT.1) THEN
            NUMPARTS=ISUMPARTS(IREC)-ISUMPARTS(IREC-1)
            NUMPOINTS=ISUMPOINTS(IREC)-ISUMPOINTS(IREC-1)
         ELSE
            NUMPARTS=ISUMPARTS(IREC)
            NUMPOINTS=ISUMPOINTS(IREC)
         END IF

c        WRITE(7,'(T2,A,I10)') 'SHP_TO_BNA:  NUMPOINTS = ',NUMPOINTS
c        WRITE(7,'(T2,A,I10)') 'SHP_TO_BNA:   NUMPARTS = ',NUMPARTS

C        Schleife Åber Polygonteile (NUMPARTS)
         DO NPA=1,NUMPARTS
            IF (IREC.GT.1) THEN
               ISPA=ISUMPARTS(IREC-1)+NPA
            ELSE
               ISPA=NPA
            END IF
Calt        NPOANF=IPARTS(ISPA)+1
            NPOANF=IPARTS(ISPA)
            IF (NPA.EQ.NUMPARTS) THEN
               NPOEND=NUMPOINTS
            ELSE
               NPOEND=IPARTS(ISPA+1)+1-1
            END IF
            IF (ANEG(IREC).LT.0.0.AND.APOS(IREC).GT.0.0) THEN
               NV=NCOMP(IREC)
            ELSE
               NV=NPOEND-NPOANF+1
               NCOMP(IREC)=0
            END IF
C
            C127PIT=C127PID(IREC)
            C127SIT=C127SID(IREC)
C
            IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23) THEN
C              Polyline
               WRITE(LU,'(5A,I10)')'"',C127PIT(1:LPID),'","',
     &                                 C127SIT(1:LSID),'",',-NV
               DO NPO=NPOANF,NPOEND
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                  WRITE(LU,*) XPOINT(ISPO),YPOINT(ISPO)
               END DO
            ELSE IF(ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25)THEN
C              zuerst nur positives Polygon
               IF (APART(NPA).GT.0.0) THEN
                  WRITE(LU,'(5A,I10)')'"',C127PIT(1:LPID),'","',
     &                                    C127SIT(1:LSID),'",',NV
                  DO NPO=NPOANF,NPOEND
                     ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                     WRITE(LU,*) XPOINT(ISPO),YPOINT(ISPO)
                     IF (NPO.EQ.NPOANF) THEN
                        XPANF=XPOINT(ISPO)
                        YPANF=YPOINT(ISPO)
                     END IF
                  END DO
               END IF
            END IF
         END DO
C
         IF (ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
            IF (ANEG(IREC).LT.0.0.AND.APOS(IREC).GT.0.0) THEN

C              nochmals Schleife Åber Polygonteile (NUMPARTS)
               DO NPA=1,NUMPARTS
                  IF (IREC.GT.1) THEN
                     ISPA=ISUMPARTS(IREC-1)+NPA
                  ELSE
                     ISPA=NPA
                  END IF
Calt              NPOANF=IPARTS(ISPA)+1
                  NPOANF=IPARTS(ISPA)
                  IF (NPA.EQ.NUMPARTS) THEN
                     NPOEND=NUMPOINTS
                  ELSE
                     NPOEND=IPARTS(ISPA+1)+1-1
                  END IF
C                 negative Polygone
                  IF (APART(NPA).LT.0.0) THEN
                     DO NPO=NPOANF,NPOEND
                        ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                        WRITE(LU,*) XPOINT(ISPO),YPOINT(ISPO)
                     END DO
                     WRITE(LU,*) XPANF,YPANF
                  END IF
               END DO
            END IF
         END IF
C
      END DO
C
      WRITE(7,*) 'SIZES_OF_SHP: RETURN'
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE READ_SHX (LU,SHX,ISHPTYP,IOFFSETBYTES,ICONLENBYTES,
     &                     NUMRECS,IDIM)
C=======================================================================
C
C     Unterprogramm zum Lesen der ESRI-SHX-Datei
C
C     *.shp = MAIN FILE
C     *.shx = INDEX FILE
C     *.dbf = DBASE FILE
C
C=======================================================================
C     INDEX FILE HEADER - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld         Wert       Typ  Order  Variablen in LESSHX
C-----------------------------------------------------------------------
C     Byte 0    FileCode     9994        I    Big   CFILCODBIG,IFILCOD
C     Byte 4    Unused       0           I    Big
C     Byte 8    Unused       0           I    Big
C     Byte 12   Unused       0           I    Big
C     Byte 16   Unused       0           I    Big
C     Byte 20   Unused       0           I    Big
C     Byte 24   FileLength   FileLength  I    Big   CFILLENBIG,IFILLEN
C     Byte 28   Version      1000        I    Lit   IVERSION
C     Byte 32   ShapeType    ShapeType   I    Lit   ISHPTYP
C     Byte 36   BoundingBox  Xmin        D    Lit   XMIN
C     Byte 44   BoundingBox  Ymin        D    Lit   XMIN
C     Byte 52   BoundingBox  Xmax        D    Lit   YMIN
C     Byte 60   BoundingBox  Ymax        D    Lit   YMAX
C     Byte 68*  BoundingBox  Zmin        D    Lit   ZMIN
C     Byte 76*  BoundingBox  Zmax        D    Lit   ZMAX
C     Byte 84*  BoundingBox  Mmin        D    Lit   MMIN
C     Byte 92*  BoundingBox  Mmax        D    Lit   MMAX
C     (* = unused, with value 0.0, if not Measured or Z type)
C=======================================================================
C     INDEX FILE HEADER - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=255) SHX
      CHARACTER (LEN=100) CFILHED
      CHARACTER (LEN=4) CFILCODBIG,CFILLENBIG
      INTEGER*4 IFILCOD,IFILLEN,IVERSION,ISHPTYP
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX
C=======================================================================
C     INDEX FILE RECORDS - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld          Wert          Typ  Order  Namen in LESSHX
C-----------------------------------------------------------------------
C     Byte 0    Offset        Offset         I    Big   IOFFSET
C     Byte 4    ContentLength ContentLength  I    Big   ICONLEN
C
C     wobei:
C     IOFFSET = Summe von (4 + Content Length) bis zum aktuellen Record
C     IOFFSET wird gemessen in "Words", 1 Byte = 2 Words
C
C-----------------------------------------------------------------------
C     INDEX FILE RECORDS - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=8) CSHXREC
      CHARACTER (LEN=4) COFFSETBIG,CCONLENBIG
      INTEGER*4 IOFFSET,ICONLEN,IREC
      INTEGER*4 IOFFSETBYTES(*),ICONLENBYTES(*)
C=======================================================================
C
      WRITE(7,'(T2,A,A)') 'OPEN ',SHX
      OPEN(LU,FILE=SHX,ACCESS='TRANSPARENT',STATUS='OLD')
C
      READ(LU) CFILHED
C     Eingelesene Bytes
      NREADBYTES=100
      CFILCODBIG=CFILHED(1:4)
      CFILLENBIG=CFILHED(25:28)
      CALL CBIG_TO_ILIT (CFILCODBIG,IFILCOD)
      CALL CBIG_TO_ILIT (CFILLENBIG,IFILLEN)
      NFILEBYTES=IFILLEN*2
      READ(CFILHED(29:32),'(A4)') IVERSION
      READ(CFILHED(33:36),'(A4)') ISHPTYP
      READ(CFILHED(37:44),'(A8)') XMIN
      READ(CFILHED(45:52),'(A8)') YMIN
      READ(CFILHED(53:60),'(A8)') XMAX
      READ(CFILHED(61:68),'(A8)') YMAX
      ITYP=ISHPTYP
      IF (ITYP.GT.10.AND.ITYP.LT.19) THEN
         READ(CFILHED(69:76),'(A8)') ZMIN
         READ(CFILHED(77:84),'(A8)') ZMAX
      ELSE
         ZMIN=0.0
         ZMAX=0.0
      END IF
      IF (ITYP.GT.20.AND.ITYP.LT.29) THEN
         READ(CFILHED(85:92),'(A8)') MMIN
         READ(CFILHED(93:100),'(A8)') MMAX
      ELSE
         MMIN=0.0
         MMAX=0.0
      END IF
c     PRINT *
c     PRINT '(T2,A)','SHX-FILE-HEADER'
c     PRINT *
c     PRINT '(T2,A,I10)','File Code = ',IFILCOD
c     PRINT '(T2,A,I10)','FileBytes = ',NFILEBYTES
c     PRINT '(T2,A,I10)','  Version = ',IVERSION
c     PRINT '(T2,A,I10)','Shapetype = ',ISHPTYP
c     PRINT '(T2,A,F12.5)','     XMIN = ',XMIN
c     PRINT '(T2,A,F12.5)','     YMIN = ',YMIN
c     PRINT '(T2,A,F12.5)','     XMAX = ',XMAX
c     PRINT '(T2,A,F12.5)','     YMAX = ',YMAX
c     PRINT '(T2,A,F12.5)','     ZMIN = ',ZMIN
c     PRINT '(T2,A,F12.5)','     ZMAX = ',ZMAX
c     PRINT '(T2,A,F12.5)','     MMIN = ',MMIN
c     PRINT '(T2,A,F12.5)','     MMAX = ',MMAX
c     PRINT *
c     PRINT '(T2,A)','SHX-FILE-RECORDS'
c     PRINT *
C
      IREC=0
C
    1 READ(LU) CSHXREC
      IREC=IREC+1
      CALL DIMTES (IREC,IDIM,1050)
c     WRITE(7,*) IREC
C     Eingelesene Bytes
      NREADBYTES=NREADBYTES+8
C     Felder auslesen
      COFFSETBIG=CSHXREC(1:4)
      CCONLENBIG=CSHXREC(5:8)
      CALL CBIG_TO_ILIT (COFFSETBIG,IOFFSET)
      CALL CBIG_TO_ILIT (CCONLENBIG,ICONLEN)
      IOFFSETBYTES(IREC)=IOFFSET*2
      ICONLENBYTES(IREC)=ICONLEN*2
c     PRINT '(T2,A,I10)',' Offset Bytes = ',IOFFSETBYTES(IREC)
c     PRINT '(T2,A,I10)','Content Bytes = ',ICONLENBYTES(IREC)
C
c     PRINT '(T2,A,I10)',' NREADBYTES = ',NREADBYTES
      IF (NREADBYTES.LT.NFILEBYTES) GOTO 1
C
      NUMRECS=IREC
C
      CLOSE (LU,STATUS='KEEP')
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_SHX (LU,SHXOUT,ISHPTYP,IOFFSETBYTES,ICONLENBYTES,
     &                      NUMRECS,XMIN,YMIN,XMAX,YMAX,
     &                      ZMIN,ZMAX,MMIN,MMAX)
C=======================================================================
C
C     Unterprogramm zum Schreiben der ESRI-SHX-Datei
C
C     *.shp = MAIN FILE
C     *.shx = INDEX FILE
C     *.dbf = DBASE FILE
C
C=======================================================================
C     INDEX FILE HEADER - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld         Wert       Typ  Order  Variablen in LESSHX
C-----------------------------------------------------------------------
C     Byte 0    FileCode     9994        I    Big   CFILCODBIG,IFILCOD
C     Byte 4    Unused       0           I    Big
C     Byte 8    Unused       0           I    Big
C     Byte 12   Unused       0           I    Big
C     Byte 16   Unused       0           I    Big
C     Byte 20   Unused       0           I    Big
C     Byte 24   FileLength   FileLength  I    Big   CFILLENBIG,IFILLEN
C     Byte 28   Version      1000        I    Lit   IVERSION
C     Byte 32   ShapeType    ShapeType   I    Lit   ISHPTYP
C     Byte 36   BoundingBox  Xmin        D    Lit   XMIN
C     Byte 44   BoundingBox  Ymin        D    Lit   XMIN
C     Byte 52   BoundingBox  Xmax        D    Lit   YMIN
C     Byte 60   BoundingBox  Ymax        D    Lit   YMAX
C     Byte 68*  BoundingBox  Zmin        D    Lit   ZMIN
C     Byte 76*  BoundingBox  Zmax        D    Lit   ZMAX
C     Byte 84*  BoundingBox  Mmin        D    Lit   MMIN
C     Byte 92*  BoundingBox  Mmax        D    Lit   MMAX
C     (* = unused, with value 0.0, if not Measured or Z type)
C=======================================================================
C     INDEX FILE HEADER - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=255) SHXOUT
      CHARACTER (LEN=100) CFILHED
      CHARACTER (LEN=4) CFILCODBIG,CFILLENBIG
      INTEGER*4 IFILCOD,IFILLEN,IVERSION,ISHPTYP
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX
C=======================================================================
C     INDEX FILE RECORDS - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld          Wert          Typ  Order  Namen in LESSHX
C-----------------------------------------------------------------------
C     Byte 0    Offset        Offset         I    Big   IOFFSET
C     Byte 4    ContentLength ContentLength  I    Big   ICONLEN
C
C     wobei:
C     IOFFSET = Summe von (4 + Content Length) bis zum aktuellen Record
C     IOFFSET wird gemessen in "Words", 1 Byte = 2 Words
C
C-----------------------------------------------------------------------
C     INDEX FILE RECORDS - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=8) CSHXREC
      CHARACTER (LEN=4) COFFSETBIG,CCONLENBIG
      INTEGER*4 IOFFSET,ICONLEN,IREC
      INTEGER*4 IOFFSETBYTES(*),ICONLENBYTES(*)
C=======================================================================
C
      WRITE(7,'(T2,A,A)') 'OPEN ',SHXOUT
c     OPEN(LU,FILE=SHXOUT,ACCESS='TRANSPARENT',STATUS='UNKNOWN')
      OPEN(LU,FILE='TEMP.SHX',ACCESS='TRANSPARENT',STATUS='UNKNOWN')
C
      IFILCOD=9994
      CALL ILIT_TO_CBIG (IFILCOD,CFILCODBIG)
      CFILHED(1:4)=CFILCODBIG
C
      NFILEBYTES=100+NUMRECS*8
      IFILLEN=NFILEBYTES/2
c     PRINT '(T2,A,I10)','FileBytes = ',NFILEBYTES
c     PRINT '(T2,A,I10)','  FileLen = ',IFILLEN
      CALL ILIT_TO_CBIG (IFILLEN,CFILLENBIG)
      CFILHED(25:28)=CFILLENBIG
C
      IVERSION=1000
      WRITE(CFILHED(29:32),'(A4)') IVERSION
      WRITE(CFILHED(33:36),'(A4)') ISHPTYP
      WRITE(CFILHED(37:44),'(A8)') XMIN
      WRITE(CFILHED(45:52),'(A8)') YMIN
      WRITE(CFILHED(53:60),'(A8)') XMAX
      WRITE(CFILHED(61:68),'(A8)') YMAX
      WRITE(CFILHED(69:76),'(A8)') ZMIN
      WRITE(CFILHED(77:84),'(A8)') ZMAX
      WRITE(CFILHED(85:92),'(A8)') MMIN
      WRITE(CFILHED(93:100),'(A8)') MMAX
C
      WRITE(LU) CFILHED
C
      DO IREC=1,NUMRECS
         IOFFSET=IOFFSETBYTES(IREC)/2
         ICONLEN=ICONLENBYTES(IREC)/2
         CALL ILIT_TO_CBIG (IOFFSET,COFFSETBIG)
         CALL ILIT_TO_CBIG (ICONLEN,CCONLENBIG)
         CSHXREC(1:4)=COFFSETBIG
         CSHXREC(5:8)=CCONLENBIG
         WRITE(LU) CSHXREC
      END DO
C
      CLOSE (LU,STATUS='KEEP')
C
      WRITE(7,'(T2,A,A)') 'SHXOUT = ',SHXOUT
C
      DO IP=1,255
         IF(SHXOUT(IP:IP).EQ.'.') LSHX=IP+3
      END DO
C
      CALL DOSCOPY('TEMP.SHX',8,SHXOUT,LSHX)
C
      CALL SYSTEM("DEL TEMP.SHX")
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CHECK_SHPSHX (ITYPSHP,NRECSHP,ISUMPARTS,ISUMPOINTS,
     &           ITYPSHX,NRECSHX,IOFFSETSHX,ICONLENSHX,IMOPT)
C-----------------------------------------------------------------------
      INTEGER*4 ITYPSHX,ITYPSHP
      INTEGER*4 ISUMPARTS(*),ISUMPOINTS(*)
      INTEGER*4 NUMPARTS,NUMPOINTS
      INTEGER*4 IOFFSETSHX(*),ICONLENSHX(*)
      INTEGER*4 IOFFSETSHP,ICONLENSHP
C
      WRITE (7,'(T2,A,I10)') 'CHECK_SHPSHX: ITYPSHP = ',ITYPSHP
      WRITE (7,'(T2,A,I10)') 'CHECK_SHPSHX: ITYPSHX = ',ITYPSHX
      WRITE (7,'(T2,A,I10)') 'CHECK_SHPSHX: NRECSHP = ',NRECSHP
      WRITE (7,'(T2,A,I10)') 'CHECK_SHPSHX: NRECSHX = ',NRECSHX
C
      IF (ITYPSHP.NE.ITYPSHX) WRITE(7,'(A)') 'Shape-Typ passt nicht!'
      IF (NRECSHP.NE.NRECSHX) WRITE(7,'(A)') 'Recordanzahl passt nicht!'
C
      DO NREC=1,NRECSHP
         IF (NREC.EQ.1) THEN
            IOFFSETSHP=100
            NUMPARTS=ISUMPARTS(NREC)
            NUMPOINTS=ISUMPOINTS(NREC)
         ELSE
            IOFFSETSHP=IOFFSETSHP+8+ICONLENSHP
            NUMPARTS=ISUMPARTS(NREC)-ISUMPARTS(NREC-1)
            NUMPOINTS=ISUMPOINTS(NREC)-ISUMPOINTS(NREC-1)
         END IF
         IF (ITYPSHP.EQ.1) THEN
            ICONLENSHP=20
         END IF
         IF (ITYPSHP.EQ.11.AND.IMOPT.EQ.0) THEN
C           nur Z-Werte
            ICONLENSHP=28
         ELSE IF (ITYPSHP.EQ.11.AND.IMOPT.EQ.1) THEN
C           Z-Werte und M-Werte
            ICONLENSHP=36
         END IF
         IF (ITYPSHP.EQ.21) THEN
C           Points mit M-Werten
            ICONLENSHP=28
         END IF
         IF (ITYPSHP.EQ.8) THEN
            ICONLENSHP=4+32+4+16*NUMPOINTS
         END IF
         IF (ITYPSHP.EQ.18.AND.IMOPT.EQ.0) THEN
C           nur Z-Werte
            ICONLENSHP=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
         ELSE IF (ITYPSHP.EQ.18.AND.IMOPT.EQ.1) THEN
C           Z-Werte und M-Werte
            ICONLENSHP=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
            ICONLENSHP=ICONLENSHP+16+(8*NUMPOINTS)
         END IF
         IF (ITYPSHP.EQ.28.AND.IMOPT.EQ.0) THEN
C           nur Multipoints ohne M-Werte
            ICONLENSHP=40+(16*NUMPOINTS)
         ELSE IF (ITYPSHP.EQ.28.AND.IMOPT.EQ.1) THEN
C           Multipoints und M-Werte
            ICONLENSHP=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
         END IF
         IF (ITYPSHP.EQ.3.OR.ITYPSHP.EQ.5) THEN
            ICONLENSHP=4+32+4+4+4*NUMPARTS+16*NUMPOINTS
         END IF
         IF ((ITYPSHP.EQ.13.OR.ITYPSHP.EQ.15).AND.IMOPT.EQ.0) THEN
C           nur Z-Werte
            ICONLENSHP=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
         ELSE IF ((ITYPSHP.EQ.13.OR.ITYPSHP.EQ.15).AND.IMOPT.EQ.1) THEN
C           Z-Werte und M-Werte
            ICONLENSHP=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
            ICONLENSHP=ICONLENSHP+16+(8*NUMPOINTS)
         END IF
         IF ((ITYPSHP.EQ.23.OR.ITYPSHP.EQ.25).AND.IMOPT.EQ.0) THEN
C           nur Polylinien oder Polygone ohne M-Werte
            ICONLENSHP=44+(4*NUMPARTS)+(16*NUMPOINTS)
         ELSE IF ((ITYPSHP.EQ.23.OR.ITYPSHP.EQ.25).AND.IMOPT.EQ.1) THEN
C           Polylinien oder Polygone mit M-Werten
            ICONLENSHP=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
         END IF
         IF (IOFFSETSHP.NE.IOFFSETSHX(NREC)) THEN
            WRITE(7,'(A,I10)') '      Record = ',NREC
            WRITE(7,'(A,I10)') ' Soll-Offset = ',IOFFSETSHX(NREC)
            WRITE(7,'(A,I10)') ' ber. Offset = ',IOFFSETSHP
            WRITE(7,'(A)') 'Offset passt nicht!'
         END IF
         IF (ICONLENSHP.NE.ICONLENSHX(NREC)) THEN
            WRITE(7,'(A,I10)') '      Record = ',NREC
            WRITE(7,'(A,I10)') 'Soll-Content = ',ICONLENSHX(NREC)
            WRITE(7,'(A,I10)') 'ber. Content = ',ICONLENSHP
            WRITE(7,'(A)') 'Content Lenght passt nicht!'
         END IF
      END DO
C
      RETURN
      END

C     Typ=18
C           zu lesende Bytes wenn nur Z vorhanden
C           NZB=40+(16*NUMPOINTS)+16+(8*NUMPOINTS)
C           optionale zu lesende Bytes wenn M vorhanden
C           NMB=NZB+16+(8*NUMPOINTS)

C     Typ=13 und 15
C           zu lesende Bytes wenn nur Z vorhanden
C           NZB=44+(4*NUMPARTS)+(16*NUMPOINTS)+16+(8*NUMPOINTS)
C           optionale zu lesende Bytes wenn M vorhanden
C           NMB=NZB+16+(8*NUMPOINTS)

C-----------------------------------------------------------------------
      SUBROUTINE AREAXY (IMAX,X,Y,AREA)
C-----------------------------------------------------------------------
      DOUBLE PRECISION X(*),Y(*),SPLUS,SMINUS,AREA
      DOUBLE PRECISION XMIN,YMIN
      XMIN=1.0*10.**38
      YMIN=1.0*10.**38
!     DATENMINIMUM ALS URSPRUNG
      DO I=1,IMAX
         IF (X(I).LT.XMIN) XMIN=X(I)
         IF (Y(I).LT.YMIN) YMIN=Y(I)
      END DO
      I=0
!     AUF XMIN,YMIN UMRECHNEN
      DO I=1,IMAX
         X(I)=X(I)-XMIN
         Y(I)=Y(I)-YMIN
      END DO
      I=0
!
!     BERECHNEN DER FLéCHE
      SPLUS=0.0
      SMINUS=0.0
      DO I=1,IMAX-1
         SPLUS=SPLUS+X(I)*Y(I+1)
      END DO
      I=0
      SPLUS=SPLUS+X(IMAX)*Y(1)
      DO L=1,IMAX-1
         SMINUS=SMINUS+Y(L)*X(L+1)
      END DO
      L=0
      SMINUS=SMINUS+Y(IMAX)*X(1)
      AREA=0.5*(SPLUS-SMINUS)
C     AREA=ABS(AREA)
!
!     AUF URSPRöNGLICHE WERTE ZURöCKRECHNEN
      DO I=1,IMAX
         X(I)=X(I)+XMIN
         Y(I)=Y(I)+YMIN
      END DO
!
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_SHP (SHP,LUSHP,LUSHX,ISHPTYP,XMIN,XMAX,YMIN,YMAX,
     &           ZMIN,ZMAX,MMIN,MMAX,NUMRECS,ISUMPARTS,ISUMPOINTS,
     &           IPARTS,XPOINT,YPOINT,ZPOINT,MPOINT,IMOPT)
C=======================================================================
C
C     Unterprogramm zum Schreiben der ESRI-SHP- und der SHX-Datei
C
C     *.shp = MAIN FILE
C     *.shx = INDEX FILE
C     *.dbf = DBASE FILE
C
C=======================================================================
C     MAIN FILE HEADER - DESCRIPTION
C-----------------------------------------------------------------------
C     Position  Feld         Wert       Typ  Order  Variablen
C-----------------------------------------------------------------------
C     Byte 0    FileCode     9994        I    Big   CFILCODBIG,IFILCOD
C     Byte 4    Unused       0           I    Big
C     Byte 8    Unused       0           I    Big
C     Byte 12   Unused       0           I    Big
C     Byte 16   Unused       0           I    Big
C     Byte 20   Unused       0           I    Big
C     Byte 24   FileLength   FileLength  I    Big   CFILLENBIG,IFILLEN
C     Byte 28   Version      1000        I    Lit   IVERSION
C     Byte 32   ShapeType    ShapeType   I    Lit   ISHPTYP
C     Byte 36   BoundingBox  Xmin        D    Lit   XMIN
C     Byte 44   BoundingBox  Ymin        D    Lit   XMIN
C     Byte 52   BoundingBox  Xmax        D    Lit   YMIN
C     Byte 60   BoundingBox  Ymax        D    Lit   YMAX
C     Byte 68*  BoundingBox  Zmin        D    Lit   ZMIN
C     Byte 76*  BoundingBox  Zmax        D    Lit   ZMAX
C     Byte 84*  BoundingBox  Mmin        D    Lit   MMIN
C     Byte 92*  BoundingBox  Mmax        D    Lit   MMAX
C     (* = unused, with value 0.0, if not Measured or Z type)
C-----------------------------------------------------------------------
C     Measure M optional in Shape-File-Typen 11,13,15,18,21,23,25,28
C     IMOPT=0   kein Measure M in Shape-Datei
C     IMOPT=1   Measure M in Shape-Datei vorbelegt
C=======================================================================
C     MAIN FILE HEADER - VEREINBARUNGEN
C-----------------------------------------------------------------------
      CHARACTER (LEN=255) SHP,SHX
      CHARACTER (LEN=100) C100SHPHED,C100SHXHED
      CHARACTER (LEN=4) CFILCODBIG,CFILLENBIG,C4,COFFSETBIG
      CHARACTER (LEN=8) C8
      CHARACTER (LEN=16) C16
      CHARACTER (LEN=40) C40
      CHARACTER (LEN=36) C36
      INTEGER*4 IFILCOD,IFILLEN,IVERSION,ISHPTYP,ITYP
      DOUBLE PRECISION XMIN,YMIN,XMAX,YMAX,ZMIN,ZMAX,MMIN,MMAX
C-----------------------------------------------------------------------
C     MAIN FILE RECORD HEADER
C-----------------------------------------------------------------------
      CHARACTER (LEN=8) CRECHED
      CHARACTER (LEN=4) CRECNUMBIG,CCONLENBIG
      INTEGER*4 IREC,ICONLEN
C=======================================================================
C     MAIN FILE RECORD CONTENTS - DESCRIPTION
C=======================================================================
C     Point Record Content - Description (ShapeType = 1)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  1        I   Lit  1        ISHPTYP
C     Byte 4    XPoint   X        D   Lit  1        XPOINT()
C     Byte 12   YPoint   Y        D   Lit  1        YPOINT()
C=======================================================================
C     PointZ Record Content - Description (ShapeType = 11)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  11       I   Lit  1        ISHPTYP
C     Byte 4    XPoint   X        D   Lit  1        XPOINT()
C     Byte 12   YPoint   Y        D   Lit  1        YPOINT()
C     Byte 20   ZPoint   Z        D   Lit  1        ZPOINT()
C     Byte 28   MPoint   M        D   Lit  1        MPOINT()
C=======================================================================
C     PolyLine Record Content - Description (ShapeType = 3)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  3        I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     (X = 44 + 4 * NumParts)
C=======================================================================
C     PolyLineZ Record Content - Description (ShapeType = 13)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  13       I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     Byte Y    Zmin     Zmin     D   Lit  1        ZBLO
C     Byte Y+8  Zmax     Zmax     D   Lit  1        ZBHI
C     Byte Y+16 Zarray   Zarray   D   Lit  NPoints  ZPOINT()
C    *Byte Z    Mmin     Mmin     D   Lit  1        MBLO
C    *Byte Z+8  Mmax     Mmax     D   Lit  1        MBHI
C    *Byte Z+16 Marray   Marray   D   Lit  NPoints  MPOINT()
C     (X = 44 + (4 * NumParts)
C     (Y = X + (16 * NumPoints))
C     (Z = Y + 16 + (8 * NumPoints))  ( * optional )
C=======================================================================
C     Polygon Record Content - Description (ShapeType = 5)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  5        I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     (X = 44 + 4 * NumParts)
C=======================================================================
C     PolygonZ Record Content - Description (ShapeType = 15)
C-----------------------------------------------------------------------
C     Position  Feld     Wert    Typ  Ord  Anzahl  Variablennamen
C-----------------------------------------------------------------------
C     Byte 0    ShpType  15       I   Lit  1        ISHPTYP
C     Byte 4    Box      Box      D   Lit  4        XBLO,XBHI,YBLO,YBHI
C     Byte 36   NParts   NParts   I   Lit  1        NUMPARTS
C     Byte 40   NPoints  NPoints  I   Lit  1        NUMPOINTS
C     Byte 44   Parts    Parts    I   Lit  NParts   IPARTS()
C     Byte X    Points   Points   P   Lit  NPoints  XPOINT(),YPOINT()
C     Byte Y    Zmin     Zmin     D   Lit  1        ZBLO
C     Byte Y+8  Zmax     Zmax     D   Lit  1        ZBHI
C     Byte Y+16 Zarray   Zarray   D   Lit  NPoints  ZPOINT()
C    *Byte Z    Mmin     Mmin     D   Lit  1        MBLO
C    *Byte Z+8  Mmax     Mmax     D   Lit  1        MBHI
C    *Byte Z+16 Marray   Marray   D   Lit  NPoints  MPOINT()
C     (X = 44 + (4 * NumParts)
C     (Y = X + (16 * NumPoints))
C     (Z = Y + 16 + (8 * NumPoints))  ( * optional )
C=======================================================================
C     MAIN FILE RECORD CONTENTS - VEREINBARUNGEN
C
C     NUMRECS = Anzahl der Records im Shape-file = Anzahl Objekte
C
C     ISUMPARTS() = Summe der NUMPARTS incl. des aktuellen Records
C     ISUMPOINTS() = Summe der NUMPOINTS incl. des aktuellen Records
C-----------------------------------------------------------------------
      DOUBLE PRECISION XBLO,YBLO,XBHI,YBHI,ZBLO,ZBHI,MBLO,MBHI
C     (Variablen sind nur Dummys; evtl. Dimensionieren f¸r Records)
      INTEGER*4 NUMPARTS,NUMPOINTS
      INTEGER*4 ISUMPARTS(*),ISUMPOINTS(*)
      INTEGER*4 IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
      DOUBLE PRECISION ZPOINT(*),MPOINT(*)
C
      XMIN=1.0*10.**38
      YMIN=1.0*10.**38
      ZMIN=1.0*10.**38
      MMIN=1.0*10.**38
      XMAX=-1.0*10.**38
      YMAX=-1.0*10.**38
      ZMAX=-1.0*10.**38
      MMAX=-1.0*10.**38
      NZB=0
      NMB=0
C
      NPARTS=ISUMPARTS(NUMRECS)
      NPOINTS=ISUMPOINTS(NUMRECS)
      IVERSION=1000
      IFILCOD=9994
C
      IFILEBYTES=100+8*NUMRECS
      IFILEBYTES=IFILEBYTES+IBYTES(ISHPTYP,NPARTS,NPOINTS,IMOPT,NUMRECS)
C
      IFILLEN=IFILEBYTES/2
C
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP   IVERSION = ',IVERSION
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP    IFILCOD = ',IFILCOD
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP    ISHPTYP = ',ISHPTYP
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP      IMOPT = ',IMOPT
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP    NUMRECS = ',NUMRECS
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP     NPARTS = ',NPARTS
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP    NPOINTS = ',NPOINTS
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP IFILEBYTES = ',IFILEBYTES
      WRITE (7,'(T2,A,I10)') 'WRITE_SHP    IFILLEN = ',IFILLEN
C
      DO NP=1,NPOINTS
         IF (XPOINT(NP).LT.XMIN) XMIN=XPOINT(NP)
         IF (YPOINT(NP).LT.YMIN) YMIN=YPOINT(NP)
         IF (ZPOINT(NP).LT.ZMIN) ZMIN=ZPOINT(NP)
         IF (MPOINT(NP).LT.MMIN) MMIN=MPOINT(NP)
         IF (XPOINT(NP).GT.XMAX) XMAX=XPOINT(NP)
         IF (YPOINT(NP).GT.YMAX) YMAX=YPOINT(NP)
         IF (ZPOINT(NP).GT.ZMAX) ZMAX=ZPOINT(NP)
         IF (MPOINT(NP).GT.MMAX) MMAX=MPOINT(NP)
      END DO
C
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    XMIN = ',XMIN
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    YMIN = ',YMIN
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    XMAX = ',XMAX
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    YMAX = ',YMAX
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    ZMIN = ',ZMIN
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    ZMAX = ',ZMAX
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    MMIN = ',MMIN
      WRITE (7,'(T2,A,F15.5)')'WRITE_SHP    MMAX = ',MMAX
C
      CALL ILIT_TO_CBIG (IFILCOD,CFILCODBIG)
      C100SHPHED(1:4)=CFILCODBIG
C
      NULL=0
      CALL ILIT_TO_CBIG (NULL,C4)
      C100SHPHED(5:8)=C4
      C100SHPHED(9:12)=C4
      C100SHPHED(13:16)=C4
      C100SHPHED(17:20)=C4
      C100SHPHED(21:24)=C4
C
      CALL ILIT_TO_CBIG (IFILLEN,CFILLENBIG)
      C100SHPHED(25:28)=CFILLENBIG
C
      WRITE(C100SHPHED(29:32),'(A4)') IVERSION
      WRITE(C100SHPHED(33:36),'(A4)') ISHPTYP
      WRITE(C100SHPHED(37:44),'(A8)') XMIN
      WRITE(C100SHPHED(45:52),'(A8)') YMIN
      WRITE(C100SHPHED(53:60),'(A8)') XMAX
      WRITE(C100SHPHED(61:68),'(A8)') YMAX
      WRITE(C100SHPHED(69:76),'(A8)') ZMIN
      WRITE(C100SHPHED(77:84),'(A8)') ZMAX
      WRITE(C100SHPHED(85:92),'(A8)') MMIN
      WRITE(C100SHPHED(93:100),'(A8)') MMAX
C
      C100SHXHED=C100SHPHED
      ISHXLEN=50+4*NUMRECS
      CALL ILIT_TO_CBIG (ISHXLEN,C4)
      C100SHXHED(25:28)=C4
C
      SHX=SHP
      DO IA=1,252
         IE=IA+3
         IF (SHP(IA:IE).EQ.'.shp') THEN
            SHX(IA:IE)='.shx'
            LSHP=IE
         END IF
         IF (SHP(IA:IE).EQ.'.Shp') THEN
            SHX(IA:IE)='.Shx'
            LSHP=IE
         END IF
         IF (SHP(IA:IE).EQ.'.SHp') THEN
            SHX(IA:IE)='.SHx'
            LSHP=IE
         END IF
         IF (SHP(IA:IE).EQ.'.SHP') THEN
            SHX(IA:IE)='.SHX'
            LSHP=IE
         END IF
      END DO
C
      OPEN(LUSHP,FILE='TEMP.SHP',STATUS='UNKNOWN')
      CLOSE(LUSHP,STATUS='DELETE')
C
      OPEN(LUSHP,FILE='TEMP.SHP',ACCESS='TRANSPARENT',STATUS='UNKNOWN')
C
      WRITE(LUSHP) C100SHPHED
C
C
      OPEN(LUSHX,FILE='TEMP.SHX',STATUS='UNKNOWN')
      CLOSE(LUSHX,STATUS='DELETE')
C
      OPEN(LUSHX,FILE='TEMP.SHX',ACCESS='TRANSPARENT',STATUS='UNKNOWN')
C
      WRITE(LUSHX) C100SHXHED
C
      ICONTENTBYTES=0
C
      DO IREC=1,NUMRECS
C
         XBLO=1.0*10.**38
         YBLO=1.0*10.**38
         ZBLO=1.0*10.**38
         MBLO=1.0*10.**38
         XBHI=-1.0*10.**38
         YBHI=-1.0*10.**38
         ZBHI=-1.0*10.**38
         MBHI=-1.0*10.**38
C
         IF (IREC.GT.1) THEN
            NUMPARTS=ISUMPARTS(IREC)-ISUMPARTS(IREC-1)
            NUMPOINTS=ISUMPOINTS(IREC)-ISUMPOINTS(IREC-1)
         ELSE
            NUMPARTS=ISUMPARTS(IREC)
            NUMPOINTS=ISUMPOINTS(IREC)
         END IF
C
         IF (IREC.EQ.1) THEN
            IOFFSETBYTES=100
         ELSE
            IOFFSETBYTES=IOFFSETBYTES+8+ICONTENTBYTES
         END IF
C
         ICONTENTBYTES=IBYTES(ISHPTYP,NUMPARTS,NUMPOINTS,IMOPT,1)
C
c        WRITE (7,'(T2,A,I10)') 'Offset  Bytes = ',IOFFSETBYTES
c        WRITE (7,'(T2,A,I10)') 'Content Bytes = ',ICONTENTBYTES
C
         IOFFSET=IOFFSETBYTES/2
         CALL ILIT_TO_CBIG (IOFFSET,COFFSETBIG)
C
         ICONLEN=ICONTENTBYTES/2
C
         CALL ILIT_TO_CBIG (IREC,CRECNUMBIG)
         CALL ILIT_TO_CBIG (ICONLEN,CCONLENBIG)
C
         CRECHED(1:4)=CRECNUMBIG
         CRECHED(5:8)=CCONLENBIG
C
         WRITE(LUSHP) CRECHED
C
         WRITE(LUSHX) COFFSETBIG,CRECHED(5:8)
C
         WRITE(C4,'(A4)') ISHPTYP
         WRITE(LUSHP) C4
C
         IF (ISHPTYP.EQ.1) THEN
C           Ausgabe
C           ISHPTYP = 1   ShapeType = Point
C
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,1)
            WRITE(C16(1:8),'(A8)') XPOINT(ISPO)
            WRITE(C16(9:16),'(A8)') YPOINT(ISPO)
            WRITE(LUSHP) C16
C
         END IF
C
         IF (ISHPTYP.EQ.11) THEN
C           Ausgabe
C           ISHPTYP = 11  ShapeType = PointZ
C
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,1)
            WRITE(C16(1:8),'(A8)') XPOINT(ISPO)
            WRITE(C16(9:16),'(A8)') YPOINT(ISPO)
            WRITE(LUSHP) C16
            IF (IMOPT.EQ.1) THEN
               WRITE(C16(1:8),'(A8)') ZPOINT(ISPO)
               WRITE(C16(9:16),'(A8)') MPOINT(ISPO)
               WRITE(LUSHP) C16
            ELSE
               WRITE(C8,'(A8)') ZPOINT(ISPO)
               WRITE(LUSHP) C8
            END IF
C
         END IF
C
         IF (ISHPTYP.EQ.21) THEN
C           Ausgabe
C           ISHPTYP = 21  ShapeType = PointM
C
            ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,1)
            WRITE(C16(1:8),'(A8)') XPOINT(ISPO)
            WRITE(C16(9:16),'(A8)') YPOINT(ISPO)
            WRITE(LUSHP) C16
            IF (IMOPT.EQ.1) THEN
               WRITE(C8,'(A8)') MPOINT(ISPO)
               WRITE(LUSHP) C8
            END IF
C
         END IF
C
         IF (ISHPTYP.EQ.8.OR.ISHPTYP.EQ.18.OR.ISHPTYP.EQ.28) THEN
C           Ermitteln der BOX und Ausgabe
C           ISHPTYP = 8   ShapeType = MultiPoint
C           ISHPTYP = 18  ShapeType = MultiPointZ
C           ISHPTYP = 28  ShapeType = MultiPointM
C
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
               IF (XPOINT(ISPO).LT.XBLO) XBLO=XPOINT(ISPO)
               IF (XPOINT(ISPO).GT.XBHI) XBHI=XPOINT(ISPO)
               IF (YPOINT(ISPO).LT.YBLO) YBLO=YPOINT(ISPO)
               IF (YPOINT(ISPO).GT.YBHI) YBHI=YPOINT(ISPO)
               IF (ZPOINT(ISPO).LT.ZBLO) ZBLO=ZPOINT(ISPO)
               IF (ZPOINT(ISPO).GT.ZBHI) ZBHI=ZPOINT(ISPO)
               IF (MPOINT(ISPO).LT.MBLO) MBLO=MPOINT(ISPO)
               IF (MPOINT(ISPO).GT.MBHI) MBHI=MPOINT(ISPO)
            END DO
C
            WRITE(C36(1:8),'(A8)') XBLO
            WRITE(C36(9:16),'(A8)') YBLO
            WRITE(C36(17:24),'(A8)') XBHI
            WRITE(C36(25:32),'(A8)') YBHI
            WRITE(C36(33:36),'(A4)') NUMPOINTS
            WRITE(LUSHP) C36
C
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
               WRITE(C16(1:8),'(A8)') XPOINT(ISPO)
               WRITE(C16(9:16),'(A8)') YPOINT(ISPO)
               WRITE(LUSHP) C16
            END DO
C
            IF (ISHPTYP.EQ.18) THEN
               WRITE(C16(1:8),'(A8)') ZBLO
               WRITE(C16(9:16),'(A8)') ZBHI
               WRITE(LUSHP) C16
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                  WRITE(C8,'(A8)') ZPOINT(ISPO)
                  WRITE(LUSHP) C8
               END DO
               IF (IMOPT.EQ.1) THEN
                  WRITE(C16(1:8),'(A8)') MBLO
                  WRITE(C16(9:16),'(A8)') MBHI
                  WRITE(LUSHP) C16
                  DO NPO=1,NUMPOINTS
                     ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                     WRITE(C8,'(A8)') MPOINT(ISPO)
                     WRITE(LUSHP) C8
                  END DO
               END IF
            ELSE IF (ISHPTYP.EQ.28) THEN
               IF (IMOPT.EQ.1) THEN
                  WRITE(C16(1:8),'(A8)') MBLO
                  WRITE(C16(9:16),'(A8)') MBHI
                  WRITE(LUSHP) C16
                  DO NPO=1,NUMPOINTS
                     ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                     WRITE(C8,'(A8)') MPOINT(ISPO)
                     WRITE(LUSHP) C8
                  END DO
               END IF
            END IF
C
         END IF
C
         IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.13.OR.ISHPTYP.EQ.23.OR.
     &      ISHPTYP.EQ.5.OR.ISHPTYP.EQ.15.OR.ISHPTYP.EQ.25) THEN
C           Ermitteln der BOX und Ausgabe
C           ISHPTYP = 3  ShapeType = PolyLine
C           ISHPTYP = 13 ShapeType = PolyLineZ
C           ISHPTYP = 23 ShapeType = PolyLineM
C           ISHPTYP = 5  ShapeType = Polygon
C           ISHPTYP = 15 ShapeType = PolygonZ
C           ISHPTYP = 25 ShapeType = PolygonM
C
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
               IF (XPOINT(ISPO).LT.XBLO) XBLO=XPOINT(ISPO)
               IF (XPOINT(ISPO).GT.XBHI) XBHI=XPOINT(ISPO)
               IF (YPOINT(ISPO).LT.YBLO) YBLO=YPOINT(ISPO)
               IF (YPOINT(ISPO).GT.YBHI) YBHI=YPOINT(ISPO)
               IF (ZPOINT(ISPO).LT.ZBLO) ZBLO=ZPOINT(ISPO)
               IF (ZPOINT(ISPO).GT.ZBHI) ZBHI=ZPOINT(ISPO)
               IF (MPOINT(ISPO).LT.MBLO) MBLO=MPOINT(ISPO)
               IF (MPOINT(ISPO).GT.MBHI) MBHI=MPOINT(ISPO)
            END DO
C
            WRITE(C40(1:8),'(A8)') XBLO
            WRITE(C40(9:16),'(A8)') YBLO
            WRITE(C40(17:24),'(A8)') XBHI
            WRITE(C40(25:32),'(A8)') YBHI
            WRITE(C40(33:36),'(A4)') NUMPARTS
            WRITE(C40(37:40),'(A4)') NUMPOINTS
            WRITE(LUSHP) C40
C
            DO NPA=1,NUMPARTS
               ISPA=IZEIGER_VERTEX(ISUMPARTS,IREC,NPA)
c              WRITE(7,*) ISPA,IPARTS(ISPA)
               IPTMP=IPARTS(ISPA)-1
               WRITE(C4,'(A4)') IPTMP
               WRITE(LUSHP) C4
            END DO
C
            DO NPO=1,NUMPOINTS
               ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
               WRITE(C16(1:8),'(A8)') XPOINT(ISPO)
               WRITE(C16(9:16),'(A8)') YPOINT(ISPO)
               WRITE(LUSHP) C16
            END DO
C
            IF (ISHPTYP.EQ.13.OR.ISHPTYP.EQ.15) THEN
               WRITE(C16(1:8),'(A8)') ZBLO
               WRITE(C16(9:16),'(A8)') ZBHI
               WRITE(LUSHP) C16
               DO NPO=1,NUMPOINTS
                  ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                  WRITE(C8,'(A8)') ZPOINT(ISPO)
                  WRITE(LUSHP) C8
               END DO
               IF (IMOPT.EQ.1) THEN
                  WRITE(C16(1:8),'(A8)') MBLO
                  WRITE(C16(9:16),'(A8)') MBHI
                  WRITE(LUSHP) C16
                  DO NPO=1,NUMPOINTS
                     ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                     WRITE(C8,'(A8)') MPOINT(ISPO)
                     WRITE(LUSHP) C8
                  END DO
               END IF
            END IF
C
            IF (ISHPTYP.EQ.23.OR.ISHPTYP.EQ.25) THEN
               IF (IMOPT.EQ.1) THEN
                  WRITE(C16(1:8),'(A8)') MBLO
                  WRITE(C16(9:16),'(A8)') MBHI
                  WRITE(LUSHP) C16
                  DO NPO=1,NUMPOINTS
                     ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
                     WRITE(C8,'(A8)') MPOINT(ISPO)
                     WRITE(LUSHP) C8
                  END DO
               END IF
            END IF
C
         END IF
C
      END DO
C
      CLOSE (LUSHP,STATUS='KEEP')
      CLOSE (LUSHX,STATUS='KEEP')
C
      LSHX=LSHP
C
      CALL DOSCOPY('TEMP.SHP',8,SHP,LSHP)
      CALL DOSCOPY('TEMP.SHX',8,SHX,LSHX)
C
      CALL SYSTEM("DEL TEMP.SHP")
      CALL SYSTEM("DEL TEMP.SHX")
C
      RETURN
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION IANZAHL_VERTEXE(ISUMPOINTS,NREC)
C
      INTEGER*4 ISUMPOINTS(*)
      IF (NREC.EQ.1) NVER=ISUMPOINTS(NREC)
      IF (NREC.GT.1) NVER=ISUMPOINTS(NREC)-ISUMPOINTS(NREC-1)
      IANZAHL_VERTEXE=NVER
      RETURN
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION IZEIGER_VERTEX(ISUMPOINTS,NREC,NPO)
C
      INTEGER*4 ISUMPOINTS(*)
      IF (NREC.EQ.1) ISPO=NPO
      IF (NREC.GT.1) ISPO=ISUMPOINTS(NREC-1)+NPO
      IZEIGER_VERTEX=ISPO
      RETURN
      END
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IBYTES(ISHPTYP,NPARTS,NPOINTS,IMOPT,NREC)
C
C     NREC=1 wenn die Bytes in einem Record ermittelt werden
C     NREC=NUMRECS wenn die Bytes in der ganzen Datei ermittelt werden
C
      INTEGER*4 ISHPTYP,NPARTS,NPOINTS,IMOPT
C
      IF (ISHPTYP.EQ.0) THEN
         IBYTES=4*NREC
      END IF

      IF (ISHPTYP.EQ.1) THEN
c        Typ Point
         IBYTES=20*NREC
      END IF

      IF (ISHPTYP.EQ.3.OR.ISHPTYP.EQ.5) THEN
c        Typ Polyline oder Polygon
         IBYTES=44*NREC+4*NPARTS+16*NPOINTS
      END IF

      IF (ISHPTYP.EQ.8) THEN
c        Typ Multipoint
         IBYTES=40*NREC+16*NPOINTS
      END IF

      IF (ISHPTYP.EQ.11) THEN
c        Typ PointZ
         IF (IMOPT.EQ.1) IBYTES=36*NREC
         IF (IMOPT.EQ.0) IBYTES=28*NREC
      END IF

      IF (ISHPTYP.EQ.13.OR.ISHPTYP.EQ.15) THEN
c        Typ PolylineZ oder PolygonZ
         IBYTES=44*NREC+4*NPARTS+16*NPOINTS+16*NREC+8*NPOINTS
         IF (IMOPT.EQ.1) THEN
            IBYTES=IBYTES+16*NREC+8*NPOINTS
         END IF
      END IF

      IF (ISHPTYP.EQ.18) THEN
c        Typ MultipointZ
         IBYTES=40*NREC+16*NPOINTS+16*NREC+8*NPOINTS
         IF (IMOPT.EQ.1) THEN
            IBYTES=IBYTES+16*NREC+8*NPOINTS
         END IF
      END IF

      IF (ISHPTYP.EQ.21) THEN
c        Typ PointM
         IBYTES=20*NREC
         IF (IMOPT.EQ.1) IBYTES=IBYTES+8*NREC
      END IF

      IF (ISHPTYP.EQ.23.OR.ISHPTYP.EQ.25) THEN
c        Typ PolylineM oder PolygonM
         IBYTES=44*NREC+4*NPARTS+16*NPOINTS
         IF (IMOPT.EQ.1) THEN
            IBYTES=IBYTES+16*NREC+8*NPOINTS
         END IF
      END IF

      IF (ISHPTYP.EQ.28) THEN
c        Typ MultipointM
         IBYTES=40*NREC+16*NPOINTS
         IF (IMOPT.EQ.1) THEN
            IBYTES=IBYTES+16*NREC+8*NPOINTS
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CUTLIN (NQNZ,XQ,YQ,SQ,NPNZ,XP,YP,SP,XS,YS,SPS,SQS)
C-----------------------------------------------------------------------
C
C     Suche des Schnittpunktes 2. Polygon mit 1. Polygon
C
      DOUBLE PRECISION XP(*),YP(*),SP(*)
      DOUBLE PRECISION XQ(*),YQ(*),SQ(*)
      DOUBLE PRECISION XS,YS,SPS,SQS
      DOUBLE PRECISION DX,DY,DIS,DIS2
      DOUBLE PRECISION XQ1,YQ1,SQ1,XQ2,YQ2,SQ2,DXQ,DYQ,RMQ,AQ,XP1,YP1
      DOUBLE PRECISION SP1,XP2,YP2,SP2,DXP,DYP,RMP,AP,DRQP,X,Y,XPA,YPA
      DOUBLE PRECISION XPI,YPI,XQA,YQA,XQI,YQI,X1,Y1,X2,Y2
C
      XS=10.0**38
      YS=10.0**38
      SPS=10.0**38
      SQS=10.0**38
C
      NQ=0
      DO NQ=1,NQNZ
        IF (NQ.GT.1) THEN
           XQ2=XQ(NQ)
           YQ2=YQ(NQ)
           SQ2=SQ(NQ)
           XQ1=XQ(NQ-1)
           YQ1=YQ(NQ-1)
           SQ1=SQ(NQ-1)
C          Steigung des GeradenstÅcks auf 2. Polygon (Querprofil)
           DXQ=(XQ2-XQ1)
           DYQ=(YQ2-YQ1)
           IF (DXQ.NE.0.0.AND.DYQ.NE.0.0) THEN
              RMQ=DYQ/DXQ
C             Achsenabschnitt des GeradenstÅcks auf 2. Polygon
              AQ=YQ1-RMQ*XQ1
           ELSE IF (DYQ.EQ.0.0) THEN
C             Steigung gleich null
              RMQ=0.0
C             theoretischer Schnittpunkt nur fÅr Y fixierbar
              AQ=YQ1
           ELSE IF (DXQ.EQ.0.0) THEN
C             Steigung unendlich
              RMQ=1.0*10.**38
              AQ=(-1.0)*10.**38
C             theoretischer Schnittpunkt nur fÅr X fixierbar
              X=XQ1
           ELSE
              WRITE(7,*) 'P2: kein Fall erfÅllt'
           END IF
C
           DO NP=1,NPNZ
             IF (NP.GT.1) THEN
               XP2=XP(NP)
               YP2=YP(NP)
               SP2=SP(NP)
               XP1=XP(NP-1)
               YP1=YP(NP-1)
               SP1=SP(NP-1)
C              Steigung des GeradenstÅcks auf 1. Polygon
               DXP=(XP2-XP1)
               DYP=(YP2-YP1)
               IF (DXP.NE.0.0.AND.DYP.NE.0.0) THEN
                  RMP=DYP/DXP
C                 Achsenabschnitt des GeradenstÅcks auf 1. Polygon
                  AP=YP1-RMP*XP1
C                 theoretischer Schnittpunkt
                  IF (DXQ.NE.0.0) THEN
                     DRQP=RMQ-RMP
                     IF (DRQP.NE.0.0) THEN
                        X=(AP-AQ)/DRQP
                        Y=RMQ*X+AQ
                     ELSE
c                       WRITE(7,*)'DRQP=0,parallele Geraden!'
c                       WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                       WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                       WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                       WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                        GOTO 4
                     END IF
                  ELSE IF (DXQ.EQ.0.0) THEN
                     X=XQ1
                     Y=RMP*X+AP
                  END IF
               ELSE IF (DYP.EQ.0.0) THEN
C                 Steigung gleich null
                  RMP=0.0
                  AP=YP1
C                 theoretischer Schnittpunkt nur fÅr Y fixierbar
                  Y=YP1
                  IF (DXQ.NE.0.0) THEN
                     IF (RMQ.NE.0.0) THEN
                        X=(Y-AQ)/RMQ
                     ELSE
c                       WRITE(7,*)'DYP=0,DXQ=0,parallele Geraden!'
c                       WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                       WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                       WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                       WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                        GOTO 4
                     END IF
                  ELSE IF (DXQ.EQ.0.0) THEN
                     X=XQ1
                     Y=RMP*X+AP
                  END IF
               ELSE IF (DXP.EQ.0.0) THEN
C                 Steigung unendlich
                  RMP=1.0*10.**38
                  AP=(-1.0)*10.**38
C                 theoretischer Schnittpunkt nur fÅr X fixierbar
                  X=XP1
                  IF (DXQ.NE.0.0) THEN
                     Y=RMQ*X+AQ
                  ELSE IF (DXQ.EQ.0.0) THEN
c                    WRITE(7,*)'DXP=0,DXQ=0,parallele Geraden!'
c                    WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                    WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                    WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                    WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                     GOTO 4
                  END IF
               ELSE
                  WRITE(7,*) 'P1: kein Fall erfÅllt!'
               END IF
C
C              Suche, ob Schnittpunkt auf GeradenstÅcken liegt
C
               IFLAG1=0
               IFLAG2=0
               X1=0.0
               Y1=0.0
               X2=0.0
               Y2=0.0
C
               XPI=DMIN1(XP1,XP2)-0.005
               XPA=DMAX1(XP1,XP2)+0.005
               YPI=DMIN1(YP1,YP2)-0.005
               YPA=DMAX1(YP1,YP2)+0.005
C
C
               IF (X.GE.XPI.AND.X.LE.XPA.AND.Y.GE.YPI.AND.Y.LE.YPA) THEN
c                 WRITE(7,'(A)') 'Schnittpunkt auf 1. GeradenstÅck'
c                 WRITE(7,'(2(A,F15.4))') '   X = ',X,'    Y = ',Y
c                 WRITE(7,'(2(A,F15.4))') 'XP1 = ',XP1,'   YP1 = ',YP1
c                 WRITE(7,'(2(A,F15.4))') 'XP2 = ',XP2,'   YP2 = ',YP2
                  IFLAG1=1
                  X1=X
                  Y1=Y
               END IF
C
               XQI=DMIN1(XQ1,XQ2)-0.005
               XQA=DMAX1(XQ1,XQ2)+0.005
               YQI=DMIN1(YQ1,YQ2)-0.005
               YQA=DMAX1(YQ1,YQ2)+0.005
C
               IF (X.GE.XQI.AND.X.LE.XQA.AND.Y.GE.YQI.AND.Y.LE.YQA) THEN
c                 WRITE(7,'(A)') 'Schnittpunkt auf 2. GeradenstÅck'
c                 WRITE(7,'(2(A,F15.4))') 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                 WRITE(7,'(2(A,F15.4))') 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                  IFLAG2=1
                  X2=X
                  Y2=Y
               END IF
C
               IF (IFLAG1.EQ.1.AND.IFLAG2.EQ.1) THEN
C                 Berechnung der Stationen
                  XS=(X1+X2)/2.
                  YS=(Y1+Y2)/2.
C                 Station auf 1. Polygon SPS
                  DX=XS-XP1
                  DY=YS-YP1
                  DIS2=DX*DX+DY*DY
                  DIS=DSQRT(DIS2)
                  SPS=SP1+DIS
C                 Station auf 2. Polygon SQS
                  DX=XS-XQ1
                  DY=YS-YQ1
                  DIS2=DX*DX+DY*DY
                  DIS=DSQRT(DIS2)
                  SQS=SQ1+DIS
C                 Ausgabe
               END IF

             END IF

    4        CONTINUE

           END DO
         END IF
      END DO
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CUTLINZ (NQ,XQ,YQ,ZQ,SQ,NP,XP,YP,SP,XS,YS,SPS,SQS,ZS)
C-----------------------------------------------------------------------
C
C     Suche des Schnittpunktes 2. Polygon mit 1. Polygon
C
      DOUBLE PRECISION XP(*),YP(*),SP(*)
      DOUBLE PRECISION XQ(*),YQ(*),ZQ(*),SQ(*)
      DOUBLE PRECISION XS,YS,SPS,SQS,ZS
      DOUBLE PRECISION DX,DY,DIS,DIS2
      DOUBLE PRECISION XQ1,YQ1,SQ1,XQ2,YQ2,SQ2,DXQ,DYQ,RMQ,AQ,XP1,YP1
      DOUBLE PRECISION SP1,XP2,YP2,SP2,DXP,DYP,RMP,AP,DRQP,X,Y,XPA,YPA
      DOUBLE PRECISION XPI,YPI,XQA,YQA,XQI,YQI,X1,Y1,X2,Y2
      DOUBLE PRECISION ZQ1,ZQ2
C
      XS=10.0**38
      YS=10.0**38
      ZS=10.0**38
      SPS=10.0**38
      SQS=10.0**38
C
      NQNZ=NQ
      NPNZ=NP
C
      NQ=0
      DO NQ=1,NQNZ
        IF (NQ.GT.1) THEN
           XQ2=XQ(NQ)
           YQ2=YQ(NQ)
           SQ2=SQ(NQ)
           ZQ2=ZQ(NQ)
           XQ1=XQ(NQ-1)
           YQ1=YQ(NQ-1)
           SQ1=SQ(NQ-1)
           ZQ1=ZQ(NQ-1)
C          Steigung des GeradenstÅcks auf 2. Polygon (Querprofil)
           DXQ=(XQ2-XQ1)
           DYQ=(YQ2-YQ1)
           IF (DXQ.NE.0.0.AND.DYQ.NE.0.0) THEN
              RMQ=DYQ/DXQ
C             Achsenabschnitt des GeradenstÅcks auf 2. Polygon
              AQ=YQ1-RMQ*XQ1
           ELSE IF (DYQ.EQ.0.0) THEN
C             Steigung gleich null
              RMQ=0.0
C             theoretischer Schnittpunkt nur fÅr Y fixierbar
              AQ=YQ1
           ELSE IF (DXQ.EQ.0.0) THEN
C             Steigung unendlich
              RMQ=1.0*10.**38
              AQ=(-1.0)*10.**38
C             theoretischer Schnittpunkt nur fÅr X fixierbar
              X=XQ1
           ELSE
              WRITE(7,*) 'P2: kein Fall erfÅllt'
           END IF
C
           DO NP=1,NPNZ
             IF (NP.GT.1) THEN
               XP2=XP(NP)
               YP2=YP(NP)
               SP2=SP(NP)
               XP1=XP(NP-1)
               YP1=YP(NP-1)
               SP1=SP(NP-1)
C              Steigung des GeradenstÅcks auf 1. Polygon
               DXP=(XP2-XP1)
               DYP=(YP2-YP1)
               IF (DXP.NE.0.0.AND.DYP.NE.0.0) THEN
                  RMP=DYP/DXP
C                 Achsenabschnitt des GeradenstÅcks auf 1. Polygon
                  AP=YP1-RMP*XP1
C                 theoretischer Schnittpunkt
                  IF (DXQ.NE.0.0) THEN
                     DRQP=RMQ-RMP
                     IF (DRQP.NE.0.0) THEN
                        X=(AP-AQ)/DRQP
                        Y=RMQ*X+AQ
                     ELSE
c                       WRITE(7,*)'DRQP=0,parallele Geraden!'
c                       WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                       WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                       WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                       WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                        GOTO 4
                     END IF
                  ELSE IF (DXQ.EQ.0.0) THEN
                     X=XQ1
                     Y=RMP*X+AP
                  END IF
               ELSE IF (DYP.EQ.0.0) THEN
C                 Steigung gleich null
                  RMP=0.0
                  AP=YP1
C                 theoretischer Schnittpunkt nur fÅr Y fixierbar
                  Y=YP1
                  IF (DXQ.NE.0.0) THEN
                     IF (RMQ.NE.0.0) THEN
                        X=(Y-AQ)/RMQ
                     ELSE
c                       WRITE(7,*)'DYP=0,DXQ=0,parallele Geraden!'
c                       WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                       WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                       WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                       WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                        GOTO 4
                     END IF
                  ELSE IF (DXQ.EQ.0.0) THEN
                     X=XQ1
                     Y=RMP*X+AP
                  END IF
               ELSE IF (DXP.EQ.0.0) THEN
C                 Steigung unendlich
                  RMP=1.0*10.**38
                  AP=(-1.0)*10.**38
C                 theoretischer Schnittpunkt nur fÅr X fixierbar
                  X=XP1
                  IF (DXQ.NE.0.0) THEN
                     Y=RMQ*X+AQ
                  ELSE IF (DXQ.EQ.0.0) THEN
c                    WRITE(7,*)'DXP=0,DXQ=0,parallele Geraden!'
c                    WRITE(7,*) 'XP1 = ',XP1,'   YP1 = ',YP1
c                    WRITE(7,*) 'XP2 = ',XP2,'   YP2 = ',YP2
c                    WRITE(7,*) 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                    WRITE(7,*) 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                     GOTO 4
                  END IF
               ELSE
                  WRITE(7,*) 'P1: kein Fall erfÅllt!'
               END IF
C
C              Suche, ob Schnittpunkt auf GeradenstÅcken liegt
C
               IFLAG1=0
               IFLAG2=0
               X1=0.0
               Y1=0.0
               X2=0.0
               Y2=0.0
C
               XPI=DMIN1(XP1,XP2)-0.005
               XPA=DMAX1(XP1,XP2)+0.005
               YPI=DMIN1(YP1,YP2)-0.005
               YPA=DMAX1(YP1,YP2)+0.005
C
C
               IF (X.GE.XPI.AND.X.LE.XPA.AND.Y.GE.YPI.AND.Y.LE.YPA) THEN
c                 WRITE(7,'(A)') 'Schnittpunkt auf 1. GeradenstÅck'
c                 WRITE(7,'(2(A,F15.4))') '   X = ',X,'    Y = ',Y
c                 WRITE(7,'(2(A,F15.4))') 'XP1 = ',XP1,'   YP1 = ',YP1
c                 WRITE(7,'(2(A,F15.4))') 'XP2 = ',XP2,'   YP2 = ',YP2
                  IFLAG1=1
                  X1=X
                  Y1=Y
               END IF
C
               XQI=DMIN1(XQ1,XQ2)-0.005
               XQA=DMAX1(XQ1,XQ2)+0.005
               YQI=DMIN1(YQ1,YQ2)-0.005
               YQA=DMAX1(YQ1,YQ2)+0.005
C
               IF (X.GE.XQI.AND.X.LE.XQA.AND.Y.GE.YQI.AND.Y.LE.YQA) THEN
c                 WRITE(7,'(A)') 'Schnittpunkt auf 2. GeradenstÅck'
c                 WRITE(7,'(2(A,F15.4))') 'XQ1 = ',XQ1,'   YQ1 = ',YQ1
c                 WRITE(7,'(2(A,F15.4))') 'XQ2 = ',XQ2,'   YQ2 = ',YQ2
                  IFLAG2=1
                  X2=X
                  Y2=Y
               END IF
C
               IF (IFLAG1.EQ.1.AND.IFLAG2.EQ.1) THEN
C                 Berechnung der Stationen
                  XS=(X1+X2)/2.
                  YS=(Y1+Y2)/2.
C                 Station auf 1. Polygon SPS
                  DX=XS-XP1
                  DY=YS-YP1
                  DIS2=DX*DX+DY*DY
                  DIS=DSQRT(DIS2)
                  SPS=SP1+DIS
C                 Station auf 2. Polygon SQS
                  DX=XS-XQ1
                  DY=YS-YQ1
                  DIS2=DX*DX+DY*DY
                  DIS=DSQRT(DIS2)
                  SQS=SQ1+DIS
C                 Z-Wert
                  ZS=ZQ1+((ZQ2-ZQ1)/(SQ2-SQ1))*(SQS-SQ1)
c                 WRITE(7,'(A)') 'Interpolation Z-Wert'
c                 WRITE(7,'(A,F15.4)') 'SQS = ',SQS
c                 WRITE(7,'(2(A,F15.4))') 'SQ1 = ',SQ1,'   SQ2 = ',SQ2
c                 WRITE(7,'(2(A,F15.4))') 'ZQ1 = ',ZQ1,'   ZQ2 = ',ZQ2
c                 WRITE(7,'(A,F15.4)') ' ZS = ',ZS
C                 Ausgabe
               END IF

             END IF

    4        CONTINUE

           END DO
         END IF
      END DO

      NQ=NQNZ
      NP=NPNZ

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE KORDTD (N,XA,YA,XA1,YA1,XA2,YA2,XN1,YN1,XN2,YN2,XN,YN)
C-----------------------------------------------------------------------
C
C     Unterprogramm zur linearen Koordinatentransformation
C     DOUBLE-PRECISION-VERSION VON 'KORDTR'
C
C
      DOUBLE PRECISION XA,YA,XA1,YA1,XA2,YA2,XN1,YN1,XN2,YN2,XN,YN
      DIMENSION XA(*),YA(*),XN(*),YN(*)
      DOUBLE PRECISION DXA,DYA,DXN,DYN,GAM,A,B,DX,DY
C
C
C     N = Anzahl der Werte
C     XA = 1D-Feld mit alten X-Werten
C     YA = 1D-Feld mit alten Y-Werten
C     XA1 = alter X-Wert fÅr Bezugspunkt 1
C     YA1 = alter Y-Wert fÅr Bezugspunkt 1
C     XA2 = alter X-Wert fÅr Bezugspunkt 2
C     YA2 = alter Y-Wert fÅr Bezugspunkt 2
C     XN1 = neuer X-Wert fÅr Bezugspunkt 1
C     YN1 = neuer Y-Wert fÅr Bezugspunkt 1
C     XN2 = neuer X-Wert fÅr Bezugspunkt 2
C     YN2 = neuer Y-Wert fÅr Bezugspunkt 2
C     XN = 1D-Feld mit neuen X-Werten
C     YN = 1D-Feld mit neuen Y-Werten
C
C
      DXA=XA2-XA1
      DYA=YA2-YA1
      DXN=XN2-XN1
      DYN=YN2-YN1
      GAM=DXA*DXA+DYA*DYA
      A=(DXA*DYN-DYA*DXN)/GAM
      B=(DYA*DYN+DXA*DXN)/GAM
C
      DO 1 I=1,N
C
         DX=B*(XA(I)-XA1)-A*(YA(I)-YA1)
         DY=A*(XA(I)-XA1)+B*(YA(I)-YA1)
         XN(I)=XN1+DX
         YN(I)=YN1+DY
C
    1 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE ADDXYZMSHP (IREC,NNEU,XNEU,YNEU,ZNEU,MNEU,ISUMPOINTS,
     &           ISUMPARTS,IPARTS,XPOINT,YPOINT,ZPOINT,MPOINT,IMOPT)
C-----------------------------------------------------------------------

      INTEGER*4 ISUMPARTS(*),ISUMPOINTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
      DOUBLE PRECISION ZPOINT(*),MPOINT(*)
      DOUBLE PRECISION XNEU(*),YNEU(*),ZNEU(*),MNEU(*)
 
      NUMPOINTS=NNEU
      IF (IREC.GT.1) THEN
         ISUMPOINTS(IREC)=ISUMPOINTS(IREC-1)+NUMPOINTS
      ELSE
         ISUMPOINTS(IREC)=NUMPOINTS
      END IF
C
      NS=0
      DO NN=1,NNEU
       IF (XNEU(NN).GT.0.0.AND.YNEU(NN).GT.0.0.AND.ZNEU(NN).GE.0.0) THEN
        NS=NS+1
        ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NS)
        XPOINT(ISPO)=XNEU(NN)
        YPOINT(ISPO)=YNEU(NN)
        ZPOINT(ISPO)=ZNEU(NN)
        IF (IMOPT.EQ.1) THEN
         MPOINT(ISPO)=MNEU(NN)
        ELSE
         MPOINT(ISPO)=0.0
        END IF
       ELSE
        ISUMPOINTS(IREC)=ISUMPOINTS(IREC)-1
        WRITE(7,*) 'XNEU,YNEU, ZNEU < 0 !! Punkt nicht gespeichert !!'
       END IF
      END DO
C
      NUMPARTS=1
      IF (IREC.GT.1) THEN
         ISUMPARTS(IREC)=ISUMPARTS(IREC-1)+NUMPARTS
      ELSE
         ISUMPARTS(IREC)=NUMPARTS
      END IF
      ISPA=IZEIGER_VERTEX(ISUMPARTS,IREC,1)
      IPARTS(ISPA)=1

      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE GETXYSHP (IREC,NTMP,XTMP,YTMP,ISUMPOINTS,ISUMPARTS,
     &           IPARTS,XPOINT,YPOINT)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XTMP(*),YTMP(*)
      INTEGER*4 ISUMPOINTS(*),ISUMPARTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*)
C
C     Anzahl der Punkte im Objekt
      NTMP=IANZAHL_VERTEXE(ISUMPOINTS,IREC)
C     Schleife ¸ber Punkte im Objekt
      DO NPO=1,NTMP
C        Zeiger auf Vertex
         ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
C        Werte speichern in tempor‰ren Vektoren
         XTMP(NPO)=XPOINT(ISPO)
         YTMP(NPO)=YPOINT(ISPO)
      END DO

      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE GETXYZSHP (IREC,NTMP,XTMP,YTMP,ZTMP,MTMP,ISUMPOINTS,
     &           ISUMPARTS,IPARTS,XPOINT,YPOINT,ZPOINT)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XTMP(*),YTMP(*),ZTMP(*),MTMP(*)
      INTEGER*4 ISUMPOINTS(*),ISUMPARTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*),ZPOINT(*)
C
C     Anzahl der Punkte im Objekt
      NTMP=IANZAHL_VERTEXE(ISUMPOINTS,IREC)
C     Schleife ¸ber Punkte im Objekt
      DO NPO=1,NTMP
C        Zeiger auf Vertex
         ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
C        Werte speichern in tempor‰ren Vektoren
         XTMP(NPO)=XPOINT(ISPO)
         YTMP(NPO)=YPOINT(ISPO)
         ZTMP(NPO)=ZPOINT(ISPO)
         MTMP(NPO)=0.0
      END DO

      RETURN
      END
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      SUBROUTINE GETXYZMSHP (IREC,NTMP,XTMP,YTMP,ZTMP,MTMP,ISUMPOINTS,
     &           ISUMPARTS,IPARTS,XPOINT,YPOINT,ZPOINT,MPOINT)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XTMP(*),YTMP(*),ZTMP(*),MTMP(*)
      INTEGER*4 ISUMPOINTS(*),ISUMPARTS(*),IPARTS(*)
      DOUBLE PRECISION XPOINT(*),YPOINT(*),ZPOINT(*),MPOINT(*)
C
C     Anzahl der Punkte im Objekt
      NTMP=IANZAHL_VERTEXE(ISUMPOINTS,IREC)
C     Schleife ¸ber Punkte im Objekt
      DO NPO=1,NTMP
C        Zeiger auf Vertex
         ISPO=IZEIGER_VERTEX(ISUMPOINTS,IREC,NPO)
C        Werte speichern in tempor‰ren Vektoren
         XTMP(NPO)=XPOINT(ISPO)
         YTMP(NPO)=YPOINT(ISPO)
         ZTMP(NPO)=ZPOINT(ISPO)
         MTMP(NPO)=MPOINT(ISPO)
      END DO

      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE CBIG_TO_ILIT (C4BIG,I4LIT)
C-----------------------------------------------------------------------
C     WANDELN VON 4 BYTE IN "BIG ENDIAN BYTE ORDER" (SUN, MOTOROLA)
C     NACH 4-BYTE INTEGER IN "LITTLE ENDIAN BYTE ORDER" (PC, INTEL)
C-----------------------------------------------------------------------
      CHARACTER (LEN=4) C4BIG,C4LIT
      INTEGER*4 I4LIT
C
      DO I=1,4
         J=5-I
         C4LIT(I:I)=C4BIG(J:J)
      END DO
      READ (C4LIT,'(A4)') I4LIT
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE ILIT_TO_CBIG (I4LIT,C4BIG)
C-----------------------------------------------------------------------
C     WANDELN VON 4-BYTE INTEGER IN "LITTLE ENDIAN BYTE ORDER" (PC)
C     NACH 4 BYTE IN "BIG ENDIAN BYTE ORDER" (SUN, MOTOROLA)
C-----------------------------------------------------------------------
      CHARACTER (LEN=4) C4BIG,C4LIT
      INTEGER*4 I4LIT
C
      WRITE (C4LIT,'(A4)') I4LIT
      DO I=1,4
         J=5-I
         C4BIG(I:I)=C4LIT(J:J)
      END DO
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
