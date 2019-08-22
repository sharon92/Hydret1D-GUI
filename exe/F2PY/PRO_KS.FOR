      PROGRAM PRO_KS
      CHARACTER*30 PROEIN,PROAUS,TEXT
      CHARACTER*1 CTAB
      CHARACTER*2 MOD,KSMOD
      REAL XP(151),YP(151),XKS(300),YKS(300)

      KSMOD='H2'

      OPEN (10,'PRO_KS.RUN',STATUS='OLD')
      READ (10,'(A)') PROEIN
      READ (10,'(A)') PROAUS
      READ (10,*) DIFFMIN
      READ (10,*) DLMIN
      CLOSE (10,STATUS='KEEP')

      OPEN (11,PROEIN,STATUS='OLD')
      OPEN (22,PROAUS,STATUS='UNKNOWN')

    1 READ (11,111,END=1111)  NSEC,NPTS,CTAB,MOD,YMAX,STAT,TEXT
  111 FORMAT (2I5,1X,A1,1X,A2,F5.0,9X,F10.1,2X,A30)
      READ (11,*) (XP(NP),YP(NP),NP=1,NPTS)

      NK=1
      XKS(NK)=XP(1)
      YKS(NK)=0.0
      NK=NK+1
      XKS(NK)=XP(1)
      YKS(NK)=30.0
      RKS=30.0
      YKS(NK)=RKS

      DO NP=2,(NPTS-1)
         IF (NP.GT.2.AND.NP.LT.NPTS) THEN
            DXL1=XP(NP)-XP(NP-1)
            DYL1=YP(NP)-YP(NP-1)
            IF (DXL1.GT.-0.001.AND.DXL1.LT.0.0) DXL1=-0.001
            IF (DXL1.LT.0.001.AND.DXL1.GE.0.0) DXL1=0.001
            DXL2=XP(NP)-XP(NP-2)
            DYL2=YP(NP)-YP(NP-2)
            IF (DXL2.GT.-0.001.AND.DXL2.LT.0.0) DXL2=-0.001
            IF (DXL2.LT.0.001.AND.DXL2.GE.0.0) DXL2=0.001
            GRADL=0.5*(DYL1/DXL1+DYL2/DXL2)
            WINKL=ATAN(GRADL)
            DXR1=XP(NP+1)-XP(NP)
            DYR1=YP(NP+1)-YP(NP)
            IF (DXR1.GT.-0.001.AND.DXR1.LT.0.0) DXR1=-0.001
            IF (DXR1.LT.0.001.AND.DXR1.GE.0.0) DXR1=0.001
            DXR2=XP(NP+2)-XP(NP)
            DYR2=YP(NP+2)-YP(NP)
            IF (DXR2.GT.-0.001.AND.DXR2.LT.0.0) DXR2=-0.001
            IF (DXR2.LT.0.001.AND.DXR2.GE.0.0) DXR2=0.001
            GRADR=0.5*(DYR1/DXR1+DYR2/DXR2)
            WINKR=ATAN(GRADR)
            ABSDIF=ABS(WINKR-WINKL)
            IF (ABS(DXL1).GT.DLMIN.AND.ABS(DXR1).GT.DLMIN) THEN
               IF (ABSDIF.GT.DIFFMIN) THEN
                  NK=NK+1
                  XKS(NK)=XP(NP)
                  YKS(NK)=RKS
                  NK=NK+1
                  XKS(NK)=XP(NP)
                  YKS(NK)=RKS
               END IF
            END IF
         ELSE
            DXL=XP(NP)-XP(NP-1)
            DYL=YP(NP)-YP(NP-1)
            IF (DXL.GT.-0.001.AND.DXL.LT.0.0) DXL=-0.001
            IF (DXL.LT.0.001.AND.DXL.GE.0.0) DXL=0.001
            GRADL=DYL/DXL
            WINKL=ATAN(GRADL)
            DXR=XP(NP+1)-XP(NP)
            DYR=YP(NP+1)-YP(NP)
            IF (DXR.GT.-0.001.AND.DXR.LT.0.0) DXR=-0.001
            IF (DXR.LT.0.001.AND.DXR.GE.0.0) DXR=0.001
            GRADR=DYR/DXR
            WINKR=ATAN(GRADR)
            ABSDIF=ABS(WINKR-WINKL)
            IF (ABS(DXL).GT.DLMIN.AND.ABS(DXR).GT.DLMIN) THEN
               IF (ABSDIF.GT.DIFFMIN) THEN
                  NK=NK+1
                  XKS(NK)=XP(NP)
                  YKS(NK)=RKS
                  NK=NK+1
                  XKS(NK)=XP(NP)
                  YKS(NK)=RKS
               END IF
            END IF
         END IF
      END DO

      NK=NK+1
      XKS(NK)=XP(NPTS)
      YKS(NK)=RKS
      NPKS=NK+1
      XKS(NPKS)=XP(NPTS)
      YKS(NPKS)=0.0

      PRINT '(T2,2(A,I5))','NSEC = ',NSEC,'  NPKS = ',NPKS

      WRITE (22,221) -NSEC,NPKS,KSMOD,'STATION :',STAT,' M',TEXT
  221 FORMAT (2I5,3X,A2,5X,A9,F10.1,A2,A30)
      WRITE (22,'(4(2F8.2,2X))') (XKS(NP),YKS(NP),NP=1,NPKS)

      WRITE (22,222) NSEC,NPTS,CTAB,MOD,YMAX,'STATION :',STAT,' M',TEXT
  222 FORMAT (2I5,1X,A1,1X,A2,F5.0,A9,F10.1,A2,A30)
      WRITE (22,'(4(2F8.2,2X))') (XP(NP),YP(NP),NP=1,NPTS)

      GOTO 1
 1111 CONTINUE

      CLOSE (11,STATUS='KEEP')
      CLOSE (22,STATUS='KEEP')

      END
