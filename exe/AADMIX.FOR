      PROGRAM AADMIX
      CHARACTER*12 MIXAUS,AADDAT(7),MIXDAT
      DIMENSION IAB(7),ITYPE(7),WIDTH(7),HEIT(7),ZTR(7)
      DIMENSION ZTL(7),XL(7),ZO(7),RNI(7),DZERO(7),QZERO(7)
      DIMENSION ZS(7),CKM(7),HHR0(7),HHR(7,7),RRN(7,7)
      DIMENSION HHRS(49),RRNS(49)
C
      OPEN(11,FILE='AADMIX.RUN',STATUS='OLD')
      READ(11,'(A12)') MIXAUS
      READ(11,*) NAAD
      DO 1 NA=1,NAAD
         READ(11,'(A12)') AADDAT(NA)
    1 CONTINUE
      READ(11,*) DHEPS, RKSMAX
      READ(11,'(A12)',END=1111) MIXDAT
 1111 CLOSE (11,STATUS='KEEP')
C
      RNMIN=1./RKSMAX
C
      OPEN(10,FILE=MIXAUS,STATUS='UNKNOWN')
      OPEN(30,FILE=MIXDAT,STATUS='UNKNOWN')
      DO 2 NA=1,NAAD
         LU=10+NA
         OPEN(LU,FILE=AADDAT(NA),STATUS='OLD')
    2 CONTINUE
C
    3 CONTINUE
C
      NN=0
C
      DO 4 NA=1,NAAD
         LU=10+NA
         READ (LU,685,END=999) IAB(NA),ITYPE(NA),WIDTH(NA),HEIT(NA),
     &        ZTR(NA),ZTL(NA),XL(NA),ZO(NA),RNI(NA),DZERO(NA),QZERO(NA),
     &        ZS(NA),CKM(NA),HHR0(NA),(HHR(NA,N),RRN(NA,N),N=1,7)
C        WASSERSPIEGEL
         NN=NN+1
         HHRS(NN)=HHR0(NA)
         RRNS(NN)=RNI(NA)
C        ERSTE BEIDEN WENDEPUNKTE
         DO 5 N=1,2
            NN=NN+1
            HHRS(NN)=HHR(NA,N)
            RRNS(NN)=RRN(NA,N)
    5       CONTINUE
    4 CONTINUE
C
      NNMX=NN
C
C     SORTIEREN
C
      DO 6 NN=1,NNMX
         IF (HHRS(NN).EQ.0.0) HHRS(NN)=9999.9
    6 CONTINUE
C
      DO 7 N1=1,NNMX-1
         DO 8 N2=(N1+1),NNMX
            IF (HHRS(N2).LT.HHRS(N1)) THEN
               HHRBAK=HHRS(N1)
               RRNBAK=RRNS(N1)
               HHRS(N1)=HHRS(N2)
               RRNS(N1)=RRNS(N2)
               HHRS(N2)=HHRBAK
               RRNS(N2)=RRNBAK
            END IF
    8    CONTINUE
    7 CONTINUE
C
      DO 9 NN=1,NNMX
         IF (HHRS(NN).EQ.9999.9) HHRS(NN)=0.0
    9 CONTINUE
C
      DO 10 NN=3,NNMX
         DH=HHRS(NN)-HHRS(NN-1)
         IF (DH.LE.DHEPS) THEN
            IF (RRNS(NN).LT.RRNS(NN-1)) THEN
               DO 11 NN1=NN,NNMX-1
                  HHRS(NN1)=HHRS(NN1+1)
                  RRNS(NN1)=RRNS(NN1+1)
   11          CONTINUE
            ELSE IF (RRNS(NN).GE.RRNS(NN-1)) THEN
               DO 12 NN1=(NN-1),NNMX-1
                  HHRS(NN1)=HHRS(NN1+1)
                  RRNS(NN1)=RRNS(NN1+1)
   12          CONTINUE
            END IF
            NNMX=NNMX-1
         END IF
   10 CONTINUE
C
C     LIMIT FUER RRNS SETZEN
C
      DO NN=1,NNMX
         IF (RRNS(NN).LT.RNMIN) RRNS(NN)=RNMIN
      END DO
C
C     AUSGABE
C
        WRITE (10,686) IAB(1),ITYPE(1),WIDTH(1),HEIT(1),ZTR(1),
     &        ZTL(1),XL(1),ZO(1),RRNS(1),DZERO(1),QZERO(1),
     &        ZS(1),CKM(1),HHRS(1),(HHRS(NN),RRNS(NN),NN=2,NNMX)
C
        WRITE (30,687) IAB(1),XL(1)/1000.,RRNS(1),
     &                 HHRS(1),(HHRS(NN),RRNS(NN),NN=2,NNMX)
C
      GOTO 3
C
  999 CLOSE (10,STATUS='KEEP')
      CLOSE (30,STATUS='KEEP')
C
  685 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.4,F7.3,F7.2,F5.2,F5.3,
     &        F8.3,7(F8.3,F8.4))
  686 FORMAT (I4,I2,4F7.2,F7.0,F7.2,F7.4,F7.3,F7.2,F5.2,F5.3,
     &        F8.3,15(F8.3,F8.4))
  687 FORMAT (I4,F8.3,F8.4,F8.3,15(F8.3,F8.4))
C
      END
