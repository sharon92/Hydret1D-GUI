         IF (I.GT.1) THEN
            IABOW = IAB(I-1)
            JQX = (I-1) * 2
            QOW = XV(JQX - 1)
            QMIT=QMIT+QOW
            NMIT=NMIT+1
            QOW = ENGSI(QOW,3)
            DOW = XV(JQX)
            DOW = ENGSI(DOW,1)
         ELSE
            KNOW = 0
            QOW = 0.0
            DOW = 0.0
         END IF
         IF (I.LT.NX) THEN
            IABUW = IAB(I+1)
            JQX = (I+1) * 2
            QUW = XV(JQX - 1)
            QMIT=QMIT+QUW
            NMIT=NMIT+1
            QUW = ENGSI(QUW,3)
            DUW = XV(JQX)
            DUW = ENGSI(DUW,1)
         ELSE
            KNUW = 0
            QUW = 0.0
            DUW = 0.0
         END IF
         DMIT=0.5*(DOW+DUW)
         DMIT=SIENG(DMIT,1)
         QMIT=ABS(QMIT/REAL(NMIT))
C
C     CHECK FOR DEPTH LESS THAN 0.1 FT
C      IF DEPTH OUT OF RANGE, REDUCE TIME STEP
C
         IF (D2(I).GT.0.1) GOTO 300
         WRITE (N6,470)
         D2$ = ENGSI(D2(I),1)
         Q2$ = ENGSI(Q2(I),3)
c        WRITE (N6,480) IABOW,DOW,QOW,DT
         WRITE (N6,480) IAB(I),D2$,Q2$,DT
c        WRITE (N6,480) IABUW,DUW,QUW,DT

         IF (ITYPE(I).GE.4) THEN
            CALL YGRENZ4(I,QMIT,YGRZ,HMIN)
            IF (YGRZ.GT.DMIT) DMIT=YGRZ
            D2(I) = DMIT
            XV(JQX) = D2(I)
            XV(JQX-1) = QMIT
         END IF

         FLAG120=.TRUE.
c        GO TO 120

  300    CONTINUE
         IF (D2(I).GT.0.01) GO TO 310
         WRITE (N6,*) 'ITIME = ',ITIME
         WRITE (N6,*) '  DOW = ',DOW
         WRITE (N6,*) ' YGRZ = ',ENGSI(YGRZ,1)
         WRITE (N6,*) '  DUW = ',DUW
         WRITE (N6,*) ' QMIT = ',ENGSI(QMIT,3)
         WRITE (N6,301) IAB(I),D2(I)
  301    FORMAT (////1X,'WASSERTIEFE WIRD KLEINER ALS 0.01 FUSS BEI ABSC
     1HNITT',I5,'     TIEFE IST',F8.4,' FUSS')
         STOP 301
  310    CONTINUE

         H = D2(I) + ZO(I)

C        CALL SHAPE (I,H,A,XWP,TOP)

         CALL SHAPE2 (I,H,A,XWP,TOP,CH2,CHR,QGRENZ)
