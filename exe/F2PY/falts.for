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