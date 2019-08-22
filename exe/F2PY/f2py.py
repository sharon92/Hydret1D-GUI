import numpy.f2py as FOR

f = FOR.run_main(['-m','scalar','NInput.for'])

with open('NInput.for','r') as t:
    fsource = t.readlines()
#fsource='''
#      SUBROUTINE BACJUN ()
#C
#C      BACKWARD SWEEP THROUGH JUNCTIONS
#
#      INCLUDE 'DIMENS.INC'
#      COMMON /COEXV/        CO(IPSED,4) , E(IPSED)    , XV(IPSED)   ,
#     1        COM(IPSED,4) , COZ(IPSED) , CON(3)
#      COMMON /CMS/          CM(IPJUN,9)
#      COMMON /AB/           IAB(IPSEC)
#      NJ1 =1
#      NJ2 =1 
#      NJ3 =1 
#      NJ4 = 1
#      I = 1
#      KLD1 = 2 * NJ1
#      KLD2 = 2 * NJ2
#      KLQ1 = KLD1 - 1
#      KLQ2 = KLD2 - 1
#      KLD3 = 2 * NJ3
#      KLQ3 = KLD3 - 1
#      XV(KLD1) = CM(I,1) + (CM(I,2) * XV(KLQ3)) - (CM(I,3) * XV(KLD3))
#      XV(KLD2) = CM(I,4) + (CM(I,5) * XV(KLQ3)) - (CM(I,6) * XV(KLD3))
#      XV(KLQ1) = (COZ(KLQ1) - COM(KLQ1,4) * XV(KLD1))/COM(KLQ1,3)
#      XV(KLQ2) = (COZ(KLQ2) - COM(KLQ2,4) * XV(KLD2))/COM(KLQ2,3)
#      IF (NJ4.EQ.0) GO TO 100
#C
#C      THIRD INFLOWING BRANCH
#C
#      KLD4 = 2 * NJ4
#      KLQ4 = KLD4 - 1
#      XV(KLD4) = CM(I,7) + (CM(I,8) * XV(KLQ3)) - (CM(I,9) * XV(KLD3))
#      XV(KLQ4) = (COZ(KLQ4) - COM(KLQ4,4) * XV(KLD4))/COM(KLQ4,3)
#  100 CONTINUE
#      RETURN
#      END
#'''
FOR.compile(''.join(fsource),modulename='Ninput',verbose=True)

