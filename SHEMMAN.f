C     *****************************************************************
      PROGRAM DATRD
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 INPNAME,OUTNAME

      character * ( 256 ) arg
      integer iargc
      integer ii
      integer numarg

      DIMENSION TITLE(20)
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/NP/NPJ(25)
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/ODXS/NPRI,NBET
      COMMON/FILENAME/OUTNAME

      numarg = iargc ( )
      
      call getarg ( 1, INPNAME )
      call getarg ( 2, OUTNAME )
c     WRITE(*,'(1X,A30/)') 'INPUT FILE NAME ?=>'
c     READ(*,'(A20)') INPNAME
      OPEN(UNIT=20,FILE=INPNAME,STATUS='OLD')
c     WRITE(*,'(1X,A30/)') 'OUTPUT FILE NAME ?=>'
c     READ(*,'(A20)') OUTNAME
      OPEN(UNIT=21,FILE=OUTNAME,STATUS='REPLACE')
      READ (20,19) TITLE
      WRITE(21,19) TITLE
      READ (20,3)NPRI,NUR,MEHAM,MESHA,MESHO,MEHAO
C     READ (20,7)HW,AMB0,AMG0,GAM0,BB42,GAMG,DELG
C     READ (20,7)BET0,BET4,BET3,ETO,AMUO,HWO,BB32
C     READ (20,7)GAMDE,DPAR,GSHAPE
      READ(20,20)HW,AMB0,AMG0,GAM0,BET0,BET4,
     *BB42,GAMG,DELG,
     *BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
   20 FORMAT(6E12.5)
      READ (20,2)(ES(I),JU(I),NPI(I),NTU(I),NNB(I),NNG(I),NNO(I),
     *I=1,NUR)
    2 FORMAT(D10.4,6I2)
      READ (20,3)(NPJ(I),I=1,16)
    3 FORMAT(25I2)
      KEV=0
      KV1=0
      IF(NPJ(1).NE.1) GO TO 4
      KEV=KEV+1
      X(KEV)=HW
    4 IF(NPJ(2).NE.1) GO TO 5
      KEV=KEV+1
      X(KEV)=AMB0
    5 IF(NPJ(3).NE.1) GO TO 6
      KEV=KEV+1
      X(KEV)=AMG0
    6 IF(NPJ(4).NE.1) GO TO 8
      KEV=KEV+1
      KV1=KEV
      X(KEV)=GAM0
    8 IF(NPJ(5).NE.1) GO TO 9
      KEV=KEV+1
      X(KEV)=BB42
    9 IF(NPJ(6).NE.1) GO TO 10
      KEV=KEV+1
      X(KEV)=GAMG
   10 IF(NPJ(7).NE.1) GO TO 11
      KEV=KEV+1
      X(KEV)=DELG
   11 IF(NPJ(8).NE.1) GO TO 12
      KEV=KEV+1
      X(KEV)=BET3
   12 IF(NPJ(9).NE.1) GO TO 13
      KEV=KEV+1
      X(KEV)=ETO
   13 IF(NPJ(10).NE.1) GO TO 14
      KEV=KEV+1
      X(KEV)=AMUO
   14 IF(NPJ(11).NE.1) GO TO 15
      KEV=KEV+1
      X(KEV)=HWO
   15 IF(NPJ(12).NE.1) GO TO 16
      KEV=KEV+1
      X(KEV)=BB32
   16 IF(NPJ(13).NE.1) GO TO 17
      KEV=KEV+1
      X(KEV)=GAMDE
   17 IF(NPJ(14).NE.1) GO TO 18
      KEV=KEV+1
      X(KEV)=DPAR
   18 NV=KEV
      READ (20,7)(EP(I),I=1,NV)
    7 FORMAT(8D10.4)
   19 FORMAT(20A4)
      CALL SEARD
      STOP
      END
C     *****************************************************************
      SUBROUTINE XISRD
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/ODXS/NPRI,NBET
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/NP/NPJ(25)
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SGB/FOLGB(10),FOLB(10),FOLKS(10),FOLGA(10)
      COMMON/AA/ANO(40)
      NPR1=NBET/NPRI*NPRI
      KEV=0
      KV1=0
      IF(NPJ(1).NE.1) GO TO 1
      KEV=KEV+1
      HW=DABS(X(KEV))
    1 IF(NPJ(2).NE.1) GO TO 2
      KEV=KEV+1
      AMB0=DABS(X(KEV))
    2 IF(NPJ(3).NE.1) GO TO 3
      KEV=KEV+1
      AMG0=DABS(X(KEV))
    3 IF(NPJ(4).NE.1)GO TO 8
      KEV=KEV+1
      KV1=KEV
      GAM0=DABS(X(KEV))
    8 IF(NPJ(5).NE.1)GO TO 9
      KEV=KEV+1
      BB42=DABS(X(KEV))
    9 IF(NPJ(6).NE.1)GO TO 10
      KEV=KEV+1
      GAMG=DABS(X(KEV))
   10 IF(NPJ(7).NE.1)GO TO 11
      KEV=KEV+1
      DELG=DABS(X(KEV))
   11 IF(NPJ(8).NE.1)GO TO 12
      KEV=KEV+1
      BET3=DABS(X(KEV))
   12 IF(NPJ(9).NE.1)GO TO 13
      KEV=KEV+1
      ETO=DABS(X(KEV))
   13 IF(NPJ(10).NE.1)GO TO 14
      KEV=KEV+1
      AMUO=DABS(X(KEV))
   14 IF(NPJ(11).NE.1)GO TO 15
      KEV=KEV+1
      HWO=DABS(X(KEV))
   15 IF(NPJ(12).NE.1)GO TO 16
      KEV=KEV+1
      BB32=DABS(X(KEV))
   16 IF(NPJ(13).NE.1)GO TO 17
      KEV=KEV+1
      GAMDE=X(KEV)
   17 IF(NPJ(14).NE.1)GO TO 18
      KEV=KEV+1
      DPAR=X(KEV)
   18 SUM=0.
      NV=KEV
      IF(MEHAO.EQ.2.OR.MEHAO.EQ.3) GO TO 4
      IF(NPJ(11).EQ.0) HWO=HW*(AMB0/AMUO)**2/BB32
      IF(BET3.EQ.0.) HWO=HWO/2.
      IF(BET3.NE.0.) HWO=HWO*BET3**2
    4 CALL SHEM
      IF(NPJ(16).EQ.1) CALL TRANS
      IF(NPJ(16).EQ.2) CALL OVLOUT
      IF(NPJ(16).EQ.3) CALL COVAR
      DO 5 I=1,NUR
      IS=JU(I)
      ITAU=NTU(I)
      NG=NNG(I)
      NB=NNB(I)
      NO=NNO(I)
      ECL=EGB(I)
      ESI=ES(I)
      IF(I.EQ.1) GO TO 5
      DECL=(ESI-ECL)/ESI
      IF(NPR1.NE.NBET) GO TO 6
      NG1=NG+1
      NO1=NO+1
      GAM=GAM0+FOLGA(NG1)
      PRINT 7,ESI,ECL,DECL,IS,ITAU,NB,NG,NO
      WRITE(21,7)ESI,ECL,DECL,IS,ITAU,NB,NG,NO
    7 FORMAT(3D13.5,5I4)
    6 SUM=SUM+DECL*DECL
    5 CONTINUE
      FU=SUM/(NUR-1.)
      RETURN
      END
C     *****************************************************************
      SUBROUTINE SEARD
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ODXS/NPRI,NBET
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1
      COMMON/OPB/C,GRR(25),FM,EPS1,NRL/NP/NPJ(25)
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      GMIN=1.D-3
      GMAX=1.0461975512
      NBET=0
      NRL=1
      NCC=1
      NI=0
      DO 1 I=1,NV
      X1(I)=X(I)
    1 X2(I)=X(I)+EP(I)*2.
      CALL XISRD
      FM=FU
   10 PRINT 11,NI,FM,(X(I),I=1,NV)
      WRITE(21,11)NI,FM,(X(I),I=1,NV)
    6 NXX=0
      N1C=0
      FU=FM
      DO 21 I=1,NV
      X(I)=X1(I)
      IF(DABS(X2(I)-X1(I))-DABS(EP(I)).LE.0.) NXX=NXX+1
   21 CONTINUE
      IF(NXX.EQ.NV)GO TO 9
      CALL DEFRD
      LL=0
      NNK=0
      DO 13 I=1,NV
   13 X2(I)=X1(I)
      NI=NI+1
      C=0.
      DO 7 I=1,NV
      GRR(I)=GR(I)
    7 C=C+GRR(I)**2
      EPS1=FM/C
    8 NX=0
      DO 30 I=1,NV
      EPSGR(I)=EPS1*GRR(I)
      IF(DABS(EPSGR(I)).GT.0.3*DABS(X1(I))) GO TO 15
      IF(DABS(EPSGR(I))-DABS(EP(I)).LE.0) NX=NX+1
   30 CONTINUE
      IF(KV1.EQ.0) GO TO 40
      IF(NPJ(KV1).EQ.0) GO TO 40
      GAM=X1(KV1)-EPSGR(KV1)
      IF(GAM.GT.GMAX.OR.GAM.LT.GMIN)GO TO 15
   40 IF(NNK.EQ.0) NX=0
   17 DO 2 I=1,NV
      HS=EPS1*GRR(I)
      HS1=X1(I)
      X(I)=HS1-HS
    2 CONTINUE
      CALL XISRD
      IF(FU-FM) 3,4,4
    3 DO 5 I=1,NV
    5 X1(I)=X(I)
      LL=LL+1
C     IF(LL.GT.3)EPS1=EPS1*5.
      N1C=1
      NCC=0
      FM=FU
      NBET=NBET+1
      PRINT 11,NI,FM,(X(I),I=1,NV)
      WRITE(21,11)NI,FM,(X(I),I=1,NV)
   11 FORMAT(I5,D30.15/(6D20.7))
      PRINT 16,HW,AMB0,AMG0,GAM0,BET0,BET4,
     *BB42,GAMG,DELG,
     *BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
      WRITE(21,16)HW,AMB0,AMG0,GAM0,BET0,BET4,
     *BB42,GAMG,DELG,
     *BET3,ETO,AMUO,HWO,BB32,GAMDE,DPAR,GSHAPE
   16 FORMAT(6E12.5)
      GO TO 17
    4 IF(NX.EQ.NV) NRL=1
      IF(NX.EQ.NV) GO TO 6
      IF(N1C.EQ.1)NCC=NCC+1
      IF(NCC-2)15,14,14
   14 EPS1=-EPS1
      NCC=0
      GO TO 17
   15 EPS1=EPS1/5.
      NNK=1
      LL=0
      NCC=1
      GO TO 8
    9 CONTINUE
      RETURN
      END
C     *****************************************************************
      SUBROUTINE DEFRD
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1
      COMMON/OPB/C,GRR(25),FM,EPS1,NRL
      F1=FU
      NS=NRL
c      DO 1 I=NS,NV
c      DL=EP(I)
c      X(I)=X(I)+DL
c      CALL XISRD
c      GR(I)=(FU-F1)/DL
c      X(I)=X(I)-DL
c   1 CONTINUE
      DO 1 I=NS,NV
      DL=EP(I)
      X(I)=X(I)-DL
      CALL XISRD
      FMI = FU
      X(I)=X(I)+2.0*DL
      CALL XISRD
      FPL = FU
      GR(I)=(FPL-FMI)/2.0/DL
      X(I)=X(I)-DL
    1 CONTINUE
     
      RETURN
      END
C     *****************************************************************
      SUBROUTINE COVAR
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1
      COMMON/OPB/C,GRR(25),FM,EPS1,NRL
      
      COMMON/SHEMM/ES(40),JU(40),NTU(40),NNB(40),NNG(40),NNO(40),
     *NPI(40)
      COMMON/ENA/EN,EL(40),BET(10),NUR,NMAX,NPD,LAS
      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
     */MENU/MEJOB,MEPOT,MEHAM,MECHA,MEPRI,MESOL,MESHA,MESHO,MEHAO
     *,MEAPP,MEVOL,MEREL,MECUL,MERZZ,MERRR
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/NP/NPJ(25)
     */SHEM2/GIT(40),EP0(40),EP1(40),EP2(40),EP12(40),ANG(40,2),EPB(40),
     *EPG(40),EGB(40),AIT(31,16),PT(40),FOG(2,8,8),XI(40),ANB(40),
     *CD(40,2)
      ! G = dE_i/dP_j
      ! Z_{i,j}^{-1} = \chi^{-2} * \sum_{k=1}^N G_{k,i}*(E_k^{exp})^{-2}*G_{k,j}
      ! covar = Z
      REAL*8 EBASE(40)
      REAL*8 DEDP(40,25),Z(25,25),COV(25,25),COR(25,25)

      CALL SHEM
      EBASE(:)=EGB(:)
      
      DO J=NRL,NV
          DL=EP(J)
          X(J)=X(J)+DL
          CALL COPPAR
          CALL SHEM
          !I=1,NUR
          DO I=1,NUR
              DEDP(I,J)=(EGB(I)-EBASE(I))/DL
          END DO
          X(J)=X(J)-DL
          CALL COPPAR
      END DO
      
      !      PRINT 44,
      !      WRITE(21,44) 

      WRITE(21,"(A)") 'dEi/dPj'
      PRINT "(A)", 'dEi/dPj'
      DO I=1,NUR
          WRITE(21,44) EBASE(I),'|',(DEDP(I,J),J=NRL,NV)
          PRINT 44, EBASE(I),'|',(DEDP(I,J),J=NRL,NV)
      END DO
     
      Z=0.0
      
      DO I=NRL,NV
          DO J=NRL,NV
              DO K=2,NUR
                  Z(I,J)=Z(I,J)+DEDP(K,I)*DEDP(K,J)/ES(K)/ES(K)
              END DO
          END DO
      END DO

      SUM=0.
      DO K=2,NUR
          SUM=SUM+((EGB(K)-ES(K))/ES(K))**2
      END DO
      SUM=SUM/(NUR-1-NV)!(NUR-1)
      
      Z=Z/SUM

      WRITE(21,"(A)") 'Z^-1'
      PRINT "(A)", 'Z^-1'

      DO I=NRL,NV
          WRITE(21,"(25E12.4)") (Z(I,J),J=NRL,NV)
          PRINT "(25E12.4)", (Z(I,J),J=NRL,NV)
      END DO
      
      WRITE(21,"(A)") 'Covariance matrix'
      PRINT "(A)", 'Covariance matrix'

      call inverse(Z(1:NV,1:NV),COV(1:NV,1:NV),NV)
      DO I=NRL,NV
          WRITE(21,"(25E10.2)") (COV(I,J),J=NRL,NV)
          PRINT "(25E10.2)", (COV(I,J),J=NRL,NV)
      END DO 
      
      WRITE(21,"(A10,E12.4)") 'chi^2=',SUM
      PRINT "(A10,E12.4)", 'chi^2=',SUM
      
      WRITE(21,"(A)") 'Correlation matrix'
      PRINT "(A)", 'Correlation matrix'

      DO I=NRL,NV
            DO J=NRL,NV
                !IF (J.NE.I)  
                COR(I,J)=COV(I,J)/DSQRT(COV(J,J)*COV(I,I))
            END DO
      END DO
      
      DO I=NRL,NV
          WRITE(21,"(25F10.2)") (COR(I,J),J=NRL,NV)
          PRINT "(25F10.2)", (COR(I,J),J=NRL,NV)
      END DO 

      WRITE(21,"(A)") 'Summary'
      PRINT "(A)", 'Summary'

      WRITE(21,"(3A15)") 'Par. value', 'Par. error', 'Par. corr.'
      PRINT "(3A15)",  'Par. value', 'Par. error', 'Par. corr.'

      
      DO I=NRL,NV
          WRITE(21,"(E10.3,A5,E10.2,A10,E12.4)") X(I),'+/-',
     *     DSQRT(COV(I,I)), '; Corr=', 1-1/(Z(I,I)*COV(I,I))
          PRINT "(E10.3,A5,E10.2,A10,F10.2)", X(I),'+/-',
     *     DSQRT(COV(I,I)), '; Corr=', 1-1/(Z(I,I)*COV(I,I))
      END DO 

      
   44 FORMAT (E12.4,A1,26E12.4)
      STOP      
      RETURN
      END
C     *****************************************************************
      SUBROUTINE COPPAR
C     *****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/OPT/X(25),GR(25),X1(25),X2(25),EP(25),EPSGR(25),FU,NV,KV1

      COMMON/SHEM1/HW,AMG0,AMB0,GAM0,BET0,BET4,GAMDE,GSHAPE
      COMMON/INRM/AMO,BMO,CMO,BB42,GAMG,DELG
     */SHAMO/BET3,ETO,AMUO,HWO,BB32,DPAR
      COMMON/NP/NPJ(25)
      
      KEV=0
      IF(NPJ(1).NE.1) GO TO 1
      KEV=KEV+1
      HW=DABS(X(KEV))
    1 IF(NPJ(2).NE.1) GO TO 2
      KEV=KEV+1
      AMB0=DABS(X(KEV))
    2 IF(NPJ(3).NE.1) GO TO 3
      KEV=KEV+1
      AMG0=DABS(X(KEV))
    3 IF(NPJ(4).NE.1)GO TO 8
      KEV=KEV+1
      KV1=KEV
      GAM0=DABS(X(KEV))
    8 IF(NPJ(5).NE.1)GO TO 9
      KEV=KEV+1
      BB42=DABS(X(KEV))
    9 IF(NPJ(6).NE.1)GO TO 10
      KEV=KEV+1
      GAMG=DABS(X(KEV))
   10 IF(NPJ(7).NE.1)GO TO 11
      KEV=KEV+1
      DELG=DABS(X(KEV))
   11 IF(NPJ(8).NE.1)GO TO 12
      KEV=KEV+1
      BET3=DABS(X(KEV))
   12 IF(NPJ(9).NE.1)GO TO 13
      KEV=KEV+1
      ETO=DABS(X(KEV))
   13 IF(NPJ(10).NE.1)GO TO 14
      KEV=KEV+1
      AMUO=DABS(X(KEV))
   14 IF(NPJ(11).NE.1)GO TO 15
      KEV=KEV+1
      HWO=DABS(X(KEV))
   15 IF(NPJ(12).NE.1)GO TO 16
      KEV=KEV+1
      BB32=DABS(X(KEV))
   16 IF(NPJ(13).NE.1)GO TO 17
      KEV=KEV+1
      GAMDE=X(KEV)
   17 IF(NPJ(14).NE.1)GO TO 18
      KEV=KEV+1
      DPAR=X(KEV)
   18 SUM=0.
      NV=KEV
      
      RETURN
      END

        subroutine inverse(a,c,n)
        !============================================================
        ! Inverse matrix
        ! Method: Based on Doolittle LU factorization for Ax=b
        ! Alex G. December 2009
        !-----------------------------------------------------------
        ! input ...
        ! a(n,n) - array of coefficients for matrix A
        ! n      - dimension
        ! output ...
        ! c(n,n) - inverse matrix of A
        ! comments ...
        ! the original matrix a(n,n) will be destroyed 
        ! during the calculation
        !===========================================================
        implicit none 
        integer n
        double precision a(n,n), c(n,n)
        double precision L(n,n), U(n,n), b(n), d(n), x(n)
        double precision coeff
        integer i, j, k

        ! step 0: initialization for matrices L and U and b
        ! Fortran 90/95 aloows such operations on matrices
        L=0.0
        U=0.0
        b=0.0

        ! step 1: forward elimination
        do k=1, n-1
            do i=k+1,n
                coeff=a(i,k)/a(k,k)
                L(i,k) = coeff
                do j=k+1,n
                    a(i,j) = a(i,j)-coeff*a(k,j)
                end do
            end do
        end do

        ! Step 2: prepare L and U matrices 
        ! L matrix is a matrix of the elimination coefficient
        ! + the diagonal elements are 1.0
        do i=1,n
            L(i,i) = 1.0
        end do
        ! U matrix is the upper triangular part of A
        do j=1,n
            do i=1,j
            U(i,j) = a(i,j)
            end do
        end do

        ! Step 3: compute columns of the inverse matrix C
        do k=1,n
            b(k)=1.0
            d(1) = b(1)
        ! Step 3a: Solve Ld=b using the forward substitution
            do i=2,n
            d(i)=b(i)
            do j=1,i-1
                d(i) = d(i) - L(i,j)*d(j)
            end do
            end do
        ! Step 3b: Solve Ux=d using the back substitution
            x(n)=d(n)/U(n,n)
            do i = n-1,1,-1
            x(i) = d(i)
            do j=n,i+1,-1
                x(i)=x(i)-U(i,j)*x(j)
            end do
            x(i) = x(i)/u(i,i)
            end do
        ! Step 3c: fill the solutions x(n) into column k of C
            do i=1,n
            c(i,k) = x(i)
            end do
            b(k)=0.0
        end do
        end subroutine inverse