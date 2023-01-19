SUBROUTINE RASCHET(UK1,AIK1,LL,NN,PPP,PP1,PP2,PPP1,PPP2,PPP3,PPP4,PPP5,PPP6,PPP7,PPP8)

    COMMON MM,M,M1,MT,M10,M20,PR,K1,K2,K3,N1,N2,N3,MPR,MTR,MMT
    (K1, PR, MMT will have been changed in this function)

    DOUBLE PRECISION
         XA(M),YA(M),XL1(M,M),D(M,M),HC(M,M),UXM(M),
        /HC1(M,M),HC2(M,M),HC3(M,M),HC4(M,M),F10(M,M),XL(M,M),G(M,M),
        /DET20,GM(M),OMP(M),R0(M),R(M),S(M),HI(M),R11(M),DET2(M),DET4(M),AIXM(M),

        /PP1,PP2,

        /PPP(1000,50), PPP1(1000,50),PPP2(1000,50),PPP3(1000,50),PPP4(1000,50),
        /PPP5(1000,50),PPP6(1000,50),PPP7(1000,50),PPP8(1000,50)

    COMPLEX
         Z(M,M)*16, Y(M,M)*16, AU(M,M)*16, E(M,M)*16, F(M,M)*16,
        /EVU(M)*16, B(M)*16, UX(M)*16, AIX(M)*16, AAI(M,M)*16, UK1(M)*16,
        /AIK1(M)*16, B1(M20)*16, B4(M20)*16, SM(M),B5(M20)*16, B6(M10)*16,
        /B7(M10)*16, B10(M10)*16, F1(M,M)*16, F2(M,M)*16, D1(M,M)*16,
        /D2(M,M)*16, D3(M,M)*16, EVI(M)*16, LU(M,M)*16, LI(M,M)*16,
        /LU1(M,M)*16, LI1(M,M)*16, LU2(M,M)*16, LU3(M,M)*16, LI2(M,M)*16,
        /LI3(M,M)*16, AG(M1,M,M)*16, DET10*16, DET1(M)*16, F3(M,M)*16,
        /F4(M,M)*16, F5(M,M)*16, F6(M,M)*16, F7(M,M)*16, DET3(M)*16, EX1*16,
        /GG1(M20,M20)*16, GG2(M20,M20)*16, GG3(M10,M20)*16, GG4(M10,M10)*16,
        /GG5(M10,M10)*16, A1(M1,M1)*16, A2(M1,M1)*16, HH(M10,M10)*16,
        /GG(M20,M20)*16, CC(M,M)*16, DD(M,M)*16, HH11(M,M)*16, HH12(M,M)*16,
        /HH13(M,M)*16, HH14(M,M)*16, HH21(M,M)*16, HH22(M,M)*16, HH23(M,M)*16,
        /HH24(M,M)*16, HH31(M,M)*16, HH32(M,M)*16, HH33(M,M)*16, HH34(M,M)*16,
        /HH41(M,M)*16, HH42(M,M)*16, HH43(M,M)*16, HH44(M,M)*16, AA(M)*16,
        /BB(M)*16,

        /SS*16, SS1*16

    INTEGER IPVT1(M1),IH(M20)

**********************************************************************
    IF(PR.EQ.1)PP1=0.
    IF(PR.EQ.2)PP2=0.
    PPP(NN,LL)=0.
    PPP1(NN,LL)=0.
    PPP2(NN,LL)=0.
    PPP3(NN,LL)=0.
    PPP4(NN,LL)=0.
    PPP5(NN,LL)=0.
    PPP6(NN,LL)=0.
    PPP7(NN,LL)=0.
    PPP8(NN,LL)=0.
**********************************************************************
    PI=3.14159
    RZ=35.3
    MMT=MM/MT
    W=FLOAT(LL)
    EX1=CMPLX(2.71828, 0.)
    REWIND 2
    READ(2,195) (XA(I), I=1,M)
    WRITE(5,195) (XA(I),I=1,M)
    READ(2,195) (YA(I),I=1,M)
    WRITE(5,195) (YA(I),I=1,M)
    READ(2,195) (OMP(I),I=1,M)
    WRITE(5,195) (OMP(I),I=1,M)
    READ(2,195) (GM(I),I=1,M)
    WRITE(5,195) (GM(I), I=1,M)
    READ(2,195) (S(I), I=1,M)
    WRITE(5,195) (S(I), I=1,M)
    READ(2,191) IH
    WRITE(5,191) IH
    DO 845 I=1,M
    R(I)=SQRT(S(I)/PI)/1000.
    HI(I)=R(I)/(2.)*SQRT(2*PI*W*50*4*PI*OMP(I)*GM(I)/20.)
    R0(I)=1000./(GM(I)*S(I))
    IF(HI(I).LT.1) R11(I)=R0(I)*(1+HI(I)**4/3.)
    IF(HI(I).GT.1) R11(I)=R0(I)*(HI(I)+0.25+3./(64.*HI(I)))
    IF(I.EQ.M)WRITE(12,198) R11
  845   CONTINUE
    1   CONTINUE
    DO 12 I=1,M10
    DO 12 J=1,M10
    HH(I,J)=0.
   12   CONTINUE
    DO 161 I=1,M
    DO 161 J=1,M
    IF(I.EQ.J)D(I,I)=R(I)
    IF(I.NE.J)D(I,J)=SQRT((XA(I)-XA(J))**2+(YA(I)-YA(J))**2)
    HC(I,J)=SQRT((XA(I)-XA(J))**2+(YA(I)+YA(J))**2)
    E(I,J)=CMPLX(0.0, 0.0)
    E(I,I)=CMPLX(1.0, 0.0)
  161   CONTINUE
    DO 740 I=1,M
    DO 740 J=1,M
    XL1(I,J)=0.145*DLOG10(1000./D(I,J))/314.16
  740 CONTINUE
    DO 743 I=1,M
    DO 743 J=1,M
        HC1(I,J)=41.4*10.**6*DLOG10(HC(I,J)/D(I,J))
  743   CONTINUE
    CALL DLINRG(M,HC1,M,HC3,M)
    CALL DMRRRR(M,M,HC1,M,M,M,HC3,M,M,M,F10,M)
    DO 744 I=1,M
    DO 744 J=1,M
    HC2(I,J)=HC3(I,J)*2.*PI*50.
  744 CONTINUE
      DO 847 I=1,M
    DO 847 J=1,M
    XL(I,J)=XL1(I,J)*W*2*50*PI
    HC4(I,J)=HC2(I,J)*W
    R10=0.0
    IF(I.EQ.J)Z(I,J)=CMPLX(R11(I),XL(I,J))
    IF(I.NE.J)Z(I,J)=CMPLX(R10,XL(I,J))
    IF(I.EQ.J)G(I,J)=0.00000004*YA(I)/YA(I)
    IF(I.NE.J)G(I,J)=-0.00000004*YA(1)/D(I,J)
    G(I,J)=0.
    Y(I,J)=CMPLX(G(I,J),HC4(I,J))
  847   CONTINUE
    DO 1300 III=1,MT
    IF(M.NE.3)GOTO 767
    DO 761 I=1,3
    B5(I)=UK1(I)
    B5(I+3)=AIK1(I)
    B5(I+6)=CMPLX(0.,0.)
    B5(I+9)=CMPLX(0.,0.)
  761   CONTINUE
  767   IF(M.NE.4)GOTO 768
    DO 762 I=1,3
    B5(I)=UK1(I)
    B5(M)=CMPLX(0.,0.)
    B5(I+M)=AIK1(I)
    B5(2*M)=CMPLX(0.,0.)
    B5(I+2*M)=CMPLX(0.,0.)
    B5(3*M)=CMPLX(0.,0.)
    B5(I+3*M)=CMPLX(0.,0.)
    B5(4*M)=CMPLX(0.,0.)
  762   CONTINUE
  768   IF(M.NE.6)GOTO 769
    DO 764 I=1,3
    B5(I)=UK1(I)
    B5(I+3)=UK1(I)
    B5(I+M)=AIK1(I)
    B5(I+M+3)=AIK1(I)
    B5(I+2*M)=CMPLX(0.,0.)
    B5(I+2*M+3)=CMPLX(0.,0.)
    B5(I+3*M)=CMPLX(0.,0.)
    B5(I+3*M+3)=CMPLX(0.,0.)
  764   CONTINUE
  769   IF(M.NE.7)GOTO 770
    DO 765 I=1,3
    B5(I)=UK1(I)
    B5(I+3)=UK1(I)
    B5(M)=CMPLX(0.,0.)
    B5(I+M)=AIK1(I)
    B5(I+M+3)=AIK1(I)
    B5(2*M)=CMPLX(0.,0.)
    B5(I+2*M)=CMPLX(0.,0.)
    B5(I+2*M+3)=CMPLX(0.,0.)
    B5(3*M)=CMPLX(0.,0.)
    B5(I+3*M)=CMPLX(0.,0.)
    B5(I+3*M+3)=CMPLX(0.,0.)
    B5(4*M)=CMPLX(0.,0.)
  765   CONTINUE
  770   IF(M.NE.8)GOTO 771
    DO 766 I=1,3
    B5(I)=UK1(I)
    B5(I+3)=UK1(I)
    B5(MPR+1)=CMPLX(0.,0.)
    B5(MPR+2)=CMPLX(0.,0.)
    B5(I+M)=AIK1(I)
    B5(I+M+3)=AIK1(I)
    B5(2*M-1)=CMPLX(0.,0.)
    B5(2*M)=CMPLX(0.,0.)
    B5(I+2*M)=CMPLX(0.,0.)
    B5(I+2*M+3)=CMPLX(0.,0.)
    B5(3*M-1)=CMPLX(0.,0.)
    B5(3*M)=CMPLX(0.,0.)
    B5(I+3*M)=CMPLX(0.,0.)
    B5(I+3*M+3)=CMPLX(0.,0.)
    B5(4*M-1)=CMPLX(0.,0.)
    B5(4*M)=CMPLX(0.,0.)
  766   CONTINUE
  771   CONTINUE
C**********************************************************************
C   ÂÛ×ÈÑËÅÍÈÅ ÌÀÒÐÈÖÛ LU
C   ÏÅÐÅÌÍÎÆÅÍÈÅ ÌÀÒÐÈÖ ÏÀÐÀÌÅÒÐÎÂ
    CALL DMCRCR(M,M,Z,M,M,M,Y,M,M,M,AU,M)
    SS1=CDSQRT(AU(1,1))
C   ÂÛ×ÈÑËÅÍÈÅ ÑÎÁÑÒÂÅÍÍÛÕ ÇÍÀ×ÅÍÈÉ ÌÀÒÐÈÖÛ ÀU
    CALL DEVLCG(M,AU,M,EVU)
    GOTO 1008

C++++++++++++++++++++++++++++++++ # MISSIONG PART # ++++++++++++++++++++++++++++++++++++
    DO 1001 I=1,M
    DO 1001 J=1,M
    D1(I,J)=E(I,J)
 1001   CONTINUE
    DO 1007 II=1,M
    DO 1007 JJ=1,M
    LU(II,JJ)=0.
 1007   CONTINUE
    DO 1002 K=1,M
    DO 1003 I=1,M
    IF(I.EQ.K) GOTO 1003
    DO 1004 II=1,M
    DO 1004 JJ=1,M
    D2(II,JJ)=(AU(II,JJ)-EVU(I)*E(II,JJ))/(EVU(K)-EVU(I))
 1004   CONTINUE
    CALL DMCRCR(M,M,D1,M,M,M,D2,M,M,M,D3,M)
    DO 1005 II=1,M
    DO 1005 JJ=1,M
    D1(II,JJ)=D3(II,JJ)
 1005   CONTINUE
 1003   CONTINUE
    DO 1006 II=1,M
    DO 1006 JJ=1,M
    LU(II,JJ)=LU(II,JJ)+CDSQRT(EVU(K))*D1(II,JJ)
 1006   CONTINUE
 1002   CONTINUE
C++++++++++++++++++++++++++++++ # MISSIONG PART # ++++++++++++++++++++++++++++++++++++++++

 1008   CONTINUE
C   ÔÎÐÌÈÐÎÂÀÍÈÅ ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 20 J=1,M
    DO 20 I=1,M
    F(I,J)=EVU(I)**(J-1)
    F1(I,J)=F(I,J)
   20   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 201 I=1,M1
    DO 201 J=1,M1
    A1(I,J)=F(I,J)
  201   CONTINUE
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
    CALL DLFDCG(M1,A2,M1,IPVT1,DET10,DET20)
C   ÂÛ×ÈÑËÅÍÈÅ ÎÏÐÅÄÅËÈÒÅËß ÂÀÍÄÅÐÌÎÍÄÀ
    CONTINUE
    SS=DET10*(10.**DET20)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 21 J=1,M1
    DO 211 II=1,M1
    DO 211 JJ=1,M1
    F(II,JJ)=F1(II,JJ)
  211   CONTINUE
    DO 22 I=1,M1
    F(I,J)=EVU(I)**0.5
   22   CONTINUE
      DO 202 II=1,M1
    DO 202 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
  202   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
      CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÎÏÐÅÄÅËÈÒÅËÅÉ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET1(J),DET2(J))
   21   CONTINUE
C   ÂÛ×ÈÑËÅÍÈÅ ÇÍÀ×ÅÍÈß ÔÓÍÊÖÈÈ ÎÒ ÌÀÒÐÈÖÛ
 1700   CONTINUE
    CALL DMCRCR(M,M,AU, M,M,M,AU,M,M,M,F2,M)
    CALL DMCRCR(M,M,F2,M,M,M,AU,M,M,M,F3,M)
    CALL DMCRCR(M,M,F3,M,M,M,AU,M,M,M,F4,M)
    CALL DMCRCR(M,M,F4,M,M,M,AU,M,M,M,F5,M)
    CALL DMCRCR(M,M,F5,M,M,M,AU,M,M,M,F6,M)
    CALL DMCRCR(M,M,F6,M,M,M,AU,M,M,M,F7,M)
    DO 441 I=1,M1
    DO 441 II=1,M
    DO 441 JJ=1,M
    IF(I.EQ.1)AG(1,II,JJ)=E(II,JJ)
    IF(I.EQ.2)AG(2,II,JJ)=AU(II,JJ)
    IF(I.EQ.3)AG(3,II,JJ)=F2(II,JJ)
    IF(I.EQ.4)AG(4,II,JJ)=F3(II,JJ)
    IF(I.EQ.5)AG(5,II,JJ)=F4(II,JJ)
    IF(I.EQ.6)AG(6,II,JJ)=F5(II,JJ)
    IF(I.EQ.7)AG(7,II,JJ)=F6(II,JJ)
    IF(I.EQ.8)AG(8,II,JJ)=F7(II,JJ)
  441   CONTINUE
      DO 442 II=1,M
    DO 442 JJ=1,M
    LU(II,JJ)=0.
  442   CONTINUE
    DO 410 I=1,M1
    DET3(I)=DET1(I)/DET10
    DET4(I)=DET2(I)-DET20
    DO 410 II=1,M
    DO 410 JJ=1,M
    LU(II,JJ)=LU(II,JJ)+AG(I,II,JJ)*DET3(I)*(10**DET4(I))
  410   CONTINUE
    CALL DMCRCR(M,M,LU,M,M,M,LU,M,M,M,F3,M)
C************************************************************
C************************************************************
C   ÂÛ×ÈÑËÅÍÈÅ ÌÀÒÐÈÖÛ LI
C   ÏÅÐÅÌÍÎÆÅÍÈÅ ÌÀÒÐÈÖ ÏÀÐÀÌÅÒÐÎÂ
    CALL DMCRCR(M,M,Y,M,M,M,Z,M,M,M,AAI,M)
C   ÂÛ×ÈÑËÅÍÈÅ ÑÎÁÑÒÂÅÍÍÛÕ ÇÍÀ×ÅÍÈÉ ÌÀÒÐÈÖÛ AAI
      CALL DEVLCG(M,AAI,M,EVI)
C   ÔÎÐÌÈÐÎÂÀÍÈÅ ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 1120 J=1,M
    DO 1120 I=1,M
    F(I,J)=EVI(I)**(J-1)
    F1(I,J)=F(I,J)
 1120   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 1201 I=1,M1
    DO 1201 J=1,M1
    A1(I,J)=F(I,J)
 1201   CONTINUE
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÎÏÐÅÄÅËÈÒÅËß ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET10,DET20)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 121 J=1,M1
    DO 1211 II=1,M1
    DO 1211 JJ=1,M1
    F(II,JJ)=F1(II,JJ)
 1211   CONTINUE
    DO 122 I=1,M1
    F(I,J)=EVI(I)**0.5
  122   CONTINUE
    DO 1202 II=1,M1
    DO 1202 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
 1202   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
      CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÎÏÐÅÄÅËÈÒÅËÅÉ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET1(J),DET2(J))
  121   CONTINUE
C   ÂÛ×ÈÑËÅÍÈÅ ÇÍÀ×ÅÍÈß ÔÓÍÊÖÈÈ ÎÒ ÌÀÒÐÈÖÛ
11700   CONTINUE
    CALL DMCRCR(M,M,AAI, M,M,M,AAI,M,M,M,F2,M)
    CALL DMCRCR(M,M,F2,M,M,M,AAI,M,M,M,F3,M)
    CALL DMCRCR(M,M,F3,M,M,M,AAI,M,M,M,F4,M)
    CALL DMCRCR(M,M,F4,M,M,M,AAI,M,M,M,F5,M)
    CALL DMCRCR(M,M,F5,M,M,M,AAI,M,M,M,F6,M)
    CALL DMCRCR(M,M,F6,M,M,M,AAI,M,M,M,F7,M)
    DO 1441 I=1,M1
    DO 1441 II=1,M
    DO 1441 JJ=1,M
    IF(I.EQ.1)AG(1,II,JJ)=E(II,JJ)
    IF(I.EQ.2)AG(2,II,JJ)=AAI(II,JJ)
    IF(I.EQ.3)AG(3,II,JJ)=F2(II,JJ)
    IF(I.EQ.4)AG(4,II,JJ)=F3(II,JJ)
    IF(I.EQ.5)AG(5,II,JJ)=F4(II,JJ)
    IF(I.EQ.6)AG(6,II,JJ)=F5(II,JJ)
    IF(I.EQ.7)AG(7,II,JJ)=F6(II,JJ)
    IF(I.EQ.8)AG(8,II,JJ)=F7(II,JJ)
 1441   CONTINUE
    DO 1442 II=1,M
    DO 1442 JJ=1,M
    LI(II,JJ)=0.
 1442   CONTINUE
    DO 1410 I=1,M1
    DO 1410 II=1,M
    DO 1410 JJ=1,M
    LI(II,JJ)=LI(II,JJ)+
    /(DET1(I)*(10**DET2(I))*AG(I,II,JJ))/(DET10*(10**DET20))
 1410   CONTINUE
C*********************************************************************
    CALL DMCRCR(M,M,LI,M,M,M,LI,M,M,M,F3,M)
    LM=MMT
    DO 7301 N=1,2
    IF (N.EQ.1) SA=-1.
    IF (N.EQ.2) SA=1.
C   ÂÛ×ÈÑËÅÍÈÅ ÌÀÒÐÈ×ÍÛÕ ÝÊÑÏÎÍÅÍÖÈÀËÜÍÛÕ ÔÓÍÊÖÈÉ
    DO 7011 II=1,M
    DO 7011 JJ=1,M
    LU1(II,JJ)=SA*MMT*LU(II,JJ)
 7011   CONTINUE
C   ÂÛ×ÈÑËÅÍÈÅ ÑÎÁÑÒÂÅÍÍÛÕ ÇÍÀ×ÅÍÈÉ ÌÀÒÐÈÖÛ LU1
    CALL DEVLCG(M,LU1,M,EVU)
C   ÔÎÐÌÈÐÎÂÀÍÈÅ ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 720 I=1,M
    DO 720 J=1,M
    F(I,J)=EVU(I)**(J-1)
  720   CONTINUE
    DO 7201 II=1,M
    DO 7201 JJ=1,M
    F1(II,JJ)=F(II,JJ)
 7201   CONTINUE
    DO 7334 II=1,M1
    DO 7334 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
 7334   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÎÏÐÅÄÅËÈÒÅËß ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET10,DET20)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 7221 J=1,M
    DO 72211 II=1,M
    DO 72211 JJ=1,M
    F(II,JJ)=F1(II,JJ)
72211   CONTINUE
    DO 7222 I=1,M
    F(I,J)=EX1**EVU(I)
 7222 CONTINUE
    DO 7335 II=1,M1
    DO 7335 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
 7335   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÎÏÐÅÄÅËÈÒÅËÅÉ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET1(J),DET2(J))
 7221   CONTINUE
    CALL DMCRCR(M,M,LU1, M,M,M,LU1,M,M,M,F2,M)
    CALL DMCRCR(M,M,F2,M,M,M,LU1,M,M,M,F3,M)
    CALL DMCRCR(M,M,F3,M,M,M,LU1,M,M,M,F4,M)
    CALL DMCRCR(M,M,F4,M,M,M,LU1,M,M,M,F5,M)
    CALL DMCRCR(M,M,F5,M,M,M,LU1,M,M,M,F6,M)
    CALL DMCRCR(M,M,F6,M,M,M,LU1,M,M,M,F7,M)
    DO 7440 I=1,M1
    DO 7440 II=1,M
    DO 7440 JJ=1,M
    IF(I.EQ.1)AG(1,II,JJ)=E(II,JJ)
    IF(I.EQ.2)AG(2,II,JJ)=LU1(II,JJ)
    IF(I.EQ.3)AG(3,II,JJ)=F2(II,JJ)
    IF(I.EQ.4)AG(4,II,JJ)=F3(II,JJ)
    IF(I.EQ.5)AG(5,II,JJ)=F4(II,JJ)
    IF(I.EQ.6)AG(6,II,JJ)=F5(II,JJ)
    IF(I.EQ.7)AG(7,II,JJ)=F6(II,JJ)
    IF(I.EQ.8)AG(8,II,JJ)=F7(II,JJ)
 7440   CONTINUE
    DO 7445 II=1,M
    DO 7445 JJ=1,M
    LU2(II,JJ)=0.
 7445   CONTINUE
    DO 7444 I=1,M1
    DO 7444 II=1,M
    DO 7444 JJ=1,M
    LU2(II,JJ)=LU2(II,JJ)+
    /(DET1(I)*(10**DET2(I))*AG(I,II,JJ))/(DET10*(10**DET20))
 7444   CONTINUE
    IF(N.EQ.2) GOTO 72411
    DO 72412 II=1,M
    DO 72412 JJ=1,M
    LU3(II,JJ)=LU2(II,JJ)
72412   CONTINUE
72411   CONTINUE
    CALL DMCRCR(M,M,LU3,M,M,M,LU2,M,M,M,F2,M)
 7301   CONTINUE


C************************************************************
C**********************************************************************
    DO 8401 N=1,2
    IF (N.EQ.1) SA=-1.
    IF (N.EQ.2) SA=1.
C   ÂÛ×ÈÑËÅÍÈÅ ÌÀÒÐÈ×ÍÛÕ ÝÊÑÏÎÍÅÍÖÈÀËÜÍÛÕ ÔÓÍÊÖÈÉ
    DO 81011 II=1,M
    DO 81011 JJ=1,M
    LI1(II,JJ)=SA*MMT*LI(II,JJ)
81011   CONTINUE
C*********************************************************************
C   ÂÛ×ÈÑËÅÍÈÅ ÑÎÁÑÒÂÅÍÍÛÕ ÇÍÀ×ÅÍÈÉ ÌÀÒÐÈÖÛ LI1
    CALL DEVLCG(M,LI1,M,EVI)
C   ÔÎÐÌÈÐÎÂÀÍÈÅ ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 8220 I=1,M
    DO 8220 J=1,M
    F(I,J)=EVI(I)**(J-1)
 8220   CONTINUE
    DO 82201 II=1,M
    DO 82201 JJ=1,M
    F1(II,JJ)=F(II,JJ)
82201   CONTINUE
    DO 8334 II=1,M1
    DO 8334 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
 8334 CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÌÀÒÐÈÖÛ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÎÏÐÅÄÅËÈÒÅËß ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET10,DET20)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    DO 8221 J=1,M
    DO 82211 II=1,M
    DO 82211 JJ=1,M
    F(II,JJ)=F1(II,JJ)
82211   CONTINUE
    DO 8222 I=1,M
    F(I,J)=EX1**EVI(I)
 8222   CONTINUE
    DO 8335 II=1,M1
    DO 8335 JJ=1,M1
    A1(II,JJ)=F(II,JJ)
 8335   CONTINUE
C   ÔÀÊÒÎÐÈÇÀÖÈß ÄÎÏÎËÍßÞÙÈÕ ÌÀÒÐÈÖ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFTCG(M1,A1,M1,A2,M1,IPVT1)
C   ÂÛ×ÈÑËÅÍÈÅ ÄÎÏÎËÍßÞÙÈÕ ÎÏÐÅÄÅËÈÒÅËÅÉ ÂÀÍÄÅÐÌÎÍÄÀ
    CALL DLFDCG(M1,A2,M1,IPVT1,DET1(J),DET2(J))
 8221   CONTINUE
    CALL DMCRCR(M,M,LI1, M,M,M,LI1,M,M,M,F2,M)
    CALL DMCRCR(M,M,F2,M,M,M,LI1,M,M,M,F3,M)
    CALL DMCRCR(M,M,F3,M,M,M,LI1,M,M,M,F4,M)
    CALL DMCRCR(M,M,F4,M,M,M,LI1,M,M,M,F5,M)
    CALL DMCRCR(M,M,F5,M,M,M,LI1,M,M,M,F6,M)
    CALL DMCRCR(M,M,F6,M,M,M,LI1,M,M,M,F7,M)
    DO 8440 I=1,M1
    DO 8440 II=1,M
    DO 8440 JJ=1,M
    IF(I.EQ.1)AG(1,II,JJ)=E(II,JJ)
    IF(I.EQ.2)AG(2,II,JJ)=LI1(II,JJ)
    IF(I.EQ.3)AG(3,II,JJ)=F2(II,JJ)
    IF(I.EQ.4)AG(4,II,JJ)=F3(II,JJ)
    IF(I.EQ.5)AG(5,II,JJ)=F4(II,JJ)
    IF(I.EQ.6)AG(6,II,JJ)=F5(II,JJ)
    IF(I.EQ.7)AG(7,II,JJ)=F6(II,JJ)
    IF(I.EQ.8)AG(8,II,JJ)=F7(II,JJ)
 8440   CONTINUE
    DO 8445 II=1,M
    DO 8445 JJ=1,M
    LI2(II,JJ)=0.
 8445   CONTINUE
    DO 8444 I=1,M1
    DO 8444 II=1,M
    DO 8444 JJ=1,M
    LI2(II,JJ)=LI2(II,JJ)+
    /(DET1(I)*(10**DET2(I))*AG(I,II,JJ))/(DET10*(10**DET20))
 8444   CONTINUE
    IF(N.EQ.2) GOTO 82411
    DO 82412 II=1,M
    DO 82412 JJ=1,M
    LI3(II,JJ)=LI2(II,JJ)
82412   CONTINUE
82411 CONTINUE
 8401   CONTINUE
    CALL DMCRCR(M,M,LI3, M,M,M,LI2,M,M,M,F2,M)
C**************************************************************************
C*************************************************************
 1112   CONTINUE
    DO 102 I=1,M20
    DO 102 J=1,M20
    GG(I,J)=0.
    GG1(I,J)=0.
    GG2(I,J)=0.
  102   CONTINUE
    DO 103 I=1,M
    GG(I,I)=1.
    GG(I,I+M)=1.
    GG(I+M,I+2*M)=1.
    GG(I+M,I+3*M)=1.
    GG1(I,I)=1.
    GG1(I,I+M)=1.
    GG1(I+M,I+2*M)=1.
    GG1(I+M,I+3*M)=1.
  103   CONTINUE
    DO 104 I=1,M
    DO 104 J=1,M
    GG(I+2*M,J)=-LU(I,J)
    GG(I+2*M,J+M)=LU(I,J)
    GG(I+3*M,J+2*M)=-LI(I,J)
    GG(I+3*M,J+3*M)=+LI(I,J)
    GG1(I+2*M,J)=LU3(I,J)
    GG1(I+2*M,J+M)=LU2(I,J)
    GG1(I+3*M,J+2*M)=LI3(I,J)
    GG1(I+3*M,J+3*M)=LI2(I,J)
  104   CONTINUE
    CALL DLINCG (M20,GG1,M20,GG2,M20)
    DO 316 I=1,M
    DO 316 J=1,M
    HH11(I,J)=GG2(I,J)
    HH12(I,J)=GG2(I,J+M)
    HH13(I,J)=GG2(I,J+2*M)
    HH14(I,J)=GG2(I,J+3*M)
    HH21(I,J)=GG2(I+M,J)
    HH22(I,J)=GG2(I+M,J+M)
    HH23(I,J)=GG2(I+M,J+2*M)
    HH24(I,J)=GG2(I+M,J+3*M)
    HH31(I,J)=GG2(I+2*M,J)
    HH32(I,J)=GG2(I+2*M,J+M)
    HH33(I,J)=GG2(I+2*M,J+2*M)
    HH34(I,J)=GG2(I+2*M,J+3*M)
    HH41(I,J)=GG2(I+3*M,J)
    HH42(I,J)=GG2(I+3*M,J+M)
    HH43(I,J)=GG2(I+3*M,J+2*M)
    HH44(I,J)=GG2(I+3*M,J+3*M)
  316   CONTINUE
    CALL DMCRCR (M,M,LU3,M,M,M,HH11,M,M,M,F,M)
    CALL DMCRCR (M,M,LU,M,M,M,F,M,M,M,HH11,M)
    CALL DMCRCR (M,M,LU2,M,M,M,HH21,M,M,M,F,M)
    CALL DMCRCR (M,M,LU,M,M,M,F,M,M,M,HH21,M)
    CALL DMCRCR (M,M,LU3,M,M,M,HH13,M,M,M,F,M)
    CALL DMCRCR (M,M,LU,M,M,M,F,M,M,M,HH13,M)
    CALL DMCRCR (M,M,LU2,M,M,M,HH23,M,M,M,F,M)
    CALL DMCRCR (M,M,LU,M,M,M,F,M,M,M,HH23,M)
    CALL DMCRCR (M,M,LI3,M,M,M,HH32,M,M,M,F,M)
    CALL DMCRCR (M,M,LI,M,M,M,F,M,M,M,HH32,M)
    CALL DMCRCR (M,M,LI2,M,M,M,HH42,M,M,M,F,M)
    CALL DMCRCR (M,M,LI,M,M,M,F,M,M,M,HH42,M)
    CALL DMCRCR (M,M,LI3,M,M,M,HH34,M,M,M,F,M)
    CALL DMCRCR (M,M,LI,M,M,M,F,M,M,M,HH34,M)
    CALL DMCRCR (M,M,LI2,M,M,M,HH44,M,M,M,F,M)
    CALL DMCRCR (M,M,LI,M,M,M,F,M,M,M,HH44,M)
    DO 105 I=1,M10
    DO 105 J=1,M20
    GG3(I,J)=0.0
  105   CONTINUE
    DO 306 I=1,M
    DO 306 J=1,M
    GG3(I,J)=-HH11(I,J)+HH21(I,J)
    GG3(I,J+2*M)=-HH13(I,J)+HH23(I,J)
    GG3(I,J+3*M)=-Z(I,J)
    GG3(I+M,J+M)=-HH32(I,J)+HH42(I,J)
    GG3(I+M,J+2*M)=-Y(I,J)
    GG3(I+M,J+3*M)=-HH34(I,J)+HH44(I,J)
  306   CONTINUE
    K1=0
    K0=0
    DO 307 J=1,M20
    IF (IH(J).EQ.1)K1=K1+1
    IF (IH(J).EQ.0)GOTO 309
    DO 308 I=1,M10
    GG4(I,K1)=-GG3(I,J)
    B10(K1)=B5(J)
  308   CONTINUE
    GOTO 307
  309   CONTINUE
    IF (IH(J).EQ.0)K0=K0+1
    IF (IH(J).EQ.1)GOTO 307
    DO 310 I=1,M10
    GG5(I,K0)=GG3(I,J)
  310   CONTINUE
  307   CONTINUE
    CALL DMUCRV (M10,M10,GG4,M10,M10,B10,1,M10,B6)
    CALL DLSLCG(M10,GG5,M10,B6,1,B7)
C*****************************************************
    K1=0
    DO 322 J=1,M20
    IF (IH(J).EQ.0) K1=K1+1
    IF (IH(J).EQ.1) GOTO 322
    B5(J)=B7(K1)
  322   CONTINUE
    DO 321 I=1,M
    UK1(I)=B5(I)
    AIK1(I)=B5(I+M)
  321   CONTINUE
    CALL DMUCRV(M,M,Z,M,M,AIK1,1,M,AA)
    CALL DMUCRV(M,M,Y,M,M,UK1,1,M,BB)
    CALL DMCRCR(M,M,LI,M,M,M,LI3,M,M,M,CC,M)
    CALL DMCRCR(M,M,LI,M,M,M,LI2,M,M,M,DD,M)
    DO 323 I=1,M
    B1(I)=UK1(I)
    B1(I+M)=AIK1(I)
    B1(I+2*M)=AA(I)
    B1(I+3*M)=BB(I)
  323   CONTINUE
    CALL DLSLCG(M20,GG,M20,B1,1,B4)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C**************************************************************************
    DO 1501 I=1,M
    AA(I)=0.
    B(I)=0.
 1501   B(I)=B4(I)
    CALL DMUCRV(M,M,LU3,M,M,B,1,M,AA)
    DO 1502 I=1,M
    BB(I)=0.
    B(I)=0.
 1502   B(I)=B4(I+M)
    CALL DMUCRV(M,M,LU2,M,M,B,1,M,BB)
    DO 1503 I=1,M
    UX(I)=AA(I)+BB(I)
    IF(LM.EQ.MMT)UK1(I)=UX(I)
 1503   UXM(I)=DSQRT(REAL(UX(I))**2+AIMAG(UX(I))**2)
    DO 1504 I=1,M
    AA(I)=0.
    B(I)=0.
 1504 B(I)=B4(I+2*M)
    CALL DMUCRV(M,M,LI3,M,M,B,1,M,AA)
    DO 1505 I=1,M
    BB(I)=0.
    B(I)=0.
 1505   B(I)=B4(I+3*M)
    CALL DMUCRV(M,M,LI2,M,M,B,1,M,BB)
    DO 1506 I=1,M
    AIX(I)=AA(I)+BB(I)
    IF(LM.EQ.MMT) AIK1(I)=AIX(I)
    AIXM(I)=SQRT(REAL(AIX(I))**2+AIMAG(AIX(I))**2)

C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DEBUGGING TOOL @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      PRINT *,AIXM(I)
C   WRITE(777,*)AIX(I)
C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DEBUGGING TOOL @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

c   ïðîâîä íîìåð 1
    IF(I.EQ.1.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP1(NN,LL)=PPP1(NN,LL)+AIXM(1)**2/2*R11(1)
    IF(I.EQ.1.AND.LL.GT.1)PPP1(NN,LL)=PPP1(NN,LL)+AIXM(1)**2/2*R11(1)
c   ïðîâîä íîìåð 2
    IF(I.EQ.2.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP2(NN,LL)=PPP2(NN,LL)+AIXM(2)**2/2*R11(2)
    IF(I.EQ.2.AND.LL.GT.1)PPP2(NN,LL)=PPP2(NN,LL)+AIXM(2)**2/2*R11(2)
c   ïðîâîä íîìåð 3
    IF(I.EQ.3.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP3(NN,LL)=PPP3(NN,LL)+AIXM(3)**2/2*R11(3)
    IF(I.EQ.3.AND.LL.GT.1)PPP3(NN,LL)=PPP3(NN,LL)+AIXM(3)**2/2*R11(3)
c   ïðîâîä íîìåð 4
    IF(I.EQ.4.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP4(NN,LL)=PPP4(NN,LL)+AIXM(4)**2/2*R11(4)
    IF(I.EQ.4.AND.LL.GT.1)PPP4(NN,LL)=PPP4(NN,LL)+AIXM(4)**2/2*R11(4)
c   ïðîâîä íîìåð 5
    IF(I.EQ.5.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP5(NN,LL)=PPP5(NN,LL)+AIXM(5)**2/2*R11(5)
    IF(I.EQ.5.AND.LL.GT.1)PPP5(NN,LL)=PPP5(NN,LL)+AIXM(5)**2/2*R11(5)
c   ïðîâîä íîìåð 6
    IF(I.EQ.6.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP6(NN,LL)=PPP6(NN,LL)+AIXM(6)**2/2*R11(6)
    IF(I.EQ.6.AND.LL.GT.1)PPP6(NN,LL)=PPP6(NN,LL)+AIXM(6)**2/2*R11(6)
c   ïðîâîä íîìåð 7
    IF(I.EQ.7.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP7(NN,LL)=PPP7(NN,LL)+AIXM(7)**2/2*R11(7)
    IF(I.EQ.7.AND.LL.GT.1)PPP7(NN,LL)=PPP7(NN,LL)+AIXM(7)**2/2*R11(7)
c   ïðîâîä íîìåð 8
    IF(I.EQ.8.AND.LL.EQ.1.AND.PR.EQ.2)
    /PPP8(NN,LL)=PPP8(NN,LL)+AIXM(8)**2/2*R11(8)
    IF(I.EQ.8.AND.LL.GT.1)PPP8(NN,LL)=PPP8(NN,LL)+AIXM(8)**2/2*R11(8)

c   ñóììàðíûå ïîòåðè âî âñåõ ïðîâîäàõ
    IF(LL.EQ.1.AND.PR.EQ.2.)PPP(NN,LL)=PPP(NN,LL)+AIXM(I)**2/2*R11(I)
    IF(LL.GT.1)PPP(NN,LL)=PPP(NN,LL)+AIXM(I)**2/2*R11(I)

c   write(10,196)ppp(nn,ll),aixm(i),r11(i)
    IF(LL.EQ.1.AND.PR.EQ.1.)PP1=PP1+AIXM(I)**2/2*R11(I)
    IF(LL.EQ.1.AND.PR.EQ.2.)PP2=PP2+AIXM(I)**2/2*R11(I)
    SM(I)=UX(I)*DCONJG(AIX(I))/2.
 1506   CONTINUE
    WRITE(10,196) UXM
    WRITE(11,196) AIXM
 2104   CONTINUE
 1300   CONTINUE
    GOTO 1200
C==============================================================================
C==============================================================================
    GOTO 10
    GOTO 1600
 1600   CONTINUE
   10 CONTINUE

  999   FORMAT(5X, F16.8)
  998 FORMAT(5X, 5I10)
  991 FORMAT('BLOCK 1 RUNS')
  992 FORMAT('BLOCK 2 RUNS')
  995   FORMAT('CALL RASCHET')
  993 FORMAT(I3,I3,I3)

    3   FORMAT(80X)
    2   FORMAT(6F20.15)
    6   FORMAT(20F15.10)
    7   FORMAT(8I6)
   77   FORMAT('******************************************************')
   78   FORMAT('FFU2')
   79   FORMAT('FFI2')
    8   FORMAT(24F20.15)
    9   FORMAT(20x,24F20.15)
   99   format(24f25.14)
  199   format(16f25.10)
  215   FORMAT(8F35.10)
  296   FORMAT(16F25.23)
  198   FORMAT(16F25.10)
  298   FORMAT(16F25.10)
  197   FORMAT(16E25.10)
  196   FORMAT(120E25.10)
  195   FORMAT(16F10.5)
  191   FORMAT(32I2)
  299   FORMAT(64E25.10)
 1200   CONTINUE
    RETURN
    END
