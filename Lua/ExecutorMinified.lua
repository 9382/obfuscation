return(function(a)local b,c,d,e=string.sub,string.byte,string.rep,string.char;local f,g=math.floor,math.log;local h,i,j,k,l,m,n=type,ipairs,select,unpack,getfenv,tonumber,assert;local o,p,q=true,false,nil;local r;local s;local function t(u)local v={P=u,L={}}function v:GL(w)local x=v.L[w]if x then return x end;if v.P then local y=v.P:GL(w)if y then return y end end;return q end;function v:SL(w,z)v:GL(w)[1]=z end;function v:ML(w,z)local x={}x[0]=w;x[1]=z;v.L[w]=x;return x end;return v end;local A=l()local B={}local function C(...)return{...},j("#",...)end;local function D(E,F,v,G)local H=#E;if H==0 then return 0 end;for I=1,H-1 do F[I]=s(E[I],v)end;local J,K=C(s(E[H],v))for I=1,K do F[H+I-1]=J[I]end;B[F]={nil,H+K}return H+K-1 end;local function L(M,N)local O=B[M]if O then if not N then B[M]=nil end;return k(M,1,O[2]-1)else return k(M)end end;s=function(P,v,Q)local R=P[5]if R==1 then return function(...)local S=t(v)local T={...}for I=1,#P[2]do local U=P[2][I]if U then S:ML(U[0],T[I])end end;if P[3]then local V=j("#",...)local W={}for I=#P[2]+1,V do W[#W+1]=T[I]end;B[W]={nil,V-#P[2]+1}S:ML(-1,W)end;local X=r(P[1],S)if not X then return else return L(X.D)end end elseif R==2 then if Q then return P,o else if P[2]then return v:GL(P[0])[1]end;return A[P[0]]end elseif R==3 then if Q then return P,o else if P[2]==p then return s(P[0],v)[P[1][0]]elseif P[2]==o then local F=s(P[0],v)local Y=F[P[1][0]]if h(Y)=="function"then return function(...)return Y(F,...)end else return Y end end end elseif R==4 then if Q then return P,o else return s(P[0],v)[s(P[1],v)]end elseif R==5 or R==6 or R==7 then local Z={}if R==6 then Z={P[2][1][0]}else D(P[2],Z,v)end;if P[3]then return({s(P[0],v)(L(Z))})[1]else return s(P[0],v)(L(Z))end elseif R==8 then return m(P[1][0])elseif R==9 then return P[1][0]elseif R==10 then return q elseif R==11 then return P[1]elseif R==12 then return L(v:GL(-1)[1],o)elseif R==13 then local Y={}local _={}for a0,a1 in i(P[1])do if a1[2]==0 then Y[s(a1[3],v)]=s(a1[1],v)elseif a1[2]==1 then Y[a1[3]]=s(a1[1],v)else _[#_+1]=a1[1]end end;D(_,Y,v)return Y elseif R==14 then local a2=s(P[0],v)local a3=P[2]if a3==1 then return-a2 elseif a3==2 then return not a2 elseif a3==3 then return#a2 end elseif R==15 then local a3=P[2]local a4=s(P[1],v)if a3==14 then return a4 and s(P[0],v)elseif a3==15 then return a4 or s(P[0],v)end;local a2=s(P[0],v)if a3==1 then return a4+a2 elseif a3==2 then return a4-a2 elseif a3==3 then return a4%a2 elseif a3==4 then return a4/a2 elseif a3==5 then return a4*a2 elseif a3==6 then return a4^a2 elseif a3==7 then return a4 ..a2 elseif a3==8 then return a4==a2 elseif a3==9 then return a4<a2 elseif a3==10 then return a4<=a2 elseif a3==11 then return a4~=a2 elseif a3==12 then return a4>a2 elseif a3==13 then return a4>=a2 end end end;local a5=function(a6,v)local R=a6[5]if R==1 then local w=a6[0]if w[5]==3 then local F=s(w[0],v)if w[2]==p then local a7=s(a6,v)F[w[1][0]]=a7 elseif w[2]==o then local a7=s(a6,v,o)F[w[1][0]]=a7 end else local a7=s(a6,v)if a6[4]then v:ML(w[0],a7)elseif w[2]then v:SL(w[0],a7)else A[w[0]]=a7 end end elseif R==2 then for a0,a8 in i(a6[0])do if not a8[0]or s(a8[0],v)then return r(a8[1],t(v))end end elseif R==3 then while s(a6[0],v)do local X=r(a6[1],t(v))if X then if X.T==2 then return elseif X.T==1 then return X end end end elseif R==4 then return r(a6[1],t(v))elseif R==5 then local a9=m(s(a6[0],v))local aa=m(s(a6[2],v))local ab=a6[3]and m(s(a6[3],v))or 1;while ab>0 and a9<=aa or ab<=0 and a9>=aa do local S=t(v)S:ML(a6[4][0],a9)local X=r(a6[1],S)if X then if X.T==2 then return elseif X.T==1 then return X end end;a9=a9+ab end elseif R==6 then local ac,ad,ae;local af=a6[0]if not af[2]then ac,ad,ae=s(af[1],v)else ac=s(af[1],v)ad=s(af[2],v)if af[3]then ae=s(af[3],v)end end;while o do local S=t(v)local Z={ac(ad,ae)}ae=Z[1]if ae==q then break end;for I=1,#a6[2]do S:ML(a6[2][I][0],Z[I])end;local X=r(a6[1],S)if X then if X.T==2 then return elseif X.T==1 then return X end end end elseif R==7 then local ag=t(v)repeat local X=r(a6[1],ag)if X then if X.T==2 then return elseif X.T==1 then return X end end until s(a6[0],ag)elseif R==8 then local Y={}D(a6[0],Y,v)for I=1,#a6[1]do local ah=a6[1][I]v:ML(ah[0],Y[I])end elseif R==9 then local ai={}D(a6[2],ai,v)return ai elseif R==10 then return o elseif R==11 then return p elseif R==12 then local function aj(I)if I==#a6[0]then return s(a6[0][I],v)else return({s(a6[0][I],v)})[1]end end;for I=1,#a6[1]do local a4,ak=s(a6[1][I],v,o)if ak then if a4[5]==2 then if a4[2]then v:SL(a4[0],aj(I))else A[a4[0]]=aj(I)end elseif a4[5]==3 then local F=s(a4[0],v)F[a4[1][0]]=aj(I)else local F=s(a4[0],v)F[s(a4[1],v)]=aj(I)end end end elseif R==13 then s(a6[0],v)end end;r=function(al,v)for a0,am in i(al)do local Y=a5(am,v)if h(Y)=="table"then if not Y.P then return{P=o,T=1,D=Y}else return Y end elseif h(Y)=="boolean"then return{P=o,T=Y==o and 2 or 3}end end end;return function()a=(function(an)local function ao(ap,aq,ar)return d(ar,aq-#ap)..ap end;local function as(at)return m(at,2)end;local function au(aq,av)if aq==0 then return ao("0",av or 1,"0")end;local aw=f(g(aq,2))local ax=""while aw>=0 do local ay=2^aw;if aq>=ay then aq=aq-ay;ax=ax.."1"else ax=ax.."0"end;aw=aw-1 end;return ao(ax,av or 1,"0")end;local function az(aA)local aw=0;local aB=0;for aC=1,#aA do local aD=b(aA,aC,aC)if aD=="1"then aB=aB+2^aw end;aw=aw-1 end;return aB end;local aE=1;local aF=""local function aG(aH)for I=1,(aH-#aF-1)/6+1 do aF=aF..au(c(an,aE,aE)%64,6)aE=aE+1 end end;local function aI(aH)aG(aH)local aJ=b(aF,1,aH)aF=b(aF,aH+1)return aJ end;local function aK(aH)return as(aI(aH))end;local function aL()return e(aK(8))end;local function aM()local aN,aO,aP=aK(1),aK(11),aI(52)aN,aO=aN==0 and 1 or-1,2^(aO-1023)return aN*aO*az("1"..aP)end;local aQ=0;local aR=1;local aS=2;local aT=3;local aU=4;local aV=5;local aW=6;local aX=7;local aY=3;local function aZ(a_)if not a_ then n(aK(aY)==aQ,"Invalid SD")end;local b0={}local b1=q;local function b2(b3)if b1 then b0[b1]=b3;b1=q else b1=b3 end end;while o do local b4=aK(aY)if b4==aR then return b0 elseif b4==aQ then b2(aZ(o))elseif b4==aS then local b5=""while o do local b6=aL()if b6=="\0"then b2(b5)break elseif b6=="\\"then b5=b5 ..aL()else b5=b5 ..b6 end end elseif b4==aT then b2(aM())elseif b4==aU then b2(aK(1)==1)elseif b4==aW then b2(aK(3))elseif b4==aV then b2(aK(5))elseif b4==aX then b2(aK(8))end end end;return aZ()end)(a)local X=r(a,t())if not X then return else return L(X.D)end end end)([=[]=])