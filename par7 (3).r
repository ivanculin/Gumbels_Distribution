pY=runif(400,0,1)   ## Simuliramo 400 y koordinata iz uniforme razdiobe
pY=sort(pY)         ## Sortiramo y koordinate (ovo je pretpostavka algoritma)
pX=runif(400,0,1)   ## Simuliramo 400 x koordinata iz uniforme razdiobe
rez=c()             ## Vektor u koji cemo na mjesto rez[i] spremati stranicu najveceg kvadrata iz tocke (pX[i],pY[i])


for(i in 1:399)     ## Ovo je glavni algoritam koji je detaljno opisan u PDF-u 
{
  M=1-pY[i]
  L=0
  R=1
 
     for(j in (i+1):400 )
     { 
       if(pY[j]!=pY[i])
        {   
          if(pX[j]<=pX[i])
            L=max(pX[j],L)
          else
            R=min(pX[j],R)
        }
       
        val=max(R-L,pY[j]-pY[i])
        M=min(val,M)

     }
       
  rez[i]=M

}

rez[400]=(1-pY[400])


hist(rez,probability=TRUE,col="red",xlab=
       "Stranica najveceg kvadrata u tocki",breaks=20) ## Histogram dobivenih vrijednosti iz vektora rez



n=400
m=200                     ## Odabrane vrijednosti n i m

maksimum=c()              ## Vektor u koji na mjestu maksimum[i] spremamo vrijednost M_m za m-tu simulaciju

for(k in 1:m)             ## Iteriramo postupak od ranije m puta
{
  pY=runif(n,0,1)
  pY=sort(pY)
  pX=runif(n,0,1)
  rez=c()
  
  
  for(i in 1:(n-1))
  {
    M=1-pY[i]
    L=0
    R=1
  
    for(j in (i+1):n )
    { 
      if(pY[j]!=pY[i])
      {   
        if(pX[j]<=pX[i])
          L=max(pX[j],L)
        else
          R=min(pX[j],R)
      }
    
      val=max(R-L,pY[j]-pY[i])
      M=min(val,M)
    }
  
    rez[i]=M
  
  }
  
  rez[n]=(1-pY[n])
  
  
  maksimum[k]=max(rez)
  
  }
  



maksimum2=n*maksimum*maksimum -log(n) -log(log(n))         ## U vektor maksimum2 spremamo zadanu transformaciju vektora maksimum
hist(maksimum2,probability=TRUE,col="red",xlab="Mm")       ## Histogram vrijednosti vektora maksimum2
GumbelDens=function(x){exp(-(x+exp(-x)))}                  ## U varijablu GumbelDens spremamo funkciju gustoće Gumbelove razdiobe
curve(GumbelDens,add=TRUE)                                 ## Graf te funkcije dodajemo na histogram 


GumbelDistr=function(x){exp(-exp(-x))}                     ## U varijablu GumbelDistr spremamo funkciju distribucije Gumbelove razdiobe 
ks.test(maksimum2,"GumbelDistr",alternative="two.sided")   ## Uspoređujemo nase podatke s Gumbelovom razdiobom pomoću K-S testa
