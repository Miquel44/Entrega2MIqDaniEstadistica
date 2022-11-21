df = enquesta
df<- read.csv("enquesta.csv")
#1a
mean(df$TABAC)#probabilidad de que una persona fume
#
mean(df$OCI==4 & df$TABAC==1) #Guarda las probabilidades de los que fuman dependiendo del ocio
mean(df$OCI==3 & df$TABAC==1)
mean(df$OCI==2 & df$TABAC==1)
mean(df$OCI==1 & df$TABAC==1)
#viendo los resultados fuman menos
#1b
d160<-mean(df$ALTURA>=160 & df$SEXE=="d") #Variable Mujeres de 160 o mas
x<-mean((df$OCI==1 | df$OCI==2) & df$ALTURA>=160 & df$SEXE=="d") #Variable de mujeres de 160 o mas que fuman y miran televisores o Pc
(x/d160)*100  #Probabilidad de que si es mujer de 160, su ocio sea televisores u ordenadores
#1c
hsport<-mean(df$OCI==4  & df$SEXE=="h")
hof<-mean(df$OCI==4 & df$TABAC==1 & df$SEXE=="h") #Variable hombres que hacen deporte y fuman 
hof/hsport*100

fumador<-mean(df$TABAC==1) #Variable fumadores
hof/fumador*100 #Probabilidades de que si es fumador sea hombre que hace deporte
#1d
mean(df$OCI==4 & df$TABAC==0) #No fumadores que hacen deporte 
mean((df$OCI==2 | df$OCI==3) & df$TABAC==1) #Fumadores que usan el computador o se aficionan a la musica/lectura

#1e
mean(df$PES>60 & df$EDAT>=20)#Personas que pesan mas de 60kg que tengan 20 o mas
mean(df$PES<70 & df$EDAT<=50)#Personas que pesan menos de 70 y tienen 50 o menos
#2a
dau<-function(k){   #Funcion de K para valores pares e impares
  if (k%%2==0){
    return(print("P(x=k)=(k-1)/(5k)"))
  }else{
    return(print("P(x=k)=(1+2k)/36"))
  }
}
#2b
x<-c(1:6) #Representamos un vector con los valores del dado y sus posibilidades
y<-c(1/12,1/10,7/36,3/20,11/36,1/6) 
plot(x,y,type="h") #Grafica masa de 
#dado1
acum<-cumsum(y) # Suma acumulada
s<-stepfun(x,c(0,acum)) #Mete los valores X a k uno a uno
plotdau1<-plot(s,verticals=FALSE) #Grafica dado 1
#dado2
xn<-c(1:6)
yn<-c(1/6,1/6,1/6,1/6,1/6,1/6)
acumn<-cumsum(yn)
sn<-stepfun(xn,c(0,acumn))
#Grafica ambos
plotdau2<-plot(sn,verticals=FALSE,col = 'red')
par(new=T)
plotdau1<-plot(s,verticals=FALSE)
#2C
plot(x,y,type="h")
sum(y[c(1,3,5)])
sum(y[c(2,4,6)])
sum(y[c(5,6)])
sum(y[c(1,2,3,4)])
#2d
mp<-mean(1/12,1/10,7/36,3/20,11/36,1/6) 
m<-mean(1,2,3,4,5,6)
varianza<-{(((1/12-mp)*(1-m))+((1/10-mp)*(2-m))+((7/36-mp)*(3-m))+((3/20-mp)*(4-m))+((11/36-mp)*(5-m))+((1/6-mp)*(6-m))/6)}
            
Esperança<-1/12*1+1/10*2+7/36*3+3/20*4+11/36*5+1/6*6

y<-c(1/12,1/10,7/36,3/20,11/36,1/6) 
list<-sample(1:6,300,replace=T,prob=(y))
mean(list)
var(list)


#2e
caso2<-(1/12)*(1/12) 
caso3<-(1/12)*(1/10)*2
caso4<-((1/12)*(7/36)*2) + (1/10)*(1/10)
ProbResul<-1-sum(caso2,caso3,caso4)

caso11<-(1/6)*(11/36)*2    #Que de 11, para restar a prob total => 5
caso12<-(1/6)*(1/6)   #Resultado 12, restar a prob total >= 5
Probtotalres<- ProbResul-caso11-caso12



#3a
x<- seq(1.00000000000000000000001,2.9999999999999999999,length=80000) #agarra 100 puntos desde el 1 al 3
plot((1/4)*((x-1)**3),type='l',xlab='1<x<3')
y<-(1/4)*((x-1)**3)

int1<-integrate(function(x){(1/4)*((x-1)**3)},lower = 1, upper = )
#1 con error absoluto<1.1e-14
1**(-1)

x1<-seq(1,3,length=10)
plot((16/((z-1)**4)),type='l',xlab='1<x<3',col="red")


#3b
z<-runif(80000,min=1,max=3)

hist((1/4)*(((z)-1)**3))
par(new=T)
plot((1/4)*((x1-1)**3),type='l',xlab='1<x<3')
#3c
#esperanza empirica
esperanza3c <- (sum(z)*1/80000)
#esperanza teorica
Esperanza3_c
#var teorica
media3c1<-(1/80000)
media3c2<-mean(z)
var3_c<- (1/80000)*sum(((1/80000)-(1/80000))*(z-media3c2))
#var empirica
variancia3<- var(z)


#4a
(295-298)/3
#mirant les tables de distribucio normal la probabilitat es 0,8413
probab1<-1-0.8413
(290-298)/3
#0.0039
(310-298)/3
#0.999968329
probab2<-0.999968329-(0.0039)
#4b
#mirant les tables seria un 1,64
#(x-298)/3=1,64
1.64*3+298
#4c
a<-(factorial(6)/(factorial(0)*factorial(6-0)))*probab1**0*(1-probab1)**(6-0)
b<-(factorial(6)/(factorial(1)*factorial(6-1)))*probab1**1*(1-probab1)**(6-1)
c<-(factorial(6)/(factorial(2)*factorial(6-2)))*probab1**2*(1-probab1)**(6-2)
d<-(factorial(6)/(factorial(3)*factorial(6-3)))*probab1**3*(1-probab1)**(6-3)
e<-(factorial(6)/(factorial(4)*factorial(6-4)))*probab1**4*(1-probab1)**(6-4)
f<-(factorial(6)/(factorial(5)*factorial(6-5)))*probab1**5*(1-probab1)**(6-5)
g<-(factorial(6)/(factorial(6)*factorial(6-6)))*probab1**6*(1-probab1)**(6-6)
pcaixa<-sum(c,d,e,f,g)
p<-0
for (i in 30:100){
  p<-p+(factorial(100)/(factorial(i)*factorial(100-i)))*pcaixa**i*(1-pcaixa)**(100-i)
}
#4d
dist<-24.4
me<-mean(a,b,c,d,f,g)
ml<-mean(0,1,2,3,4,5,6)
desv<-sqrt(((((a-me)*(0-ml))**2+((b-me)*(1-ml))**2+((c-me)*(2-ml))**2+((d-me)*(3-ml))**2+((e-me)*(4-ml))**2+((f-me)*(5-ml))**2)+((g-me)*(6-ml))**2)/7)
#4e
(30-dist)/desv
#1-0.999996602
