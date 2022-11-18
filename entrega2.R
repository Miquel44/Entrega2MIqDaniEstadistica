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
hof<-mean(df$OCI==4 & df$TABAC==1 & df$SEXE=="h") #Variable hombres que hacen deporte y fuman 

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
#2b
x<-c(1:6)
y<-c(1/12,1/10,7/36,3/20,11/36,1/6)
plot(x,y,type="h")
acum<-cumsum(y)
s<-stepfun(x,c(0,acum))
plotdau1<-plot(s,verticals=FALSE)
xn<-c(1:6)
yn<-c(1/6,1/6,1/6,1/6,1/6,1/6)
acumn<-cumsum(yn)
sn<-stepfun(xn,c(0,acumn))
plotdau2<-plot(sn,verticals=FALSE)
library(ggpubr)
combined_plot <- ggarrange(plotdau1,
                           plotdau2,
                           nrow = 2,
                           ncol = 1) 
