df<- read.csv("enquesta.csv")
#1a
mean(df$TABAC)#probabilidad de que una persona fume
#
mean(df$OCI==4 & df$TABAC==1)
mean(df$OCI==3 & df$TABAC==1)
mean(df$OCI==2 & df$TABAC==1)
mean(df$OCI==1 & df$TABAC==1)
#viendo los resultados fuman menos
#1b
d160<-mean(df$ALTURA>=160 & df$SEXE=="d")
x<-mean((df$OCI==1 | df$OCI==2) & df$ALTURA>=160 & df$SEXE=="d")
(x/d160)*100
#1c
hof<-mean(df$OCI==4 & df$TABAC==1 & df$SEXE=="h")

fumon<-mean(df$TABAC==1)
hof/fumon*100
#1d
mean(df$OCI==4 & df$TABAC==0)
mean((df$OCI==2 | df$OCI==3) & df$TABAC==1)
#1e

