df<- read.csv("enquesta.csv")
head(df)
mean(df$TABAC)
table(df$TABAC)
df$OCI
table(df$OCI)
sum<-0
if (df$OCI==4 & df$TABAC==1):
  sum<-sum+1
  
