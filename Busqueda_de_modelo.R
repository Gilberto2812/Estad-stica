#Buscando el mejor modelo
source("funciones.R")
buscar_arma(c1,0,20)
#ARMA(12,12)   AIC  -276.4801
rm(buscar_arma)

#Mejor AR
n<-20
a<-Inf
o<-c(0,0,0)
for(j in 1:n){
  aic.x<-try(AIC(arima(c1,order = c(j,0,0),method = "ML")))
  if(a>aic.x){
    a<-aic.x
    o<-c(j,0,0)
  }
}
#Mejor es AR(16) AIC -244.61

#Mejor MA
n<-20
a<-Inf
o<-c(0,0,0)
for(j in 1:n){
  aic.x<-try(AIC(arima(c1,order = c(0,0,j),method = "ML")))
  if(a>aic.x){
    a<-aic.x
    o<-c(0,0,j)
  }
}
#Mejor es MA(19) AIC -163.4603}


#######MEJOR MODELO ARMA(12,12)