#Leemos datos y los convertimos a serie de tiempo
uc<-read.csv("https://raw.githubusercontent.com/Gilberto2812/Estad-stica/main/datos.csv")
uc1<-uc[199:218,]
s1<-ts(data = uc1$Monto_mensual, start = c(2020,1), end = c(2021,8),
       frequency = 12)
#Por problems del coronavirus limitaremos nuestro analisis 
#hasta el primer mes del 2020
uc2<-uc[1:199,]
s<-ts(data = uc2$Monto_mensual, start = c(2003,7), end = c(2020,1),
      frequency = 12)
plot(s,main="Serie de tiempo: Gastos por siniestros",
     xlab="Tiempo en años",ylab="Gasto mensual en USD")
rm(uc1,uc2)



#Hacemos pruebas de estacionariedad
#¿Es estacionaria en media?
mod<-lm(s~time(s))
summary(mod)
#H0: b1=0   (No tiene tendencia lineal)
#Ha: b1!=0  (Tiene tendencia lineal)
#p valor de 0.000104, rechazamos H0
#Tiene tendencia lineal
##No es estacionaria en media

#Modelo cuadrado
t<-seq(2003.5,2020,by=1/12)
mod_cua<-lm(s~poly(t,2,raw=T))
summary(mod_cua)

#Modelo de potencia
z<-log(s)
w<-log(t)
mod_pot<-lm(z~w)
summary(mod_pot)

#¿Es estacionaria en varianza?
library(tseries)
adf.test(s)
#H0: No es estacionaria en varianza
#Ha: Es estacionaria en varianza
#p_valor de 0.3035
#No rechazamos H0
##No es estacionaria en varianza


#########NO ES ESTACIONARIA