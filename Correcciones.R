#Hacemos correcciones
c1<-diff(log(s))
plot(c1,main="Serie de tiempo de la primer corrección",
     xlab="Tiempo en años",ylab="Valor de la corrección")
#Hacemos pruebas de estacionariedad
#¿Es estacionaria en media?
mod1<-lm(c1~time(c1))
summary(mod1)
rm(mod1)
#H0: b1=0   (No tiene tendencia lineal)
#Ha: b1!=0  (Tiene tendencia lineal)
#p valor de 0.9818, no rechazamos H0
#No tiene tendencia lineal
#Por la forma de la gráfica no parece tener algún
#otro tipo de tendencia
##Es estacionaria en media


#¿Es estacionaria en varianza?
adf.test(c1)
#H0: No es estacionaria en varianza
#Ha: Es estacionaria en varianza
#p_valor menor a .01
##Es estacionaria en varianza

#################################ES ESTACIONARIA