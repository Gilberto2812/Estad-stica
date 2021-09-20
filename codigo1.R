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


#H0: No es estacionaria en varianza
#Ha: Es estacionaria en varianza
#p_valor de 0.3035
#No rechazamos H0
##No es estacionaria en varianza

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



#H0: No es estacionaria en varianza
#Ha: Es estacionaria en varianza
#p_valor menor a .01
##Es estacionaria en varianza

###Es estacionaria


#Buscando el mejor modelo
# source("funciones.R")
# buscar_arma(c1,0,20)
# #ARMA(12,12)   AIC  -276.4801
# rm(buscar_arma)
# 
# #Mejor AR
# n<-20
# a<-Inf
# o<-c(0,0,0)
# for(j in 1:n){
#   aic.x<-try(AIC(arima(c1,order = c(j,0,0),method = "ML")))
#   if(a>aic.x){
#     a<-aic.x
#     o<-c(j,0,0)
#   }
# }
# #Mejor es AR(16) AIC -244.61
# 
# #Mejor MA
# n<-20
# a<-Inf
# o<-c(0,0,0)
# for(j in 1:n){
#   aic.x<-try(AIC(arima(c1,order = c(0,0,j),method = "ML")))
#   if(a>aic.x){
#     a<-aic.x
#     o<-c(0,0,j)
#   }
# }
#Mejor es MA(19) AIC -163.4603


#Probando supuestos de ruido blanco
modelo<-arima(c1,order=c(12,0,12),method = "ML")

r<-residuals(modelo)

#Normalidad
hist(r)
shapiro.test(r)
#p_valor de 0.2133
#Es normal

#Media 0  (Cómo se distribuye normal podemo hacer uso de una prueba t)
t.test(r)
#H0: media = 0
#Ha: media !=0
#p-value = 0.98
#No rechazamos H0
#Tiene media 0

#Verificaos varianza constante
plot(r,type="p")
abline(h=0,col="red")
abline(h=c(3*sd(r),-3*sd(r)),col="blue",lty=2)
#Se puede ver en la gráfica que los datos tienen una varianza constante
#ya que en ningún momento se empiezan a dispersar o a converger.

#Concluimos que tiene varianza constante


#Independencia
acf(r,main="Residuales")
pacf(r,main="Residuales")
#Las correlaciones se mantienen en los intervalos de confianza
#Por lo que concluimos que hay independencia

#Prueba analítica de independencia
Box.test(r)
#H0: Elementos independientes
#Ha: Elementos no independientes
#p-value = 0.9751
#Concluimos que los residules son independientes

r_out<-r[(r^2)^.5>3*sd(r)]
gr<-ggplot()+
  geom_point(aes(time(r),r,col="Observaciones"))+
  geom_hline(aes(yintercept=0,col="Media"))+
  geom_hline(aes(yintercept=3*sd(r),col="3 D.E."),lty=2)+
  geom_hline(aes(yintercept=-3*sd(r),col="3 D.E."),lty=2)+
  guides(color = guide_legend(
    override.aes = list(shape = c(NA, NA,16), 
                        linetype = c(2, 1,NA))))+
  labs(col="Simbología",x="Fecha",y="Error")+
  scale_color_manual(values = c("darkblue","blue","cadetblue3"))
pgr<-ggplotly(gr)
#Como cumple con todos los supuestos incluyendo normalidad
#decimos que es ruido blanco gaussiano

#Por lo tanto podemos hacer predicciones
modelo_orig<-arima(s,order=c(12,1,12),method = "ML")

n_pred<-22
pre<-predict(modelo_orig,n.ahead = n_pred)$pred
pre<-ts(data=c(s[199],pre), start = 2020, end = (2020+n_pred/12),
        frequency = 12)
t1<-seq(2003.5,(2020+n_pred/12),by=1/12)
ml<-function(x) mod$coefficients[1]+mod$coefficients[2]*x
mc<-function(x) mod_cua$coefficients[1]+
  mod_cua$coefficients[2]*x+mod_cua$coefficients[3]*x^2
mp<-function(x) exp(mod_pot$coefficients[1])*x^(mod_pot$coefficients[2])

library(ggplot2)
library(plotly)
grafica<-ggplot()+
  aes(time(s),s)+
  geom_line()+
  geom_line(aes(time(pre),pre,col="ARIMA(12,1,12)"))+
  geom_line(aes(time(s1),s1,col="Valor real"))+
  geom_line(aes(x=t1,y=ml(t1),col="Modelo lineal"))+
  geom_line(aes(x=t1,y=mc(t1),col="Modelo cuadrático"))+
  geom_line(aes(x=t1,y=mp(t1),col="Modelo de potencia"))+
  labs(col="Simbología",x="Tiempo en años",y="Monto en USD",
       title = "Gasto mensual Siniestros: Seguro Desempleo",
       size="Ecuaciones")+
  scale_color_manual(values=c("red","purple","green","cadetblue3","blue"))+
  geom_text(aes(2010,3.8e05),label="Y = 2803420-1349X",col="cadetblue3")+
  geom_text(aes(2010,3e05),label="Y=-2.1e+09+2.1e+06X-5.3e+2X^2",
            col="purple")+
  geom_text(aes(2010,2.2e05),label="Y=1.1e+116X^-33.64",col="green")
grafica
p<-ggplotly(grafica)
p

modelo1<-arima(s,order=c(16,1,0),method = "ML")
pre1<-predict(modelo1,n.ahead = n_pred)$pred
pre1<-ts(data=c(s[199],pre1), start = 2020, end = (2020+n_pred/12),
        frequency = 12)

modelo2<-arima(s,order=c(0,1,19),method = "ML")
pre2<-predict(modelo2,n.ahead = n_pred)$pred
pre2<-ts(data=c(s[199],pre2), start = 2020, end = (2020+n_pred/12),
        frequency = 12)


gts<-ggplotly(
  ggplot()+
    aes(time(s),s)+
    geom_line()+
    geom_line(aes(time(pre1),pre1,col="ARIMA(16,1,0)"))+
    geom_line(aes(time(pre2),pre2,col="ARIMA(0,1,19)"))+
    geom_line(aes(time(pre),pre,col="ARIMA(12,1,12)"))+
    labs(title = "Serie de tiempo: Gastos por siniestros",
         x="Tiempo en años",y="Gasto mensual en USD",
         col="Simbología")
)
