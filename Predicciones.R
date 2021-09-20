#Predicciones
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




#Predicciones con modelos AR, MA y ARMA
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
