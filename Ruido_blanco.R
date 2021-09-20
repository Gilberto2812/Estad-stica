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
t_out<-time(r)[(r^2)^.5>3*sd(r)]
gr<-ggplot()+
  geom_point(aes(time(r),r,col="Observaciones"))+
  geom_point(aes(t_out,r_out,col="Datos atipicos"))+
  geom_hline(aes(yintercept=0,col="Media"))+
  geom_hline(aes(yintercept=3*sd(r),col="3 D.E."),lty=2)+
  geom_hline(aes(yintercept=-3*sd(r),col="3 D.E."),lty=2)+
  guides(color = guide_legend(
    override.aes = list(shape = c(NA, NA,16,16), 
                        linetype = c(2, 1,NA,NA))))+
  labs(col="Simbología",x="Fecha",y="Error",title="Residuales")+
  scale_color_manual(values = c("darkblue","darkblue","blue","cadetblue3"))
pgr<-ggplotly(gr)
#Como cumple con todos los supuestos incluyendo normalidad
#decimos que es ruido blanco gaussiano

#Por lo tanto podemos hacer predicciones