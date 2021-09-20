buscar_arma<-function(serie,inicio,fin){
  n<-fin
  a<-Inf
  o<-c(0,0,0)
  for(i in inicio:n){
    for(j in inicio:n){
      aic.x<-try(AIC(arima(serie,order = c(j,0,i),method = "ML")))
      if(a>aic.x){
        a<-aic.x
        o<-c(j,0,i)
      }
    }
  }
  return(c(o,a))
}