#####################################################################################
## DESCRIPCIÓN:                                                                    ##
## Función de cálculo de la tasa incremental y odds ratio para el modelo logístico ##
#####################################################################################

tasa.incremental<-function(modelo,x1,x2,decimales=2,return=FALSE){
  incre <- abs(x1-x2)
  P1 <- data.frame(a=x1); names(P1) <- names(modelo$coeff[2])
  P2 <- data.frame(a=x2); names(P2) <- names(modelo$coeff[2])

  P1.pred <- predict(modelo,P1,type="response") 
  P2.pred <- predict(modelo,P2,type="response")

  # Tasa incremental de cambio
  tasa <- incre*modelo$coef[2]*P1.pred*(1-P1.pred)

  # Calculo de los odds ratio
  P1.odds <- P1.pred/(1-P1.pred)
  P2.odds <- P2.pred/(1-P2.pred)
  OR <- P2.odds/P1.odds
 
  # Salidas
  cat("Argumentos de entrada:\n")
  cat(sprintf("  x1=%g; x2=%g; incremento=%g",x1,x2,incre),"\n\n")
  cat("Probabilidades estimadas:\n")
  cat(sprintf("  pi.x1=%g; pi.x2=%g; tasa incremental=%g",round(P1.pred,decimales),round(P2.pred,decimales),round(tasa,decimales)),"\n\n")
  cat("Odds estimados:\n")
  cat(sprintf(" odds.x1=%g; odds.x2=%g; OR(odds.x2/odds.x1)=%g",round(P1.odds,decimales),round(P2.odds,decimales),round(OR,decimales)),"\n\n")
  
  resultado <- c("pi.x1"=round(P1.pred,decimales), "pi.x2"=round(P2.pred,decimales), "tasa.inc"=round(tasa,decimales),
                 "odds.x1"=round(P1.odds,decimales), "odds.x2"=round(P2.odds,decimales), "tasa.inc.odds"=round(OR,decimales))
  
  if(return)  return(resultado)
}