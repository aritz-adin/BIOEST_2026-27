odds<-function(a,b=1,conf.level=0.95) {

if(all(rownames(a)==c("1","2"))){
  titulo1 <- "fila 1 vs fila 2"
  titulo1 <- "fila 2 vs fila 1"
}else{
  titulo1 <- paste0(rownames(a),collapse=" vs ")
  titulo2 <- paste0(rev(rownames(a)),collapse=" vs ")
}
    
if (nrow(a)!=2) {print("Cuidado con la longitud")
                           break}
conf.level<-conf.level+(1-conf.level)/2
if (b==1){
odds<-(a[1,1]*a[2,2])/(a[1,2]*a[2,1])
relative.risk<-(a[1,1]/(a[1,1]+a[1,2]))/(a[2,1]/(a[2,1]+a[2,2]))
print(titulo1)
} else
{
odds<-(a[1,2]*a[2,1])/(a[1,1]*a[2,2])
relative.risk<-(a[2,1]/(a[2,1]+a[2,2]))/(a[1,1]/(a[1,1]+a[1,2]))
print(titulo2)
}
p1<-(a[1,1]/(a[1,1]+a[1,2]))
p2<-(a[2,1]/(a[2,1]+a[2,2]))
log.odds<-log(odds)
log.RR<-log(relative.risk)
se.log.odds<-sqrt(1/a[1,1]+1/a[2,2]+1/a[1,2]+1/a[2,1])
se.log.RR<-sqrt((1-p1)/(p1*(a[1,1]+a[1,2])) +(1-p2)/(p2*(a[2,1]+a[2,2])))
conf.int.i<-log.odds-qnorm(conf.level)*se.log.odds
conf.int.s<-log.odds+qnorm(conf.level)*se.log.odds
conf.int.iRR<-log.RR-qnorm(conf.level)*se.log.RR
conf.int.sRR<-log.RR+qnorm(conf.level)*se.log.RR
print(cbind(oddsratio=odds,conf.int.i.OR=exp(conf.int.i),conf.int.s.OR=exp(conf.int.s)))
print(cbind(relative.risk=relative.risk,conf.int.i.RR=exp(conf.int.iRR),conf.int.s.RR=exp(conf.int.sRR)))}

