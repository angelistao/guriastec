######### DADOS DE ENTRADA ######### 

estacoes<-read.table("estacoes.txt", sep="", header=FALSE) #leitura do arquivo

tabela2<-c()

for(i in 1:dim(estacoes)[1])
{
  est1<-estacoes[i,1]
  est2<-paste(est1,".txt")
  est2<-gsub(" ","",est2)
  est3<-paste(est1,"_metricas.txt")
  est3<-gsub(" ","",est3)
  est4<-paste(est1,"_tabela.txt")
  est4<-gsub(" ","",est4)
  
  #Serie de precipitação
  
  dados<-read.table(est2, sep="", header=FALSE) #leitura do arquivo
  P<-dados[,4]  #coluna com dados de precipitacao
  
  #--- PADRAO DA FUNCAO
  #coeficiente de escoamento/runoff 0.9
  #volume descartado 2 mm
  #volume de agua inicial no reservatorio 0 L
  #volumes de demanda 100 e 200 L
  #areas de telhado 50 e 100 metros quadrados
  #tamanhos de reservatorios 500 e 1000 L
  
  ###--PARA O NOSSO EXPERIMENTO
  reservatorio<-c(500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000,30000,40000,50000)
  demanda<-c(100,200,300,400,500,600,700,800,900,1000)
  area<-c(50,100,150,200,300,400,500,1000)
  #os demais serao os valores padrao da funcao
  
  
  ######### FUNCAO APROVEITAMENTO DE AGUA DA CHUVA ######### 
  source("funcao_aac.R")
  
  fit<-aac(P, Reservatorio = reservatorio, Demanda = demanda, Area = area)
  
  
  
  ######### ANALISE DAS METRICAS ###########
  
#  write.table(fit$matriz,file=est3, sep = ";")
  
  teste1<-rep(est1,dim(fit$matrizEf)[1])
  tabela<-cbind(teste1,fit$matrizEf)
  tabela2<-rbind(tabela2,tabela)
  
  #library(GGally)
  #ggpairs(data.frame(metricas))
  
  
  ######### GERANDO GRAFICOS ######### 
  
  ###-- ORGANIZACAO DOS DADOS
  R<-reservatorio
  D<-demanda
  X<-fit$EficienciaGlobal #fit1$EficienciaAtendimento #informar qual parametro quer plotar
  par1<-"EficienciaGlobal" #nome para diferenciar o nome dos arquivos
  par2<-"Eficiência Global (%)" #nome eixo y
  
  ###-- GERANDO OS GRAFICOS
#  n<-length(R)
#  M<-matrix(X, ncol = length(D))
#  
#  for(h in 1:length(D))
#  {
#    ng<-paste(est1,par1,"D",D[h],".png")
    
#    d100a50  <- M[1:n,h]
#    d100a100 <- M[(n+1):(2*n),h]
#    d100a150 <- M[(2*n+1):(3*n),h]
#    d100a200 <- M[(3*n+1):(4*n),h]
#    d100a300 <- M[(4*n+1):(5*n),h]
#    d100a400 <- M[(5*n+1):(6*n),h]
#    d100a500 <- M[(6*n+1):(7*n),h]
    
#    png(file=ng, width = 680, height = 480)
#    plot(R, d100a50, ylab= par2, xlab="Volume do reservatório (L)", type="l", ylim = c(0,100))
#    lines(R, d100a100, lty=2)
#    lines(R, d100a150, lty=3)
#    lines(R, d100a200, lty=4)
#    lines(R, d100a300, lty=5)
#    lines(R, d100a400, lty=6)
#    lines(R, d100a500, lty=7)
#    points(R, d100a50, pch=1)
#    points(R, d100a100, pch=2)
#    points(R, d100a150, pch=3)
#    points(R, d100a200, pch=4)
#    points(R, d100a300, pch=5)
#    points(R, d100a400, pch=6)
#    points(R, d100a500, pch=7)
#    legend("bottomright",legend=c("A=50m²", "A=100m²","A=150m²","A=200m²","A=300m²","A=400m²","A=500m²"), lty=1:7, pch=1:7, cex=0.8, box.lty=0)
#    dev.off()
#  }
}






write.table(tabela2,file="Resultado.txt", sep = ";")

# #ideia, padronizar cada uma (-media/desvio padrao)
# #coloca negativo que quer maximizar e soma, qto menor melhor o resultado e pode ser o conjunto ideal
# 
# eee<-(-1)*fit1$EficienciaAtendimento
# eee2<-(-1)*fit1$EficienciaGeral
# 
# efic_padronizada<-(eee-min(eee))/(max(eee)-min(fit1$eee))
# de_padronizada<-(fit1$VolDemandadoExtravasado-min(fit1$VolDemandadoExtravasado))/(max(fit1$VolDemandadoExtravasado)-min(fit1$VolDemandadoExtravasado))
# EG_padronizada<-(eee2-min(eee2))/(max(eee2)-min(eee2))
# 
# soma<-100*(efic_padronizada+de_padronizada+EG_padronizada)/3
# 
# xx<-rep(R,length(D)*length(area))
# plot(xx, soma)
# 
# X<-data.frame(fit1[1:13])
