# Define o diretório de trabalho para a pasta onde o script está localizado
# Isso garante que os caminhos relativos funcionem corretamente
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######### DADOS DE ENTRADA #########

estacoes<-read.table("../1-dados-brutos/estacoes.txt", sep="", header=FALSE) #leitura do arquivo

tabela2<-c()

for(i in 1:dim(estacoes)[1])
{
  est1<-estacoes[i,1]
  # Caminho corrigido para encontrar os dados de chuva
  est2<-paste("../1-dados-brutos/dados-pluviometricos/", est1, ".txt", sep="")
  est2<-gsub(" ","",est2)

  #Serie de precipitação
  dados<-read.table(est2, sep="", header=FALSE) #leitura do arquivo
  P<-dados[,4]  #coluna com dados de precipitacao

  ###--PARA O NOSSO EXPERIMENTO
  reservatorio<-c(500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000,30000,40000,50000)
  demanda<-c(100,200,300,400,500,600,700,800,900,1000)
  area<-c(50,100,150,200,300,400,500,1000)
  #os demais serao os valores padrao da funcao


  ######### FUNCAO APROVEITAMENTO DE AGUA DA CHUVA #########
  source("funcao_aac.R")

  fit<-aac(P, Reservatorio = reservatorio, Demanda = demanda, Area = area)



  ######### ANALISE DAS METRICAS ###########

  teste1<-rep(est1,dim(fit$matrizEf)[1])
  tabela<-cbind(teste1,fit$matrizEf)
  tabela2<-rbind(tabela2,tabela)

}

# Caminho corrigido para salvar o resultado final
write.table(tabela2, file="../4-dados-resultados/Resultado.csv", sep = ";", row.names = FALSE, col.names = TRUE)