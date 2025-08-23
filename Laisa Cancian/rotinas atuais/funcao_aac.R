aac<-function(P, Area=c(50,100), Demanda=c(100,200), Reservatorio=c(500,1000), VolInicial=0, descarte=2, C=0.9)
{
  nn<-c()
  nn1<-c()
  nn2<-c()
  for(i in 1:length(Demanda))
  {
    for (j in 1:length(Area))
    {
      for (k in 1:length(Reservatorio))
      {
        nn<-rbind(nn,paste("D",Demanda[i],"A",Area[j],"R",Reservatorio[k]))
        nn1<-c(Demanda[i],Area[j],Reservatorio[k])
        nn2<-rbind(nn2,nn1)
      }
    }
  }
  nome<-nn
  
  ###################----------------------------------######################
  ###################         BALANCO HIDRICO          ######################
  ###################----------------------------------######################
  
  #INICIALIZACOES
  
  n<-length(P)
  P2<-rep(0,n)
  volP<-matrix(0,length(P2),length(Area))
 
  #CALCULO DA CHUVA APOS O DESCARTE 
  if (P[1]<=descarte){P2[1]<-0}else{P2[1]<-P[1]-descarte}
  
  for (i in 2:n)
  {
    if (P[i]<=descarte)
    {
      P2[i]<-0
    }else if (P2[i-1]>0)
    {
      P2[i]<-P[i]
    }else 
    {
      P2[i]<-P[i]-descarte
    }
  }
  
  #CALCULO DO VOLUME PRODUZIDO PELA CHUVA EM FUNCAO DA AREA DO TELHADO
  for(i in (1:length(Area)))
  {
    for (j in 1:length(P2))
    {
      volP[j,i]<-C*Area[i]*P2[j]
    }
  }
  
  #------------------------
  #INICIALIZACOES
  volRes<-volExt<-volCon<-volNA<-ResOcioso<-diasFalha<-Max_falha<-c()
  volReservado<-volExtravasado<-volConsumido<-volNAtendido<-diasfalha<-Res_ocioso<-rep(NA,length(P2))

  
  
  ############ --- INICIO DO BALANCO HIDRICO --- ############
  for (l in 1:length(Demanda))  #varia as demandas
  {
    for (z in 1:dim(volP)[2])#varia a area do telhado (volume produzido em funcao da area)
    {
      for(k in 1:length(Reservatorio)) #varia o tamanho do reservatorio
      {
        for (j in 1:length(P2)) #laço das entradas de precipitação
        {
          if (j==1) #no primeiro elemento, para considerar o volume de contorno do reservatorio
          {
            Q<-VolInicial+volP[j,z]-Demanda[l]
          }else #calcula o balanco 
          {
            Q<-volReservado[j-1]+volP[j,z]-Demanda[l]
            #print(c("calculou q",Q, volReservado[j-1,w],volP[j,i],Demanda[l]))
          }
          
          # print (Q)
          if (Q<0) #se balanco deu negativo, ficou com zero de volume
          {
            volReservado[j]<-0
            volExtravasado[j]<-0
            if (j==1) #calcula para o primeiro elemento o volume consumido
            {
              volConsumido[j]<-VolInicial+volP[j,z]
            }else #calcula para os demais elementos o volume consumido
            {
              volConsumido[j]<-volReservado[j-1]+volP[j,z]
            }
          }else if (Q>Reservatorio[k]) #se extravasou
          {
            volReservado[j]<-Reservatorio[k] #volume armazenado igual ao tamanho do reservatorio
            volExtravasado[j]<-Q-Reservatorio[k] #volume extravasado
            volConsumido[j]<-Demanda[l] #volume consumido igual a demanda
          }else #volume produzido >0 mas menor que o tamanho do reservatorio
          {
            volReservado[j]<-Q #volume armazenado igual ao produzido
            volExtravasado[j]<-0 #nao teve extravasamento
            volConsumido[j]<-Demanda[l] #volume consumido igual a demandada
          }
          Res_ocioso[j]<-(Reservatorio[k]-volReservado[j])/Reservatorio[k]*100 #volume ocioso no reservatorio em % por dia
          volNAtendido[j]<-volConsumido[j]-Demanda[l] #volume não atendido
          if(volNAtendido[j]<0)#contador para dias não antendido #CONTADOR FALHAS NÃO CONSECUTIVAS.
          {
            if (j==1)
            {
              diasfalha[j]<-1 
            }else
            {diasfalha[j]<-diasfalha[j-1]+1}
          }else{diasfalha[j]<-0}
        }
        Max_falha<-rbind(Max_falha,max(diasfalha))
        #saidas
        volRes<-cbind(volRes,volReservado)
        volExt<-cbind(volExt,volExtravasado)
        volCon<-cbind(volCon,volConsumido)
        volNA<-cbind(volNA,volNAtendido)
        ResOcioso<-cbind(ResOcioso,Res_ocioso)
        diasFalha<-cbind(diasFalha,diasfalha)
        volReservado<-rep(NA,length(P2))
        volExtravasado<-rep(NA,length(P2))
        volConsumido<-rep(NA,length(P2))
        Res_ocioso<-rep(NA,length(P2))
        volNAtendido<-rep(NA,length(P2))
        diasfalha<-rep(NA,length(P2))
      }
    }
  }
 ############ --- FIM DO BALANCO HIDRICO --- ############
  
   
 ###################----------------------------------######################
 ###################       CALCULO DAS MEDIDAS        ######################
 ###################----------------------------------######################
  
  #INFORMACOES AUXILIARES
  n_ext<-A2<-nd_cons<-rep(NA,dim(volExt)[2])
  
  
  for(e in 1:dim(volExt)[2])
  {
    n_ext[e]<-length(volExt[volExt[ ,e]>0,e])
    A2[e]<-length(volNA[volNA[ ,e]<0,e])
    nd_cons[e]<-length(volCon[volCon[ ,e]>0,e])
  }
  
  demanda2<-rep(Demanda[1]*length(P2),length(Reservatorio)*length(Area)) #vetor com demandas totais
  for(i in 2:length(Demanda))
  {
    demanda2 <- cbind(demanda2, rep(Demanda[i]*length(P2),length(Reservatorio)*length(Area)))
  }
  
  V_acu<-colSums(volP)
  Vol_PrecipitadoAcu<-rep(V_acu[1],length(Reservatorio))
  for(i in 2:length(V_acu))
  {
    Vol_PrecipitadoAcu <- cbind(Vol_PrecipitadoAcu, rep(V_acu[i],length(Reservatorio)))
  }
  V_acu2<-rep(Vol_PrecipitadoAcu, length(Demanda)) 
  V_cons<-colSums(volCon)
  v_na<-colSums(volNA)
  
  ############ --- CALCULO DAS MEDIDAS --- ############
  Falhas<-A2*100/length(P2)  # % de Falha no atendimento a demanda total
  E=100-Falhas  #Eficiência do Sistema no atendimento a demanda total (%)
  P_DE<-n_ext*100/length(P2) # % de dias com extravasamento 
  V_ext_acu<-colSums(volExt) # % de volume disponível extravasado
  P_VDE<-V_ext_acu*100/V_acu2 # % de volume demandado extravasado
  Pot_apr<-V_cons*100/demanda2    #Potencial de aproveitamento
  P_VNA<-(-1)*v_na*100/demanda2 #Percentual de volume nao atendido
  Ef<-nd_cons/length(P2)*100 #Eficiencia geral de atendimento
  
  ############ --- CALCULO DOS INDICES DE OCIOSIDADE POR FAIXA --- ############

  #Dias com o reservatório ocioso
  diasOscioso25<- diasOscioso50<- diasOscioso75<-diasOscioso100<-rep(NA,dim(ResOcioso)[2])
  
  for(h in 1:dim(ResOcioso)[2])
  {
    diasOscioso25[h]<-length(ResOcioso[ResOcioso[ ,h]<=25,h])
    diasOscioso50[h]<-length(ResOcioso[ResOcioso[ ,h]>25 & ResOcioso[ ,h]<=50,h])
    diasOscioso75[h]<-length(ResOcioso[ResOcioso[ ,h]>50 & ResOcioso[ ,h]<=75,h])
    diasOscioso100[h]<-length(ResOcioso[ResOcioso[ ,h]>=75,h])
  }
  
  Index_Ocioso25<-diasOscioso25/length(P2)*100
  Index_Ocioso50<-diasOscioso50/length(P2)*100
  Index_Ocioso75<-diasOscioso75/length(P2)*100
  Index_Ocioso100<-diasOscioso100/length(P2)*100
  
  ###################----------------------------------######################
  ###################           ORGANIZACAO            ######################
  ###################----------------------------------######################
  
  #INSERINDO NOMES NAS MATRIZES E VETORES
  #nomes<-nome[1:length(nome)]
  names(Falhas)<-nome
  names(E)<-nome
  names(P_DE)<-nome
  names(P_VDE)<-nome
  names(Pot_apr)<-nome
  names(P_VNA)<-nome
  names(Ef)<-nome
  
  colnames(volRes)<-nome
  colnames(volCon)<-nome
  colnames(volExt)<-nome
  colnames(volNA)<-nome
  colnames(diasFalha)<-nome
  rownames(Max_falha)<-nome
  colnames(ResOcioso)<-nome
  
  z<-c()
  z$Falhas<-Falhas
  z$EficienciaAtendimento<-E
  z$DiasExtravasou<-P_DE
  z$VolDispoExtravasado<-P_VDE
  z$PotAprov<-Pot_apr
  z$VolNaoAtendido<-P_VNA
  z$EficienciaGlobal<-Ef
  # z$VolReservatorio<-volRes
  # z$VolConsumido<-volCon
  # z$VolExtravasado<-volExt
  # z$VolNaoAtendido<-volNA
  # z$DiasFalha<-diasFalha
  # z$MaxFalha<-Max_falha
  # z$ReservatorioOcioso<-ResOcioso
  
  compila<-c(P_DE,P_VDE,Pot_apr,P_VNA,Ef,E,Index_Ocioso25, Index_Ocioso50, Index_Ocioso75, Index_Ocioso100)
  compila<-matrix(compila, nrow=length(P_DE))
  colnames(compila)<-c("%DiasExt", "%volDispExtr","PotApr","%VolNaoAten", 
                       "EfGlobal","EfAtenDem","%25","%50","%75","%100")
  rownames(compila)<-nome
  z$matriz<-compila
  
  compila2<-cbind(nn2,Ef)
  colnames(compila2)<-c("Demanda", "Area","Volume","EficienciaGlobal")
  rownames(compila2)<-seq(1:dim(compila2)[1])
  z$matrizEf<-compila2
  
return(z)
}