# Funcao para Aproveitamento de Agua da Chuva (AAC) - VERSÃO CORRIGIDA
# Elaborada por Profa. Dra. Andrea Polegatto, UNIPAMPA
# Ultima atualizacao: 14/06/2022 (Corrigida para execução correta do loop)

aac<-function(P, Reservatorio=c(500, 1000), Demanda=c(100, 200), Area=c(50, 100), coef=0.9, Vd=2, Vi=0)
{
  n<-length(P)
  par<-expand.grid(Reservatorio,Demanda,Area)
  npar<-dim(par)[1]

  # Inicializa os vetores de métricas vazios
  EficienciaAtendimento<-vector("numeric", npar)
  EficienciaGeral<-vector("numeric", npar)
  VolAbastecido<-vector("numeric", npar)
  VolNaoAbastecido<-vector("numeric", npar)
  VolColetado<-vector("numeric", npar)
  VolDescartado<-vector("numeric", npar)
  VolExtravasado<-vector("numeric", npar)
  DiasSemChuva<-vector("numeric", npar)
  DiasComChuva<-vector("numeric", npar)
  DiasReservatorioCheio<-vector("numeric", npar)
  DiasReservatorioVazio<-vector("numeric", npar)
  EficienciaGlobal<-vector("numeric", npar)

  # Loop principal para calcular as métricas para cada cenário
  for (j in 1:npar)
  {
    k<-par[j,1]
    d<-par[j,2]
    A<-par[j,3]

    # Zera as variáveis do balanço hídrico para cada novo cenário
    Q<-c()
    V<-c()
    Y<-c()
    S<-c()

    V[1]<-Vi
    Y[1]<-0
    S[1]<-0
    Q[1]<-0

    for (i in 2:n)
    {
      if(P[i]>Vd) { Q[i] = coef*(P[i]-Vd)*A } else { Q[i] = 0 }
      if((V[i-1]+Q[i])>d) { Y[i] = d } else { Y[i] = V[i-1]+Q[i] }
      if ((V[i-1]+Q[i]-Y[i])>k)
      {
        V[i] = k
        S[i] = V[i-1]+Q[i]-Y[i]-k
      } else {
        V[i] = V[i-1]+Q[i]-Y[i]
        S[i] = 0
      }
    }

    # Preenche os vetores de métricas com os resultados do cenário 'j'
    EficienciaAtendimento[j]<- (sum(Y) / (n*d))*100
    EficienciaGeral[j]<- (sum(Y) / (sum(Q)))*100
    VolAbastecido[j]<-sum(Y)
    VolNaoAbastecido[j]<-(n*d)-sum(Y)
    VolColetado[j]<-sum(Q)
    VolDescartado[j]<-sum(coef*Vd*A)
    VolExtravasado[j]<-sum(S)
    DiasSemChuva[j]<-sum(P==0)
    DiasComChuva[j]<-sum(P>0)
    DiasReservatorioCheio[j]<-sum(V==k)
    DiasReservatorioVazio[j]<-sum(V==0)
    EficienciaGlobal[j]<-(sum(Y==d)/n)*100
  }

  # --- CORREÇÃO: Monta as tabelas APÓS o loop terminar ---
  matriz<-cbind(par,EficienciaAtendimento, EficienciaGeral, VolAbastecido, VolNaoAbastecido, VolColetado,VolDescartado,VolExtravasado,DiasSemChuva,DiasComChuva,DiasReservatorioCheio,DiasReservatorioVazio)
  matrizEf<-cbind(par,EficienciaGlobal)

  colnames(matriz)<-c("Volume","Demanda","Area", "EficienciaAtendimento", "EficienciaGeral", "VolAbastecido", "VolNaoAbastecido", "VolColetado","VolDescartado","VolExtravasado","DiasSemChuva","DiasComChuva","DiasReservatorioCheio","DiasReservatorioVazio")
  colnames(matrizEf)<-c("Volume_Reservatorio_L","Demanda_L_dia","Area_Captacao_m2","EficienciaGlobal")

  lista<-list(matriz=matriz, matrizEf=matrizEf, EficienciaGlobal=matrizEf[,4])
  return(lista)
}