library(dplyr)
library(ggplot2)
library(scales)

#### Pular comandos abaixo, se variáveis já estiverem ativas no ambiente de trabalho do R ####

estados_sim = readxl::read_xlsx(path = "C:/arquivos/Mestrado/Dissertacao/Desenvolvimento/Usina_Assu_SClara/Resultados/03_Cenarios/Cenarios.xlsx", sheet = "Sheet1")

setwd("C:/arquivos/Mestrado/Dissertacao/Desenvolvimento/Usina_Assu_SClara/Resultados/01_ArquivosModelagem")
dorig<-readRDS("Clusterizacao.RData")
centroides<- readRDS("Centroides.RData")
clusters<-readRDS("Clusters_dados.RData")
setwd("C:/arquivos/Mestrado/Dissertacao/Desenvolvimento/Usina_Assu_SClara/Resultados/04_Analises/")

#### Estatísticas ##############################################################

##### Média e DP históricos ####

#Atenção: definir capacidades instaladas diferentes de 1 apenas se os dados estiverem em fator de capacidade
cap_inst_eol<-1
cap_inst_sol<-1

dadoshist<-dorig[,2:5]
dadoshist[,3]<-dadoshist[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eólica
dadoshist[,4]<-dadoshist[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

##Por hora x mês 
estatis_hist_mes_hora<-group_by(dadoshist, Mes, Hora)%>%summarise(MediaGE_H=mean(GE),MediaGS_H=mean(GS),DPGE_H=sd(GE),DPGS_H=sd(GS))
write.xlsx(estatis_hist_mes_hora, "Est_hist_mes_hora_eol_sol.xlsx")

##Por mês 
estatis_hist_mes<-group_by(dadoshist, Mes)%>%summarise(MediaGE_H=mean(GE),MediaGS_H=mean(GS),DPGE_H=sd(GE),DPGS_H=sd(GS)) #Sem desconsiderar período noturno da solar
write.xlsx(estatis_hist_mes, "Est_hist_mes_eol_sol.xlsx")

solar_diurno_hist<-dadoshist[which(dadoshist$GS!=0),]
solar_diurno_hist<-group_by(solar_diurno_hist, Mes)%>%summarise(MediaGS_H=mean(GS),DPGS_H=sd(GS)) #Considerando apenas período diurno da solar
write.xlsx(solar_diurno_hist, "Est_hist_mes_diurno_sol.xlsx")

#### Média Horária histórica

media_horaria_hist_solar<-group_by(dadoshist, Hora)%>%summarise(MediaGE_HH=mean(GE), MediaGS_HH=mean(GS))
write.xlsx(media_horaria_hist_solar, "Media_Horaria_Histórica.xlsx")

#### Média e DP simulados ####

dadosim<-estados_sim[,c(2,4,6,7)]
dadosim[,3]<-dadosim[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eólica
dadosim[,4]<-dadosim[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

##Por hora x mês 
estatis_sim_mes_hora<-group_by(dadosim, Mes, Hora)%>%summarise(MediaGE_Sim=mean(GE),MediaGS_Sim=mean(GS),DPGE_Sim=sd(GE),DPGS_Sim=sd(GS))
write.xlsx(estatis_sim_mes_hora, "Est_sim_mes_hora_eol_sol.xlsx")

##Por mês 
estatis_sim_mes<-group_by(dadosim, Mes)%>%summarise(MediaGE_Sim=mean(GE),MediaGS_Sim=mean(GS),DPGE_Sim=sd(GE),DPGS_Sim=sd(GS)) #Sem desconsiderar período noturno da solar
write.xlsx(estatis_sim_mes, "Est_sim_mes_eol_sol.xlsx")

solar_diurno_sim<-dadosim[which(dadosim$GS!=0),]
solar_diurno_sim<-group_by(solar_diurno_sim, Mes)%>%summarise(MediaGS_Sim=mean(GS),DPGS_Sim=sd(GS)) #Considerando apenas período diurno da solar
write.xlsx(solar_diurno_sim, "Est_sim_mes_diurno_sol.xlsx")

#### Média Horária simulada

media_horaria_sim_solar<-group_by(estados_sim, Hora)%>%summarise(MediaGE_SH=mean(GE), MediaGS_SH=mean(GS))
write.xlsx(media_horaria_sim_solar, "Media_Horaria_Simulada.xlsx")

##Diferenças %

desvio_mes_hora<-estatis_hist_mes_hora[,c(1,2)]
desvio_mes_hora$Dif_MediaGE<-''
desvio_mes_hora$Dif_DPGE<-''
desvio_mes_hora$Dif_MediaGS<-''
desvio_mes_hora$Dif_DPGS<-''

for (i in 1:288){
  desvio_mes_hora$Dif_MediaGE[i]=(abs(estatis_hist_mes_hora$MediaGE_H[i]-estatis_sim_mes_hora$MediaGE_Sim[i]))/estatis_hist_mes_hora$MediaGE_H[i]
  desvio_mes_hora$Dif_DPGE[i]=(abs(estatis_hist_mes_hora$DPGE_H[i]-estatis_sim_mes_hora$DPGE_Sim[i]))/estatis_hist_mes_hora$DPGE_H[i]
  desvio_mes_hora$Dif_MediaGS[i]=(abs(estatis_hist_mes_hora$MediaGS_H[i]-estatis_sim_mes_hora$MediaGS_Sim[i]))/estatis_hist_mes_hora$MediaGS_H[i]
  desvio_mes_hora$Dif_DPGS[i]=(abs(estatis_hist_mes_hora$DPGS_H[i]-estatis_sim_mes_hora$DPGS_Sim[i]))/estatis_hist_mes_hora$DPGS_H[i]
}

write.table(desvio_mes_hora, file='Dif%_mes_hora.csv', sep=';', dec=',')

desvio_mes_eol<-estatis_hist_mes[,1]
desvio_mes_eol$Dif_MediaGE<-''
desvio_mes_eol$Dif_DPGE<-''

for (i in 1:12){
  desvio_mes_eol$Dif_MediaGE[i]=(abs(estatis_hist_mes$MediaGE_H[i]-estatis_sim_mes$MediaGE_Sim[i]))/estatis_hist_mes$MediaGE_H[i]
  desvio_mes_eol$Dif_DPGE[i]=(abs(estatis_hist_mes$DPGE_H[i]-estatis_sim_mes$DPGE_Sim[i]))/estatis_hist_mes$DPGE_H[i]
}

write.table(desvio_mes_eol, file='Dif%_mes_eol.csv', sep=';', dec=',')

desvio_mes_sol<-estatis_hist_mes[,1]
desvio_mes_sol$Dif_MediaGS<-''
desvio_mes_sol$Dif_DPGS<-''

for (i in 1:12){
  desvio_mes_sol$Dif_MediaGS[i]=(abs(solar_diurno_hist$MediaGS_H[i]-solar_diurno_sim$MediaGS_Sim[i]))/solar_diurno_hist$MediaGS_H[i]
  desvio_mes_sol$Dif_DPGS[i]=(abs(solar_diurno_hist$DPGS_H[i]-solar_diurno_sim$DPGS_Sim[i]))/solar_diurno_hist$DPGS_H[i]
}

write.table(desvio_mes_sol, file='Dif%_mes_sol.csv', sep=';', dec=',')

#### Histogramas ##############################################################

NomeMes<-c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")

# Mudar índice da variável "Hora" para o horário desejado:

Hora<-12 #0 a 23

for (Mes in 1:12){
  clustersh<-clusters[[((Hora+1)+24*(Mes-1))]] #pegando o vetor da clusterização do histórico para o horário desejado
  
  numclustersh<-dorig[which(dorig$Mes==Mes & 
                            dorig$Hora==Hora),]
  numclustersh<-numclustersh$cluster
  numclustersh<-max(as.integer(numclustersh))
  
  
  freqhist<-hist(clustersh$cluster,probability = T, 
                 c(0:numclustersh))
  
  filtrohorasim<-estados_sim[which(estados_sim$Mes==Mes & 
                                     estados_sim$Hora==Hora),]
  freqsim<-hist(filtrohorasim$Estado,probability = T, 
                c(0:numclustersh))
  
  Hist_tab<-data.frame(c(rep("Histórico",numclustersh),
                         rep("Simulação",numclustersh)),
                       c(freqhist$density,
                         freqsim$density),
                       rep(c(1:numclustersh),2))
  colnames(Hist_tab)<-c("Origem", "Densidade", "Estados")
  
  ggplot(Hist_tab, aes(x= as.factor(Estados), y=Densidade, fill=Origem)) +
    geom_bar(stat='identity', position='dodge') + 
    ylab("Densidade") +
    xlab("Estados") +
    ggtitle(paste0(NomeMes[Mes]," - Hora ",Hora))+
    scale_fill_manual(values = c("#8997B7","#F5C574"))+  #5d1113
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          title =element_text(size=12),
          plot.title = element_text(hjust = 0.5))+
  # theme(legend.position="none") #Removendo a legenda inteira
    theme(legend.title=element_blank(),
          legend.position="bottom", #Legenda sem titulo e embaixo
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.5,'cm'))
  
  ggsave(paste0("Hist_",Mes,"_",Hora,".png"), width = 15, height = 8, units = "cm")  
}

#### FACs #####################################################################

NomeFonte=c("Eólica","Solar")
CorSim=c("#097F05","#DC9404")

for(Fonte in 4:5) { #obs: índice 4 para eólica, 5 para solar
  for (Mes in 1:12){
    
    dorig2<-dorig[which((dorig$Mes == Mes)),]  
    estadosim2<-estados_sim[which((estados_sim$Mes == Mes)),] 
    
    conf.level <- 0.95
    ciline <- qnorm((1 + conf.level)/2)/sqrt(nrow(dorig2))
    bacf <- acf(dorig2[,Fonte], lag=52, plot = FALSE) 
    bacfdf <- with(bacf, data.frame(lag, acf))
    
    bacf2 <- acf(estadosim2[,Fonte+2], lag=52, plot = FALSE)
    bacfdf2 <- with(bacf2, data.frame(lag, acf))
    
    bacfdf$teste<-"Histórico"
    bacfdf2$teste<-"Simulação"
    bacfdf<- rbind(bacfdf,bacfdf2)
    
    ggplot(data=bacfdf,aes(x=lag, y=acf, colour=teste, linetype=teste, size=teste))+
      geom_line()+
      scale_linetype_manual(values = c("solid", "dashed"))+
      scale_size_manual(values=c(1.2,1.2))+
      scale_color_manual(values = c("#645858", CorSim[Fonte-3]))+  
      ggtitle(paste("Geração ", NomeFonte[Fonte-3]," - ", NomeMes[Mes]))+
      scale_y_continuous(name = "FAC",
                         labels = label_number(accuracy = 0.01))+
      scale_x_continuous(name = "Lag (horas)")+
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(colour="black"),
            axis.text.y = element_text(colour="black"),
            title =element_text(size=12),
            plot.title = element_text(hjust = 0.5))+
      geom_hline(yintercept=c(ciline, -ciline), linetype="dashed")+
      theme(legend.position="none") #Removendo a legenda inteira
      #theme(legend.title = element_blank(),  #Para salvar com legenda, opção 1
      #      legend.position = c(0.89, 0.15), #0.89,0.85 para eólica e 0.89,0.15 para solar
      #      legend.key.size = unit(0.72,'cm'))+
      #theme(legend.title = element_blank(),  #Para salvar com legenda, opção 2
      #      legend.position = "bottom",
      #      legend.text = element_text(size = 18),
      #      legend.key.size = unit(2,'cm'))+
      #guides(linetype = guide_legend(override.aes = list(size = 1.1))) 
    
    ggsave(paste0("FAC_",Mes,"_",NomeFonte[Fonte-3],".png"), width = 15, height = 8, units = "cm")

 }
}

#### Gráfico nuvem médias + Média ano validação ########################################

#Lendo geração do ano de validação (alterar de acordo com usina)
anovalid = readxl::read_xlsx(path = "C:/arquivos/Mestrado/Dissertacao/Desenvolvimento/Usina_Assu_SClara/Dados/Dados2022.xlsx") 
anovalid[,4]<-anovalid[,4]*cap_inst_eol #Multiplicando pela capacidade instalada eólica
anovalid[,5]<-anovalid[,5]*cap_inst_sol #Multiplicando pela capacidade instalada solar

#Médias mensais do ano de validação
media_valid_eol<-group_by(anovalid, Mes)%>%summarise(MediaGE_V=mean(GE)) 
media_valid_sol<-anovalid[which(anovalid$GS!=0),]
media_valid_sol<-group_by(media_valid_sol, Mes)%>%summarise(MediaGS_V=mean(GS))

#Médias mensais simuladas
media_sim_eol<-estatis_sim_mes[,c(1,2)]
names(media_sim_eol)[names(media_sim_eol) == "MediaGE_Sim"] <- "MediaGE_S"
media_sim_sol<-solar_diurno_sim[,c(1,2)]
names(media_sim_sol)[names(media_sim_sol) == "MediaGS_Sim"] <- "MediaGS_S"

##Limites simulados
dadosim<-estados_sim[,c(1,2,6,7)]
dadosim[,3]<-dadosim[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eólica
dadosim[,4]<-dadosim[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

media_cen_sim_eol<-group_by(dadosim, Sim, Mes)%>%summarise(MediaGE_C=mean(GE))
media_cen_sim_sol<-dadosim[which(dadosim$GS!=0),]
media_cen_sim_sol<-group_by(media_cen_sim_sol, Sim, Mes)%>%summarise(MediaGS_C=mean(GS))

limites_eol<-data.frame(Mes=c(1:12))
limites_eol$Inferior<-''
limites_eol$Superior<-''
limites_sol<-data.frame(Mes=c(1:12))
limites_sol$Inferior<-''
limites_sol$Superior<-''

for (mes in 1:12){
  
  auxiliar_eol<-filter(media_cen_sim_eol[,c(2,3)],Mes==mes)
  limites_eol$Inferior[mes]<-quantile(auxiliar_eol$MediaGE_C,prob=0.05)
  limites_eol$Superior[mes]<-quantile(auxiliar_eol$MediaGE_C,prob=0.95)
  
  auxiliar_sol<-filter(media_cen_sim_sol[,c(2,3)],Mes==mes)
  limites_sol$Inferior[mes]<-quantile(auxiliar_sol$MediaGS_C,prob=0.05)
  limites_sol$Superior[mes]<-quantile(auxiliar_sol$MediaGS_C,prob=0.95)
  
}

timeline_table_eol = data.frame(Month=rep(1:12,5),
                            Series=c(rep("Média Validação",12),
                                     rep("Média Simulada",12),
                                     rep("Média Histórica",12),
                                     rep("Limite Inferior",12),  
                                     rep("Limite Superior",12)), 
                            Value = as.numeric(c(media_valid_eol$MediaGE_V,
                                                 media_sim_eol$MediaGE_S,
                                                 estatis_hist_mes$MediaGE_H,
                                                 limites_eol$Inferior, 
                                                 limites_eol$Superior)))

timeline_table_eol$Series<-factor(timeline_table_eol$Series,levels = c("Limite Superior","Média Validação","Média Histórica", "Média Simulada","Limite Inferior"))

timeline_table_sol = data.frame(Month=rep(1:12,5),
                                Series=c(rep("Média Validação",12),
                                         rep("Média Simulada",12),
                                         rep("Média Histórica",12),
                                         rep("Limite Inferior",12),
                                         rep("Limite Superior",12)),
                                Value = as.numeric(c(media_valid_sol$MediaGS_V,
                                                     media_sim_sol$MediaGS_S,
                                                     solar_diurno_hist$MediaGS_H,
                                                     limites_sol$Inferior, 
                                                     limites_sol$Superior)))

timeline_table_sol$Series<-factor(timeline_table_sol$Series,levels = c("Limite Superior","Média Validação","Média Histórica","Média Simulada","Limite Inferior"))

#### Nuvens de cenários das duas fontes

medias_eol<-media_cen_sim_eol
as.numeric(medias_eol$MediaGE_C)

medias_sol<-media_cen_sim_sol
as.numeric(medias_sol$MediaGS_C)

###### Gráfico final - sem legenda

## Eólica
names(medias_eol)[names(medias_eol) == "MediaGE_C"] <- "Value"
names(medias_eol)[names(medias_eol) == "Mes"] <- "Month"
names(medias_eol)[names(medias_eol) == "Sim"] <- "Series"
medias_eol<-medias_eol[,c(2,1,3)]
medias_eol$Series<-factor(medias_eol$Series)
timeline_table_eol_2 <- rbind(medias_eol,timeline_table_eol)

ggplot(data=timeline_table_eol_2, aes(x=Month ,y=Value, group=Series, color=Series, linetype=Series, size=Series))+
  geom_line()+
  scale_linetype_manual(values = c(rep("solid",200),"dashed", "solid", "solid", "solid", "dashed"))+
  scale_colour_manual(values = c(rep("grey",200), "#4E4E4E", "#4CA268", "#212020", "#2819AE", "#4E4E4E"))+
  scale_size_manual(values=c(rep(0.5,200),1.1,1.1,1.1,1.1,1.1),guide="none")+
  theme_classic()+
  scale_x_continuous(name = "Mês",
                     breaks=seq(1,12))+
  scale_y_continuous(name = "Geração (MWh)",
                     breaks = seq(20,120,20),
                     labels = label_number(accuracy = 0.01))+
  ggtitle("Fonte Eólica")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

ggsave("Eólica_SemLegenda.png", width = 20, height = 11, units = "cm")

## Solar
names(medias_sol)[names(medias_sol) == "MediaGS_C"] <- "Value"
names(medias_sol)[names(medias_sol) == "Mes"] <- "Month"
names(medias_sol)[names(medias_sol) == "Sim"] <- "Series"
medias_sol<-medias_sol[,c(2,1,3)]
medias_sol$Series<-factor(medias_sol$Series)
timeline_table_sol_2 <- rbind(medias_sol,timeline_table_sol)

ggplot(data=timeline_table_sol_2, aes(x=Month ,y=Value, group=Series, color=Series, linetype=Series, size=Series))+
  geom_line()+
  scale_linetype_manual(values = c(rep("solid",200),"dashed", "solid", "solid", "solid", "dashed"))+
  scale_colour_manual(values = c(rep("grey",200), "#4E4E4E", "#FFFF3B", "#212020", "#DC9404", "#4E4E4E"))+
  scale_size_manual(values=c(rep(0.5,200),1.1,1.1,1.1,1.1,1.1),guide="none")+
  theme_classic()+
  scale_x_continuous(name = "Mês",
                     breaks=seq(1,12))+
  scale_y_continuous(name = "Geração (MWh)",
                     #breaks = seq(1.5,2.25,0.25),
                     labels = label_number(accuracy = 0.01))+
  ggtitle("Fonte Solar")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

ggsave("Solar_SemLegenda.png", width = 20, height = 11, units = "cm")

########################### Média Horária do ano validação ################

media_horaria_valid_solar<-group_by(anovalid, Hora)%>%summarise(MediaGE_VH=mean(GE), MediaGS_VH=mean(GS))
write.xlsx(media_horaria_valid_solar, "Media_Horaria_Validação.xlsx")

########################## Contando número mínimo e máximo de clusters ########

numclusters<-""
as.vector(numclusters)
for (i in 1:200) {
  numclusters[i]<-nrow(as.data.frame(centroides[i][1]))
}
min(numclusters)
max(numclusters)

######################### Boxplot horário #############

## Ano validação 

eolica_assu<-anovalid
solar_assu<-anovalid

eolica_assu[,'Hora']<-lapply(eolica_assu[,'Hora'], factor)
solar_assu[,'Hora']<-lapply(solar_assu[,'Hora'], factor)

ggplot(eolica_assu,aes(Hora,GE))+
  geom_boxplot(color="black", fill="#173357")+ #173357
  scale_x_discrete(name = "Hora")+
  scale_y_continuous(name = "Geração (MWh)")+
  ggtitle("Usina Eólica Santa Clara - Ano Validação")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black",size = 11),
        axis.text.y = element_text(colour="black",size = 11),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5))

ggsave("SantaClara_Boxplot_Valid.png", width = 20, height = 11, units = "cm")

ggplot(solar_assu,aes(Hora,GS))+
  geom_boxplot(color="black", fill="#8E6902")+    #9E7400
  scale_x_discrete(name = "Hora")+
  scale_y_continuous(name = "Geração (MWh)")+
  ggtitle("Usina Solar Assú V - Ano Validação")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black",size = 11),
        axis.text.y = element_text(colour="black",size = 11),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5))

ggsave("Assu_Boxplot_Valid.png", width = 20, height = 11, units = "cm")

## Simulação

eolica_assu_sim<-estados_sim
solar_assu_sim<-estados_sim

eolica_assu_sim[,'Hora']<-lapply(eolica_assu_sim[,'Hora'], factor)
solar_assu_sim[,'Hora']<-lapply(solar_assu_sim[,'Hora'], factor)

ggplot(eolica_assu_sim,aes(Hora,GE))+
  geom_boxplot(color="black", fill="#173357")+ #173357
  scale_x_discrete(name = "Hora")+
  scale_y_continuous(name = "Geração (MWh)")+
  ggtitle("Usina Eólica Santa Clara - Simulação")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black",size = 11),
        axis.text.y = element_text(colour="black",size = 11),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5))

ggsave("SantaClara_Boxplot_Sim.png", width = 20, height = 11, units = "cm")

ggplot(solar_assu_sim,aes(Hora,GS))+
  geom_boxplot(color="black", fill="#8E6902")+    #9E7400
  scale_x_discrete(name = "Hora")+
  scale_y_continuous(name = "Geração (MWh)")+
  ggtitle("Usina Solar Assú V - Simulação")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black",size = 11),
        axis.text.y = element_text(colour="black",size = 11),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5))

ggsave("Assu_Boxplot_Sim.png", width = 20, height = 11, units = "cm")


