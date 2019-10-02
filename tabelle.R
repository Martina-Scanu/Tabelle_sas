library(readxl)
library(tidyverse)
##
###### select species and area
specie<- "MTS" #FAO alpha code
area<-"17" #GSA
par<-2 ## ampiezza bin
####
setwd("~/CNR/Stock Assessment/2019/Michela tabelle 19-22 luglio 2019/File_catture") ### file LFD
######################## LFD per quarter
C1 <- read_excel("C1.xlsx") ### C1 solo per trimestri
names(C1)<-str_replace(names(C1), " ", "_")
names(C1)[7]<-paste("alpha_code") ### verificare sia la colonna giusta
names(C1)[9]<-paste("classe_lun") 
C6 <- read_excel("c 06 temp.xlsx") ### file nsamp
names(C6)<-str_replace(names(C6), " ", "_")
names(C6)[13]<-paste("alpha_code")
names(C6)[2]<-paste("GSA")
names(C6)[1]<-paste("Anno")
names(C6)[7]<-paste("Trimestre")
######## filtri per area, specie ecc
dataset<-C1 %>% dplyr::filter(alpha_code ==specie) %>% dplyr::filter(GSA == area)
tab_samp<-C6  %>% dplyr::filter(alpha_code ==specie) %>% dplyr::filter(GSA == area)
##### calcolo n samp
tab_samp$Attrezzo<-ifelse(grepl("OTB", tab_samp$Codice_Metier), "Strascico", ifelse(grepl("TBB", tab_samp$Codice_Metier) , "Rapido", ifelse(grepl("GNS", tab_samp$Codice_Metier) , "Reti_posta",ifelse( grepl("FPO", tab_samp$Codice_Metier), "Nasse", ifelse(grepl("GTR", tab_samp$Codice_Metier) ,"Tremaglio", ifelse(grepl("PTM", tab_samp$Codice_Metier),"Volante","Other" ))))))
n_samp<-tab_samp %>% dplyr::distinct(Attrezzo, DATA, .keep_all=T)%>% dplyr::group_by(Attrezzo, Anno, Trimestre) %>%dplyr::count(Trimestre) %>% dplyr::filter(Attrezzo != "Volante")
### tabella per lfd
dataset<- dataset %>%dplyr::select(Anno, Trimestre, Numero_espanso, classe_lun, Attrezzo) %>% tidyr::drop_na()
bin<-as.data.frame(seq(min(dataset$classe_lun),max(dataset$classe_lun), by=par))
names(bin)<-paste("bin")
dataset$bin<-rep(0, nrow(dataset))
for (i in 1:nrow(dataset)) {  
  if(is.na(match(dataset$classe_lun[i], bin$bin))==F){
    dataset$bin[i]<-dataset$classe_lun[i]
  } else {
    dataset$bin[i]<-bin$bin[which(bin$bin %between% c(dataset$classe_lun[i]-(par), dataset$classe_lun[i])==T)]
  }
  # 2. sequence
  #output[[i]] <- median(df[[i]])      # 3. body
  #print("dataset")
}
tab_1<-dataset %>% dplyr::select(-classe_lun) %>% dplyr::group_by(Anno, Trimestre, Attrezzo, bin)%>%dplyr::summarize(num=sum(Numero_espanso))%>%dplyr::distinct(Anno, Trimestre, Attrezzo, bin, num)%>% tidyr::spread(., bin, num)%>% arrange(Attrezzo, Anno, Trimestre) 
tab_1[is.na(tab_1)] <- 0
tab_1$Attrezzo<-ifelse(grepl("Reti", tab_1$Attrezzo), "Reti_posta", tab_1$Attrezzo)
tab_LFD_quarter<-tab_1 %>% dplyr::inner_join(., n_samp, by=c("Anno", "Trimestre", "Attrezzo")) %>% dplyr::arrange(Attrezzo, Anno, Trimestre)
tab_LFD_quarter<-tab_LFD_quarter[, c(1:3, ncol(tab_LFD_quarter), 4:ncol(tab_LFD_quarter)-1)]  ### serve a mettere n samp al posto giusto, modificare in base alle dimensioni del df



####################### LFD annuale
C8 <- read_excel("C8 C.xls") ### C1 solo per trimestri
names(C8)<-str_replace(names(C8), " ", "_")
names(C8)[6]<-paste("alpha_code") ### verificare sia la colonna giusta
names(C8)[8]<-paste("classe_lun") 
# modifico n samp
n_samp_ann<-n_samp %>% dplyr::group_by(Attrezzo, Anno) %>% dplyr::mutate(n_sam = sum(n)) %>%dplyr::distinct(Attrezzo, Anno, n_sam)
dataset<-C8 %>% dplyr::filter(alpha_code ==specie) %>% dplyr::filter(GSA == area)
dataset$Attrezzo<-ifelse(grepl("OTB", dataset$Codice_Metier), "Strascico", ifelse(grepl("TBB", dataset$Codice_Metier) , "Rapido", ifelse(grepl("GNS", dataset$Codice_Metier) , "Reti_posta",ifelse( grepl("FPO", dataset$Codice_Metier), "Nasse", ifelse(grepl("GTR", dataset$Codice_Metier) ,"Tremaglio", ifelse(grepl("PTM", dataset$Codice_Metier),"Volante","Other" ))))))
dataset<- dataset %>%dplyr::select(Anno, Numero_espanso, classe_lun, Attrezzo) %>% tidyr::drop_na()
dataset$bin<-rep(0, nrow(dataset))
for (i in 1:nrow(dataset)) {  
  if(is.na(match(dataset$classe_lun[i], bin$bin))==F){
    dataset$bin[i]<-dataset$classe_lun[i]
  } else {
    dataset$bin[i]<-bin$bin[which(bin$bin %between% c(dataset$classe_lun[i]-(par), dataset$classe_lun[i])==T)]
  }
  # 2. sequence
  #output[[i]] <- median(df[[i]])      # 3. body
  #print("dataset")
}
dataset<-dataset %>% dplyr::select(-classe_lun) %>% dplyr::group_by(Anno, Attrezzo, bin)%>%dplyr::summarize(num=sum(Numero_espanso))%>%dplyr::distinct(Anno, Attrezzo, bin, num)%>% tidyr::spread(., bin, num)%>% arrange(Attrezzo, Anno) 
dataset[is.na(dataset)] <- 0
dataset$Attrezzo<-ifelse(grepl("Reti", dataset$Attrezzo), "Reti_posta", dataset$Attrezzo)
tab_LFD_year<-dataset %>% dplyr::inner_join(., n_samp_ann, by=c("Anno",  "Attrezzo")) %>% dplyr::arrange(Attrezzo, Anno)
tab_LFD_year<-tab_LFD_year[, c(1:2, ncol(tab_LFD_year), 3:ncol(tab_LFD_year)-1)] ### serve a mettere n samp al posto giusto, modificare in base alle dimensioni del df

###################### Landings
Landings <- read_excel("Landings.xlsx") ### C1 solo per trimestri
names(Landings)[1]<-paste("Anno") 
names(Landings)[4]<-paste("GSA")
names(Landings)[7]<-paste("alpha_code")
names(Landings)[6]<-paste("Codice_Metier")
dataset<-Landings %>% dplyr::filter(alpha_code ==specie) %>% dplyr::filter(GSA == area) %>%dplyr::select(-'anno t')
dataset$Attrezzo<-ifelse(grepl("OTB", dataset$Codice_Metier), "Strascico", ifelse(grepl("TBB", dataset$Codice_Metier) , "Rapido", ifelse(grepl("GNS", dataset$Codice_Metier) , "Reti_posta",ifelse( grepl("FPO", dataset$Codice_Metier), "Nasse", ifelse(grepl("GTR", dataset$Codice_Metier) ,"Tremaglio", ifelse(grepl("PTM", dataset$Codice_Metier),"Volante","Other" ))))))
dataset$Trimestre<-ifelse(dataset$MESE %in% c("1", "2", "3"), "1", ifelse(dataset$MESE %in% c("4", "5", "6"), "2", ifelse(dataset$MESE %in% c("7", "8", "9"),"3", "4")) )
Landing_year<-dataset %>% dplyr::group_by(Attrezzo, Anno) %>%dplyr::summarize(Lan=sum(WEIGHT)/1000) %>% dplyr::arrange(Attrezzo, Anno)
Landing_quarter<-dataset %>% dplyr::group_by(Attrezzo, Anno, Trimestre) %>%dplyr::summarize(Lan=sum(WEIGHT)/1000) %>% dplyr::arrange(Attrezzo, Anno, Trimestre)

##################### Campionamenti per area
C7 <- read_excel("c 7temp.xlsx") ### file nsamp
logbook <- read_excel("~/CNR/Stock Assessment/2019/Michela tabelle 19-22 luglio 2019/LO01_LOGBOOK.xlsx") %>% dplyr::select(LO01_ID, LO01_GSA, LO01_PORTO, LO01_ATTREZZO, LO01_DATA ) %>%dplyr::rename("DATA" ="LO01_DATA")
names(C7)[14]<-paste("alpha_code")
names(C7)[2]<-paste("GSA")
names(C7)[13]<-paste("n_land")
names(C7)[11]<-paste("classe_lun")
dataset<-C7%>% dplyr::filter(alpha_code ==specie) %>% dplyr::filter(GSA == area)
LFD_campbiol<-dataset%>% inner_join(., logbook, by=c("DATA", "LO01_ID"))%>% dplyr::select(anno,GSA,LO01_TIPO_OSSERVAZIONE, LO01_ID, LO01_TRIMESTRE, LO01_ATTREZZO.x, classe_lun, n_land, alpha_code, LO01_PORTO) %>% tidyr::drop_na()

###########
setwd("~/CNR/Stock Assessment/2019/csv_plot")
write.csv(tab_LFD_quarter, "LFD_quarter.csv")
write.csv(tab_LFD_year, "LFD_year.csv")
write.csv(Landing_year, "Landing_year.csv")
write.csv(Landing_quarter, "Landing_quarter.csv")
write.csv(LFD_campbiol, "LFD_campbiol.csv")

warnings()


