library(readr)
library(tidyr)
library(dplyr)
setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")
enligne_fichier_parametres_acoustiques_niveau_phone = "R_ENLIGNE/ANALYSES/analyse_v13_ENLIGNE2_InfosLoc.txt"
presentieL_fichier_parametres_acoustiques_niveau_phone = "R_MIC/ANALYSES/analyse_v13_tousphones_micro_InfosLoc.txt"
parametres_acoustiques_enligne = read_tsv(enligne_fichier_parametres_acoustiques_niveau_phone, na = c("", "NA", "None"))
parametres_acoustiques_presentiel = read_tsv(presentieL_fichier_parametres_acoustiques_niveau_phone, na = c("", "NA", "None"))

fichier_durees_enligne = "totalduration.csv"
dureeavecpauses_enligne = read_tsv(fichier_durees_enligne)
fichier_durees_presentiel = "totalduration_mic.csv"
dureeavecpauses_presentiel = read_tsv(fichier_durees_presentiel)
noms_textgrid_recodes = str_replace(dureeavecpauses_presentiel$textgrid_file, "_reduced", "")

dureeavecpauses_presentiel$textgrid_file = sapply(
  noms_textgrid_recodes,
  function(x) as.character(x[1])
)

categoriephone = read_tsv("categories_phones.txt")
parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(categoriephone,by=c("label"="segmentSAMPA"))
parametres_acoustiques_presentiel = parametres_acoustiques_presentiel %>% left_join(categoriephone,by=c("label"="segmentSAMPA"))
parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(dureeavecpauses_enligne,by=c("textgrid_file"="textgrid_file"))
parametres_acoustiques_presentiel = parametres_acoustiques_presentiel %>% left_join(dureeavecpauses_presentiel,by=c("textgrid_file"="textgrid_file"))
parametres_acoustiques_enligne$condition <- "enligne"
parametres_acoustiques_presentiel$condition <- "presentiel"

codecontroles = read_tsv("../paired_result_code2.txt")
parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(codecontroles)
parametres_acoustiques_enligne$age_locuteur[parametres_acoustiques_enligne$code_controle=="HER"]<-58
donneesenligne_codecontrole = parametres_acoustiques_enligne %>% 
  filter(code_controle != "NA")
parametres_acoustiques_presentiel = parametres_acoustiques_presentiel %>% left_join(codecontroles)
donneespresentiel_codecontrole = parametres_acoustiques_presentiel %>% 
  filter(code_controle != "NA")

names(donneesenligne_codecontrole) <- names(donneespresentiel_codecontrole)
donnees_codecontrole = rbind(donneesenligne_codecontrole, donneespresentiel_codecontrole)

parametres_acoustiques_presentiel = mutate(parametres_acoustiques_presentiel, label = case_when(
  label =="9~" ~ "e~",
  TRUE ~ label))#, 
#TRUE ~ condition))

parametres_acoustiques_enligne = mutate(parametres_acoustiques_enligne, label = case_when(
  label =="9~" ~ "e~",
  TRUE ~ label))


listenasales = c("a~", "e~", "o~")
listeocclusivessourdes = c("p", "t", "k")
listevoyelles = c("a","i")
listefricativesourde = c("s", "S", "f")
listenasales_a = c("a~", "e~", "o~", "a") 
listevoyelles2 = c( "u", "i", "y")
consonnesnasales = c("m", "n")
listevoyelles_eE2 = c("e","E","2", "a", "i", "o", "u", "y","O","o~", "a~")
listevoyelles_tout = c("e","E","2", "a", "i", "o", "u", "y","O","o~", "a~", "9", "9~")
listevoyelles_nonasale = c("e","E","2", "a", "i", "o", "u", "y","O", "9")

F0_donnees_codecontrole = donnees_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(F0moy = round(mean(`mean_F0(Hz)`, na.rm = T)))%>%
  spread(condition, F0moy)
F0_donnees_codecontrole

donneespresentiel_codecontrole
F0_donneespresentiel_codecontrole = donneespresentiel_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, age_locuteur, sexe_locuteur) %>%
  summarise(F0moy = mean(`mean_F0(Hz)`, na.rm = T))#%>%
 # spread(code, F0moy)
F0_donneespresentiel_codecontrole
F0_donneesenligne_codecontrole = donneesenligne_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, age_locuteur, sexe_locuteur) %>%
  summarise(F0moy = mean(mean_F0.Hz., na.rm = T))#%>%
# spread(code, F0moy)
F0_donneesenligne_codecontrole
cor(F0_donneesenligne_codecontrole$F0moy[F0_donneesenligne_codecontrole$sexe_locuteur=="F"], F0_donneesenligne_codecontrole$age_locuteur[F0_donneesenligne_codecontrole$sexe_locuteur=="F"])
cor(F0_donneesenligne_codecontrole$F0moy[F0_donneesenligne_codecontrole$sexe_locuteur=="M"], F0_donneesenligne_codecontrole$age_locuteur[F0_donneesenligne_codecontrole$sexe_locuteur=="M"])
cor(F0_donneespresentiel_codecontrole$F0moy[F0_donneespresentiel_codecontrole$sexe_locuteur=="M"], F0_donneespresentiel_codecontrole$age_locuteur[F0_donneespresentiel_codecontrole$sexe_locuteur=="M"])
cor(F0_donneespresentiel_codecontrole$F0moy[F0_donneespresentiel_codecontrole$sexe_locuteur=="F"], F0_donneespresentiel_codecontrole$age_locuteur[F0_donneespresentiel_codecontrole$sexe_locuteur=="F"])

cor(F0_donnees_codecontrole$presentiel, F0_donnees_codecontrole$enligne)
write.csv(F0_donnees_codecontrole, file = "R_enligne/CSV/F0_donneescode.csv")
# à faire plus tard cov(F0_donnees_codecontrole$age_locuteur[F0_donnees_codecontrole$sexe_locuteur=="F"], F0_donnees_codecontrole$enligne[F0_donnees_codecontrole$sexe_locuteur=="F"])

F1_donnees_codecontrole = donnees_codecontrole %>%
  filter(label %in% listevoyelles_nonasale) %>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(F1moybark = mean(bark(`mean_F1(Hz)`), na.rm = T))%>%
  spread(condition, F1moybark)
F1_donnees_codecontrole

F1_donneespresentiel_codecontrole = donneespresentiel_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, age_locuteur, sexe_locuteur) %>%
  summarise(F1moy = mean(`mean_F1(Hz)`, na.rm = T))#%>%

F1_donneesenligne_codecontrole = donneesenligne_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, age_locuteur, sexe_locuteur) %>%
  summarise(F1moy = mean(`mean_F1(Hz)`, na.rm = T))#%>%
spread(code, F0moy)
cor(F1_donneesenligne_codecontrole$F1moy[F1_donneesenligne_codecontrole$sexe_locuteur=="F"], F1_donneesenligne_codecontrole$age_locuteur[F1_donneesenligne_codecontrole$sexe_locuteur=="F"])
cor(F1_donneesenligne_codecontrole$F1moy[F1_donneesenligne_codecontrole$sexe_locuteur=="M"], F1_donneesenligne_codecontrole$age_locuteur[F1_donneesenligne_codecontrole$sexe_locuteur=="M"])
cor(F1_donneespresentiel_codecontrole$F1moy[F1_donneespresentiel_codecontrole$sexe_locuteur=="M"], F1_donneespresentiel_codecontrole$age_locuteur[F1_donneespresentiel_codecontrole$sexe_locuteur=="M"])
cor(F1_donneespresentiel_codecontrole$F1moy[F1_donneespresentiel_codecontrole$sexe_locuteur=="F"], F1_donneespresentiel_codecontrole$age_locuteur[F1_donneespresentiel_codecontrole$sexe_locuteur=="F"])


F2_donnees_codecontrole = donnees_codecontrole %>%
  filter(label %in% listevoyelles_nonasale) %>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(F2moybark =as.numeric(format(round(mean(bark(`mean_F2(Hz)`), na.rm = T), 2),nsmall =2)))%>%
  spread(condition, F2moybark)
F2_donnees_codecontrole
format(round(1.20, 2), nsmall = 2)

F3_donnees_codecontrole = donnees_codecontrole %>%
  filter(label %in% listevoyelles_nonasale) %>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(F3moybark = mean(bark(`mean_F3(Hz)`), na.rm = T))%>%
  spread(condition, F3moybark)
F3_donnees_codecontrole

totalduration_donnees_codecontrole = donnees_codecontrole %>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(durmoy = as.numeric(format(round(mean(`totalduration(s)`, na.rm = T), 2),nsmall =2)))%>%
  spread(condition, durmoy)
totalduration_donnees_codecontrole

myfxn <- function(var1,var2){
  (sqrt(var1)^2-sqrt(var2)^2)
}

calculvariabilite <- function(var1,var2){
  (as.numeric(format(round(((var1-var2)/var2),3), nsmall = 3)))*100
  #(valeur en ligne - valeur presentiel)/valeur presentiel
}

F0_donnees_codecontrole$taux_de_variabilite = mapply(
  calculvariabilite,
  F0_donnees_codecontrole$enligne,
  F0_donnees_codecontrole$presentiel)
write.csv(F0_donnees_codecontrole, file = "R_enligne/CSV/F0_donneescode.csv")
summary(sqrt((F0_donnees_codecontrole$difference)^2))

F1_donnees_codecontrole$difference = mapply(
  myfxn,
  F1_donnees_codecontrole$enligne,
  F1_donnees_codecontrole$presentiel)
write.csv(F1_donnees_codecontrole, file = "R_enligne/CSV/F1_donneescode.csv")
summary(sqrt((F1_donnees_codecontrole$difference)^2))

F2_donnees_codecontrole$difference = mapply(
  calculvariabilite,
  F2_donnees_codecontrole$enligne,
  F2_donnees_codecontrole$presentiel)
write.csv(F2_donnees_codecontrole, file = "R_enligne/CSV/F2_donneescode.csv")

summary(sqrt(F2_donnees_codecontrole$difference^2)) 
 
F3_donnees_codecontrole$difference = mapply(
  calculvariabilite,
  F3_donnees_codecontrole$enligne,
  F3_donnees_codecontrole$presentiel)
summary(sqrt(F3_donnees_codecontrole$difference^2))

totalduration_donnees_codecontrole$difference = mapply(
  calculvariabilite,
  totalduration_donnees_codecontrole$enligne,
  totalduration_donnees_codecontrole$presentiel)
write.csv(totalduration_donnees_codecontrole, file = "R_enligne/CSV/totalduration_donneescode.csv")
