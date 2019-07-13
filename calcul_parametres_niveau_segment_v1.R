library(readr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(wesanderson)
library(emuR)

setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")
#boxplot(parametres_acoustiques_enligne$duration.s.~parametres_acoustiques_enligne$sexe_locuteur)
ENLIGNE_fichier_parametres_acoustiques_niveau_phone = "R_ENLIGNE/ANALYSES/analyse_v13_ENLIGNE1POINT_InfosLoc.txt"
ENLIGNE_fichier_parametres_acoustiques_niveau_phrase = "R_ENLIGNE/ANALYSES/analyse_result_nasales_parphrase_ENLIGNE.txt"
MICRO_fichier_parametres_acoustiques_niveau_phone = "R_MIC/ANALYSES/analyse_v13_tousphones_micro_InfosLoc.txt"
MICRO_fichier_parametres_acoustiques_niveau_phrase = "R_MIC/ANALYSES/analyse_result_nasales_parphrase_v1_MICRO.txt"
# lecture du fichier de resultats
parametres_acoustiques_enligne = read_tsv(ENLIGNE_fichier_parametres_acoustiques_niveau_phone, na = c("", "NA", "None"))
#parametres_acoustiques_enligne = read.delim(ENLIGNE_fichier_parametres_acoustiques_niveau_phone,sep="\t", header = TRUE)
parametres_acoustiques_micro = read_tsv(MICRO_fichier_parametres_acoustiques_niveau_phone, na = c("", "NA", "None"))
#parametres_acoustiques_micro = read.delim(MICRO_fichier_parametres_acoustiques_niveau_phone,sep="\t", header = TRUE)

problems(parametres_acoustiques_micro)
problems(parametres_acoustiques_enligne)

fichier_durees_enligne = "totalduration.csv"
dureeavecpauses_enligne = read_tsv(fichier_durees_enligne)

fichier_durees_micro = "totalduration_mic.csv"
dureeavecpauses_micro = read_tsv(fichier_durees_micro)
noms_textgrid_recodes = str_replace(dureeavecpauses_micro$textgrid_file, "_reduced", "")

dureeavecpauses_micro$textgrid_file = sapply(
  noms_textgrid_recodes,
  function(x) as.character(x[1])
)

# chargement de la definition des fonctions de regroupement
source("vector_grouping_functions.R")

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

categoriephone = read_tsv("categories_phones.txt")

parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(categoriephone,by=c("label"="segmentSAMPA"))
parametres_acoustiques_micro = parametres_acoustiques_micro %>% left_join(categoriephone,by=c("label"="segmentSAMPA"))

parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(dureeavecpauses_enligne,by=c("textgrid_file"="textgrid_file"))
parametres_acoustiques_micro = parametres_acoustiques_micro %>% left_join(dureeavecpauses_micro,by=c("textgrid_file"="textgrid_file"))

parametres_acoustiques_enligne$condition <- "ENLIGNE"
parametres_acoustiques_micro$condition <- "PRESENTIEL"

codecontroles = read_tsv("../paired_result_code2.txt")
parametres_acoustiques_enligne = parametres_acoustiques_enligne %>% left_join(codecontroles)
donneesenligne_codecontrole = parametres_acoustiques_enligne %>% 
  filter(code_controle != "NA")
parametres_acoustiques_micro = parametres_acoustiques_micro %>% left_join(codecontroles)
donneesmicro_codecontrole = parametres_acoustiques_micro %>% 
  filter(code_controle != "NA")

parametres_acoustiques_enligne$age_locuteur[parametres_acoustiques_enligne$code_controle=="HER"]<-58


donneesenligne_codecontrole$age_locuteur[donneesenligne_codecontrole$code_controle=="HER"]<-58

#names(donneesenligne_codecontrole) <- names(donneesmicro_codecontrole)
donnees_codecontrole = rbind(donneesenligne_codecontrole, donneesmicro_codecontrole)

parametres_acoustiques_micro = mutate(parametres_acoustiques_micro, label = case_when(
  label =="9~" ~ "e~",
  TRUE ~ label))#, 
  #TRUE ~ condition))

parametres_acoustiques_enligne = mutate(parametres_acoustiques_enligne, label = case_when(
  label =="9~" ~ "e~",
  TRUE ~ label))

help(spread)
head(parametres_acoustiques_micro$label[donnees_codecontrole$statutCV=="V"])
labs <- table(parametres_acoustiques_enligne$label[donnees_codecontrole$statutCV=="V"])
names(labs[labs==max(labs)])
sort.default(labs, decreasing = T)
table(donnees_codecontrole$code_controle)
codes = c("CLA","HER","MAM","MAN","MARINE","MOI","PAP","PAU","SYL","TAT","ULR","VERO","XAV") 

   

F0_donnees_codecontrole = donnees_codecontrole %>%
  filter(label %in% listevoyelles_nonasale)%>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(F0moy = round(mean(`mean_F0(Hz)`, na.rm = T)))%>%
  spread(condition, F0moy)
F0_donnees_codecontrole

# donneesmicro_codecontrole
# F0_donneesmicro_codecontrole = donneesmicro_codecontrole %>%
#   filter(label %in% listevoyelles_nonasale)%>%
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>%
#   summarise(F0moy = mean(`mean_F0(Hz)`, na.rm = T))#%>%
#  # spread(code, F0moy)
# F0_donneesmicro_codecontrole
# F0_donneesenligne_codecontrole = donneesenligne_codecontrole %>%
#   filter(label %in% listevoyelles_nonasale)%>%
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>%
#   summarise(F0moy = mean(mean_F0.Hz., na.rm = T))#%>%
# # spread(code, F0moy)
# F0_donneesenligne_codecontrole
# cor(F0_donneesenligne_codecontrole$F0moy[F0_donneesenligne_codecontrole$sexe_locuteur=="F"], F0_donneesenligne_codecontrole$age_locuteur[F0_donneesenligne_codecontrole$sexe_locuteur=="F"])
# cor(F0_donneesenligne_codecontrole$F0moy[F0_donneesenligne_codecontrole$sexe_locuteur=="M"], F0_donneesenligne_codecontrole$age_locuteur[F0_donneesenligne_codecontrole$sexe_locuteur=="M"])
# cor(F0_donneesmicro_codecontrole$F0moy[F0_donneesmicro_codecontrole$sexe_locuteur=="M"], F0_donneesmicro_codecontrole$age_locuteur[F0_donneesmicro_codecontrole$sexe_locuteur=="M"])
# cor(F0_donneesmicro_codecontrole$F0moy[F0_donneesmicro_codecontrole$sexe_locuteur=="F"], F0_donneesmicro_codecontrole$age_locuteur[F0_donneesmicro_codecontrole$sexe_locuteur=="F"])
# 
# cor(F0_donnees_codecontrole$MICRO, F0_donnees_codecontrole$ENLIGNE)
#write.csv(F0_donnees_codecontrole, file = "R_ENLIGNE/CSV/F0_donneescode.csv")
# # à faire plus tard cov(F0_donnees_codecontrole$age_locuteur[F0_donnees_codecontrole$sexe_locuteur=="F"], F0_donnees_codecontrole$ENLIGNE[F0_donnees_codecontrole$sexe_locuteur=="F"])
# 
# F1_donnees_codecontrole = donnees_codecontrole %>% 
#   filter(label %in% listevoyelles_nonasale) %>% 
#   group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>% 
#   summarise(F1moybark = mean(bark(`mean_F1(Hz)`), na.rm = T))%>% 
#   spread(condition, F1moybark)
# F1_donnees_codecontrole
# 
# F1_donneesmicro_codecontrole = donneesmicro_codecontrole %>% 
#   filter(label %in% listevoyelles_nonasale)%>% 
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>% 
#   summarise(F1moy = mean(`mean_F1(Hz)`, na.rm = T))#%>% 
# 
# F1_donneesenligne_codecontrole = donneesenligne_codecontrole %>% 
#   filter(label %in% listevoyelles_nonasale)%>% 
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>% 
#   summarise(F1moy = mean(`mean_F1(Hz)`, na.rm = T))#%>% 
# spread(code, F0moy)
# cor(F1_donneesenligne_codecontrole$F1moy[F1_donneesenligne_codecontrole$sexe_locuteur=="F"], F1_donneesenligne_codecontrole$age_locuteur[F1_donneesenligne_codecontrole$sexe_locuteur=="F"])
# cor(F1_donneesenligne_codecontrole$F1moy[F1_donneesenligne_codecontrole$sexe_locuteur=="M"], F1_donneesenligne_codecontrole$age_locuteur[F1_donneesenligne_codecontrole$sexe_locuteur=="M"])
# cor(F1_donneesmicro_codecontrole$F1moy[F1_donneesmicro_codecontrole$sexe_locuteur=="M"], F1_donneesmicro_codecontrole$age_locuteur[F1_donneesmicro_codecontrole$sexe_locuteur=="M"])
# cor(F1_donneesmicro_codecontrole$F1moy[F1_donneesmicro_codecontrole$sexe_locuteur=="F"], F1_donneesmicro_codecontrole$age_locuteur[F1_donneesmicro_codecontrole$sexe_locuteur=="F"])
# 
# 
# F2_donnees_codecontrole = donnees_codecontrole %>%
#   filter(label %in% listevoyelles_nonasale) %>%
#   group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
#   summarise(F2moybark =as.numeric(format(round(mean(bark(`mean_F2(Hz)`), na.rm = T), 2),nsmall =2)))%>%
#   spread(condition, F2moybark)
# F2_donnees_codecontrole
# format(round(1.20, 2), nsmall = 2)
# # F3_donnees_codecontrole = donnees_codecontrole %>% 
#   filter(label %in% listevoyelles_nonasale) %>% 
#   group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>% 
#   summarise(F3moybark = mean(bark(`mean_F3(Hz)`), na.rm = T))%>% 
#   spread(condition, F3moybark)
# F3_donnees_codecontrole
# 
totalduration_donnees_codecontrole = donnees_codecontrole %>%
  group_by(code_controle, condition, age_locuteur, sexe_locuteur) %>%
  summarise(durmoy = as.numeric(format(round(mean(`totalduration(s)`, na.rm = T), 2),nsmall =2)))%>%
  spread(condition, durmoy)
totalduration_donnees_codecontrole

# totalduration_micro_codecontrole = donneesmicro_codecontrole %>% 
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>% 
#   summarise(durmoy = mean(`totalduration(s)`, na.rm = T))#%>% 
#   #spread(condition, durmoy)
# totalduration_enligne_codecontrole = donneesenligne_codecontrole %>% 
#   group_by(code_controle, age_locuteur, sexe_locuteur) %>% 
#   summarise(durmoy = mean(`totalduration(s)`, na.rm = T))#%>% 
#   #spread(condition, durmoy)
# cor(totalduration_enligne_codecontrole$durmoy[totalduration_enligne_codecontrole$sexe_locuteur=="F"], totalduration_enligne_codecontrole$age_locuteur[totalduration_enligne_codecontrole$sexe_locuteur=="F"])
# cor(totalduration_enligne_codecontrole$durmoy[totalduration_enligne_codecontrole$sexe_locuteur=="M"], totalduration_enligne_codecontrole$age_locuteur[totalduration_enligne_codecontrole$sexe_locuteur=="M"])
# cor(totalduration_micro_codecontrole$durmoy[totalduration_micro_codecontrole$sexe_locuteur=="M"], totalduration_micro_codecontrole$age_locuteur[totalduration_micro_codecontrole$sexe_locuteur=="M"])
# cor(totalduration_micro_codecontrole$durmoy[totalduration_micro_codecontrole$sexe_locuteur=="F"], totalduration_micro_codecontrole$age_locuteur[totalduration_micro_codecontrole$sexe_locuteur=="F"])

myfxn1 <- function(var1,var2){
  var1-var2
}

myfxn <- function(var1,var2){
  
  sqrt((sqrt(var1)^2-sqrt(var2)^2)^2)
}

calculvariabilite <- function(var1,var2){
  #format(round((sqrt(((var1)^2)-sqrt((var2)^2))/(sqrt((var2)^2))),3), nsmall = 3)
  #sqrt(((var1)^2)-sqrt((var2)^2))/(sqrt((var2)^2))
  (as.numeric(format(round(((var1-var2)/var2),3), nsmall = 3)))*100
  
  #(valeur en ligne - valeur micro)/valeur micro
}

# F0_donnees_codecontrole$taux_de_variabilite = mapply(
#   calculvariabilite,
#   F0_donnees_codecontrole$ENLIGNE,
#   F0_donnees_codecontrole$MICRO)
# write.csv(F0_donnees_codecontrole, file = "R_ENLIGNE/CSV/F0_donneescode.csv")
# 
# summary(sqrt((F0_donnees_codecontrole$difference)^2))
# 
# 
# F1_donnees_codecontrole$difference = mapply(
#   myfxn,
#   F1_donnees_codecontrole$ENLIGNE,
#   F1_donnees_codecontrole$MICRO)
# write.csv(F1_donnees_codecontrole, file = "R_ENLIGNE/CSV/F1_donneescode.csv")
# 
# 
# summary(sqrt((F1_donnees_codecontrole$difference)^2)) 
# 
# F2_donnees_codecontrole$difference = mapply(
#   calculvariabilite,
#   F2_donnees_codecontrole$ENLIGNE,
#   F2_donnees_codecontrole$MICRO)
# write.csv(F2_donnees_codecontrole, file = "R_ENLIGNE/CSV/F2_donneescode.csv")
# # 
# summary(sqrt(F2_donnees_codecontrole$difference^2)) 
# 
# F3_donnees_codecontrole$difference = mapply(
#   calculvariabilite,
#   F3_donnees_codecontrole$ENLIGNE,
#   F3_donnees_codecontrole$MICRO)
# 
# summary(sqrt(F3_donnees_codecontrole$difference^2)) 
# 
totalduration_donnees_codecontrole$difference = mapply(
  calculvariabilite,
  totalduration_donnees_codecontrole$ENLIGNE,
  totalduration_donnees_codecontrole$MICRO)
 write.csv(totalduration_donnees_codecontrole, file = "R_ENLIGNE/CSV/totalduration_donneescode.csv")
# 
# summary(sqrt(totalduration_donnees_codecontrole$difference^2))
# #Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# #0.0009195 0.0236028 0.0596060 0.0940247 0.1265946 0.2782059
# boxplot(donnees_codecontrole$`mean_F0(Hz)`~donnees_codecontrole$age_locuteur)
# boxplot(F0_donnees_codecontrole$ENLIGNE~F0_donnees_codecontrole$age_locuteur)
# 
# #######
# defnition prealable : fonction pour obtenir au format large un ensemble de 
# adapte de https://community.rstudio.com/t/spread-with-multiple-value-columns/5378/5
spread_multiple <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, variable, !!keyq) %>%
    spread(temp, value)
}

#######

# regroupement par phrase + ajout de la duree du phrase et du temps au milieu de chaque phone (utilise par certains calculs ensuite)
#parametres_acoustiques = parametres_acoustiques %>% 
#  left_join()

parametres_acoustiques_enligne = parametres_acoustiques_enligne %>%
  mutate(
    tMilieuPhone = start_time + (end_time-start_time)/2
  ) %>%
  group_by(idPhrase, nom_locuteur, sexe_locuteur, age_locuteur, classe_age, label, condition, code_controle)

parametres_acoustiques_micro = parametres_acoustiques_micro %>%
  mutate(
    tMilieuPhone = start_time + (end_time-start_time)/2
  ) %>%
  group_by(idPhrase, nom_locuteur, sexe_locuteur, age_locuteur, classe_age, label, condition, code_controle)
#################
parametres_acoustiques_par_phrase_enligne = parametres_acoustiques_enligne %>%
  group_by(nom_locuteur,idPhrase, condition, age_locuteur, code_controle, sexe_locuteur, classe_age)%>%
  summarise(
    dureePhrase = sum(duration.s., na.rm = T),
    nPhones = n(),
    premierPhone = first_element_value(label),
    dernierPhone = last_element_value(label),
    dureePhonesMoy = mean(duration.s., na.rm = T),
    dureePhonesSD = sd(duration.s., na.rm = T),
    dureePhonesMediane = median(duration.s., na.rm = T),
    dureePhonesQ5 = quantile(duration.s., .05, na.rm = T),
    dureePhonesQ25 = quantile(duration.s., .25, na.rm = T),
    dureePhonesQ75 = quantile(duration.s., .75, na.rm = T),
    dureePhonesQ95 = quantile(duration.s., .95, na.rm = T),
    dureePhonesMin = min(duration.s., na.rm = T),
    dureePhonesMax = max(duration.s., na.rm = T),
    dureePhonesPositionRelativeMin = min_relative_position(duration.s.),
    dureePhonesPositionRelativeMax = max_relative_position(duration.s.),
    dureePremierPhone = first_element_value(duration.s.),
    dureeRelativePremierPhone = first_element_relative_value(duration.s.),
    dureeDernierPhone = last_element_value(duration.s.),
    dureeRelativeDernierPhone = last_element_relative_value(duration.s.),
    nPVIdureePhones = nPVI(duration.s.),
    penteDureePhones = linear_regression_slope(duration.s., tMilieuPhone),
    F0Moy = mean(mean_F0.Hz., na.rm = T),
    F0SD = sd(mean_F0.Hz., na.rm = T),
    ZCRMoy = mean(mean_ZCR, na.rm = T)
    
  )

parametres_acoustiques_par_phrase_micro = parametres_acoustiques_micro %>% 
  group_by(nom_locuteur,idPhrase, condition, age_locuteur, code_controle, sexe_locuteur, classe_age)%>%
  summarise(
    dureePhrase = sum(`duration(s)`, na.rm = T),
    nPhones = n(),
    premierPhone = first_element_value(label),
    dernierPhone = last_element_value(label),
    dureePhonesMoy = mean(`duration(s)`, na.rm = T),
    dureePhonesSD = sd(`duration(s)`, na.rm = T),
    dureePhonesMediane = median(`duration(s)`, na.rm = T),
    dureePhonesQ5 = quantile(`duration(s)`, .05, na.rm = T),
    dureePhonesQ25 = quantile(`duration(s)`, .25, na.rm = T),
    dureePhonesQ75 = quantile(`duration(s)`, .75, na.rm = T),
    dureePhonesQ95 = quantile(`duration(s)`, .95, na.rm = T),
    dureePhonesMin = min(`duration(s)`, na.rm = T),
    dureePhonesMax = max(`duration(s)`, na.rm = T),
    dureePhonesPositionRelativeMin = min_relative_position(`duration(s)`),
    dureePhonesPositionRelativeMax = max_relative_position(`duration(s)`),
    dureePremierPhone = first_element_value(`duration(s)`),
    dureeRelativePremierPhone = first_element_relative_value(`duration(s)`),
    dureeDernierPhone = last_element_value(`duration(s)`),
    dureeRelativeDernierPhone = last_element_relative_value(`duration(s)`),
    nPVIdureePhones = nPVI(`duration(s)`),
    penteDureePhones = linear_regression_slope(`duration(s)`, tMilieuPhone),
    F0Moy = mean(`mean_F0(Hz)`, na.rm = T),
    F0SD = sd(`mean_F0(Hz)`, na.rm = T),
    ZCRMoy = mean(`mean_ZCR`, na.rm = T)
  )

# decompte des phonemes (label) par phrase
decompte_phonemes_par_phrase_enligne = parametres_acoustiques_enligne %>%
  count(label) %>%  # on compte le nombre d'occurrences par sous-categorie pour chaque phrase
  filter(!is.na(label)) %>% # on retire les lignes eventuelles pour lesquels label n'est pas defini
  spread(label, n) %>% # on passe en format "large" avec une ligne par phrase et une colonne par categorie de phone
  rename_at(vars(-idPhrase, -nom_locuteur), ~ paste0("n_", .)) %>% # on ajoute le prefixe n_ aux noms de colonne correspondant a chaque modalite
  replace(is.na(.), 0) # comme il s'agit de comptage, on remplace les valeurs indefinies NA par 0

decompte_phonemes_par_phrase_micro = parametres_acoustiques_micro %>%
  count(label) %>%  # on compte le nombre d'occurrences par sous-categorie pour chaque phrase
  filter(!is.na(label)) %>% # on retire les lignes eventuelles pour lesquels label n'est pas defini
  spread(label, n) %>% # on passe en format "large" avec une ligne par phrase et une colonne par categorie de phone
  rename_at(vars(-idPhrase, -nom_locuteur), ~ paste0("n_", .)) %>% # on ajoute le prefixe n_ aux noms de colonne correspondant a chaque modalite
  replace(is.na(.), 0) # comme il s'agit de comptage, on remplace les valeurs indefinies NA par 0

# idem avec voyelles/consonnes (statutCV a la place de label)
decompte_CV_par_phrase_enligne = parametres_acoustiques_enligne %>%
  count(statutCV) %>% 
  filter(!is.na(statutCV)) %>% 
  spread(statutCV, n) %>% 
  rename_at(vars(-idPhrase, -nom_locuteur), ~ paste0("n_", .)) %>% 
  replace(is.na(.), 0)

decompte_CV_par_phrase_micro = parametres_acoustiques_micro %>%
  count(statutCV) %>% 
  filter(!is.na(statutCV)) %>% 
  spread(statutCV, n) %>% 
  rename_at(vars(-idPhrase, -nom_locuteur), ~ paste0("n_", .)) %>% 
  replace(is.na(.), 0)
#########

decompte_voyellesnasales_par_phrase_enligne = parametres_acoustiques_enligne %>% 
  filter(statutCV=="V" & nasalite=="nasale") %>% 
  group_by(nom_locuteur,age_locuteur, sexe_locuteur, classe_age, code_controle, classe_age) %>% 
  count(label) %>% 
  spread(label, n)

decompte_voyellesnasales_par_phrase_micro = parametres_acoustiques_micro %>% 
  filter(statutCV=="V" & nasalite=="nasale") %>% 
  group_by(nom_locuteur, age_locuteur, sexe_locuteur, classe_age, code_controle, classe_age) %>% 
  count(label) %>% 
  spread(label, n) 
#######

# uniquement sur les voyelles : meme principe mais on ajoute un filtrage initial
 parametres_acoustiques_voyelles_par_phrase_enligne = parametres_acoustiques_enligne %>%
   filter(statutCV=="V") %>%
  group_by(nom_locuteur,idPhrase, condition, age_locuteur, code_controle, sexe_locuteur, classe_age)%>%
  summarise(
    nVoyelles = n(),
    premiereVoyelle = first_element_value(label),
    derniereVoyelle = last_element_value(label),
    dureeVoyellesMoy = mean(duration.s., na.rm = T),
    dureeVoyellesSD = sd(duration.s., na.rm = T),
    dureeVoyellesMediane = median(duration.s.),
    dureeVoyellesQ5 = quantile(duration.s., .05, na.rm = T),
    dureeVoyellesQ25 = quantile(duration.s., .25, na.rm = T),
    dureeVoyellesQ75 = quantile(duration.s., .75, na.rm = T),
    dureeVoyellesQ95 = quantile(duration.s., .95, na.rm = T),
    dureeVoyellesMin = min(duration.s., na.rm = T),
    dureeVoyellesMax = max(duration.s., na.rm = T),
    dureeVoyellesPositionRelativeMin = min_relative_position(duration.s.),
    dureeVoyellesPositionRelativeMax = max_relative_position(duration.s.),
    dureePremiereVoyelle = first_element_value(duration.s.),
    dureeRelativePremiereVoyelle = first_element_relative_value(duration.s.),
    dureeDerniereVoyelle = last_element_value(duration.s.),
    dureeRelativeDerniereVoyelle = last_element_relative_value(duration.s.),
    F0VoyellesMoy = mean(mean_F0.Hz., na.rm = T),
    F0VoyellesSD = sd(mean_F0.Hz., na.rm = T),
    F0VoyellesMediane = median(mean_F0.Hz., na.rm = T),
    F0VoyellesQ5 = quantile(mean_F0.Hz., .05, na.rm = T),
    F0VoyellesQ25 = quantile(mean_F0.Hz., .25, na.rm = T),
    F0VoyellesQ75 = quantile(mean_F0.Hz., .75, na.rm = T),
    F0VoyellesQ95 = quantile(mean_F0.Hz., .95, na.rm = T),
    F0VoyellesIQ = interquartile_range(mean_F0.Hz.),
    F0VoyellesMin = min(mean_F0.Hz.),
    F0VoyellesMax = max(mean_F0.Hz.),
    F0VoyellesPositionRelativeMin = min_relative_position(mean_F0.Hz.),
    F0VoyellesPositionRelativeMax = max_relative_position(mean_F0.Hz.),
    nPVIdureeVoyelles = nPVI(duration.s.),
    penteDureeVoyelles = linear_regression_slope(duration.s., tMilieuPhone),
    F0DerniereVoyelle = last_element_value(mean_F0.Hz.),
    F0RelativeDerniereVoyelle = last_element_relative_value(mean_F0.Hz.),
    penteF0Voyelles = linear_regression_slope(mean_F0.Hz., tMilieuPhone),
    # quelques mesures globales sur les formants pour avoir une idee du centroide et de la taille de l'espace vocalique
    F1moy = mean(mean_F1.Hz., na.rm = T),
    F2moy = mean(mean_F2.Hz., na.rm = T),
    F3moy = mean(mean_F3.Hz., na.rm = T),
    F1min = min(mean_F1.Hz., na.rm = T),
    F2min = min(mean_F2.Hz., na.rm = T),
    F3min = min(mean_F3.Hz., na.rm = T),
    F1max = max(mean_F1.Hz., na.rm = T),
    F2max = max(mean_F2.Hz., na.rm = T),
    F3max = max(mean_F3.Hz., na.rm = T),
    F1SD = sd(mean_F1.Hz., na.rm = T),
    F1IQ = interquartile_range(mean_F1.Hz.),
    F2SD = sd(mean_F2.Hz., na.rm = T),
    F2IQ = interquartile_range(mean_F2.Hz.),
    F3SD = sd(mean_F3.Hz., na.rm = T),
    F3IQ = interquartile_range(mean_F3.Hz.)
  )

#warnings()
parametres_acoustiques_voyelles_par_phrase_micro =  parametres_acoustiques_micro %>%
  group_by(nom_locuteur,idPhrase, condition, age_locuteur, code_controle, sexe_locuteur, classe_age)%>%
  filter(statutCV=="V") %>%
  summarise(
    nVoyelles = n(),
    premiereVoyelle = first_element_value(label),
    derniereVoyelle = last_element_value(label),
    dureeVoyellesMoy = mean(`duration(s)`, na.rm = T),
    dureeVoyellesSD = sd(`duration(s)`, na.rm = T),
    dureeVoyellesMediane = median(`duration(s)`),
    dureeVoyellesQ5 = quantile(`duration(s)`, .05, na.rm = T),
    dureeVoyellesQ25 = quantile(`duration(s)`, .25, na.rm = T),
    dureeVoyellesQ75 = quantile(`duration(s)`, .75, na.rm = T),
    dureeVoyellesQ95 = quantile(`duration(s)`, .95, na.rm = T),
    dureeVoyellesMin = min(`duration(s)`, na.rm = T),
    dureeVoyellesMax = max(`duration(s)`, na.rm = T),
    dureeVoyellesPositionRelativeMin = min_relative_position(`duration(s)`),
    dureeVoyellesPositionRelativeMax = max_relative_position(`duration(s)`),
    dureePremiereVoyelle = first_element_value(`duration(s)`),
    dureeRelativePremiereVoyelle = first_element_relative_value(`duration(s)`),
    dureeDerniereVoyelle = last_element_value(`duration(s)`),
    dureeRelativeDerniereVoyelle = last_element_relative_value(`duration(s)`),
    F0VoyellesMoy = mean(`mean_F0(Hz)`, na.rm = T),
    F0VoyellesSD = sd(`mean_F0(Hz)`, na.rm = T),
    F0VoyellesMediane = median(`mean_F0(Hz)`, na.rm = T),
    F0VoyellesQ5 = quantile(`mean_F0(Hz)`, .05, na.rm = T),
    F0VoyellesQ25 = quantile(`mean_F0(Hz)`, .25, na.rm = T),
    F0VoyellesQ75 = quantile(`mean_F0(Hz)`, .75, na.rm = T),
    F0VoyellesQ95 = quantile(`mean_F0(Hz)`, .95, na.rm = T),
    F0VoyellesIQ = interquartile_range(`mean_F0(Hz)`),
    F0VoyellesMin = min(`mean_F0(Hz)`),
    F0VoyellesMax = max(`mean_F0(Hz)`),
    F0VoyellesPositionRelativeMin = min_relative_position(`mean_F0(Hz)`),
    F0VoyellesPositionRelativeMax = max_relative_position(`mean_F0(Hz)`),
    nPVIdureeVoyelles = nPVI(`duration(s)`),
    penteDureeVoyelles = linear_regression_slope(`duration(s)`, tMilieuPhone),
    F0DerniereVoyelle = last_element_value(`mean_F0(Hz)`),
    F0RelativeDerniereVoyelle = last_element_relative_value(`mean_F0(Hz)`),
    penteF0Voyelles = linear_regression_slope(`mean_F0(Hz)`, tMilieuPhone),
    # quelques mesures globales sur les formants pour avoir une idee du centroide et de la taille de l'espace vocalique
    F1moy = mean(`mean_F1(Hz)`, na.rm = T),
    F2moy = mean(`mean_F2(Hz)`, na.rm = T),
    F3moy = mean(`mean_F3(Hz)`, na.rm = T),
    F1min = min(`mean_F1(Hz)`, na.rm = T),
    F2min = min(`mean_F2(Hz)`, na.rm = T),
    F3min = min(`mean_F3(Hz)`, na.rm = T),
    F1max = max(`mean_F1(Hz)`, na.rm = T),
    F2max = max(`mean_F2(Hz)`, na.rm = T),
    F3max = max(`mean_F3(Hz)`, na.rm = T),
    F1SD = sd(`mean_F1(Hz)`, na.rm = T),
    F1IQ = interquartile_range(`mean_F1(Hz)`),
    F2SD = sd(`mean_F2(Hz)`, na.rm = T),
    F2IQ = interquartile_range(`mean_F2(Hz)`),
    F3SD = sd(`mean_F3(Hz)`, na.rm = T),
    F3IQ = interquartile_range(`mean_F3(Hz)`)
  )
#warnings()
# 
# 
# # calcul des formants : uniquement sur les voyelles orales, en separant les differentes voyelles
# parametres_acoustiques_voyelles_par_phraseXvoyelle_enligne = parametres_acoustiques_enligne %>%
#   filter(statutCV=="V" & nasalite=="orale") %>%
#   group_by(idPhrase, nom_locuteur, label) %>% 
#   summarise(
#     F1moy = mean(mean_F1.Hz., na.rm = T),
#     F1SDmoy = mean(std_dev_F1.Hz., na.rm = T),
#     F1SD = sd(mean_F1.Hz., na.rm = T),
#     F2moy = mean(mean_F2.Hz., na.rm = T),
#     F2SDmoy = mean(std_dev_F2.Hz., na.rm = T),
#     F2SD = sd(mean_F2.Hz., na.rm = T),
#     F3moy = mean(mean_F3.Hz., na.rm = T),
#     F3SDmoy = mean(std_dev_F3.Hz., na.rm = T),
#     F3SD = sd(mean_F3.Hz., na.rm = T)
#   ) %>%
#   spread_multiple(
#     label,
#     c(F1moy, F1SDmoy, F1SD, F2moy, F2SDmoy, F2SD, F3moy, F3SDmoy, F3SD)
#   )
# 
# parametres_acoustiques_voyelles_par_phraseXvoyelle_micro = parametres_acoustiques_micro %>%
#   filter(statutCV=="V" & nasalite=="orale") %>%
#   group_by(idPhrase, nom_locuteur, label) %>% 
#   summarise(
#     F1moy = mean(`mean_F1(Hz)`, na.rm = T),
#     F1SDmoy = mean(`std_dev_F1(Hz)`, na.rm = T),
#     F1SD = sd(`mean_F1(Hz)`, na.rm = T),
#     F2moy = mean(`mean_F2(Hz)`, na.rm = T),
#     F2SDmoy = mean(`std_dev_F2(Hz)`, na.rm = T),
#     F2SD = sd(`mean_F2(Hz)`, na.rm = T),
#     F3moy = mean(`mean_F3(Hz)`, na.rm = T),
#     F3SDmoy = mean(`std_dev_F3(Hz)`, na.rm = T),
#     F3SD = sd(`mean_F3(Hz)`, na.rm = T)
#   ) %>%
#   spread_multiple(
#     label,
#     c(F1moy, F1SDmoy, F1SD, F2moy, F2SDmoy, F2SD, F3moy, F3SDmoy, F3SD)
#   )

# on reunit le tout dans un meme tableau avec toujours une ligne par phrase et toutes les colonnes correspondant aux parametres calcules
parametres_acoustiques_par_phrase_all_enligne = parametres_acoustiques_par_phrase_enligne %>%  
  left_join(decompte_phonemes_par_phrase_enligne, by = c("idPhrase", "nom_locuteur")) %>% 
  left_join(decompte_CV_par_phrase_enligne, by = c("idPhrase", "nom_locuteur")) %>% 
  left_join(parametres_acoustiques_voyelles_par_phrase_enligne, by = c("idPhrase", "nom_locuteur", "condition","age_locuteur", "code_controle", "sexe_locuteur", "classe_age"))  
#  left_join(parametres_acoustiques_voyelles_par_phraseXvoyelle_enligne, by = c("idPhrase", "nom_locuteur"))
parametres_acoustiques_par_phrase_all_micro = parametres_acoustiques_par_phrase_micro %>% 
  left_join(decompte_phonemes_par_phrase_micro, by = c("idPhrase", "nom_locuteur")) %>% 
  left_join(decompte_CV_par_phrase_micro, by = c("idPhrase", "nom_locuteur")) %>% 
  left_join(parametres_acoustiques_voyelles_par_phrase_micro, by = c("idPhrase", "nom_locuteur", "condition","age_locuteur", "code_controle", "sexe_locuteur", "classe_age")) #%>% 
#  left_join(parametres_acoustiques_voyelles_par_phraseXvoyelle_micro, by = c("idPhrase", "nom_locuteur"))
#parametres_acoustiques_par_phrase_all_micro$code_controle

# on remplace par NA les valeurs infinies (valeur par defaut des fonctions min et max quand le vecteur ne contient que des NA) qui pourraient biaiser les analyses ensuite, ainsi que les NaN
# parametres_acoustiques_par_phrase_all_micro = parametres_acoustiques_par_phrase_all_micro %>%
#   mutate_at(vars(-group_cols()), list(~ifelse(!is.finite(.), NA, .)))
#parametres_acoustiques_par_phrase_all_micro$condition
# 
# 
# parametres_acoustiques_par_phrase_all_enligne = parametres_acoustiques_par_phrase_all_enligne %>%
#   mutate_at(vars(-group_cols()), list(~ifelse(!is.finite(.), NA, .)))
# parametres_acoustiques_par_phrase_all_enligne$label


# et on exporte le resultat final dans un fichier
write_tsv(parametres_acoustiques_par_phrase_all_micro, path = MICRO_fichier_parametres_acoustiques_niveau_phrase, na = "None")
write_tsv(parametres_acoustiques_par_phrase_all_enligne, path = ENLIGNE_fichier_parametres_acoustiques_niveau_phrase, na = "None")
#parametres_acoustiques_par_phrase_all_enligne$label
#############################################################
########### Ajout colonne de fusion des variantes ###########
# regroupement_micro = mutate(parametres_acoustiques_micro, phone = case_when(
#   label =="9~" ~ "e~",
#   TRUE ~ label, 
#   TRUE ~ condition))
# regroupement_micro$age_locuteur
# 
# regroupement_enligne = mutate(parametres_acoustiques_enligne, phone = case_when(
#   label =="9~" ~ "e~",
#   TRUE ~ label))
# regroupement_enligne$age_locuteur
# regroupe_tout <- rbind(regroupement_micro, regroupement_enligne)
# regroupe_tout$condition


#########################################################
################ COMPARAISON LOCUTEURS ###################
codes = c("CLA","HER","MAM","MAN","MARINE","MOI","PAP","PAU","SYL","TAT","ULR","VERO","XAV") 

# totalduration_comp = ggplot(donnees_codecontrole, 
#                                aes(x=code_controle, y=`totalduration(s)`, color=condition))+#, fill=condition))+
#   geom_boxplot()+
#   theme(legend.title=element_text(size=14), 
#         legend.text=element_text(size=12))+
#   #ggtitle("Duree du phone en fonction de l'age du locuteur \n Donnees enligne")+
#   xlab("Age du locuteur (ans)")+
#   ylab("Duree moyenne du phone (s)")+
#   scale_colour_manual(values=c("#5B1A18","#D67236"))
#   #scale_fill_manual(values=c("#5B1A18","#D67236"))
# 
# print(totalduration_comp)



#library(plogr)
###################################################################
##########  durée totale au cours du temps #################
F0_donnees_enligne = parametres_acoustiques_enligne %>% 
  filter(label %in% listevoyelles_nonasale)%>%
  group_by( age_locuteur, sexe_locuteur) %>%
  summarise(F0moy = mean(mean_F0.Hz., na.rm = T))#%>%
  #spread(condition, F0moy)
cor(F0_donnees_enligne$F0moy[F0_donnees_enligne$sexe_locuteur=="F"], F0_donnees_enligne$age_locuteur[F0_donnees_enligne$sexe_locuteur=="F"])
cor(F0_donnees_enligne$F0moy[F0_donnees_enligne$sexe_locuteur=="M"], F0_donnees_enligne$age_locuteur[F0_donnees_enligne$sexe_locuteur=="M"])


totalduration_enligne = ggplot(parametres_acoustiques_enligne, 
                          aes(x=age_locuteur, y=`totalduration(s)`, color=sexe_locuteur, fill=sexe_locuteur))+
  geom_smooth(method = "loess", formula = "y ~ x")+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  #ggtitle("Duree du phone en fonction de l'age du locuteur \n Donnees enligne")+
  xlab("Age du locuteur (ans)")+
  ylab("durée moyenne de la phrase (s)")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
ggsave(totalduration_enligne, filename = "R_ENLIGNE/IMAGES/totalduration_enligne.pdf", width = largeurFigCm, height = hauteurFigCm, units = 'cm')

print(totalduration_enligne)

library("ggsci")
totalduration_enligne_phrase = ggplot(parametres_acoustiques_enligne, 
                               aes(x=age_locuteur, y=`totalduration(s)`, color=idPhrase, fill=idPhrase))+
  geom_smooth(method = "lm", formula = "y ~ x")+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  facet_wrap(~sexe_locuteur)+
  #ggtitle("Duree du phone en fonction de l'age du locuteur \n Donnees enligne")+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne de la phrase (s)")+
  scale_color_manual(values = cbp1) +
  scale_fill_manual(values = cbp1) 

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#CC79A7", "#FFDB6D", "#C4961A", "#F4EDCA", 
          "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")





print(totalduration_enligne_phrase)
ggsave(totalduration_enligne_phrase, filename = "R_ENLIGNE/IMAGES/totalduration_enligne_phrase.pdf", width = largeurFigCm, height = hauteurFigCm, units = 'cm')

totalduration_enligne_sexe = ggplot(parametres_acoustiques_enligne, 
                               aes(x=age_locuteur, y=`totalduration(s)`, color=sexe_locuteur, fill=sexe_locuteur))+
  geom_smooth(method = "lm", formula = "y ~ x")+
  #geom_point(alpha = 0.2) +
  theme(legend.title=element_text(size=11), 
        legend.text=element_text(size=10),
        plot.title = element_text(size = 12))+
  ggtitle("Evolution de la duree de l'enonce \nen fonction de l'age du locuteur et du sexe\n Donnees en ligne")+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne de l'enonce (s)")+
  #scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"))+
  #scale_colour_manual(values=wes_palette(n=3, name="GrandBudapest1"))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
print(totalduration_enligne_sexe)
ggsave(totalduration_enligne_sexe, filename = "R_ENLIGNE/IMAGES/compare_totaldurations_sexe.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

totalduration_micro = ggplot(parametres_acoustiques_micro %>% 
                               filter(sexe_locuteur=="F"), 
                             aes(x=age_locuteur, y=`totalduration(s)`, color=sexe_locuteur, fill=sexe_locuteur))+
  geom_smooth(method = "lm", formula = "y ~ x")+#, color ="#5B1A18")+
  theme(legend.title=element_text(size=11), 
        legend.text=element_text(size=10),
        plot.title = element_text(size = 12))+
  ggtitle("Evolution de la duree de l'enonce \nen fonction de l'age de la locutrice \n Donnees micro")+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne de l'enonce (s)")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
#scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"))+
#scale_colour_manual(values=wes_palette(n=3, name="GrandBudapest1"))+

print(totalduration_micro)
ggsave(totalduration_micro, filename = "R_ENLIGNE/IMAGES/compare_totalduration_micro_femme.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


totalduration_micro_phrase = ggplot(parametres_acoustiques_micro %>% 
                               filter(sexe_locuteur=="F"), 
                               aes(x=age_locuteur, y=`totalduration(s)`, color=idPhrase, fill=idPhrase))+
  geom_smooth(method = "lm", formula = "y ~ x")+#, color ="#5B1A18")+
  #geom_point(alpha = 0.2) +
  theme(legend.title=element_text(size=11), 
        legend.text=element_text(size=10),
        plot.title = element_text(size = 12))+
  ggtitle("Evolution de la duree de l'enonce \nen fonction de l'age de la locutrice par phrase\n Donnees micro")+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne de l'enonce (s)")#+

largeurFigCm = 16
hauteurFigCm = 10
print(totalduration_micro_phrase)
ggsave(totalduration_micro_phrase, filename = "R_ENLIGNE/IMAGES/compare_totalduration_micro_femme_phrase.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

largeurFigCm = 30
hauteurFigCm = 10
compare_totalduration_bycondition = plot_grid(totalduration_enligne_sexe, totalduration_micro)
ggsave(compare_totalduration_bycondition, filename = "R_ENLIGNE/IMAGES/compare_totalduration_bycondition.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")
largeurFigCm = 20
hauteurFigCm = 10
compare_totaldurations = tibble(dureetotale = c(parametres_acoustiques_micro$`totalduration(s)`, parametres_acoustiques_enligne$`totalduration(s)`), 
                                age = c(parametres_acoustiques_micro$age_locuteur, parametres_acoustiques_enligne$age_locuteur),
                                condition = c(parametres_acoustiques_micro$condition, parametres_acoustiques_enligne$condition),
                                sexe = c(parametres_acoustiques_micro$sexe_locuteur, parametres_acoustiques_enligne$sexe_locuteur))

compare_totaldurations_fig = ggplot(compare_totaldurations %>% 
                                    filter(sexe == "F"),
                                    aes(x=age, y=dureetotale, color=condition, fill=condition))+
  geom_smooth(method = "lm")+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  #facet_wrap(~sexe)+
  #ggtitle("Duree du phone en fonction de l'age du locuteur \n Donnees enligne")+
  #xlab("Age du locuteur (ans)")+
  #ylab("Duree moyenne du phone (s)")+
  #scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"))+
  #scale_colour_manual(values=wes_palette(n=3, name="GrandBudapest1"))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))

compare_totaldurations_fig

warnings()
##################################################################
############ Comparaison classes d'âge en graphes ################
wes_palettes$Darjeeling1 #("MoonRise1")
largeurFigCm = 20
hauteurFigCm = 10
compare_classe = tibble(age = c(parametres_acoustiques_micro$age_locuteur, parametres_acoustiques_enligne$age_locuteur), 
                        condition = c(parametres_acoustiques_micro$condition, parametres_acoustiques_enligne$condition),
                        nom = c(parametres_acoustiques_micro$nom_locuteur, parametres_acoustiques_enligne$nom_locuteur))
n_distinct(compare_classe$nom)

effectifs_enligne = parametres_acoustiques_enligne %>% group_by(classe_age,age_locuteur, nom_locuteur, condition, sexe_locuteur) %>% summarise(variable = mean(mean_F0.Hz., na.rm = T)) #%>% spread(nom_locuteur, variable)
classe_hist_enligne = ggplot(effectifs_enligne,
                             aes(x = age_locuteur))+
  geom_histogram(alpha = .5, color = "black")
classe_hist_enligne
effectifs_micro = parametres_acoustiques_micro %>% group_by(classe_age,age_locuteur, nom_locuteur, condition, sexe_locuteur) %>% summarise(variable = mean(`mean_F0(Hz)`, na.rm = T)) #%>% spread(nom_locuteur, variable)
effectifs = rbind(effectifs_enligne, effectifs_micro)
compare_classe_hist = ggplot(effectifs,# %>% 
                               #n_distinct(compare_classe$nom, na.rm = F), 
                             aes(x = age_locuteur, fill = condition)) + 
  geom_histogram(alpha = .5, color = "black")+
  xlab("Age du locuteur (ans)")+
  ylab("Effectif")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
compare_classe_hist
ggsave(compare_classe_hist, filename = "R_ENLIGNE/IMAGES/compare_AGE_hist.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

test = n_distinct(parametres_acoustiques_enligne$nom_locuteur)
n_distinct(parametres_acoustiques_enligne$nom_locuteur)

compare_classe_density = ggplot(compare_classe, 
                              aes(x = age, fill = condition)) + 
  geom_density(alpha = .5)+
  scale_fill_manual(values= wes_palette(n=2, name = "GrandBudapest1", type = "discrete"))+
  xlab("Age du locuteur (ans)")+
  ylab("Densite de l'effectif")

compare_classe_density
ggsave(compare_classe_density, filename = "R_ENLIGNE/IMAGES/compare_AGE_density.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

################################################################################
############ Répartition des locuteurs en fonction de leur sexe ################

compare_sexe = tibble(sexe = c(parametres_acoustiques_micro$sexe_locuteur, parametres_acoustiques_enligne$sexe_locuteur), 
                      condition = c(rep("MICRO",length(parametres_acoustiques_micro$sexe_locuteur)), rep("EN LIGNE", length(parametres_acoustiques_enligne$sexe_locuteur))))
compare_sexe_hist = ggplot(effectifs, 
                             aes(x = sexe_locuteur, fill = condition)) + 
  geom_histogram(alpha = .5,color = "black", stat="count")+
  xlab("Sexe du locuteur")+
  ylab("Effectif")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))

compare_sexe_hist
ggsave(compare_sexe_hist, filename = "R_ENLIGNE/IMAGES/compare_sexe_hist.pdf", width = 15, height = 9, unit = "cm")

##################################################################
############ Comparaison de la répartition des phones ############

largeurFigCm = 20
hauteurFigCm = 10
phones_enligne = barplot(table(parametres_acoustiques_enligne$label), cex.names=1.6) # repartition des sons 
phones_micro = barplot(table(parametres_acoustiques_micro$label), cex.names=1.6) # repartition des sons 

compare_label = tibble(phones = c(parametres_acoustiques_micro$label, parametres_acoustiques_enligne$label), 
                       condition = c(rep("MICRO",length(parametres_acoustiques_micro$label)), rep("EN LIGNE", length(parametres_acoustiques_enligne$label))))
compare_label_hist = ggplot(compare_label, 
                            aes(x = phones, fill = condition)) + 
  geom_histogram(alpha = .4, color = "black", stat="count")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))+
  xlab("Phones")+
  ylab("Effectif")
compare_label_hist
ggsave(compare_label_hist, filename = "R_ENLIGNE/IMAGES/compare_label_hist.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

##################################################################
############ Comparaison classes d'âge en tableau ################

classeMIC = table(effectifs$classe_age[effectifs$condition=="MICRO"], dnn = c("classe_age"))
classeENLIGNE = table(effectifs$classe_age[effectifs$condition=="ENLIGNE"], dnn = "classe_age")

enligne<-data.frame(classeENLIGNE)
enligne
micro<-data.frame(classeMIC)
micro

comparaison<-enligne %>% left_join(micro, by=("classe_age"))
names(comparaison)[2]="effectif_micro"
names(comparaison)[3]="effectif_enligne"
write.csv(comparaison, file = "R_ENLIGNE/CSV/effectif_classeage.csv")
comparaison[is.na(comparaison)] <- 0
comparaison
###############################################################################
########## Comparaison des classes d'âges en fonction du sexe #################

classexe_enligne = table(effectifs_enligne$classe_age, effectifs_enligne$sexe_locuteur, dnn = c("classe_age", "sexe"))
classexe_micro = table(effectifs_micro$classe_age, effectifs_micro$sexe_locuteur, dnn = c("classe_age", "sexe"))

classeagesexe_enligne<-data.frame(classexe_enligne)
classeagesexe_enligne
classeagesexe_micro<-data.frame(classexe_micro)
classeagesexe_micro

comparaison2<-classeagesexe_enligne %>% left_join(classeagesexe_micro, by=c("sexe", "classe_age"))
names(comparaison2)[3]="effectif_micro"
names(comparaison2)[4]="effectif_enligne"
comparaison2
comparaison2[is.na(comparaison2)] <-0
write.csv(comparaison2, file = "R_ENLIGNE/CSV/effectif_classeage_sexe.csv")

#############################################################################
############ Comparaison de l'évolution de la durée du phone ################

largeurFigCm = 40
hauteurFigCm = 15
wes_palettes
duration_enligne = ggplot(parametres_acoustiques_par_phrase_enligne, 
                          aes(x=age_locuteur, y=dureePhonesMoy, color=sexe_locuteur, fill=sexe_locuteur))+
  geom_smooth(method = "loess", formula = y~x)+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  #ggtitle("Duree du phone en fonction de l'age du locuteur \n Donnees enligne")+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne du phone (s)")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))

  #scale_x_continuous(breaks=c(20,30,40,50,60,70,80,90))#, labels=c("20","30","40","50","60","70","80","90"))
duration_enligne

# duration_micro = ggplot(parametres_acoustiques_par_phrase_micro, 
#                         aes(x=age_locuteur, y=dureePhonesMoy, color=sexe_locuteur, fill=sexe_locuteur))+
#   geom_smooth(method = "loess", formula = "y ~ x")+
#   theme(legend.title=element_text(size=14), 
#         legend.text=element_text(size=12))+
#   xlab("Age du locuteur (ans)")+
#   ylab("Duree moyenne du phone (s)")+
#   scale_x_continuous(breaks=c(20,30,40,50,60,70,80,90))

duration_micro_femme = ggplot(parametres_acoustiques_par_phrase_micro %>% 
                                filter(sexe_locuteur=="F"), 
                              aes(x=age_locuteur, y=dureePhonesMoy, color=sexe_locuteur, fill=sexe_locuteur))+
  geom_smooth(method = "loess", formula = "y ~ x")+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12))+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne du phone (s)")+
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
duration_micro_femme
compdurationphone=plot_grid(duration_micro_femme, duration_enligne, ncol = 2, nrow = 1, labels=c('   Donnees micro', '   Donnees en ligne'))
compdurationphone
ggsave(compdurationphone, filename = "R_ENLIGNE/IMAGES/duration_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


largeurFigCm = 20
hauteurFigCm = 12

parametres_acoustiques_par_phrase_micro_femme <- parametres_acoustiques_par_phrase_micro[parametres_acoustiques_par_phrase_micro$sexe_locuteur == "F", ]
parametres_acoustiques_par_phrase_enligne_femme <- parametres_acoustiques_par_phrase_enligne[parametres_acoustiques_par_phrase_enligne$sexe_locuteur == "F", ]

parametres_acoustiques_par_phrase_micro_homme <- parametres_acoustiques_par_phrase_micro[parametres_acoustiques_par_phrase_micro$sexe_locuteur == "M", ]
parametres_acoustiques_par_phrase_enligne_homme <- parametres_acoustiques_par_phrase_enligne[parametres_acoustiques_par_phrase_enligne$sexe_locuteur == "M", ]

compare_durationphone_femme = tibble(age = c(parametres_acoustiques_par_phrase_micro_femme$age_locuteur, parametres_acoustiques_par_phrase_enligne_femme$age_locuteur),
                                     duree = c(parametres_acoustiques_par_phrase_micro_femme$dureePhonesMoy, parametres_acoustiques_par_phrase_enligne_femme$dureePhonesMoy), 
                                     condition = c(rep("MICRO",length(parametres_acoustiques_par_phrase_micro_femme$age_locuteur)), rep("EN LIGNE", length(parametres_acoustiques_par_phrase_enligne_femme$age_locuteur))))
fig_compare_durationphone_femme = ggplot(compare_durationphone_femme, 
                                         aes(x = age, y= duree, color = condition, fill = condition)) + 
  geom_smooth(method ="loess") + 
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))
fig_compare_durationphone_femme
ggsave(fig_compare_durationphone_femme, filename = "R_ENLIGNE/IMAGES/duration_comp_one_femme.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

compare_durationphone = tibble(age = c(parametres_acoustiques_par_phrase_micro$age_locuteur, parametres_acoustiques_par_phrase_enligne$age_locuteur), 
                               duree = c(parametres_acoustiques_par_phrase_micro$dureePhonesMoy, parametres_acoustiques_par_phrase_enligne$dureePhonesMoy), 
                               condition = c(rep("MICRO",length(parametres_acoustiques_par_phrase_micro$age_locuteur)), rep("EN LIGNE", length(parametres_acoustiques_par_phrase_enligne$age_locuteur))))
fig_compare_durationphone = ggplot(compare_durationphone, 
                                   aes(x = age, y= duree, color = condition, fill = condition)) + 
  geom_point()+
  geom_smooth(method ="loess") +
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
fig_compare_durationphone
ggsave(fig_compare_durationphone, filename = "R_ENLIGNE/IMAGES/duration_comp_one.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

fig_compare_durationfric_enligne = ggplot(parametres_acoustiques_enligne %>% 
                                             filter(mode == "fricative"), 
                                   aes(x = age_locuteur, y= duration.s., color = sexe_locuteur, fill = sexe_locuteur)) + 
  geom_smooth(method ="loess") +
  facet_wrap(~voisement)+
  
  #guides(fill=guide_legend(title="New Legend Title"))+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne du phone (s)")+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"), name = "Sexe")+
  scale_fill_manual(values=c("#5B1A18","#D67236"), name = "Sexe")
fig_compare_durationfric_enligne
largeurFigCm = 22
hauteurFigCm = 10
ggsave(fig_compare_durationfric_enligne, filename = "R_ENLIGNE/IMAGES/compare_durationfric_enligne.pdf", width = largeurFigCm, height = hauteurFigCm, units = "cm")




fig_compare_durationfric_enligne_femmes = ggplot(parametres_acoustiques_enligne %>% 
                                            filter(mode == "fricative" & sexe_locuteur =="F"), 
                                          aes(x = age_locuteur, y= duration.s.)) + 
  geom_smooth(method ="loess") + 
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
fig_compare_durationfric_enligne_femmes

fig_compare_durationfric_enligne_hommes = ggplot(parametres_acoustiques_enligne %>% 
                                            filter(mode == "fricative" & sexe_locuteur =="M"), 
                                          aes(x = age_locuteur, y= duration.s.)) + 
  geom_smooth(method ="loess") + 
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
fig_compare_durationfric_enligne_hommes

fig_compare_durationocc_enligne = ggplot(parametres_acoustiques_enligne %>% 
                                                  filter(mode == "occlusive"), 
                                         aes(x = age_locuteur, y= duration.s., color = sexe_locuteur, fill = sexe_locuteur)) + 
  facet_wrap(~voisement)+
  geom_smooth(method = "loess", formula = y~x) + 
  #geom_point()+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne du phone (s)")+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"), name = "Sexe")+
  scale_fill_manual(values=c("#5B1A18","#D67236"), name = "Sexe")
fig_compare_durationocc_enligne

fig_compare_durationocc_enligne_hommes = ggplot(parametres_acoustiques_enligne %>% 
                                            filter(mode == "occlusive"& sexe_locuteur=="M"), 
                                          aes(x = age_locuteur, y= duration.s.)) + 
  geom_smooth(method = "loess", formula = y~x) + 
  #geom_point()+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
fig_compare_durationocc_enligne_hommes

fig_compare_durationocc_enligne_femmes = ggplot(parametres_acoustiques_enligne %>% 
                                           filter(mode == "occlusive"& sexe_locuteur=="F"), 
                                         aes(x = age_locuteur, y= duration.s.)) + 
  geom_smooth(method ="loess") + 
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
fig_compare_durationocc_enligne_femmes

compare_durationphone_homme = tibble(age = c(parametres_acoustiques_par_phrase_micro_homme$age_locuteur, parametres_acoustiques_par_phrase_enligne_homme$age_locuteur), 
                                     duree = c(parametres_acoustiques_par_phrase_micro_homme$dureePhonesMoy, parametres_acoustiques_par_phrase_enligne_homme$dureePhonesMoy), 
                                     condition = c(rep("MICRO",length(parametres_acoustiques_par_phrase_micro_homme$age_locuteur)), rep("EN LIGNE", length(parametres_acoustiques_par_phrase_enligne_homme$age_locuteur))))
ggplot(compare_durationphone_homme, aes(x = age, y= duree, color = condition, fill = condition)) + 
  geom_smooth(method ="loess") + 
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))


cor(parametres_acoustiques_enligne$duration.s.[parametres_acoustiques_enligne$label=="a"&parametres_acoustiques_enligne$sexe_locuteur == "F"],
    parametres_acoustiques_enligne$age_locuteur[parametres_acoustiques_enligne$label=="a"&parametres_acoustiques_enligne$sexe_locuteur =="F"])
cor(parametres_acoustiques_enligne$mean_F0.Hz.[parametres_acoustiques_enligne$sexe_locuteur == "F"],
    parametres_acoustiques_enligne$age_locuteur[parametres_acoustiques_enligne$sexe_locuteur =="F"], use="complete.obs")
parametres_acoustiques_enligne$mean_F0.Hz.
cor(parametres_acoustiques_enligne$mean_CoG.Hz.[parametres_acoustiques_enligne$mode=="fricative"&
                                                 parametres_acoustiques_enligne$sexe_locuteur == "M" & 
                                                 parametres_acoustiques_enligne$voisement =="sourde"],
    parametres_acoustiques_enligne$age_locuteur[parametres_acoustiques_enligne$mode=="fricative"&
                                                  parametres_acoustiques_enligne$sexe_locuteur =="M"&
                                                  parametres_acoustiques_enligne$voisement =="sourde"])


fig_compare_durationvoyelle_enligne = ggplot(parametres_acoustiques_enligne %>% 
                                           filter(statutCV=="V" ), 
                                         aes(x = age_locuteur, y= duration.s., color = sexe_locuteur, fill = sexe_locuteur)) + 
  #facet_wrap(~voisement)+
  geom_smooth() + 
  #geom_point()+
  facet_wrap(~idPhrase)+
  xlab("Age du locuteur (ans)")+
  ylab("Duree moyenne du phone (s)")+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"), name = "Sexe")+
  scale_fill_manual(values=c("#5B1A18","#D67236"), name = "Sexe")

fig_compare_durationvoyelle_enligne
ggsave(fig_compare_durationvoyelle_enligne, filename = "compare_durationvoyelle_enligne.pdf", width = largeurFigCm, height = hauteurFigCm, units = "cm")
#################################################################################
############ Comparaison DE L'ÉVOLUTION de la durée de la phrase ################

durationbyphrase_mic = ggplot(parametres_acoustiques_par_phrase_micro %>% 
                                filter(sexe_locuteur=="F"), 
                              aes(x=age_locuteur, y=dureePhonesMoy, color=idPhrase, fill=idPhrase))+
  geom_smooth(method = "loess", formula = "y ~ x")+
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10))

durationbyphrase_enligne = ggplot(parametres_acoustiques_par_phrase_enligne %>% 
                                    filter(sexe_locuteur=="F"), 
                                  aes(x=classe_age, y=dureePhonesMoy, color=idPhrase, fill=idPhrase))+
  geom_boxplot()+
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10))
durationbyphrase_enligne
compdurationphrase=plot_grid(durationbyphrase_mic, durationbyphrase_enligne, ncol = 2, nrow = 1, labels=c('   Donnees micro', '   Donnees en ligne'))
compdurationphrase
ggsave(compdurationphrase, filename = "R_ENLIGNE/IMAGES/duration_comp_phrase.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

durationphrase_femme_mic = ggplot(parametres_acoustiques_par_phrase_micro %>% 
                                    filter(sexe_locuteur=="F") ,
                                  aes(x=age_locuteur, y=dureePhrase, color = sexe_locuteur, fill = sexe_locuteur))+
  geom_smooth(method = "loess", formula = "y ~ x")+#, color="lightgreen", fill="lightgreen") +
  ggtitle("Evolution de la duree de la phrase en fonction de l'age du locuteur (micro)")+
  #ylab("duree des phrases (s)") +
  xlab("age du locuteur (an)")+
  scale_x_continuous(limits = c(18,90))+
  scale_y_continuous("duree des phrases (s)")+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
durationphrase_femme_mic
durationphrase_femme_enligne = ggplot(parametres_acoustiques_par_phrase_enligne %>% 
                                        subset(!is.na(dureePhrase)),# %>% 
                                        #filter(sexe_locuteur=="F") ,
                                      aes(x=age_locuteur, y=dureePhrase, color =sexe_locuteur))+
  geom_smooth(method = "loess", formula = "y ~ x") +
  #geom_point()+
  facet_wrap(~idPhrase)+
  ggtitle("Evolution de la duree de la phrase \nen fonction de l'age du locuteur")+
  ylab("duree des phrases (s)") +
  xlab("age du locuteur (an)")+
  scale_x_continuous(limits = c(18,90))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))
durationphrase_femme_enligne$
#compdurationphrase_femme=plot_grid(durationphrase_femme_mic, durationphrase_femme_enligne, ncol = 2, nrow = 1)
#compdurationphrase_femme
#ggsave(compdurationphrase_femme, filename = "R_ENLIGNE/IMAGES/durationphrase_AGE_femme.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")
dureesphrase = parametres_acoustiques_par_phrase_enligne %>% 
  summarise(correlation =cor(dureePhrase, age_locuteur, use="complete.obs")) %>% 
  group_by(idPhrase) %>%  
  spread(idPhrase, correlation)


parametres_acoustiques_par_phrase_enligne$dureePhrase
listephrase = c("phrase01", "phrase02", "phrase03", "phrase04", "phrase05", "phrase06", "phrase07", "phrase08", "phrase09","phrase10", "phrase11", "phrase12", "phrase13", "phrase14", "phrase15")
bilan_phrase = list()
for (phrase_traitee in listephrase) {
  bilan_phrase[[phrase_traitee]]= parametres_acoustiques_par_phrase_enligne %>% 
    filter(idPhrase==phrase_traitee) %>%
    group_by(idPhrase, sexe_locuteur) %>% 
    summarise(correl = cor(dureePhrase, age_locuteur, use = "complete.obs")) %>% 
    spread(sexe_locuteur, correl)
  }
bilan_phrase = bind_rows(bilan_phrase)
mean(bilan_phrase$F)
mean(bilan_phrase$M)

bilan_phrase= parametres_acoustiques_par_phrase_enligne %>% group_by(sexe_locuteur, idPhrase) %>% summarise(correl = cor(dureePhrase, age_locuteur, use = "complete.obs")) %>% spread(sexe_locuteur, correl)
bilanF0moyLarge_micro
# compare_duration = tibble(age = c(parametres_acoustiques_micro$age_locuteur, parametres_acoustiques_enligne$age_locuteur), 
#                           duree = c(parametres_acoustiques_micro$`duration(s)`, parametres_acoustiques_enligne$duration.s.), 
#                           condition = c(rep("MICRO",length(parametres_acoustiques_micro$`duration(s)`)), rep("EN LIGNE", length(parametres_acoustiques_enligne$duration.s.))))
# ggplot(compare_duration, aes(x = age, y = duree, fill = condition)) + geom_smooth(method = "loess", alpha = .5) #+ scale_x_continuous(limits = c(0,.5))


###########################################################
################ Fréquence Fondamentale ###################

bilanF0moyLarge_micro = parametres_acoustiques_micro %>% group_by(label, classe_age, sexe_locuteur) %>% summarise(f0moy = mean(F0Moy, na.rm = TRUE)) %>% spread(classe_age, f0moy)
bilanF0moyLarge_micro

bilanF0moyLarge_enligne = parametres_acoustiques_enligne %>% group_by(label, classe_age, sexe_locuteur) %>% summarise(f0moy = mean(mean_F0.Hz., na.rm = TRUE)) %>% spread(classe_age, f0moy)
bilanF0moyLarge_enligne

decompte = parametres_acoustiques_enligne %>% 
  group_by(label) %>% 
  filter(statutCV == "V") %>% 
  count(label,sort = TRUE) 

voyelles = c("a", "e", "i", "a~", "o~")

write_csv(regroupement_enligne, path="R_ENLIGNE/CSV/regroupement_enligne.csv")
write_csv(regroupement_micro, path="R_MIC/CSV/regroupement_micro.csv")

comparaison_F0<- bilanF0moyLarge_micro %>% left_join(bilanF0moyLarge_enligne, by=c("sexe_locuteur","label"))
names(comparaison_F0)[3]="15-19_micro"
names(comparaison_F0)[4]="20-29_micro"
names(comparaison_F0)[5]="30-39_micro"
names(comparaison_F0)[6]="50-59_micro"
names(comparaison_F0)[7]="60-69_micro"
names(comparaison_F0)[8]="80-89_micro"
names(comparaison_F0)[9]="15-19_enligne"
names(comparaison_F0)[10]="20-29_enligne"
names(comparaison_F0)[11]="30-39_enligne"
names(comparaison_F0)[12]="50-59_enligne"
names(comparaison_F0)[13]="60-69_enligne"
comparaison_F0
write.csv(comparaison_F0, file = "R_ENLIGNE/CSV/F0_age_sexe.csv")
summary(parametres_acoustiques_enligne$classe_age)
compare_F0 = tibble(age = c(parametres_acoustiques_enligne$age_locuteur, parametres_acoustiques_micro$age_locuteur), 
                    F0 = c(parametres_acoustiques_enligne$mean_F0.Hz., parametres_acoustiques_micro$`mean_F0(Hz)`), 
                    label = c(parametres_acoustiques_enligne$label, parametres_acoustiques_micro$label), 
                    condition = c(parametres_acoustiques_enligne$condition,parametres_acoustiques_micro$condition), 
                    sexe = c(parametres_acoustiques_enligne$sexe_locuteur,parametres_acoustiques_micro$sexe_locuteur), 
                    classe = c(parametres_acoustiques_enligne$classe_age, parametres_acoustiques_micro$classe_age))
compareF0_fig = ggplot(compare_F0,# %>% 
                       #filter(statutCV =="V"& nasalite=="orale"), 
                       aes(y= F0, x = condition, 
                           color = classe)) + 
  facet_wrap(~sexe)+ 
  geom_boxplot()+ #alpha = .4
  scale_colour_brewer(palette = "Spectral")+#, aesthetics = "colour")+ #7
  scale_fill_brewer(palette= "Spectral")
compareF0_fig
ggsave(compareF0_fig, filename = "R_ENLIGNE/IMAGES/F0_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")



compareF0_fig = ggplot(parametres_acoustiques_enligne,# %>% 
                         #filter(statutCV =="V"& nasalite=="orale"), 
                       aes(y= mean_F0.Hz., x = sexe_locuteur, 
                           color = classe_age)) + 
 # facet_wrap(~sexe_locuteur)+ 
  geom_boxplot()+ #alpha = .4
  scale_colour_brewer(palette = "Spectral")+#, aesthetics = "colour")+ #7
  scale_fill_brewer(palette= "Spectral")
compareF0_fig

compareF0_fig = ggplot(parametres_acoustiques_enligne,# %>% 
                         #filter(statutCV =="C"& mode=="fricative", voisement=="sonore"), 
                       aes(y= mean_F0.Hz., x = age_locuteur, 
                           color = sexe_locuteur)) + 
  # facet_wrap(~sexe_locuteur)+ 
  geom_smooth(method = "loess")+ #alpha = .4
  scale_colour_manual(values=c("#5B1A18","#D67236"))
#scale_fill_brewer(palette= "Spectral")
compareF0_fig
listevoyelles_nonasale
ggsave(compareF0_fig, filename = "R_ENLIGNE/IMAGES/F0_comp_line.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

listenasales = c("a~", "e~", "o~")
listeocclusivessourdes = c("p", "t", "k")
listevoyelles = c("a","i")
listefricativesourde = c("s", "S", "f")
listenasales_a = c("a~", "e~", "o~", "a") 
listevoyelles2 = c( "u", "i", "y")
consonnesnasales = c("m", "n")
listevoyelles_eE2 = c("e","E","2", "a", "i", "o", "u", "y","O","o~", "a~")
listevoyelles_tout = c("e","E","2", "a", "i", "o", "u", "y","O","o~", "a~", "9", "9~")


bilanF0_nasales_micro=list()
bilanF0_voyelles_enligne=list()

for (voyelle_traitee in voyelles) {
  bilanF0_voyelles_enligne[[voyelle_traitee]]= parametres_acoustiques_enligne %>% 
    filter(label==voyelle_traitee) %>%
    group_by(classe_age, sexe_locuteur, label) %>% 
    summarise(f0moy = mean(mean_F0.Hz., na.rm = TRUE)) %>% 
    spread(classe_age, f0moy)
}
bilanF0_voyelles_enligne = bind_rows(bilanF0_voyelles_enligne)
bilanF0_voyelles_enligne
voyelles = c("a", "e", "i", "a~", "o~", "E", "~e")
nasales = c("a~", "o~", "e~")
figF0voyelles=ggplot(parametres_acoustiques_enligne %>% 
         filter(label %in% voyelles),
       aes(x = age_locuteur, y = mean_F0.Hz., color = sexe_locuteur, fill = sexe_locuteur))+
  geom_smooth(method = "loess")+
  theme_bw(base_size =20)+
  theme(strip.text = element_text(size = 22),
  #      strip.text.y = element_text(size = 20),
  #      axis.text=element_text(size=20),
        axis.title=element_text(face="bold"))+
  facet_wrap(~label, nrow = 3) + 
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))
figF0voyelles
ggsave(figF0voyelles, filename = "R_ENLIGNE/IMAGES/F0_voyelles.pdf", width = largeurFigCmBig, height = hauteurFigCmBig)
table(parametres_acoustiques_enligne$mode)
ggplot(parametres_acoustiques_enligne %>% 
         filter(label=="l"),
       aes(x = age_locuteur, y = mean_F0.Hz., color = sexe_locuteur))+#, fill = sexe_locuteur))+
  geom_smooth(method = "loess")+
  #geom_boxplot()+
  facet_wrap(~label) + 
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  scale_fill_manual(values=c("#5B1A18","#D67236"))

for (nasale_traitee in listenasales) {
  bilanF0_nasales_enligne[[nasale_traitee]]= regroupement_enligne %>% 
    filter(label==nasale_traitee) %>% 
    group_by(classe_age.x, sexe_locuteur.x, label) %>% 
    summarise(f0moy = mean(F0Moy, na.rm = TRUE)) %>% 
    spread(classe_age.x, f0moy)
}
bilanF0_nasales_enligne = bind_rows(bilanF0_nasales_enligne)
bilanF0_nasales_enligne

fig_F0_micros = ggplot(regroupement_micro %>% filter(label==listenasales),
                       aes(x = classe_age.x, 
                           y = F0Moy, 
                           colour = factor(label)) ) + 
  geom_boxplot()+
  facet_wrap(~sexe_locuteur.x)+
  ggtitle("F0 moyenne en fonction de la classe d'age et du sexe du \nlocuteur.") +
  theme_bw(base_size = 10)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=11))+
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))#c("#5B1A18","#D67236", ""))
print(fig_F0_micros)

fig_F0_enligne = ggplot(regroupement_enligne %>% filter(label==listenasales),
                       aes(x = classe_age, 
                           y = F0Moy, 
                           colour = factor(sexe_locuteur)) ) + 
  geom_boxplot()+
  facet_wrap(~label)+
  ggtitle("F0 moyenne en fonction de la classe d'age et du sexe du \nlocuteur.") +
  theme_bw(base_size = 10)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=11))+
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))#c("#5B1A18","#D67236", ""))

print(fig_F0_enligne)

########################################################################################################
########### Comparaison des données sur la F0 des nasales enligne / micro en fonction du sexe ##########

compare_F0_nasale = tibble(age = c(regroupement_micro$age_locuteur, regroupement_enligne$age_locuteur), 
                           F0 = c(regroupement_micro$F0Moy, regroupement_enligne$F0Moy), 
                           label = c(regroupement_micro$label, regroupement_enligne$label), 
                           condition = c(rep("MICRO",length(regroupement_micro$F0Moy)), rep("EN LIGNE", length(regroupement_enligne$F0Moy))), 
                           sexe = c(regroupement_micro$sexe_locuteur,regroupement_enligne$sexe_locuteur), 
                           classe = c(regroupement_micro$classe_age, regroupement_enligne$classe_age))
compareF0_nasale_fig = ggplot(compare_F0_nasale %>% 
                                filter(label%in%listenasales),
                              aes(y= F0, x = condition, color = label))+#, fill = label)) + 
  facet_wrap(~sexe)+ 
  geom_boxplot()+ #alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))

compareF0_nasale_fig
largeurFigCm = 30
hauteurFigCm = 12

compare_F0_nasale_wage = tibble(age = c(regroupement_micro$age_locuteur, regroupement_enligne$age_locuteur), 
                                F0 = c(regroupement_micro$F0Moy, regroupement_enligne$F0Moy), 
                                label = c(regroupement_micro$label, regroupement_enligne$label), 
                                condition = c(rep("MICRO",length(regroupement_micro$F0Moy)), rep("EN LIGNE", length(regroupement_enligne$F0Moy))), 
                                sexe = c(regroupement_micro$sexe_locuteur,regroupement_enligne$sexe_locuteur), 
                                classe = c(regroupement_micro$classe_age, regroupement_enligne$classe_age))
compareF0_nasale_fig_wage = ggplot(compare_F0_nasale_wage %>% 
                                filter(label%in%listenasales),
                              aes(y= F0, x = classe, color = condition))+#, fill = label)) + 
  facet_wrap(label~sexe)+ 
  geom_boxplot()+ #alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10))

compareF0_nasale_fig_wage
ggsave(compareF0_nasale_fig_wage, filename = "R_ENLIGNE/IMAGES/essai_F0_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")
###############
compare_F0_occlusive_s = tibble(age = c(regroupement_micro$age_locuteur, regroupement_enligne$age_locuteur), 
                                F0 = c(regroupement_micro$F0Moy, regroupement_enligne$F0Moy), 
                                label = c(regroupement_micro$label, regroupement_enligne$label), 
                                condition = c(rep("MICRO",length(regroupement_micro$F0Moy)), rep("EN LIGNE", length(regroupement_enligne$F0Moy))), 
                                sexe = c(regroupement_micro$sexe_locuteur,regroupement_enligne$sexe_locuteur), 
                                classe = c(regroupement_micro$classe_age, regroupement_enligne$classe_age), 
                                idPhrase = c(regroupement_micro$idPhrase,regroupement_enligne$idPhrase))
compareF0_occlusive_fig_femme = ggplot(compare_F0_occlusive_s %>% 
                                filter(label%in%listeocclusivessourdes & sexe == "F"),
                              aes(y= F0, x = classe, color = label))+#, fill = label)) + 
  facet_wrap(~condition)+ 
  geom_boxplot()+ #alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))
compareF0_occlusive_fig_femme

compareF0_occlusive_fig_homme = ggplot(compare_F0_occlusive_s %>% 
                                         filter(label%in%listeocclusivessourdes & sexe == "M"),
                                       aes(y= F0, x = classe, color = label))+#, fill = label)) + 
  facet_wrap(~condition)+ 
  geom_boxplot()+ #alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))

compareF0_occlusive_fig_homme
plot_grid(compareF0_occlusive_fig_femme,compareF0_occlusive_fig_homme)
###############
compareF0_R_fig_femme = ggplot(compare_F0 %>% 
                                         filter(label=="a" & sexe == "F"),
                                       aes(y= F0, x = classe))+#, fill = label)) + 
  facet_wrap(~condition)+ 
  geom_boxplot()+ #alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))
compareF0_R_fig_femme


compareF0_consnasale_fig = ggplot(compare_F0 %>% 
                                 filter(label %in% consonnesnasales),
                               aes(y= F0, x = classe, fill = condition)) + 
  facet_wrap(label~sexe)+ 
  geom_boxplot(alpha=.8)+
  scale_fill_manual(values=wes_palette(name = "BottleRocket2"))
compareF0_consnasale_fig

fig_F0_tout = ggplot(regroupe_tout %>% filter(label %in% listenasales),
                     aes(x = classe_age, 
                         y = F0Moy, 
                         colour = factor(sexe_locuteur)) ) + 
  geom_boxplot()+
  facet_wrap(~label)+
  ggtitle("F0 moyenne en fonction de la classe d'age et du sexe du \nlocuteur.") +
  theme_bw(base_size = 10)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=11))+
  scale_color_manual(values=c("#5B1A18","#D67236", ""))
warnings()
print(fig_F0_tout)

compareF0_consnasale_fig = ggplot(regroupe_tout %>% 
                                    filter(label %in% consonnesnasales),
                                  aes(y = F0Moy, x = classe_age.x, fill = condition)) + 
  facet_wrap(label~sexe_locuteur.x)+ 
  geom_boxplot(alpha=.8)+
  scale_fill_manual(values=wes_palette(name = "BottleRocket2"))
compareF0_consnasale_fig

compareF0_nasale_fig = ggplot(regroupe_tout %>% 
                                filter(label %in% listenasales),
                              aes(y = F0Moy, x = classe_age)) + 
  facet_wrap(~sexe_locuteur)+ 
  geom_boxplot(alpha=.8, color = "#D67236")#, color= c("#5B1A18","#D67236"))
#scale_color_manual(values=c("#5B1A18","#D67236"))
compareF0_nasale_fig
ggsave(compareF0_nasale_fig, filename = "R_ENLIGNE/IMAGES/F0_classe_sexe_nasale.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


compareF0_a_fig = ggplot(regroupe_tout %>% 
                           filter(label  %in% listenasales_a),
                         aes(y = F0Moy, x = classe_age)) + 
  facet_wrap(~sexe_locuteur)+ 
  geom_boxplot(alpha=.8, color = "#D67236")+#, color= c("#5B1A18","#D67236"))
  xlab("Classe d'age du locuteur")+
  ylab("F0 moyenne ")
#scale_color_manual(values=c("#5B1A18","#D67236"))
compareF0_a_fig
ggsave(compareF0_a_fig, filename = "R_ENLIGNE/IMAGES/F0_classe_sexe_nasale_a.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


largeurFigCmPortrait = 30
hauteurFigCmPortrait = 20


#################################################################
############################ FORMANTS ###########################

#regroupe_tout$F
compareF1_voyelles_fig = ggplot(parametres_acoustiques_enligne%>% 
                                  filter(label %in% c("a", "E")),
                           #filter(statutCV =="V" & nasalite =="orale"& sexe_locuteur=="F"),
                         aes(y = bark(mean_F1.Hz.), x = classe_age, color = sexe_locuteur)) + 
  facet_wrap(~label)+ 
  geom_boxplot()+ #660033
  ggtitle("Valeur moyenne de F1 pour les voyelles nasales [a~],[e~] et [o~] et \nla voyelle [a] en fonction du sexe et de la classe d'age")+
  xlab("Classe d'age du locuteur")+
  ylab("F1 moyenne ")+
  scale_color_manual(values=c("#5B1A18","#D67236"))
compareF1_voyelles_fig
ggsave(compareF1_a_nasales_fig, filename = "R_ENLIGNE/IMAGES/F1_classe_sexe_nasale_a.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

compareF1_voyelles_fig = ggplot(parametres_acoustiques_enligne%>% 
                                  filter(label %in% c("a", "E")),
                                #filter(statutCV =="V" & nasalite =="orale"& sexe_locuteur=="F"),
                                aes(y = bark(mean_F1.Hz.), x =age_locuteur, color = sexe_locuteur)) + 
  facet_wrap(~label)+ 
  geom_smooth(method = "loess")+ #660033
  ggtitle("Valeur moyenne de F1 pour les voyelles [a] et [E]\nen fonction de l'age et du sexe du locuteur")+
  xlab("Age du locuteur (ans)")+
  ylab("F1 moyenne (Bark)")+
  scale_color_manual(values=c("#5B1A18","#D67236"))
compareF1_voyelles_fig
ggsave(compareF1_voyelles_fig, filename = "R_ENLIGNE/IMAGES/F1_voyelles_aE.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


compareF2_voyelles_fig = ggplot(parametres_acoustiques_enligne%>% 
                                  #filter(label %in% c("a", "E")),
                                  filter(label %in% c("9","a","o")),
                                  aes(y = bark(mean_F2.Hz.), x = age_locuteur, color = sexe_locuteur)) + 
  facet_wrap(~label)+ 
  #geom_boxplot()+ #660033
  geom_smooth(method = "loess")+
  ggtitle("Valeur moyenne de F2 pour les voyelles [a],[9] et [o] et \nen fonction du sexe et de l'age")+
  xlab("Age du locuteur (ans)")+
  ylab("F2 moyenne (Bark)")+
  scale_color_manual(values=c("#5B1A18","#D67236"))
compareF2_voyelles_fig
ggsave(compareF2_voyelles_fig, filename = "R_ENLIGNE/IMAGES/F2_voyelles_a9o.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")


compareF3_voyelles_fig = ggplot(parametres_acoustiques_enligne%>% 
                                  filter(label %in% c("a", "O", "o", "u", "y")),
                                  #filter(statutCV=="V" & nasalite=="orale"),
                                aes(y = bark(mean_F3.Hz.), x = age_locuteur, color = sexe_locuteur)) + 
  facet_wrap(~label)+ 
  #geom_boxplot()+ #660033
  geom_smooth(method = "loess")+
  ggtitle("Valeur moyenne de F3 pour les voyelles [a], [o], [O], [u] et [y] et \nen fonction du sexe et de l'age du locuteur")+
  xlab("Age du locuteur (ans)")+
  ylab("F3 moyen (Bark)")+
  scale_color_manual(values=c("#5B1A18","#D67236"))
compareF3_voyelles_fig
ggsave(compareF3_voyelles_fig, filename = "R_ENLIGNE/IMAGES/F2_voyelles_aoOuy.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

listevoyelles_eE = c("e", "E")
compareF2_o_fig = ggplot(regroupe_tout %>% 
                           filter(label %in% listevoyelles_eE),
                         aes(y = F2moy, x = classe_age.x)) + 
  facet_wrap(sexe_locuteur.x~condition)+ 
  geom_boxplot(color= "#D67236")+ #660033
  xlab("Classe d'age du locuteur")+
  ylab("F2 moyenne ")
compareF2_o_fig

###########

compare_F1_F2 = tibble(age = c(regroupe_tout$age_locuteur.x, regroupe_tout$age_locuteur.x),
                       Formant = c(regroupe_tout$F1moy, regroupe_tout$F1moy), 
                       Formant2 = c(regroupe_tout$F2moy, regroupe_tout$F2moy),
                       label = c(regroupe_tout$label, regroupe_tout$label), 
                       condition = c(regroupe_tout$condition, regroupe_tout$condition), 
                       sexe = c(regroupe_tout$sexe_locuteur.x,regroupe_tout$sexe_locuteur.x), 
                       classe = c(regroupe_tout$classe_age.x, regroupe_tout$classe_age.x), 
                       idPhrase = c(regroupe_tout$idPhrase,regroupe_tout$idPhrase))
compare_F1_fig = ggplot(regroupe_tout %>%
                             filter(sexe_locuteur.x == "F" & label %in% listenasales_a & condition =="MICRO"),
                           aes(x = age_locuteur.x, y =F1moy, color=label,fill = label))+#, fill = label)) + 
  #facet_wrap(~condition)+ 
  geom_smooth(method = "loess")+#alpha = .4
  theme_bw(base_size = 12)+
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))+
  scale_fill_manual(values=wes_palette(name = "BottleRocket2"))+
  xlab("Age de la locutrice")+
  ylab("F1 moyenne ")
compare_F1_fig

compare_F2_fig = ggplot(regroupe_tout %>%
                             filter(sexe_locuteur.x == "F" & label %in% listenasales_a & condition =="MICRO"),
                           aes(x = age_locuteur.x, y =F2moy, color=label,fill = label))+#, fill = label)) + 
  #facet_wrap(~condition)+ 
  theme_bw(base_size = 12)+
  geom_smooth(method = "loess")+#alpha = .4
  scale_color_manual(values=wes_palette(name = "BottleRocket2"))+
  scale_fill_manual(values=wes_palette(name = "BottleRocket2"))+
  xlab("Age de la locutrice")+
  ylab("F2 moyenne ")
compare_F2_fig
hauteurFigCm= 8
compare_F1_F2_fig = plot_grid(compare_F1_fig, compare_F2_fig, labels = c("  Donnees micro", "  Donnees micro et en ligne"))
ggsave(compare_F1_F2_fig, filename = "R_ENLIGNE/IMAGES/F1_F2_2_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

compare_F2_fig = ggplot(regroupe_tout %>%
                          filter( label %in% listevoyelles_eE2 & sexe_locuteur.x == "F" & condition =="MICRO"),
                        aes(x = age_locuteur.x, y =F2moy, color=label,fill = label))+#, fill = label)) + 
  #facet_wrap(~condition)+ 
  theme_bw(base_size = 12)+
  geom_smooth(method = "loess")+#alpha = .4
   scale_color_brewer(palette = "Paired")+
   scale_fill_brewer(palette = "Paired")+
  # # scale_color_manual(values=wes_palette(name = "BottleRocket2"))+
  # scale_fill_manual(values=wes_palette(name = "BottleRocket2"))+
  xlab("Age de la locutrice")+
  ylab("F2 moyenne ")
compare_F2_fig
hauteurFigCm= 15
longueurFigCm = 20
ggsave(compare_F2_fig, filename = "R_ENLIGNE/IMAGES/F2_eE_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

compare_F3_fig = ggplot(regroupe_tout %>%
                          filter( label %in% listevoyelles_eE2 & sexe_locuteur.x == "F" & condition =="MICRO"),
                        aes(x = age_locuteur.x, y =F3moy, color=label,fill = label))+#, fill = label)) + 
  #facet_wrap(~condition)+ 
  theme_bw(base_size = 12)+
  geom_smooth(method = "loess")+#alpha = .4
  scale_color_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+
  # # scale_color_manual(values=wes_palette(name = "BottleRocket2"))+
  # scale_fill_manual(values=wes_palette(name = "BottleRocket2"))+
  xlab("Age de la locutrice")+
  ylab("F3 moyenne ")
compare_F3_fig
ggsave(compare_F3_fig, filename = "R_ENLIGNE/IMAGES/F3_comp.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

#################################################################
############################## ZCR ##############################

parametres_acoustiques_par_phrase_enligne
fig_ZCR_fricative = ggplot(parametres_acoustiques_enligne %>% 
                             #filter(voisement == "sourde"& statutCV =="C", mode =="fricative"),
                             filter(label  %in%  c("E", "i", "y")), #& sexe_locuteur=="F"
                              aes(x = age_locuteur, 
                                  y = mean_ZCR, 
                                  color = sexe_locuteur)) + 
  geom_smooth(method = "loess")+
  facet_wrap(~ label)+
  ggtitle("ZCR moyen en fonction de l'age et du sexe du \nlocuteur pour les fricatives sourdes.") +
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=12),
        legend.position="bottom")+
  xlab("Classe d'age")+
  ylab("ZCR moyen")+  scale_color_manual(values=c("#5B1A18","#D67236"))


print(fig_ZCR_fricative)
largeurFigCm = 15
hauteurFigCm = 9
ggsave(fig_ZCR_fricative, filename = "R_ENLIGNE/IMAGES/ZCR_VOyS.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

largeurFigCmPetit = 15
hauteurFigCmPetit = 9

listefricativesourde = c("s", "S", "f")
fig_ZCR_fric = ggplot(regroupe_tout %>% 
                     filter(label %in% listefricativesourde & sexe_locuteur.x=="F"), 
                   #filter(label=="t" & sexe_locuteur=="F"), 
                   aes(x = classe_age, 
                       y = ZCRMoy, 
                       colour = condition)
) + 
  geom_boxplot()+
  facet_wrap(~label)+
  ggtitle("ZCR moyen en fonction de la classe d'age de la \nlocutrice par fricative sourde") +
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=12),
        legend.position="bottom")+
  scale_color_brewer(palette = "Spectral")+
  #scale_color_manual(values=c("#5B1A18","#D67236"))+
  xlab("Classe d'age")+
  ylab("ZCR moyen ")
print(fig_ZCR_fric)
ggsave(fig_ZCR_fric, filename = "R_ENLIGNE/IMAGES/ZCR_AGE_F_fric.pdf", width = largeurFigCm, height = hauteurFigCm, unit = "cm")

t.test(parametres_acoustiques_enligne$duration.s., parametres_acoustiques_micro$`duration(s)`)
t.test(parametres_acoustiques_enligne$age_locuteur, parametres_acoustiques_micro$age_locuteur)
#on rrejette H0 différences non dûes au hasard


# Si le label suivant du label en cours est <p:> et que le label 
# précédent du label suivant est <p:> et les deux labels appartiennent à la même phrase :
#   la longueur de la pause est égale au temps auquel commence le label suivant - le temps auquel le label en cours finit
##############################################


fig_COG_fricative = ggplot(parametres_acoustiques_enligne %>% 
                           #filter(lieu != "centrale" & lieu != "posterieure"),#%>% 
                             filter(mode == "fricative", voisement == "sourde"),
                           aes(x = classe_age, 
                               y = mean_CoG.Hz., 
                               color = sexe_locuteur)) + 
  geom_boxplot(alpha = .5)+
  facet_wrap( ~ lieu)+
  ggtitle("CGS moyen en fonction de la classe d'age du \nlocuteur pour les consonnes fricatives sourdes") +
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=12),
        legend.position="bottom")+
  xlab("Classe d'age")+
  ylab("CGS moyen")+
  scale_color_manual(values=c("#5B1A18","#D67236"))

print(fig_COG_fricative)
ggsave(fig_COG_fricative, filename = "R_ENLIGNE/IMAGES/COG_fricative.pdf", width = largeurFigCm, height = hauteurFigCm, units = "cm")


fig_COG = ggplot(parametres_acoustiques_enligne %>% 
                             #filter(lieu != "centrale" & lieu != "posterieure"),#%>% 
                             filter(mode == "fricative", voisement == "sourde"),
                             #filter(nasalite == "orale", statutCV=="V"),
                           aes(x = age_locuteur, 
                               y = mean_CoG.Hz., 
                               color = sexe_locuteur)) + 
  geom_smooth(method = "loess")+
  facet_wrap( lieu~ label)+
  ggtitle("CGS moyen en fonction de la classe d'age du \nlocuteur pour les consonnes fricatives sourdes") +
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size=12),
        legend.position="bottom")+
  xlab("Classe d'age")+
  ylab("CGS moyen")+
  scale_color_manual(values=c("#5B1A18","#D67236"))
fig_COG
ggsave(fig_COG, filename = "R_ENLIGNE/IMAGES/COG_fricative_curve.pdf", width = largeurFigCm, height = hauteurFigCm, units = "cm")




###################### CREATE FILE FOR CLASSIFICATION #######################


F0_donneesenligne = parametres_acoustiques_enligne %>%
  filter(statutCV == "V" & nasalite =="orale")%>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(F0moy = round(mean(mean_F0.Hz., na.rm = T)),
            F0sd = round(mean(std_dev_F0.Hz., na.rm = T), 2))#%>%
#View(F0_donneesenligne)

F1_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label == "a")%>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(F1moy = round(mean(bark(mean_F1.Hz., na.rm = T)), 1),
            F1sd = round(mean(std_dev_F1.Hz., na.rm = T), 2))
#View(F1_donneesenligne)

F2_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label == "a")%>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(F2moy = round(mean(bark(mean_F2.Hz., na.rm = T)), 1),
            F2sd = round(mean(std_dev_F2.Hz., na.rm = T), 2))
#View(F2_donneesenligne)

F3_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label == "a")%>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(F3moy = round(mean(bark(mean_F3.Hz., na.rm = T)), 1),
            F3sd = round(mean(std_dev_F3.Hz., na.rm = T), 2))
#View(F3_donneesenligne)

CGS_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label %in% c("f","s"))%>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(CGSmoy = round(mean(mean_CoG.Hz., na.rm = T)),
            CGSsd = round(mean(std_dev_CoG.Hz., na.rm = T), 2))
#View(CGS_donneesenligne)

dureefric_donneesenligne = parametres_acoustiques_enligne %>%
  filter(mode == "fricative" & voisement =="sourde") %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(DureeFricmoy = round(mean(duration.s., na.rm = T), 3))
#View(dureefric_donneesenligne)

dureeVoy_donneesenligne = parametres_acoustiques_enligne %>%
  filter(statutCV == "V" & nasalite =="orale") %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(DureeVoymoy = round(mean(duration.s., na.rm = T), 3))
#View(dureeVoy_donneesenligne)

totalduree_donneesenligne = parametres_acoustiques_enligne %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(totalDurmoy = round(mean(`totalduration(s)`, na.rm = T), 3))
#View(totalduree_donneesenligne)

ZCRvoy_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label %in% c("i", "y")) %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(ZCRiyMoy = round(mean(mean_ZCR, na.rm = T), 1),
            ZCRiysd = round(mean(std_dev_ZCR, na.rm = T)))
#View(ZCRvoy_donneesenligne)

ZCRFricSon_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label %in% c("z", "Z")) %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(ZCRzZMoy = round(mean(mean_ZCR, na.rm = T), 1),
            ZCRzZsd = round(mean(std_dev_ZCR, na.rm = T)))
#View(ZCRFricSon_donneesenligne)

ZCRFricSou_donneesenligne = parametres_acoustiques_enligne %>%
  filter(label %in% c("f", "s")) %>%
  group_by(nom_locuteur, textgrid_file, age_locuteur, sexe_locuteur, classe_age) %>%
  summarise(ZCRfsmoy = round(mean(mean_ZCR, na.rm = T), 1),
            ZCRfssd = round(mean(std_dev_ZCR, na.rm = T)))
#View(ZCRFricSou_donneesenligne)


fileforclass = F0_donneesenligne %>% 
  left_join(F1_donneesenligne) %>% 
  left_join(F2_donneesenligne) %>%
  left_join(F3_donneesenligne) %>%
  left_join(CGS_donneesenligne) %>%
  left_join(dureefric_donneesenligne) %>%
  left_join(dureeVoy_donneesenligne) %>%  
  left_join(totalduree_donneesenligne) %>%
  left_join(ZCRvoy_donneesenligne) %>%
  left_join(ZCRFricSon_donneesenligne) %>%
  left_join(ZCRFricSou_donneesenligne) 

fileforclass[is.na(fileforclass)] <- 0
write.csv(fileforclass, file = "descripteursphonetiques.csv")
fileforclass_femme =fileforclass %>%filter(sexe_locuteur=="F")
fileforclass_homme =fileforclass %>%filter(sexe_locuteur=="M")

write.csv(fileforclass_femme, file = "../descripteursphonetiques_femme.csv")
write.csv(fileforclass_homme, file = "../descripteursphonetiques_homme.csv")


diff(fileforclass$F0moy,lag=1)
length(diff(c(0,fileforclass$F0moy),lag=1))
derivee_df = matrix(unlist(lapply(donnees, diff)), nrow = nrow(donnees)-1)
colnames(derivee_df) = paste0("d", colnames(donnees))
derivee_df = matrix(unlist(lapply(fileforclass, diff)), nrow = nrow(fileforclass)-1)
colnames(derivee_df) = paste0("d", colnames(donnees))

fichiermfcc = read.delim("../descripteurs_moy_sd_values.csv", sep = ",")

combineall = fichiermfcc %>% 
  left_join(fileforclass)
write.csv(combineall, file = "../tousdescripteurs.csv")


donneesfemmes = combineall %>% filter(sexe_locuteur=="F")
write.csv(donneesfemmes, file = "../tousdescripteurs_femme.csv")

donneeshommes = combineall %>% filter(sexe_locuteur=="M")
write.csv(donneeshommes, file = "../tousdescripteurs_homme.csv")

