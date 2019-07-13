#traitement des extractions de weka 

setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/WEKA/")

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

fichier_sortie_weka_brut = "J48/extraction_phonetique_homme_better.csv"
fichier_sortie_weka_traite = "J48/extraction_phonetique_homme_better_traite.csv"
donnees_entree = read_csv(fichier_sortie_weka_brut)
fichier_classes_age = "../R_ENLIGNE/classeAge.txt"
classes_age = read_tsv(file = fichier_classes_age)
fichier_textgridresults = "../new_TextGrid_results.txt"
donnees_result = read.delim(fichier_textgridresults, sep="\t", header = TRUE)
donnees_entree_results = donnees_entree %>%
  left_join(donnees_result)#, by = c("fichier"="textgrid_file"))

newinfolist = str_split(donnees_entree_results$result_file,"_")
donnees_entree_results$age_locuteur = sapply(
  newinfolist,
  function(x) x[3] 
)
donnees_entree_results$sexe_locuteur = sapply(
  newinfolist,
  function(x) x[2]
)
donnees_entree_results$nom_locuteur = sapply(
  newinfolist,
  function(x) strsplit(x[4], ".", fixed=T)[[1]][1]
)
donnees_entree_results$idPhrase = sapply(
  donnees_entree_results$textgrid_file,
  function(x) strsplit(strsplit(x, "_")[[1]][2], ".", fixed=T)[[1]][1]
)
donnees_entree_results$classe_age = as.character(sapply(
  as.numeric(donnees_entree_results$age_locuteur),
  function(x) classes_age %>%
    filter(x >= Min & x <= Max) %>%
    select(Classe)
))
#fichier_sortie_weka_traite = "extractionJ48_MFCC_traite.csv"
donnees_entree_results$age_median = sapply(
  as.character(donnees_entree_results$classe_age),
  function(x) {
    (as.numeric(str_split(x, "-")[[1]][1])+as.numeric(str_split(x, "-")[[1]][2]))/2
  }
)
donnees_entree_results$age_median_predicted = sapply(
  as.character(donnees_entree_results$predicted),
  function(x) {
    (as.numeric(str_split(str_split(x, "-")[[1]][1],":")[[1]][2])+as.numeric(str_split(x, "-")[[1]][2]))/2
  }
)
donnees_entree_results_error = donnees_entree_results %>% filter(error == "+")
donnees_entree_results_error$distance_erreur = donnees_entree_results_error$age_median_predicted - donnees_entree_results_error$age_median 
summary(donnees_entree_results_error$distance_erreur)    
errorbylocphrase = donnees_entree_results_error %>% 
  group_by(nom_locuteur, idPhrase) %>% 
  summarise(distMoy = mean(distance_erreur)) %>% 
  spread(nom_locuteur, distMoy)
errorbylocphrase[is.na(errorbylocphrase)]<-0

errorbyLoc = donnees_entree_results_error %>% 
  group_by(nom_locuteur) %>% 
  summarise(distMoy = mean(distance_erreur))

errorbyPhrase = donnees_entree_results_error %>% 
  group_by(idPhrase) %>% 
  summarise(distMoy = mean(distance_erreur))# %>% 

errorbyClasse = donnees_entree_results_error %>% 
  group_by(classe_age) %>% 
  summarise(distMoy = mean(distance_erreur), distMax = max(distance_erreur), distMin = min(distance_erreur))# %>% 

distanceMoy = donnees_entree_results_error %>% group_by(actual, predicted) %>% 
  summarise(distMoy = mean(distance_erreur)) %>% 
  spread(actual, distMoy)

donnees_entree_results$distance_erreur = donnees_entree_results$age_median_predicted - donnees_entree_results$age_median
essai2 = donnees_entree_results %>% group_by(actual, predicted) %>% 
  summarise(distMoy = mean(distance_erreur)) %>% 
  spread(actual, distMoy)

obj1 =table(donnees_entree_results$distance_erreur,
      donnees_entree_results$classe_age)
obj2 = as.data.frame(obj1)
obj3 = obj2 %>% group_by(Var1, Var2) %>% spread(Var1, Freq)

ggplot(obj2,
       aes(y = Freq, x = Var1, color=Var1))+
  geom_boxplot()+
  facet_wrap(~Var2)
obj1_loc =table(donnees_entree_results$nom_locuteur,
            donnees_entree_results$distance_erreur)
obj2_loc = as.data.frame(obj1_loc)
obj3_loc = obj2_loc %>% group_by(Var1, Var2) %>% spread(Var2, Freq)

obj4_loc = obj2_loc %>%  group_by(Var1) %>% filter(Var2 != 0) %>% 
  summarise(erreurfreq = sum(Freq))
  
  
max(obj2$Freq)
print(obj2$Var2[obj2$Freq == max(obj2$Freq)])
obj5 = mean(obj4$erreurfreq)

typeof(obj2$Var2) 
min(obj2$Var2)


obj4 = obj2 %>%  group_by(Var1, Var2) %>% summarise(erreurbyloc = mean(Freq))


plot((donnees_entree_results_error$distance_erreur))
ggplot()
boxplot(donnees_entree_results_error$distance_erreur~donnees_entree_results_error$classe_age)

ggplot(donnees_entree_results,
       aes(y=prediction, x=actual, color=sexe_locuteur))+
  scale_colour_manual(values=c("#5B1A18","#D67236"))+
  geom_boxplot()

matrice_de_confusion = table(donnees_entree_results$actual,donnees_entree_results$predicted)
write.csv(matrice_de_confusion, "matrice_extractionJ48_MFCC.csv")

table(donnees_entree_results$age_locuteur,donnees_entree_results$nom_locuteur)


