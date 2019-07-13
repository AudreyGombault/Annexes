library(tidyverse)
library(matrixStats)

setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/MFCC")
#fichier = "0062d5424f33aca374a58ab50d7e6f24_phrase01.txt"
#fichiers = c("0062d5424f33aca374a58ab50d7e6f24_phrase01.txt", "0062d5424f33aca374a58ab50d7e6f24_phrase02.txt", "0062d5424f33aca374a58ab50d7e6f24_phrase03.txt")
fichiers = dir(pattern = "*.txt")
lignes = list()
for (iFichier in 1:length(fichiers)) {
  fichier = fichiers[iFichier]
  
  donnees = read_tsv(fichier)
  
  print(fichier)
  #lignes[[iFichier]] = descripteurs_phrase
  moy_donnees = colMeans(donnees)
  names(moy_donnees) =  paste0("moy_", colnames(donnees))
  sd_donnees = colSds(as.matrix(donnees))
  names(sd_donnees) = paste0("sd_", colnames(donnees))
  
  # calcul dérivées
  derivee_df = matrix(unlist(lapply(donnees, diff)), nrow = nrow(donnees)-1)
  colnames(derivee_df) = paste0("d", colnames(donnees))
  
  moy_derivee = colMeans(derivee_df)
  names(moy_derivee) =  paste0("moy_", colnames(derivee_df))
  sd_derivee = colSds(derivee_df)
  names(sd_derivee) = paste0("sd_", colnames(derivee_df))
  
  descripteurs_phrase = c(moy_donnees, sd_donnees, moy_derivee, sd_derivee)
  descripteurs_phrase = as.data.frame(t(descripteurs_phrase))
  
  descripteurs_phrase$textgrid_file = fichier
  descripteurs_phrase = descripteurs_phrase %>% separate(textgrid_file, into = c("nom_locuteur", "idPhrase"), extra = "drop",sep = "[/_\\.]")
  descripteurs_phrase$textgrid_file =  paste0((str_split(fichier, ".txt")[[1]][1]), ".TextGrid")
  lignes[[iFichier]] = descripteurs_phrase
}

descripteurs_df = bind_rows(lignes)
descripteurs_df = descripteurs_df %>% select(nom_locuteur, idPhrase, everything())
write_csv(descripteurs_df, path = "../descripteurs_moy_sd_values.csv")

fichier_textgridresults = "../R/new_TextGrid_results.txt"
donnees_result = read.delim(fichier_textgridresults, sep="\t", header = TRUE)

liste_classe = c("15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")


#descripteurs_df = descripteurs_df %>%
#  left_join(donnees_result, by = c("textgrid_file"="textgrid_file"))
data = str_split(donnees_result$result_file, "_")

donnees_result$age_locuteur = sapply(
  data,
  function(x) x[3] 
)
donnees_result$sexe = sapply(
  data,
  function(x) x[2] 
)

fichier_classes_age = "../R/R_MIC/classeAge.txt"

classes_age = read_tsv(file = fichier_classes_age)

donnees_result$classe_age = as.character(sapply(
  as.numeric(donnees_result$age_locuteur),
  function(x) 
    classes_age %>% 
    filter( x >= Min & x <= Max) %>%
    select(Classe)
))

descripteurs_df = descripteurs_df %>%
  left_join(donnees_result, by = c("textgrid_file"="textgrid_file"))
write_csv(descripteurs_df, path = "../descripteurs_moy_sd_values.csv")

# ENLIGNE_fichier_parametres_acoustiques_niveau_phone = "../R/R_ENLIGNE/ANALYSES/analyse_v13_tousphones_ENLIGNE_InfosLoc.txt"
# 
# recupclasse = read_tsv(ENLIGNE_fichier_parametres_acoustiques_niveau_phone)
# descripteurs_df = descripteurs_df %>%
#   left_join(recupclasse$classe_age, by = c("fichier"="result_file"))



test_homme = descripteurs_df %>%  filter(sexe =="M")
write.csv(test_homme, file = "../descripteursMFCC_homme.csv")
test_femme = descripteurs_df %>%  filter(sexe =="F")
write.csv(test_femme, file = "../descripteursMFCC_femme.csv")
