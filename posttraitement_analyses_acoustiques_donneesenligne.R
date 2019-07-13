setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")

fichier_sortie_praat_brut = "R_ENLIGNE/ANALYSES/analyse_v13_COMP_result.txt"
fichier_classes_age = "R_ENLIGNE/classeAge.txt"
fichier_sortie_praat_posttraite = "R_ENLIGNE/ANALYSES/analyse_v13_COMP_InfosLoc.txt"

library(readr)
library(stringr)
library(dplyr)

donnees2 = read_tsv(file = fichier_sortie_praat_brut, na = c("","NA","--undefined--"))
classes_age = read_tsv(file = fichier_classes_age)

noms_textgrids_recodes = str_replace(donnees2$textgrid_file, "_reduced", "")
infos_locuteurs_list = str_split(noms_textgrids_recodes, "_")
newinfolist = str_split(donnees2$result_file,"_")
donnees2$age_locuteur = sapply(
  newinfolist,
  function(x) x[3] 
  )

donnees2$sexe_locuteur = sapply(
  newinfolist,
  function(x) x[2]
)
donnees2$nom_locuteur = sapply(
  newinfolist,
  function(x) strsplit(x[4], ".", fixed=T)[[1]][1]
)

donnees2$idPhrase = sapply(
  infos_locuteurs_list,
  function(x) strsplit(x[2], ".", fixed=T)[[1]][1]
)

donnees2$classe_age = as.character(sapply(
  as.numeric(donnees2$age_locuteur),
  function(x) classes_age %>%
    filter(x >= Min & x <= Max) %>%
    select(Classe)
))

write_tsv(donnees2, path = fichier_sortie_praat_posttraite)

