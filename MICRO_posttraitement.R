setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")

library(readr)
library(stringr)
library(dplyr)

fichier_sortie_praat_brut = "R_MIC/ANALYSES/analyse_v13_tousphones_micro.txt"
fichier_classes_age = "R_MIC/classeAge.txt"
fichier_sortie_praat_posttraite = "R_MIC/ANALYSES/analyse_v13_tousphones_micro_InfosLoc.txt"

donnees = read_tsv(file = fichier_sortie_praat_brut, na = c("","NA","--undefined--"))
classes_age = read_tsv(file = fichier_classes_age)
#attach(classe_age)


noms_textgrids_recodes = str_replace(donnees$textgrid_file, "_reduced", "")
infos_locuteurs_list = str_split(noms_textgrids_recodes, "_")

fichier_textgridresults = "new_TextGrid_results_micro.txt"

donnees_result = read.delim(fichier_textgridresults, sep="\t", header = TRUE)
noms_textgrid_recodes_result = str_replace(donnees_result$textgrid_file, "_reduced", "")

donnees_result$textgrid_file = sapply(
  noms_textgrid_recodes_result,
  function(x) as.character(x[1])
)

donnees$textgrid_file = sapply(
  noms_textgrids_recodes,
  function(x) as.character(x[1])
)
donnees = donnees %>%
  left_join(donnees_result, by = c("textgrid_file"="textgrid_file"))

donnees$age_locuteur = sapply(
  infos_locuteurs_list,
  function(x) x[1]
  )

donnees$sexe_locuteur = sapply(
  infos_locuteurs_list,
  function(x) x[2]
)

donnees$nom_locuteur = sapply(
  infos_locuteurs_list,
  function(x) paste0(x[1], "_", x[2], "_", x[3])
)

donnees$idPhrase = sapply(
  infos_locuteurs_list,
  function(x) strsplit(x[4], ".", fixed=T)[[1]][1]
)

donnees$classe_age = as.character(sapply(
  as.numeric(donnees$age_locuteur),
  function(x) classes_age %>%
    filter(x >= Min & x <= Max) %>%
    select(Classe)
))

# export du fichier posttraite
write_tsv(donnees, path = fichier_sortie_praat_posttraite)

