setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")

library(readr)
library(tidyr)
library(dplyr)
library(stringr)

fichier_entree = "R_ENLIGNE/ANALYSES/analyse_v13_tousphones_enligne_new.txt"
fichier_sortie = "R_ENLIGNE/ANALYSES/analyse_v13_tousphones_enligne_new_result.txt"
fichier_textgridresults = "new_TextGrid_results.txt"


donnees_entree = read.delim(fichier_entree, sep="\t", header = TRUE)
donnees_result = read.delim(fichier_textgridresults, sep="\t", header = TRUE)
#attach(donnees_entree)
#donnees_entree$textgrid_file
#donnees_result$textgrid_file
donnees_entree_results = donnees_entree %>%
  left_join(donnees_result, by = c("textgrid_file"="textgrid_file"))

summary(donnees_entree_results$result_file)
write_tsv(donnees_entree_results, path = fichier_sortie, na = "None")
