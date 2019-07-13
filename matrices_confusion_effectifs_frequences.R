setwd("/Users/audreygombault/MASTER/M2STAGE/DONNEES/R/")

fichier_donnees = "WEKA/J48/extraction_phonetique_homme_better.csv"
donnees = read_csv(fichier_donnees)
donnees$ref_num = sapply(
  donnees$actual,
  function(x) str_split(x,":")[[1]][1]
)
donnees$ref_classe = sapply(
  donnees$actual,
  function(x) str_split(x,":")[[1]][2]
)
donnees$reponse = sapply(
  donnees$predicted,
  function(x) str_split(x,":")[[1]][1]
)
donnees$reponse_classe = sapply(
  donnees$predicted,
  function(x) str_split(x,":")[[1]][2]
)
colonne_classe_reference = "ref_classe"
colonne_classe_predite = "reponse_classe"
#fichier_resultats_effectifs = "WEKA/matrice_confusion_effectifs_extractionSMO_all.csv"
fichier_resultats_frequences = "WEKA/J48/matrice_confusion_frequences_extraction_J48_hommes.csv"
inclure_colonne_Ntotal_par_classe = T

export_frequences_en_pourcentages = T
precision_pourcentages = 1 # 1 = arrondi au pourcent, .1 = 1 décimale, .01 = 2 décimales, etc.
separateur_decimales = ","

library(tidyverse)
library(scales)
definir_ordre_apparition_classes = TRUE
ordre_apparition_classes = c("15-19","20-29", "30-39", "40-49","50-59", "60-69")

matrice_confusion_effectifs = donnees
if(definir_ordre_apparition_classes){
  matrice_confusion_effectifs = matrice_confusion_effectifs %>%
    mutate_at(c(colonne_classe_reference, colonne_classe_predite), factor, levels = ordre_apparition_classes)
}
#donnees = read_csv(fichier_donnees)

# matrice de confusion avec le nombre d'occurrences
matrice_confusion_effectifs = donnees %>%
  # nombre d'occurrences pour chaque combinaison référence * prédiction
  group_by_at(.vars = c(colonne_classe_reference, colonne_classe_predite)) %>%
  summarise(n = n()) %>% 
  # catégories prédites en colonnes
  spread(colonne_classe_predite, n) %>%
  # catégories de référence comme noms de lignes (permet de simplifier la conversion en fréquences)
  column_to_rownames(var = "ref_classe") %>% 
  # on remplace les NA (combinaisons non-utilisées) par 0
  replace(., is.na(.), 0)

# conversion fréquences (pourcentage pour chaque catégorie dans les données en entrée)
N_total_par_classe_reference = rowSums(matrice_confusion_effectifs)
matrice_confusion_frequences = matrice_confusion_effectifs / N_total_par_classe_reference
# affichage optionnel sous forme de pourcentages
matrice_confusion_frequences_formatte = matrice_confusion_frequences
if(export_frequences_en_pourcentages){
  matrice_confusion_frequences_formatte[] = lapply(matrice_confusion_frequences, percent, accuracy = precision_pourcentages, decimal.mark = separateur_decimales)
}# optionnel : inclure une colonne avec le nombre total d'occurrences par classe

if(inclure_colonne_Ntotal_par_classe) {
  matrice_confusion_effectifs$N_total = N_total_par_classe_reference
  matrice_confusion_frequences_formatte$N_total = N_total_par_classe_reference
}
# export dans des fichiers TSV
#write_tsv(matrice_confusion_effectifs %>% rownames_to_column(var = "Référence"), path = fichier_resultats_effectifs)
write_tsv(matrice_confusion_frequences_formatte %>% rownames_to_column(var = "Référence"), path = fichier_resultats_frequences)




#df <- data.frame(matrix(ncol = 2, nrow = 1))
classification <- c("JRIP_phonetique", "JRIP_MFCC", "JRIP_combinaison")
taux = c(64.25,84.25 , 84.5)
df <- data.frame(classification, taux)

hauteurCm = 6
pdf("WEKA/JRIP/comparaison_desc.pdf", height = hauteurCm)
b=barplot(taux, xpd=FALSE,ylim = c(50,90), beside = TRUE)
axis(side=1, at=b,labels=classification)
box(bty="l")
dev.off()


classification <- c("JRIP_phon_T", "JRIP_phon_F", "JRIP_phon_H", "JRIP_MFCC_T", "JRIP_MFCC_F","JRIP_MFCC_H", "JRIP_tout_T", "JRIP_tout_F", "JRIP_tout_H")
taux = c(62, 63.00, 65.5,76,83.5, 85.00 ,76.5, 84.5,84.5)
df <- data.frame(classification, taux)

hauteurCm = 6
pdf("WEKA/JRIP/comparaison_desc.pdf", height = hauteurCm)
b=barplot(taux, xpd=FALSE,ylim = c(50,90), beside = TRUE)
axis(side=1, at=b,labels=classification)
box(bty="l")
dev.off()

hauteurCm = 6

pdf("WEKA/JRIP/comparaison_desc.pdf", height = hauteurCm)
b=barplot(taux, xpd=FALSE,ylim = c(50,90), beside = TRUE, col = couleurs, ylab="Score de classification de l'age (%)")
axis(side=1, at=b, labels = classification, las=2, cex.axis=0.7)
box(bty="l")
dev.off()

classification <- c("J48_phon_T", "J48_phon_F", "J48_phon_H", "J48_MFCC_T", "J48_MFCC_F","J48_MFCC_H", "J48_tout_T", "J48_tout_F", "J48_tout_H")
taux = c(58.3, 63.2, 60.6, 77, 81, 85.9 ,78, 83,86.7)
pdf("WEKA/J48/comparaison_desc.pdf", height = hauteurCm)
b=barplot(taux, xpd=FALSE,ylim = c(50,90), beside = TRUE, col = couleurs, ylab="Score de classification de l'age (%)")
axis(side=1, at=b, labels = classification, las=2, cex.axis=0.7)
box(bty="l")
dev.off()

classification2 <- c("J48_phonetique", "J48_MFCC", "J48_tout")
taux2 = c(96, 86, 94)
pdf("WEKA/J48/classification_sexe.pdf")
b=barplot(taux2, xpd=FALSE,ylim = c(70,100), beside = TRUE, col = couleurs, ylab="Score de classification du sexe (%)")
axis(side=1, at=b, labels = classification2)
box(bty="l")
dev.off()

couleurs = c("#999999","#666666", "#333333", "#999999","#666666", "#333333", "#999999","#666666", "#333333")




classification3 <- c("SMO_phon_T", "SMO_phon_F", "SMO_phon_H", "SMO_MFCC_T", "SMO_MFCC_F","SMO_MFCC_H", "SMO_tout_T", "SMO_tout_F", "SMO_tout_H")
taux3 = c(59.7, 63.45, 63, 87, 92, 98, 87, 94, 98)
pdf("WEKA/SMO/comparaison_desc.pdf", height = hauteurCm)
b=barplot(taux3, xpd=FALSE,ylim = c(50,100), beside = TRUE, col = couleurs, ylab="Score de classification de l'age (%)")
axis(side=1, at=b, labels = classification3, las=2, cex.axis=0.7)
box(bty="l")
dev.off()

classification_sexe <- c("SMO_phonetique", "SMO_MFCC", "SMO_tout")
taux_sexe = c(96.77, 94.6, 98.96)
pdf("WEKA/SMO/classification_sexe.pdf")
b=barplot(taux_sexe, xpd=FALSE,ylim = c(80,100), beside = TRUE, col = couleurs, ylab="Score de classification du sexe (%)")
axis(side=1, at=b, labels = classification_sexe)
box(bty="l")
dev.off()

classification_sexeJrip <- c("JRIP_phonetique", "JRIP_MFCC", "JRIP_tout")
taux_sexe_Jrip = c(95.5, 85, 94)
pdf("WEKA/JRIP/classification_sexe.pdf")
b=barplot(taux_sexe_Jrip, xpd=FALSE,ylim = c(80,100), beside = TRUE, col = couleurs, ylab="Score de classification du sexe (%)")
axis(side=1, at=b, labels = classification_sexeJrip)
box(bty="l")
dev.off()


ggplot(df, aes(y = taux, x = classification)) +
    geom_bar(aes(width = .5), stat = "identity", beside =T,fill = "steelblue", ylim = c(50,90), xpd=FALSE)  #+
    #stat_identity(aes(label = as.character(x)), geom = "text", colour = "yellow", hjust = 1.75) +
  
  # on renverse equivalent à horiz = T
  #coord_flip() +
  
  # par defaut c'est gris ce qui n'est pas mal mais bon...
  #theme_bw()

          35/37 = précision
  a   b   c   d   e   f   <-- classified as
390   2   2   4  13   0 |   a = 20-29
  6  69   0   0   4   0 |   b = 40-49
 10   0  35   0   0   0 |   c = 15-19   35/45 = rappel
 32   0   0  26   2   0 |   d = 30-39
 23   0   0   0 127   0 |   e = 50-59
  1   0   0   0   0  29 |   f = 60-69

matrice_confusion_effectifs$`20-29`

colonnes = c("15-19", "20-29", "30-39", "40-49", "50-59", "60-69")


(matrice_confusion_effectifs$`20-29`[[1]]/(matrice_confusion_effectifs$`20-29`[[1]]+
                                            matrice_confusion_effectifs$`20-29`[[2]]+
                                            matrice_confusion_effectifs$`20-29`[[3]]+
                                            matrice_confusion_effectifs$`20-29`[[4]]+
                                            matrice_confusion_effectifs$`20-29`[[5]])+
matrice_confusion_effectifs$`30-39`[[2]]/(matrice_confusion_effectifs$`30-39`[[1]]+
                                            matrice_confusion_effectifs$`30-39`[[2]]+
                                            matrice_confusion_effectifs$`30-39`[[3]]+
                                            matrice_confusion_effectifs$`30-39`[[4]]+
                                            matrice_confusion_effectifs$`30-39`[[5]])+
matrice_confusion_effectifs$`40-49`[[3]]/(matrice_confusion_effectifs$`40-49`[[1]]+
                                            matrice_confusion_effectifs$`40-49`[[2]]+
                                            matrice_confusion_effectifs$`40-49`[[3]]+
                                            matrice_confusion_effectifs$`40-49`[[4]]+
                                            matrice_confusion_effectifs$`40-49`[[5]])+
matrice_confusion_effectifs$`50-59`[[4]]/(matrice_confusion_effectifs$`50-59`[[1]]+
                                            matrice_confusion_effectifs$`50-59`[[2]]+
                                            matrice_confusion_effectifs$`50-59`[[3]]+
                                            matrice_confusion_effectifs$`50-59`[[4]]+
                                            matrice_confusion_effectifs$`50-59`[[5]])+
matrice_confusion_effectifs$`60-69`[[5]]/(matrice_confusion_effectifs$`60-69`[[1]]+
                                            matrice_confusion_effectifs$`60-69`[[2]]+
                                            matrice_confusion_effectifs$`60-69`[[3]]+
                                            matrice_confusion_effectifs$`60-69`[[4]]+
                                            matrice_confusion_effectifs$`60-69`[[5]]))/5



###################### histogramme poids des descripteurs #####################



descripteur = c("F0Moy",
  "F0sd",
  "F1Moy",
  "F1sd",
  "F2Moy",
  "F2sd",
  "F3Moy",
  "F3sd",
  "CGSMoy",
  "CGSsd",
  "DurMoyFric",
  "DurMoyVoy",
  "DurTotale",
  "ZCRMoy/iy/",
  "ZCRsd/iy/",
  "ZCRMoy/zZ/",
  "ZCRsd/zZ/",
  "ZCRMoy/fs/",
  "ZCRsd/fs/")
  
taux = c(100,
  83,
  100,
  83,
  100,
  50,
  100,
  67,
  83,
  67,
  83,
  83,
  100,
  83,
  50,
  83,
  50,
  83,
  67
)

df <- data.frame(descripteur, taux)
pdf("WEKA/meilleursdesc.pdf", height = 6, width = 10)
b = barplot(df$taux,xpd=FALSE, beside = TRUE)
axis(side=1, at=b, labels = descripteur, las=2, cex.axis=0.9 )
box(bty="l")
dev.off()
