import csv, os

def ajoutcolonne(reader, outputfile):
    """
    fonction qui permet d'ajouter l'information du fichier result dans une nouvelle colonne du fichier de sortie praat.
    args :  le lecteur de csv du fichier de dortie PRAAT, le nom de fichier de sortie
    sortie : aucune
    """
    liste=[]
    row0 = next(reader)
    row0.append('fichier_result')
    liste.append(row0)
    folder_path = "/Users/audreygombault/MASTER/M2STAGE/DONNEES/donnees_enligne/"
    for row in reader:
        dirname = (row[0].split("_")[0]) # On récupère le nom du locuteur qui sera également dans le nom de fichier results associé
        with open('R_ENLIGNE/ANALYSES/{}.txt'.format(outputfile),"w") as w:
            writer = csv.writer(w, delimiter="\t")
            for path, dirs, files in os.walk(folder_path): # on détermine le chemin, les répertoires et les fichiers du répertoire results
                for filename in files:
                    if dirname in filename and "results" in filename:
                        print(dirname+"\n")
                        row.append(filename)
                        liste.append(row)
            writer.writerows(liste)
    w.close()    

nomfichier_entree = input("Entrez le chemin vers votre fichier: ")
# analyse_version_labels_provenancedonnées.txt

fichier = open(nomfichier_entree,'r')
fichier_reader = csv.reader(fichier , delimiter="\t")

nom_fichier_sortie = input("choisissez le nom du fichier de sortie (sans l'extension): ")
# analyse_version_labels_provenancedonnées_result.txt

ajoutcolonne(fichier_reader, nom_fichier_sortie)    