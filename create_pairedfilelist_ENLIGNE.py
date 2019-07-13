#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May  3 11:54:13 2019
@author: audreygombault
Script qui crée un fichier texte comprenant le noms des fichiers wav et le fichier textgrid associé
afin de pouvoir utiliser le script Praat d'extraction de paramètres acoustiques de Mr Nicolas Audibert
"""
import os
folder_path = "../TEXTGRIDS_ENLIGNE/"
file = open("fichiersapparies_enligne1.txt", "w")
file.write("wav\tTextGrid\n")
for path, dirs, files in os.walk(folder_path): 
    for filenames in files: # pour tous les noms de fichiers 
        if ".DS_Store" not in filenames:
            wavfile = filenames.split('.')[0]+".wav"
            file.write(wavfile)
            file.write('\t')
            file.write(filenames)
            file.write("\n")
    file.close()