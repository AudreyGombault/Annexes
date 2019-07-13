import numpy, path, os, csv, praatio
from python_speech_features import mfcc
from python_speech_features import logfbank
import scipy.io.wavfile as wav
import python_speech_features
from praatio import tgio

path_to_textgrid = "TEXTGRIDS_ENLIGNE/"

folderpath = "donnees_enligne/"
for path, dirs, files in os.walk(folderpath):
    for filenames in files :
        if filenames.endswith(".wav"):
            (rate,sig) = wav.read(folderpath+filenames)
            for chemins, directions, fichiers in os.walk(path_to_textgrid):
                for nomfichiers in fichiers:
                    if ".DS_Store" not in nomfichiers:
                        if nomfichiers.split('.')[0] == filenames.split('.')[0]:
                            tg = tgio.openTextgrid(path_to_textgrid+nomfichiers)
                            entryList = tg.tierDict["KAN-MAU"].entryList # Get all intervals
                            startpoint = entryList[0][0]
                            startpoint1 = (startpoint*rate)
                            endpoint = entryList[-1][1]
                            endpoint1 = (endpoint*rate)
                            mfcc = python_speech_features.base.mfcc(sig[int(startpoint1):int(endpoint1)], samplerate=rate, numcep=19, nfilt=38, nfft = 1200)# winlen = 0.025, winstep = 0.010 = 10ms
                            numpy.savetxt("MFCC/{}.txt".format(filenames.split(".")[0]), mfcc, delimiter="\t", header="c0\tc1\tc2\tc3\tc4\tc5\tc6\tc7\tc8\tc9\tc10\tc11\tc12\tc13\tc14\tc15\tc16\tc17\tc18", comments='')
