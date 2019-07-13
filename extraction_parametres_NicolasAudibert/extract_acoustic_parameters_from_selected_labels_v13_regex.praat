#####################################################################################################################
# extract_acoustic_parameters_from_selected_labels_v12_regex.praat
#
# This script extracts from a set of matched sound files and textgrid objects every label
# matching a set defined in a text file with their start and end time and duration. If no text file is specified,
# all non-empty intervals are processed.
# Aligned labels on other tiers can be optionally extracted (either one selected secondary tier or all other),
# as well as previous and next labels on each target tier.
# The following acoustic parameters can be extracted on request(averaged on the whole interval and on a
# variable number of user-defined points + min, max with time and standard deviation on request):
# - F0
# - Intensity
# - Formants 1 to 4
# - Spectral center of gravity (CoG)
# - Harmonics-to-noise ratio (HNR)
# - Zero-crossing rate (ZCR)
#
# The script assumes that matched textgrid and sound files have the same name, and that all textgrids
# have the same structure.
#
# This version of the script selects .TextGrid files based on a regular expression.
#
# Author: Nicolas Audibert, LPP UMR7018, January 2011 - last modified April 2019
#####################################################################################################################

form Extract_acoustic_parameters
	comment Folder with textgrids (all textgrids must have the same structure)
	text textgrids_folder extraits_NCCFr
	comment Regular expression for filtering textgrids in folder (* = any string)
	text regexp *.TextGrid
	comment Folder with sounds (leave empty if same as textgrids folder or to extract only duration and context)
	text wavefiles_folder
	comment Output file
	text results_file resultats_analyse_acoustique.txt
	comment Index of the tier with labels to be processed
	positive reference_tier 1
	comment Path to the parameters file
	text parameters_file extract_all_parameters_default_settings.txt
	comment File with relative positions of the target points for parameters extraction
	text extraction_points_definition_file positions_5points.txt
	comment Text file that contains the labels to be processed (leave empty to process all non-empty labels)
	text dictionary_file voyelles_orales_francais_LIMSI.txt
endform

# Clear info window
clearinfo

# Get the list of textgrids in the specified folder that match the regular expression
flist = Create Strings as file list: "filelist", "'textgrids_folder$'/'regexp$'"

# Build a table with paired .TextGrid and .wav file names
nFiles = Get number of strings
flistWav = Replace all: ".TextGrid$", ".wav", 1, "regular expressions"
flistTable = Create Table with column names: "filelist", nFiles, "wav TextGrid"
for iFile from 1 to nFiles
	selectObject: flistWav
	wavFile$ = Get string: iFile
	selectObject: flist
	tgFile$ = Get string: iFile
	selectObject: flistTable
	Set string value: iFile, "wav", wavFile$
	Set string value: iFile, "TextGrid", tgFile$
endfor
removeObject: flist, flistWav
selectObject: flistTable

# Call the main script
runScript: "extract_acoustic_parameters_from_selected_labels_v13.praat", textgrids_folder$, wavefiles_folder$, results_file$, reference_tier, parameters_file$, extraction_points_definition_file$, dictionary_file$
