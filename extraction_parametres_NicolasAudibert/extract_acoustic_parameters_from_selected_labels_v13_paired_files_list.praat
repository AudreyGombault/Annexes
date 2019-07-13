#####################################################################################################################
# extract_acoustic_parameters_from_selected_labels_v12_fileslist.praat
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
# This version of the script selects .TextGrid files listed in the input file.
#
# Author: Nicolas Audibert, LPP UMR7018, January 2011 - last modified January 2019
#####################################################################################################################

form Extract_acoustic_parameters
	comment Folder with textgrids (all textgrids must have the same structure)
	text textgrids_folder /Users/audreygombault/MASTER/M2STAGE/DONNEES/TEXTGRIDS_MICRO
	comment Text file with the list of paired .wav and .TextGrid files to be processed
	sentence paired_files_list ../fichiersapparies_micro.txt
	comment Folder with sounds (leave empty if same as textgrids folder or to extract only duration and context)
	text wavefiles_folder /Users/audreygombault/MASTER/M2STAGE/DONNEES/donnees_micro/
	comment Output file
	text results_file analyse_v13.txt
	comment Index of the tier with labels to be processed
	positive reference_tier 3
	comment Path to the parameters file
	text parameters_file extract_all_parameters_default_settings.txt
	comment File with relative positions of the target points for parameters extraction
	text extraction_points_definition_file positions_3points.txt
	comment Text file that contains the labels to be processed (leave empty to process all non-empty labels)
	text dictionary_file ../touslesphones_fr_SAMPA.txt
endform

# Clear info window
clearinfo

# Read the list of textgrids from the specified file
flistTable = Read Table from tab-separated file: paired_files_list$
wavColIndex = Get column index: "wav"
tgColIndex = Get column index: "TextGrid"

# Check that columns "wav" and "TextGrid" both exist
if wavColIndex>0 and tgColIndex>0
	# Call the main script
	runScript: "extract_acoustic_parameters_from_selected_labels_v13.praat", textgrids_folder$, wavefiles_folder$, results_file$, reference_tier, parameters_file$, extraction_points_definition_file$, dictionary_file$
else
	appendInfoLine: "File ", paired_files_list$, " does not have columns named wav and TextGrid. Aborting."
endif
