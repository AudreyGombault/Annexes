#####################################################################################################################
# extract_acoustic_parameters_from_selected_labels_v12.praat
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
# The script assumes that all textgrids have the same structure.
#
# This version of the script assumes that a Table object with the list of WAV and TextGrid files to be processed is selected.
# It is typically called by either extract_acoustic_parameters_from_selected_labels_v12_regex.praat or extract_acoustic_parameters_from_selected_labels_v12_fileslist.praat.
#
# Author: Nicolas Audibert, LPP UMR7018, January 2011 - last modified January 2019
#####################################################################################################################

form Extract_acoustic_parameters
	comment Folder with textgrids (all textgrids must have the same structure)
	text textgrids_folder 
	comment Folder with sounds (leave empty if same as textgrids folder or to extract only duration and context)
	text wavefiles_folder
	comment Output file
	text results_file 
	comment Index of the tier with labels to be processed
	positive reference_tier 
	comment Path to the parameters file
	text parameters_file extract_all_parameters_default_settings.txt
	comment File with relative positions of the target points for parameters extraction
	text extraction_points_definition_file 
	comment Text file that contains the labels to be processed (leave empty to process all non-empty labels)
	text dictionary_file 
endform

# Clear info window
clearinfo

# Get the list of textgrids (selected Strings object)
flist = selected("Table")
ntextgrids = Get number of rows

# Check values of parameters defined in form
if wavefiles_folder$ = ""
	wavefiles_folder$ = textgrids_folder$
endif
if dictionary_file$ = ""
	filter_labels = 0
else
	filter_labels = 1
	# Get list of segments to be extracted from reference tier
	stringsDictionary = Read Strings from raw text file: dictionary_file$
	dictionarySize = Get number of strings
endif

# Read the external parameters file
parametersTable = Read Table from tab-separated file: parameters_file$
nParameters = Get number of rows
for iParam from 1 to nParameters
	currentParamName$ = Get value: iParam, "Parameter"
	currentParamType$ = Get value: iParam, "Type"
	if currentParamType$ = "Txt"
		currentParamName$ = currentParamName$ + "$"
		'currentParamName$' = Get value: iParam, "Value"
	elsif currentParamType$ = "Num"
		'currentParamName$' = Get value: iParam, "Value"
	else
		appendInfoLine: "File ",parameters_file$ ,", line ", iParam+1," - unknown type for parameter ", currentParamName$
	endif
endfor
removeObject: parametersTable

# Get the names and relative position of the points in a Matrix object
extracted_points_relative_times = Read Matrix from raw text file: extraction_points_definition_file$
n_extracted_points = Get number of rows

# Write the results file header
#fileappend 'results_file$' textgrid_file'tab$'label'tab$'previousLabel'tab$'followingLabel'tab$'start'tab$'end'tab$'duration(s)'tab$'mean_f0(Hz)'tab$'f0_point1(Hz)'tab$'f0_point2(Hz)'tab$'f0_point3(Hz)'newline$'
writeFile: results_file$, "textgrid_file", tab$, "label", tab$, "start_time", tab$,"end_time", tab$, "duration(s)"
if extract_left_and_right_context
	appendFile: results_file$, tab$, "previousLabel", tab$, "followingLabel"
endif
if extract_F0
	appendFile: results_file$, tab$, "mean_F0(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "F0_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_F0(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_F0(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_F0(Hz)", tab$, "min_F0_relative_time", tab$, "max_F0(Hz)", tab$, "max_F0_relative_time"
	endif
endif
if extract_intensity
	appendFile: results_file$, tab$, "mean_intensity(dB)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "intensity_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(dB)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_intensity(dB)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_intensity(dB)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_intensity(dB)", tab$, "min_intensity_relative_time", tab$, "max_intensity(dB)", tab$, "max_intensity_relative_time"
	endif
endif
if extract_CoG
	appendFile: results_file$, tab$, "mean_CoG(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "CoG_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_CoG(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_CoG(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_CoG(Hz)", tab$, "min_CoG_relative_time", tab$, "max_CoG(Hz)", tab$, "max_CoG_relative_time"
	endif
endif
if extract_HNR
	appendFile: results_file$, tab$, "mean_HNR(dB)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "HNR_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(dB)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_HNR(dB)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_HNR(dB)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_HNR(dB)", tab$, "min_HNR_relative_time", tab$, "max_HNR(dB)", tab$, "max_HNR_relative_time"
	endif
endif
if extract_ZCR
	appendFile: results_file$, tab$, "mean_ZCR"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "ZCR_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_ZCR"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_ZCR"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_ZCR", tab$, "min_ZCR_relative_time", tab$, "max_ZCR", tab$, "max_ZCR_relative_time"
	endif
endif
if extract_formants
	appendFile: results_file$, tab$, "mean_F1(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "F1_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_F1(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_F1(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_F1(Hz)", tab$, "min_F1_relative_time", tab$, "max_F1(Hz)", tab$, "max_F1_relative_time"
	endif
	appendFile: results_file$, tab$, "mean_F2(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "F2_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_F2(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_F2(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_F2(Hz)", tab$, "min_F2_relative_time", tab$, "max_F2(Hz)", tab$, "max_F2_relative_time"
	endif
	appendFile: results_file$, tab$, "mean_F3(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "F3_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_F3(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_F3(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_F3(Hz)", tab$, "min_F3_relative_time", tab$, "max_F3(Hz)", tab$, "max_F3_relative_time"
	endif
	appendFile: results_file$, tab$, "mean_F4(Hz)"
	for ipoint from 1 to n_extracted_points
		selectObject: extracted_points_relative_times
		current_point_relative_postition = Get value in cell: ipoint, 1
		appendFile: results_file$, tab$, "F4_pt"+fixed$(ipoint,0)+"_"+fixed$(round(100*current_point_relative_postition),0)+"%(Hz)"
	endfor
	if get_median
		appendFile: results_file$, tab$, "median_F4(Hz)"
	endif
	if get_standard_deviation
		appendFile: results_file$, tab$, "std_dev_F4(Hz)"
	endif
	if get_min_max_with_time
		appendFile: results_file$, tab$, "min_F4(Hz)", tab$, "min_F4_relative_time", tab$, "max_F4(Hz)", tab$, "max_F4_relative_time"
	endif
endif

# The rest of the results file header will be written only when processing the first textgrid
header_written = 0
# Init ntiers to 0 before actual number of tiers is known
ntiers = 0

# fileappend "'results_file$'" 'newline$'

# Loop every selected textgrid
for itextgrid to ntextgrids
	# Get its name, display it in 'info' windows and read it
	selectObject: flist
	tg$ = Get value: itextgrid, "TextGrid"
	appendInfoLine: "Processing file ", tg$, "..."
	current_tg = Read from file: "'textgrids_folder$'/'tg$'"
	ntiers = Get number of tiers

	if header_written = 0
		# Finish writing results file header on the first loop increment
		selectObject: current_tg
		if secondary_tier>0
			# Get the name of the selected secondary tier
			selectObject: current_tg
			tiername$ = Get tier name: secondary_tier
			appendFile: results_file$, tab$, tiername$
			if extract_left_and_right_context
				appendFile: results_file$, tab$, "previous_'tiername$'", tab$, "next_'tiername$'"
			endif
		elsif secondary_tier = -1
			# Get the names of all interval tiers
			for itier from 1 to ntiers
				# Ignore it if it's the reference tier (already processed) or a point tier (no labels to extract)
				selectObject: current_tg
				interv_tier = Is interval tier: itier
				if itier<>reference_tier and interv_tier=1
					# Get tier name and write it to results file
					selectObject: current_tg
					tiername$ = Get tier name: itier
					appendFile: results_file$, tab$, tiername$
					if extract_left_and_right_context
						appendFile: results_file$, tab$, "previous_'tiername$'", tab$, "next_'tiername$'"
					endif
				endif
			endfor
		endif
		# Append a linebreak to results file to finish writing the header
		appendFile: results_file$, newline$
		header_written = 1
	endif

	# Read corresponding sound if at least one type of acoustic analysis is selected
	if extract_F0+extract_intensity+extract_formants+extract_CoG+extract_HNR+extract_ZCR>0
		selectObject: flist
		snd$ = Get value: itextgrid, "wav"
		current_snd = Open long sound file: "'wavefiles_folder$'/'snd$'"
		current_snd_total_duration = Get total duration
	endif

	# Extract info from every non-empty interval
	selectObject: current_tg
	ninterv = Get number of intervals: reference_tier
	# Loop every interval on reference tier
	for iinterv from 1 to ninterv
		selectObject: current_tg
		label$ = Get label of interval: reference_tier, iinterv
		# Do something only if the interval label is not empty and matches the set of symbols to be processed (if defined)
		if length(label$)>0
			# Check if the current label is included in the dictionary
			foundString = 0
			if filter_labels
				if dictionarySize > 0
					currentStringIndex = 1
					while foundString = 0 and currentStringIndex <= dictionarySize
						selectObject: stringsDictionary
						currentString$ = Get string: currentStringIndex
						if label$ = currentString$
							foundString = 1
						endif
						currentStringIndex = currentStringIndex + 1
					endwhile
				endif
			endif
			# If filtering is on, extract values only if the current phoneme is included
			if filter_labels = 0 or foundString = 1
				selectObject: current_tg
				#  Extract phonemic context
				if extract_left_and_right_context
					if iinterv>1
						previousLabel$ = Get label of interval: reference_tier, iinterv-1
					else
						previousLabel$ = "--undefined--"
					endif
					if iinterv<ninterv
						followingLabel$ = Get label of interval: reference_tier, iinterv+1
					else
						followingLabel$ = "--undefined--"
					endif
				endif
				# Extract start and end times, and calculate segment duration
				start_time = Get start point: reference_tier, iinterv
				end_time = Get end point: reference_tier, iinterv
				duration = end_time-start_time

				# Get the signal extract as a Sound object, and compute acoustic parameters
				if extract_F0+extract_intensity+extract_formants+extract_CoG+extract_HNR+extract_ZCR>0
					# Get the start and end time of the signal extract ( Sound object including offset before and after the target interval)
					extract_start_time = start_time - offset_for_acoustic_parameters_extraction_milliseconds/1000
					extract_end_time = end_time + offset_for_acoustic_parameters_extraction_milliseconds/1000
					# Check that the extract start and end times are not off limits
					if extract_start_time < 0
						extract_start_time = 0
					endif
					if extract_end_time > current_snd_total_duration
						extract_end_time = current_snd_total_duration
					endif
					selectObject: current_snd
					current_snd_extract = Extract part: extract_start_time, extract_end_time, "yes"
					nChannels = Get number of channels
					if nChannels > 1
						current_snd_extract_tmp = current_snd_extract
						current_snd_extract = Extract one channel: target_channel
						removeObject: current_snd_extract_tmp
					endif

					# Get F0, intensity and formants values of the extract if requested
					# Store values extracted for each target point in TableOfReal objects
					if extract_F0
						selectObject: current_snd_extract
						current_pitch = To Pitch: timeStepF0detection, minF0, maxF0
						f0_values_extracted_points = Create TableOfReal: "f0_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_pitch
							current_value = Get value at time: current_point_time, "Hertz", "Linear"
							selectObject: f0_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_pitch
						mean_f0 = Get mean: start_time, end_time, "Hertz"
						if get_median
							median_f0 = Get quantile: start_time, end_time, 0.5, "Hertz"
						endif
						if get_standard_deviation
							std_f0 = Get standard deviation: start_time, end_time, "Hertz"
						endif
						if get_min_max_with_time
							min_f0 = Get minimum: start_time, end_time, "Hertz", "Parabolic"
							min_f0_relative_time = Get time of minimum: start_time, end_time, "Hertz", "Parabolic"
							min_f0_relative_time = (min_f0_relative_time - start_time) / duration
							max_f0 = Get maximum: start_time, end_time, "Hertz", "Parabolic"
							max_f0_relative_time = Get time of maximum: start_time, end_time, "Hertz", "Parabolic"
							max_f0_relative_time = (max_f0_relative_time - start_time) / duration
						endif
						removeObject: current_pitch
					endif
					
					if extract_intensity
						selectObject: current_snd_extract
						current_intensity = To Intensity: minF0, timeStepIntensityExtraction, subtractMeanFromIntensityValues$
						intensity_values_extracted_points = Create TableOfReal: "intensity_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_intensity
							current_value = Get value at time: current_point_time, "Cubic"
							selectObject: intensity_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_intensity
						mean_intensity = Get mean: start_time, end_time, "dB"
						if get_median
							median_intensity = Get quantile: start_time, end_time, 0.5
						endif
						if get_standard_deviation
							std_intensity = Get standard deviation: start_time, end_time
						endif
						if get_min_max_with_time
							min_intensity = Get minimum: start_time, end_time, "Cubic"
							min_intensity_relative_time = Get time of minimum: start_time, end_time, "Cubic"
							min_intensity_relative_time = (min_intensity_relative_time - start_time) / duration
							max_intensity = Get maximum: start_time, end_time, "Cubic"
							max_intensity_relative_time = Get time of maximum: start_time, end_time, "Cubic"
							max_intensity_relative_time = (max_intensity_relative_time - start_time) / duration
						endif
						removeObject: current_intensity
					endif
					
					if extract_CoG
						cog_values_extracted_points = Create TableOfReal: "cog_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							# Get number of points in extract
							current_extract_start_time = current_point_time - (extractsDurationForCoGcomputationMilliseconds/2000)
							if current_extract_start_time < start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								current_extract_start_time = start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
							endif
							current_extract_end_time = current_point_time + (extractsDurationForCoGcomputationMilliseconds/2000)
							if current_extract_end_time > end_time + (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								current_extract_end_time = end_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
							endif
							selectObject: current_snd_extract
							currentSoundSlice = Extract part: current_extract_start_time, current_extract_end_time, windowShapeForCenterOfGravityComputation$, relativeWidthForCenterOfGravityComputation, "no"
							currentSoundSliceSpectrum = To Spectrum: useFFTinCenterOfGravityComputation$
							current_value = Get centre of gravity: powerValueInCenterOfGravityComputation
							selectObject: cog_values_extracted_points
							Set value: ipoint, 1, current_value
							removeObject: currentSoundSlice, currentSoundSliceSpectrum
						endfor
						
						selectObject: current_snd_extract
						currentSoundSlice = Extract part: start_time, end_time, windowShapeForCenterOfGravityComputation$, relativeWidthForCenterOfGravityComputation, "no"
						currentSoundSliceSpectrum = To Spectrum: useFFTinCenterOfGravityComputation$
						mean_CoG = Get centre of gravity: powerValueInCenterOfGravityComputation
						removeObject: currentSoundSlice, currentSoundSliceSpectrum
						
						# If the computation of the median, standard deviation or minumum/maximum is requested, compute values using a sliding window and store them in a Table object
						cog_values_sliding_window = Create Table with column names: "table_CoG_values", 0, "times values"
						if get_median+get_standard_deviation+get_min_max_with_time > 0
							current_point_index = 0
							current_point_time = start_time
							while current_point_time <= end_time
								current_extract_start_time = current_point_time - (extractsDurationForCoGcomputationMilliseconds/2000)
								if current_extract_start_time < start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
									current_extract_start_time = start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								endif
								current_extract_end_time = current_point_time + (extractsDurationForCoGcomputationMilliseconds/2000)
								if current_extract_end_time > end_time + (offset_for_acoustic_parameters_extraction_milliseconds/1000)
									current_extract_end_time = end_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								endif
								selectObject: current_snd_extract
								currentSoundSlice = Extract part: current_extract_start_time, current_extract_end_time, windowShapeForCenterOfGravityComputation$, relativeWidthForCenterOfGravityComputation, "no"
								currentSoundSliceSpectrum = To Spectrum: useFFTinCenterOfGravityComputation$
								current_value = Get centre of gravity: powerValueInCenterOfGravityComputation
								if current_value<>undefined
									selectObject: cog_values_sliding_window
									Append row
									current_point_index = current_point_index + 1
									Set numeric value: current_point_index, "times", current_point_time
									Set numeric value: current_point_index, "values", current_value
								endif
								removeObject: currentSoundSlice, currentSoundSliceSpectrum
								current_point_time = current_point_time + timestepForCoGcomputationMilliseconds/1000
							endwhile
						endif
						
						selectObject: cog_values_sliding_window
						nrows_cog_values_sliding_window = Get number of rows
						if get_median
							if nrows_cog_values_sliding_window>0
								median_CoG = Get quantile: "values", 0.5
							else
								median_CoG = undefined
							endif
						endif
						if get_standard_deviation
							if nrows_cog_values_sliding_window>0
								std_CoG = Get standard deviation: "values"
							else
								std_CoG = undefined
							endif
						endif
						if get_min_max_with_time
							if nrows_cog_values_sliding_window>0
								Sort rows: "values times"
								min_CoG = Get value: 1, "values"
								min_CoG_relative_time = Get value: 1, "times"
								min_CoG_relative_time = (min_CoG_relative_time - start_time) / duration
								# reverse sorting: replace values by their negative counterpart (so the first values in time will be selected in cas of equal values)
								Formula: "values", "- (self)"
								Sort rows: "values times"
								max_CoG = Get value: 1, "values"
								max_CoG = -max_CoG
								max_CoG_relative_time = Get value: 1, "times"
								max_CoG_relative_time = (max_CoG_relative_time - start_time) / duration
							else
								min_CoG = undefined
								min_CoG_relative_time = undefined
								min_CoG_relative_time = undefined
								max_CoG = undefined
								max_CoG_relative_time = undefined
								max_CoG_relative_time = undefined
							endif
						endif
						removeObject: cog_values_sliding_window
					endif
					
					if extract_HNR
						selectObject: current_snd_extract
						current_HNR = To Harmonicity (cc): timeStepHNRextraction, minF0, silenceTresholdHNRextraction, periodsPerWindowHNRextraction
						hnr_values_extracted_points = Create TableOfReal: "hnr_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_HNR
							current_value = Get value at time: current_point_time, interpolationMethodUsedInHNRextraction$
							selectObject: hnr_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_HNR
						mean_HNR = Get mean: start_time, end_time
						if get_median
							# No "Get quantile" function defined for Harmonicity objects: export values as Table
							selectObject: current_HNR
							mat_HNR_tmp = To Matrix
							tmat_HNR_tmp = Transpose
							tor_HNR_tmp = To TableOfReal
							table_HNR_tmp = To Table: "rowLabel"
							Set column label (index): 2, "value"
							median_HNR = Get quantile: "value", 0.5
							removeObject: mat_HNR_tmp, tmat_HNR_tmp, tor_HNR_tmp, table_HNR_tmp
						endif
						if get_standard_deviation
							selectObject: current_HNR
							std_HNR = Get standard deviation: start_time, end_time
						endif
						if get_min_max_with_time
							selectObject: current_HNR
							min_HNR = Get minimum: start_time, end_time, interpolationMethodUsedInHNRextraction$
							min_HNR_relative_time = Get time of minimum: start_time, end_time, interpolationMethodUsedInHNRextraction$
							min_HNR_relative_time = (min_HNR_relative_time - start_time) / duration
							max_HNR = Get maximum: start_time, end_time, interpolationMethodUsedInHNRextraction$
							max_HNR_relative_time = Get time of maximum: start_time, end_time, interpolationMethodUsedInHNRextraction$
							max_HNR_relative_time = (max_HNR_relative_time - start_time) / duration
						endif
						removeObject: current_HNR
					endif
					
					if extract_ZCR
						selectObject: current_snd_extract
						current_zcr_pp = To PointProcess (zeroes): 1, includeRaisingPartsInZeroCrossingRateComputation$, includeFallingPartsInZeroCrossingRateComputation$
						current_zcr_tt = Up to TextTier: ""
						current_zcr_tg = Into TextGrid
						removeObject: current_zcr_pp, current_zcr_tt
						
						zcr_values_extracted_points = Create TableOfReal: "zcr_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							# Get number of points in extract
							current_extract_start_time = current_point_time - (extractsDurationForZCRcomputationMilliseconds/2000)
							if current_extract_start_time < start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								current_extract_start_time = start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
							endif
							current_extract_end_time = current_point_time + (extractsDurationForZCRcomputationMilliseconds/2000)
							if current_extract_end_time > end_time + (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								current_extract_end_time = end_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
							endif
							selectObject: current_zcr_tg
							current_zcr_tg_extract = Extract part: current_extract_start_time, current_extract_end_time, "no"
							nCrossingPoints = Get number of points: 1
							current_value = nCrossingPoints / (current_extract_end_time - current_extract_start_time)
							selectObject: zcr_values_extracted_points
							Set value: ipoint, 1, current_value
							removeObject: current_zcr_tg_extract
						endfor
						selectObject: current_zcr_tg
						zcr_tg_extract = Extract part: start_time, end_time, "no"
						nCrossingPoints = Get number of points: 1
						mean_ZCR = nCrossingPoints / (end_time - start_time)
						removeObject: zcr_tg_extract
						
						# If the computation of the median, standard deviation or minumum/maximum is requested, compute values using a sliding window and store them in a Table object
						zcr_values_sliding_window = Create Table with column names: "table_ZCR_values", 0, "times values"
						if get_median+get_standard_deviation+get_min_max_with_time > 0
							current_point_index = 0
							current_point_time = start_time
							while current_point_time <= end_time
								current_extract_start_time = current_point_time - (extractsDurationForZCRcomputationMilliseconds/2000)
								if current_extract_start_time < start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
									current_extract_start_time = start_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								endif
								current_extract_end_time = current_point_time + (extractsDurationForZCRcomputationMilliseconds/2000)
								if current_extract_end_time > end_time + (offset_for_acoustic_parameters_extraction_milliseconds/1000)
									current_extract_end_time = end_time - (offset_for_acoustic_parameters_extraction_milliseconds/1000)
								endif
								selectObject: current_zcr_tg
								current_zcr_tg_extract = Extract part: current_extract_start_time, current_extract_end_time, "no"
								nCrossingPoints = Get number of points: 1
								current_value = nCrossingPoints / (current_extract_end_time - current_extract_start_time)
								if current_value<>undefined
									selectObject: zcr_values_sliding_window
									Append row
									current_point_index = current_point_index + 1
									Set numeric value: current_point_index, "times", current_point_time
									Set numeric value: current_point_index, "values", current_value
								endif
								removeObject: current_zcr_tg_extract
								current_point_time = current_point_time + timestepForZCRcomputationMilliseconds/1000
							endwhile
						endif
						
						selectObject: zcr_values_sliding_window					
						nrows_zcr_values_sliding_window = Get number of rows
						if get_median
							if nrows_zcr_values_sliding_window>0
								median_ZCR = Get quantile: "values", 0.5
							else
								median_ZCR = undefined
							endif
						endif
						if get_standard_deviation
							if nrows_zcr_values_sliding_window>0
								std_ZCR = Get standard deviation: "values"
							else
								std_ZCR = undefined
							endif
						endif
						if get_min_max_with_time
							if nrows_zcr_values_sliding_window>0
								Sort rows: "values times"
								min_ZCR = Get value: 1, "values"
								min_ZCR_relative_time = Get value: 1, "times"
								min_ZCR_relative_time = (min_ZCR_relative_time - start_time) / duration
								# reverse sorting: replace values by their negative counterpart (so the first values in time will be selected in cas of equal values)
								Formula: "values", "- (self)"
								Sort rows: "values times"
								max_ZCR = Get value: 1, "values"
								max_ZCR = -max_ZCR
								max_ZCR_relative_time = Get value: 1, "times"
								max_ZCR_relative_time = (max_ZCR_relative_time - start_time) / duration
							else
								min_ZCR = undefined
								min_ZCR_relative_time = undefined
								min_ZCR_relative_time = undefined
								max_ZCR = undefined
								max_ZCR_relative_time = undefined
								max_ZCR_relative_time = undefined
							endif
						endif
						removeObject: current_zcr_tg, zcr_values_sliding_window
					endif
					
					if extract_formants
						selectObject: current_snd_extract
						current_formant = To Formant (burg): timeStepFormantsDetection, nDetectedFormants, maxFrequencyForFormantsDetection, windowLengthFormantsDetectionSeconds, preEmphasisFormantsDetectionHertz
						f1_values_extracted_points = Create TableOfReal: "f1_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_formant
							current_value = Get value at time: 1, current_point_time, "Hertz", "Linear"
							selectObject: f1_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_formant
						mean_f1 = Get mean: 1, start_time, end_time, "Hertz"
						if get_median
							median_f1 = Get quantile: 1, start_time, end_time, "hertz", 0.5
						endif
						if get_standard_deviation
							std_f1 = Get standard deviation: 1, start_time, end_time, "Hertz"
						endif
						if get_min_max_with_time
							min_f1 = Get minimum: 1, start_time, end_time, "Hertz", "Parabolic"
							min_f1_relative_time = Get time of minimum: 1, start_time, end_time, "Hertz", "Parabolic"
							min_f1_relative_time = (min_f1_relative_time - start_time) / duration
							max_f1 = Get maximum: 1, start_time, end_time, "Hertz", "Parabolic"
							max_f1_relative_time = Get time of maximum: 1, start_time, end_time, "Hertz", "Parabolic"
							max_f1_relative_time = (max_f1_relative_time - start_time) / duration
						endif

						f2_values_extracted_points = Create TableOfReal: "f2_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_formant
							current_value = Get value at time: 2, current_point_time, "Hertz", "Linear"
							selectObject: f2_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_formant
						mean_f2 = Get mean: 2, start_time, end_time, "Hertz"
						if get_median
							median_f2 = Get quantile: 2, start_time, end_time, "hertz", 0.5
						endif
						if get_standard_deviation
							std_f2 = Get standard deviation: 2, start_time, end_time, "Hertz"
						endif
						if get_min_max_with_time
							min_f2 = Get minimum: 2, start_time, end_time, "Hertz", "Parabolic"
							min_f2_relative_time = Get time of minimum: 2, start_time, end_time, "Hertz", "Parabolic"
							min_f2_relative_time = (min_f2_relative_time - start_time) / duration
							max_f2 = Get maximum: 2, start_time, end_time, "Hertz", "Parabolic"
							max_f2_relative_time = Get time of maximum: 2, start_time, end_time, "Hertz", "Parabolic"
							max_f2_relative_time = (max_f2_relative_time - start_time) / duration
						endif

						f3_values_extracted_points = Create TableOfReal: "f3_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_formant
							current_value = Get value at time: 3, current_point_time, "Hertz", "Linear"
							selectObject: f3_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_formant
						mean_f3 = Get mean: 3, start_time, end_time, "Hertz"
						if get_median
							median_f3 = Get quantile: 3, start_time, end_time, "hertz", 0.5
						endif
						if get_standard_deviation
							std_f3 = Get standard deviation: 3, start_time, end_time, "Hertz"
						endif
						if get_min_max_with_time
							min_f3 = Get minimum: 3, start_time, end_time, "Hertz", "Parabolic"
							min_f3_relative_time = Get time of minimum: 3, start_time, end_time, "Hertz", "Parabolic"
							min_f3_relative_time = (min_f3_relative_time - start_time) / duration
							max_f3 = Get maximum: 3, start_time, end_time, "Hertz", "Parabolic"
							max_f3_relative_time = Get time of maximum: 3, start_time, end_time, "Hertz", "Parabolic"
							max_f3_relative_time = (max_f3_relative_time - start_time) / duration
						endif

						f4_values_extracted_points = Create TableOfReal: "f4_values", n_extracted_points, 1
						for ipoint from 1 to n_extracted_points
							selectObject: extracted_points_relative_times
							current_point_relative_position = Get value in cell: ipoint, 1
							current_point_time = start_time + (current_point_relative_position * duration)
							selectObject: current_formant
							current_value = Get value at time: 4, current_point_time, "Hertz", "Linear"
							selectObject: f4_values_extracted_points
							Set value: ipoint, 1, current_value
						endfor
						selectObject: current_formant
						mean_f4 = Get mean: 4, start_time, end_time, "Hertz"
						if get_median
							median_f4 = Get quantile: 4, start_time, end_time, "hertz", 0.5
						endif
						if get_standard_deviation
							std_f4 = Get standard deviation: 4, start_time, end_time, "Hertz"
						endif
						if get_min_max_with_time
							min_f4 = Get minimum: 4, start_time, end_time, "Hertz", "Parabolic"
							min_f4_relative_time = Get time of minimum: 4, start_time, end_time, "Hertz", "Parabolic"
							min_f4_relative_time = (min_f4_relative_time - start_time) / duration
							max_f4 = Get maximum: 4, start_time, end_time, "Hertz", "Parabolic"
							max_f4_relative_time = Get time of maximum: 4, start_time, end_time, "Hertz", "Parabolic"
							max_f4_relative_time = (max_f4_relative_time - start_time) / duration
						endif

						removeObject: current_formant
					endif
					removeObject: current_snd_extract
				endif

				# Write information to results file
				appendFile: results_file$, tg$, tab$, label$, tab$, start_time, tab$, end_time, tab$, duration
				if extract_left_and_right_context
					appendFile: results_file$, tab$, previousLabel$, tab$, followingLabel$
				endif
				# F0
				if extract_F0
					appendFile: results_file$, tab$, mean_f0
					selectObject: f0_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_f0
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_f0
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_f0, tab$, min_f0_relative_time, tab$, max_f0, tab$, max_f0_relative_time
					endif
				endif
				# intensity
				if extract_intensity
					appendFile: results_file$, tab$, mean_intensity
					selectObject: intensity_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_intensity
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_intensity
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_intensity, tab$, min_intensity_relative_time, tab$, max_intensity, tab$, max_intensity_relative_time
					endif
				endif
				# CoG
				if extract_CoG
					appendFile: results_file$, tab$, mean_CoG
					selectObject: cog_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_CoG
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_CoG
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_CoG, tab$, min_CoG_relative_time, tab$, max_CoG, tab$, max_CoG_relative_time
					endif
				endif
				# HNR
				if extract_HNR
					appendFile: results_file$, tab$, mean_HNR
					selectObject: hnr_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_HNR
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_HNR
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_HNR, tab$, min_HNR_relative_time, tab$, max_HNR, tab$, max_HNR_relative_time
					endif
				endif
				# ZCR
				if extract_ZCR
					appendFile: results_file$, tab$, mean_ZCR
					selectObject: zcr_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_ZCR
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_ZCR
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_ZCR, tab$, min_ZCR_relative_time, tab$, max_ZCR, tab$, max_ZCR_relative_time
					endif
				endif
				# formants
				if extract_formants
					appendFile: results_file$, tab$, mean_f1
					selectObject: f1_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_f1
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_f1
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_f1, tab$, min_f1_relative_time, tab$, max_f1, tab$, max_f1_relative_time
					endif
					appendFile: results_file$, tab$, mean_f2
					selectObject: f2_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_f2
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_f2
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_f2, tab$, min_f2_relative_time, tab$, max_f2, tab$, max_f2_relative_time
					endif
					appendFile: results_file$, tab$, mean_f3
					selectObject: f3_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_f3
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_f3
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_f3, tab$, min_f3_relative_time, tab$, max_f3, tab$, max_f3_relative_time
					endif
					appendFile: results_file$, tab$, mean_f4
					selectObject: f4_values_extracted_points
					for ipoint from 1 to n_extracted_points
						current_point_value = Get value: ipoint, 1
						appendFile: results_file$, tab$, current_point_value
					endfor
					if get_median
						appendFile: results_file$, tab$, median_f4
					endif
					if get_standard_deviation
						appendFile: results_file$, tab$, std_f4
					endif
					if get_min_max_with_time
						appendFile: results_file$, tab$, min_f4, tab$, min_f4_relative_time, tab$, max_f4, tab$, max_f4_relative_time
					endif
				endif
				# Clean-up: remove temporary storage of values extracted on each point
				if extract_F0
					removeObject: f0_values_extracted_points
				endif
				if extract_intensity
					removeObject: intensity_values_extracted_points
				endif
				if extract_CoG
					removeObject: cog_values_extracted_points
				endif
				if extract_HNR
					removeObject: hnr_values_extracted_points
				endif
				if extract_ZCR
					removeObject: zcr_values_extracted_points
				endif
				if extract_formants
					removeObject: f1_values_extracted_points, f2_values_extracted_points, f3_values_extracted_points, f4_values_extracted_points
				endif

				# Extract labels from other tiers and append information to the results file
				# Get interval midpoint (used as reference to extract information from other tiers)
				mid_point = start_time + duration/2
				if secondary_tier>0
					# Get the corresponding label on the selected secondary tier
					selectObject: current_tg
					intervtmp = Get interval at time: secondary_tier, mid_point
					secondaryTierlabel$ = Get label of interval: secondary_tier, intervtmp
					appendFile: results_file$, tab$, secondaryTierlabel$
					if extract_left_and_right_context
						if intervtmp-1 > 0
							previousLabelSecondaryTier$ = Get label of interval: secondary_tier, intervtmp-1
						else
							previousLabelSecondaryTier$ = "--undefined--"
						endif
						nIntervalsSecondaryTier = Get number of intervals: secondary_tier
						if intervtmp+1 <= nIntervalsSecondaryTier
							followingLabelSecondaryTier$ = Get label of interval: secondary_tier, intervtmp+1
						else
							followingLabelSecondaryTier$ = "--undefined--"
						endif
						appendFile: results_file$, tab$, previousLabelSecondaryTier$, tab$, followingLabelSecondaryTier$
					endif
				elsif secondary_tier = -1
					# Get the corresponding labels on all interval tiers
					# Loop every tier
					for itier from 1 to ntiers
						# Ignore it if it's the reference tier (already processed) or a point tier (no labels to extract)
						selectObject: current_tg
						interv_tier = Is interval tier: itier
						if itier<>reference_tier and interv_tier=1
							selectObject: current_tg
							# Get label at reference tier's current interval midpoint and append it to results file
							intervtmp = Get interval at time: itier, mid_point
							secondaryTierlabel$ = Get label of interval: itier, intervtmp
							appendFile: results_file$, tab$, secondaryTierlabel$
							if extract_left_and_right_context
								if intervtmp-1 > 0
									previousLabelSecondaryTier$ = Get label of interval: itier, intervtmp-1
								else
									previousLabelSecondaryTier$ = "--undefined--"
								endif
								nIntervalsSecondaryTier = Get number of intervals: itier
								if intervtmp+1 <= nIntervalsSecondaryTier
									followingLabelSecondaryTier$ = Get label of interval: itier, intervtmp+1
								else
									followingLabelSecondaryTier$ = "--undefined--"
								endif
								appendFile: results_file$, tab$, previousLabelSecondaryTier$, tab$, followingLabelSecondaryTier$
							endif
						endif
					endfor
				endif

				# Append a line break to the results file before proceeding to the next interval
				appendFile: results_file$, newline$
			endif
		endif
	endfor
	# Clean-up: remove current textgrid, pitch, intensity, formant and sound objects
	removeObject: current_tg
	if extract_F0+extract_intensity+extract_formants+extract_CoG+extract_HNR+extract_ZCR>0
		removeObject: current_snd
	endif
endfor

appendInfoLine: newline$, "Processed ", ntextgrids, " files."

# Clean-up: remove lists of textgrids and vowels
removeObject: flist, extracted_points_relative_times
if filter_labels = 1
	removeObject: stringsDictionary
endif
