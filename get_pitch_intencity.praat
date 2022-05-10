# This is a Praat script made for investigation of Abaza vowels. It analyses multiple selected sounds 
# (TextGrids should be also uploaded to Praat Obects). The file should have the following structure:
# * first tier --- word label
# * second tier --- translation label
# * third tier --- sound label
# * fourth tier --- utterance label


# This script is distributed under the GNU General Public License.
# George Moroz 09.05.2022

form Get Pitch listing from a file
  comment Where should the script write a result file
  text directory /home/agricolamz/for_work/HSE/students/2022_Kuznetsova/data/
  comment How should the script name a result file
  text resultfile log.txt
  comment Time step
  real step 0.01
  comment Pitch floor (Hz)
  integer floor 90
  comment Pitch ceiling (Hz)
  integer ceiling 250
  comment 5. formant ceiling (Hz)
  integer fceiling 5500
  comment Minimum pitch for intensity (Hz)
  integer mpitch 200  
endform

n = numberOfSelected("Sound")
for j to n
	sound[j] = selected("Sound", j)
endfor
for k to n
	selectObject: sound[k]
	object_name$ = selected$ ("Sound")
	select TextGrid 'object_name$'
	number_of_intervals = Get number of intervals... 3
			for b from 1 to number_of_intervals
				select TextGrid 'object_name$'
				interval_label$ = Get label of interval... 3 'b'
				utterance$ = Get label of interval... 4 'b'
				if interval_label$ <> ""
					start = Get starting point... 3 'b'
					end = Get end point... 3 'b'
                    			duration = end - start
					int_1 = Get interval at time... 1 end
					word$ = Get label of interval... 1 int_1
					trans$ = Get label of interval... 2 int_1
					select Sound 'object_name$'
					s = Extract part: start, end, "rectangular", 1, "yes"
					select s
					fragment_name$ = selected$ ("Sound")
					pitch = To Pitch... step floor ceiling
					selectObject: s
					formant = To Formant (burg): 0, 5, fceiling, 0.025, 50
                    selectObject: s
                    intensity = To Intensity: mpitch, 0, "no"
					i = start
					while i <= end
						select Pitch 'fragment_name$'
						f0 = Get value at time... 'i' Hertz Linear
						select Formant 'fragment_name$'
						f1 = Get value at time: 1, i, "Hertz", "Linear"
						f2 = Get value at time: 2, i, "Hertz", "Linear"
						f3 = Get value at time: 3, i, "Hertz", "Linear"
                        			select Intensity 'fragment_name$'
		                            intvalue = Get value at time: 'i', "cubic"
						i = i + 0.01
						fileappend "'directory$''resultfile$'" 'object_name$''tab$''interval_label$''tab$''utterance$''tab$''word$''tab$''trans$''tab$''f0''tab$''f1''tab$''f2''tab$''f3''tab$''intvalue''tab$''duration''tab$''i''newline$'
					endwhile
					removeObject: s
					removeObject: pitch
					removeObject: formant
					removeObject: intensity
				endif
			endfor
#	removeObject: "Sound 'object_name$'"
#	removeObject: "TextGrid 'object_name$'"
endfor

