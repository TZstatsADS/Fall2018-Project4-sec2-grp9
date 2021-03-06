# Project: OCR (Optical Character Recognition) 

### Code lib Folder

The lib directory contains various files with function definitions and code.

`char.RData` is the frequency table of all 26 letters in the ground truth corpus.

`two_char.RData` is the is the frequency table of 2-letter combinations in the ground truth corpus.

`deletiontable.RData` is the table containing all possible deletion errors from ground truth; it can be generated by running `deletiontable.R`.

`dict.RData` is the word corpus from ground truth. `tf.RData` is the contingency table of all vocabularies in the ground truth corpus. `words.RData` is the entire ground truth corpus unduplicated. These can be obtained by running `dict.R`.

`iftoken.R` contains the function of error detection.

`DELETION.R` is used to generate all deletion candidates of one text file.

`insersion.R` is used to generate all insersion candidates of one text file.

`substitute.R` is used to generate all substitution candidates of one text file.

`Final candidate.R` is used to combine all possible candidates and select the ones with the highest score.

`create new tess.R` is used to generate the tesseract vector after correction.

`mc run word level.R` is used to loop through all text files and create `final_csv.csv`.

`mc run char.R` is used to loop through all text files and create `final_csv_char.csv`.

`final_csv.csv` contains all the performance measurements of all 100 text files on word level.

`final_csv_char.csv` contains all the performance measurements of all 100 text files on letter level.
