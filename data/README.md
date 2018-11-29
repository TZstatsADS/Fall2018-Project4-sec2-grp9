# Project: OCR (Optical Character Recognition) 

### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

In this project, there are two subfolders -- ground_truth and tesseract. Each folder contains 100 text files with same file names correspondingly. In addition, there are 3 csv files, which are the confusion matrices in the paper ''Probability scoring for spelling correction''. We used deletion_matrix, insertion_matrix and substituton_matrix to compute the channel probabilities.


