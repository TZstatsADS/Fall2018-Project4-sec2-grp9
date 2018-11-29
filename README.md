# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Fall 2018

+ Team 9
+ Team members
	+ Jin, Xin (xj2215)
	+ Wang, Binhan (bw2544)
	+ Wang, Yujia (yw3085)
	+ Zhang, Peilu (pz2233)
	+ Zhu, Kehui (kz2293)

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. We implemented error detection and error correction according to the papers regrading rule-based error detection and spelling correction. For each text file, we first apply the detection algorithm to extract the false words. Then, we propose the probability scoring method to find the highest score word among all candidates to replace the wrong words. However, we think that both papers are not suitable for OCR error detection and OCR correction. For example, the garbage detection rules may take the right words as errors, which will harm the performance of correction. Also, the CORRECT algorithm is only useful for the spelling correction which is not compatible for OCR correction, because CORRECT method just assumes that only one error take places in a word, and it also doesn’t take words separation as consideration. Therefore, in the future, we better find more targeted algorithm for OCR error detection and correction.      
	
**Contribution statement**: All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.

Jin, Xin : Error detection implementation, error correction substitution score section, created the ground truth dictionary, whole algorithm implementation on character level.

Wang, Binhan: Error detection check, error correction deletion score section, candidates combination and selection, whole algorithm implementation on word level, cleaned github repository, edited final `main_notebook` report file.

Wang, Yujia: Error detection check, error correction deletion section, construct evaluation procedure, prepare and deliver presentation.

Zhang, Peilu: Error detection check, error correction insertion section, compare OCR error and ground truth to find real false words, creation of corrected text.

Zhu, Kehui: Error detection check, error correction deletion section, candidates combination and selection, cleaned github repository, edited descriptions for repository.    


Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.



```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
