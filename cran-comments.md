## Resubmission

This is a resubmission. In this version I have:

* Removed redundant "in R" phrase from title and description

* Rewrote description to not start with "This package", etc

* Added missing \value to .Rd file for aba_plot function

* Replaced \dontrun{} with \donttest{} in examples where relevant

* Removed files from vignettes/examples/ folder which had hard-coded filepaths

* Replaced 'options(warn = -1)' with 'suppresWarnings()' where relevant in the code


## R CMD check results

* There were no ERRORs or WARNINGs. 

* The words 'Biomarker' and 'biomarker' sometimes give notes as possibly misspelled words but they are correctly spelled.
