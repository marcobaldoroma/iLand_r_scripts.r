# Hi Marco, I made a new simpler management script + csv table.   Will send you a bit later and you can replace it with the older one, and can try this one out too.  It will be now all stands in STP2, that shelterwood, and can look there these regeneration cuts even easier I think.  (and we will go on with this in the future, to set in the script to change STP automatically based on for example spruce proportion in the stand. And not having it fixed in time, just allow to dynamically set by the model, as the species distribution changes in the stand)





___________________________________________________________ Knitr Report _____________________________________________________________________________

# needed to work properly with latex

tinytex::install_tinytex()
tinytex::tlmgr_update()

# libraries needed for knitr report

library (knitr)
library (tinytex)         # librerie necessarie


# vedi la funzione stitch e trova altri template da qui. https://rdrr.io/cran/knitr/man/stitch.html
# I hided only warnings #delete the warning, message and codes in the report.
# To hide the warnings use the code here or go in the link to study other functionalities

knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)

stitch("D:/iLand.r/report_13_10/script_13.10_report.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))  



