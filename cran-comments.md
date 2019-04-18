## Test environments
* local Windows 7 install, R 3.5.3
* rhub
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 NOTEs:

- Possibly mis-spelled words in DESCRIPTION: lme
  
  This is a reference to the lme4 package

- checking dependencies in R code (2.2s)
  Packages in Depends field not imported from: 'ggplot2' 'ggpubr'
  
  These functions from these packages are prefixed with ggplot2::
  or ggpubr:: throughout to avoid confusion with other packages

- checking R code for possible problems
  Numerous notes
  
  Functions are from the stats package, which is installed by 
  default with base (coef, confint, pf, weighted.mean,
  p.adjust.methods, p.adjust, vcov)
  
  Variables are defined in other package functions called by the 
  current function
