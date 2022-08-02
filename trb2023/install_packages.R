if (!require("librarian")) install.packages("librarian")
librarian::shelf(
  bookdown, #rmarkdown, knitr,
  data.table,
  furniture,
  gridExtra,
  htmlTable,
  mlogit, 
  pander, 
  pscl, 
  rprojroot,
  scales, 
  stargazer,
  texreg,
  tictoc,
  tidyverse,
  lme4,
  ggthemes,
  whereami
  )
librarian::shelf("cities-lab/rticles")
librarian::shelf("benmarwick/wordcountaddin")
librarian::shelf("tidyverse/multidplyr")
