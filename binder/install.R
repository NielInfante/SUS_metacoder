pkgs = c("tidyverse", "metacoder")
ncores = parallel::detectCores()
install.packages(pkgs, Ncpus = ncores)
