pkgs = c("BiocManager", "tidyverse", "plotly","knitr", "rprojroot", "rmarkdown","scales","taxize", "data.table","ape")
ncores = parallel::detectCores()
install.packages(pkgs, Ncpus = ncores)

BiocManager::install("phyloseq")

