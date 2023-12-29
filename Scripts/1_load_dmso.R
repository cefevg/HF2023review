library(readxl)

# Negative controls (DMSO)

# get data

files <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/DMSO", pattern = "\\.xlsx$", full.names = T)

dmso.data <- lapply(files, read_xlsx)

names(dmso.data) <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/DMSO", pattern = "\\.xlsx$")

# select only dmso data and merge

dmso.data <- lapply(dmso.data, function(x) x[x$WELL_TYPE_CODE=="DMSO",])

dmso.data <- do.call(rbind, dmso.data)
