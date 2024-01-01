library(readxl)

# Negative controls (DMSO)

# get data

files <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Refs", pattern = "\\.xlsx$", full.names = T)

refs.data <- lapply(files, read_xlsx)

names(refs.data) <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Refs", pattern = "\\.xlsx$")

# merge

refs.data <- do.call(rbind, refs.data)
