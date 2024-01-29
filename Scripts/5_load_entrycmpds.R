library(readxl)

# Compound data

# get data

files <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Samples/Entry", pattern = "\\.xlsx$", full.names = T)

cmpds.data <- lapply(files, read_xlsx)

names(cmpds.data) <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Samples/Entry", pattern = "\\.xlsx$")

# merge

cmpds.data <- lapply(cmpds.data, function(x) x[x$WELL_TYPE_CODE=="SAM",])

cmpds.data <- do.call(rbind, cmpds.data)
