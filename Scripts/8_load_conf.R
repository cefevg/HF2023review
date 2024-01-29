library(readxl)

# Compound data

# get data

files <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Samples/Conf", pattern = "\\.xlsx$", full.names = T)

conf.data <- lapply(files, read_xlsx)

names(conf.data) <- list.files("C:/Users/GMCDP/OneDrive - Bayer/Desktop/Biology/Primary/Hit-Fisher CT/2021-23/data/Samples/Conf", pattern = "\\.xlsx$")

# merge

conf.data <- lapply(conf.data, function(x) x[x$WELL_TYPE_CODE=="SAM",])

conf.data <- do.call(rbind, conf.data)
