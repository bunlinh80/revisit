# library() or require() only load one package at a time
# but...


packages <- c("hrbrthemes",
              "ggthemes",
              "swatches",
              "lubridate",
              "RColorBrewer",
              "viridis",
              "scales",
              "grid",
              "foreign",
              "haven",
              "readstata13",
              "RStata",
              "lattice",
              "prediction",
              "reshape2",
              "creditboomsgonebust",
              "plm",
              "plyr")


lapply(packages, library, character.only = TRUE)

# this loads as many as you put in 'Packages'. They need to be installed first, of course.



# Alternatives
# pkgList <- c("reshape2", "plyr", "lubridate", # data munging
#              "rgdal", "gdalUtils", # spatial tools
#              "rgbif" # pull species data from gbif API
# )
# 
# inst <- pkgList %in% installed.packages()
# 
# if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst], dependencies = TRUE)
# 
# lapply(pkgList, library, character.only = TRUE, quietly=TRUE)