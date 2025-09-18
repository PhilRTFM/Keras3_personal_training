##### variable cleaning and environment setup

rm(list = ls(all = TRUE))

options(stringsAsFactors = FALSE)



# Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Users/Nicolas/AppData/Local/Programs/MiKTeX/miktex/bin/x64",sep=";"))



##### load libraries

library("BiocCheck")

library("devtools")

library("knitr")

library("Rcpp")

library("rmarkdown")

library("roxygen2")



##### set working directory

# setwd("C:/Users/Nicolas/Desktop/CellVizR.full")

# setwd("C:/Users/NTC.KALIMBA/Desktop/CellVizR.full")

# setwd("C:/Users/Gwenou/Documents/05_I3/02_CellVizR_package/CellVizR.full/")

setwd("D:/05_Developpement/CellVizR.full/")





##### remove previous package folders

unlink("./package", recursive = TRUE)

unlink("./package.tar.gz", recursive = TRUE)

unlink("./package.doc", recursive = TRUE)

Sys.sleep(2)

#####



##### create package template

dir.create("package/")

dir.create("package.tar.gz/")

dir.create("package.doc/")

dir.create("package/R")

dir.create("package/man")

#####



##### copy sources files

file.copy("./sources_files/DESCRIPTION", "./package/DESCRIPTION", overwrite = TRUE)

file.copy("./sources_files/NEWS", "./package/NEWS", overwrite = TRUE)

file.copy("./sources_files/LICENSE", "./package/LICENSE", overwrite = TRUE)

#####



##### copy sources scripts

source.file <- list.files("./sources_scripts")

file.copy(paste0("./sources_scripts/", source.file), paste0("./package/R/", source.file), overwrite = TRUE)

#####



##### build package

setwd("./package/")

devtools::document(roclets = c("rd","collate","namespace","vignette"))

devtools::build()

setwd("../")

#####



##### create doc

devtools::document("./package")

system("R CMD Rd2pdf --no-preview package")

#####



##### move pdf

pdffile <- list.files("./", pattern = ".pdf", full.names = TRUE)

pdffile <- pdffile[grepl("package", pdffile)]

file.rename(pdffile, paste0("./package.doc/", basename(pdffile)))

#####



##### move tar

file <- list.files("./", pattern = ".gz", full.names = TRUE)

file <- file[!grepl("package", file)]

file.rename(file, paste0("./package.tar.gz/", basename(file)))

#####



##### check

check("./package")

file <- list.files("./package.tar.gz/", pattern = ".gz", full.names = TRUE)

BiocCheck(file, "no-check-vignettes" = TRUE)

#####



##### move pdf and md

file.copy(paste0("./package.doc/", basename(pdffile)),paste0("./package/documentation.pdf"))

file.copy("README.md", "./package/README.md")

dir.create('./package/README_files')

file.copy("./README_files/", "./package/",recursive =TRUE, overwrite = TRUE)

#####



####### install package

detach("package:CellVizR", unload = TRUE)

remove.packages("CellVizR")

Sys.sleep(2)

install.packages(file, repos = NULL, type = "source")