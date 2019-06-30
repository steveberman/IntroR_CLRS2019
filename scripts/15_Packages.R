.libPaths()
myPackages <- list.files(.libPaths()[1])
head(myPackages)
## install.packages("ggplot2")
## library(MRMR)
## require(MRMR)
whatsLoaded <- .packages()
whatsLoaded

print(.packages())
## detach("package:MRMR", unload = TRUE)
## remove.packages("IdontWantThisAnymore")
