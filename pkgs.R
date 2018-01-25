# instala e carrega pacotes -----------------------------------------------

pkgs <- function(packages.list) {
   new.packages <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
   if(length(new.packages))
      install.packages(new.packages)
   lapply(packages.list, require, character.only = TRUE)
}