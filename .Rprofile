.First <- function(){
  
  ### load packages
  #if(any(grepl("VANBE", getwd()))) .libPaths("C:/Users/VANBE/Documents/R/win-library/3.1")
  list.of.packages <- c("ggplot2","plyr","gridExtra","reshape2")
  new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, function(x) suppressMessages(require(x, character.only = TRUE)))
  
  ### source src directory
  invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))
  
  ### ggplot layout
  theme_new <- theme_set(theme_classic())
  theme_new <- theme_update(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
}

