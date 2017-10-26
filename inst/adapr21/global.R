
#if(!require(shinyIncubator)){devtools::install_github("shiny-incubator", "rstudio")}
#require(shinyIncubator)
library(devtools)
library(adapr)

all.orchards <-get_orchard()


adapr_options <- getAdaprOptions(TRUE)
