library(tidyverse)
library(here)
library(synapser)
library(synExtra)

synLogin()

# set directories, import files ------------------------------------------------
###############################################################################T

syn_parent <- "syn18457321"
release <- "chembl_v29"
syn_release <- synPluck(syn_parent, release)

dir_data <- here("data")

syn <- synDownloader(dir_data, ifcollision = "overwrite.local", followLink = TRUE)

syn_tables <- synPluck(syn_parent, release, "website_tables")

files <- synChildren(syn_tables) %>% {
  .[
    !str_ends(names(.), ".qs") &
      !str_detect(names(.), "shiny_inchis")
  ]
}

syn(files)
