library(tidyverse)
library(here)
library(synapser)
library(synExtra)

synLogin()

# set directories, import files ------------------------------------------------
###############################################################################T

syn_parent <- "syn18457321"
release <- "chembl_v27"
syn_release <- synPluck(syn_parent, release)

dir_data <- here("data")

syn <- synDownloader(dir_data, ifcollision = "overwrite")

syn_tables <- synPluck(syn_parent, release, "website_tables")

files <- synChildren(syn_tables)

syn(files)
