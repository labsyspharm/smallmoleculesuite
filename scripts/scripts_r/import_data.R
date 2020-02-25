library(tidyverse)
library(here)
library(synapser)
library(synExtra)

synLogin()

# set directories, import files ------------------------------------------------
###############################################################################T

syn_parent <- "syn18457321"
release <- "chembl_v25"
fp_name <- "morgan_normal"

syn_tables <- synPluck(syn_parent, release, "db_tables", fp_name, "website")

files <- c(
  "phenotypic_rscore.fst",
  "shiny_fingerprints.bin",
  "shiny_selectivity.fst",
  "tas_score.fst",
  "shiny_compounds.fst",
  "shiny_targets.fst",
  "shiny_chemical_probes.fst"
)

dir_data <- here("data")

walk(
  files,
  function(fn) {
    synGet(
      synPluck(syn_tables, fn),
      downloadFile = TRUE,
      downloadLocation = dir_data,
      ifcollision = "overwrite.local"
    )
  }
)
