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

dir_data <- here("data")

syn_tables <- synPluck(syn_parent, release, "db_tables", fp_name, "website")

files <- c(
  "phenotypic_rscore.fst",
  "shiny_fingerprints.bin",
  "shiny_selectivity.fst",
  "shiny_compounds.fst",
  "shiny_targets.fst",
  "shiny_chemical_probes.fst",
  "lspci_id_name_map.csv.gz",
  "name_lspci_id_map.fst",
  "lspci_id_name_map.fst",
  "shiny_tas.fst"
)

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

# Gene symbol mapping

c("syn21965763", "syn21965765") %>%
  walk(
    synGet,
    downloadFile = TRUE,
    downloadLocation = dir_data,
    ifcollision = "overwrite.local"
  )
