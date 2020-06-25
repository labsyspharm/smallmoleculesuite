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
  "all_names_lspci_id_map.fst",
  "shiny_phenotypic_rscore.fst",
  "shiny_fingerprints.bin",
  "shiny_selectivity.fst",
  "shiny_compounds.fst",
  "shiny_targets.fst",
  "shiny_chemical_probes.fst",
  "lspci_id_name_map.fst",
  "shiny_tas.fst",
  "shiny_commercial_info.fst",
  "shiny_biochemical.fst",
  "shiny_optimal_compound_table.fst",
  # Gene lists
  synChildren(syn_tables) %>%
    magrittr::extract(str_ends(names(.), fixed(".txt")))
)

walk(
  files,
  function(fn) {
    synGet(
      if (str_starts(fn, fixed("syn")))
        fn
      else
        synPluck(syn_tables, fn),
      downloadFile = TRUE,
      downloadLocation = dir_data,
      ifcollision = "overwrite.local"
    )
  }
)
