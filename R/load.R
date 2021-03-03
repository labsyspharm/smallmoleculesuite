dir_data <- here("data")

c(
  "shiny_chemical_probes.fst",
  "shiny_compound_names.fst",
  "shiny_compounds.fst",
  "shiny_inchis.fst",
  "shiny_library.fst",
  "shiny_pfp.fst",
  "shiny_selectivity.fst",
  "shiny_targets.fst",
  "shiny_tas.fst"
) %>%
  set_names(
    str_replace(., fixed("shiny"), "data") %>%
      str_replace(fixed(".fst"), "")
  ) %>%
  map(
    ~read_fst(
      file.path(dir_data, .x),
      as.data.table = TRUE
    )
  ) %>%
  iwalk(
    ~assign(.y, .x, envir = .GlobalEnv)
  )

data_gene_lists <- fread(
  file.path(dir_data, "gene_lists.csv.gz")
)
