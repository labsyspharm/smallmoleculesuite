# library only ----
data_genes <- fs::dir_ls("data", glob = "*.txt") %>%
  stats::setNames(gsub("(?:^data/|(?:_\\d+)?\\.txt$)", "", .)) %>%
  lapply(readr::read_tsv, col_names = "genes") %>%
  lapply(. %>% dplyr::arrange(genes) %>% dplyr::pull())

dir_data <- here("data")

data_cmpd_info <- file.path(dir_data, "shiny_compounds_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

name_lspci_id_map <- file.path(dir_data, "name_lspci_id_map.fst") %>%
  fst::read_fst() %>%
  {set_names(.[["lspci_id"]], .[["name"]])}

lspci_id_name_map <- file.path(dir_data, "lspci_id_name_map.fst") %>%
  fst::read_fst() %>%
  {set_names(.[["name"]], .[["lspci_id"]])}

data_selection_chemprobes <- file.path(dir_data, "shiny_chemical_probes_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  .[avg_rating == 4]

data_gene_info <- file.path(dir_data, "shiny_targets_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

data_pfp <- file.path(dir_data, "phenotypic_rscore_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

data_tas <- file.path(dir_data, "tas_score_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

data_fingerprints <- morgancpp::MorganFPS$new(
  file.path(dir_data, "shiny_fingerprints_morgan_normal.bin"), from_file = TRUE
)

data_affinity_selectivity <- file.path(dir_data, "shiny_selectivity_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

for (col in c("toolscore", "ontarget_IC50_Q1", "offtarget_IC50_Q1", "IC50_diff", "Kd_Q1")) {
  set(data_affinity_selectivity, j = col, value = signif(data_affinity_selectivity[[col]], digits = 2))
}
