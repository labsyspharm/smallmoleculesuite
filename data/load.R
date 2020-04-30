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

data_tas <- file.path(dir_data, "shiny_tas_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  {.[data_gene_info[, .(gene_id, symbol)], on = "gene_id", nomatch = NULL]}

data_fingerprints <- morgancpp::MorganFPS$new(
  file.path(dir_data, "shiny_fingerprints_morgan_normal.bin"), from_file = TRUE
)

data_affinity_selectivity <- file.path(dir_data, "shiny_selectivity_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  {.[data_gene_info, on = "gene_id", nomatch = NULL]}

for (col in c("toolscore", "affinity_Q1", "offtarget_affinity_Q1", "affinity_Q1_diff", "selectivity", "investigation_bias", "wilcox_pval")) {
  set(data_affinity_selectivity, j = col, value = signif(data_affinity_selectivity[[col]], digits = 2))
}

# data_biochem <- file.path(dir_data, "shiny_biochemical_morgan_normal.fst") %>%
#   fst::read_fst(as.data.table = TRUE)

data_commercial <- file.path(dir_data, "shiny_commercial_info_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

commercially_available <- data_commercial[["lspci_id"]] %>%
  unique()

# save(
#   list = c(
#     "data_genes",
#     "data_cmpd_info",
#     "name_lspci_id_map",
#     "lspci_id_name_map",
#     "data_selection_chemprobes",
#     "data_gene_info",
#     "data_pfp",
#     "data_tas",
#     "data_affinity_selectivity"
#   ),
#   file = "test.rdata",
#   ascii = FALSE,
#   compress = "gzip"
# )
#
# tic();load("test.rdata", envir = x);toc()
