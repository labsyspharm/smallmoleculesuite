# library only ----
data_genes <- fs::dir_ls("data", glob = "*.txt") %>%
  stats::setNames(gsub("(?:^data/|(?:_\\d+)?\\.txt$)", "", .)) %>%
  lapply(readr::read_tsv, col_names = "genes") %>%
  lapply(. %>% dplyr::arrange(genes) %>% dplyr::pull())

dir_data <- here("data")

data_cmpd_info <- file.path(dir_data, "shiny_compounds_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

lspci_id_name_map <- file.path(dir_data, "lspci_id_name_map.fst") %>%
  fst::read_fst() %>%
  {set_names(.[["name"]], .[["lspci_id"]])}

data_names <- file.path(dir_data, "all_names_lspci_id_map.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  # Reorder data.table so that some nice normal compounds are at the front
  {
    .[
      order(lspci_id_unique %in% c("16241-1", "78621-1", "90319-1", "96316-1", "76418-1", "78036-1", "83706-1", "81903-1", "72090-1", "97590-1"), decreasing = TRUE)
    ]
  }

# data_selection_chemprobes <- file.path(dir_data, "shiny_chemical_probes_morgan_normal.fst") %>%
#   fst::read_fst(as.data.table = TRUE) %>%
#   .[avg_rating == 4]

data_gene_info <- file.path(dir_data, "shiny_targets_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

data_pfp <- file.path(dir_data, "shiny_phenotypic_rscore_morgan_normal.fst") %>%
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

data_optimal_compounds <- file.path(dir_data, "shiny_optimal_compound_table_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  merge(data_affinity_selectivity, by = c("lspci_id", "gene_id"), all.x = TRUE, all.y = FALSE) %>%
  merge(data_cmpd_info[, .(lspci_id, max_phase)], by = "lspci_id", all.x = TRUE, all.y = FALSE)

data_chemical_probes <- file.path(dir_data, "shiny_chemical_probes_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  {
    .[!is.na(lspci_id) & !is.na(gene_id)][
      , gene_id := as.integer(gene_id)
    ][
      data_gene_info, on = "gene_id", nomatch = NULL
    ]
  }

# data_biochem <- file.path(dir_data, "shiny_biochemical_morgan_normal.fst") %>%
#   fst::read_fst(as.data.table = TRUE)

# data_commercial <- file.path(dir_data, "shiny_commercial_info_morgan_normal.fst") %>%
#   fst::read_fst(as.data.table = TRUE)

# commercially_available <- data_commercial[["lspci_id"]] %>%
#   unique()

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
