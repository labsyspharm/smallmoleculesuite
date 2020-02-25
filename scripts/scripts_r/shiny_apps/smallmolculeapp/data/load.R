# library only ----
data_genes <- fs::dir_ls("data", glob = "*.txt") %>%
  stats::setNames(gsub("(?:^data/|(?:_\\d+)?\\.txt$)", "", .)) %>%
  lapply(readr::read_tsv, col_names = "genes") %>%
  lapply(. %>% dplyr::arrange(genes) %>% dplyr::pull())

dir_data <- here("data")

data_selection_selectivity <- file.path(dir_data, "shiny_selectivity_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)
for (col in c("toolscore", "ontarget_IC50_Q1", "offtarget_IC50_Q1", "IC50_diff", "Kd_Q1")) {
  set(data_selection_selectivity, j = col, value = signif(data_selection_selectivity[[col]], digits = 2))
}

data_cmpd_info <- file.path(dir_data, "shiny_compounds_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)

data_selection_chemprobes <- file.path(dir_data, "shiny_chemical_probes_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE) %>%
  .[avg_rating == 4]

data_gene_info <- file.path(dir_data, "shiny_targets_morgan_normal.fst") %>%
  fst::read_fst(as.data.table = TRUE)
