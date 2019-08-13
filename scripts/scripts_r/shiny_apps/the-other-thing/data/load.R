# library only ----
data_genes <- fs::dir_ls("data", glob = "*.txt") %>% 
  stats::setNames(gsub("(?:^data/|(?:_\\d+)?\\.txt$)", "", .)) %>% 
  lapply(readr::read_tsv, col_names = "genes") %>% 
  lapply(. %>% dplyr::arrange(genes) %>% dplyr::pull())

data_selection_selectivity <- "data/selection_table_selectivity_edited_121017.csv" %>%
  readr::read_csv(progress = FALSE) %>% 
  dplyr::mutate_at(vars(mean_Kd), ~ signif(., 2)) %>% 
  dplyr::mutate(
    SD_aff = NA_real_
  )

data_selection_clinical <- "data/selection_table_clinical_development.csv" %>%
  readr::read_csv(progress = FALSE) %>% 
  dplyr::mutate_at(vars(c(`mean_Kd`, `SD_aff`)), ~ signif(., 2))

data_cmpd_info <- "data/cmpd_info_library_designer_v2.csv" %>% 
  readr::read_csv(progress = FALSE)

data_selection_chemprobes <- "data/selection_table_ChemProbesOrg_v1.csv" %>% 
  readr::read_csv(progress = FALSE)

data_gene_info <- "data/gene_info_library_designer.csv" %>% 
  readr::read_csv(progress = FALSE)

# selectivity / similarity ----
data_affinity_selectivity <- "data/affinity_selectivity_table_ChemblV22_1_20170804.csv" %>% 
  readr::read_csv(progress = FALSE) %>% 
  dplyr::mutate_at(
    vars(c(`mean_Kd_(nM)`, `SD_Kd_(nM)`:offtarget_IC50_N)),
    ~ signif(., 2)
  )

data_similarity <- "data/similarity_table_ChemblV22_1_20170804.csv" %>%
  readr::read_csv(progress = FALSE) %>% 
  dplyr::mutate_at(vars(PFP, TAS, structural_similarity), ~ round(., 2))
