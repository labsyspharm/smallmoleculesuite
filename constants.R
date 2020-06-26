SELECTIVITY_ORDER <- c(
  "Most selective",
  "Semi-selective",
  "Poly-selective",
  "Unknown",
  "Other"
)

SELECTIVITY_COLORS <- c("#225ea8", "#41b6c4", "#a1dab4", "#969696", "#cccccc") %>%
  set_names(SELECTIVITY_ORDER)

TAS_COLORS <- c(`1` = "#b2182b", `2` = "#ef8a62", `3` = "#fddbc7", `10` = "#d9d9d9")

COLUMN_TITLE_MAP <- c(
  "name" = "Compound",
  "chembl_id" = "ChEMBL ID",
  "structural_similarity" = "Structural similarity",
  "tas_similarity" = "TAS similarity",
  "pfp_correlation" = "Phenotypic correlation",
  "selectivity_class" = "Selectivity class",
  "affinity_Q1" = "Affinity (nM)",
  "offtarget_affinity_Q1" = "Offtarget affinity (nM)",
  "selectivity" = "Selectivity",
  "references" = "References",
  "symbol" = "Gene symbol",
  "pref_name" = "Compound",
  "max_phase" = "Clinical phase",
  "affinity_N" = "N measurements",
  "gene_id" = "Gene ID",
  "reason_included" = "Reason included",
  "tas" = "TAS",
  "source" = "Source",
  "measurement" = "Affinity measurement",
  "unit" = "Measurement unit"
)

DT_DOM <- '<"row justify-content-between"<"col-sm-12 col-md-auto"B><"col-sm-12 col-md-auto"l><"col-sm-12 col-md-auto .ml-auto"f>><"row"<"col-sm-12"t>><"row"<"col-sm-12 col-md-5"i><"col-sm-12 col-md-7"p>>'
