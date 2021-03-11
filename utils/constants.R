SELECTIVITY_ORDER <- c(
  most_selective = "Most selective",
  semi_selective = "Semi-selective",
  poly_selective = "Poly-selective",
  unknown_selective = "Unknown",
  other_selective = "Other"
)

SELECTIVITY_COLORS <- c("#225ea8", "#41b6c4", "#a1dab4", "#969696", "#cccccc") %>%
  set_names(SELECTIVITY_ORDER)

TAS_COLORS <- c(`1` = "#b2182b", `2` = "#ef8a62", `3` = "#fddbc7", `10` = "#d9d9d9")

COLUMN_SPECS <- read_csv(here("data", "column_specs.csv"))

DT_DOM <- '<"row justify-content-between"<"col-sm-12 col-md-auto"B><"col-sm-12 col-md-auto"l><"col-sm-12 col-md-auto ml-md-auto"f>><"row"<"col-sm-12"t>><"row"<"col-sm-12 col-md-5"i><"col-sm-12 col-md-7"p>>'
