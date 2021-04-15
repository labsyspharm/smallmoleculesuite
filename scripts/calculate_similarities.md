# Compute compound similarity metrics

Run `install.packages(remotes)`, if not already installed.

Download the following files from synapse and place them in the same
folder as this script.

-   <https://www.synapse.org/#!Synapse:syn24986654>
-   <https://www.synapse.org/#!Synapse:syn24986663>
-   <https://www.synapse.org/#!Synapse:syn25163837>
-   <https://www.synapse.org/#!Synapse:syn24986667>

## Load packages

``` r
if(!require(fst)) install.packages("fst")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table")
if(!require(morgancpp)) remotes::install_github("labsyspharm/morgancpp")

library(fst)
library(tidyverse)
library(data.table)
library(morgancpp)
```

## Load data

``` r
data_tas <- read_fst("shiny_tas.fst", as.data.table = TRUE)
data_compound_names <- read_fst("shiny_compound_names.fst", as.data.table = TRUE)
data_fingerprints <- MorganFPS$new("shiny_fingerprints.bin", from_file = TRUE)
data_compounds <- read_fst("shiny_compounds.fst", as.data.table = TRUE)
```

## Similarity functions

``` r
tas_weighted_jaccard <- function(query_ids, target_ids = NULL, min_n = 6) {
  query_ids <- convert_compound_ids(query_ids)
  target_ids <- convert_compound_ids(target_ids)

  query_tas <- data_tas[
    lspci_id %in% query_ids,
    .(query_lspci_id = lspci_id, lspci_target_id, tas)
  ]
  target_tas <- data_tas[
    if (is.null(target_ids)) TRUE else lspci_id %in% target_ids,
    .(target_lspci_id = lspci_id, lspci_target_id, tas)
  ]
  target_tas[
    query_tas,
    on = .(lspci_target_id),
    nomatch = NULL
  ][
    ,
    mask := tas < 10 | i.tas < 10
  ][
    ,
    if (sum(mask) >= min_n) .(
      "tas_similarity" = sum(pmin(tas[mask], i.tas[mask])) / sum(pmax(tas[mask], i.tas[mask])),
      "n" = sum(mask),
      "n_prior" = .N
    ) else .(
      tas_similarity = double(),
      n = integer(),
      n_prior = integer()
    ),
    by = .(query_lspci_id, target_lspci_id)
  ] %>%
    merge_compound_names()
}

chemical_similarity <- function(query_ids, target_ids = NULL) {
  query_ids <- convert_compound_ids(query_ids)
  target_ids <- convert_compound_ids(target_ids)

  query_ids %>%
    set_names() %>%
    map(
      data_fingerprints$tanimoto_all
    ) %>%
    map(setDT) %>%
    rbindlist(idcol = "query_lspci_id") %>% {
      .[
        ,
        .(
          query_lspci_id = as.integer(query_lspci_id),
          target_lspci_id = id,
          structural_similarity
        )
      ][
        if (is.null(target_ids)) TRUE else target_lspci_id %in% target_ids
      ]
    } %>%
    merge_compound_names()
}

find_compound_ids <- function(compound_names) {
  if (is.null(compound_names))
    return(NULL)
  key_match <- map(
    compound_names,
    ~str_detect(
      data_compound_names[["name"]], fixed(.x, ignore_case = TRUE)
    )
  ) %>%
    reduce(`|`)
  data_compound_names[
    key_match
  ][
    ,
    match_len := str_length(name)
  ][
    order(
      match_len
    )
  ][
    ,
    .(name = head(name, 1)),
    by = .(lspci_id)
  ] %>%
    unique()
}

merge_compound_names <- function(df) {
  reduce(
    array_branch(str_match(names(df), "^(.*)lspci_id$"), margin = 1),
    function(df, match) {
      lspci_id_col <- match[1]
      compound_col <- paste0(match[2], "compound")
      if (any(is.na(c(lspci_id_col, compound_col))))
        return(df)
      merge(
        df,
        data_compounds[lspci_id %in% df[[lspci_id_col]]][
          , .(lspci_id, pref_name)
        ] %>%
          setnames("pref_name", compound_col),
        by.x = lspci_id_col, by.y = "lspci_id", all = FALSE
      )
    }, .init = df
  )
}

convert_compound_ids <- function(ids) {
  if (is.numeric(ids))
    # Assume it's already lspci_ids
    ids
  else {
    find_compound_ids(ids)[["lspci_id"]]
  }
}
```

## Examples

In these examples, we compute the target similarity (TAS similarity)
between Ruxolitinib and two other compounds, or with all other
compounds.

``` r
tas_weighted_jaccard("ruxolitinib", c("tofacitinib", "ruxolitinib"))
```

    ##    target_lspci_id query_lspci_id tas_similarity   n n_prior query_compound
    ## 1:           66153          66153      1.0000000 162     469    RUXOLITINIB
    ## 2:           78036          66153      0.3897716 288     606    RUXOLITINIB
    ##    target_compound
    ## 1:     RUXOLITINIB
    ## 2:     TOFACITINIB

``` r
tas_weighted_jaccard("ruxolitinib")
```

    ##       target_lspci_id query_lspci_id tas_similarity   n n_prior query_compound
    ##    1:            2373          66153      0.3055556  10      11    RUXOLITINIB
    ##    2:            3261          66153      0.3289474   9      17    RUXOLITINIB
    ##    3:            4623          66153      0.3157895   6       6    RUXOLITINIB
    ##    4:            4646          66153      0.2572115  43      55    RUXOLITINIB
    ##    5:            5052          66153      0.3510815 189     228    RUXOLITINIB
    ##   ---                                                                         
    ## 3730:        18401681          66153      0.2105263  13      13    RUXOLITINIB
    ## 3731:        19136561          66153      0.2727273   9       9    RUXOLITINIB
    ## 3732:        19982143          66153      0.1698113   6       6    RUXOLITINIB
    ## 3733:        19982145          66153      0.1886792   6       6    RUXOLITINIB
    ## 3734:        20262294          66153      0.3260870   6       6    RUXOLITINIB
    ##       target_compound
    ##    1:    ELLAGIC ACID
    ##    2:      NORHARMANE
    ##    3:           IQ-1S
    ##    4:    CHEMBL289959
    ##    5:       SP-600125
    ##   ---                
    ## 3730:   CHEMBL4168305
    ## 3731:   CHEMBL4284499
    ## 3732:   CHEMBL4164334
    ## 3733:   CHEMBL4174988
    ## 3734:   CHEMBL4294760

Same as above, except we calculate chemical similarities based on the
Morgan fingerprints of each compound.

``` r
chemical_similarity("ruxolitinib", c("tofacitinib", "ruxolitinib"))
```

    ##    target_lspci_id query_lspci_id structural_similarity query_compound
    ## 1:           66153          66153             1.0000000    RUXOLITINIB
    ## 2:           78036          66153             0.1777778    RUXOLITINIB
    ##    target_compound
    ## 1:     RUXOLITINIB
    ## 2:     TOFACITINIB

``` r
chemical_similarity("ruxolitinib")
```

    ##          target_lspci_id query_lspci_id structural_similarity query_compound
    ##       1:               2          66153            0.00000000    RUXOLITINIB
    ##       2:               3          66153            0.01851852    RUXOLITINIB
    ##       3:               4          66153            0.00000000    RUXOLITINIB
    ##       4:               5          66153            0.00000000    RUXOLITINIB
    ##       5:               6          66153            0.01818182    RUXOLITINIB
    ##      ---                                                                    
    ## 1755201:        20527365          66153            0.09489051    RUXOLITINIB
    ## 1755202:        20527366          66153            0.09629630    RUXOLITINIB
    ## 1755203:        20527400          66153            0.08173077    RUXOLITINIB
    ## 1755204:        20527414          66153            0.08247423    RUXOLITINIB
    ## 1755205:        20527435          66153            0.09230769    RUXOLITINIB
    ##                     target_compound
    ##       1:    TETRAMETHYLAMMONIUM ION
    ##       2:              CHEMBL2009828
    ##       3:          TRIMETHYLAMMONIUM
    ##       4: Trimethyl-sulfonium iodide
    ##       5:                   INOSITOL
    ##      ---                           
    ## 1755201:              CHEMBL2058991
    ## 1755202:              CHEMBL2058767
    ## 1755203:                 RapaLink-1
    ## 1755204:              CHEMBL4216357
    ## 1755205:              CHEMBL2058187
