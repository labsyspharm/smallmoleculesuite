library(magrittr)
library(dplyr)
library(data.table)
library(readr)
library(fst)
library(here)
library(plotly)
library(crosstalk)
library(htmltools)
library(markdown)
library(glue)
library(purrr)
library(rlang)
library(morgancpp)
library(yonder)
library(stringr)
library(DT)
library(shinycssloaders)
library(writexl)
# library(future)

# plan(multicore)

# source("awspass.config")

# Components used in multiple apps
source("modules/components/chembl_tabs.R", local = TRUE)
source("modules/components/affinity_tables.R", local = TRUE)
source("modules/components/download_buttons.R", local = TRUE)
source("modules/components/reference_modal.R", local = TRUE)
source("modules/components/filter_commercial.R", local = TRUE)
source("modules/components/select_compounds.R", local = TRUE)
source("modules/components/dt_header_tooltip.R", local = TRUE)
source("modules/components/modal_column.R", local = TRUE)
source("modules/components/select_targets.R", local = TRUE)

# Modules
source("modules/selectivity.R", local = TRUE)
source("modules/similarity.R", local = TRUE)
source("modules/library.R", local = TRUE)
source("modules/binding.R", local = TRUE)

source("data/load.R", local = TRUE)

source("utils/utils.R", local = TRUE)
source("utils/constants.R", local = TRUE)

source("app_ui.R", local = TRUE)


# enableBookmarking(store = "url")

