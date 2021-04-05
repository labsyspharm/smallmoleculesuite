library(tidyverse)
library(here)
library(synapser)
library(synExtra)
library(withr)

synLogin()

# set directories, import files ------------------------------------------------
###############################################################################T

syn_parent <- "syn18457321"
release <- "chembl_v27"

dir_dl <- here("www", "sms", "assets", "downloads")
dir.create(dir_dl, showWarnings = FALSE)

syn <- synDownloader(dir_dl, ifcollision = "overwrite.local")

syn_tables <- synPluck(syn_parent, release, "db_tables") %>%
  synChildren() %>%
  magrittr::extract(
    str_ends(names(.), fixed(".gz")) &
      !str_detect(names(.), fixed("fingerprints"))
  )

files <- syn(syn_tables)

with_dir(
  tempdir(),
  system2(
    "tar",
    c(
      "-cf",
      file.path(dir_dl, paste0("sms_tables_", release, ".tar")),
      file.path(paste0("sms_tables_", release), basename(files[str_ends(files, fixed(".csv.gz"))]))
    )
  )
)

# file.copy(files[str_ends(files, fixed(".sql.gz"))], file.path(dir_dl, paste0("sms_db_", release, ".sql.gz")))
