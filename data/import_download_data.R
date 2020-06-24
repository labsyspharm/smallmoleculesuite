library(tidyverse)
library(here)
library(synapser)
library(synExtra)

synLogin()
syn <- synDownloader(tempdir(), ifcollision = "overwrite.local")

# set directories, import files ------------------------------------------------
###############################################################################T

syn_parent <- "syn18457321"
release <- "chembl_v25"
fp_name <- "morgan_normal"

dir_dl <- here("www", "sms", "assets", "downloads")
dir.create(dir_dl, showWarnings = FALSE)

syn_tables <- synPluck(syn_parent, release, "db_tables", fp_name) %>%
  synChildren() %>%
  magrittr::extract(str_ends(names(.), fixed(".gz")))

files <- syn(syn_tables)

system2(
  "tar",
  c(
    "-cf",
    file.path(dir_dl, "sms_tables.tar"),
    files[str_ends(files, fixed(".csv.gz"))]
  )
)

file.rename(files[str_ends(files, fixed(".sql.gz"))], file.path(dir_dl, "sms_db.sql.gz"))
