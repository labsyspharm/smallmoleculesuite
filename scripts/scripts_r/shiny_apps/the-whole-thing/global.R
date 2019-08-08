library(shiny)
library(shiny.semantic)
library(dplyr)
library(readr)
library(DT)
library(plotly)
library(crosstalk)
library(shinyjs)
library(magrittr)
library(markdown)
library(clipr)
library(rclipboard)
library(glue)
library(aws.s3)

source("modules/selectivity.R")

zipped_csv <- function(df_list, zippedfile, filenames, stamp) {
  dir = tempdir()
  mkdir = paste0("mkdir ", dir, "/", stamp)
  system(mkdir)
  len = length(df_list)
  for (i in 1:len) {
    # filename in temp directory 
    assign(paste0("temp",i), paste0(dir, "/", stamp, "/", filenames[i], ".csv"))
    # write temp csv
    write_csv(df_list[[i]], path=get(paste0("temp",i)))
  }
  
  # zip temp csv
  print(dir)
  print(filenames)
  
  zip(zippedfile, paste0(dir,"/", stamp, "/", filenames, ".csv"), flags = "-j" )
  # delete temp csv
  for(i in 1:len) {
    unlink( paste0("temp",i) )
  }
}

makeQueryString <- function(lst) {
  paste0("?", paste0(names(lst), "=", unlist(lst), collapse = "&"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
