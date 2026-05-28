suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(DT)
  library(bigrquery)
  library(bsicons)
})

shiny::shinyAppDir("src")
