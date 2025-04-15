# script to run pipeline
library(targets)

targets::tar_make()
tar_load_everything()

targets::tar_visnetwork()

source("libraries.R")
