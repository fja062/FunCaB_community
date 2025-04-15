# script to run pipeline
library(targets)

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint

targets::tar_visnetwork()

source("libraries.R")
