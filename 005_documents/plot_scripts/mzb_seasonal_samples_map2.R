ch_col  = c("#d95f02", "#666666", "#5f64ff", "#dcce00")
source(textConnection(readLines(file.path(dir_sa_inv_rs,"01_explore_stream_types.R"))[c(28:49)]))
dt_data = readRDS(file.path(dir_sa_inv, "base_data_seasons.RDS"))
source(textConnection(readLines(file.path(dir_sa_inv_rs,"01_explore_stream_types.R"))[c(238:239)]))
source(textConnection(readLines(file.path(dir_sa_inv_rs,"01_explore_stream_types.R"))[c(244:248)]))

