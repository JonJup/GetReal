# ----------------------------------------------------- #
### --- Typical assemblages mzb - ls_all, species --- ### 
# ----------------------------------------------------- #

#date written\modified : 01.04.20, 30.06. 
#date used: 30.06.20, 01.07., 03.07 
#Jonathan Jupke

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)
setwd(here())

# load data  --------------------------------------------------------------
# load family speciesXsites with ls broad types 
spe = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_spe.RDS")
gen = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_gen.RDS")
fol = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_foh.RDS")

# join data ---------------------------------------------------------------


spe <- spe[,(c(2:24,26)) := NULL]
gen <- gen[,(c(2:24,26)) := NULL]
fol <- fol[,(c(2:24,26)) := NULL]

names_spe <- names(spe)[-c(1,2)]
names_gen <- names(gen)[-c(1,2)]
names_fol <- names(fol)[-c(1,2)]

sg <- spe[gen, on = "gr_sample_id"]
all <- sg[fol, on = "gr_sample_id"]

all[is.na(ls_bd_20) & !is.na(i.ls_bd_20), ls_bd_20 := i.ls_bd_20]
all[is.na(ls_bd_20) & !is.na(i.ls_bd_20.1), ls_bd_20 := i.ls_bd_20.1]
all[, c("i.ls_bd_20", "i.ls_bd_20.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(all))) set(all, which(is.na(all[[j]])), j, 0)

# test that taxa vector will work 
rm(spe, gen, fol, j ,sg);gc()


# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps
good_types   <- paste0("RT", c(4,5,9:13, 16))
medium_types <- paste0("RT", c(1:3, 8, 14, 15, 18))

# Turn river type into factor. 
all$ls_bd_20 %<>% factor

# subset data sets according to the visual categorization of stream type representation
all <- all[ls_bd_20 %in% good_types | ls_bd_20 %in% medium_types]

# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completly NA sign matrix.
all$ls_bd_20 %<>% droplevels()

rm(good_types, medium_types)

# compute indval ----------------------------------------------------------

# increased number of permutations to get smaller p-values 

data_sets <- c("all")

for (k in seq_along(data_sets)) {
        print(paste("K", k, "start @", Sys.time()))
        loop_data <- get(data_sets[k])
        
        for (i in 1:20) {
                print(paste("i", i, "start @", Sys.time()))
                i_print <- ifelse(i < 10, paste0("0",i), i)
                group_var <- paste0("RT", i)
                #if (nrow(mzb[ls_bd_20 == group_var]) < thresh) next()
                if (nrow(loop_data[ls_bd_20 == group_var]) == 0) next()
                assign(x = paste0(data_sets[k],"_sc_", i_print),
                       value = indicators(X = loop_data[,-c(1:2)],
                                          cluster = loop_data$ls_bd_20, 
                                          group = group_var,
                                          max.order = 1, 
                                          verbose = FALSE,
                                          At = 0,
                                          Bt = 0,
                                          func = "IndVal.g", 
                                          control = how(nperm = 999)
                       ))
                rm(group_var)
                print(paste("i", i, "end @", Sys.time()))
        }  
        print(paste("K", k, "end @", Sys.time()))
}; beepr::beep()


files  <- ls()[which(str_detect(ls(), "all_sc_[0-9]*$"))]


loop_list_all   <- list()


lists_vector <- c("loop_list_all")
files_vector <- c("files")

overal_output_list <- list("loop_list_all" = list())


for (k in 1) {
        
        loop_list_files <- files_vector[k] 
        
        for (i in seq_along(get(loop_list_files))) {
                files_loop  <- get(loop_list_files)[i]
                files_loop2 <- data.table( taxon   = row.names(get(files_loop)[[7]]), 
                                           A       = get(files_loop)[[9]],
                                           B       = get(files_loop)[[10]], 
                                           sqrtIV  = get(files_loop)[[11]],
                                           p_value = get(files_loop)[[12]])
                
                files_loop2[, n_taxa := unlist(lapply(str_extract_all(string = files_loop2$taxon, pattern = "\\+"), 
                                                      FUN = length))]
                files_loop2[, n_taxa := n_taxa + 1]
                files_loop2[,group := paste0("RT", str_extract(files_loop,pattern = "[0-9]*$"))]
                
                overal_output_list[[k]][[i]] <- files_loop2
        }
}


loop_list_all   <- overal_output_list[[1]]


loop_dt_all   <- rbindlist(loop_list_all)


spe  <- loop_dt_all[taxon %in% names_spe]
gen  <- loop_dt_all[taxon %in% names_gen]
fol  <- loop_dt_all[taxon %in% names_fol]

nrow(loop_dt_all) == sum(nrow(gen), nrow(spe), nrow(fol))

# save to file ------------------------------------------------------------
setwd("003_processed_data/")

saveRDS(object = spe,   file = paste0("006_",Sys.Date(),"_indicator_spe.RDS"))
saveRDS(object = gen,   file = paste0("006_",Sys.Date(),"_indicator_gen.RDS"))
saveRDS(object = fol,   file = paste0("006_",Sys.Date(),"_indicator_foh.RDS"))

if (readline("delete all? ") == "yes") rm(list = ls())
