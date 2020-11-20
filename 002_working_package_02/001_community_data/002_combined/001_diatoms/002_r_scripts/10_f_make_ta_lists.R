# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------- Diatoms ------------ ###
### ---------- Make Lists   ---------- ###
# -------------------------------------- #

# 16.11.20
# GetReal
# Working Package 2 
# Diatoms

ch_river_types = unique(dt_bty$group)
copy_list = list()
for (i in seq_along(ch_river_types)){
        var = ch_river_types[i]
        dt_bty[group == var, taxon] %>%
                str_replace_all(pattern = "\\.", "\\ ") %>%
                paste(sep = ",", collapse = ", ") -> ch_temp
        copy_list[[i]] = paste(paste0("## ",ch_river_types[i],"   "), ch_temp, "   ") 
}

copy_list %>% 
        unlist %>% 
        writeClipboard()
