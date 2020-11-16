# ------------------------------------------ #
### -- redundancy function for diatoms --- ### 
# ------------------------------------------ #

redundant <- function(x){
        riv_x      <- x
        riv_x_taxa <- dt_bty[group == riv_x, unique(taxon)]
        n_taxa_x   <- length(riv_x_taxa)
        # typical_me[river_type == riv_x, redundancy := sum(typical_me[river_type == riv_x, taxon] %in% typical_me[river_type == riv_y, taxon] /typical_me[river_type == riv_x, .N]) * 100]
        redundancy <- list()
        list_id <- 1
        for (i in ch_river_types){
                if (i == x) next()
                #riv_y <- ifelse(i>9, paste0("RT", i), paste0("RT0", i))
                riv_y_taxa <- dt_bty[group == i, unique(taxon)]
                redundancy[[list_id]] <-  round(sum(riv_x_taxa %in% riv_y_taxa)/n_taxa_x * 100,1)
                names(redundancy[[list_id]]) <- i
                list_id <- list_id + 1
        }
        redundancy <- unlist(redundancy)
        max_red <- max(redundancy)
        max_id  <- which(redundancy == max_red)
        if (length(max_id) > 1) {
                max_id <- paste(names(max_id), collapse = "+")
                print(paste(x, "is most similiar is", max_id, "with", max_red))
        } else {
                print(paste(x, " is most similiar is", names(max_id), "with", max_red))
        }
}