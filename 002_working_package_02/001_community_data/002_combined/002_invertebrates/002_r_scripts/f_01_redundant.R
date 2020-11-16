### ----------------------------------------- ###
### --- FUNCTION : -------------------------- ###
### ------------- INVERTEBRATES REDUNDANT --- ### 
### ----------------------------------------- ### 

# date written/ modified: 20.08.20
# date used: 20.08.20
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates 

redundant <- function(x, data){
        riv_x_taxa <- data[group == x, unique(taxon)]
        n_taxa_x   <- length(riv_x_taxa)
        # typical_me[river_type == riv_x, redundancy := sum(typical_me[river_type == riv_x, taxon] %in% typical_me[river_type == riv_y, taxon] /typical_me[river_type == riv_x, .N]) * 100]
        redundancy <- list()
        list_id <- 1
        for (i in 1:length(ch_river_types)){
                riv_y <- ch_river_types[i]
                if (riv_y == x) next()
                riv_y_taxa <- data[group == riv_y, unique(taxon)]
                redundancy[[i]] <-  round(sum(riv_x_taxa %in% riv_y_taxa)/n_taxa_x * 100,1)
                names(redundancy[[i]]) <- riv_y
        }
        redundancy <- unlist(redundancy)
        max_red <- max(redundancy)
        max_id  <- which(redundancy == max_red)
        if (length(max_id) > 1) {
                max_id <- paste(names(max_id), collapse = "+")
                print(paste(x, "is most similiar is", max_id, "with", max_red, "% overlap"))
        } else {
                print(paste(x, "is most similiar to" ,names(max_id), "with", max_red, "% overlap"))
        }
}