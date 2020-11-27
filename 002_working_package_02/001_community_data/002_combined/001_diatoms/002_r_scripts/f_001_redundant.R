# ------------------------------------------ #
### -- redundancy function for diatoms --- ### 
# ------------------------------------------ #

redundant <- function(x){
        
        ch_rt         = unique(x$group)
        ma_redundancy = matrix(data = 0, ncol = length(ch_rt), nrow = length(ch_rt))
        colnames(ma_redundancy) = ch_rt
        rownames(ma_redundancy) = ch_rt
        diag(ma_redundancy) = 100
        
        for (fi in seq_along(ch_rt)){
                loop_rt = ch_rt[fi]
                riv_x_taxa = x[group == loop_rt, unique(taxon)]  
                n_taxa_x   <- length(riv_x_taxa)
                
                for (fj in seq_along(ch_rt)){
                        if (fj == fi) next()
                        loop_rt2 = ch_rt[fj]
                        riv_y_taxa <- x[group == loop_rt2, unique(taxon)]
                        ma_redundancy[fi, fj] <-  round(sum(riv_x_taxa %in% riv_y_taxa)/n_taxa_x * 100,1)
                }
        }
        
        ma_redundancy
}