# --------------------------- #
### ------- Function ------ ###
### - Boxplot with jitter - ###
# --------------------------- #

# date written: 23.09.20 
# date changed: 

#' @param dataset dataset from which the data should be plotted 
#' @param variable variable which should be plotted against river type 
#' @param nogen boolean; should taxa where traits are only available at the genus level be omitted? 
#' @param nodub boolean; should taxa that occur in multiple TAs be omitted? 
#' @return a plot object 


bwjp <- function(dataset = dt_dia_trait_sub, variable, nogen = F, nodub = F){
        
        dt_fun_pre <- copy(dataset)
        
        if(nogen) dt_fun_pre <- dt_fun_pre[is.na(ll_traits)]
        if (nodub) {
                vec_dub_id <- duplicated(dt_fun_pre$taxon)
                vec_dub_names <- dt_fun_pre$taxon[vec_dub_id]
                dt_fun_pre <- dt_fun_pre[!taxon %in% vec_dub_names]
        }
        dt_fun_sub <- dt_fun_pre[,c((variable), "group"), with = F]
        
        names(dt_fun_sub) <- c("var", "group")
        dt_fun_sub %>%
                ggplot(aes(x = group, y = var, fill = group)) +
                geom_jitter(width = .1, height = .1, shape = 21) +
                geom_boxplot( alpha = 0.2) +
                theme_minimal() +
                theme(legend.position = "none", 
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())
        
}
