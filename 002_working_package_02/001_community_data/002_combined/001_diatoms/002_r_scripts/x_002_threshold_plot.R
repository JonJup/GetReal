### --- diatom threshold plot --- ### 

# date written: 09.10.20
# getreal 

# setup -------------------------------------------------------------------
pacman::p_load(here, magrittr, data.table, ggplot2, cowplot)

setwd(here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"))
set_all   =readRDS("005_2020-08-17_clean_diatoms_observations.RDS")
dt_genus  =readRDS("006_2020-11-04_taxon_list_genus.RDS")
dt_family =readRDS("006_2020-11-04_taxon_list_family.RDS")
dt_order  =readRDS("006_2020-11-04_taxon_list_order.RDS")

set_all <- set_all[- which(set_all$species == "Fontigonium rectangulare")]
set_all[, c("genus_check2", "genus_new2", "double_checked", "species_new", "species_clean", "comment", "species_old") := NULL]
set_all[, c("final_taxon", "final_taxon_level") := .(character(), character())]

remove_orders <-  dt_order[n_observations <= 10, order_name]
set_all <- set_all[!order %in% remove_orders]

dt_loop_out = data.table(threshold=rep(70:95, each=2),
                         n_observations=numeric(0),
                         n_taxa=numeric(0),
                         taxon_level=rep(c("species","genus"), times=15))

for (l2 in c(95:70)) { #BEGIN FOR-LOOP 1 NOT-NESTED OVER:
                       #Threshold for how many percent of data must be below the current level for it to continue downward.
        
        dt_set_all_loop <- copy(set_all)
        # set cut off data_sets (loop)
        requiered_percent  <- l2
        
        # genus -------------------------------------------------------------------
        genus_lvl_id   <- dt_genus[,species < requiered_percent]
        genus_taxa     <- dt_genus[genus_lvl_id, genus_name]
        
        dt_set_all_loop[genus %in% genus_taxa & is.na(final_taxon_level), 
                c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
        ## -- qa -- ## 
        #dt_set_all_loop[final_taxon_level == "genus"]
        
        ## --    -- ## 
        rm(genus_lvl_id, genus_taxa);gc()
        
        # species -----------------------------------------------------------------
        species_lvl_id <- dt_genus[,species >= requiered_percent]
        species_taxa   <- dt_genus[species_lvl_id, genus_name]
        
        dt_set_all_loop[genus %in% species_taxa & is.na(final_taxon_level), 
                c("final_taxon", "final_taxon_level") := .(species, "species") ]
        
        rm(species_lvl_id, species_taxa);gc()
        
        # left over  --------------------------------------------------------------
        dt_set_all_loop[is.na(final_taxon) & is.na(species) & final_taxon_level == "species" & !is.na(genus), 
                c("final_taxon", "final_taxon_level") := 
                        .(genus, "genus")]
        dt_loop_out[threshold==l2&taxon_level=="species", n_observations:=dt_set_all_loop[final_taxon_level=="species", .N]]
        dt_loop_out[threshold==l2&taxon_level=="genus",   n_observations:=dt_set_all_loop[final_taxon_level=="genus", .N]]
        dt_loop_out[threshold==l2&taxon_level=="species", n_taxa:=dt_set_all_loop[final_taxon_level=="species", length(unique(final_taxon))]]
        dt_loop_out[threshold==l2&taxon_level=="genus",   n_taxa:=dt_set_all_loop[final_taxon_level=="genus", length(unique(final_taxon))]]
        print(l2)
}


dt_loop_out[,total_obs := sum(n_observations), by = threshold]
dt_loop_out[,percent := n_observations/total_obs * 100]


threshplot_total = dt_loop_out %>% 
        ggplot(aes(x = threshold, y = n_observations, col = taxon_level)) + 
        geom_line(size = 1.3)  + 
        scale_y_log10() + 
        xlab(label="number of observations") + 
        xlab("theshold [%]") +
        labs(col = "taxon level") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
threshplot_percent = dt_loop_out %>% 
        ggplot(aes(x = threshold, y = percent, col = taxon_level)) + 
        geom_line(size = 1.3)  + 
        xlab(label="percent of observations") + 
        xlab("theshold [%]") +
        labs(col = "taxon level") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
prow = cowplot::plot_grid(
        threshplot_total   + theme(legend.position = "none"), 
        threshplot_percent + theme(legend.position = "none"),
        labels = c("A", "B"))
legend <- cowplot::get_legend(
        threshplot_percent + theme(legend.position = "bottom")
)
plot_thresh = plot_grid(prow, legend, ncol = 1, rel_heights = c(1,.1))
ggsave(plot_thresh, filename="../004_plots/threshold_plot/threshold_plot_diatoms.jpeg")

