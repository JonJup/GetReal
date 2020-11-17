### --- GETREAL 
### --- Working Package 2 
### --- Diatoms 
### --- Save Sensitivity Analysis plots 


# setup -------------------------------------------------------------------
pacman::p_load(dplyr,ggplot2,here,stringr)
dir = list(load = here("005_documents/plot_scripts/dia_sensitivity_analysis.R"),
           save = here("002_working_package_02/001_community_data/002_combined/001_diatoms/004_plots/sensitivity analysis/")
)

# load plots  -------------------------------------------------------------
source(dir$load)

# save plots --------------------------------------------------------------

ch_plot_files = ls()[grepl(pattern = "^gg", x = ls())]
for (i in seq_along(ch_plot_files)) {
        loop_plot <- get(ch_plot_files[i])
        save_name = ch_plot_files[i] %>% 
                str_remove("^gg_") %>% 
                str_replace("sa_", "sensitivity_analysis_") %>% 
                str_replace("rich", "richness") %>% 
                str_replace("uniq", "uniqueness") %>% 
                str_replace("spe", "species") %>% 
                str_replace("gen", "genus") %>% 
                str_replace("fol", "family_or_lower")  %>% 
                str_replace("dia", "diatom") %>% 
                paste0(".png")
                
                
        ggsave(plot = loop_plot, 
               filename = file.path(dir$save, save_name))
}