# --------------------------------------- #
### --- Typical assemblages Diatoms --- ### 
# --------------------------------------- #

#date written\modified : 04.11.
#date used: 04.11. 
#Jonathan Jupke
# GetReal
# Working Package 2 
# Diatoms  

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr,
               here, 
               indicspecies, 
               magrittr,   
               stringr)

DIR = list(pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"),
           rs = here("002_working_package_02/001_community_data/002_combined/001_diatoms/002_r_scripts/"))

OPT = list(save_one = TRUE)


# compute indvals ---------------------------------------------------------
source(file.path(DIR$rs, "10_b_compute_indvals.R"))

# setup TAs ---------------------------------------------------------------
source(file.path(DIR$rs, "10_c_setup_ta_analysis.R"))

# redundancy --------------------------------------------------------------
source(file.path(DIR$rs, "10_d_redundancy.R"))

# sensitivity analysis -----------------------------------------------------
# source(file.path(DIR$rs, "10_e_sensitivity_analysis.R"))

# redundancy analysis -----------------------------------------------------
source(file.path(DIR$rs, "10_f_make_ta_lists.R"))

