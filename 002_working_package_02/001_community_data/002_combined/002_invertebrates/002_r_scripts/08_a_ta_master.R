# ------------------------------- #
### --- TA Master Scripts  --- ### 
# ------------------------------ #

#date written\modified : 20.11.20
# Jonathan Jupke
# GetReal
# Working Package 2 
# Macroinvertebrates

pacman::p_load(data.table, 
               dplyr,
               here, 
               indicspecies, 
               magrittr,   
               stringr)

DIR = list(pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"),
           rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/"))

OPT = list(save_one = TRUE)

# settings ----------------------------------------------------------------

x_ls_thesholds = list(spe = list(a = .90, b = .20, b2 = 0.05),
                      gen = list(a = .85, b = .40, b2 = 0.10),
                      foh = list(a = .80, b = .60, b2 = 0.15))

x_ls_combine = list(c(2,3), c(4,5), c(8,9,10,11,15,16))
#x_ls_combine = list(c(2,3),c(4,5), c(8,9,10,11), c(15,16))


# compute indvals ---------------------------------------------------------
source(file.path(DIR$rs, "08_b_compute_indvals.R"))

# setup ta analysis -------------------------------------------------------
source(file.path(DIR$rs, "08_c_setup_ta_analysis.R"))

# redundancy analysis -----------------------------------------------------
source(file.path(DIR$rs, "08_d_redundancy.R"))

# sensitivity analysis -----------------------------------------------------
# source(file.path(DIR$rs, "08_e_sensitivity_analysis.R"))

# make lists -----------------------------------------------------
source(file.path(DIR$rs, "08_f_make_ta_lists.R"))
