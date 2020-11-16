# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Diatoms- ------------------------ ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 02.09.20 + 15.
# date used: 02.09.20 + 08. + 15. + 09.11
# Jonathan Jupke 
# Get Real WP 2
# Diatoms 
# Seasonal changes

# Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr
               )

dir_dia = here("002_working_package_02/003_seasonality/003_results/diatoms/")

# Read in and prepare data ------------------------------------------------
data  = readRDS(file.path(dir_dia, "001_2020-11-09_diatoms_w_rt.RDS"))

data[, final_taxon := str_trim(final_taxon, side = "both")]

# Subset  --------------------------------------------------------------
data$gr_sample_id %<>% as.character()

sub_1 <- data[(is.na(year) | year >= 2000) & !is.na(season)]

unique(sub_1$season)

rt_large = sub_1[rt %in% c("RT1", "RT2", "RT4", "RT5", "RT8", "RT16", "RT17", "RT18", "RT19"), rt := "RT_large"]
rt_10    = sub_1[rt %in% c("RT10")]
rt_11    = sub_1[rt %in% c("RT11")]
rt_14    = sub_1[rt %in% c("RT14")]
rt_15    = sub_1[rt %in% c("RT15")]

ch_14_sites = c(
        "ÉseraPlan de l'Hospital de Benasque",
        "Aguas LimpiasE. Sarra",
        "GállegoBiescas",
        "Noguera PallaresaIsil",
        "Noguera PallaresaLlavorsí",
        "Noguera CardósLladorre",
        "VallfarreraAlins",
        "SegreLlivia",
        "GállegoFormigal",
        "VeralZuriza",
        "BarrosaParzán",
        "Noguera de TorLlesp",
        "UrbiónViniegra de Abajo",
        "MayorAguas Abajo Villoslada de Cameros",
        "AragónCandanchú - Puente de Santa Cristina",
        "AlhamaMagana",
        "Noguera RibagorzanaPont de Suert",
        "Noguera de TortBoi",
        "IreguaPuente De Almarza",
        "LumbrerasVilloslada de Cameros",
        "BrievaBrieva de Cameros",
        "GaronaCampo Golf Salardhu",
        "YnolaUnha",
        "Aiguamoix /Tredos",
        "GaronaArties",
        "GaronaAguas Abajo Aubert",
        "Noguera RibagorzanaSenet",
        "GaronaCasarilh",
        "CaldaresPanticosa",
        "EscarraEscarrilla",
        "IzasHotel Santa Cristina-Antigua central Hidroeléctrica",
        "GállegoEmbalse del Gállego",
        "NajerillaVentrosa (Puente de la Hiedra)",
        "GaronaBossost",
        "GaronaGessa",
        "Noguera de TorBarruera",
        "Noguera RibagorzanaVilaller",
        "SegreAguas abajo de Martinet",
        "ValiraAduana",
        "Noguera PallaresaSant Romá de Tavernoles",
        "ÉseraCamping Aneto",
        "Noguera PallaresaGerri de la Sal",
        "NeilaVillavelayo",
        "CarolPuigcerdá",
        "CHASSEZAC A ST-FREZAL-D'ALBUGES",
        "CHASSEZAC A CHASSERADES",
        "SEGRE A LLO",
        "SEGRE A LLIVIA",
        "REC DE JUELL A ANGOUSTRINE",
        "TOSSA A VALCEBOLLERE",
        "RIU DE QUEROL A ENVEITG 2",
        "RIU DE QUEROL A LATOUR-DE-CAROL 2",
        "RIU DE QUEROL A LATOUR-DE-CAROL 1",
        "RIU LLAVENERA A PALAU-DE-CERDAGNE",
        "ALTIER A CUBIERES 2"
)

rt_large = rt_large[season != "winter" & !data.set %in% c("JJM", "dia_Ecosurv")]
rt_10    = rt_10[season != "spring" & data.set != "dia_Miguel_Iglesias"]
rt_11    = rt_11[season != "spring" & data.set != "dia_Miguel_Iglesias"]
rt_14    = rt_14[season != "winter" & original_site_name %in% ch_14_sites]
rt_15    = rt_15[season != "spring" & data.set != "dia_Miguel_Iglesias"]


rm(sub_1, data, ch_14_sites, dir_dia);gc()

files <- ls()

for (i in seq_along(files)) {
        ld <- get(files[i])
        ld <- ld[,.(gr_sample_id, final_taxon, final_taxon_level, season)] 
        assign(x = files[i],
               value = ld)
        rm(ld, i)
}
 
## -- different levels
for (i in c("spe", "gen")) {
        taxon_var <- ifelse(i == "spe", "species", "genus")
        for (k in files){
                assign(x     = paste0(k, "_", i), 
                       value = get(k)[final_taxon_level %in% taxon_var] 
                )
        }}

 
rm(list = files)
rm (i, k, taxon_var, files);gc()
files <- ls()
 
# Turn to site X species matrix --------------------------------------------------------
 
for (i in files) {
        ld <- get(i)
        ld[, final_taxon_level := NULL]
        ldj <- copy(ld)
        ldj[, final_taxon  := NULL]
        ldj <- unique(ldj, by = "gr_sample_id")
        assign( x = i,
                value = splist2presabs(data = ld, sites.col = 1, sp.col = 2)
                )
         ld2 <- get(i)
         setDT(ld2)
         
         ld3 <- ldj[ld2, on = "gr_sample_id"]
        
        assign(x = i,
               value = ld3)
        if (any(duplicated(ld3$gr_sample_id))) break()
        
        rm(ld,ld2,ld3,i,ldj);gc()

}

# Remove rare taxa --------------------------------------------------------

# rare taxa are taxa that occur in less that 1% of sampling sites. 

dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")
ls_rare = readRDS(file.path(dir_pd, "008_a_rare_names.RDS"))

for (i in 1:length(files)) {
        dt_loop = get(files[i])
        if (str_detect(string=files[i], pattern = "spe")){
                
        } else if (str_detect(string=files[i], pattern = "gen")){
                in_loop_id = which(names(dt_loop) %in% ls_rare[[2]])
                dt_loop = dt_loop[,-in_loop_id, with = FALSE]
        }
        assign(x = files[i],
               value = dt_loop)
        rm(i);gc()
}


# Remove all entries with only two columns i.e. no taxa 
for (i in files) {
        ld <- get(i)
        if (ncol(ld) < 3){
                rm(list = i)
                print(paste("removed", i))
                id <- which(files == i)
                files <- files[-id]
        } 
        rm(i);gc()
}

# Save data to file ---------------------------------------------------

dir_save = "002_working_package_02/003_seasonality/003_results/diatoms/001_speciesXsites_tables/"

for (i in seq_along(files)) {
        save.name = paste0(files[i], ".RDS")
        save.file = get(files[i])
        saveRDS(object = save.file,
                file = file.path(dir_save,
                                 paste0(Sys.Date(),
                                        "_",
                                        save.name)))
} 

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
