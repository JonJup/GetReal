# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Diatoms- ------------------------ ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 02.09.20 + 15.
# date used: 02.09.20 + 08. + 15. 
# Jonathan Jupke 
# Get Real WP 2
# Diatoms 
# Seasonal changes? 

# Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr
               )

setwd(here())

# Read in and prepare data ------------------------------------------------
data  = readRDS("003_results/diatoms/001_2020-09-01_diatoms_w_rt.RDS")
data[, final_taxon := str_trim(final_taxon, side = "both")]

# Subset  --------------------------------------------------------------
data$gr_sample_id %<>% as.character()

sub_1 <- data[(is.na(year) | year >= 2000) & !is.na(season)]

unique(sub_1$season)

rt08 <- sub_1[rt == "RT8"]
rt10 <- sub_1[rt == "RT10"] 
rt11 <- sub_1[rt == "RT11"] 
rt14 <- sub_1[rt == "RT14"]
rt15 <- sub_1[rt == "RT15"]
rt16 <- sub_1[rt == "RT16"]
rt17 <- sub_1[rt == "RT17"]
rt18 <- sub_1[rt == "RT18"]
rt19 <- sub_1[rt == "RT19"]

rt14_sites <- c("ÉseraPlan de l'Hospital de Benasque","Aguas LimpiasE. Sarra","GállegoBiescas","Noguera PallaresaIsil","Noguera PallaresaLlavorsí","Noguera CardósLladorre","VallfarreraAlins","SegreLlivia","GállegoFormigal","VeralZuriza","BarrosaParzán","Noguera de TorLlesp","UrbiónViniegra de Abajo","MayorAguas Abajo Villoslada de Cameros","AragónCandanchú - Puente de Santa Cristina","AlhamaMagana","Noguera RibagorzanaPont de Suert","Noguera de TortBoi","IreguaPuente De Almarza","LumbrerasVilloslada de Cameros","BrievaBrieva de Cameros","GaronaCampo Golf Salardhu","YnolaUnha","Aiguamoix /Tredos","GaronaArties","GaronaAguas Abajo Aubert","Noguera RibagorzanaSenet","GaronaCasarilh","CaldaresPanticosa","EscarraEscarrilla","IzasHotel Santa Cristina-Antigua central Hidroeléctrica","GállegoEmbalse del Gállego","NajerillaVentrosa (Puente de la Hiedra)","GaronaBossost","GaronaGessa","Noguera de TorBarruera","Noguera RibagorzanaVilaller","SegreAguas abajo de Martinet","ValiraAduana","Noguera PallaresaSant Romá de Tavernoles","ÉseraCamping Aneto","Noguera PallaresaGerri de la Sal","NeilaVillavelayo","CarolPuigcerdá","CHASSEZAC A ST-FREZAL-D'ALBUGES","CHASSEZAC A CHASSERADES","SEGRE A LLO","SEGRE A LLIVIA","REC DE JUELL A ANGOUSTRINE","TOSSA A VALCEBOLLERE","RIU DE QUEROL A ENVEITG 2","RIU DE QUEROL A LATOUR-DE-CAROL 2","RIU DE QUEROL A LATOUR-DE-CAROL 1","RIU LLAVENERA A PALAU-DE-CERDAGNE","ALTIER A CUBIERES 2")
rt16_sites <- c("REBENTY A MERIAL 1","EBRE A FONT-ROMEU-ODEILLO-VIA","RUISSEAU DE BOLQUERE A FONT-ROMEU-ODEILLO-VIA","ANGUST A FONT-ROMEU-ODEILLO-VIA","ANGUST A ESTAVAR 1","REC DE L'ESTAHUJA A ESTAVAR","RAHUR A BOURG-MADAME","SEGRE A BOURG-MADAME 2","RIU DE QUEROL A PORTE-PUYMORENS","TECH A PRATS-DE-MOLLO-LA-PRESTE 1","TECH A PRATS-DE-MOLLO-LA-PRESTE 2","RIU FERRER A ARLES-SUR-TECH","TET A SAUTO","ROTJA A PY","CASTELLANA A CATLLAR","LENTILLA A FINESTRET","AUDE A PUYVALADOR 3","AUDE A LES-ANGLES 1","AUDE A MATEMALE","GALBE A FRONTRABIOUSE","RUISSEAU DE GALBE A PUYVALADOR","AUDE A ESCOULOUBRE 1","AIGUETTE A STE-COLOMBE-SUR-GUETTE 2","AUDE A AXAT 2","REBENTY A CAILLA","AUDE A BELVIANES-ET-CAVIRAC","AUDE A CAMPAGNE-SUR-AUDE 2","BRUYANTE A ROUZE","AIGUETTE A STE-COLOMBE-SUR-GUETTE 3")        
rt17_sites <- c("REGINO A OCCHIATANA 2","ALISO A OLETTA 2","GOLO A CAMPILE","GOLO A VOLPAJOLA","GOLO A LUCCIANA 3","FIUM ALTO A PENTA-DI-CASINCA","TAVIGNANO A ANTISANTI","TAVIGNANO A ANTISANTI 1","TAVIGNANO A ALERIA 1","TAGNONE A ALERIA","FIUMORBO A GHISONACCIA","RIZZANESE A SARTENE","TARAVO A CASALABRIVA","TARAVO A URBALACONE","PRUNELLI A BASTELICACCIA","LIAMONE A ARBORI","LIAMONE A MURZO","FANGO A GALERIA","FANGO A MANSO")
rt18_sites <- c("TARTAGINE A CASTIFAO","ASCO A MOLTIFAO","TAVIGNANO A ALTIANI","TARAVO A FORCIOLO" )
rt19_sites <- c("LURI A LURI","RUISSEAU DE PIETRACORBARA A PIETRACORBARA","ALISO A SAN-GAVINO-DI-TENDA","BEVINCO A RUTALI","FIUME SECCU A MONTEGROSSO","CASALUNA A GAVIGNANO","BRAVONE A PIANELLO","RIVIERE DU BUSSO A NOVALE","ALESANI A SAN-GIULIANO 2","BRAVONE A LINGUIZZETTA","RU D'AITONE A EVISA","CRUZINI A AZZANA","GRAVONE A BOCOGNANO 1","FIUMORBO A GHISONI","PRUNELLI A BASTELICA 1","ABATESCO A SERRA-DI-FIUMORBO","TRAVO A VENTISERI","FIUMICELLU A FORCIOLO","CAVO A ZONZA 2","RUISSEAU DE VENTILEGNE A BONIFACIO")

rt08 <- rt08[data.set == "dia_Naiades" & season != "spring"]                    
rt10 <- rt10[season != "spring" & data.set != "dia_Miguel_Iglesias"]
rt11 <- rt11[season != "spring" & data.set != "dia_Miguel_Iglesias"]
rt14 <- rt14[original_site_name %in% rt14_sites & season != "winter"]
rt15 <- rt15[!season %in% c("spring") & data.set != "dia_Miguel_Iglesias"]
rt16 <- rt16[original_site_name %in% rt16_sites & season != "winter"]
rt17 <- rt17[!original_site_name %in% rt17_sites & data.set != "dia_Miguel_Iglesias"]
rt18 <- rt18[!original_site_name %in% rt18_sites & !season %in% c("spring", "winter")]
rt19 <- rt19[!original_site_name %in% rt19_sites & season != "winter"]

rm(sub_1, data, rt14_sites, rt16_sites, rt17_sites, rt18_sites, rt19_sites);gc()

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
# i.e. < 5 taxa per site or < 5 occurrences in data set 

# -- rare taxa -- #

for (i in files) {
        
        ld <- get(i)
        rare_taxon_id <- which(colSums(x = ld[,-c(1,2)]) < 5) + 1
        rare_names    <- names(rare_taxon_id)
        if (length(rare_names) != 0) {
                assign(x     = i,
                       value = ld[, -rare_names, with = FALSE])
                rm(rare_taxon_id, rare_names, ld);gc()
        } else {
                rm(rare_names, rare_taxon_id, ld);gc()
        }
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

setwd("003_results/diatoms/")

for (i in seq_along(files)) {
        save.name = paste0(files[i],".RDS")
        save.file = get(files[i])
        saveRDS(
                object = save.file,
                file = paste0("001_speciesXsites_tables/",
                              Sys.Date(),
                              "_",
                              save.name)
        )
} 

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
