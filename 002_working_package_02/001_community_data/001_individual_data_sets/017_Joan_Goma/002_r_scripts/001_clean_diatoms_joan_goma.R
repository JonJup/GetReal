# ---------------------------------------- #
### --- Clean Diatom data Joan Gomà --- ### 
# ---------------------------------------- #

# GR WP2 
# Cleaning/ Homogenization of diatom data from Joan Gomà
# EPSG: 23031 -- ED50 / UTM zone 31N

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###


# 01. Setup ---------------------------------------------------------------
pacman::p_load(dplyr, 
               data.table, 
               lubridate,
               magrittr, 
               sf, 
               stringr,
               taxize,
               tmap)
tmap_mode("view")

setwd(here::here("17_Joan_Gomà/"))
data <- readxl::read_excel("01_OriginalData/Diatoms CAT JGomà.xlsx") %>% 
        setDT
env.data = readxl::read_excel("01_OriginalData/Diatoms CAT JGomà.xlsx", sheet = 2) %>% 
        setDT
site.data <- readxl::read_excel("01_OriginalData/Diatoms CAT JGomà.xlsx", sheet = 3, skip = 1) %>%
        setDT


# 02. Cleaning ------------------------------------------------------------
names(data)[1:5] <- c("site_id", "id", "season_year", "site","date2")
names(site.data) = c("id","Region","stream", "site","x.coord","y.coord", "s", "a", "d")

data[1,1:5] <- NA

# The origin is always the same for excel data. See
# https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-with-lubridate
data[,date := as.Date(as.numeric(data$date2), origin = "1899-12-30")]

taxonnames = names(data)[6:158] %>% as.character
omnida.codes =   data[1,6:158]  %>% as.character

tempOmnidia = data.table("NameJG" = taxonnames, "OmnidiaCode" = omnida.codes)

## load table with Omnida Codes
ominda.file = readxl::read_excel("../03_Saul_Blanco/01_data_raw/OMNIDIAFILE.xls", skip = 1) %>% setDT
ominda.file2 = ominda.file[, c("CODE","DENOM")]

joinOmnidia = left_join(tempOmnidia, ominda.file2, by = c("OmnidiaCode" = "CODE")) %>% setDT
#joinOmnidia[,1] = as.character(joinOmnidia[,1])

# fill NA in new coulmn with info from old column
for (i in 1:nrow(joinOmnidia)) {
        
        if ( is.na(joinOmnidia[i,3]) ) {
                
                joinOmnidia[i,3] = joinOmnidia[i,1]
                
        }
        
}

joinOmnidia = joinOmnidia[,-1]
names(joinOmnidia) = c("Code","taxon")
joinOmnidia[Code == "FRUS", taxon := "Frustulia"]
joinOmnidia[Code == "MAST", taxon := "Mastogloia"]
joinOmnidia[Code == "RHOP", taxon := "Rhopalodia"]

names(data)[6:158] = joinOmnidia[,1] %>% pull() %>% as.vector()
data = data[-1,]
data2 = tidyr::gather(data,Taxon, Abundanz, -c(1:5, 159)) %>% setDT
data2 = data2[Abundanz != 0]
data3 = left_join(data2, joinOmnidia, by = c("Taxon" = "Code")) %>% setDT
data3[,c("Taxon") := NULL]
data3[, c("year", "season") := 
              list(year(date),
      case_when(
              month(date) %in% c(12,1,2) ~ "winter",
              month(date) %in% c(3,4,5) ~ "spring",
              month(date) %in% c(6,7,8) ~ "summer",
              month(date) %in% c(9,10,11) ~ "autumn"))]


saveRDS(data3, "03_FinalData/01_191210_data_before_taxon_clean_Joan_Goma.RDS")
data3 = readRDS("03_FinalData/01_191210_data_before_taxon_clean_Joan_Goma.RDS")

# subset; remove middle whitespace from id so its equal to that in species data.
site.data2 = site.data[,
                       list(
                               site_id = str_remove_all(id, "\ "),
                               site,
                               stream,
                               x.coord,
                               y.coord
                       )
                       ]


# Taxonomical data --------------------------------------------------------

TU = data3$taxon %>% unique %>% sort

classification.object = classification(TU, db = "gbif")

# save/ load classification.object as backup
saveRDS(classification.object, "03_FinalData/02_191210_classification.object_Joan_Goma.RDS")
classification.object = readRDS("03_FinalData/02_191210_classification.object_Joan_Goma.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)



taxontable[order != "NA", clean := T]
response.vector = NULL
response = c()

for (i in 1:nrow(taxontable)) {
        
        # skip if clean
        if (taxontable$clean[i] == TRUE) {
                next()
        }
        # fill tax object 
        tax = classification.object[taxontable$taxon[i]]
        
        # is a response vector loaded?
        if (!(is.null(response.vector))) {
                response[i] = response.vector[i]
                
                # decline if tax is NA 
        } else if (is.na(tax)) {
                response[i] = ""
                
                # look if the found species is found in the original name 
        } else if (str_detect( names(tax), tax[[1]][nrow(tax[[1]]),1])) {
                
                response[i] = "y"
                
        } else {
                cat(
                        "",
                        "\n",
                        "\ ",
                        names(tax),
                        "\n",
                        "\ ",
                        ifelse(
                                "species" %in% tax[[1]][, 2],
                                tax[[1]][which(tax[[1]][, 2] == "species"), 1],
                                ifelse(
                                        "genus" %in% tax[[1]][, 2],
                                        tax[[1]][which(tax[[1]][, 2] == "genus"), 1],
                                        ifelse("family" %in% tax[[1]][, 2],
                                               tax[[1]][which(tax[[1]][, 2] == "family"), 1],  tax[[1]][1])
                                )
                        ),
                        fill = T
                )
                
                response[i] = readline("good?")
        }
        
        if (response[i] == "y") {
                taxontable$clean[i] = T
                if ("species" %in% tax[[1]]$rank) {
                        spe.row = which(tax[[1]]$rank == "species")
                        taxontable[i, species := tax[[1]]$name[spe.row]]
                }
                if ("genus" %in% tax[[1]]$rank) {
                        spe.row = which(tax[[1]]$rank == "genus")
                        taxontable[i, genus := tax[[1]]$name[spe.row]]
                }
                if ("family" %in% tax[[1]]$rank) {
                        spe.row = which(tax[[1]]$rank == "family")
                        taxontable[i, family := tax[[1]]$name[spe.row]]
                }
                if ("order" %in% tax[[1]]$rank) {
                        spe.row = which(tax[[1]]$rank == "order")
                        taxontable[i, order := tax[[1]]$name[spe.row]]
                }
        }
        if (response[i] == "break")
                break()
}

saveRDS(taxontable, "03_FinalData/03_191210_initial_taxon_clean_Joan_Goma.RDS")
taxontable = readRDS("03_FinalData/03_191210_initial_taxon_clean_Joan_Goma.RDS")

# run cleaning scripts ...synonyms for synonyms and gbif_errors for taxa that
# gbif does not know or erroneously rates as Synonyms.
clean_diatoms_synonyms.R
clean_gbif_errors.R

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F]

# save taxontable 
saveRDS(taxontable, "03_FinalData/04_191210_post_correction_taxontable_Joan_Goma.RDS")
taxontable = readRDS("03_FinalData/04_191210_post_correction_taxontable_Joan_Goma.RDS")
# add taxonomical information to data 
data4 = data3[taxontable, on = "taxon"]


# not in algaebase: Naviculadicta fennica
data4[species == "NA", taxon] %>% unique %>% sort
data4[genus == "NA", taxon] %>% unique %>% sort
data4[family == "NA"] %>% unique(by = "taxon")
data4[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")  

data4[species == "NA", species := NA]
data4[genus == "NA", genus := NA]
data4[family == "NA", family := NA]
data4[order == "NA", order := NA]


source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
dia.orders = unique(data4[, order])
index = dia.orders %in% accepted.diatoms
dia.orders[!index] %>% sort

saveRDS(data4, "03_FinalData/05_191210_final_taxon_join_dia_Joan_Goma.RDS")
data4 = readRDS("03_FinalData/05_191210_final_taxon_join_dia_Joan_Goma.RDS")


# The id column in the species data has three digits while that in the site data
# only has two. I suppose the third number doesn't reflect a site but a sampling
# event at a site. So to connect observations to sampling locations I can safely
# remove the third digit, so that the columns are equal and can be used for a
# join.
data4[, site_id := str_remove(site_id, "-[0-9]$")]

# join data sets 
data5 = merge(x = data4, y = site.data2, all.x = T, by = "site_id")


# 03. Final touches -------------------------------------------------------
names(data5)[1] <- "site_org_id"
unique_sites = unique(data5$site_org_id)
unique_dates = unique(data5$date)

data5[, c("site_id", "date_id") := list(
        map_int(data5$site_org_id, ~ which(unique_sites == .x )),
        map_int(data5$date, ~ which(unique_dates == .x ))
)]


data5[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data5[, date_id2 := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data5[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_joan_goma")]

n.sample.data = data5[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data6 = data5[n.sample.data, on = "site_id2"]

for (i in seq_along(unique(data6$gr_sample_id))) {
        
        temp.reduced.data = data6[gr_sample_id == unique(gr_sample_id)[i]]
        n.spec = nrow(temp.reduced.data)
        n.gen1 = unique(temp.reduced.data$gen)
        n.gen2 = length(n.gen1)
        n.gen3 = sum(is.na(temp.reduced.data$gen))
        n.gen = ifelse(n.gen3 > 0, sum(n.gen2, n.gen3, -1), n.gen2)
        n.fam1 = unique(temp.reduced.data$fam)
        n.fam2 = length(n.fam1)
        n.fam3 = sum(is.na(temp.reduced.data$fam))
        n.fam = ifelse(n.fam3 > 0, sum(n.fam2, n.fam3, -1), n.fam2)
        n.ord1 = unique(temp.reduced.data$ord)
        n.ord2 = length(n.ord1)
        n.ord3 = sum(is.na(temp.reduced.data$ord))
        n.ord = ifelse(n.ord3 > 0, sum(n.ord2, n.ord3, -1), n.ord2)
        
        data6[gr_sample_id == unique(gr_sample_id)[i],
              c("n.species", "n.genus", "n.family", "n.order") := 
                      list(n.spec, n.gen, n.fam, n.ord)
              ]
        
        print(i)        
};beepr::beep()

for (i in seq_along(colnames(data4))) {
        x = pull(data4[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4)[i])
        
}

## prisitne 

#data4[, pristine := ifelse(artificial >= 10, 0,1)]

data7 = data6[, list(
        gr_sample_id,
        original_site_name = site_org_id,
        date,
        year,
        season,
        site_id,
        date_id,
        species,
        genus,
        family,
        order,
        abundance = Abundanz,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord,
        y.coord,
        EPSG = 23031,
        data.set = "joan_goma",
        n.samples
        
)]



# there are no NAs in the coordinates 
is.na(data5$y.coord) %>% sum
is.na(data5$x.coord) %>%  sum

final = st_as_sf(data7, coords = c("x.coord", "y.coord"), crs = data7$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "n.species")

st_write(final, "03_FinalData/06_191210_DIA_Joan_Goma.gpkg")   


