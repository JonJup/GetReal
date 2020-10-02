#########################################
# --- Clean DIA data from STARS --- # 
#########################################

# 08.10.19

# GR WP 02 
# EPSG = 4326



# 01. Setup -------------------------------------------------------------------
pacman::p_load(dplyr, readxl, stringr, readr, data.table, taxize, sf, purrr, tmap)
tmap_mode("view")
setwd(here::here("12_Christian Feld"))

# these files use abbreviations for taxa names 
# mountain sites
dia1 = read_excel("01_OriginalData/STAR_diatoms.xls", sheet = 1) 
# lowland sites
dia2 = read_excel("01_OriginalData/STAR_diatoms.xls", sheet = 2)  

# the link from abbreviation to full name can be made using this file 
link_names = read_excel("../03_Saul_Blanco/01_data_raw/OMNIDIAFILE.xls", col_names = T, skip = 1, sheet = 1)
link_names2 = read_excel("../03_Saul_Blanco/01_data_raw/OMNIDIAFILE.xls", col_names = T, skip = 0, sheet = 2)

link_names_bind = bind_rows(link_names, link_names2)
rm(link_names, link_names2)

# file for dates 
dates = read_excel("01_OriginalData/STAR_Metadata_all_Sites.xls") %>% 
  setDT

# Initial data cleaning ---------------------------------------------------

link_names.sub = select(link_names_bind,CODE,
                        DENOM)
rm(link_names_bind)
taxa.names = dia1[,1] %>% .[-c(1:4),]
taxa.names2 = dia2[,1] %>% .[-c(1:4),]
names(taxa.names) = "Taxa_Code"
names(taxa.names2) = "Taxa_Code"

# join Codes with species names 
taxa.names_join = left_join(x = taxa.names,
                            y = link_names.sub,
                            by = c("Taxa_Code" = "CODE"))

taxa.names2_join = left_join(x = taxa.names2,
                             y = link_names.sub,
                             by = c("Taxa_Code" = "CODE"))
rm(taxa.names, taxa.names2)

# fill new table with full names and long format ...
# ... for mountainous sites 
for (i in 2:ncol(dia1)) {
  
  sd = dia1[,c(1,i)]
  sd2 = sd[-c(1:4), ]
  sd2[,2] = as.numeric(pull(sd2, 2))
  sd3 = sd2[which(sd2[,2] > 0), ]
  sd3.j = left_join(sd3,
                    taxa.names_join,
                    by = c("...1" = "Taxa_Code" ))
  
  sd3.j.sub = sd3.j[,2:3]  
  
  
  if (nrow(sd3.j.sub) > 0) {
    
    assign(
      paste0("dia1",i,"_tb"),
      tibble(taxon = sd3.j.sub$DENOM,
             abundance = pull(sd3.j.sub[,1]),
             stream = sd[1,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
             site_name = sd[2,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
             site_id = names(sd2)[2] %>%  rep(nrow(sd3.j.sub)),
             sampling_id =  paste0(sd[4,2], "-", str_remove(sd[3,2], " -")) %>% rep(nrow(sd3.j.sub))
             
      ))
  }
}

star.dia.mount = rbindlist(mget(ls()[grepl(x = ls(), pattern = "_tb$")]))
rm(list = ls()[grepl(x = ls(), pattern = "_tb$")])
rm(dia1, sd, sd2, sd3, sd3.j, sd3.j.sub)
## find dates / seasons 
dia_spring = pull(dates[,11])
dia_summer = pull(dates[,12])
dia_autumn = pull(dates[,13])
for (dataset in c("dia_spring", "dia_summer", "dia_autumn")) {
        
        data = get(dataset)
        length.index = length(data)
        for (k in 1:length.index) {
                if (is.na(data[k])) next()
                if (str_detect(data[k], ";")) {
                        new_sampd_id = str_split(data[k], "; ")
                        data = data[-k]
                        data = append(data,
                                      unlist(new_sampd_id),
                                      after = k - 1)
                        length.index = append(length.index, 
                                              max(length.index) + 1)      
                }
        }
        assign(dataset, data)
}
rm(data, dataset, i,k,length.index, new_sampd_id)

for (i in seq_len(length(unique(star.dia.mount$sampling_id)))) {
        
        samp_ID =  unique(star.dia.mount$sampling_id)[i]
        
        ids = which(star.dia.mount$sampling_id == samp_ID)
        
        spring = samp_ID %in% dia_spring
        summer = samp_ID %in% dia_summer
        autumn = samp_ID %in% dia_autumn
        
        # no date -> skip
        if (!(any(spring, summer, autumn))) next()
        if (sum(spring, summer, autumn) > 1) break("more than one")
        
        star.dia.mount[ids, "season" := ifelse(spring, "spring", ifelse(
                summer, "summer", "autumn"))
              ]
}

## have a look at those that are still na 
no_season_sites = str_remove(star.dia.mount[is.na(season), unique(site_id)],
                             "S")
for (q in seq_len(length(no_season_sites))) {
        
        rowid = which(dates[,1] == no_season_sites[q])
        dates_subset = dates[rowid, .SD, .SDcols = 11:13]
        na_sum = sum(is.na(dates_subset))
        if (na_sum == 2) {
                ex_season = c("spring","summer","autumn")[!(is.na(dates_subset))]
                star.dia.mount[site_id == paste0("S", no_season_sites[q]),
                               season := ex_season]
                
        }        
}
# ... for lowland sites 

for (i in 2:ncol(dia2)) {
        
        sd = dia2[,c(1,i)]
        sd2 = sd[-c(1:4), ]
        sd2[,2] = as.numeric(pull(sd2, 2))
        sd3 = sd2[which(sd2[,2] > 0), ]
        sd3.j = left_join(sd3,
                          taxa.names2_join,
                          by = c("...1" = "Taxa_Code" ))
        
        sd3.j.sub = sd3.j[,2:3]
        
        
        if (nrow(sd3.j.sub) > 0) {
                
                assign(
                        paste0("dia1",i,"_tb"),
                        tibble(taxon = sd3.j.sub$DENOM, 
                               abundance = pull(sd3.j.sub[,1]), 
                               stream = sd[1,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_name = sd[2,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_id = names(sd2)[2] %>%  rep(nrow(sd3.j.sub)),
                               sampling_id =  paste0(sd[4,2], "-", str_remove(sd[3,2], " -")) %>% rep(nrow(sd3.j.sub)))
                )
        }
}

star.dia.low = rbindlist(mget(ls()[grepl(x = ls(), pattern = "_tb$")]))
rm(list = ls()[grepl(x = ls(), pattern = "_tb$")])
rm(dia2, sd, sd2, sd3, sd3.j, sd3.j.sub)

for (i in seq_len(length(unique(star.dia.low$sampling_id)))) {
        
        samp_ID =  unique(star.dia.low$sampling_id)[i]
        
        ids = which(star.dia.mount$low == samp_ID)
        
        spring = samp_ID %in% dia_spring
        summer = samp_ID %in% dia_summer
        autumn = samp_ID %in% dia_autumn
        
        # no date -> skip
        if (!(any(spring, summer, autumn))) next()
        if (sum(spring, summer, autumn) > 1) break("more than one")
        
        star.dia.low[ids, "season" := ifelse(spring, "spring", ifelse(
                summer, "summer", "autumn"))
                ]
}

## have a look at those that are still na 
no_season_sites = str_remove(star.dia.low[is.na(season), unique(site_id)],
                             "S")
for (q in seq_len(length(no_season_sites))) {
        
        rowid = which(dates[,1] == no_season_sites[q])
        dates_subset = dates[rowid, .SD, .SDcols = 11:13]
        na_sum = sum(is.na(dates_subset))
        if (na_sum == 2) {
                ex_season = c("spring","summer","autumn")[!(is.na(dates_subset))]
                star.dia.low[site_id == paste0("S", no_season_sites[q]),
                               season := ex_season]
                
        }        
}
rm(spring, autumn, summer, dates, dates_subset, dia_autumn, dia_spring, dia_summer, 
   ex_season, i, ids, link_names.sub, na_sum, no_season_sites, q, rowid, samp_ID, taxa.names_join, 
   taxa.names2_join)


# combine both datasets 
data = bind_rows(star.dia.mount, star.dia.low) %>% setDT
rm(star.dia.low, star.dia.mount)
###  now we can add coordinates 
# create a vector with all sites 
all.sites = data$site_id %>% unique
# one sites is missing the S
all.sites[!(str_detect(all.sites, "^S"))]  <- str_glue("S",
                                                       all.sites[!(str_detect(all.sites, "^S"))])
# new data table with site id column 
stars.coords = data.table("ID" = all.sites)
rm(all.sites)
wiserstar = read_excel("01_OriginalData/WISER-STAR_Code_Assignment.xlsx", sheet = 1) %>% 
        setDT

# add S to ids 
wiserstar$Site_Number = paste0("S",wiserstar$Site_Number)

wiser = read_excel("01_OriginalData/WISER_Metadata_Abiotics.xls") %>% 
        setDT
wiser.sub = wiser[,1:3]
stars.coo.wi = stars.coords[wiserstar, on = c("ID" = "Site_Number")] 
stars.coo.wi2 = stars.coo.wi[wiser.sub, on = c("Station_Code_WISER" = "StationCode")] 
stars.coo.wi3 = stars.coo.wi2[,-c(5:7)]
data2 = left_join(data, stars.coo.wi3, by = c("site_id" = "ID")) %>% setDT
rm(data, wiserstar, wiser, wiser.sub, stars.coo.wi, stars.coo.wi2, stars.coo.wi3, stars.coords)

# add pristine state 

data2 = read_excel("01_OriginalData/STAR_Metadata_all_Sites.xls", skip = 3) %>% 
  select(Site_Number, `pre-classification`) %>% 
  mutate(Site_Number2 = paste0("S", Site_Number)) %>% 
  right_join(data2, c("Site_Number2" = "site_id")) %>% 
  select(-Site_Number) %>% 
  rename(site_id = Site_Number2, prot_pristine = `pre-classification`)

saveRDS(data2, "03_FinalData/01_191212_data_before_taxon_clean_dia_STAR.RDS")
data2 = readRDS("03_FinalData/01_191212_data_before_taxon_clean_dia_STAR.RDS")


# taxonomical data --------------------------------------------------------

TU = sort(unique(data2$taxon))

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191212_classification.object_dia_STAR.RDS")
classification.object = readRDS("03_FinalData/02_191212_classification.object_dia_STAR.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)
# sourcing does not work properly. Open the file up and run it manually. 
clean_diatoms_synonyms.R

taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% length

response.vector = NULL
response = c()

# In this loop two errors pop up. Both are due to the taxon beeing on a class level. 
# Can be skipped and will be taken care of in a later step.
for (i in 1:(nrow(taxontable) + 1)) {
        
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

# quick save and starting point for later sessions
saveRDS(taxontable, "03_FinalData/03_191212_initial_taxon_clean_dia_STAR.RDS")
taxontable = readRDS("03_FinalData/03_191212_initial_taxon_clean_dia_STAR.RDS")


# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191212_post_correction_taxontable_dia_STAR.RDS")
taxontable = readRDS("03_FinalData/04_191212_post_correction_taxontable_dia_STAR.RDS")

# add taxonomical information to data 
data3 = left_join(data2, 
                  taxontable, 
                  by = "taxon") %>% 
  setDT

 
# data3[species == "NA", taxon] %>% unique %>% sort
# data3[genus == "NA", taxon] %>% unique %>% sort
# data3[family == "NA"] %>% unique(by = "taxon")
# data3[order == "NA"] %>% unique(by = "taxon")

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data3[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191212_final_taxon_join_dia_dia_STAR.RDS")
data4 = readRDS("03_FinalData/05_191212_final_taxon_join_dia_dia_STAR.RDS")


# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site_id)
unique_dates = unique(data4$season) %>% sort

data4[, c("site_id", "date_id") := list(
  unlist(map(data4$site_id, ~ which(unique_sites == .x ))),
  unlist(map(data4$season, function(x) { if (is.na(x)) {NA} else {which(unique_dates == x)} }))
)]

data4[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[, date_id2 := case_when(
  nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
  nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
  nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
  nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
  nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_STAR")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

for (i in seq_along(unique(data5$gr_sample_id))) {
  
  temp.reduced.data = data5[gr_sample_id == unique(gr_sample_id)[i]]
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
  
  data5[gr_sample_id == unique(gr_sample_id)[i],
        c("n.species", "n.genus", "n.family", "n.order") := 
          list(n.spec, n.gen, n.fam, n.ord)
        ]
  
  print(i)        
};beepr::beep()

for (i in seq_along(colnames(data5))) {
  x = pull(data5[,.SD,.SDcols = i])
  y = sum(is.na(x))        
  if (y > 0) print(names(data5)[i])
  
}


data6 = data5[, list(
  gr_sample_id,
  original_site_name = `site name`,
  date = NA,
  year = NA,
  season,
  site_id,
  date_id,
  species,
  genus,
  family,
  order,
  abundance,
  pristine = ifelse(prot_pristine == "good", 1, 0),
  n.species,
  n.genus,
  n.family,
  n.order,
  x.coord = Longitude,
  y.coord = Latitude,
  EPSG = 4326,
  data.set = "dia_Star",
  n.samples
  
)]

data6 = data6[!(is.na(x.coord))]


final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191212_dia_Star.gpkg")   

