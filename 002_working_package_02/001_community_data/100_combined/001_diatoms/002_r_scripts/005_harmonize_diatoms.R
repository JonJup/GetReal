# ----------------------------------------- #
### --- Clean Diatoms with Marias table -- ## 
# ----------------------------------------- #

# dates written/ modified: 17.03.20, 07.04., 30.06., 29.07., 30.07
# dates used: ..., 30.06.20, 01.07., 02.07, 03.07, 29.07., 30.07, 12.08 + 21.09

# setup -------------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, readxl, here, taxadb, stringr, xlsx, microbenchmark)
setwd(here())

# read data ---------------------------------------------------------------
data   <- readRDS("003_processed_data/004_2020-06-30_dia_data_low_ls_all.RDS")
fwb    <- read_excel("Emai_Maria/fwb13490-sup-0003-TableS2.xlsx")
omnida <- read_excel("omnida codes/OMNIDIAFILE.xls")
maria  <- read_excel("Emai_Maria/Kiselalger-i-svenska-sotvatten_30.xlsx", sheet = 2)
petra  <- read_excel("Emai_Petra/taxaliste_deutschl 2007_2010 PW.xls")
sanne  <- read_excel("../../../004_Traits/Diatoms/001_raw_data/2019-02-12-Diat.barcode-release-version 7.1.xlsx")

algaebase <- readRDS("003_processed_data/005x_2020-08-13_algae_base_vector.RDS")
gbif      <- readRDS("003_processed_data/005x_2020-08-13_gbif_vector.RDS")
# Functions --------------------------------
# Function to turn first word to uppercase 
upper_first <- function(x) {
   if (class(x) != "character") stop("x must be a string")
   x <- str_trim(x)
   substr(x,1,1) <- toupper(substr(x,1,1))
   return(x)
}
# function for fixes 
fixer <- function(from, to) {
   new_spe_name <- data2[original_name == to, unique(species_clean)]
   new_gen_name <- data2[original_name == to, unique(genus)]
   new_fam_name <- data2[original_name == to, unique(family)]
   new_ord_name <- data2[original_name == to, unique(order)]
   new_from     <- data2[original_name == to, unique(name_source)]
   for (i in seq_along(from)) {
      data2[original_name == from[i], c("species", "genus", "family", "order", "name_source") :=
               .(new_spe_name, new_gen_name, new_fam_name, new_ord_name, new_from)]
   }
}

# Prepare data  -----------------------------------------------------------

### ---    prepare Maria's table      --- ###
setDT(maria) 
# drop some columns 
maria2 <- maria[,c("usage", "Omnidia code", "Taxon name", "new")]
## synonym and no new name? 
maria2[usage == "synonym" & (is.na(new) | new == "-" | new == "")]
# Achnantes nodosa - remove row 
maria2 <- maria2[!(usage == "synonym" & new == "-")]
## -- new name but no synonym? -- ## 
maria2[usage == "accepted" &  !(is.na(new) | new == "-" | new == "")]
# there are some. I will put there "usage" to synonym 
maria2[usage == "accepted" &  !(is.na(new) | new == "-" | new == ""), usage := "synonym"]
maria2[usage == "accepted", unique(new)]

## -- list all remaining new entries so I can fix them manually 
unm <- maria2[usage == "synonym",sort(unique(new))]
maria2[new == "CBPI,CBIF et al.", new := "CBPI"]
maria2[new == "CHER et al.", new := "CHER"]
maria2[new == "CLCT,CFON", new := "CLCT"]
maria2[new == "CNCI,CLBE,CSCI,CDNO,CCYM et al.", new := "CNCI"]
maria2[new == "EFOM, EMYR", new := "EFOM"]
maria2[new == "FKRA,FRAM,FCRS,FRSA,FERI", new := "FKRA"]
maria2[new == "PLBIss, PICU", new := "PLBIss"]
maria2[new == "SEMU, SMTO", new := "SEMU"]
maria2[new == "STAN et al.", new := "STAN"]
unm <- maria2[usage == "synonym",sort(unique(new))]

names(maria2) <- c("usage", "code", "taxon", "new")



## -- Quality Check -- ## 
if (any(!unm %in% maria2$code)) print("Quality Check Failed") else print("Quality Check passed")
## --               -- ## 
## -- if check was failed control here -- ## 
# for (i in 1:length(unm)) {
#    if (!unm[i] %in% maria2$code) print(i)
# }
## --                                   -- ## 
maria2 <- maria2[!is.na(taxon)]
maria2[, taxon_cleaned := clean_names(taxon)]
maria2[, taxon_cleaned := upper_first(taxon_cleaned)]

rm(unm, maria);gc()

### ---    prepare fwb table      --- ###
setDT(fwb)
fwb <- fwb[-1,]
fwb[,c("...3", "...4", "...5") := NULL]
names(fwb)[c(1,2)] <- c("new_name", "old_name")
fwb[, clean_old := clean_names(old_name)]
fwb[, clean_old := upper_first(clean_old)]

fwb %<>% unique(by = c("new_name", "old_name", "clean_old"))
fwb_c <- unique(fwb, by = c("new_name", "clean_old"))

### ---    prepare Petra's table      --- ###
setDT(petra)
petra <- petra[,c('Taxon-Name', 'Taxon-Name neu')]
names(petra) <- c("name", "name_new")
petra[,taxon_clean := clean_names(name)]
petra[,taxon_clean := upper_first(taxon_clean)]

### ---    prepare Sanne's table      --- ###
setDT(sanne)
sanne <- sanne$`Original taxon name`
zero_id <- which(sanne == "0")
sanne <- sanne[-zero_id]
sanne <- unique(sanne)
sanne_clean <- unique(clean_names(sanne))
sanne_clean <- upper_first(sanne_clean)

rm(zero_id);gc()

### ---    prepare Omnida      --- ###
setDT(omnida)
names(omnida)[1:5] <- c("code", "id", "taxon", "references", "synonym")
omnida <- omnida[,c(1,3,5)]
omnida <- omnida[-1]
omnida$synonym %<>% str_remove(pattern = "\\(.*")
omnida$synonym %<>% str_remove(pattern = "=")
omnida$synonym %<>% str_remove(pattern = "\\=.*")
omnida$synonym %<>% str_remove(pattern = "\\ .*")
omnida$synonym %<>% str_remove(pattern = "\\?")
omnida$synonym %<>% str_remove(pattern = "\\)")
omnida[str_detect(synonym, "[0-9]")]
omnida[str_detect(synonym, "\\)")]
omnida[synonym == "1894EPSU", synonym := "EPSU"]
omnida[synonym == "1960FTAU", synonym := "FTAU"]
omnida[str_detect(synonym, "^[0-9]"), synonym := NA]
omnida[synonym == "PRADpp.", synonym := "PRADpp"]
omnida[synonym == "SUTEpp.", synonym := "SUTEpp"]
omnida[synonym == "PLTVss.Rumrich", synonym := "PLTVss"]
omnida[str_detect(synonym, "\\."), synonym := NA]
omnida[str_detect(synonym, "\\/"), synonym := NA]
omnida[synonym %in% c("Cocquyt94", "Cleve-Euler", "CE52p84f1491a-cWit00ID7p389f194:1-5", "CE53p128f747a-b", "CE53p70f631", 
                      "British", "B91f48:1-3", "Alaska", "Amerique", "fig48-50", "fig68-75", "Foged", "fossil", "Fossil", 
                      "Genkal93DR8", "Ghana", "Gran04p38f1:13-14Wit00ID7p310f128:9"), synonym := NA]
omnida$synonym %<>% str_remove(pattern = "pp")
omnida[synonym == "", synonym := NA]
omnida[,taxon_clean := clean_names(taxon)]
omnida[,taxon_clean := upper_first(taxon_clean)]

# initial automatic clean -----------------------------------------------

# add a column with clean names from taxadb 
data[species == "NA", species := NA]
data[, original_name_cleaned := upper_first(clean_names(original_name))]
TU   <- sort(unique(data$original_name))
TU_c <- sort(unique(data$original_name_cleaned))

# automatic clean -----------------------------------------------
## -- MARIA - ORIGINAL NAMES -- ## 
for (i in seq_along(TU)) {
   if (TU[i] %in% maria2$taxon) {
      id <- which(maria2$taxon == TU[i])
      
      if (length(id) > 1) {
         usage <- maria2$usage[id]
         id_acc <- which(usage == "accepted")
         if (length(id_acc) == 0) {
            print("breaking")
            break()
         }
         id <- id[id_acc]

      }
      
      if (maria2$usage[id] == "accepted") {
         data[original_name == TU[i], name_maria := maria2$taxon[id]]
         print(i)
         next()
      }
      if (maria2$usage[id] == "synonym")  {
         new_code <- maria2$new[id]
         #if (stringr::str_detect(new_code, ",")) next()
         new_id   <- which(maria2$code == new_code)
         new_name <- maria2$taxon[new_id]
         # check that this is accepted
         if (maria2$usage[new_id] == "synonym") {
            new_code <- maria2$new[new_id]
            new_id   <-
               which(maria2$code == new_code)
            new_name <- maria2$code[new_id]
            if (maria2$usage[new_id] == "synonym") {
               new_code <- maria2$new[new_id]
               new_id   <-
                  which(maria2$code == new_code)
               new_name <-
                  maria2$taxon[new_id]
            }
            
         }
         if (maria2$usage[new_id] == "synonym") {
            print(paste("broken by", i))
            break()
         }
         if (maria2$usage[new_id] == "accepted") {
            data[original_name == TU[i], name_maria := maria2$taxon[new_id]]
         }
      }
   }

   print(paste(i, "of", length(TU)))
}
## -- MARIA - CLEAN NAMES    -- ##  
for (i in seq_along(TU_c)) {
   if (TU_c[i] %in% maria2$taxon_cleaned) {
      
      id <- which(maria2$taxon_cleaned == TU_c[i])
      
      if (length(id) > 1) {
         usage <- maria2$usage[id]
         id_acc <- which(usage == "accepted")
         if (length(id_acc) == 0) next()
         id <- id[id_acc[1]]
      }
      
      if (maria2$usage[id] == "accepted") {
         data[original_name_cleaned == TU_c[i], name_maria_cleaned := maria2$taxon_cleaned[id]]     
         print(i)
         next()
      }
      if (maria2$usage[id] == "synonym")  {
         new_code <- maria2$new[id]
         #if (stringr::str_detect(new_code, ",")) next()
         new_id   <- which(maria2$code == new_code)
         new_name <- maria2$taxon_cleaned[new_id]
         # check that this is accepted
         if (maria2$usage[new_id] == "synonym") {
            new_code <- maria2$new[new_id]
            new_id   <- which(maria2$code == new_code)
            new_name <- maria2$code[new_id]
            if (maria2$usage[new_id] == "synonym") {
               new_code <- maria2$new[new_id]
               new_id   <- which(maria2$code == new_code)
               new_name <- maria2$taxon_cleaned[new_id]
            }
            
         }
         if (maria2$usage[new_id] == "synonym") {
            print(paste("broken by", i))
            break()
         }
         if (maria2$usage[new_id] == "accepted") {
            data[original_name_cleaned == TU_c[i], name_maria_cleaned := maria2$taxon_cleaned[new_id]]
         }
      }
   }
   print(paste(i, "of", length(TU_c)))
}
## -- PETRA - ORIGINAL NAMES -- ## 
for (i in seq_along(TU)) {
   if (TU[i] %in% petra$name) {
      id <- which(petra$name == TU[i])
      
      if (length(id) > 1) {
         if (sum(!is.na(petra$name_new)) == 0) break()
         new_id <- which(!is.na(petra$name_new[id]))
         id <- id[new_id]
      }
      
      if (sum(!is.na(petra$name_new[id])) == 0) {
         data[original_name == TU[i], name_petra := petra$name[id]]
         print(paste(i, "of", length(TU)))
         next()
      }
      if (sum(!is.na(petra$name_new[id])) != 0)  {
         data[original_name == TU[i], name_petra := petra$name_new[id]]
         print(paste(i, "of", length(TU)))
         next()
      }
   }
   print(paste(i, "of", length(TU)))
}
## -- PETRA - CLEAN NAMES    -- ## 
for (i in seq_along(TU_c)) {
   if (TU_c[i] %in% petra$taxon_clean) {
      id <- which(petra$taxon_clean == TU_c[i])
      
      if (length(id) > 1) {
         if (sum(!is.na(petra$taxon_clean[id])) == 0) break()
         new_id <- which(!is.na(petra$name_new[id]))
         if (length(new_id) > 1 & length(unique(na.omit(petra$name_new[id]))) > 1) next()
         id <- id[new_id[1]]
      }
      
      if (sum(!is.na(petra$name_new[id])) == 0) {
         data[original_name_cleaned == TU_c[i], name_petra_clean := petra$taxon_clean[id]]
         print(paste(i, "of", length(TU_c)))
         next()
      }
      if (sum(!is.na(petra$name_new[id])) != 0)  {
         data[original_name_cleaned == TU_c[i], name_petra_clean := petra$name_new[id]]
         print(paste(i, "of", length(TU_c)))
         next()
      }
   }
   print(paste(i, "of", length(TU_c)))
}
## -- SANNE - ORIGINAL NAMES -- ## 
for (i in seq_along(TU)) {
   if (TU[i] %in% sanne) {
      id <- which(sanne == TU[i])
      
      data[original_name == TU[i], name_sanne := sanne[id]]
      print(paste(i, "of", length(TU)))
      next()
   }
   print(paste(i, "of", length(TU)))
}
## -- SANNE - CLEAN NAMES    -- ## 
for (i in seq_along(TU_c)) {
   if (TU_c[i] %in% sanne_clean) {
      id <- which(sanne_clean == TU_c[i])
      
      data[original_name_cleaned == TU_c[i], name_sanne_clean := sanne_clean[id]]
      print(paste(i, "of", length(TU_c)))
      next()
   }
   print(paste(i, "of", length(TU_c)))
}
## -- FWB - ORIGINAL NAMES   -- ## 
for (i in seq_along(TU)) {
   # is the name in the db? 
   if (TU[i] %in% fwb$old_name) { #BEGIN IF1
      id <- which(fwb$old_name == TU[i])
      
      if (length(id) > 1) {print("breaking"); break()}

      data[original_name == TU[i], name_fwb := fwb$new_name[id]]
      print(paste(i, "of", length(TU)))
      next()
      
         
   } # END of IF1 
   print(paste(i, "of", length(TU)))
}
## -- FWB - CLEAN NAMES     -- ## 
for (i in seq_along(TU_c)) {
   # is the name in the db? 
   if (TU_c[i] %in% fwb$old_name) { #BEGIN IF1
      id <- which(fwb$old_name == TU_c[i])
      
      if (length(id) > 1) {print("breaking"); break()}
      
      data[original_name_cleaned == TU_c[i], name_fwb_clean := fwb$new_name[id]]
      print(paste(i, "of", length(TU)))
      next()
      
      
   } # END of IF1 
   print(paste(i, "of", length(TU)))
}
## -- OMNIDA - ORIGINAL NAMES -- ## 
for (i in seq_along(TU)) {
   if (TU[i] %in% omnida$taxon) {
      id <- which(omnida$taxon == TU[i])
      if (omnida$taxon[id] == "Bacillaria paradoxa") {
         data[original_name == TU[i], name_omnida := "Bacillaria paxillifera"]
         next()
      }
      if (length(id) > 1) {
         syn <- omnida$synonym[id]
         id_acc <- which(is.na(syn))
         if (length(id_acc > 1)) {
            id_acc <- id_acc[1]
            # which()
            # readline_id <- readline(cat(TU[i] , ":", paste(omnida$taxon[id[id_acc]])))
            # id_acc <- id_acc[as.numeric(readline_id)]
         }
         if (length(id_acc) == 0) {
            if(length(unique(syn)) == 1){
               id_acc = 1
            } else {
               next()
            }
         }
         id <- id[id_acc]
         
      }
      
      if (is.na(omnida$synonym[id])) {
         data[original_name == TU[i], name_omnida := omnida$taxon[id]]
         print(i)
         next()
      }
      if (!is.na(omnida$synonym[id])) {
         
         new_code <- omnida$synonym[id]
         new_id   <- which(omnida$code == new_code)
         if (length(new_id) == 0) next()
         new_name <- omnida$taxon[new_id]
         # check that this is accepted
         if (!is.na(omnida$synonym[new_id])) {
            new_code <- omnida$synonym[new_id]
            new_id   <- which(omnida$code == new_code)
            new_name <- omnida$taxon[new_id]
            if (!is.na(omnida$synonym[new_id])) {
               new_code <- omnida$synonym[new_id]
               new_id   <- which(omnida$code == new_code)
               new_name <- omnida$taxon[new_id]
               if (!is.na(omnida$synonym[new_id])) {
                  new_code <- omnida$synonym[new_id]
                  new_id   <- which(omnida$code == new_code)
                  new_name <- omnida$taxon[new_id]
               }
            }
            
            
         }
         if (!is.na(omnida$synonym[new_id])) {
            print(paste("broken by", i))
            break()
         }
         if (is.na(omnida$synonym[new_id])) {
            data[original_name == TU[i], name_omnida := omnida$taxon[new_id]]
         }
      }
   }
   
   print(paste(i, "of", length(TU)))
}
## -- OMNIDA CLEAN - ORIGINAL NAMES -- ## 
for (i in seq_along(TU)) {
   if (TU[i] %in% omnida$taxon_clean) {
      id <- which(omnida$taxon_clean == TU[i])
      if (omnida$taxon_clean[id] == "Bacillaria paradoxa") {
         data[original_name == TU[i], name_omnida_clean := "Bacillaria paxillifera"]
         next()
      }
      if (length(id) > 1) {
         syn <- omnida$synonym[id]
         id_acc <- which(is.na(syn))
         if (length(id_acc > 1)) {
            id_acc <- id_acc[1]
            # which()
            # readline_id <- readline(cat(TU[i] , ":", paste(omnida$taxon_clean[id[id_acc]])))
            # id_acc <- id_acc[as.numeric(readline_id)]
         }
         if (length(id_acc) == 0) {
            if(length(unique(syn)) == 1){
               id_acc = 1
            } else {
               next()
            }
         }
         id <- id[id_acc]
         
      }
      
      if (is.na(omnida$synonym[id])) {
         data[original_name == TU[i], name_omnida_clean := omnida$taxon_clean[id]]
         print(i)
         next()
      }
      if (!is.na(omnida$synonym[id])) {
         
         new_code <- omnida$synonym[id]
         new_id   <- which(omnida$code == new_code)
         if (length(new_id) == 0) next()
         new_name <- omnida$taxon_clean[new_id]
         # check that this is accepted
         if (!is.na(omnida$synonym[new_id])) {
            new_code <- omnida$synonym[new_id]
            new_id   <- which(omnida$code == new_code)
            new_name <- omnida$taxon_clean[new_id]
            if (!is.na(omnida$synonym[new_id])) {
               new_code <- omnida$synonym[new_id]
               new_id   <- which(omnida$code == new_code)
               new_name <- omnida$taxon_clean[new_id]
               if (!is.na(omnida$synonym[new_id])) {
                  new_code <- omnida$synonym[new_id]
                  new_id   <- which(omnida$code == new_code)
                  new_name <- omnida$taxon_clean[new_id]
               }
            }

            
         }
         if (!is.na(omnida$synonym[new_id])) {
            print(paste("broken by", i))
            break()
         }
         if (is.na(omnida$synonym[new_id])) {
            data[original_name == TU[i], name_omnida_clean := omnida$taxon_clean[new_id]]
         }
      }
   }
   
   print(paste(i, "of", length(TU)))
}
## -- OMNIDA - CLEAN NAMES -- ## 
for (i in seq_along(TU_c)) {
   if (TU_c[i] %in% omnida$taxon) {
      id <- which(omnida$taxon == TU_c[i])
      if (omnida$taxon[id] == "Bacillaria paradoxa") {
         data[original_name_cleaned == TU_c[i], name_clean_omnida := "Bacillaria paxillifera"]
         next()
      }
      if (length(id) > 1) {
         syn <- omnida$synonym[id]
         id_acc <- which(is.na(syn))
         if (length(id_acc > 1)) {
            id_acc <- id_acc[1]
            # which()
            # readline_id <- readline(cat(TU_c[i] , ":", paste(omnida$taxon[id[id_acc]])))
            # id_acc <- id_acc[as.numeric(readline_id)]
         }
         if (length(id_acc) == 0) {
            if(length(unique(syn)) == 1){
               id_acc = 1
            } else {
               next()
            }
         }
         id <- id[id_acc]
         
      }
      
      if (is.na(omnida$synonym[id])) {
         data[original_name_cleaned == TU_c[i], name_clean_omnida := omnida$taxon[id]]
         print(i)
         next()
      }
      if (!is.na(omnida$synonym[id])) {
         
         new_code <- omnida$synonym[id]
         new_id   <- which(omnida$code == new_code)
         if (length(new_id) == 0) next()
         new_name <- omnida$taxon[new_id]
         # check that this is accepted
         if (!is.na(omnida$synonym[new_id])) {
            new_code <- omnida$synonym[new_id]
            new_id   <- which(omnida$code == new_code)
            new_name <- omnida$taxon[new_id]
            if (!is.na(omnida$synonym[new_id])) {
               new_code <- omnida$synonym[new_id]
               new_id   <- which(omnida$code == new_code)
               new_name <- omnida$taxon[new_id]
               if (!is.na(omnida$synonym[new_id])) {
                  new_code <- omnida$synonym[new_id]
                  new_id   <- which(omnida$code == new_code)
                  new_name <- omnida$taxon[new_id]
               }
            }
            
            
         }
         if (!is.na(omnida$synonym[new_id])) {
            print(paste("broken by", i))
            break()
         }
         if (is.na(omnida$synonym[new_id])) {
            data[original_name_cleaned == TU_c[i], name_clean_omnida := omnida$taxon[new_id]]
         }
      }
   }
   
   print(paste(i, "of", length(TU_c)))
}
## -- OMNIDA CLEAN - CLEAN NAMES -- ##  
for (i in seq_along(TU_c)) {
   if (TU_c[i] %in% omnida$taxon_clean) {
      id <- which(omnida$taxon_clean == TU_c[i])
      if (omnida$taxon_clean[id] == "Bacillaria paradoxa") {
         data[original_name_cleaned == TU_c[i], name_clean_omnida_clean := "Bacillaria paxillifera"]
         next()
      }
      if (length(id) > 1) {
         syn <- omnida$synonym[id]
         id_acc <- which(is.na(syn))
         if (length(id_acc > 1)) {
            id_acc <- id_acc[1]
            # which()
            # readline_id <- readline(cat(TU_c[i] , ":", paste(omnida$taxon_clean[id[id_acc]])))
            # id_acc <- id_acc[as.numeric(readline_id)]
         }
         if (length(id_acc) == 0) {
            if(length(unique(syn)) == 1){
               id_acc = 1
            } else {
               next()
            }
         }
         id <- id[id_acc]
         
      }
      
      if (is.na(omnida$synonym[id])) {
         data[original_name_cleaned == TU_c[i], name_clean_omnida_clean := omnida$taxon_clean[id]]
         print(i)
         next()
      }
      if (!is.na(omnida$synonym[id])) {
         
         new_code <- omnida$synonym[id]
         new_id   <- which(omnida$code == new_code)
         if (length(new_id) == 0) next()
         new_name <- omnida$taxon_clean[new_id]
         # check that this is accepted
         if (!is.na(omnida$synonym[new_id])) {
            new_code <- omnida$synonym[new_id]
            new_id   <- which(omnida$code == new_code)
            new_name <- omnida$taxon_clean[new_id]
            if (!is.na(omnida$synonym[new_id])) {
               new_code <- omnida$synonym[new_id]
               new_id   <- which(omnida$code == new_code)
               new_name <- omnida$taxon_clean[new_id]
               if (!is.na(omnida$synonym[new_id])) {
                  new_code <- omnida$synonym[new_id]
                  new_id   <- which(omnida$code == new_code)
                  new_name <- omnida$taxon_clean[new_id]
               }
            }
            
            
         }
         if (!is.na(omnida$synonym[new_id])) {
            print(paste("broken by", i))
            break()
         }
         if (is.na(omnida$synonym[new_id])) {
            data[original_name_cleaned == TU_c[i], name_clean_omnida_clean := omnida$taxon_clean[new_id]]
         }
      }
   }
   
   print(paste(i, "of", length(TU_c)))
}
# assign new names  ----------------------------------------------------

# Idea 
# fwb > fwb_c > omnida > omnida_c_names > c_omnida > c_omnida_c_names > maria > maria_c > Petra > Petra_c > sanne > sanne_c
data[!is.na(species) & !is.na(name_fwb)                                    , c("species_new", "name_source") := .(name_fwb,                "fwb") ]
data[!is.na(species) & !is.na(name_fwb_clean)          & is.na(species_new), c("species_new", "name_source") := .(name_fwb_clean,          "fwb_clean")]
data[!is.na(species) & !is.na(name_omnida)             & is.na(species_new), c("species_new", "name_source") := .(name_omnida,             "omnida")]
data[!is.na(species) & !is.na(name_omnida_clean)       & is.na(species_new), c("species_new", "name_source") := .(name_omnida_clean,       "omnida_clean")]
data[!is.na(species) & !is.na(name_clean_omnida_clean) & is.na(species_new), c("species_new", "name_source") := .(name_clean_omnida_clean, "omnida_cc")]
data[!is.na(species) & !is.na(name_maria)              & is.na(species_new), c("species_new", "name_source") := .(name_maria,              "maria")]
data[!is.na(species) & !is.na(name_maria_cleaned)      & is.na(species_new), c("species_new", "name_source") := .(name_maria_cleaned,      "maria_clean")]
data[!is.na(species) & !is.na(name_petra)              & is.na(species_new), c("species_new", "name_source") := .(name_petra,              "petra")]
data[!is.na(species) & !is.na(name_petra_clean)        & is.na(species_new), c("species_new", "name_source") := .(name_petra_clean,        "petra_clean")]
data[!is.na(species) & !is.na(name_sanne)              & is.na(species_new), c("species_new", "name_source") := .(name_sanne,              "sanne")]
data[!is.na(species) & !is.na(name_sanne_clean)        & is.na(species_new), c("species_new", "name_source") := .(name_sanne_clean,        "sanne_clean")]

# check against algaebase 
data[!is.na(species) & is.na(species_new) & original_name %in% algaebase, c("species_new", "name_source") := .(species, "algaebase")]
# check against gbif 
data[!is.na(species) & is.na(species_new) & original_name %in% gbif, c("species_new", "name_source") := .(species, "gbif")]
# mark all entries that still remain without entry 
data[!is.na(species) & is.na(species_new), name_source :=  "no-db"]
# there should be no NAs left 
if (data[is.na(name_source) & !is.na(species), .N] == 0) print("Quality Check Passed") else print("Quality Check failed")

uni_nodb <- data[name_source == "no-db", sort(unique(original_name))]

# fix missing entries 
# these ones comes first due to formatting (theirs messes up all the others) 
data[original_name == "Achnanthes stolida  Krasske  alt. Navicula schmassmannii Hustedt (jfr 2/4, T24)" ,c("species_new", "name_source") := .("Achnanthes stolida", "fwb_manual")]
data <- data[!original_name == "Gomphonema goronatum"] # not found in any db!
 
data[original_name == "Achnanthes austriaca v.ventricosa"                               , c("species_new", "name_source"             ) := .("Psammothidium helveticum"            , "omnida_manual")]
data[original_name == "Achnanthes cf. pseudoswazi Carter"                               , c("species_new", "name_source"             ) := .("Psammothidium perpusillum"           , "omnida_manual")]
data[original_name == "Achnanthes flexella (Kützing) Brun"                              , c("species_new", "name_source"             ) := .("Eucocconeis flexella"                , "omnida_manual")]
data[original_name == "Achnanthes kriegerii"                                            , c("species_new", "name_source"             ) := .("Achnanthidium kriegeri"              , "omnida_manual")]
data[original_name == "Achnanthes kriegerii big"                                        , c("species_new", "name_source"             ) := .("Achnanthidium kriegeri"              , "omnida_manual")]
data[original_name == "Achnanthes kriegerii"                                            , c("species_new", "name_source"             ) := .("Achnanthidium kriegeri"              , "omnida_manual")]
data[original_name == "Achnanthes minutissima-grupp"                                    , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                               
data[original_name == "Achnanthes minutissima (långsmal)"                               , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                                              
data[original_name == "Achnanthes minutissima forma eutrop"                             , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                                              
data[original_name == "achnanthes minutissima v.inconspicua"                            , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                                              
data[original_name == "Achnanthes minutissima v.jackii"                                 , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                                              
data[original_name == "Achnanthes minutissima var. cryptocephala"                       , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                                                              
data[original_name == "Achnanthes minutissima var. subrobusta"                          , c("species_new", "name_source"             ) := .("Achnanthidium minutissimum"          , "omnida_manual")]                                         
data[original_name == "Achnanthidium minutissima var. affinis"                          , c("species_new", "name_source"             ) := .("Achnanthidium affine"                , "omnida_manual")]
data[original_name == "Achnanthidium thermale"                                          , c("species_new", "name_source"             ) := .("Crenotia thermalis"                  , "algaebase")]
data[original_name == "Adlafia muralis"                                                 , c("species_new", "name_source", "comment"  ) := .("Navicula krasskei-egregia-minuscula" , "fwb_manual"   , "via algaebase"    )]
data[original_name == "Amphora neglecta f. densestriata"                                , c("species_new", "name_source"             ) := .("Halamphora normanii"                 , "fwb_manual"                     )]
data[original_name == "Amphora normanii"                                                , c("species_new", "name_source", "comment"  ) := .("Halamphora normanii"                 , "fwb_manual"   , "via omnida_manual")]
data[original_name == "Aneumastus tuscula"                                              , c("species_new", "name_source", "comment"  ) := .("Aneumastus tusculus"                 , "fwb_manual"   , "typo"             )]
data[original_name == "Berkeleya scopulorum"                                            , c("species_new", "name_source"             ) := .("Berkeleya scopulorum"                , "fwb_manual")]
data[original_name == "Berkeleya scopulorum (Brebisson) Cox"                            , c("species_new", "name_source"             ) := .("Berkeleya scopulorum"                , "fwb_manual")]
data[original_name == "Cocconeis euglypta Ehrenberg emend Romero & Jahn"                , c("species_new", "name_source"             ) := .("Cocconeis euglypta"                  , "omnida_manual")]
data[original_name == "Cocconeis hauniensis"                                            , c("species_new", "name_source"             ) := .("Cocconeis nugalas"                   , "algaebase")]
data[original_name == "Cyclotella balatonis"                                            , c("species_new", "name_source"             ) := .("Lindavia balatonis"                  , "algaebase")]
data[original_name == "Cyclotella michigiana"                                           , c("species_new", "name_source"             ) := .("Cyclotella michigiana"               , "fwb_manual")]
data[original_name == "Cyclotella polymorpha Meyer & Håkansson"                         , c("species_new", "name_source"             ) := .("Cyclotella polymorpha"               , "omnida_manual")]
data[original_name == "Encyonema mesianum (Cholnoky) D.G. Mann in Round Crawford & Mann", c("species_new", "name_source"             ) := .("Encyonema mesianum"                  , "fwb_manual")]
data[original_name == "Eolimna crassulexigua (E.Reichardt) Reichardt"                   , c("species_new", "name_source"             ) := .("Sellaphora crassulexigua"            , "algaebase")]
data[original_name == "Eolimna raederae"                                                , c("species_new", "name_source"             ) := .("Sellaphora raederae"                 , "algaebase")]
data[original_name == "Eolimna raederae (Lange-Bertalot) Lange-Bertalot & Kulikovskiy"  , c("species_new", "name_source"             ) := .("Sellaphora raederae"                 , "algaebase")]
data[original_name == "Eolimna rotunda"                                                 , c("species_new", "name_source"             ) := .("Sellaphora rotunda"                  , "algaebase")]
data[original_name == "Eolimna rotunda (Hustedt) Lange-Bertalot & Kulikovskiy"          , c("species_new", "name_source"             ) := .("Sellaphora rotunda"                  , "algaebase")]
data[original_name == "Eolimna submuralis (Hustedt) Lange-Bertalot & Kulikovskiy"       , c("species_new", "name_source"             ) := .("Sellaphora submuralis Complex"       , "fwb_manual")]
data[original_name == "Epithemia ocellata (Ehrenberg) Kützing"                          , c("species_new", "name_source"             ) := .("Epithemia argus"                     , "omnida_manual")]
data[original_name == "Eunoria curtagrunowii Nörpel-Sch. & Lange-Bertalot"              , c("species_new", "name_source", "comment"  ) := .("Eunotia Complex"                     , "fwb_manual"    , "typo")]
data[original_name == "Eunotia meisterii"                                               , c("species_new", "name_source"             ) := .("Eunotia  exigua/elegans Complex"     , "fwb_manual")]
data[original_name == "Eunotia turgidula"                                               , c("species_new", "name_source"             ) := .("Epithemia turgida"                   , "algaebase")]
data[original_name == "Fragilaria (Ulnaria) ulna Sippen angustissima"                   , c("species_new", "name_source"             ) := .("Fragilaria ulna"                     , "omnida_manual")]
data[original_name == "Fragilaria cf. delicatissima (W. Smith) Lange-Bertalot"          , c("species_new", "name_source"             ) := .("Fragilaria delicatissima"            , "omnida_manual")]
data[original_name == "Fragilaria construens f. binodis (Ehrenberg.) Hustedt"           , c("species_new", "name_source"             ) := .("Staurosira construens var. binoids"  , "omnida_manual")]
data[original_name == "Fragilaria construens f. construens (Ehrenberg) Grunow"          , c("species_new", "name_source"             ) := .("Staurosira construens"               , "omnida_manual")]
data[original_name == "Fragilaria construens for. binodis"                              , c("species_new", "name_source"             ) := .("Staurosira construens var. binoids"  , "omnida_manual")]
data[original_name == "Fragilaria construens var. cruciata"                             , c("species_new", "name_source"             ) := .("Staurosira construens"               , "omnida_manual")]
data[original_name == "Fragilaria sopotensis"                                           , c("species_new", "name_source"             ) := .("Nanofrustulum sopotense"             , "algaebase")]
data[original_name == "Fragilaria ulna var. danica (Kützing) Lange-Bertalot"            , c("species_new", "name_source", "comment"  ) := .("Ulnaria ulna complex"                , "fwb_manual"     , "via omnida_manual")]
data[original_name == "Fragilaria zellerii"                                             , c("species_new", "name_source"             ) := .("Fragilaria zellerii"                 , "omnida_manual")]
data[original_name == "Frustulia rhomboides (Ehrenberg) De Toni"                        , c("species_new", "name_source"             ) := .("Frustulia rhomboides Complex"        , "fwb_manual")]
data[original_name == "Frustulia rhomboides var. crassinervia (Brébisson) Ross"         , c("species_new", "name_source"             ) := .("Frustulia rhomboides Complex"        , "fwb_manual")]
data[original_name == "Frustulia rhomboides var. saxonica (Rabenhorst) De Toni"         , c("species_new", "name_source"             ) := .("Frustulia rhomboides Complex"        , "fwb_manual")]
data[original_name == "Gomphoneis olivaceolacua"                                        , c("species_new", "name_source"             ) := .("Gomphonella olivaceolacua"           , "algaebase")]
data[original_name == "Gomphonema ventricosa"                                           , c("species_new", "name_source", "comment"  ) := .("Gomphonema ventricosum"              , "fwb_manual"    , "typo")]
data[original_name == "Gomphospenia tackei"                                             , c("species_new", "name_source", "genus_new") := .(NA                                    , "fwb_manual"    , "Gomphosphenia")]
data[original_name == "Handmannia comta"                                                , c("species_new", "name_source"             ) := .("Lindavia comta"                      , "algaebase")]
data[original_name == "Navicula atomus v.recondita"                                     , c("species_new", "name_source", "comment"  ) := .("Mayamaea Complex"                    , "fwb_manual"    , "via omnida_manual")]
data[original_name == "Navicula capitata v.hungarica"                                   , c("species_new", "name_source"             ) := .("Hippodonta hungarica"                , "omnida_manual")]
data[original_name == "Navicula capitata var. linearis"                                 , c("species_new", "name_source"             ) := .("Hippodonta capitata"                 , "omnida_manual")]
data[original_name == "Navicula dificillima"                                            , c("species_new", "name_source"             ) := .("Navicula arvensis-difficillima++"    , "fwb_manual")]
data[original_name == "Navicula schmasmannii"                                           , c("species_new", "name_source"             ) := .("Achnanthes acares/ricula/carissima"  , "fwb_manual")]
data[original_name == "Navicula schroeterii"                                            , c("species_new", "name_source"             ) := .("Navicula schroeteri"                 , "fwb_manual")]
data[original_name == "Naviculadicta seminulum"                                         , c("species_new", "name_source", "comment"  ) := .("Sellaphora seminulum"                , "algaebase"     , "typo")]
data[original_name == "Naviculadicta seminulum (Grunow) Lange Bertalot f. anormale"     , c("species_new", "name_source", "comment"  ) := .("Sellaphora seminulum"                , "algaebase"     , "typo")]
data[original_name == "Neidium longiseps"                                               , c("species_new", "name_source", "comment"  ) := .("Neidium affine Complex"              , "fwb_manual"    , "typo")]
data[original_name == "Nitzschia homburgensis"                                          , c("species_new", "name_source"             ) := .("Nitzschia homburgensis"              , "fwb_manual")]
data[original_name == "Nitzschia subtilissima"                                          , c("species_new", "name_source", "comment"  ) := .("Nitzschia subtilis"                  , "omnida_manual" , "typo")]
data[original_name == "Pinnularia acrospheria var. acrospheria"                         , c("species_new", "name_source", "genus_new") := .(NA, "fwb_manual"                      , "Pinnularia")]
data[original_name == "Planothidium pericava"                                           , c("species_new", "name_source", "comment"  ) := .("Planothidium pericavum"              , "omnida_clean"  , "typo")]
data[original_name == "Psammodictyon constricta"                                        , c("species_new", "name_source", "comment"  ) := .("Psammodictyon constrictum"           , "algaebase"     , "typo")]
data[original_name == "Psammothidium grishunum f.daonensis"                             , c("species_new", "name_source"             ) := .("Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", "fwb_manual")]
data[original_name == "Stauroneis phonicenteron"                                        , c("species_new", "name_source"             ) := .("Stauroneis phoenicenteron Complex"   , "fwb_manual")]
data[original_name == "Staurosirella leptostauron (Ehr.) Williams & Round"              , c("species_new", "name_source"             ) := .("Staurosirella leptostauron complex"  , "fwb_manual")]
data[original_name == "Stenopterobia intermedia var. crassior"                          , c("species_new", "name_source", "genus_new") := .(NA, "fwb_manual"                      , "Stenopterobia")]
data[original_name == "Stephanodiscus dubius"                                           , c("species_new", "name_source"             ) := .("Cyclostephanos dubius"               , "fwb_manual")]
  

# All entries without species_new also don't have a species entry 
data[is.na(species) & is.na(species_new), .N] == data[is.na(species), .N]

# also check visually 
data[is.na(species_new), sort(unique(original_name))]

data[is.na(species_new) & !is.na(name_fwb)                             , c("genus_new", "name_source") := .(name_fwb,                "fwb")]
data[is.na(species_new) & !is.na(name_fwb)           & is.na(genus_new), c("genus_new", "name_source") := .(name_fwb_clean,          "fwb_clean")]
data[is.na(species_new) & !is.na(name_fwb)           & is.na(genus_new), c("genus_new", "name_source") := .(name_omnida,             "onmida")]
data[is.na(species_new) & !is.na(name_fwb)           & is.na(genus_new), c("genus_new", "name_source") := .(name_omnida_clean,       "onmida_clean")]
data[is.na(species_new) & !is.na(name_fwb)           & is.na(genus_new), c("genus_new", "name_source") := .(name_clean_omnida_clean, "onmida_cc")]
data[is.na(species_new) & !is.na(name_maria)         & is.na(genus_new), c("genus_new", "name_source") := .(name_maria,              "maria") ]
data[is.na(species_new) & !is.na(name_maria_cleaned) & is.na(genus_new), c("genus_new", "name_source") := .(name_maria_cleaned,      "maria_cleaned") ]
data[is.na(species_new) & !is.na(name_petra)         & is.na(genus_new), c("genus_new", "name_source") := .(name_petra,              "petra") ]
data[is.na(species_new) & !is.na(name_petra_clean)   & is.na(genus_new), c("genus_new", "name_source") := .(name_petra_clean,        "petra_clean") ]
data[is.na(species_new) & !is.na(name_sanne)         & is.na(genus_new), c("genus_new", "name_source") := .(name_sanne,              "sanne") ]
data[is.na(species_new) & !is.na(name_sanne)         & is.na(genus_new), c("genus_new", "name_source") := .(name_sanne_clean,        "sanne_clean") ]

data[is.na(species_new) & is.na(genus_new)]
data[original_name == "Navicula [1]"                                     , c("genus_new", "name_source") := .("Navicula", "manual")]
data[original_name == "Navicula(dicta) seminulum (Grunow) Lange Bertalot", species_new                   := "Eolimna minima-seminulum-atomoides"]
data[species_new   == "Ulnaria ulna"                                     , species_new                   := "Ulnaria ulna complex"]
data[species_new   == "Syndedra cyclopum/Hannaea arcus"                  , c("species_new", "genus_new") := .("Hannaea arcus", "Hannaea")]
data[species_new   == "Hannaea arcus"                                    , genus_new                     := "Hannaea"]
data[species_new   == "Cyclotella cf"                                    , c("species_new", "name_source") := .("Cyclotella distinguenda complex", "fwb_manual")]
if (nrow(data[is.na(species_new) & is.na(genus_new)]) == 0) print("Quality Check passed")

# The genus will have changed sometimes. I account for that by extracting the first word from each species_new 
data[is.na(genus_new), genus_new := word(species_new, 1)]
data[                , genus_new := clean_names(genus_new)]
data[                , genus_new := str_to_title(genus_new)]

data[genus_new     == "Hippondonta"                                      , genus_new                     := "Hippodonta"]

sort(unique(data$genus_new))

# Especially the fwb data set sometimes summarize diverse species to one genus.
# In these cases I need to take out the species_new observation and replace it
# with an NA.
data[species_new == genus_new, species_new := NA]

data[, genus_compare := genus == genus_new]
data[genus_compare == T, c("family_new", "order_new") := .(family, order)]

TU_changed_taxonomiy <- data[genus_compare == F, sort(unique(genus_new))]

for (i  in seq_along(TU_changed_taxonomiy)) {
   loop_var <- TU_changed_taxonomiy[i]
   if (loop_var == "Na")       next()
   new_family_variable <- data[genus == loop_var, unique(family)]
   new_order_variable  <- data[genus == loop_var, unique(order)]
   if (length(new_family_variable) != 1 |
       length(new_order_variable) != 1) {
      print(loop_var)
      next()
   }
   data[genus_new == loop_var, c("family_new", "order_new") := .(new_family_variable, new_order_variable)]
}

## -- manual fixes -- ## 
data[genus_new == "Campylodiscus", c("family_new", "order_new") := .("Surirellaceae"   , "Surirellales" )]
data[genus_new == "Ceratoneis"   , c("family_new", "order_new") := .("Fragilariaceae"  , "Fragilariales")]
data[genus_new == "Cocconeiopsis", c("family_new", "order_new") := .("Cocconeidaceae"  , "Cocconeidales")]
data[genus_new == "Delicata"     , c("family_new", "order_new") := .("Gomphonemataceae", "Cymbellales"  )]
data[genus_new == "Distrionella" , c("family_new", "order_new") := .("Tabellariaceae"  , "Tabellariales")]
data[genus_new == "Navicymbula"  , c("family_new", "order_new") := .("Cymbellaceae"    , "Cymbellales"  )]
data[genus_new == "Pulchella"    , c("family_new", "order_new") := .("Naviculaceae"    , "Naviculales"  )]
data[genus_new == "Scoliopleura" , c("family_new", "order_new") := .("Neidiaceae"      , "Naviculales"  )]
data[genus_new == "Crenotia"     , c("family_new", "order_new") := .("Achnanthidiaceae", "Cocconeidales")]

## -- Quality Check -- ## 
if(nrow(data[is.na(genus_new)]) == 0 & nrow(data[is.na(family_new) & genus_compare == F]) == 0)print("Quality Check passed") else print("Quality check failed")
## --               -- ##

## Check that for each species has only one genus each genus only one family and each family only one order
# If nothing except for NA is printed everything is fine. 
uni_spe <- unique(data$species_new)
uni_gen <- unique(data$genus_new)
uni_fam <- unique(data$family_new)

for (i in 1:3) {
   loop_var_i <- switch(i, uni_spe, uni_gen, uni_fam)
   for (k in seq_along(loop_var_i)) {
      if (i == 1) {
         if (length(data[species_new == loop_var_i[k], unique(genus_new)]) != 1) print(loop_var_i[k])
      } else if (i == 2) {
         if (length(data[genus_new == loop_var_i[k], unique(family_new)]) != 1) print(loop_var_i[k])
      } else {
         if (length(data[family_new == loop_var_i[k], unique(order_new)]) != 1) print(loop_var_i[k])
      }
   }
}

data2 <- data[, c(
   "gr_sample_id",
   "WSO1_ID",
   "original_name",
   "species",
   "species_new",
   "genus_new",
   "family_new",
   "order_new",
   "name_source",
   "data.set",
   "geometry",
   "original_site_name",
   "date",
   "year",
   "season",
   "site_id",
   "date_id",
   "comment"
)]

## --  Quality Check -- ##  
#data2[is.na(species_new)] 
if(data2[is.na(genus_new), .N] == 0 & data2[is.na(family_new), .N] == 0 & data2[is.na(order_new), .N] == 0) print("Quality Check Passed") else print("Quality Check Failed")

# change names 
names(data2)[4:8] <- c("species_old","species", "genus", "family", "order")

data2[!(str_detect(species, "/"      ) | 
        str_detect(species, "complex") | 
        str_detect(species, "Complex") |
        str_detect(species, "\\++"   ) |
        str_detect(species, "-"      )), 
      species_clean := upper_first(clean_names(species))]

data2[str_detect(species, "Lange-Bertalot"), species_clean := upper_first(clean_names(species)) ]
data2[str_detect(species, "Cleve-Euler"), species_clean := upper_first(clean_names(species)) ]
data2[str_detect(species, "Stachura-Suchoples"), species_clean := upper_first(clean_names(species)) ]

data2[is.na(species_clean), species_clean := species]

data2[species_clean == "Fragilaria capucina", species_clean := "Fragilaria capucina complex"]

data2[, species := species_clean]

# new data set only for viewing 
# test_set <- unique(data2, by = c("species", "species_clean"))
# test_set <- test_set[,c("species", "species_clean") ]
# setorderv(test_set, "species")
# View(test_set)
# test_set <- unique(data2, by = c("original_name", "species_clean", "genus"))
# test_set <- test_set[,c("original_name", "species_clean", "name_source") ]
# setorderv(test_set, "original_name")
# write.xlsx2(test_set, paste0(Sys.Date(),"_test_set.xlsx"))
# setorderv(test_set, "species")
# write.xlsx2(test_set, "test_set2.xlsx")

##### ----- fixer calls used to be here. Now moved to 005x_fixer_calls.R ###


# In some cases new names are synonyms ---------------------------------------

# This needs to be run several time for cases were even the improvements are bad. 
# Run until no numbers are printed by the loop. 
data3 <- copy(data2)
data3$double_checked = NA
# start iterations here 
TU_loop <- sort(unique(data3$species))

counter <- 0
for (i in seq_along(TU_loop)) {
   # given a new species name 'loop_var'
   loop_var <- TU_loop[i]
   # skip if it is NA
   if (is.na(loop_var))
      next()
   # what source is the name from
   loop_var2 <- data3[species == loop_var, unique(name_source)]
   loop_var2 <-
      unique(str_remove_all(string = loop_var2, pattern = "_[a-z]*"))
   loop_bool <- data3[species == loop_var, unique(double_checked)]
   
   # if the name already comes from fwb - there is nothing to improve upon here
   if (!is.na(loop_bool) & loop_bool) next()
   if (!(
      loop_var %in% fwb$old_name       |
      loop_var %in% fwb$clean_old      |
      loop_var %in% omnida$taxon       |
      loop_var %in% omnida$taxon_clean |
      loop_var %in% petra$name         |
      loop_var %in% petra$taxon_clean
   )) {
      data3[species == loop_var, double_checked := T]
      next()
   }
   
   # fwb old name
   if (loop_var %in% fwb$old_name) {
      change_id <- which(fwb$old_name == loop_var)
      if (fwb$new_name[change_id] == fwb$old_name[change_id]){
         data3[species == loop_var, double_checked := T]
         next()
      }
      data3[species == loop_var, c("species", "name_source") := .(fwb$new_name[change_id], "fwb_rerun")]
      counter <- counter + 1
      next()
   } else if (loop_var %in% fwb$clean_old) {
      change_id <- which(fwb$clean_old == loop_var)
      if (length(change_id != 1))
         change_id <- change_id[1]
      if (fwb$new_name[change_id] == fwb$clean_old[change_id]){
         data3[species == loop_var, double_checked := T]
         next()
      }
      data3[species == loop_var, c("species", "name_source") := .(fwb$new_name[change_id], "fwb_rerun")]
      counter <- counter + 1
      next()
   } else if (loop_var %in% omnida$taxon_clean) {
      if ("fwb" %in% loop_var2) {
         data3[species == loop_var, double_checked := T]
         next()
      }
      change_id <- which(omnida$taxon_clean == loop_var)
      if (all(is.na(omnida$synonym[change_id]))) {
         data3[species == loop_var, double_checked := T]
         next()
      }
      if (length(change_id) != 1) {
         all_options <- paste(1:length(change_id), omnida$taxon[change_id], collapse = "\n")
         writeLines(paste("For", loop_var, "there are \n", all_options))
         read_id <- readline()
         if (read_id == "exit") break()
         change_id <- change_id[as.numeric(read_id)]
      }
      synonym_code    <- omnida$synonym[change_id]
      if (!synonym_code %in% omnida$code){
         data3[species == loop_var, double_checked := T]
         next()
      }
         
      new_id <- which(omnida$code == synonym_code)
      writeLines(
         paste(
            "For",
            loop_var,
            "which was read as",
            "\n",
            omnida$taxon[change_id],
            "\n",
            "omnida suggests",
            "\n",
            omnida$taxon[new_id]
         )
      )
      enter_new_name <- readline("new name:")
      if (enter_new_name == "exit") break()
      if (str_length(enter_new_name) == 1) {
         data3[species == loop_var, species := omnida$taxon[new_id]]
         data3[species == loop_var, name_source := "omnida_rerun"]
         counter <- counter + 1
         next()
      } else if (str_length(enter_new_name) > 1) {
         data3[species == loop_var, species := enter_new_name]
         data3[species == loop_var, name_source := "omnida_rerun"]
         counter <- counter + 1
         next()
      }
      
   } else if (loop_var %in% omnida$taxon) {
      if ("fwb" %in% loop_var2) {
         data3[species == loop_var, double_checked := T]
         next()
      }
        
      change_id <- which(omnida$taxon == loop_var)
      if (all(is.na(omnida$synonym[change_id]))) next()
      if (length(change_id) != 1) {
         all_options <- paste(1:length(change_id), omnida$taxon[change_id], collapse = "\n")
         writeLines(paste("For", loop_var, "there are \n", all_options))
         read_id <- readline()
         if (read_id == "exit") break()
         change_id <- change_id[as.numeric(read_id)]
      }
      synonym_code    <- omnida$synonym[change_id]
      if (!synonym_code %in% omnida$code) {
         data3[species == loop_var, double_checked := T]
         next()
      }
         
      new_id <- which(omnida$code == synonym_code)
      writeLines(
         paste(
            "For",
            loop_var,
            "which was read as",
            "\n",
            omnida$taxon[change_id],
            "\n",
            "omnida suggests",
            "\n",
            omnida$taxon[new_id]
         )
      )
      enter_new_name <- readline("new name:")
      if (enter_new_name == "exit") break()
      if (str_length(enter_new_name) == 1) {
         data3[species == loop_var, species := omnida$taxon[new_id]]
         data3[species == loop_var, name_source := "omnida_rerun"]
         counter <- counter + 1
         next()
      } else if (str_length(enter_new_name) > 1) {
         data3[species == loop_var, species := enter_new_name]
         data3[species == loop_var, name_source := "omnida_rerun"]
         counter <- counter + 1
         next()
      }
      
   }
   
}


data3[species == "Discostella  complex", species := "Discostella complex"]
data3[species == "Fallacia lenzi(Hustedt) Lange-Bertalot", species := "Fallacia lenzi"]
data3[species == "Achnanthidium biasolettianum(Grunow) Lange-Bertalot abnormal form", species := "Achnanthidium minutissimum"]
data3[species == "Achnanthidium pyrenaicum (Hustedt) Kobayasi" , species := "Achnanthidium minutissimum"]
data3[species == "Adlafia bryophila"                           , species := "Kobayasiella Complex"]
data3[species == "Adlafia minuscula"                           , species := "Navicula krasskei-egregia-minuscula"]
data3[species == "Brachysira vitrea (Grunow) Ross in Hartley"  , species := "Brachysira vitrea Complex"]
data3[species == "Encyonema prostratrum"                       , species := "Encyonema prostratum"]
data3[species == "Fragilaria virescens Ralfs"                  , species := "Fragilaria virescens complex"]
data3[species == "Geissleria ignota"                           , species := "Geissleria ignota complex"]
data3[species == "Navicula lanceolata"                         , species := "Navicula lanceolata complex"]
data3[species %in% c("Navicula sancti",
                     "Navicula sancti-naumii Levkov et Metzeltin"), species := "Navicula sancti-naumii"]
data3[species == "Navicula rhyncocephala"                         , species := "Navicula rhynchocephala"]
data3[species == "Nitzschia wuellerstorfii"                       , species := "Nitzschia wuellerstorffii"]
data3[species == "Planothidium daui (Foged) Lange-Bertalot"       , species := "Planothidium daui-granum"]
data3[species == "Stauroneis complex small capitate"              , species := "Stauroneis complex small"]
data3[species %in% c("Ulnaria ulna (Nitzsch.) Compère",
                     "Ulnaria ulna (Nitzsch.) Compère abnormal form",
                     "Ulnaria ulna (Nitzsch.) Compère var. acus (Kütz.) Lange-Bertalot"),
      species := "Ulnaria ulna complex"]


data3[species %in% c("Actinocyclus normanii(Greg. ex Grev.) Hustedt morphotype normanii",
                     "Achnanthidium subhudsonis (Hustedt) H. Kobayasi",
                     "Achnanthidium minutissimum (Kützing) Czarnecki",
                     "Aneumastus tusculus (Ehrenberg) D.G. Mann & Stickle",
                     "Bacillaria socialis(Gregory) Ralfs",
                     "Brachysira styriaca (Grunow) Ross in Hartley", 
                     "Geissleria decussis(Ostrup) Lange-Bertalot & Metzeltin", 
                     "Karayevia ploenensis (Hustedt) Bukhtiyarova",
                     "Lemnicola hungarica (Grunow) Round & Basson",
                     "Naviculadicta laterostrata Hustedt",
                     "Nitzschia filiformis var.conferta (Richter) Lange-Bertalot",
                     "Nupela lapidosa (Lange-Bertalot) Lange-Bertalot var.lapidosa",
                     "Planothidium delicatulum(Kütz.) Round & Bukhtiyarova",
                     "Planothidium lanceolatum(Breb. ex Kütz.) Lange-Bertalot abnormal fo.",
                     "Psammothidium curtissimum (Carter) Aboal",
                     "Reimeria sinuata (Gregory) Kociolek & Stoermer",
                     "Shionodiscus oestrupii (Ostenfeld) Adverson Kang & Theriot",
                     "Staurosira brevistriata (Grunow) Grunow",
                     "Staurosira lapponica (Grunow) Lange-Bertalot",
                     "Stephanodiscus hantzschii fo.tenuis(Hustedt)Hakansson et Stoermer",
                     "Surirella brebissonii Krammer & Lange-Bertalot var.brebissonii",
                     "Tabularia fasciculata (Agardh)Williams et Round",
                     "Tabularia tabulata (C.A.Agardh) Snoeijs",
                     "Tabularia tabulata (C.A.Agardh) Snoeijs abnormal form",
                     "Thalassiosira lacustris (Grunow) Hasle in Hasle & Fryxell",
                     "Tryblionella calida (grunow in Cl. & Grun.) D.G. Mann",
                     "Tryblionella coarctata (Grunow in Cl. & Grun.) D.G. Mann",
                     "Tryblionella punctata Wm. Smith",
                     "Caloneis fontinalis (Grunow) Cleve-Euler",
                     "Cosmioneis pusilla (W.Smith) Mann & Stickle",
                     "Craticula accomoda (Hustedt) Mann",
                     "Craticula halophila (Grunow ex Van Heurck) Mann",
                     "Ctenophora pulchella (Ralfs ex Kütz.) Williams et Round",
                     "Cymatopleura solea (Brebisson in Breb. & Godey) W.Smith var.solea",
                     "Cymbella lange-bertalotii Krammer",
                     "Cymbopleura cuspidata (Kützing) Krammer",
                     "Cymbopleura subaequalis (Grunow) Krammer var. subaequalis",
                     "Diadesmis contenta (Grunow ex V. Heurck) Mann",
                     "Encyonema perpusillum (A. Cleve) D.G. Mann",
                     "Encyonopsis cesatii (Rabenhorst) Krammer",
                     "Encyonopsis falaisensis (Grunow) Krammer",
                     "Encyonopsis microcephala (Grunow) Krammer",
                     "Eolimna minima(Grunow) Lange-Bertalot",
                     "Eucocconeis austriaca (Hustedt)Lange-Bertalot",
                     "Fragilaria capucina Desmazieres var.vaucheriae(Kützing)Lange-Bertalot",
                     "Gomphonema rhombicum M. Schmidt",
                     "Gyrosigma obtusatum (Sullivan & Wormley) Boyer",
                     "Gyrosigma sciotense (Sullivan et Wormley) Cleve",
                     "Halamphora coffeaeformis (Agardh) Levkov",
                     "Hippodonta costulata (Grunow)Lange-Bertalot Metzeltin & Witkowski",
                     "Luticola mutica (Kützing) D.G. Mann",
                     "Lyrella abrupta (Gregory) D.G. Mann",
                     "Mayamaea atomus (Kützing) Lange-Bertalot",
                     "Nitzschia linearis(Agardh) W.M.Smith var.tenuis (W.Smith) Grunow",
                     "Nitzschia panduriformis Gregory var. panduriformis",
                     "Placoneis amphibola (Cleve) E.J. Cox",
                     "Placoneis elginensis (Greg) Cox",
                     "Planothidium minutissimum (Krasske) Morales"
                     ), species := upper_first(clean_names(species))]

View(sort(unique(data3$species))) 

data4 <- copy(data3)


# update genera again ----------------------------------------------------

## -- if a genus is new to the data set I need to enter the family and order manually 
#data3[genus_new2 == "Crenotia", c("genus", "family", "order") := .("Crenotia", )]


# Run this up unto and including the for loop twice to make sure it worked. If
# it worked the loop should not print numbers.
data3[, genus_new2 := word(species, 1)]
data3[, genus_check2 := genus == genus_new2]
data3[genus_check2 == FALSE]
data3[species == genus_new2, species := NA]
data3[genus_new2 == "Achanthes"   , c("species", "genus", "genus_new2")          := .("Achnanthes impexiformis", "Achnanthes", "Achnanthes")]
data3[genus_new2 == "Hippondonta" , c("species", "genus", "genus_new2")          := .(NA, "Hippodonta", "Hippodonta")]
data3[genus_new2 == "Shionodiscus", c("genus","family", "order", "genus_check2") := .("Shionodiscus", "Thalassiosiraceae", "Thalassiosirales", TRUE)]
data3[genus_new2 == "Syndedra"    , c("species", "genus", "genus_new2")          := .("Hannaea arcus", "Hannaea", "Hannaea")]


# unique new genera
data3[, genus_check2 := genus == genus_new2]
ung <- sort(unique(data3$genus_new2))
for (i in seq_along(ung)) {
   
   # skip this new genus (i) if none of its entries have mismatch between genus and first word of species
   if (data3[genus_new2 == ung[i] & genus_check2 == FALSE, .N] == 0) {next()
   } else {
      print(i)
      # name of the new genus 
      loop_newgenus  <- data3[genus_new2 == ung[i] & genus_check2 == FALSE, unique(genus_new2)]
      loop_newfamily <- data3[genus == loop_newgenus, unique(family)]
      loop_neworder  <- data3[family == loop_newfamily, unique(order)]
      data3[genus_new2 == ung[i] &
               genus_check2 == FALSE, c("genus", "family", "order") := .(loop_newgenus, loop_newfamily, loop_neworder)]
   }
   
} #fl1 

data3[species == genus, species := NA]

# second excel check  -----------------------------------------------------
test_set <- unique(data3, by = c("original_name", "species", "genus"))
test_set <- test_set[,c("original_name", "species", "genus", "name_source") ]
setorderv(test_set, "original_name")
write.xlsx2(test_set, paste0(Sys.Date(),"_test_set.xlsx"))

# some more manual cleans necessary ---------------------------------------

data3[species == "Bacillaria paxillifer", species := "Bacillaria paxillifera"]
data3[species == "Diatoma vulgare"      , species := "Diatoma vulgaris"]


data3[original_name == "Diploneis cf. elliptica (Kützing) Clev"      , species := "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica"]
data3[original_name == "Diploneis petersenii Hustedt"      , species := NA]
data3[original_name == "Encyonopsis lange-bertalotii"      , species := "Encyonopsis lange-bertalotii"]
data3[original_name %in% c("Fragilaria capucina var. vaucheriae abnormal form",
                           "Fragilaria capucina var.vaucheriae(Kütz.)Lange-Bertalot f. anormale",
                           "Fragilaria capucina vaucheriae",
                           "Fragilaria capucina Desmazieres var.vaucheriae(Kützing)Lange-Bertalot",
                           "Fragilaria vaucheriae",
                           "Fragilaria vaucheriae (K.) Pet. var.elliptica Manguin ex Kociolek & Revier",
                           "Fragilaria vaucheriae (Kützing) Petersen",
                           "Fragilaria vaucheriae var. capitellata"
                           ), c("species") := "Fragilaria virescens complex"]
data3[original_name == "Gomphonema cf. gracile Ehrenberg" , species := "Gomphonema gracile complex"]
data3[original_name == "Gomphonema cf. clavatum Ehrenberg", species := "Gomphonema clavatum"]
data3[original_name == "Gomphonema cf. pumilum (Grunow) Reichardt", species := "Gomphonema pumilum complex"]
data3[original_name %in% c("Navicula bryophila","Navicula bryophila Petersen"), c("species", "genus") := .("Kobayasiella Complex", "Kobayasiella")]
data3[original_name %in% c("Navicula cataracta-rheni","Navicula cataracta-rheni Lange-Bertalot"), c("species") := .("Navicula cataracta-rheni")]
data3[original_name == "Navicula jarnefeltii"                               , species := "Cavinula cocconeiformis/jaernefeltii"]
data3[original_name == "Nitzschia cf. bavarica Hustedt"                     , species := "Nitzschia bavarica Complex"]
data3[original_name == "Nitzschia cf. dissipata var. media (Kützing) Grunow", species := "Nitzschia dissipata-recta Complex"]
data3[original_name == "Nitzschia cf. palea (Kützing) W. Smith"             , species := "Nitzschia palea-paleacea"]
data3[original_name %in% c("Nitzschia levidensis group salinarum",
                           "Nitzschia levidensis v.salinarum"), species := NA]
data3[original_name == "Pinnularia cf. pisciculus Ehrenberg"  , species := "Pinnularia pisciculus"]
data3[original_name == "Pinnularia roland-schmidtii Metzeltin & Lange-Bertalot", species := "Pinnularia roland-schmidtii"]
data3[original_name == "Stauroneis kriegerii", species := "Stauroneis complex small"]

# Save to file ------------------------------------------------------------
saveRDS(data3, paste0("003_processed_data/005_",Sys.Date(),"_clean_diatoms_observations.RDS"))

new_and_old_names <- unique(data3, by = c("original_name", "species", "genus"))
new_and_old_names <- new_and_old_names[,c("original_name", "species", "genus", "name_source") ]
setorderv(new_and_old_names, "original_name")
write.xlsx2(new_and_old_names, paste0(Sys.Date(),"_new_and_old_names.xlsx"))


## -- cleaner 
rm(list = ls());gc()


# Old stuff ---------------------------------------------------------------

## -- along original names for omnida
# for (i in seq_along(TU)) {
#    if (TU[i] %in% omnida$taxon) {
#       id <- which(omnida$taxon == TU[i])
#       
#       if (length(id) > 1) {
#          syn <- omnida$synonym[id]
#          id_acc <- which(is.na(syn))
#          if (length(id_acc) == 0) {
#             print("breaking")
#             break()
#          }
#          id <- id[id_acc]
#          
#       }
#       
#       if (is.na(omnida$synonym[id])) {
#          data[original_name == TU[i], name_omnida := omnida$taxon[id]]
#          print(i)
#          next()
#       }
#       if (!is.na(omnida$synonym[id])) {
#          new_code <- omnida$synonym[id]
#          new_id   <- which(omnida$code == new_code)
#          if (length(new_id) == 0) next()
#          new_name <- omnida$taxon[new_id]
#          # check that this is accepted
#          if (!is.na(omnida$synonym[new_id])) {
#             new_code <- omnida$synonym[id]
#             new_id   <- which(omnida$code == new_code)
#             new_name <- omnida$taxon[new_id]
#             if (!is.na(omnida$synonym[new_id])) {
#                new_code <- omnida$synonym[id]
#                new_id   <- which(omnida$code == new_code)
#                new_name <- omnida$taxon[new_id]
#             }
#             
#          }
#          if (!is.na(omnida$synonym[new_id])) {
#             print(paste("broken by", i))
#             break()
#          }
#          if (is.na(omnida$synonym[new_id])) {
#             data[original_name == TU[i], name_omnida := omnida$taxon[new_id]]
#          }
#       }
#    }
#    
#    print(paste(i, "of", length(TU)))
# }
