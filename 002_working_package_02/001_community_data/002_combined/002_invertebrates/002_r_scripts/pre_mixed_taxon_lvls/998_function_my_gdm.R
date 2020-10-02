my_format_table <- function(bioData,
          siteColumn = NULL,
          XColumn,
          YColumn,
          predData,
          sampleSites = 1) {
        
        
        toRemove <- NULL
        removeRand <- NULL
        distData <- NULL
        
        setDT(predData)
        setDT(bioData)
        
        locs   <- predData[, c(XColumn, YColumn), with = F]
        sppDat <- bioData[, 2:ncol(bioData)]
        sppDat[sppDat >= 1]   <- 1
        sppDat[sppDat == 0]   <- 0
        sppDat[is.na(sppDat)] <- 0
        
        fullSites <- bioData[, siteColumn, with = F]
        randRows <-
                sample(1:nrow(bioData), round(nrow(bioData) *
                                                      sampleSites, 0))
        bioData <- bioData[c(randRows), ]
        removeRand <-
                fullSites[which(!(fullSites %in% bioData[, siteColumn, with = F]))]
        
        
        colnames(bioData)[colnames(bioData) == siteColumn] <-
                "gettingCoolSiteColumn"
        colnames(predData)[colnames(predData) == siteColumn] <-
                "gettingCoolSiteColumn"
        predData <- unique(predData)
        predData <-
                predData[gettingCoolSiteColumn %in% bioData$gettingCoolSiteColumn]
        colnames(bioData)[colnames(bioData) == "gettingCoolSiteColumn"] <-
                siteColumn
        colnames(predData)[colnames(predData) == "gettingCoolSiteColumn"] <-
                siteColumn
        predSite <- which(names(predData) == siteColumn)
        bioSite <- which(names(bioData) == siteColumn)
        predData <- predData[order(predData[, predSite, with = F])]
        bioData <- bioData[order(bioData[, bioSite, with = F])]
        sppData <- bioData[, -1]
        
        # originally if abundance == F
        sppData[sppData >= 1] <- 1
        sppData[sppData == 0] <- 0
        sppData[is.na(sppData)] <- 0
        sppData <- as.matrix(sppData)
        distData <-
                parallelDist::parDist(sppData, method = "dice", threads = 10)
        s1.xCoord <-
                s1.yCoord <- s2.xCoord <- s2.yCoord <- NULL
        s1 <- s2 <- NULL
        
        if ((siteColumn %in% colnames(predData)) == T) {
                count <- seq(nrow(unique(predData[, siteColumn, with = F])) - 1,
                             1,-1)
        }
        
        s1 <-
                unlist(sapply(seq(length(count), 1), function(y) {
                        c(s1, rep((max(
                                count
                        ) - y) + 1, times = y))
                }))
        s2 <-
                unlist(sapply(seq(length(count), 1), function(y) {
                        c(s2, (max(count) - y + 2):(max(count) + 1))
                }))
        
        gdmTable <-
                data.table(distance = as.vector(distData),
                           weights = 1)
        if ((XColumn %in% colnames(predData)) == T) {
                if ((siteColumn %in% colnames(predData)) == T) {
                        checkTab <- table(predData[,
                                                   siteColumn,
                                                   with = F])
                }
                if (sum(checkTab > 1) > 0) {
                        stop(
                                "A site has two or more unique entries of data associated with it."
                        )
                }
                s1.xCoord <- predData[s1, XColumn, with = F]
                s2.xCoord <- predData[s2, XColumn, with = F]
                s1.yCoord <- predData[s1, YColumn, with = F]
                s2.yCoord <- predData[s2, YColumn, with = F]
        }
        gdmForm <-
                cbind(gdmTable, s1.xCoord, s1.yCoord, s2.xCoord,
                      s2.yCoord)
        xhold <- which(names(predData) == XColumn)
        yhold <- which(names(predData) == YColumn)
        sitehold <- which(names(predData) == siteColumn)
        sitehold2 <-
                which(names(predData) == "siteUltimateCoolness")
        predData <-
                predData[, c(xhold, yhold, sitehold, sitehold2) := NULL]
        if (ncol(predData) > 0) {
                gdmTableFill <- cbind(gdmForm, predData[s1, 1:ncol(predData)],
                                      predData[s2, 1:ncol(predData)])
                names.s1 <-
                        paste("s1.", names(predData[1:ncol(predData)]),
                              sep = "")
                names.s2 <-
                        paste("s2.", names(predData[1:ncol(predData)]),
                              sep = "")
                colnames(gdmTableFill) <-
                        c(colnames(gdmTableFill)[1:6],
                          names.s1, names.s2)
        }
        outTable <- gdmTableFill
        
        class(outTable) <- c("gdmData", "data.frame")
        return(outTable)
}        
        
        
        