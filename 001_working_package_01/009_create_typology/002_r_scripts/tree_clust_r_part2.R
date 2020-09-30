my_treeClustrpart <- function(i, dfx, d.num, control, rcontrol, weights) 
{
        outlist <- list(DevRat = 0, Size = 1)
        if (length(unique(dfx[, i])) == 1) 
                return(outlist)
        if (any(is.na(dfx[, i]))) 
                response.had.NAs <- TRUE
        else response.had.NAs <- FALSE
        if (is.element(class(dfx[, i]), c("factor", "character"))) 
                rcontrol$parms <- list(split = "information")
        resp.name <- names(dfx)[i]
        rcontrol$formula <- eval(parse(text = paste(resp.name, "~ .")))
        rcontrol$cost <- weights 
        cols.to.drop <- NULL
        while (1) {
                if (length(cols.to.drop) == 0) {
                        rcontrol$data <- dfx
                }
                else {
                        rcontrol$data <- dfx[, -cols.to.drop]
                }
                mytree <- do.call(rpart::rpart, rcontrol)
                if (nrow(mytree$frame) == 1) 
                        return(outlist)
                cptbl <- mytree$cptable
                min.cp.dex <- which(cptbl[, "xerror"] == min(cptbl[, 
                                                                   "xerror"]))[1]
                serule.value <- cptbl[min.cp.dex, "xerror"] + control$serule * 
                        cptbl[min.cp.dex, "xstd"]
                good.rows <- which(cptbl[, "xerror"] <= serule.value)
                if (length(good.rows) > 1) 
                        best.row <- good.rows[2]
                else best.row <- good.rows
                if (best.row == 1) 
                        return(outlist)
                mytree <- rpart::prune.rpart(mytree, cp = (cptbl[best.row, 
                                                                 "CP"] + cptbl[best.row - 1, "CP"])/2)
                if (is.factor(dfx[, i])) 
                        devs <- rp.deviance(mytree)
                else devs <- mytree$frame$dev
                orig.dev <- devs[1]
                new.dev <- sum(devs[mytree$frame$var == "<leaf>"])
                outlist$DevRat <- (orig.dev - new.dev)/orig.dev
                outlist$Size <- sum(mytree$frame[, "var"] == "<leaf>")
                if (outlist$DevRat <= control$DevRatThreshold) 
                        break
                cols.to.drop <- c(cols.to.drop, which(names(dfx) == 
                                                              mytree$frame[1, "var"]))
                outlist <- list(DevRat = 0, Size = 1)
                if (length(cols.to.drop) >= ncol(dfx) - 1) 
                        return(outlist)
        }
        if (response.had.NAs) {
                mytree$where.orig <- mytree$where
                mytree$where <- rpart.predict.leaves(mytree, dfx, type = "where")
                outlist$leaf.where <- factor(mytree$where)
        }
        else outlist$leaf.where <- factor(mytree$where)
        if (control$keep.trees) 
                outlist$tree <- mytree
        return(outlist)
}
