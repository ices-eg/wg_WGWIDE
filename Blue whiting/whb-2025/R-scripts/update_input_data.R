####################################################################################################################-
# Load input data and update with newest available from current assessment year
####################################################################################################################-

# Load input data
cn      <- read.ices(file.path(stock.dir,"data/cn.dat"))
cw      <- read.ices(file.path(stock.dir,"data/cw.dat"))
dw      <- read.ices(file.path(stock.dir,"data/dw.dat"))
lw      <- read.ices(file.path(stock.dir,"data/lw.dat"))
mo      <- read.ices(file.path(stock.dir,"data/mo.dat"))
nm      <- read.ices(file.path(stock.dir,"data/nm.dat"))
pf      <- read.ices(file.path(stock.dir,"data/pf.dat"))
pm      <- read.ices(file.path(stock.dir,"data/pm.dat"))
sw      <- read.ices(file.path(stock.dir,"data/sw.dat"))
lf      <- read.ices(file.path(stock.dir,"data/lf.dat"))
surveys <- read.ices(file.path(stock.dir,"data/survey.dat"))

# Replace the preliminary data from last year's assessment with the final data from this year's assessment
## Catch numbers
cn[nrow(cn),]             <- final$canum

## Catch weight at age
cw[nrow(cw),]             <- final$weca

# Add the preliminary data for this year's assessment
## Raised preliminary catch numbers
cn                        <- rbind(cn, newCscaled)
row.names(cn)[nrow(cn)]   <- assessmentYear

## Preliminary catch weigth at age
cw                        <- rbind(cw, newW)
row.names(cw)[nrow(cw)]   <- assessmentYear

# Store catch weight at age as discards, landings and stock weight at age
dw                        <- cw
lw                        <- cw
sw                        <- cw

# Add new row of data to remaining data files to represent current assessment year
lf                        <- rbind(lf, lf[nrow(lf),])
mo                        <- rbind(mo, mo[nrow(mo),])
nm                        <- rbind(nm, nm[nrow(nm),])
pf                        <- rbind(pf, pf[nrow(pf),])
pm                        <- rbind(pm, pm[nrow(pm),])
rownames(lf) <- rownames(mo) <- rownames(nm) <- rownames(pf) <- rownames(pm) <- as.character(c(1981:assessmentYear))

# Add survey data of current assessment year
## Save attributes from original survey file
attrs <- attributes(surveys$IBWSS)

## Add row with new data
surveys$IBWSS             <- rbind(surveys$IBWSS, survey_values)
rownames(surveys$IBWSS)[nrow(surveys$IBWSS)]   <- as.character(assessmentYear)

## Add new year to attributes
attrs$dim                 <- dim(surveys$IBWSS)
attrs$dimnames[[1]]       <- c(attrs$dimnames[[1]], as.character(assessmentYear))

## Restore the attributes
attributes(surveys$IBWSS) <- attrs

## Function to save survey data file
write.survey.file<-function (surveys, fileout, ...) 
{
  top <- paste0(fileout, " auto written\n", 100 + length(surveys))
  write(top, fileout, ...)
  for (s in 1:length(surveys)) {
    write(paste0(names(surveys)[s]), fileout, append = TRUE, ...)
    S <- surveys[[s]]
    yr <- range(as.integer(rownames(S)))
    ar <- range(as.integer(colnames(S)))
    write(paste0(yr[1], " ", yr[2]), fileout, append = TRUE, ...)
    st <- attr(S,"time")
    write(paste0(1, " ", 1, " ", st[1], " ", st[2]), fileout, append = TRUE, ...)
    write(paste0(ar[1], " ", ar[2]), fileout, append = TRUE, ...)
    x <- cbind(1, S)
    write(t(x), fileout, ncolumns = ncol(x), append = TRUE, sep = "  \t", ...)
  }
}

# Save input data files
write.ices(cn, fileout = file.path(stock.dir,"data/cn.dat"))
write.ices(cw, fileout = file.path(stock.dir,"data/cw.dat"))
write.ices(dw, fileout = file.path(stock.dir,"data/dw.dat"))
write.ices(lw, fileout = file.path(stock.dir,"data/lw.dat"))
write.ices(mo, fileout = file.path(stock.dir,"data/mo.dat"))
write.ices(nm, fileout = file.path(stock.dir,"data/nm.dat"))
write.ices(pf, fileout = file.path(stock.dir,"data/pf.dat"))
write.ices(pm, fileout = file.path(stock.dir,"data/pm.dat"))
write.ices(sw, fileout = file.path(stock.dir,"data/sw.dat"))
write.ices(lf, fileout = file.path(stock.dir,"data/lf.dat"))
write.survey.file(surveys, fileout = file.path(stock.dir,"data/survey.dat"))
