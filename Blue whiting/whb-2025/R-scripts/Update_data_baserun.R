####################################################################################################################-
# Load input data and update with newest available from current assessment year
# Note: for base model run only, not for extended run for forecast
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

# Add newest catch data to input data
## Catch numbers
cn                        <- rbind(cn, final$canum)
row.names(cn)[nrow(cn)]   <- assessmentYear

## Catch weight at age
cw                        <- rbind(cw, final$weca)
row.names(cw)[nrow(cw)]   <- assessmentYear

## 'Save' catch weight at age as discards, landings and stock weight at age
dw                        <- cw
lw                        <- cw
sw                        <- cw

# Add survey data of current assessment year
## Save attributes from original survey file 
attrs                     <- attributes(surveys$IBWSS)

## Add row with new data
surveys$IBWSS             <- rbind(surveys$IBWSS, survey_values)
rownames(surveys$IBWSS)[nrow(surveys$IBWSS)]   <- as.character(assessmentYear)

## Add new year to attributes
attrs$dim                 <- dim(surveys$IBWSS)
attrs$dimnames[[1]]       <- c(attrs$dimnames[[1]], as.character(assessmentYear))

## Restore the attributes
attributes(surveys$IBWSS) <- attrs

# Add new row of data to remaining data files to represent current assessment year
lf                        <- rbind(lf, lf[nrow(lf),])
mo                        <- rbind(mo, mo[nrow(mo),])
nm                        <- rbind(nm, nm[nrow(nm),])
pf                        <- rbind(pf, pf[nrow(pf),])
pm                        <- rbind(pm, pm[nrow(pm),])
rownames(lf) <- rownames(mo) <- rownames(nm) <- rownames(pf) <- rownames(pm) <- as.character(c(1981:assessmentYear))

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
write.ices(surveys, fileout = file.path(stock.dir,"data/survey.dat"))