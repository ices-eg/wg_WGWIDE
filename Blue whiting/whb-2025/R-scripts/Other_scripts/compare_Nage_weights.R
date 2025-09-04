####################################################################################################################-
# Compare numbers at age and weights from this year's forecast with previous year's forecast
#
# Author: Esther D. Beukhof
####################################################################################################################-

library(stockassessment)
library(ggplot2)

# Set assessment year
assessmentYear   <- fit$data$years[(length(fit$data$years)-1)]

# Get overall working directory where all assessments are stored
wide.root       <-file.path(orig_wd, "Blue whiting")

# Get list of paths of assessments
assess <-c(
  file.path(wide.root, paste0('whb-',assessmentYear-1),paste0('bw_',assessmentYear-1,'_preliminary_catch_extended')),
  file.path(wide.root, paste0('whb-',assessmentYear),paste0('bw_',assessmentYear,'_preliminary_catch_extended'))
  )

# Path to save results
outPath         <-file.path(wide.root,paste0('whb-',assessmentYear),paste0('bw_',assessmentYear,'_preliminary_catch_extended'),'res')



####################################################################################################################-
# N at age -----

# Get estimated stock numbers at age from the two assessments
ntabList <- lapply(assess, function(root){
  print(root)
  # setwd(x) #set working directory
  load(paste0(root,"/run/model.RData"))
  ntab          <- ntable(fit)
})

# Create empty data frame
compareN        <- data.frame(Age = c(1:10))

# Calculate ratios between assessments for several years
# years           <- c(2019:2022) #if intermediate year is NOT included
#years           <- c(2019:2023) #if intermediate year is included
# years           <- c(2020:2024) #if intermediate year is included
years           <- c((assessmentYear-4):assessmentYear) #if intermediate year is included

for(iYear in 1:length(years)){
  # Extract data
  ntabCurrent         <- as.data.frame(ntabList[[2]])
  ntabPrevious        <- as.data.frame(ntabList[[1]])
  
  # Replace recruitment value in terminal year of previous assessment by GM
  if(iYear == length(years)){
    ntabPrevious[nrow(ntabPrevious),1] <- GMlast
  }
  
  # In case SAM assessment not used in most recent assessment, replace by GM
  if(iYear == length(years)){
    ntabCurrent[nrow(ntabCurrent)-1,1] <- ntabCurrent[nrow(ntabCurrent)-1,1] * my.nscale[1]
  }

  # Calculate ratio
  ratio               <- ntabCurrent[paste(years[iYear]),] / ntabPrevious[paste(years[iYear]),]
  ratioT              <- t(ratio)
  compareN            <- cbind(compareN,ratioT)
}

# Save
write.csv(compareN, file = paste0(outPath,"/compare Nage with previous assessment.csv"), row.names = FALSE)


# MV added, begin
library(tidyverse)
library(kableExtra)
#library(webshot2)
#library(magick)

compareN<-compareN %>% mutate_if(is.numeric,round,2)
pp<-max(abs(range(as.matrix(compareN)[,-1])-1))

if (FALSE) {
  display.brewer.pal(n = 11, name = 'RdBu')
  ccc<-brewer.pal(n = 11, name = "RdBu")
  # Use topo.colors
  barplot(1:11, col=rev(topo.colors(11)))
  
  # Use cm.colors
  barplot(1:11, col=rev(cm.colors(11)))
  cm<-cm.colors(11)
}

colorIt<-function(itab,rounds=2) {
  cm<-cm.colors(11)

  pp<-max(abs(range(as.matrix(itab)[,-1])-1))
  itab<-itab %>% mutate_if(is.numeric,round,rounds)
  cN<-itab
  
  cN[2:6] <- lapply(itab[2:6], function(x) {
    cell_spec(x, bold = F, 
              background = spec_color(x, palette=cm,scale_from=c(1-pp,1+pp)))
  })
  cN[1:1] <- lapply(itab[1:1], function(x) {
    cell_spec(x, bold = T)
  })

  kbl(cN, escape = F, align = "c") %>%
    kable_classic("striped", full_width = F) # does not work ? %>% save_kable("test.png")
}

colorIt(compareN) # you have to save the plot manually as as .png file



####################################################################################################################-
# N at age * Weight at age -----

# Get estimated stock numbers at age from the two assessments
ntabList <- lapply(assess, function(root){
  print(root)
  # setwd(x) #set working directory
  load(paste0(root,"/run/model.RData"))
  ntab          <- ntable(fit)
})

# Get estimated weight at age from the two assessments
wecaList <- lapply(assess, function(root){
  print(root)
  read.ices(paste0(root,'/data/cw.dat'))
})

# Create empty data frame
compareBiomass  <- data.frame(Age = c(1:10))

# Calculate ratios between assessments for several years
# years           <- c(2019:2022) #if intermediate year is NOT included
# years           <- c(2020:2024) #if intermediate year is included
years           <- c((assessmentYear-4):assessmentYear) #if intermediate year is included

for(iYear in 1:length(years)){
  # Select data
  ntabCurrent         <- as.data.frame(ntabList[[2]])
  ntabPrevious        <- as.data.frame(ntabList[[1]])
  wtabCurrent         <- as.data.frame(wecaList[[2]])
  wtabPrevious        <- as.data.frame(wecaList[[1]])
  
  # Replace recruitment value in terminal year of previous assessment by GM
  if(iYear == length(years)){
    ntabPrevious[nrow(ntabPrevious),1] <- GMlast
  }
  
  # In case SAM assessment not used in most recent assessment, replace by GM
  if(iYear == length(years)){
    ntabCurrent[nrow(ntabCurrent)-1,1] <- ntabCurrent[nrow(ntabCurrent)-1,1] * my.nscale[1]
  }
  
  # Drop forecast year from current assessment
  ntabCurrent         <- ntabCurrent[-nrow(ntabCurrent),]
  
  # Add average weights as latest weight for each forecast year
  wAddCurrent         <- colMeans(wtabCurrent[c((nrow(wtabCurrent)-2):nrow(wtabCurrent)),])
  wtabCurrent[nrow(wtabCurrent),] <- wAddCurrent

  wAddPrevious        <- colMeans(wtabPrevious[c((nrow(wtabPrevious)-2):nrow(wtabPrevious)),])
  wtabPrevious        <- rbind(wtabPrevious, wAddPrevious)
  rownames(wtabPrevious)[nrow(wtabPrevious)] <- years[length(years)]

  # Mulitiply N at age with weight at age
  nwCurrent           <- ntabCurrent * wtabCurrent
  nwPrevious          <- ntabPrevious * wtabPrevious
  
  # Calculate ratio
  ratio               <- nwCurrent[paste(years[iYear]),] / nwPrevious[paste(years[iYear]),]
  ratioT              <- t(ratio)
  compareBiomass      <- cbind(compareBiomass,ratioT)
  
  # For the final year plot mean weight at age used in current and previous forecast
  if(iYear == length(years)){
    
    # Set up data frame for plotting
    datPlot           <- data.frame(Age = rep(c(1:10),2),
                                    Weight = c(t(wtabCurrent[paste(years[iYear]),]),
                                               t(wtabPrevious[paste(years[iYear]),])),
                                    Assessment = c(rep(paste0("WGWIDE ", assessmentYear),10), rep(paste0("WGWIDE ", assessmentYear-1),10)))
    # Plot
    p <- ggplot(datPlot, aes(Age, Weight, colour=Assessment)) +
                  geom_point() +
                  geom_line() +
      scale_x_continuous(n.breaks = 10) +
      scale_y_continuous(limits = c(0,0.22), 
                         expand = c(0,0)) +
      labs(x="Age (year)", y="Mean weight (kg)") +
      theme_bw() +
      theme(legend.position = c(0.8,0.2),
            legend.box.background = element_rect(colour = "black"),
            legend.title = element_blank(),
            axis.text = element_text(size=12),
            axis.title = element_text(size=12),
            panel.grid.minor.y = element_blank())
    print(p)
    
    ggsave("compare mean weight at age forecast with previous assessment.png", p, path = outPath, width = 4, height = 3)
  }
}

# Save
write.csv(compareBiomass, file = paste0(outPath, "/compare Nage x weight at age with previous assessment.csv"), row.names = FALSE)

colorIt(compareBiomass) # you have to save the plot manually as as .png file


