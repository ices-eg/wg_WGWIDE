#  1. Load the data runs you want to compare

load(file.path("Blue whiting/whb-2025/bw_2025_preliminary_catch",'baserun','model.Rdata'),verbose=TRUE)
base<-fit

load(file.path("Blue whiting/whb-2025/bw_2025_preliminary_catch",'run','model.Rdata'),verbose=TRUE)
run<-fit

# 2. put them all into a list of runs and assign the list as a "samset" object, to be able to use the default SAM output
comp<-list(WGWIDE_2024=base, WGWIDE_2025=run)
attr(comp, "class")<- "samset"

# plot using SAM scrips
fbarplot(comp, addCI = TRUE)
ssbplot(comp, addCI = TRUE)
recplot(comp, addCI = TRUE)


# all in one plot
png(filename=file.path(stock.dir,'res','compare_runs_base.png'),width=900,height=1200,pointsize=25)
par(mfrow=c(3,1))
par(mar=c(c(2, 5, 2.0, 2) + 0.1)) #c(bottom, left, top, right) 
ssbplot(comp, addCI = TRUE)
fbarplot(comp, addCI = TRUE)
recplot(comp, addCI = TRUE)
par(mfrow=c(1,1))
dev.off()

