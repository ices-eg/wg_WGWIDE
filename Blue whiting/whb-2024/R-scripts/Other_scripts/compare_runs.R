#  1. Load the data runs you want to compare

load(file.path("m:/WGWIDE/whb-2023/bw_2023_preliminary_catch",'run','Model.Rdata'),verbose=TRUE)
prelim<-fit

load(file.path("m:/WGWIDE/whb-2023/bw_2023_final_2023_catch",'run','Model.Rdata'),verbose=TRUE)
final<-fit

# load(file.path(year.root,"bw_2022_preliminary_2022_catch_prelim2021", 'run','Model.Rdata'),verbose=TRUE)
# prelim_2021_2022<-fit

# 2. put them all into a list of runs and assign the list as a "samset" object, to be able to use the default SAM output
comp<-list(WGWIDE_2023_prelim=prelim, WGWIDE_2023_final=final)
attr(comp, "class")<- "samset"

# plot using SAM scrips
fbarplot(comp)
ssbplot(comp)
recplot(comp)


# all in one plot
png(filename=file.path(stock.dir,'data','compare_runs_prelim_final.png'),width=900,height=1200,pointsize=25)
par(mfrow=c(3,1))
par(mar=c(c(2, 5, 2.0, 2) + 0.1)) #c(bottom, left, top, right) 
ssbplot(comp)
fbarplot(comp)
recplot(comp)
par(mfrow=c(1,1))
cleanup()
