####################################################################################################################-
# Gather data from previous assessment to prepare for current assessment
# Note: for base model run only, not for extended run for forecast
####################################################################################################################-

# Set data path
## Check first if directories exists - if not, then create
if(!dir.exists(stock.dir) & extended == FALSE){
  
  # Create new directory for base model run
  dir.create(file.path(getwd(),stock.dir))
  
}
# if(!dir.exists(file.path(year.root,paste0("bw_",assessmentYear,"_preliminary_catch_extended"))) & extended == TRUE){
#   
#   # Create new directory for extended model run for forecast only
#   dir.create(file.path(getwd(),year.root,"bw_2025_preliminary_catch_extended"))
#   
# }

# The "extended version includes dummy survey data (NAs) for the year after the last catch year to get stock numbers and SSB at the
# start of the "intermediate" year (which in this case is the TAC year).
# Data for the extended are copied from the "normal" run, and an extra year (and data) is added for mo.dat, nm.dat, pf.dat. pm.dat, 
# sw.dat and survey.dat.
# For sw.dat, we duplicate weights in the last year (e.g. unchanged mean weight for the last two years in sw.dat)
# For survey.dat the new data line should be missing observations (- 1).


####################################################################################################################-
# Prepare for base model run ----

# Function to copy folder structure
copy_folder_structure <- function(source_dir, target_dir) {
  
  # Get all directories recursively from the source
  dirs <- list.dirs(path = source_dir, full.names = TRUE, recursive = TRUE)
  
  # Create corresponding directories in the target
  for (dir in dirs) {
    
    # Replace source path with target path
    new_dir <- sub(source_dir, target_dir, dir)
    if (!dir.exists(new_dir)) {
      dir.create(new_dir, recursive = TRUE)
    }
  }
}

# Function to copy files
copy_files <- function(file_list, target_dir) {
  
  for (file_path in file_list) {
    file_name <- basename(file_path)
    new_path <- file.path(target_dir, file_name)
    file.copy(file_path, new_path, overwrite = TRUE)
  }
}

# Create SAM folders based on previous assessment year
if(folder_structure == TRUE){
  copy_folder_structure(source_dir = file.path(file.path(getwd(),"Blue whiting", paste0('whb-',assessmentYear-1)),paste0("bw_",assessmentYear-1,"_preliminary_catch")),
                      target_dir = file.path(getwd(),stock.dir))
}

# Copy relevant files from previous assessment to current assessment
## Base run
if(base_run == TRUE){
  file.copy(from = file.path(file.path(getwd(),"Blue whiting", paste0('whb-',assessmentYear-1)),paste0("bw_",assessmentYear-1,"_preliminary_catch/run/model.RData")),
          to = file.path(getwd(),paste0(stock.dir,"/baserun")))
}

## Configuration file
if(config_file == TRUE){
  file.copy(from = file.path(file.path(getwd(),"Blue whiting", paste0('whb-',assessmentYear-1)),paste0("bw_",assessmentYear-1,"_preliminary_catch/conf/model.cfg")),
          to = file.path(getwd(),paste0(stock.dir,"/conf")))
}

## Input data to SAM

### Set source directory, which is data folder of last year's assessment
if(sam_data == TRUE){
  source_dir     <- file.path(file.path(getwd(),"Blue whiting", paste0('whb-',assessmentYear-1)),paste0("bw_",assessmentYear-1,"_preliminary_catch/data"))

### Select files to move
files_to_copy  <- c(file.path(source_dir, "cn.dat"),
                    file.path(source_dir, "cw.dat"),
                    file.path(source_dir, "dw.dat"),
                    file.path(source_dir, "lf.dat"),
                    file.path(source_dir, "lw.dat"),
                    file.path(source_dir, "mo.dat"),
                    file.path(source_dir, "nm.dat"),
                    file.path(source_dir, "pf.dat"),
                    file.path(source_dir, "pm.dat"),
                    file.path(source_dir, "survey.dat"),
                    file.path(source_dir, "sw.dat"))

### Copy files
copy_files(file_list = files_to_copy, 
           target_dir = file.path(getwd(),paste0(stock.dir,"/data")))
}

## Source code
if(source_code == TRUE){
  ### Set source directory, which is data folder of last year's assessment
  source_dir     <- file.path(file.path(getwd(),"Blue whiting", paste0('whb-',assessmentYear-1)),paste0("bw_",assessmentYear-1,"_preliminary_catch/src"))
  
  ### Select files to copy (which is all)
  files_to_copy  <- list.files(source_dir, full.names = TRUE, recursive = FALSE)
  
  ### Copy files
  copy_files(file_list = files_to_copy, 
             target_dir = file.path(getwd(),paste0(stock.dir,"/src")))
}