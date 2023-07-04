#------------------------------------------------------
# LOAD PACKAGES
#------------------------------------------------------
library(dplyr)
library(reshape2)
library(scales)
library(stringr)
library(purrr)

#------------------------------------------------------
# IMPORT DATA
#------------------------------------------------------

data.path <- "Model output/runs to analyze/"

file.list <- list.files(data.path, recursive=TRUE)

filePaths <- list.files(data.path, "\\.csv$", full.names = TRUE, recursive=TRUE)
str(filePaths)
head(filePaths)

file.key <- as.data.frame(filePaths)
head(file.key)


# unlist data from one rowxcolumn cell
regruns.split.names <- strsplit(as.character(file.key$filePaths), "/")
regruns.split.names <- as.data.frame(do.call(rbind, regruns.split.names))
head(regruns.split.names)
names(regruns.split.names)


# for col 6 which has full names, split it out then split the parts (CHECK sometimes this is a different column number)
regruns.split.names.tr <- regruns.split.names[,6]
regruns.split.names.tr <- strsplit(as.character(regruns.split.names.tr), "_") #unlist data from one rowxcolumn cell
regruns.split.names.tr <- as.data.frame(do.call(rbind, regruns.split.names.tr))
head(regruns.split.names.tr)
str(regruns.split.names.tr)

# add informative column names
colnames(regruns.split.names.tr) <- c("param", "model", "setting", "N", "run")
head(regruns.split.names.tr)

# remove last 4 chars of run string (".csv")
regruns.split.names.tr$run <- str_sub(regruns.split.names.tr$run, end=-5)
head(regruns.split.names.tr)

# add full name
regruns.split.names.tr$long.name <- with(regruns.split.names.tr, paste(param, model, setting, N, run, sep="."))

# bind back to full file path
head(regruns.split.names)
head(file.key)

all.file.names <- cbind.data.frame(file.key, regruns.split.names.tr)

# check
head(all.file.names)
names(all.file.names)
length(all.file.names$filePaths)

unique(all.file.names$setting)
#unique(all.file.names$long.name)

##############################
# subset regular runs for each model type
##############################

# distinct
model.setting <- all.file.names %>%
  dplyr::distinct(model, setting, .keep_all = FALSE)


model.setting$run.status <- "to run" #update as pieces are run separately, outside loop
model.setting$run.status[1] <- "RAN" #update as pieces are run separately, outside loop
model.setting$run.status[2] <- "RAN" #update as pieces are run separately, outside loop

model.setting

model.setting.torun <- subset(model.setting, run.status=="to run")


# LOOP to compile all data by model type and setting
##############################

for (mXs in 1:length(model.setting.torun$model)) {#mXs=1
  loop.model <- as.character(model.setting.torun$model[mXs])
  loop.setting <- as.character(model.setting.torun$setting[mXs])
  mXs.data <- subset(all.file.names, model==loop.model & setting==loop.setting) #head(mXs.data) unique(mXs.data$long.name) 
  
  # create an empty list that will serve as a container to receive the incoming files
  list.data<-list()
  
  # create a loop to read in all the data files ending in .csv
  for (i in 1:length(mXs.data$filePath)) 
  {
    #print(i)
    list.data[[i]]<-read.csv(as.character(mXs.data$filePath[i]), header=FALSE)
  }
  
  # add the names of each data set to the list
  names(list.data)<-mXs.data$long.name
  #names(list.data) #check
  
  # format blank compiled dataframe to bind all row data to
  compiled.runs <- setNames(data.frame(matrix(ncol = 103, nrow = 0)),c("file", "s", "a", paste0("g", sprintf("%03d", c(1:100)))))
  
  start.time <- Sys.time()
  
  for (dataf in (1:length(list.data))) {
    #data <- list.data[d]
    file.name <- names(list.data[dataf])
    
    #show progress
    #print(dataf)
    prog <- (dataf/length(list.data))*100
    print(prog)
    #print(file.name) #to show progress
    
    #check data is in correct format, SKIP LOOP and go to next list.data if <10
    check <- lengths(list.data[dataf], use.names=FALSE)
    if(check<10) next
    
    for (c in (1:10)) {
      #curr.c <- c
      #cc <- data[,c]
      cc <- as.character(list.data[[dataf]][,c]) #call first column, all rows
      
      for (cell in (1:length(cc))) {
        cell.split <- unlist(strsplit(cc[cell], ",")) #unlist data for each cell (by row (row,col))
        no.curls <- t(gsub("[{}]", "", cell.split)) #delete curly brackets and save as "row"
        num.data <- as.data.frame(t(as.numeric(no.curls)))
        row.data <- cbind(file.name, num.data) #add file name
        colnames(row.data) <- c("file", "s", "a", paste0("g", sprintf("%03d", c(1:100))))
        compiled.runs <- rbind(compiled.runs, row.data) # and bind new row to main df
      }
    }
  }
  
  #find total run time
  end.time <- Sys.time()
  end.time-start.time
  
  #check
  head(compiled.runs)
  str(compiled.runs)
  
  #write data for each model X setting combo
  library(tidyverse)
  path_out <- 'Rawdata_compiled/'
  
  write.csv(compiled.runs, paste0(path_out, "compiledruns.", loop.model, ".", loop.setting, ".csv"))    
  
}

