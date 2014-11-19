
#####
# PACKAGES
#####
library(xtable)

#####
# DEFINE WORKING DIRECTORY CONDITIONAL TO SYSTEM
# (don't set, so that it can work with Sweave)
#####
if ( Sys.info()["sysname"] == "Linux" ){
  wd <- "/home/joebrew/Documents/haiti/household_representatives"
} else {
  wd <- "C:/Users/BrewJR/Documents/haiti/household_representatives"
}

#####
# READ IN RANDOMLY GENERATED HOUSEHOLD REPS SPREADSHEET
#####
hh <- read.csv(paste0(wd, "/spreadsheet_for_krishna.csv"))

#####
# DEFINE FUNCTION FOR CLEANING UP LABEL NAMES
##### 
TableFun <- function(hh_number, names = TRUE){
  
  # GET DATA AND SPECS
  chars <- nchar(hh_number)
  x <- hh[hh_number,]
  x <- x[,-1]
  
  # LINE 1
  line1 <- paste0("HH number: ", 
                  hh_number, 
                  " ",
                  paste0(rep(" ", (5-chars)), collapse = ""),
                  " ",
                  "HH size: ",
                  paste0(1:7, "|", collapse = ""),
                  "\n")
  
  
  # LINE 2
  line2 <- paste0("Representative: ",
                  paste0(x[1,], "|", collapse = ""))
  line2 <- toString(line2)
  line2 <- gsub(" , ", "", line2)
  line2 <- gsub(":", ": ", line2)
  line2 <- gsub(", ", "|", line2)
  line2 <- paste0("          ", line2, "\n", collapse = "")
  
  complete_lines <- paste0(line1, line2, collapse = "")
  toString(complete_lines)
}



#####
# WRITE TEXT FILE FOR USE AT fileformat.info
#####

# Instructions: http://www.fileformat.info/tool/label/mergeformat.htm
# MAIL MERGE
# http://www.fileformat.info/tool/label/avery5161/mailmerge.htm
sink(paste0(wd, "/print_labels/output.txt"))
for (i in 1:3000){ cat(TableFun(i),"\n")}
sink()


