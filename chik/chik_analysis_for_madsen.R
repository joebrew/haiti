######
# Attach packages
#######
library(gdata)
library(dplyr)
library(maptools)
library(rgdal)

######
# Set working directory to the haiti directories on local machine
######
if(Sys.info()["sysname"] == "Windows"){
  public <- 'C:/Users/BrewJR/Documents/haiti/chik'
  private <- 'C:/Users/BrewJR/Documents/private_data/haiti/chik'
} else {
  public <- '/home/joebrew/Documents/haiti/chick'
  private <- '/home/joebrew/Documents/private_data/haiti/chik'
}
setwd(private)

######
# Read in the 2 datasets sent by Madsen
######
df1 <- read.xls("Clean_Copy of Chikun-V.xlsx",
                stringsAsFactors = FALSE)
df2 <- read.xls("Copy of CHIKV_DATA.xls",
                stringsAsFactors = FALSE)

######
# Clean up column names 
######

# lower case all names
names(df1) <- tolower(names(df1)) 
names(df2) <- tolower(names(df2)) 

# replace periods with underscores
names(df1) <- gsub("[.]", "_", names(df1))
names(df2) <- gsub("[.]", "_", names(df2))

# clear trailing underscores
names(df1) <- gsub("_(?=_*$)", " ", names(df1), perl=TRUE)
names(df2) <- gsub("_(?=_*$)", " ", names(df2), perl=TRUE)

# clear leading/trailing spaces
names(df1) <- gsub("^\\s+|\\s+$", "", names(df1))
names(df2) <- gsub("^\\s+|\\s+$", "", names(df2))

# clear repeat underscores
names(df1) <- gsub("___|__", "_", names(df1))
names(df2) <- gsub("___|__", "_", names(df2))

######
# Fill missings with NAs
######

# Define function for missing
Missing <- function(var){
  nchar(as.character(var)) == 0
}

# Set to NA anything that is missing
for (j in 1:ncol(df1) ){
  x <- df1[,j]
  x[which(Missing(x))] <- NA
  df1[,j] <- x
}

for (j in 1:ncol(df2) ){
  x <- df2[,j]
  x[which(Missing(x))] <- NA
  df2[,j] <- x
}

######
# Format dates
######

# df1
df1$date <- as.Date(df1$date, format = "%Y-%d-%m")
# fill the NAs with the most previous date
for (i in 1:nrow(df1)){
  if(is.na(df1$date[i])){
    correct_dates <- df1$date[1:i] #all the prior dates
    correct_dates <- correct_dates[!is.na(correct_dates)] # remove the NA's
    correct_date <- correct_dates[length(correct_dates)] # take last (most recent)
  } else{
    correct_date <- df1$date[i]
  }
  df1$date[i] <- correct_date
}

df2$date_collected <- as.Date(df2$date_collected, format = "%d-%b-%y")
df2$extraction_date <- as.Date(df2$extraction_date, format = "%d-%b-%y")
df2$pcr_date <- as.Date(df2$pcr_date, format = "%d-%b-%y")

######
# Clean up temperature column
######
df1$temperature <- as.numeric(gsub("°C|\\s", "", df1$temperature))

######
# Clean up localisation column
######
df1$localisation <- gsub("D°|[:]|[.]", "", df1$localisation)
df1$localisation <- gsub("^\\s+|\\s+$", "", df1$localisation)
df1$localisation[which(Missing(df1$localisation))] <- NA

######
# Clean up associatd_symp column
######
df1$associatd_symp <- gsub("D°|D °|°|[:]|[.]", "", df1$associatd_symp)
df1$associatd_symp <- gsub("^\\s+|\\s+$", "", df1$associatd_symp)
df1$associatd_symp[which(Missing(df1$associatd_symp))] <- NA

######
# Merge by child code
######
df2$child_code <- df2$child_code_no
df2$child_code_no <- NULL

df <- merge(x = df1,
            y = df2,
            by = "child_code",
            all.x = TRUE,
            all.y = FALSE)

####################################################################
# DATA CLEANING DONE
####################################################################

# Madsen's instructions: do an analysis on sex, age, grade, temperature
# age by chikv_rst
# sex by chikv_rst

######
# SEX
######
table(df$sexe)
mybp <- barplot(table(df$sexe),
                ylim = c(0, max(table(df$sexe))*1.2),
                col = adjustcolor(c("darkblue", "darkgreen"),
                                  alpha.f = 0.4),
                border = "grey")
text(x = mybp[,1],
     y = table(df$sexe),
     pos = 1,
     labels = paste0(100* round(prop.table(table(df$sexe)), digits = 4)," %"),
     cex = 1.5)
text(x = mybp[,1],
     y = table(df$sexe),
     pos = 3,
     labels = paste0(table(df$sexe), " élèves"),
     cex = 0.75)
box("plot")
title(main = "Distribution des observations par sexe")
title(sub = "Distribution of observations by sex")

######
# AGE
######
my_colors <- adjustcolor(colorRampPalette(c("darkblue", "white", "darkgreen"))(20), alpha.f = 0.6)
hist(df$age, breaks = 20,
     col = my_colors,
     xlab = "Age (ans)",
     ylab = "Fréquence",
     main = "Distribution de l'age des élèves")
title(sub = "Distribution of observations by age")
barplot(table(df$age))