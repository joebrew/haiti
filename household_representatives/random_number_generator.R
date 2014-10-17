# Function to generate one random number between 1 and household size (input)
RandomFun <- function(hh_size = 7){
  
  myvec <- vector(length = hh_size, mode = "numeric")
  for (i in 1:7){
    possibs <- 1:i
    
    myvec[i] <-
      sample(x = possibs,
             size = 1,
             replace = FALSE)
  }

return(myvec)

# Travis' suggestion:
# sapply(1:hh_size, function(x) sample(x, size=1, replace=FALSE))

}

# Create dataframe 
mydf <- data.frame("hh_number" = 1:3000)

# Generate columns for each possible household size
for (i in 1:7){
  mydf[,paste0("hh_size_", i)] <- NA
}

# Populate dataframe with random sequence for each house
for (i in 1:nrow(mydf)){
    mydf[i,2:8] <- RandomFun()
}

# Validate to make sure we got a good draw 
# (ie, no draws larger than household size, and equal distributions)

# First, define function for calculating confidence interval on proportion:
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

# Now calculate confidence interval on each proportion, and visualize
library(Hmisc)
par(mfrow = c(3,3))
par(mar = c(4,4,2,1))
par(oma = c(0,0,0,0))
for (i in 1:7){
  
  mytable <- table(mydf[,paste0("hh_size_", i)])
  ptable <- prop.table(mytable)
  mycis <- simpasym(n = 3000,
                    p = ptable,
                    z = 1.96,
                    cc = TRUE)
  
  bp <- barplot(table(mydf[,paste0("hh_size_",i)])  / 3000,
          main = paste0("Household size = ", i),
          xlab = "HH member selected",
          ylab = "Likelihood of being selected",
          ylim = c(0, max(mycis$ub)),
          cex.lab = 0.6)
  errbar(x = bp[,1],
         y = ptable,
         yplus = mycis$ub,
         yminus = mycis$lb,
         add = TRUE,
         pch = NA,
         errbar.col = adjustcolor("darkred", alpha.f = 0.5)
         )
}
par(mfrow = c(1,1))

# Write to spreadsheet
setwd("C:/Users/BrewJR/Documents/haiti/household_representatives")
row.names(mydf) <- NULL
write.csv(mydf, "spreadsheet_for_krishna.csv", row.names = FALSE)

# Function to generate individual table
TableFun <- function(hh_number){
  x <- mydf[hh_number,]
  names(x) <- c("Household number",
                paste0("Household size: ",
                       1: 7))
  row.names(x) <- "Person to be interviewed"
  return(x)
}

# Function to write individual table to a spreadsheet
WriteTable <- function(hh_number, out_file = NULL){
  
  x <- mydf[hh_number,]
  names(x) <- c("Household number",
                paste0("Household size: ",
                       1: 7))
  row.names(x) <- "Person to be interviewed"
  
  if(is.null(out_file)){
    write.csv(x, paste0("hh_number_", hh_number, ".csv"))
  } else {
    write.csv(x, out_file)
  }
}

# Example of how to write an individual table to a csv
WriteTable(hh_number = 123)
WriteTable(hh_number = 113)
WriteTable(hh_number = 508)

# If you want an individual csv for EVERY household, run the following
setwd("spreadsheets")
for (i in 1:3000){
  WriteTable(hh_number = i)
}
