library(RCurl)

source_https <- function(u, unlink.tmp.certs = FALSE) {
  #http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

# source gadm function
source_https("https://raw.githubusercontent.com/joebrew/maps/master/gadm_function.R")

# source choro function
source_https("https://raw.githubusercontent.com/joebrew/misc/master/functions/functions.R")

gadm("haiti", 1)
haiti1$NAME_1

# Manually compile malaria dataframe
# from 
# http://www.mspp.gouv.ht/site/downloads/Rapport%20Statistique%20MSPP%202013.pdf
malaria <- data.frame("departement" = c("L'Artibonite",
                                        "Centre",
                                        "Grand'Anse",
                                        "Nippes",
                                        "Nord", 
                                        "Nord-Est",
                                        "Nord-Ouest",
                                        "Ouest",
                                        "Sud",
                                        "Sud-Est"),
                      "malaria" = c(921,
                                    132,
                                    500,
                                    426,
                                    661,
                                    419,
                                    167,
                                    1201,
                                    354,
                                    202),
                      "population" = c(1299398, # got this from the below shapefile
                                       581505,
                                       626928,
                                       NA,
                                       823043,
                                       309918,
                                       531198,
                                       3096967,
                                       621651,
                                       484675))

# Merge malaria data to haiti1
library(dplyr)
haiti1$departement <- haiti1$NAME_1
haiti1@data <- left_join(x = haiti1@data,
                         y = malaria,
                         by = "departement")

# Plot
par(mfrow = c(1,2))
par(mar = c(1,3,1,1))
haiti_boundary <- collapse_map(haiti1)
choro(shape = haiti1,
      boundary = haiti_boundary,
      var = haiti1$malaria,
      legend_round = -2,
      legend_pos = "topleft")
title(main = "Cas de paludisme par département",
      cex.main = 0.75)

choro(shape = haiti1,
      boundary = haiti_boundary,
      var = haiti1$malaria / haiti1$population * 1000,
      legend_round = 1,
      legend_pos = "topleft")
title(main = "Cas de paludisme par département (par 1000 résidents)",
      cex.main = 0.75)
title(sub="Cartes préparées par Joe Brew",
      cex.sub = 0.6,
      line = -5)

# Get haitian population density map
library(rgdal)
pop <- readOGR("/home/joebrew/Documents/haiti/malaria/map", "Haiti_ADM3_stats")
# Get population density by department
by(pop@data,
   pop@data$DEPARTEMEN,
   function(x) sum(x["POPULATION"], na.rm = T))
# add population density to malaria data

