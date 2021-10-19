# install.packages("RSQLite")
library(RSQLite)



file<-"D:/TEST_folder/output/subregion_medium_test1.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    (here can read in by table names.... depending on what you have in your outputfile)

carbon <- dbReadTable(db1,"carbon")
# wind <- dbReadTable(db1,"wind")
# barkbeetle <- dbReadTable(db1,"barkbeetle")
# lremoved <- dbReadTable(db1,"landscape_removed")
landscape <- dbReadTable(db1,"landscape")
abeStand <- dbReadTable(db1, "abeStand")
stand <- dbReadTable(db1, "stand")

dbDisconnect(db1)    # close the file

# Make a plot with ggplot, volume, colored by species....

library(ggplot2)
ggplot(landscape, aes(year,volume_m3, fill=species))+
  geom_area()

# ggplot(abeStand, aes(year,volume, fill=standid))+   
#   geom_line()

library(dplyr)

# subsetting: https://dplyr.tidyverse.org/reference/filter.html


x <- filter(abeStand, standid == "2021")
stid1 <-ggplot(x, aes(year,volume)) + geom_area() + ggtitle("Stand ID 2021 (area 4.35ha)")
stid1 + theme(plot.title = element_text(hjust = 0.5))

ggplot(x, aes(dbh, volume)) + 
  ggtitle("Stand ID 2021/ correlation dbh - volume(m3)") +
  geom_line(colour = "blue")


x1 <- filter(abeStand, standid == "414")
stid2 <- ggplot(x1, aes(year,volume)) + geom_area() + ggtitle("Stand ID 414 (area 2.36 ha)")
stid2 + theme(plot.title = element_text(hjust = 0.5))


x2 <- filter(abeStand, standid == "274")
stid3 <- ggplot(x2, aes(year, volume)) + geom_area() + ggtitle("Stand ID 274 (area 0.98 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))

# Title of the graphics : http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
# +ggtitle("theme_tufte()") 
# GGPlot2 essentials r
# Machine Learning essentials r
# Practical Guide for Cluster Analysis essential r (unsupervised machine learning)


x3 <- filter(abeStand, standid == "846")
stid3 <- ggplot(x3, aes(year, volume)) + geom_area() + ggtitle("Stand ID 846 (area 17.29 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))


x4 <- filter(abeStand, standid == "1173")
stid3 <- ggplot(x4, aes(year, volume)) + geom_area() + ggtitle("Stand ID 1173 (area 1.55 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))


x5 <- filter(abeStand, standid == "2020")
stid3 <- ggplot(x5, aes(year, volume)) + geom_area() + ggtitle("Stand ID 2020 (area 1.49 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))

x6 <- filter(abeStand, standid == "70")
stid3 <- ggplot(x6, aes(year, volume)) + geom_area() + ggtitle("Stand ID 70 (area 6.5 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))

# volume-year x stand colour by species
spvol <- filter(stand, rid == "8411")
ggplot(spvol, aes(year,volume_m3, fill=species))+
  geom_area()

spvol2 <- filter(stand, rid == "19809")
ggplot(spvol2, aes(year,volume_m3, fill=species))+
  geom_area()

spvol2 <- filter(stand, rid == "9086")
ggplot(spvol2, aes(year,volume_m3, fill=species))+
  geom_area()

summary(spvol$volume_m3)

boxplot(spvol$volume_m3)

z <- log10(spvol$volume_m3)

boxplot(z)

volume_year <- hist(spvol$volume_m3) 
summary (volume_year)



___________________________________________________________ Knitr Report _____________________________________________________________________________

# needed to work properly with latex

tinytex::install_tinytex()
tinytex::tlmgr_update()

# libraries needed for knitr report

library (knitr)
library (tinytex)         # librerie necessarie


# vedi la funzione stitch e trova altri template da qui. https://rdrr.io/cran/knitr/man/stitch.html
# I hided only warnings #delete the warning, message and codes in the report.
# To hide the warnings use the code here or go in the link to study other functionalities

knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)

stitch("D:/iLand.r/script_13.10_report.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))  
