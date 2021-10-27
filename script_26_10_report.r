                                                 ###   3° SCRIPT REPORT ON FOREST MODELLING DATA ANALYSIS  ###


# iLand plans for Marco

# Quick links:
# project file: http://iland-model.org/project+file
# ABE: http://iland-model.org/ABE
# ABE outputs: http://iland-model.org/ABE+outputs
# More on ABE scripts:  http://iland-model.org/apidoc/modules/ABE.html

#Check if the management operations that we set, really working inside the model

#Tools: use abeStand (volume, species, age), abeStandRemoval (activites) outputs and look 

# Look if the stands with STP1 management are having a clearcut that really clears everything (volume, basal area, and also age drops to 1 after):
# 1. here as stands start from STP2, can only happen after some stands went into STP1 in time, so can run some 200 years, and look in the abeStandRemoval activities: which stand has clearcut in the activities  (and not clearcut_sw, that is for shelterwood, stp2). If you know some id-s, can make some plots for those stands, and if the clearcut is not clearing everything just note the stand id, and I will check why.  Collect some 2-3 problematic stands ( If there is none, then good! but I think we have some)
# 2. Look a bit at the javascript, try to understand where the activities are, how they listed in stp definition, and where is the agent definition. From the scheduling of the activities, you will see when they should happen in the model.  (schedule: { minRel: 0.34, optRel: 0.35, maxRel: 0.36}   here the Rel. mean relative to rotation time, so if rotation is 100years, this activity happens at year 35 optimally but can happen in year 34 and 36 too.)
# 3. Look at some stands and find which are jumping out some activities, which are having all (planting, tending, thinning1, thinning2, thinning3, (regcut), clearcut).  Activities are in the abeStandRemoval output table.  Note that: regcut and tending not written well to this table. If there is a previous activity in that simulation for that stand, it keeps the name of the previous one, if it is the first activity it will tell “none”.
# 200 years, stp3, no planting trees activity in management # the species richness-diversity seem to be a lot more positivi with natural regeneration


                                                                        # STP1b = ABIES ALBA > 50% IN VOLUME M3/HA
                                                                        # STP2b = ABIES ALBA < 50% V m3/ha 
# first jvscrip
                                  

                             #### first report for iLand #######
                                  

# install.packages("RSQLite")
library(RSQLite)



file<-"D:/TEST_folder_2/output/subregion_medium_test1.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

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
 landscape <- ggplot(landscape, aes(year,volume_m3, fill=species))+
  geom_area() + ggtitle("Cz Landscape scale wood volume m3/ha")
landscape+ theme(plot.title =element_text(hjust = 0.5))

# ggplot(abeStand, aes(year,volume, fill=standid))+   
#   geom_line()

library(dplyr)

# subsetting: https://dplyr.tidyverse.org/reference/filter.html


x <- filter(abeStand, standid == "2021")
stid1 <-ggplot(x, aes(year,volume)) + 
geom_area() + ggtitle("CZ Stand ID 2021 (Initial Volume 0.0 m3, area 4.5 ha)")
stid1 + theme(plot.title = element_text(hjust = 0.5))

ggplot(x, aes(dbh, volume)) + 
  ggtitle("Stand RU 2742/ correlation dbh - volume(m3)") +
  geom_line(colour = "blue")


x1 <- filter(abeStand, standid == "414")
stid2 <- ggplot(x1, aes(year,volume)) + 
geom_area() + ggtitle("CZ Stand ID 414 (initial volume 1411 m3, area 17 ha)")
stid2 + theme(plot.title = element_text(hjust = 0.5))


x2 <- filter(abeStand, standid== "2032")
stid3 <- ggplot(x2, aes(year, volume)) + 
geom_area() + ggtitle("CZ Stand ID 2032 (initial volume 359 m3, area 10 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))

# Title of the graphics : # http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
# +ggtitle("theme_tufte()") 
# GGPlot2 essentials r
# Machine Learning essentials r
# Practical Guide for Cluster Analysis essential r (unsupervised machine learning)


x3 <- filter(abeStand, standid == "428")
stid3 <- ggplot(x3, aes(year, volume)) + 
  geom_area() + ggtitle("CZ Stand ID 428 (initial volume 1868 m3, area 0.7)")
stid3 + theme(plot.title =element_text(hjust = 0.5))


x4 <- filter(abeStand, standid == "417")
stid3 <- ggplot(x4, aes(year, volume)) + 
  geom_area() + ggtitle("CZ Stand ID 417 (initial volume 3.6, area 9 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))


x5 <- filter(abeStand, standid == "68")
stid3 <- ggplot(x5, aes(year, volume)) + 
  geom_area() + ggtitle("Cz Stand ID 68 (int. volume 207.12 m3, 3.34 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))

x6 <- filter(abeStand, standid == "749")
stid3 <- ggplot(x6, aes(year, volume)) + 
  geom_area() + ggtitle("Cz Stand ID 68 (int. volume 30 m3, 0.97 ha)")
stid3 + theme(plot.title =element_text(hjust = 0.5))


# volume-year x stand colour by species
spvol <- filter(stand, ru == "2742")
stid_2742<- ggplot(spvol, aes(year,volume_m3, fill=species)) + 
  geom_area() + ggtitle("Cz Stand ID 2742 (STP1 -> STP2)")
stid_2742+ theme(plot.title =element_text(hjust = 0.5))


spvol2 <- filter(stand, ru == "247")
stid247 <- ggplot(spvol2, aes(year,volume_m3, fill=species))+
  geom_area() + ggtitle("Cz Stand ID 247 (STP2)")
stid247 + theme(plot.title =element_text(hjust = 0.5))


spvol3 <- filter(stand, ru == "2224")
stid2224 <- ggplot(spvol3, aes(year,volume_m3, fill=species)) +
  geom_area() + ggtitle("Cz Stand ID 2224 (STP1)")
stid2224 + theme(plot.title =element_text(hjust = 0.5))


spvol4 <- filter(stand, ru == "2219")
stid4 <- ggplot(spvol4, aes(year,volume_m3, fill=species)) +
  geom_area() + ggtitle("Cz Stand ID 2219 (STP2)")
stid4 + theme(plot.title =element_text(hjust = 0.5))


spvol5 <- filter(stand, ru == "1589")
stid5 <- ggplot(spvol5, aes(year,volume_m3, fill=species))+
  geom_area() + ggtitle("Cz Stand ID 1589 (STP2 -> STP1)")
stid5 + theme(plot.title =element_text(hjust = 0.5))
