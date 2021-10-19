# Create the first analysis report. I create the first plots and graphs using the stands of part of the CZ case study for a 75 year Time series in iLand forest model. I select 7 different stands and after a period of 75 years. I compared the wood volume dynamics in this different stand. I also compared the volume at the DBH in one stand, and a superficial boxplot statistical and logarithm analysis for one "RU".

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
spvol <- filter(stand, ru == "1")
ggplot(spvol, aes(year,volume_m3, fill=species))+
  geom_area()

spvol2 <- filter(stand, ru == "9")
ggplot(spvol2, aes(year,volume_m3, fill=species))+
  geom_area()

spvol2 <- filter(stand, ru == "56")
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

stitch("D:/iLand.r/report_13_10/script_13.10_report.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))  


#############§ç###@#################################### JAVA SCRIPT FOR THE ILAND MODEL TEST 1 §§§§§§#######@###@#ç########



<project>
  <system>
    <path>
      <home></home>
      <database>database</database>
      <lip>lip</lip>
      <temp>temp</temp>
      <script>scripts</script>
      <init>init</init>
      <output>output</output>
    </path>
    <database>
       <in>species_param_europe_allometry_20190521_SK.sqlite</in>
      <out>subregion_medium_test1.sqlite</out>
      <climate>C:/Users/baldo/OneDrive/Desktop/TEST_folder/database/MTCLIM_corrected_vpd_1961-2016.sqlite</climate>
    </database>
    <logging>
      <logTarget>file</logTarget>
      <logFile>log/test1.txt</logFile>
      <flush>false</flush>
    </logging>
    <settings>
      <multithreading>true</multithreading>
      <debugOutput>0</debugOutput>
      <debugOutputAutoSave>true</debugOutputAutoSave>
      <randomSeed>0</randomSeed>
      <expressionLinearizationEnabled>true</expressionLinearizationEnabled>
      <logLevel>Warning</logLevel>
    </settings>
    <javascript>
      <fileName/>
    </javascript>
  </system>
  <model>
    <settings>
      <regenerationEnabled>true</regenerationEnabled>
      <mortalityEnabled>true</mortalityEnabled>
      <growthEnabled>true</growthEnabled>
      <carbonCycleEnabled>true</carbonCycleEnabled>
      <epsilon>2.7</epsilon>
      <lightExtinctionCoefficient>0.6</lightExtinctionCoefficient>
      <lightExtinctionCoefficientOpacity>0.6</lightExtinctionCoefficientOpacity>
      <temperatureTau>6</temperatureTau>
      <airDensity>1.204</airDensity>
      <laiThresholdForClosedStands>3</laiThresholdForClosedStands>
      <boundaryLayerConductance>0.2</boundaryLayerConductance>
      <interceptionStorageNeedle>4</interceptionStorageNeedle>
      <interceptionStorageBroadleaf>2</interceptionStorageBroadleaf>
      <snowMeltTemperature>0</snowMeltTemperature>
      <waterUseSoilSaturation>false</waterUseSoilSaturation>
      <groundVegetationLAI>1</groundVegetationLAI>
      <groundVegetationPsiMin>-1.5</groundVegetationPsiMin>
      <usePARFractionBelowGroundAllocation>true</usePARFractionBelowGroundAllocation>
      <seedDispersal>
        <seedBelt>
          <enabled>false</enabled>
          <width>25</width>
          <sizeX>9</sizeX>
          <sizeY>10</sizeY>
          <species_1_9>pisy 0.2 bepe 0.07 abal 0.01 alin 0.08 potr 0.02 lade 0.03 piab 0.59</species_1_9>
          <species_2_9>fasy 0.02 pisy 0.19 bepe 0.04 soau 0.02 abal 0.02 potr 0.02 lade 0.02 piab 0.67</species_2_9>
          <species_3_9>pisy 0.64 potr 0.01 lade 0.01 piab 0.33 saca 0.01</species_3_9>
          <species_4_9>pisy 0.54 abal 0.01 acps 0.01 alin 0.04 tico 0.01 potr 0.01 lade 0.01 piab 0.35 saca 0.02</species_4_9>

        </seedBelt>
        <dumpSeedMapsEnabled>false</dumpSeedMapsEnabled>
        <dumpSeedMapsPath/>
        <externalSeedEnabled>false</externalSeedEnabled>
        <externalSeedSource/>
        <externalSeedSpecies>abal, piab, lade, pisy, fasy, quro, acps, frex, cabe, bepe, alin, qupe, algl, casa, pini, acca, acpl, qupu, pice, soau, soar, coav, alvi, potr, poni, tico, tipl, ulgl, saca</externalSeedSpecies>
        <externalSeedBuffer/>
        <externalSeedBackgroundInput>abal 0.0002 piab 0.000 lade 0.000175 pisy 0.0001 fasy 0.00035 quro 0.00005 acps 0.000125 frex 0.000075 cabe 0.00005 bepe 0.0001 qupe 0.000075 algl 0.00005 pini 0.00005 acca 0.00005 acpl 0.00005 qupu 0.000075 pice 0.000005 soau 0.0001 soar 0.00005 alvi 0.00005 tico 0.00005 tipl 0.00005 ulgl 0.00005 saca 0.00005</externalSeedBackgroundInput>
        <recruitmentDimensionVariation>0.1</recruitmentDimensionVariation>
        <longDistanceDispersal>
          <rings>5</rings>
          <thresholdArea>0.0001</thresholdArea>
          <thresholdLDD>0.0000001</thresholdLDD>
          <LDDSeedlings>0.5</LDDSeedlings>
        </longDistanceDispersal>
      </seedDispersal>
      <soil>
        <qb>5</qb>
        <qh>14.5</qh>
        <leaching>0.47</leaching>
        <el>0.152</el>
        <er>0.319</er>
        <swdDBHClass12>20</swdDBHClass12>
        <swdDBHClass23>100</swdDBHClass23>
        <useDynamicAvailableNitrogen>false</useDynamicAvailableNitrogen>
        <nitrogenDeposition>21.8</nitrogenDeposition>
      </soil>
      <grass>
        <enabled>false</enabled>
        <type>pixel</type>
        <grassDuration>polygon(x, 0,0, 6,0, 6,1, 30,1, 30,0)</grassDuration>
        <LIFThreshold>0.2</LIFThreshold>
        <grassPotential>polygon(0.9999*x^0.15)</grassPotential>
        <maxTimeLag>3</maxTimeLag>
        <grassEffect>polygon(0.9999*x^0.15)</grassEffect>
      </grass>
      <browsing>
        <enabled>false</enabled>
        <browsingPressure>1</browsingPressure>
      </browsing>
    </settings>
    <species>
      <source>species</source>
      <reader>readerstamp.bin</reader>
      <nitrogenResponseClasses>
        <class_1_a>-0.045</class_1_a>
        <class_1_b>10</class_1_b>
        <class_2_a>-0.055</class_2_a>
        <class_2_b>25</class_2_b>
        <class_3_a>-0.065</class_3_a>
        <class_3_b>40</class_3_b>
      </nitrogenResponseClasses>
      <CO2Response>
        <p0>1</p0>
        <baseConcentration>380</baseConcentration>
        <compensationPoint>80</compensationPoint>
        <beta0>0.3</beta0>
      </CO2Response>
      <lightResponse>
        <shadeTolerant>min(10*lri,max(0.1613*lri+0.7871,lri))</shadeTolerant>
        <shadeIntolerant>1-exp(-5.5*(lri-0.05))</shadeIntolerant>
        <LRImodifier>exp(ln(lri)/0.5*(1-0.5*relH))</LRImodifier>
      </lightResponse>
      <phenology>
<type id='1'>
          <vpdMin>0.9</vpdMin>
          <vpdMax>4.1</vpdMax>
          <dayLengthMin>10</dayLengthMin>
          <dayLengthMax>11</dayLengthMax>
          <tempMin>-2</tempMin>
          <tempMax>5</tempMax>
 
</type>
<type id='2'>
          <vpdMin>1</vpdMin>
          <vpdMax>4.1</vpdMax>
          <dayLengthMin>10</dayLengthMin>
          <dayLengthMax>11</dayLengthMax>
          <tempMin>-4</tempMin>
          <tempMax>3</tempMax>
 
</type>
      </phenology>
    </species>
    <world>
      <cellSize>2</cellSize>
      <width>18000</width>
      <height>10000</height>
      <buffer>200</buffer>
      <latitude>47</latitude>
      <resourceUnitsAsGrid>true</resourceUnitsAsGrid>
      <environmentEnabled>true</environmentEnabled>
      <environmentMode>grid</environmentMode>
      <environmentGrid>gis/environment_grid_medium.asc</environmentGrid>
      <environmentFile>gis/environment_NP_file_20190905.txt</environmentFile>
      <areaMask>
        <enabled>false</enabled>
        <imageFile>AFJZ_mask.png</imageFile>
      </areaMask>
      <timeEventsEnabled>false</timeEventsEnabled>
      <timeEventsFile></timeEventsFile>
      <location>
        <x>-347020.025100000028</x>
        <y>-1204487.113799999934 </y>
        <z>0</z>
        <rotation>0</rotation>
      </location>
      <standGrid>
        <enabled>true</enabled>
        <fileName>gis/medium_id_stands_1996.asc</fileName>
      </standGrid>
      <DEM/>
    </world>
    <site>
      <availableNitrogen>84</availableNitrogen>
      <soilDepth>38</soilDepth>
      <pctSand>9</pctSand>
      <pctSilt>53</pctSilt>
      <pctClay>38</pctClay>
      <youngLabileC>16360</youngLabileC>
      <youngLabileN>652.1</youngLabileN>
      <youngLabileDecompRate>0.4527519</youngLabileDecompRate>
      <youngLabileAbovegroundFraction>0.35</youngLabileAbovegroundFraction>
      <youngRefractoryC>46214</youngRefractoryC>
      <youngRefractoryN>121.2</youngRefractoryN>
      <youngRefractoryDecompRate>0.2093879</youngRefractoryDecompRate>
      <youngRefractoryAbovegroundFraction>0.15</youngRefractoryAbovegroundFraction>
      <somC>182000</somC>
      <somN>83.68</somN>
      <somDecompRate>0.02415841</somDecompRate>
      <soilHumificationRate>0.25</soilHumificationRate>
    </site>
    <climate>
      <co2concentration>340</co2concentration>
      <tableName>climateE9_S4_A4</tableName>
      <batchYears>40</batchYears>
      <temperatureShift>0</temperatureShift>
      <precipitationShift>1</precipitationShift>
      <randomSamplingEnabled>true</randomSamplingEnabled>
      <randomSamplingList></randomSamplingList>
      <filter/>
    </climate>
    <initialization>
      <mode>snapshot</mode>
      <type>iland</type>
      <randomFunction>max(1-x^2,0)</randomFunction>
      <file>after600y_spinup_snapshot.sqlite</file>
      <saplingFile></saplingFile>
      <snags>
        <swdC>12000</swdC>
        <swdCN>417.9</swdCN>
        <swdCount>50</swdCount>
        <otherC>4000</otherC>
        <otherCN>22.95</otherCN>
        <otherAbovegroundFraction>0.3</otherAbovegroundFraction>
        <swdDecompRate>0.036</swdDecompRate>
        <woodDecompRate>0.071</woodDecompRate>
        <swdHalfLife>13.926</swdHalfLife>
      </snags>
      <heightGrid>
        <enabled>false</enabled>
        <fileName>lidar_np.txt</fileName>
        <maxTries>10</maxTries>
        <fitFormula>polygon(x, 0,0, 0.8,1, 1.12, 1, 1.5,0)</fitFormula>
      </heightGrid>
    </initialization>
    <management>
      <enabled>true</enabled>
      <file></file>
      <abeEnabled>true</abeEnabled>
      <abe>
       <file>abe/tatra_v9_killsaplings_h.js</file>
        <agentDataFile>abe/BAU_2020_2.csv</agentDataFile>
      </abe>
    </management>
    <parameter>
      <torus>false</torus>
      <debug_tree>0</debug_tree>
      <debug_clear>false</debug_clear>
      <gpp_per_year>0</gpp_per_year>
      <debugDumpStamps>false</debugDumpStamps>
    </parameter>
  </model>
  <output>
    <dynamic>
      <enabled>false</enabled>
      <columns> dbh.mean, dbh.max, dbh.min, dbh.p5, dbh.p25, dbh.p75, dbh.p95, height.mean, height.max, height.min, height.p5,height.p95, height.p25,height.p75, stress.mean, stress.max, if(stress&gt;0,1,0).sum, if(stress&gt;0,stress,0).sum, if(dbh&gt;0,1,0).sum, leafarea.sum,woodymass.sum,rootmass.sum,foliagemass.sum
      </columns>
    </dynamic>
    <tree>
      <enabled>false</enabled>
      <filter/>
    </tree>
    <treeremoved>
      <enabled>false</enabled>
      <filter/>
    </treeremoved>
    <stand>
      <enabled>true</enabled>
      <condition/>
      <by_ru>true</by_ru>
    </stand>
    <standdead>
      <enabled>false</enabled>
    </standdead>
    <production_month>
      <enabled>false</enabled>
    </production_month>
    <management>
      <enabled>false</enabled>
    </management>
    <sapling>
      <enabled>false</enabled>
      <condition/>
    </sapling>
    <saplingdetail>
      <enabled>false</enabled>
      <condition/>
      <minDbh/>
    </saplingdetail>
    <carbon>
      <enabled>true</enabled>
      <condition/>
      <conditionRU>1=0</conditionRU>
    </carbon>
    <carbonflow>
      <enabled>true</enabled>
      <condition/>
      <conditionRU>1=0</conditionRU>
    </carbonflow>
    <soilinput>
	<enabled>true</enabled>
   </soilinput>
    <water>
      <enabled>false</enabled>
      <condition/>
      <conditionRU>in(year, 5,10,15,20,25,30,35)</conditionRU>
    </water>
    <landscape>
      <enabled>true</enabled>
      <condition/>
    </landscape>
    <dynamicstand>
      <enabled>false</enabled>
      <condition/>
      <rufilter/>
      <comment/>
      <treefilter/>
      <by_species/>
      <by_ru/>
      <columns>
if(dbh&gt;=20 and dbh&lt;40,volume,0).sum, if(dbh&gt;=40 and dbh&lt;60,volume,0).sum, if(dbh&gt;=60,1,0).sum,
if(dbh&lt;5,basalarea,0).sum, if(dbh&gt;=5 and dbh&lt;10,basalarea,0).sum, if(dbh&gt;=10 and dbh&lt;15,basalarea,0).sum, if(dbh&gt;=15 and dbh&lt;20,basalarea,0).sum, if(dbh&gt;=20 and dbh&lt;25,basalarea,0).sum, if(dbh&gt;=25 and dbh&lt;30,basalarea,0).sum, if(dbh&gt;=30 and dbh&lt;35,basalarea,0).sum, if(dbh&gt;=35 and dbh&lt;40,basalarea,0).sum, if(dbh&gt;=40 and dbh&lt;45,basalarea,0).sum, if(dbh&gt;=45 and dbh&lt;50,basalarea,0).sum, if(dbh&gt;=50 and dbh&lt;55,basalarea,0).sum, if(dbh&gt;=55 and dbh&lt;60,basalarea,0).sum, if(dbh&gt;=60 and dbh&lt;65,basalarea,0).sum, if(dbh&gt;=65 and dbh&lt;70,basalarea,0).sum, if(dbh&gt;=70 and dbh&lt;75,basalarea,0).sum, if(dbh&gt;=75 and dbh&lt;80,basalarea,0).sum, if(dbh&gt;=80,basalarea,0).sum,
if(height&lt;4,basalarea,0).sum, if(height&gt;=4 and height&lt;6,basalarea,0).sum, if(height&gt;=6 and height&lt;8,basalarea,0).sum, if(height&gt;=8 and height&lt;10,basalarea,0).sum, if(height&gt;=10 and height&lt;12,basalarea,0).sum, if(height&gt;=12 and height&lt;14,basalarea,0).sum, if(height&gt;=14 and height&lt;16,basalarea,0).sum, if(height&gt;=16 and height&lt;18,basalarea,0).sum, if(height&gt;=18 and height&lt;20,basalarea,0).sum, if(height&gt;=20 and height&lt;22,basalarea,0).sum, if(height&gt;=22 and height&lt;24,basalarea,0).sum, if(height&gt;=24 and height&lt;26,basalarea,0).sum, if(height&gt;=26 and height&lt;28,basalarea,0).sum, if(height&gt;=28 and height&lt;30,basalarea,0).sum, if(height&gt;=30 and height&lt;32,basalarea,0).sum, if(height&gt;=32 and height&lt;34,basalarea,0).sum, if(height&gt;=34 and height&lt;36,basalarea,0).sum, if(height&gt;=36,basalarea,0).sum,
basalarea.sum, dbh.mean, dbh.sd, dbh.p5, dbh.p25, dbh.p75, dbh.p95,
height.mean, height.sd, height.p5, height.p25, height.p75, height.p95,age.mean,age.sd,age.min,age.max
</columns>
    </dynamicstand>
    <barkbeetle>
      <enabled>false</enabled>
    </barkbeetle>
    <wind>
      <enabled>false</enabled>
    </wind>
    <fire>
      <enabled>false</enabled>
    </fire>
    <landscape_removed>
      <enabled>false</enabled>
      <includeHarvest>true</includeHarvest>
      <includeNatural>true</includeNatural>
    </landscape_removed>
    <abeStand>
      <enabled>true</enabled>
    </abeStand>
    <abeUnit>
      <enabled>true</enabled>
    </abeUnit>
    <abeStandRemoval>
      <enabled>false</enabled>
    </abeStandRemoval>
    <abeStandDetail>
      <enabled>false</enabled>
    </abeStandDetail>
  </output>
  <modules>
    <fire>
      <enabled>false</enabled>
      <onlySimulation>true</onlySimulation>
      <KBDIref>0.3</KBDIref>
      <rFireSuppression>1</rFireSuppression>
      <rLand>1</rLand>
      <meanAnnualPrecipitation>5000</meanAnnualPrecipitation>
      <averageFireSize>9650000</averageFireSize>
      <fireSizeSigma>1.633</fireSizeSigma>
      <fireReturnInterval>10</fireReturnInterval>
      <fireExtinctionProbability>0.05</fireExtinctionProbability>
      <fuelKFC1>0.75</fuelKFC1>
      <fuelKFC2>0.75</fuelKFC2>
      <fuelKFC3>0.75</fuelKFC3>
      <crownKill1>0.21111</crownKill1>
      <crownKill2>0.00445</crownKill2>
      <crownKillDbh>40</crownKillDbh>
      <burnSOMFraction>0.02</burnSOMFraction>
      <burnFoliageFraction>0.9</burnFoliageFraction>
      <burnBranchFraction>0.51</burnBranchFraction>
      <burnStemFraction>0.11</burnStemFraction>
      <wind>
        <speedMin>10</speedMin>
        <speedMax>20</speedMax>
        <direction>270</direction>
      </wind>
    </fire>
    <wind>
      <enabled>false</enabled>
      <speciesParameter>wind</speciesParameter>
      <soilFreezeMode>auto</soilFreezeMode>
      <triggeredByTimeEvent>true</triggeredByTimeEvent>
      <durationPerIteration>2</durationPerIteration>
      <gustModifier>0.2</gustModifier>
      <topoModifier>1</topoModifier>
      <directionVariation>30</directionVariation>
      <direction>0</direction>
      <dayOfYear>200</dayOfYear>
      <speed>0</speed>
      <duration>100</duration>
      <topoGridFile>gis/svf32_plot_RESCALED2.asc</topoGridFile>
      <factorEdge>3</factorEdge>
      <edgeDetectionThreshold>10</edgeDetectionThreshold>
      <topexModifierType>multiplicative</topexModifierType>
      <LRITransferFunction>max(min(3.733-6.467*LRI, 3.41),3)</LRITransferFunction>
      <edgeProbability>polygon(x,0,0,20,0.5)</edgeProbability>
      <edgeAgeBaseValue>20</edgeAgeBaseValue>
      <edgeBackgroundProbability>0.1</edgeBackgroundProbability>
      <onAfterWind/>
    </wind>
    <barkbeetle>
      <enabled>false</enabled>
      <minimumDbh>15</minimumDbh>
      <backgroundInfestationProbability>0.000685</backgroundInfestationProbability>
      <stormInfestationProbability>0.8</stormInfestationProbability>
      <baseWinterMortality>0.4</baseWinterMortality>
      <winterMortalityFormula>1-exp(-0.1005*x)</winterMortalityFormula>
      <spreadKernelFormula>exp(-((x/4.5)^2)/4/40.5)</spreadKernelFormula>
      <spreadKernelMaxDistance>257</spreadKernelMaxDistance>
      <cohortsPerGeneration>20</cohortsPerGeneration>
      <cohortsPerSisterbrood>30</cohortsPerSisterbrood>
      <colonizeProbabilityFormula>0.85*x+0.15</colonizeProbabilityFormula>
      <deadTreeSelectivity>0.9</deadTreeSelectivity>
      <outbreakClimateSensitivityFormula>Psummer^-0.9609</outbreakClimateSensitivityFormula>
      <outbreakDurationMin>10</outbreakDurationMin>
      <outbreakDurationMax>12</outbreakDurationMax>
      <outbreakDurationMortalityFormula>polygon(t, 0.5,0, 1,1)</outbreakDurationMortalityFormula>
      <initialInfestationProbability>0.000685</initialInfestationProbability>
      <referenceClimate>
        <tableName>climateE7_S4_A4</tableName>
        <seasonalPrecipSum>215, 366.13, 194.54, 100.81</seasonalPrecipSum>
        <seasonalTemperatureAverage>5.49, 14.48, 6.07, -4.39</seasonalTemperatureAverage>
      </referenceClimate>
      <onAfterBarkbeetle/>
    </barkbeetle>
  </modules>
  <user>
    <windspeed_factor>1</windspeed_factor>
    <code>value</code>
  </user>
</project>
