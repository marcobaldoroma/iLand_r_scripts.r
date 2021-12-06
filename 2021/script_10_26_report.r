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
# 1a. here as stands start from STP2, can only happen after some stands went into STP1 in time, so can run some 200 years, and look in the abeStandRemoval activities: which stand has clearcut in the activities  (and not clearcut_sw, that is for shelterwood, stp2). If you know some id-s, can make some plots for those stands, and if the clearcut is not clearing everything just note the stand id, and I will check why.  Collect some 2-3 problematic stands ( If there is none, then good! but I think we have some)
# 1b. Look a bit at the javascript, try to understand where the activities are, how they listed in stp definition, and where is the agent definition. From the scheduling of the activities, you will see when they should happen in the model.  (schedule: { minRel: 0.34, optRel: 0.35, maxRel: 0.36}   here the Rel. mean relative to rotation time, so if rotation is 100years, this activity happens at year 35 optimally but can happen in year 34 and 36 too.)
# 1c. Look at some stands and find which are jumping out some activities, which are having all (planting, tending, thinning1, thinning2, thinning3, (regcut), clearcut).  Activities are in the abeStandRemoval output table.  Note that: regcut and tending not written well to this table. If there is a previous activity in that simulation for that stand, it keeps the name of the previous one, if it is the first activity it will tell “none”.
# 200 years, stp3, no planting trees activity in management # the species richness-diversity seem to be a lot more positivi with natural regeneration
# Check if we can improve in the shelterwood management, to have the regeneration cut more close to reality. And also try to check what is happening at the stand after regeneration cut (here Laura need to find out how to visualize small trees)

# 2. Change the management operations to have more STPs on the landscape that are dynamically change in time (can find out some rules here)

# 3. As of graphs, you can try to combine more variables in one plot, such as species proportions as you have it now, with columns showing harvests on the other Y axis. Check our papers, can find there such.



                                                                        # STP1b = ABIES ALBA > 50% IN VOLUME M3/HA
                                                                        # STP2b = ABIES ALBA < 50% V m3/ha 
                                  


# ____________________________________________________ R script 27/10/2021 __________________________________________________________________________________________
                                  

# install.packages("RSQLite")
library(RSQLite)



file<-"D:/TEST3_folder/output/subregion_medium_test1.sqlite"   # file to read

# second script in TEST3_p2_folder -> abe -> tatra.JS (add planting activities)
# file<-"C:/TEST3_folder_p2/output/subregion_medium_test2.sqlite" 



sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# --- --- --- --- --- --- --- --- --- --- --- --- 3° REPORT iLand model in Czech Republic 27-10-2021 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# needed to work properly with latex

tinytex::install_tinytex()
tinytex::tlmgr_update()

# libraries needed for knitr report

library (knitr)
library (tinytex)         # librerie necessarie


# vedi la funzione stitch e trova altri template da qui. https://rdrr.io/cran/knitr/man/stitch.html
# I hided only warnings #delete the warning, message and codes in the report.
# To hide the warnings use the code here or go in the link to study other functionalities

# knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)

stitch("D:/iLand.r/script_27_10_report.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

######## test3 report pt2

# stitch("C:/iLand.r/script_27_10_report_p2.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))


#________________________________________________________ JAVA Script 26/10/2021 MANAGEMENT ________________________________________________________________________



#____________/*
This management script is doing businness-as-usual activities on the full landscape of our study area in the Low Tatra for iLand model.
Laura Dobor
2017/08/18 - 2021/10/14

We have one agent and one unit. But have 2 stps: one for stand where beech and fir presents (in 0, or 1 layer), they go to stp2 to a shelterwood management (1b), others go to stp1 (1a) small scale clearcut.
This selection was done in preprocess (in R).

Activities that this script is doing:

Planting
Tending
Thinning max 4 times in one rotation period
Regeneration cut: in shelterwood cases
Clearcut

Salvaging: We added there salvaging in order to remove dead trees which produced in the BB module.




 */



//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                 ---       P L A N T I N G  ----
//
//  At the input file there are columns with the fractions how planting should be done: PlantPiab	PlantAbal	PlantPisy	PlantFasy	PlantLade	PlantBrLf
//  Values are summing up to 1.  The last one is BrLf means broadleaf, and distributed into acps (40%), frex(30%) and ulgl(30%). 
// Defines a fraction 0-1 of 2x2m pixels for which to plant trees of the given species. The `fraction` is interpreted as a probability, i.e. for each 2x2m pixel a random number decides whether to plant or not.
//   Planting is in 2nd year of every rotation (schedule: 2 )
//   50 cm high trees are planted wall to wall
//   We are not clearing the existing tree saplings at the site. (clear: false)

var a_planting = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "piab",  height: 0.5, fraction: function() { return  stand.flag("PlantPiab");}, clear: false},
		  {species: "abal",  height: 0.5, fraction: function() { return  stand.flag("PlantAbal");}	},
		  {species: "lade",  height: 0.5, fraction: function() { return  stand.flag("PlantLade");}	},
		  {species: "pisy",  height: 0.5, fraction: function() { return  stand.flag("PlantPisy");}	},
		  {species: "fasy",  height: 0.5, fraction: function() { return  stand.flag("PlantFasy");}	},
                       
		  {species: "acps",  height: 0.5, fraction: function() { return  (4*stand.flag("PlantBrLf")/10)	;}},
		  {species: "frex",  height: 0.5, fraction:  function() { return (3*stand.flag("PlantBrLf")/10);}	},
		  {species: "ulgl",  height: 0.5, fraction:  function() { return (3*stand.flag("PlantBrLf")/10);}	}],
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting piab fraction= " + stand.flag("PlantPiab") );
    			     	fmengine.log("Planting lade fraction= " + stand.flag("PlantLade") );
				fmengine.log("Planting abal fraction= " + stand.flag("PlantAbal") );
			      	fmengine.log("Planting pisy fraction= " + stand.flag("PlantPisy") );
				fmengine.log("Planting fasy fraction= " + stand.flag("PlantFasy") );
				fmengine.log("Planting acps fraction= " + (4*stand.flag("PlantBrLf")/10));
				fmengine.log("Planting frex fraction= " + (3*stand.flag("PlantBrLf")/10));
				fmengine.log("Planting ulgl fraction= " + (3*stand.flag("PlantBrLf")/10));}
};

//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---  T E N D I N G    --->          
// Tending is scriped as a manual activity (there is no builit-in tending module like planting, so this is more detailed here
// 3 activity, because for different SIs tended at different age. 
// For a given stand only that one is activated which is needed. This is set based on input data file, checking which years is written into tending column. To make it active is set in the bottom of this script.
// 
// now the tree selection is based on dbh, going from the smallest.              


////  
var a_tending = { type: "scheduled",     
	                schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21, force: false}, // the min-max window  force true: if there is a stand which is older than this it will cut in the 0 year
                    onEvaluate: function(){ 
                                  trees.loadAll();    // Load all trees from the stand
                                              

	fmengine.log("We need to remove this total volume at tending: "+stand.volume*stand.area*20/100);             		
             
				   trees.sort("dbh");      // Sort trees: small trees at the beginning of the list
				   trees.filter("incsum(volume) < " + (stand.volume*stand.area*20/100)     );      // Select as much trees to have the volume that we need to remove 
				   trees.simulate=false;    // Removes trees next line immediately.
                   trees.harvest();
                   trees.loadAll();
                                   
	fmengine.log("Stand volume after tending: "+stand.volume*stand.area);	 
                                   return true;  },   // end on onevaluate
			onExecute: function() {}
}


//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---  T H I N N I N G S  --->   

// http://iland-model.org/ABE+activity+thinning
// For thinnings there are build in method, and we use it. Stand top height must be > 4m, dbh>5cm


var a_thinning1 = { type: "thinning", 
				   schedule: { minRel: 0.34, optRel: 0.35, maxRel: 0.36 , force=true},
				   constraint: ["stand.topHeight>4"],
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				   targetValue: 35,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [45, 45, 10, 0, 0]
}


var a_thinning2 =  {type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.5, maxRel: 0.51, force=true },
				   constraint: ["stand.topHeight>4"],
                                   thinning: "custom",
				   onEvaluate: function() {console.log("2nd THINNING ");return true;},
			           targetValue: 22,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [45, 45, 10, 0, 0]
}


var a_thinning3 =  {type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.6, maxRel:0.61, force=true },
				   constraint: ["stand.topHeight>4"],
                                   thinning: "custom",
				   onEvaluate: function() {console.log("3nd THINNING ");return true;},
				   targetValue: 17,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [45, 45, 10, 0, 0]
}





///             F R O M   A B O V E    A T   S T P 2     
  
 
var a_thinning3_FA =  {type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.6, maxRel:0.61 , force=true},
				   constraint: ["stand.topHeight>4"],
                                   thinning: "custom",
				   onEvaluate: function() {console.log("3nd THINNING ");return true;},
				   targetValue: 17,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [0, 0, 10, 45, 45]
}



//- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---  R E G E N E R A T I O N    C U T S  --->   


/// This is a harvest before the clearcut, which used the shelterwood stands. the timing relative to the rotation period 75-80% of full rotation
/// The intensity is 40% of volume to remove


 var a_regcut = { type: "scheduled",
					schedule: { minRel: 0.75, optRel: 0.785, maxRel: 0.8, force: true},
					onEvaluate: function() {
						 trees.loadAll(); 
						 
 	fmengine.log("Volume of the stand per ha before REGCUT: "+stand.volume);
 	fmengine.log("Stand area: "+stand.area);
 	fmengine.log("Volume of the stand: "+stand.volume*stand.area);

					var nbefore=trees.count
					var volumetoremove=stand.volume*stand.area*60/100  

	fmengine.log("We need to remove total volume of : "+volumetoremove);
		
                    trees.sort("-dbh")                // Sort trees: thick trees at the beginning of the list
                    trees.filter("incsum(volume) < " + volumetoremove);      // We select the 40% of the volume
				     		 		
				    trees.simulate=false;    // Removes trees next line immediately.
                    trees.harvest();

   fmengine.log("Volume per ha after regcut: "+stand.volume);	
                     trees.loadAll();
                     var nafter=trees.count
                           
	fmengine.log("Number of trees before vs after: "+nbefore+" vs "+nafter);
                     return true; 
					 },   
			onExecute: function() { },
			

};



//- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---   C L E A R C U T S  --->   



/// final harvest for all stands, specific dbh treshold can be set in the options of the stp
//  we use general 10 cm dbh threshold

// clearcut for STP1: we removed all the saplings and clear the whole stand:
var a_clearcut =  { type: "scheduled",
		   schedule: { minRel: 0.95, optRel: 1.0, maxRel: 1.05, force=true },
				onEvaluate: function(){ 
        fmengine.log("finalHarvest:" + activity.finalHarvest);

				         trees.loadAll();

        fmengine.log("---THE U WHAT iLAND USE HERE:" + stand.U);
    
                          return true;  
 					},
					
				  onExecute: function() { 
        fmengine.log("I really do the harvest..."); 

					    trees.removeMarkedTrees(); // but do the same thing as the default operation 
		fmengine.log("removed " + n + " saplings");
                        trees.killSaplings("");
						 },
					onCreate: function() { activity.finalHarvest=true; },
					onSetup: function() {  },
					

};

// clearcut for STP2: we do NOT removethe saplings + we remove only trees that are older than the 50% of the rotation time.

var a_clearcut_sw =  { type: "scheduled",
			 schedule: { minRel: 0.95, optRel: 1.0, maxRel: 1.05, force=true },

			onEvaluate: function(){ 

	fmengine.log("finalHarvest:" + activity.finalHarvest);
					trees.loadAll();

	fmengine.log("---THE U WHAT iLAND USE HERE:" + stand.U);
	fmengine.log("Number of trees before selecting: "+trees.count);

					trees.filter("age>80")

	fmengine.log("clearcut: using age threshold:80 " );
	fmengine.log("Number of trees after selecting: "+trees.count);

					trees.harvest(); 
					return true; 
                                        
 					},
			onExecute: function() { 
         fmengine.log("I really do the harvest..."); 
					trees.removeMarkedTrees(); 	
                 			},
					onCreate: function() {activity.finalHarvest=true;  },
					onSetup: function() {  } 
					

};


//- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---   S A L V A G E R  ---> 
// http://iland-model.org/ABE+activity+salvage
// after disturbances salvager start to work:
// We set in project file what % to remove after disturbance. We remove only trees with dbh>10
// We look how many trees remained after the disurbance, and if the thresholds exceeds the onExecute part gets active. In this case We reset the stand to have rotation start again, and do planting, after the damage, and salvage.
// NOTE: salvager is doing the restart of stands after big disturbance, so even if we do not want to make salvage, needed to set some small value to project file to activate this feature of restarting. e.g 0.1% to remove or sth like this.

var a_salvager = { type: 'salvage',  schedule: { repeat: true },
    		 disturbanceCondition: "dbh>10 and rnd(0,1)<"+Globals.setting('user.salvage.remove'),
    		 onExecute: function() {
		 trees.loadAll();
		  
 fmengine.log("Remained after disturbance: " + trees.count + " normal trees");
                 var n = trees.killSaplings('');
 fmengine.log("Plus remained: " + n + " saplings");
 fmengine.log("We do not do harvest, and clearing, only RESET")

                fmengine.runActivity(stand.id, "planting"); // assuming the name of the activity is "planting"      
 fmengine.log("PLANTING AFTER RESET");

	//stand.trace = true; // enable tracing ...
		
	 },

	 debugSplit: false,
	 thresholdIgnoreDamage: 10, // modified, WR: was 100000  above this threshold clearing and splitting is tested
	 thresholdClearStand: 0.8,  // added WR    if the relative damage is higher than this,the onExecute part is activated, and we do resart of the rotation there.
	 thresholdSplitStand: 1.0, // added WR (no splitting)   if this is smaller than the clearstand, it is splitted if rel.damage higher than this value. WE do not want stands to be splitted and re-numbered.
     




    }			

// now for STP2 it is the same as to stp1
var a_salvager_sw = { type: 'salvage',  schedule: { repeat: true , force:true},
     disturbanceCondition: "dbh>10 and rnd(0,1)<"+Globals.setting('user.salvage.remove'),
     onExecute: function() {
		 
		 trees.loadAll();
		 //trees.harvest("dbh>5");  // ... WE DO NOT DO CLEARING
		  
   fmengine.log("Remained after disturbance: " + trees.count + " normal trees");
                  var n = trees.killSaplings('');
   fmengine.log("Plus remained: " + n + " saplings");
                 //trees.killSaplings("");
   fmengine.log("We do not do harvest, and clearing, one RESET")

                fmengine.runActivity(stand.id, "planting"); // assuming the name of the activity is "planting"      
                fmengine.log("PLANTING AFTER RESET");
		 //stand.trace = true; // enable tracing ...
	
	 },

	 debugSplit: false,
	 thresholdIgnoreDamage: 10, // modified, WR: was 100000  above this threshold clearing and splitting is tested
	 thresholdClearStand: 0.8,  // added WR    if the relative damage is higher than this, it is cleared
	 thresholdSplitStand: 1.0, // added WR (no splitting)   if this is smaller than the clearstand, it is splitted if rel.damage higher than this value

    }	


var stand_monitor= { type: 'general', schedule: { repeat: true, repeatInterval: 5 },
                     action: function() {
                         fmengine.log('stand_monitor active: piab: ' + stand.relSpeciesBasalAreaOf('piab'));
                         if (stand.relSpeciesBasalAreaOf('piab')>0.5) {
                             // or some other condition
                             fmengine.log('switch stand ' + stand.Id + ' to other STP');
                             stand.stp = 'STP1';
                         }
                     }
        }
		
		
/// This show what activites are set for STP1 and STP2		
var stp1 = { U: [100,100,100], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting: a_planting, 
            tending: a_tending,
			
			thinning1: a_thinning1,
			thinning2: a_thinning2,
			thinning3: a_thinning3,

			clearcut: a_clearcut,
			//salvaging: a_salvager,
			stand_monitor1: stand_monitor,
                  onInit: function() {}  
			
};

var stp2 = { U: [120,120,120], // short/normal/long rotation age
            options: {}, // no options
            tending: a_tending,
			
			thinning1: a_thinning1,
			thinning2: a_thinning2,
			thinning3_FA: a_thinning3_FA,
                		
			regcut: a_regcut,
 			clearcut_sw: a_clearcut_sw ,
			//salvaging_sw: a_salvager_sw,
			stand_monitor1: stand_monitor,
    		onInit: function() {}
};

// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp1, 'STP1');
fmengine.addManagement(stp2, 'STP2');


// minimal agent type
var base_agent = {
	scheduler: { 
		 enabled: true,
                 minScheduleHarvest: 1,
		 maxScheduleHarvest: 10,
		 scheduleRebounceDuration: 15,
		 maxHarvestLevel: 1.5, // default: 1.5
		 deviationDecayRate: 0.1,  // default: 0.1
		 useSustainableHarvest: 0,   // 0 is bottom-up harvest planning (i.e., stands are always processed at their optimal dates), and 1 is top-down approach (i.e, the scheduling algorithm decides when a stand should be processed).
		 harvestIntensity: 1}, 

	// a list of all STPs the agent has access to....
	stp: {  'STP1': 'STP1' ,'STP2': 'STP2', 'default':'STP2'	},
	newAgent: function() { return {  scheduler: this.scheduler }; },

        onSelect: function() { if (stand.relSpeciesBasalAreaOf('piab')>0.50) {
			               console.log(stand.relSpeciesBasalAreaOf('piab')) ;
								console.log('we should go back to STP1');
								return 'STP1';
								}; 
							   if (stand.relSpeciesBasalAreaOf('piab')<=0.50) {
								   console.log(stand.relSpeciesBasalAreaOf('piab')) ;
								   console.log('we are in STP2');
							     return 'STP2';
		}},
	run: function() { console.log('base-agent run called');
                         console.log(stand.relSpeciesBasalAreaOf('piab'))  }
};






console.log('adding Agents...');
// register the agent-factory object base_agent under the name agenttype
fmengine.addAgentType(base_agent, 'agenttype');
// use the 'agenttype' agent-factory and create an agent named 'agent'
fmengine.addAgent('agenttype', 'agent');


// Writing out grids:   this is only to write out some grids (maps) of some info...

function onYearEnd()
{
 console.log("GlobalEvent: on year end: " + Globals.year);



}

                                            
                                            
#____________________________________________ JAVA Script 26/10/2021 ____________________________________________________________________________________

                                            
                                            
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
      <climate>MTCLIM_corrected_vpd_1961-2016.sqlite</climate>
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
       <file>abe/tatra_simplified_v0.js</file>
        <agentDataFile>abe/BAU_2021.csv</agentDataFile>
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
      <enabled>true</enabled>
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
      <enabled>true</enabled>
    </management>
    <sapling>
      <enabled>true</enabled>
      <condition/>
    </sapling>
    <saplingdetail>
      <enabled>true</enabled>
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
      <enabled>true</enabled>
    </abeStandRemoval>
    <abeStandDetail>
      <enabled>true</enabled>
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

