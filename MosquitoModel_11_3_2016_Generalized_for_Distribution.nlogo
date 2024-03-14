extensions [ gis rnd]

globals[landcover humandensity alifespan elifespan fecundity lakes lakescent vzltraploc avg.daily.dis avg.daily.dis.sd long.dis.max number-emmigrate Hab0prob Hab1prob Hab2prob Hab3prob Hab4prob Hab5prob
         gravidHab0prob gravidHab1prob gravidHab2prob gravidHab3prob gravidHab4prob gravidHab5prob gravidHab10prob    
         Playaprob NonPlayaprob GC1length GCsublength Preblood infectrate longdis.disp.prob Adultdaily-survival
         simlengthminus1 roads ] ; make sure to put things in here, instead of in the "turtles-own" section below if you want 

breed [mosqs mosq]
breed [eggs egg]
breed [wetlands wetland]
patches-own [habitat playa playacent gravidLC popdensity vzltrap]
mosqs-own [mosqs-here Aage egglay-countdown mosqsize emmigrate GC infected filler]
wetlands-own [IDnumber]
eggs-own [Eage] 

; The setup function accomplishes four important tasks, including
; 1. Sets NetLogo world, including setting the directory of datasets, and imports them. 
; 2. It assigns values from the datasets to NetLogo variables
; 3. It sets up landcover values, playa lake id's, and playa center id's in each patch. 
; 4. It sets up the color scheme for the visualization 

to setup   ;this sets up the initial variables
  clear-all

set-current-directory "C:\\Users\\danidaws\\Documents\\Master Documents\\Research\\NetLogo\\Code For Sharing"  ; this sets the directory where the file is

  gis:load-coordinate-system "WGS_84_Geographic.prj"
                              ;"US_Orthographic.prj"
                              ;"Lambert_Conformal_Conic.prj"
  set landcover gis:load-dataset   "lubbock_landcover_wgs1984_ascii.asc" 
 
  set humandensity gis:load-dataset "HD_km2_120_ascii_3.asc"

   set lakes gis:load-dataset "lubbock_nwis_08_07_20ag_prob_raster_ascii.asc"; ; this is a polgon raster of playa lakes    
   set lakescent gis:load-dataset "lubbock_nwis_08_07_20ag_prob_raster_centers_ascii.asc"
  
   set vzltraploc gis:load-dataset "VZL_trap_locations_ascii.asc" 
 
   set roads gis:load-dataset "Lubbock_roads_WGS1984.shp" 

  ;gis:set-world-envelope-ds (gis:envelope-union-of (gis:envelope-of landcover)        ; this sets the envelope of the world to that of the dataset
                                              
  ;                                               (gis:envelope-of lakescent)
   ;                                              (gis:envelope-of lakes)
    ;                                              (gis:envelope-of vzltraploc)
     ;                                              (gis:envelope-of humandensity))
  gis:apply-raster landcover habitat                                          
  gis:apply-raster lakes playa
  gis:apply-raster lakescent playacent
  gis:apply-raster vzltraploc vzltrap
  gis:apply-raster humandensity popdensity
  
  gis:set-drawing-color blue
  gis:draw roads 1
 
 ask patches [ patchcolor] ; this uses the below function called patchcolor to assign the colors based on the habitat type
 
  setup-wetlands
  setup-habitatNA
  setup-playaNA
  setup-gravidLC 

  ;Disease parameters
   
  set Alifespan 100
  set Elifespan 100

   reset-ticks 

end 



to setup-wetlands 
 ask patches [ if playacent > 0  [sprout-wetlands 1]]  
             ask wetlands [ set IDnumber playacent] 
  end

to setup-habitatNA
 ask patches[ ifelse (habitat <= 0) or (habitat >= 0)  ; this sets up an ifelse situation to identify cells that are and aren't part of the landscsape; it sets non habitat to habitat class 5
  [ if habitat = 1 [set habitat 1]
    if habitat = 2 [set habitat 2]
    if habitat = 3 [set habitat 3]
    if habitat = 0 [set habitat 0]
    if habitat = 4 [set habitat 4]]
    [ set habitat 5]]
end 

; This establishes each patch as either as a particular playa or not, if not, then its 999
to setup-playaNA
 ask patches[ ifelse (playa <= 0) or (playa >= 0)   
    [ set playa playa ]
    [ set playa 999 ]
    if playa = 0 [ set playa 999]] 
 
end 

; This sets up each patch as either a breeding wetland or not, and if then 10. This is because not everything classifed as a wetland is a breeding wetland. 
to setup-gravidLC
  ask patches [ ifelse playa = 999 [set gravidLC habitat] [ set gravidLC 10]]
  end

;This sets up the color scheme for the visualization
to patchcolor
  if habitat = 1 [set pcolor green] ; pastures
  if habitat = 2 [set pcolor red]   ; crop agriculture
  if habitat = 3 [set pcolor white]  ; urban development
  if habitat = 0 [set pcolor orange] ; forests
  if habitat = 4 [set pcolor green - 2] ; shrubland
  if habitat = 5 [set pcolor blue] ; wetlands/plaays
   if (playa > 0 and playa < 999)  [set pcolor blue]  ; this turns only playa locations blue
   if vzltrap > 0 [set pcolor black]                 
end



;;;;;;
;;Go Procedures
;;;;;

to go ;this says that for each tick, adult mosquitoes get older, their egglay-counter clock advances, they can become infected(not utilized) for the current model, they move, and they reproduce 
  get-older
  gravid?
  infected? 
  move-mosqs
  reproduce
  tick
end

;This makes it so the mosquito adult mosquito and eggs(i.e. larvae) gets a tick older every day. And once they reach the max lifespan they die. 
to get-older  
  ask mosqs [ set Aage (Aage + 1)
                if Aage > Alifespan [die]]
   ask eggs [ set Eage (Eage + 1)
                if Eage > Elifespan [die]]
      
end

;This advances the egglay-countdown counter that determines which individuals can lay eggs. 
to gravid?
   ask mosqs [ set egglay-countdown (egglay-countdown + 1)]   
 end 
  

; This randomly dtermines whether an individual is infected with a pathogen or not; section under development 
to infected?  ; This sets an "infected" status, that says they start as uninfected, they have to be old enough to take a blood-meal
 ask mosqs [  
     if infected = 0 [            
     if Aage > Preblood [
     if random 100 < infectrate 
     [set infected 1]]]]
end
  
;################                         #################
;################ Mosquito movement section####################
;################                          #####################
; This section governs the movement of the initlal individual, adult survival, and movement. 
; Survival is a probabilistic function determined by a Adult-survival rate set in the R code. 
;The movement section has four main submodels, 2 for each of dispersal and oviposition dispersal behavior. To determine which combination of two to use, specify for both nulliparous(GC=0) and parous(GC>0) individuals below.  

to move-mosqs    
            
 ask mosqs [  
    
   ifelse (filler = 1) [forward 0]
   ;survival function
  [ if random 100 < (100 - Adultdaily-survival) ; this is the adult daily survival function
   [die] 
   
  ; emmigration fuction
   
   if habitat = 5 [set emmigrate (emmigrate + 1)]  
   if emmigrate > 1 [die ]     
   
   ;if habitat = 5 [die]
                                     ;[forward random long.dis.max + random-float 14.7 - random-float 14.7] this is for normal operation 

   if GC = 0 [ ;this is for nulliparous movement
             ; ifelse (egglay-countdown > GC1length) [gravid-disperse] [ifelse random 100 < longdis.disp.prob [forward random long.dis.max] [random-disperse]  ]]   
              ifelse (egglay-countdown > GC1length) [gravid-disperse-nearest][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse] ]]
              ;ifelse (egglay-countdown > GC1length) [gravid-disperse] [ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse]  ]]   
              ;ifelse (egglay-countdown > GC1length) [gravid-disperse-nearest][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [random-disperse] ]]
              ;ifelse (egglay-countdown > GC1length) [gravid-disperse] [ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse]  ]] 
   if GC > 0 [; this is for parous movement
              ifelse (egglay-countdown > GCsublength) [gravid-disperse-nearest][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse] ]]
               ;ifelse (egglay-countdown > GCsublength) [gravid-disperse][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse] ]]
              ;ifelse (egglay-countdown > GCsublength) [gravid-disperse-nearest][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [random-disperse] ]]
            ; ifelse (egglay-countdown > GCsublength) [gravid-disperse][ ifelse random 100 <= longdis.disp.prob [forward random long.dis.max + random-float 14.7 - random-float 14.7] [landcover-disperse] ]]


      ]]

   end
   
; This submodel directs gravid females (females surpassing their egglay-countdown threshold) move to the nearest wetland     
 to gravid-disperse-nearest      
 
    
    let nearest-wetland min-one-of patches with [gravidlc = 10] [distance myself] 
    let xsgc [pxcor] of nearest-wetland
    let ysgc [pycor] of nearest-wetland
    facexy xsgc ysgc
    let distrg ([distance myself] of nearest-wetland)
    ifelse distrg >= avg.daily.dis [ forward avg.daily.dis] [ forward distrg] 
     end
 
 ; This submodel directs gravid females to move according to landcover probability values.    
   to gravid-disperse      ; this works by looking at all the cells within a radius of them, assigning probabilities to them, and then moving to one if it is a wetland(with a very low probability of not) 
          let dist.movegc avg.daily.dis  + random-float 2.68 - random-float 2.68
          let nearbygc patches in-radius avg.daily.dis 
          let dist ([distance myself] of nearbygc) ;this makes a list of the distances to cells 
          let xsgc [pxcor] of nearbygc
          let ysgc [pycor] of nearbygc
          
   
           let habgc [gravidLC] of nearbygc ; this takes the gravidLC values of the nearby 
         
            ; the below assigns probabilities of each grid cell based on the probabilities above; essentially its  string of ifelse statements; its important to note that this was accomplished using a series of hard brackets, with the "else" part being a new ifelse statement inside a new set of hard brackets. 
           ; the probabilities ares assigned in the R code. 
           let probgc (map [ifelse-value (? = 0) [gravidHab0prob] [ifelse-value (? = 1) [gravidHab1prob] [ifelse-value (? = 2) [gravidHab2prob] [ifelse-value (? = 3) [gravidHab3prob] [ifelse-value (? = 4) [gravidHab4prob] [ifelse-value (? = 5) [gravidHab4prob] [ gravidHab10prob]]]]]]] habgc ) 

           let sumprobgc sum probgc ; this sums the overall probabilities of the available grid cells
           let scaledprobgc (map [? / sumprobgc] probgc) ; this scales each probability to the overall 
           let pairsgc (map list xsgc ysgc) ; in order to get dist to the cell, this is done in three steps with the map function accompanied by list to create sublists
           let pairs2gc ( map list pairsgc dist) 
           let pairs3gc (map list pairs2gc scaledprobgc) ; this last sublist still only has two items; item 1 is the xycoords and dist, and item two is the probability
                    
           let disp.cellgc item 0 rnd:weighted-one-of pairs3gc [ item 1 ? ] ; this says to take a weighted random draw on the list called "pairs2gc", using the item 1(the second item in the list) as the weighting factor, then use the first item(item 0) as the value. Item 0 or pairs is the ID number, while item 1 is the scaled probabilities
           
           ;This last section sets the coordinates towards the chosen heading 
           let coordsgc item 0 disp.cellgc
           let distgc item 1 disp.cellgc
           let xcoordgc item 0 coordsgc
           let ycoordgc item 1 coordsgc
        
           facexy xcoordgc ycoordgc  ;this tells the turtle to turn toward the coordinates above, 
           set heading heading   
           
           forward distgc ;

           end

;;;Dispersal Movement;

;This directs females to disperse randomly 
 to random-disperse
   let dist.move_ran avg.daily.dis + random-float 2.68 - random-float 2.68
   set heading random 360
   forward dist.move_ran
 
 end


;This directs females to disperse according to landcover values   
   to landcover-disperse
   
   let dist.move avg.daily.dis + random-float 2.68 - random-float 2.68
   let roundup ceiling dist.move
  
   let thispatch patch-here   ; this begins the landcover influence on movement 
   
   ; because this is a cellular approximation of radius, this seems to result in mostly predictable numbers of cells selected expanding from the center; 
   ;it say to select cells the distance of the roundedup dispersal distance, within +-0.5   
   let nearby patches with [
     distance thispatch > roundup - 0.5  and 
     distance thispatch < roundup + 0.5]   
                             
   let xs [pxcor] of nearby
   let ys [pycor] of nearby
  
   let hab [habitat] of nearby ; this takes the habitat values of the nearby 
        
  ; the below assigns probabilities of each grid cell based on the probabilities above; essentially its  string of ifelse statements; its important to note that this was accomplished using a series of hard brackets, with the "else" part being a new ifelse statement inside a new set of hard brackets. 
  let prob (map [ifelse-value (? = 0) [Hab0prob] [ifelse-value (? = 1) [Hab1prob] [ifelse-value (? = 2) [Hab2prob] [ifelse-value (? = 3) [Hab3prob] [ifelse-value (? = 4) [Hab4prob] [Hab5prob]]]]]] hab ) ; 
 
   let sumprob sum prob ; this sums the overall probabilities of the available grid cells
   let scaledprob (map [? / sumprob] prob) ; this scales each probability to the overall 
   let pairs (map list xs ys) 
   let pairs2 ( map list pairs scaledprob)
    
   let disp.cell item 0 rnd:weighted-one-of pairs2 [ item 1 ? ] ; this says to take a weighted random draw on the list called "pairs2", using the item 1(the second item in the list) as the weighting factor, then use the first item(item 0) as the value. Item 0 or pairs is the ID number, while item 1 is the scaled probabilities
      
   let xcoord item 0 disp.cell
   let ycoord item 1 disp.cell
   facexy xcoord ycoord  ;this tells the turtle to turn toward the coordinates above, 
   set heading heading + random 6 - random 6 ; this tells the turtles the set the heading its going, plus add an up to 5 degree wobble  
   forward dist.move
    
   end
        

;This governs reproductive behavior. There is a section for both nulliparous and parous behavior. 
to reproduce   

ask mosqs [ ; This section sets it so that they eggs at different times, depending on their GC number. GC1 has a longer length because of the no blood-feeding stage. 
  if playa != 999 and Habitat != 5 [
       
       if GC = 0 [  ifelse (egglay-countdown > GC1length) [ hatch-eggs ((fecundity + random 29 - random 29) * 0.5 ) ; this lays eggs around a mean within  +- 1SD.   
                                                           set egglay-countdown 0 ; reset egglay countdown
                                                           set GC GC + 1] ; makes subsequent GC's have a shorter GC threshold. 
                                                          [set heading random 360 ; after laying eggs, females fly off. 
                                                           forward avg.daily.dis]]
                                                            
       if GC > 0 [  ifelse (egglay-countdown > GCsublength) [ hatch-eggs ( (fecundity + random 29 - random 29) * 0.5) set egglay-countdown 0]
                                                         
                                                          [set heading random 360
                                                          forward avg.daily.dis]]]
  
  ]
        

 
end




 




  
@#$#@#$#@
GRAPHICS-WINDOW
291
41
1215
838
-1
-1
2.0
1
10
1
1
1
0
0
0
1
0
456
0
382
1
1
1
ticks
30.0

BUTTON
21
101
84
134
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
19
156
87
189
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
1261
74
1370
119
Number of Adults
count mosqs
17
1
11

PLOT
1364
113
1564
263
plot 1
Ticks
Number Adult Mosquitoes
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Adults" 1.0 0 -5298144 true "" "plot count mosqs"

MONITOR
1237
273
1338
318
Number of Eggs
count eggs
17
1
11

@#$#@#$#@
#Spatially Explicit Population Model for *Culex tarsalis* in Lubbock County, TX



This is the NetLogo part of a hybrid IBM-Matrix model, with matrix models run out of program R using the R package RNetLogo. For full details about the parameterization of this model, please see the PhD dissertation by Daniel Dawson (2016).  

In order to run this model properly, please find the RScripts(or RMarkDown scripts) called "SetupScript_MosquitoModel_RNetlogo" and "MultipleRunScript_MosquitoModel_RNetlogo". These two R scripts control the operation of the model, including all major variable parameterizations, and should be used to run model simulations. 

In order to run the current model, the user must install the NetLogo extensions "rnd" and "GIS". The former allows for random draws(which is used in mosquito movement) and the latter allows for the of importation landscape datasets. 


##Model Overview##

In this model, population dynamics of C. tarsalis mosquitoes are modeled using a combination of an Individual-based model for adult dynamics, and matrix models for aquatic phase behavior. The matrix models for the aquatic phase are associated with wetlands, with a seperate matrix model being associated with wetland. The reasoning for this model design is that the life history characterstics of the aquatic stage are likely to be very similar within wetlands, whereas the life history characteristics of the adult stage are likely to be different for each adult, depending on where on the landscape the adult is located. In addition, the spatial distribution of adults and aquatic phase individuals in real landscapes is heterogeneous due to the heterogeneous distribution of breeding wetlands. Lastly, because mosquito control is applied in specific locations on the landscape, the effects of treatment are likely to be very localized, leading to further spatial heterogeneity of landscape-level population dynamics. To reflect this, the design allows the user to adjust life history characteristics of the aquatic phase based on the individual wetland(allowing simulation of wetland-level mosquito control(i.e. larviciding). It also allows for landcover influenced movement, reflecting the idea that adults may move more readily through some habitats than others. It does not currently allow for other landcover influences on adult life hisory, including adult mortality, and therefore does not allow for the influence of adulticiding. 

###Current Model World
  The model was designed for the landscape of Lubbock County, Texas. To do so, it uses the GIS extension for Netlogo to import in several raster datasets to set patch values. To set landcover values, it uses a classified Landsat landcover dataset (Global variable "landcover") that was converted to and ASCII format and does not currently have a projection associated with it. The model also includes wetlands from a USFWS Wetland Investory dataset. These wetlands are imported into the NetLogo landscape as a raster file that was created in ArGIS 10.2 from a polygon dataset (global variable "lakes"). The centers of the wetlands were also imported into the landscape as a seperate raster, and was created from a point dataset(global variable "lakescent"). In this two-wetland dataset system, the model used the polygon-based raster allowed mosquitoes to lay eggs anywhere in a wetlands once arriving there, and the wetland center dataset to provide a location for adult mosquitoes to emerge from. In order to estimate risk of mosquito contact, a dataset of human density is imported into the model (global variable "humandensity"). This dataset was created by converting a polygon based dataset from the US Census bureau into a raster. Lastly, survey locations were imported into the landscape to provide locations(as a raster, convered from point locations) for "seed" mosquitoes to emerge from for the purpose of model evaluation(global variable "vzltraploc"). This variable is only during model evaluation phases. 

To make sure everything aligns properly, the envelope for the imported datasets is set to that of the landcover dataset using the gis:set-world-envelope-ds command. To associate the values of the rasters to the patches, the values for the global variables landcover, lakes, lakescent, vzltraploc, and humandensity are assigned to the patches-own variables habitat, playa, playacent, vzltrap, popdensity, respectively.  

Lastly, in order to import rasters into the model, they must be located in a directory that is specified in the NetLogo model using the "set-current-directory" function. Note the "//" format for the file director. 

###Model Setup
Prior to running the model, the model sets up the patches on the landscape. This involves setting the values of three main aspects, including landcover values, breeding wetlands, and cells outside the margins of the landscape.

In the case of landcover values, it uses the submodel **setup-habitatNA** to assign values to each value in the ASCII file, including NA values(those outside the landscape) which it assigns a value of 5. 


To setup playa wetlands, the **setup-playaNA** submodel assigns the ID value of the playa wetland in the ASCII dataset. It deals with NA's by assigning non-playa locations as 999. 

To set up playa wetland centers, the **setup-wetlands** submodel creates a turtle called "wetlands" for each playacent patch, setting a value called IDnumber to the playacent ID, which associated with ASCII file. The id number of the wetland and the playa number of the patch should align. 

The **setup-gravidLC** submodel provides a target to move toward for finding breeding habitat by creating a patch variable called gravidlc. This variable paralleled the habitat value, except that it assigned values of 10 to where breeding playas existed.  

Lastly, the **patchcolor** submodel controlled colors for patches. In the current model, green = crop agriculture, red = pasture, white = urban, orange = forest, light-green = shublands, and blue equal wetlands. Traplocations(vzltraps) were set to black.   

###Model Run Procedures

Model run procedures included every within the go submodel, and included all adult mosquito behaviors. Adult behavior includes aging (submodel **get-older**), the determination of whether a mosquito was gravid or not (submodel **gravid?**), mosquito movement(submodel **move-mosqs**), and egg-laying (submodel **reproduce**). There are also rudimentary functions for simulating the spatial distribution of mosquito infection with a pathogen (submodel **infected?**) this isn't specifically utilized in this model. After each submodel is run, the model advances a tick, and runs until the user specified length in the accompanying R scripts are completed.

Blood-seeking behavior and mating is not explicitly modeled, but is instead assumed to occur between the end of one gonotrophic cycle and the begining of a new one. Mosquitoes are assumed to mate after emergence, and always immediately find a blood meal after laying a clutch of eggs. 

###Mosquito behaviors

##### to get-older
Mosquitoes age 1 day every day, with a maximum lifespan allowed. Practically mosquitoes in the simulation rarely live much past 1 gonotrophic cycle, but this is there as a backstop. In addition, the code has a reporter that asks for all adult mosquitoes at each time step that throws an error if no adults exist. Therefore, an initial adult mosquito is added to the start of the model. By giving it an age that is just short of the maximum life span, and preventing it from moving(see to move-mosqs) this adult mosquito dies quickly after and doesn't breed.     

##### to gravid? 

Each day, females also increase their egglay-countdown clocks 1 day. This is a threshold that decides when they can lay eggs.


#####to move-mosqs
In this submodel, mosquitoes that have aged and who egglay-counter clocks have advanced are allowed to live or die, and to move. 

First, the simple probabilistic survival function determines whether a given adult lives or dies in the day. Also, an emmigration function specifies that adults that move outside the world die.  
   
Next, submodels governing mosquito movement are implemented. Mosquitoes move in two main ways, including dispersal behavior(which includes finding a bloodmeal) and in search of breeding wetlands(oviposition behavior) in which to oviposit. They engage in dispersal behavior until they pass the threshold of their egglay-countdown, at which they oviposition dispersal behavior. This model explores two general hypotheses about how mosquitoes make both dispersal and oviposition movements. For dispersal movements, these random movement, and movement based on landcover,in which the direction in which mosquitoes move is dependent upon the landover. For the random movement, mosquitoes choose a random direction. For landcover-based movement, mosquitoes assses the cells around them at a certain radius and choose a cell based on a probabilistic function determined by its landcover type. For ovipoisition behavior, the two hypotheses explored including landcover-based movement and movement toward the nearest suitable wetland. The landcover-based oviposition movement function uses the same probabilities as the corresponding dispersal movement function, except that the mosquito always chooses a wetland if it happens to be within its movement radius. In the nearest wetland function, the mosquito always moves toward the nearest wetland. In both dispersal and oviposition movement, distance moved is based on an average movement, plus or minus a random amount. Because C. taralis is known to disperse long distances, mosquitoes can also move a long distance with a certain probability.  

To utilize the each of the four movement options, the user must specify which submodel combination, including **gravid-disperse**, **gravid-disperse-nearest**, **landcover-disperse**, and **random-disperse** to utilize for both first gonotrophic cycle (GC = 0) individuals (non-parous), and subsequent GC individuals (GC > 0).      


#####to reproduce
In this submodel, mosquitoes that arrive on breeding wetlands are allowed to lay eggs. Mosquitoes only lay eggs in wetlands if their egglay-countdown clocks exceed the appropriate thresholds for their first or subsequent GC's (GC1length, GCsublength). After eggs are laid, the mosquitoes reset their egglay-countdown clocks, they advance their GC counter 1, and they move off again in a random directly. The number of eggs laid is 1/2 (to account for sex ratio) a randomly fluctuates around a experimentally derived average value. In the R portion of the code, the eggs are then brought into R-based matrix models, and the eggs in the Netlogo model die. When new adults are produced in the R matrix models, they emerge from the same wetland.    
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

lake
true
0
Polygon -7500403 true true 60 45 120 15 165 15 210 30 240 60 255 90 270 135 270 165 270 210 240 255 225 270 180 255 165 270 135 270 120 255 90 255 75 255 75 225 60 210 45 180 30 165 30 120 45 75 45 60

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mosquito
true
0
Circle -7500403 true true 88 88 94
Polygon -7500403 true true 120 120 135 90 120 45 75 30 45 45 45 75 75 105
Polygon -7500403 true true 150 120 135 90 150 45 195 30 225 45 225 75 195 105
Circle -7500403 true true 105 150 30
Polygon -7500403 true true 150 180 120 180 135 270

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
