

##parallel processing script 



---
  title: "MultipleRunScript_MosquitoModel_RNetLogo"
author: "Dan Dawson"
date: "October 31, 2016"
output: html_document
---
  
  This is second script of the Spatially Explicit Poulation Model for Culex tarsalis in Lubbock County, Texas. Paste the output from the first script in the indicated spots. Then, load the function. 

MosqMod<-function ( VarMat, NumberMosquitoes=1, NoSimulations=100) { 
  
  ############################################################################  
  nl.path<-"C:/Program Files (x86)/NetLogo 5.2.0"  #NetLogo file path on this computer
  NLStart(nl.path, gui=F)  #Starting NetLogo from the filepate. Note, for multiple runs, turn off the the GUI for faster computation 
  NLLoadModel("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/MosquitoModel_5_9_2016_LCDisperse_NearestWLdisperse_fullrun_imagery_8_7_14_popdensity.nlogo")
  
  #Set up reporting lists###
  
  mosrasterlist<-list()
  mosrasterlist1<-list()
  
  NumberMosqs<-c(NumberMosquitoes,rep(0,simulation-1))
 
  #parameterset<-NULL
  #parameterlist<-c("AdultSurvival", "GC1Length", "GCsubLength", "fecundity", "avg.daily.dis", "long.dis.max", "longdis.disp.prob", "LarvaSurvival1", "TimeToEmerge1", "Temp", "EggDailySurvival")
  #for ( i in 1:NoSimulations){
   # parameterset[i]<-list(c(rep(0, length(parameterlist))))}
  
  ##START THE MAIN SIMULATION
  
  #for (j in 1:NoSimulations) {
    
    #This asks all the mosquitoes from the previous run to die and resets the ticks for the new run 
    
    NLCommand("setup")
    #NLCommand("reset-ticks")
    
    #This creates a mosquito at the beginning of simulations that doesn't reproduce but that is present in order for the set not to be empty for #reporter purposes.   
    createmosqcommand<-paste("create-mosqs", NumberMosquitoes, "[setxy 200 200]", "ask mosqs [set size 7 set color yellow set Aage (100 - ",VarMat[1,2],") set filler 1]")
    
    NLCommand(createmosqcommand)
    
    
    ###Variable Parameters#####
    
    #Variable categories: 
    #random versus landcover based dispersal
    #nearest wetland versus random encounter gravid behavior
    
    #I'm going to need to set up a vector for each parameter for the thing to referennce. 
    
    #####Parameters to set ahead of time
    
    
    parametercommand<-paste("set Adultdaily-survival", VarMat[1],
                            " set GC1Length", VarMat[2],
                            "set GCsubLength",VarMat[3],
                            "set fecundity", VarMat[4],
                            "set avg.daily.dis", VarMat[5],
                            "set long.dis.max", VarMat[6],
                            "set longdis.disp.prob", VarMat[7],  
                            "set simlengthminus1", simulation-1,
                            
                          
                            "set Hab0prob", WetlandProb,   
                            "set Hab1prob", ShrublandProb,
                            "set Hab2prob", GrasslandProb,
                            "set Hab3prob", DevelopedProb,
                            "set Hab4prob", CroplandProb,
                            "set Hab5prob", NAProb, 
                            "set gravidHab0prob", gravidWetlandProb,
                            "set gravidHab1prob", gravidShrublandProb,
                            "set gravidHab2prob", gravidGrasslandProb,
                            "set gravidHab3prob", gravidDevelopedProb,
                            "set gravidHab4prob", gravidCroplandProb,
                            "set gravidHab5prob", gravidNaProb,
                            "set gravidHab10prob", gravidOvipositWetlandsProb)  
    
    NLCommand (parametercommand)
    
    
    ###########################
    ##PASTE HERE Output from "MosquitoModel_with_dispersal_survival_development_submodels" here
    #################################################################################################
    #NLCommand("setup")
    LarvaSurvival1=LSurRatevec * TreatmentMat1
    
    TimeToPupate1=LDevRatevec
    
    PupaSurvival1=PSurRatevec
    
    TimeToEmerge1=PDevRatevec
    
    EggDailySurvival1=EggDailySurvival
    
    larv.surv1=LarvaSurvival1^(1/TimeToPupate1) 
    
    larv.dr1=1/TimeToPupate1 
    
    pup.surv1=PupaSurvival1^(1/TimeToEmerge1) 
    
    pup.dr1=1/TimeToEmerge1 
    
    egg.surv=EggDailySurvival1^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS1=larv.surv1*(1-larv.dr1) 
    
    LT1=larv.surv1*larv.dr1 
    
    PS1=pup.surv1*(1-pup.dr1) 
    
    PT1=pup.surv1*pup.dr1 
    
    LarvaSurvival2=LSurRatevec * TreatmentMat2
    
    TimeToPupate2=LDevRatevec
    
    PupaSurvival2=PSurRatevec
    
    TimeToEmerge2=PDevRatevec
    
    EggDailySurvival2=EggDailySurvival
    
    larv.surv2=LarvaSurvival2^(1/TimeToPupate2) 
    
    larv.dr2=1/TimeToPupate2 
    
    pup.surv2=PupaSurvival2^(1/TimeToEmerge2) 
    
    pup.dr2=1/TimeToEmerge2 
    
    egg.surv=EggDailySurvival2^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS2=larv.surv2*(1-larv.dr2) 
    
    LT2=larv.surv2*larv.dr2 
    
    PS2=pup.surv2*(1-pup.dr2) 
    
    PT2=pup.surv2*pup.dr2 
    
    LarvaSurvival3=LSurRatevec * TreatmentMat3
    
    TimeToPupate3=LDevRatevec
    
    PupaSurvival3=PSurRatevec
    
    TimeToEmerge3=PDevRatevec
    
    EggDailySurvival3=EggDailySurvival
    
    larv.surv3=LarvaSurvival3^(1/TimeToPupate3) 
    
    larv.dr3=1/TimeToPupate3 
    
    pup.surv3=PupaSurvival3^(1/TimeToEmerge3) 
    
    pup.dr3=1/TimeToEmerge3 
    
    egg.surv=EggDailySurvival3^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS3=larv.surv3*(1-larv.dr3) 
    
    LT3=larv.surv3*larv.dr3 
    
    PS3=pup.surv3*(1-pup.dr3) 
    
    PT3=pup.surv3*pup.dr3 
    
    LarvaSurvival4=LSurRatevec * TreatmentMat4
    
    TimeToPupate4=LDevRatevec
    
    PupaSurvival4=PSurRatevec
    
    TimeToEmerge4=PDevRatevec
    
    EggDailySurvival4=EggDailySurvival
    
    larv.surv4=LarvaSurvival4^(1/TimeToPupate4) 
    
    larv.dr4=1/TimeToPupate4 
    
    pup.surv4=PupaSurvival4^(1/TimeToEmerge4) 
    
    pup.dr4=1/TimeToEmerge4 
    
    egg.surv=EggDailySurvival4^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS4=larv.surv4*(1-larv.dr4) 
    
    LT4=larv.surv4*larv.dr4 
    
    PS4=pup.surv4*(1-pup.dr4) 
    
    PT4=pup.surv4*pup.dr4 
    
    LarvaSurvival5=LSurRatevec * TreatmentMat5
    
    TimeToPupate5=LDevRatevec
    
    PupaSurvival5=PSurRatevec
    
    TimeToEmerge5=PDevRatevec
    
    EggDailySurvival5=EggDailySurvival
    
    larv.surv5=LarvaSurvival5^(1/TimeToPupate5) 
    
    larv.dr5=1/TimeToPupate5 
    
    pup.surv5=PupaSurvival5^(1/TimeToEmerge5) 
    
    pup.dr5=1/TimeToEmerge5 
    
    egg.surv=EggDailySurvival5^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS5=larv.surv5*(1-larv.dr5) 
    
    LT5=larv.surv5*larv.dr5 
    
    PS5=pup.surv5*(1-pup.dr5) 
    
    PT5=pup.surv5*pup.dr5 
    
    LarvaSurvival6=LSurRatevec * TreatmentMat6
    
    TimeToPupate6=LDevRatevec
    
    PupaSurvival6=PSurRatevec
    
    TimeToEmerge6=PDevRatevec
    
    EggDailySurvival6=EggDailySurvival
    
    larv.surv6=LarvaSurvival6^(1/TimeToPupate6) 
    
    larv.dr6=1/TimeToPupate6 
    
    pup.surv6=PupaSurvival6^(1/TimeToEmerge6) 
    
    pup.dr6=1/TimeToEmerge6 
    
    egg.surv=EggDailySurvival6^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS6=larv.surv6*(1-larv.dr6) 
    
    LT6=larv.surv6*larv.dr6 
    
    PS6=pup.surv6*(1-pup.dr6) 
    
    PT6=pup.surv6*pup.dr6 
    
    LarvaSurvival7=LSurRatevec * TreatmentMat7
    
    TimeToPupate7=LDevRatevec
    
    PupaSurvival7=PSurRatevec
    
    TimeToEmerge7=PDevRatevec
    
    EggDailySurvival7=EggDailySurvival
    
    larv.surv7=LarvaSurvival7^(1/TimeToPupate7) 
    
    larv.dr7=1/TimeToPupate7 
    
    pup.surv7=PupaSurvival7^(1/TimeToEmerge7) 
    
    pup.dr7=1/TimeToEmerge7 
    
    egg.surv=EggDailySurvival7^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS7=larv.surv7*(1-larv.dr7) 
    
    LT7=larv.surv7*larv.dr7 
    
    PS7=pup.surv7*(1-pup.dr7) 
    
    PT7=pup.surv7*pup.dr7 
    
    LarvaSurvival8=LSurRatevec * TreatmentMat8
    
    TimeToPupate8=LDevRatevec
    
    PupaSurvival8=PSurRatevec
    
    TimeToEmerge8=PDevRatevec
    
    EggDailySurvival8=EggDailySurvival
    
    larv.surv8=LarvaSurvival8^(1/TimeToPupate8) 
    
    larv.dr8=1/TimeToPupate8 
    
    pup.surv8=PupaSurvival8^(1/TimeToEmerge8) 
    
    pup.dr8=1/TimeToEmerge8 
    
    egg.surv=EggDailySurvival8^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS8=larv.surv8*(1-larv.dr8) 
    
    LT8=larv.surv8*larv.dr8 
    
    PS8=pup.surv8*(1-pup.dr8) 
    
    PT8=pup.surv8*pup.dr8 
    
    LarvaSurvival9=LSurRatevec * TreatmentMat9
    
    TimeToPupate9=LDevRatevec
    
    PupaSurvival9=PSurRatevec
    
    TimeToEmerge9=PDevRatevec
    
    EggDailySurvival9=EggDailySurvival
    
    larv.surv9=LarvaSurvival9^(1/TimeToPupate9) 
    
    larv.dr9=1/TimeToPupate9 
    
    pup.surv9=PupaSurvival9^(1/TimeToEmerge9) 
    
    pup.dr9=1/TimeToEmerge9 
    
    egg.surv=EggDailySurvival9^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS9=larv.surv9*(1-larv.dr9) 
    
    LT9=larv.surv9*larv.dr9 
    
    PS9=pup.surv9*(1-pup.dr9) 
    
    PT9=pup.surv9*pup.dr9 
    
    LarvaSurvival10=LSurRatevec * TreatmentMat10
    
    TimeToPupate10=LDevRatevec
    
    PupaSurvival10=PSurRatevec
    
    TimeToEmerge10=PDevRatevec
    
    EggDailySurvival10=EggDailySurvival
    
    larv.surv10=LarvaSurvival10^(1/TimeToPupate10) 
    
    larv.dr10=1/TimeToPupate10 
    
    pup.surv10=PupaSurvival10^(1/TimeToEmerge10) 
    
    pup.dr10=1/TimeToEmerge10 
    
    egg.surv=EggDailySurvival10^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS10=larv.surv10*(1-larv.dr10) 
    
    LT10=larv.surv10*larv.dr10 
    
    PS10=pup.surv10*(1-pup.dr10) 
    
    PT10=pup.surv10*pup.dr10 
    
    LarvaSurvival11=LSurRatevec * TreatmentMat11
    
    TimeToPupate11=LDevRatevec
    
    PupaSurvival11=PSurRatevec
    
    TimeToEmerge11=PDevRatevec
    
    EggDailySurvival11=EggDailySurvival
    
    larv.surv11=LarvaSurvival11^(1/TimeToPupate11) 
    
    larv.dr11=1/TimeToPupate11 
    
    pup.surv11=PupaSurvival11^(1/TimeToEmerge11) 
    
    pup.dr11=1/TimeToEmerge11 
    
    egg.surv=EggDailySurvival11^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS11=larv.surv11*(1-larv.dr11) 
    
    LT11=larv.surv11*larv.dr11 
    
    PS11=pup.surv11*(1-pup.dr11) 
    
    PT11=pup.surv11*pup.dr11 
    
    LarvaSurvival12=LSurRatevec * TreatmentMat12
    
    TimeToPupate12=LDevRatevec
    
    PupaSurvival12=PSurRatevec
    
    TimeToEmerge12=PDevRatevec
    
    EggDailySurvival12=EggDailySurvival
    
    larv.surv12=LarvaSurvival12^(1/TimeToPupate12) 
    
    larv.dr12=1/TimeToPupate12 
    
    pup.surv12=PupaSurvival12^(1/TimeToEmerge12) 
    
    pup.dr12=1/TimeToEmerge12 
    
    egg.surv=EggDailySurvival12^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS12=larv.surv12*(1-larv.dr12) 
    
    LT12=larv.surv12*larv.dr12 
    
    PS12=pup.surv12*(1-pup.dr12) 
    
    PT12=pup.surv12*pup.dr12 
    
    LarvaSurvival13=LSurRatevec * TreatmentMat13
    
    TimeToPupate13=LDevRatevec
    
    PupaSurvival13=PSurRatevec
    
    TimeToEmerge13=PDevRatevec
    
    EggDailySurvival13=EggDailySurvival
    
    larv.surv13=LarvaSurvival13^(1/TimeToPupate13) 
    
    larv.dr13=1/TimeToPupate13 
    
    pup.surv13=PupaSurvival13^(1/TimeToEmerge13) 
    
    pup.dr13=1/TimeToEmerge13 
    
    egg.surv=EggDailySurvival13^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS13=larv.surv13*(1-larv.dr13) 
    
    LT13=larv.surv13*larv.dr13 
    
    PS13=pup.surv13*(1-pup.dr13) 
    
    PT13=pup.surv13*pup.dr13 
    
    LarvaSurvival14=LSurRatevec * TreatmentMat14
    
    TimeToPupate14=LDevRatevec
    
    PupaSurvival14=PSurRatevec
    
    TimeToEmerge14=PDevRatevec
    
    EggDailySurvival14=EggDailySurvival
    
    larv.surv14=LarvaSurvival14^(1/TimeToPupate14) 
    
    larv.dr14=1/TimeToPupate14 
    
    pup.surv14=PupaSurvival14^(1/TimeToEmerge14) 
    
    pup.dr14=1/TimeToEmerge14 
    
    egg.surv=EggDailySurvival14^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS14=larv.surv14*(1-larv.dr14) 
    
    LT14=larv.surv14*larv.dr14 
    
    PS14=pup.surv14*(1-pup.dr14) 
    
    PT14=pup.surv14*pup.dr14 
    
    LarvaSurvival15=LSurRatevec * TreatmentMat15
    
    TimeToPupate15=LDevRatevec
    
    PupaSurvival15=PSurRatevec
    
    TimeToEmerge15=PDevRatevec
    
    EggDailySurvival15=EggDailySurvival
    
    larv.surv15=LarvaSurvival15^(1/TimeToPupate15) 
    
    larv.dr15=1/TimeToPupate15 
    
    pup.surv15=PupaSurvival15^(1/TimeToEmerge15) 
    
    pup.dr15=1/TimeToEmerge15 
    
    egg.surv=EggDailySurvival15^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS15=larv.surv15*(1-larv.dr15) 
    
    LT15=larv.surv15*larv.dr15 
    
    PS15=pup.surv15*(1-pup.dr15) 
    
    PT15=pup.surv15*pup.dr15 
    
    LarvaSurvival16=LSurRatevec * TreatmentMat16
    
    TimeToPupate16=LDevRatevec
    
    PupaSurvival16=PSurRatevec
    
    TimeToEmerge16=PDevRatevec
    
    EggDailySurvival16=EggDailySurvival
    
    larv.surv16=LarvaSurvival16^(1/TimeToPupate16) 
    
    larv.dr16=1/TimeToPupate16 
    
    pup.surv16=PupaSurvival16^(1/TimeToEmerge16) 
    
    pup.dr16=1/TimeToEmerge16 
    
    egg.surv=EggDailySurvival16^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS16=larv.surv16*(1-larv.dr16) 
    
    LT16=larv.surv16*larv.dr16 
    
    PS16=pup.surv16*(1-pup.dr16) 
    
    PT16=pup.surv16*pup.dr16 
    
    LarvaSurvival17=LSurRatevec * TreatmentMat17
    
    TimeToPupate17=LDevRatevec
    
    PupaSurvival17=PSurRatevec
    
    TimeToEmerge17=PDevRatevec
    
    EggDailySurvival17=EggDailySurvival
    
    larv.surv17=LarvaSurvival17^(1/TimeToPupate17) 
    
    larv.dr17=1/TimeToPupate17 
    
    pup.surv17=PupaSurvival17^(1/TimeToEmerge17) 
    
    pup.dr17=1/TimeToEmerge17 
    
    egg.surv=EggDailySurvival17^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS17=larv.surv17*(1-larv.dr17) 
    
    LT17=larv.surv17*larv.dr17 
    
    PS17=pup.surv17*(1-pup.dr17) 
    
    PT17=pup.surv17*pup.dr17 
    
    LarvaSurvival18=LSurRatevec * TreatmentMat18
    
    TimeToPupate18=LDevRatevec
    
    PupaSurvival18=PSurRatevec
    
    TimeToEmerge18=PDevRatevec
    
    EggDailySurvival18=EggDailySurvival
    
    larv.surv18=LarvaSurvival18^(1/TimeToPupate18) 
    
    larv.dr18=1/TimeToPupate18 
    
    pup.surv18=PupaSurvival18^(1/TimeToEmerge18) 
    
    pup.dr18=1/TimeToEmerge18 
    
    egg.surv=EggDailySurvival18^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS18=larv.surv18*(1-larv.dr18) 
    
    LT18=larv.surv18*larv.dr18 
    
    PS18=pup.surv18*(1-pup.dr18) 
    
    PT18=pup.surv18*pup.dr18 
    
    LarvaSurvival19=LSurRatevec * TreatmentMat19
    
    TimeToPupate19=LDevRatevec
    
    PupaSurvival19=PSurRatevec
    
    TimeToEmerge19=PDevRatevec
    
    EggDailySurvival19=EggDailySurvival
    
    larv.surv19=LarvaSurvival19^(1/TimeToPupate19) 
    
    larv.dr19=1/TimeToPupate19 
    
    pup.surv19=PupaSurvival19^(1/TimeToEmerge19) 
    
    pup.dr19=1/TimeToEmerge19 
    
    egg.surv=EggDailySurvival19^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS19=larv.surv19*(1-larv.dr19) 
    
    LT19=larv.surv19*larv.dr19 
    
    PS19=pup.surv19*(1-pup.dr19) 
    
    PT19=pup.surv19*pup.dr19 
    
    LarvaSurvival20=LSurRatevec * TreatmentMat20
    
    TimeToPupate20=LDevRatevec
    
    PupaSurvival20=PSurRatevec
    
    TimeToEmerge20=PDevRatevec
    
    EggDailySurvival20=EggDailySurvival
    
    larv.surv20=LarvaSurvival20^(1/TimeToPupate20) 
    
    larv.dr20=1/TimeToPupate20 
    
    pup.surv20=PupaSurvival20^(1/TimeToEmerge20) 
    
    pup.dr20=1/TimeToEmerge20 
    
    egg.surv=EggDailySurvival20^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS20=larv.surv20*(1-larv.dr20) 
    
    LT20=larv.surv20*larv.dr20 
    
    PS20=pup.surv20*(1-pup.dr20) 
    
    PT20=pup.surv20*pup.dr20 
    
    LarvaSurvival21=LSurRatevec * TreatmentMat21
    
    TimeToPupate21=LDevRatevec
    
    PupaSurvival21=PSurRatevec
    
    TimeToEmerge21=PDevRatevec
    
    EggDailySurvival21=EggDailySurvival
    
    larv.surv21=LarvaSurvival21^(1/TimeToPupate21) 
    
    larv.dr21=1/TimeToPupate21 
    
    pup.surv21=PupaSurvival21^(1/TimeToEmerge21) 
    
    pup.dr21=1/TimeToEmerge21 
    
    egg.surv=EggDailySurvival21^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS21=larv.surv21*(1-larv.dr21) 
    
    LT21=larv.surv21*larv.dr21 
    
    PS21=pup.surv21*(1-pup.dr21) 
    
    PT21=pup.surv21*pup.dr21 
    
    LarvaSurvival22=LSurRatevec * TreatmentMat22
    
    TimeToPupate22=LDevRatevec
    
    PupaSurvival22=PSurRatevec
    
    TimeToEmerge22=PDevRatevec
    
    EggDailySurvival22=EggDailySurvival
    
    larv.surv22=LarvaSurvival22^(1/TimeToPupate22) 
    
    larv.dr22=1/TimeToPupate22 
    
    pup.surv22=PupaSurvival22^(1/TimeToEmerge22) 
    
    pup.dr22=1/TimeToEmerge22 
    
    egg.surv=EggDailySurvival22^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS22=larv.surv22*(1-larv.dr22) 
    
    LT22=larv.surv22*larv.dr22 
    
    PS22=pup.surv22*(1-pup.dr22) 
    
    PT22=pup.surv22*pup.dr22 
    
    LarvaSurvival23=LSurRatevec * TreatmentMat23
    
    TimeToPupate23=LDevRatevec
    
    PupaSurvival23=PSurRatevec
    
    TimeToEmerge23=PDevRatevec
    
    EggDailySurvival23=EggDailySurvival
    
    larv.surv23=LarvaSurvival23^(1/TimeToPupate23) 
    
    larv.dr23=1/TimeToPupate23 
    
    pup.surv23=PupaSurvival23^(1/TimeToEmerge23) 
    
    pup.dr23=1/TimeToEmerge23 
    
    egg.surv=EggDailySurvival23^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS23=larv.surv23*(1-larv.dr23) 
    
    LT23=larv.surv23*larv.dr23 
    
    PS23=pup.surv23*(1-pup.dr23) 
    
    PT23=pup.surv23*pup.dr23 
    
    LarvaSurvival24=LSurRatevec * TreatmentMat24
    
    TimeToPupate24=LDevRatevec
    
    PupaSurvival24=PSurRatevec
    
    TimeToEmerge24=PDevRatevec
    
    EggDailySurvival24=EggDailySurvival
    
    larv.surv24=LarvaSurvival24^(1/TimeToPupate24) 
    
    larv.dr24=1/TimeToPupate24 
    
    pup.surv24=PupaSurvival24^(1/TimeToEmerge24) 
    
    pup.dr24=1/TimeToEmerge24 
    
    egg.surv=EggDailySurvival24^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS24=larv.surv24*(1-larv.dr24) 
    
    LT24=larv.surv24*larv.dr24 
    
    PS24=pup.surv24*(1-pup.dr24) 
    
    PT24=pup.surv24*pup.dr24 
    
    LarvaSurvival25=LSurRatevec * TreatmentMat25
    
    TimeToPupate25=LDevRatevec
    
    PupaSurvival25=PSurRatevec
    
    TimeToEmerge25=PDevRatevec
    
    EggDailySurvival25=EggDailySurvival
    
    larv.surv25=LarvaSurvival25^(1/TimeToPupate25) 
    
    larv.dr25=1/TimeToPupate25 
    
    pup.surv25=PupaSurvival25^(1/TimeToEmerge25) 
    
    pup.dr25=1/TimeToEmerge25 
    
    egg.surv=EggDailySurvival25^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS25=larv.surv25*(1-larv.dr25) 
    
    LT25=larv.surv25*larv.dr25 
    
    PS25=pup.surv25*(1-pup.dr25) 
    
    PT25=pup.surv25*pup.dr25 
    
    LarvaSurvival26=LSurRatevec * TreatmentMat26
    
    TimeToPupate26=LDevRatevec
    
    PupaSurvival26=PSurRatevec
    
    TimeToEmerge26=PDevRatevec
    
    EggDailySurvival26=EggDailySurvival
    
    larv.surv26=LarvaSurvival26^(1/TimeToPupate26) 
    
    larv.dr26=1/TimeToPupate26 
    
    pup.surv26=PupaSurvival26^(1/TimeToEmerge26) 
    
    pup.dr26=1/TimeToEmerge26 
    
    egg.surv=EggDailySurvival26^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS26=larv.surv26*(1-larv.dr26) 
    
    LT26=larv.surv26*larv.dr26 
    
    PS26=pup.surv26*(1-pup.dr26) 
    
    PT26=pup.surv26*pup.dr26 
    
    LarvaSurvival27=LSurRatevec * TreatmentMat27
    
    TimeToPupate27=LDevRatevec
    
    PupaSurvival27=PSurRatevec
    
    TimeToEmerge27=PDevRatevec
    
    EggDailySurvival27=EggDailySurvival
    
    larv.surv27=LarvaSurvival27^(1/TimeToPupate27) 
    
    larv.dr27=1/TimeToPupate27 
    
    pup.surv27=PupaSurvival27^(1/TimeToEmerge27) 
    
    pup.dr27=1/TimeToEmerge27 
    
    egg.surv=EggDailySurvival27^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS27=larv.surv27*(1-larv.dr27) 
    
    LT27=larv.surv27*larv.dr27 
    
    PS27=pup.surv27*(1-pup.dr27) 
    
    PT27=pup.surv27*pup.dr27 
    
    LarvaSurvival28=LSurRatevec * TreatmentMat28
    
    TimeToPupate28=LDevRatevec
    
    PupaSurvival28=PSurRatevec
    
    TimeToEmerge28=PDevRatevec
    
    EggDailySurvival28=EggDailySurvival
    
    larv.surv28=LarvaSurvival28^(1/TimeToPupate28) 
    
    larv.dr28=1/TimeToPupate28 
    
    pup.surv28=PupaSurvival28^(1/TimeToEmerge28) 
    
    pup.dr28=1/TimeToEmerge28 
    
    egg.surv=EggDailySurvival28^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS28=larv.surv28*(1-larv.dr28) 
    
    LT28=larv.surv28*larv.dr28 
    
    PS28=pup.surv28*(1-pup.dr28) 
    
    PT28=pup.surv28*pup.dr28 
    
    LarvaSurvival29=LSurRatevec * TreatmentMat29
    
    TimeToPupate29=LDevRatevec
    
    PupaSurvival29=PSurRatevec
    
    TimeToEmerge29=PDevRatevec
    
    EggDailySurvival29=EggDailySurvival
    
    larv.surv29=LarvaSurvival29^(1/TimeToPupate29) 
    
    larv.dr29=1/TimeToPupate29 
    
    pup.surv29=PupaSurvival29^(1/TimeToEmerge29) 
    
    pup.dr29=1/TimeToEmerge29 
    
    egg.surv=EggDailySurvival29^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS29=larv.surv29*(1-larv.dr29) 
    
    LT29=larv.surv29*larv.dr29 
    
    PS29=pup.surv29*(1-pup.dr29) 
    
    PT29=pup.surv29*pup.dr29 
    
    LarvaSurvival30=LSurRatevec * TreatmentMat30
    
    TimeToPupate30=LDevRatevec
    
    PupaSurvival30=PSurRatevec
    
    TimeToEmerge30=PDevRatevec
    
    EggDailySurvival30=EggDailySurvival
    
    larv.surv30=LarvaSurvival30^(1/TimeToPupate30) 
    
    larv.dr30=1/TimeToPupate30 
    
    pup.surv30=PupaSurvival30^(1/TimeToEmerge30) 
    
    pup.dr30=1/TimeToEmerge30 
    
    egg.surv=EggDailySurvival30^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS30=larv.surv30*(1-larv.dr30) 
    
    LT30=larv.surv30*larv.dr30 
    
    PS30=pup.surv30*(1-pup.dr30) 
    
    PT30=pup.surv30*pup.dr30 
    
    LarvaSurvival31=LSurRatevec * TreatmentMat31
    
    TimeToPupate31=LDevRatevec
    
    PupaSurvival31=PSurRatevec
    
    TimeToEmerge31=PDevRatevec
    
    EggDailySurvival31=EggDailySurvival
    
    larv.surv31=LarvaSurvival31^(1/TimeToPupate31) 
    
    larv.dr31=1/TimeToPupate31 
    
    pup.surv31=PupaSurvival31^(1/TimeToEmerge31) 
    
    pup.dr31=1/TimeToEmerge31 
    
    egg.surv=EggDailySurvival31^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS31=larv.surv31*(1-larv.dr31) 
    
    LT31=larv.surv31*larv.dr31 
    
    PS31=pup.surv31*(1-pup.dr31) 
    
    PT31=pup.surv31*pup.dr31 
    
    LarvaSurvival32=LSurRatevec * TreatmentMat32
    
    TimeToPupate32=LDevRatevec
    
    PupaSurvival32=PSurRatevec
    
    TimeToEmerge32=PDevRatevec
    
    EggDailySurvival32=EggDailySurvival
    
    larv.surv32=LarvaSurvival32^(1/TimeToPupate32) 
    
    larv.dr32=1/TimeToPupate32 
    
    pup.surv32=PupaSurvival32^(1/TimeToEmerge32) 
    
    pup.dr32=1/TimeToEmerge32 
    
    egg.surv=EggDailySurvival32^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS32=larv.surv32*(1-larv.dr32) 
    
    LT32=larv.surv32*larv.dr32 
    
    PS32=pup.surv32*(1-pup.dr32) 
    
    PT32=pup.surv32*pup.dr32 
    
    LarvaSurvival33=LSurRatevec * TreatmentMat33
    
    TimeToPupate33=LDevRatevec
    
    PupaSurvival33=PSurRatevec
    
    TimeToEmerge33=PDevRatevec
    
    EggDailySurvival33=EggDailySurvival
    
    larv.surv33=LarvaSurvival33^(1/TimeToPupate33) 
    
    larv.dr33=1/TimeToPupate33 
    
    pup.surv33=PupaSurvival33^(1/TimeToEmerge33) 
    
    pup.dr33=1/TimeToEmerge33 
    
    egg.surv=EggDailySurvival33^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS33=larv.surv33*(1-larv.dr33) 
    
    LT33=larv.surv33*larv.dr33 
    
    PS33=pup.surv33*(1-pup.dr33) 
    
    PT33=pup.surv33*pup.dr33 
    
    LarvaSurvival34=LSurRatevec * TreatmentMat34
    
    TimeToPupate34=LDevRatevec
    
    PupaSurvival34=PSurRatevec
    
    TimeToEmerge34=PDevRatevec
    
    EggDailySurvival34=EggDailySurvival
    
    larv.surv34=LarvaSurvival34^(1/TimeToPupate34) 
    
    larv.dr34=1/TimeToPupate34 
    
    pup.surv34=PupaSurvival34^(1/TimeToEmerge34) 
    
    pup.dr34=1/TimeToEmerge34 
    
    egg.surv=EggDailySurvival34^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS34=larv.surv34*(1-larv.dr34) 
    
    LT34=larv.surv34*larv.dr34 
    
    PS34=pup.surv34*(1-pup.dr34) 
    
    PT34=pup.surv34*pup.dr34 
    
    LarvaSurvival35=LSurRatevec * TreatmentMat35
    
    TimeToPupate35=LDevRatevec
    
    PupaSurvival35=PSurRatevec
    
    TimeToEmerge35=PDevRatevec
    
    EggDailySurvival35=EggDailySurvival
    
    larv.surv35=LarvaSurvival35^(1/TimeToPupate35) 
    
    larv.dr35=1/TimeToPupate35 
    
    pup.surv35=PupaSurvival35^(1/TimeToEmerge35) 
    
    pup.dr35=1/TimeToEmerge35 
    
    egg.surv=EggDailySurvival35^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS35=larv.surv35*(1-larv.dr35) 
    
    LT35=larv.surv35*larv.dr35 
    
    PS35=pup.surv35*(1-pup.dr35) 
    
    PT35=pup.surv35*pup.dr35 
    
    LarvaSurvival36=LSurRatevec * TreatmentMat36
    
    TimeToPupate36=LDevRatevec
    
    PupaSurvival36=PSurRatevec
    
    TimeToEmerge36=PDevRatevec
    
    EggDailySurvival36=EggDailySurvival
    
    larv.surv36=LarvaSurvival36^(1/TimeToPupate36) 
    
    larv.dr36=1/TimeToPupate36 
    
    pup.surv36=PupaSurvival36^(1/TimeToEmerge36) 
    
    pup.dr36=1/TimeToEmerge36 
    
    egg.surv=EggDailySurvival36^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS36=larv.surv36*(1-larv.dr36) 
    
    LT36=larv.surv36*larv.dr36 
    
    PS36=pup.surv36*(1-pup.dr36) 
    
    PT36=pup.surv36*pup.dr36 
    
    LarvaSurvival37=LSurRatevec * TreatmentMat37
    
    TimeToPupate37=LDevRatevec
    
    PupaSurvival37=PSurRatevec
    
    TimeToEmerge37=PDevRatevec
    
    EggDailySurvival37=EggDailySurvival
    
    larv.surv37=LarvaSurvival37^(1/TimeToPupate37) 
    
    larv.dr37=1/TimeToPupate37 
    
    pup.surv37=PupaSurvival37^(1/TimeToEmerge37) 
    
    pup.dr37=1/TimeToEmerge37 
    
    egg.surv=EggDailySurvival37^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS37=larv.surv37*(1-larv.dr37) 
    
    LT37=larv.surv37*larv.dr37 
    
    PS37=pup.surv37*(1-pup.dr37) 
    
    PT37=pup.surv37*pup.dr37 
    
    LarvaSurvival38=LSurRatevec * TreatmentMat38
    
    TimeToPupate38=LDevRatevec
    
    PupaSurvival38=PSurRatevec
    
    TimeToEmerge38=PDevRatevec
    
    EggDailySurvival38=EggDailySurvival
    
    larv.surv38=LarvaSurvival38^(1/TimeToPupate38) 
    
    larv.dr38=1/TimeToPupate38 
    
    pup.surv38=PupaSurvival38^(1/TimeToEmerge38) 
    
    pup.dr38=1/TimeToEmerge38 
    
    egg.surv=EggDailySurvival38^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS38=larv.surv38*(1-larv.dr38) 
    
    LT38=larv.surv38*larv.dr38 
    
    PS38=pup.surv38*(1-pup.dr38) 
    
    PT38=pup.surv38*pup.dr38 
    
    LarvaSurvival39=LSurRatevec * TreatmentMat39
    
    TimeToPupate39=LDevRatevec
    
    PupaSurvival39=PSurRatevec
    
    TimeToEmerge39=PDevRatevec
    
    EggDailySurvival39=EggDailySurvival
    
    larv.surv39=LarvaSurvival39^(1/TimeToPupate39) 
    
    larv.dr39=1/TimeToPupate39 
    
    pup.surv39=PupaSurvival39^(1/TimeToEmerge39) 
    
    pup.dr39=1/TimeToEmerge39 
    
    egg.surv=EggDailySurvival39^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS39=larv.surv39*(1-larv.dr39) 
    
    LT39=larv.surv39*larv.dr39 
    
    PS39=pup.surv39*(1-pup.dr39) 
    
    PT39=pup.surv39*pup.dr39 
    
    LarvaSurvival40=LSurRatevec * TreatmentMat40
    
    TimeToPupate40=LDevRatevec
    
    PupaSurvival40=PSurRatevec
    
    TimeToEmerge40=PDevRatevec
    
    EggDailySurvival40=EggDailySurvival
    
    larv.surv40=LarvaSurvival40^(1/TimeToPupate40) 
    
    larv.dr40=1/TimeToPupate40 
    
    pup.surv40=PupaSurvival40^(1/TimeToEmerge40) 
    
    pup.dr40=1/TimeToEmerge40 
    
    egg.surv=EggDailySurvival40^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS40=larv.surv40*(1-larv.dr40) 
    
    LT40=larv.surv40*larv.dr40 
    
    PS40=pup.surv40*(1-pup.dr40) 
    
    PT40=pup.surv40*pup.dr40 
    
    LarvaSurvival41=LSurRatevec * TreatmentMat41
    
    TimeToPupate41=LDevRatevec
    
    PupaSurvival41=PSurRatevec
    
    TimeToEmerge41=PDevRatevec
    
    EggDailySurvival41=EggDailySurvival
    
    larv.surv41=LarvaSurvival41^(1/TimeToPupate41) 
    
    larv.dr41=1/TimeToPupate41 
    
    pup.surv41=PupaSurvival41^(1/TimeToEmerge41) 
    
    pup.dr41=1/TimeToEmerge41 
    
    egg.surv=EggDailySurvival41^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS41=larv.surv41*(1-larv.dr41) 
    
    LT41=larv.surv41*larv.dr41 
    
    PS41=pup.surv41*(1-pup.dr41) 
    
    PT41=pup.surv41*pup.dr41 
    
    LarvaSurvival42=LSurRatevec * TreatmentMat42
    
    TimeToPupate42=LDevRatevec
    
    PupaSurvival42=PSurRatevec
    
    TimeToEmerge42=PDevRatevec
    
    EggDailySurvival42=EggDailySurvival
    
    larv.surv42=LarvaSurvival42^(1/TimeToPupate42) 
    
    larv.dr42=1/TimeToPupate42 
    
    pup.surv42=PupaSurvival42^(1/TimeToEmerge42) 
    
    pup.dr42=1/TimeToEmerge42 
    
    egg.surv=EggDailySurvival42^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS42=larv.surv42*(1-larv.dr42) 
    
    LT42=larv.surv42*larv.dr42 
    
    PS42=pup.surv42*(1-pup.dr42) 
    
    PT42=pup.surv42*pup.dr42 
    
    LarvaSurvival43=LSurRatevec * TreatmentMat43
    
    TimeToPupate43=LDevRatevec
    
    PupaSurvival43=PSurRatevec
    
    TimeToEmerge43=PDevRatevec
    
    EggDailySurvival43=EggDailySurvival
    
    larv.surv43=LarvaSurvival43^(1/TimeToPupate43) 
    
    larv.dr43=1/TimeToPupate43 
    
    pup.surv43=PupaSurvival43^(1/TimeToEmerge43) 
    
    pup.dr43=1/TimeToEmerge43 
    
    egg.surv=EggDailySurvival43^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS43=larv.surv43*(1-larv.dr43) 
    
    LT43=larv.surv43*larv.dr43 
    
    PS43=pup.surv43*(1-pup.dr43) 
    
    PT43=pup.surv43*pup.dr43 
    
    LarvaSurvival44=LSurRatevec * TreatmentMat44
    
    TimeToPupate44=LDevRatevec
    
    PupaSurvival44=PSurRatevec
    
    TimeToEmerge44=PDevRatevec
    
    EggDailySurvival44=EggDailySurvival
    
    larv.surv44=LarvaSurvival44^(1/TimeToPupate44) 
    
    larv.dr44=1/TimeToPupate44 
    
    pup.surv44=PupaSurvival44^(1/TimeToEmerge44) 
    
    pup.dr44=1/TimeToEmerge44 
    
    egg.surv=EggDailySurvival44^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS44=larv.surv44*(1-larv.dr44) 
    
    LT44=larv.surv44*larv.dr44 
    
    PS44=pup.surv44*(1-pup.dr44) 
    
    PT44=pup.surv44*pup.dr44 
    
    LarvaSurvival45=LSurRatevec * TreatmentMat45
    
    TimeToPupate45=LDevRatevec
    
    PupaSurvival45=PSurRatevec
    
    TimeToEmerge45=PDevRatevec
    
    EggDailySurvival45=EggDailySurvival
    
    larv.surv45=LarvaSurvival45^(1/TimeToPupate45) 
    
    larv.dr45=1/TimeToPupate45 
    
    pup.surv45=PupaSurvival45^(1/TimeToEmerge45) 
    
    pup.dr45=1/TimeToEmerge45 
    
    egg.surv=EggDailySurvival45^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS45=larv.surv45*(1-larv.dr45) 
    
    LT45=larv.surv45*larv.dr45 
    
    PS45=pup.surv45*(1-pup.dr45) 
    
    PT45=pup.surv45*pup.dr45 
    
    LarvaSurvival46=LSurRatevec * TreatmentMat46
    
    TimeToPupate46=LDevRatevec
    
    PupaSurvival46=PSurRatevec
    
    TimeToEmerge46=PDevRatevec
    
    EggDailySurvival46=EggDailySurvival
    
    larv.surv46=LarvaSurvival46^(1/TimeToPupate46) 
    
    larv.dr46=1/TimeToPupate46 
    
    pup.surv46=PupaSurvival46^(1/TimeToEmerge46) 
    
    pup.dr46=1/TimeToEmerge46 
    
    egg.surv=EggDailySurvival46^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS46=larv.surv46*(1-larv.dr46) 
    
    LT46=larv.surv46*larv.dr46 
    
    PS46=pup.surv46*(1-pup.dr46) 
    
    PT46=pup.surv46*pup.dr46 
    
    LarvaSurvival47=LSurRatevec * TreatmentMat47
    
    TimeToPupate47=LDevRatevec
    
    PupaSurvival47=PSurRatevec
    
    TimeToEmerge47=PDevRatevec
    
    EggDailySurvival47=EggDailySurvival
    
    larv.surv47=LarvaSurvival47^(1/TimeToPupate47) 
    
    larv.dr47=1/TimeToPupate47 
    
    pup.surv47=PupaSurvival47^(1/TimeToEmerge47) 
    
    pup.dr47=1/TimeToEmerge47 
    
    egg.surv=EggDailySurvival47^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS47=larv.surv47*(1-larv.dr47) 
    
    LT47=larv.surv47*larv.dr47 
    
    PS47=pup.surv47*(1-pup.dr47) 
    
    PT47=pup.surv47*pup.dr47 
    
    LarvaSurvival48=LSurRatevec * TreatmentMat48
    
    TimeToPupate48=LDevRatevec
    
    PupaSurvival48=PSurRatevec
    
    TimeToEmerge48=PDevRatevec
    
    EggDailySurvival48=EggDailySurvival
    
    larv.surv48=LarvaSurvival48^(1/TimeToPupate48) 
    
    larv.dr48=1/TimeToPupate48 
    
    pup.surv48=PupaSurvival48^(1/TimeToEmerge48) 
    
    pup.dr48=1/TimeToEmerge48 
    
    egg.surv=EggDailySurvival48^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS48=larv.surv48*(1-larv.dr48) 
    
    LT48=larv.surv48*larv.dr48 
    
    PS48=pup.surv48*(1-pup.dr48) 
    
    PT48=pup.surv48*pup.dr48 
    
    LarvaSurvival49=LSurRatevec * TreatmentMat49
    
    TimeToPupate49=LDevRatevec
    
    PupaSurvival49=PSurRatevec
    
    TimeToEmerge49=PDevRatevec
    
    EggDailySurvival49=EggDailySurvival
    
    larv.surv49=LarvaSurvival49^(1/TimeToPupate49) 
    
    larv.dr49=1/TimeToPupate49 
    
    pup.surv49=PupaSurvival49^(1/TimeToEmerge49) 
    
    pup.dr49=1/TimeToEmerge49 
    
    egg.surv=EggDailySurvival49^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS49=larv.surv49*(1-larv.dr49) 
    
    LT49=larv.surv49*larv.dr49 
    
    PS49=pup.surv49*(1-pup.dr49) 
    
    PT49=pup.surv49*pup.dr49 
    
    LarvaSurvival50=LSurRatevec * TreatmentMat50
    
    TimeToPupate50=LDevRatevec
    
    PupaSurvival50=PSurRatevec
    
    TimeToEmerge50=PDevRatevec
    
    EggDailySurvival50=EggDailySurvival
    
    larv.surv50=LarvaSurvival50^(1/TimeToPupate50) 
    
    larv.dr50=1/TimeToPupate50 
    
    pup.surv50=PupaSurvival50^(1/TimeToEmerge50) 
    
    pup.dr50=1/TimeToEmerge50 
    
    egg.surv=EggDailySurvival50^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS50=larv.surv50*(1-larv.dr50) 
    
    LT50=larv.surv50*larv.dr50 
    
    PS50=pup.surv50*(1-pup.dr50) 
    
    PT50=pup.surv50*pup.dr50 
    
    LarvaSurvival51=LSurRatevec * TreatmentMat51
    
    TimeToPupate51=LDevRatevec
    
    PupaSurvival51=PSurRatevec
    
    TimeToEmerge51=PDevRatevec
    
    EggDailySurvival51=EggDailySurvival
    
    larv.surv51=LarvaSurvival51^(1/TimeToPupate51) 
    
    larv.dr51=1/TimeToPupate51 
    
    pup.surv51=PupaSurvival51^(1/TimeToEmerge51) 
    
    pup.dr51=1/TimeToEmerge51 
    
    egg.surv=EggDailySurvival51^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS51=larv.surv51*(1-larv.dr51) 
    
    LT51=larv.surv51*larv.dr51 
    
    PS51=pup.surv51*(1-pup.dr51) 
    
    PT51=pup.surv51*pup.dr51 
    
    LarvaSurvival52=LSurRatevec * TreatmentMat52
    
    TimeToPupate52=LDevRatevec
    
    PupaSurvival52=PSurRatevec
    
    TimeToEmerge52=PDevRatevec
    
    EggDailySurvival52=EggDailySurvival
    
    larv.surv52=LarvaSurvival52^(1/TimeToPupate52) 
    
    larv.dr52=1/TimeToPupate52 
    
    pup.surv52=PupaSurvival52^(1/TimeToEmerge52) 
    
    pup.dr52=1/TimeToEmerge52 
    
    egg.surv=EggDailySurvival52^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS52=larv.surv52*(1-larv.dr52) 
    
    LT52=larv.surv52*larv.dr52 
    
    PS52=pup.surv52*(1-pup.dr52) 
    
    PT52=pup.surv52*pup.dr52 
    
    LarvaSurvival53=LSurRatevec * TreatmentMat53
    
    TimeToPupate53=LDevRatevec
    
    PupaSurvival53=PSurRatevec
    
    TimeToEmerge53=PDevRatevec
    
    EggDailySurvival53=EggDailySurvival
    
    larv.surv53=LarvaSurvival53^(1/TimeToPupate53) 
    
    larv.dr53=1/TimeToPupate53 
    
    pup.surv53=PupaSurvival53^(1/TimeToEmerge53) 
    
    pup.dr53=1/TimeToEmerge53 
    
    egg.surv=EggDailySurvival53^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS53=larv.surv53*(1-larv.dr53) 
    
    LT53=larv.surv53*larv.dr53 
    
    PS53=pup.surv53*(1-pup.dr53) 
    
    PT53=pup.surv53*pup.dr53 
    
    LarvaSurvival54=LSurRatevec * TreatmentMat54
    
    TimeToPupate54=LDevRatevec
    
    PupaSurvival54=PSurRatevec
    
    TimeToEmerge54=PDevRatevec
    
    EggDailySurvival54=EggDailySurvival
    
    larv.surv54=LarvaSurvival54^(1/TimeToPupate54) 
    
    larv.dr54=1/TimeToPupate54 
    
    pup.surv54=PupaSurvival54^(1/TimeToEmerge54) 
    
    pup.dr54=1/TimeToEmerge54 
    
    egg.surv=EggDailySurvival54^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS54=larv.surv54*(1-larv.dr54) 
    
    LT54=larv.surv54*larv.dr54 
    
    PS54=pup.surv54*(1-pup.dr54) 
    
    PT54=pup.surv54*pup.dr54 
    
    LarvaSurvival55=LSurRatevec * TreatmentMat55
    
    TimeToPupate55=LDevRatevec
    
    PupaSurvival55=PSurRatevec
    
    TimeToEmerge55=PDevRatevec
    
    EggDailySurvival55=EggDailySurvival
    
    larv.surv55=LarvaSurvival55^(1/TimeToPupate55) 
    
    larv.dr55=1/TimeToPupate55 
    
    pup.surv55=PupaSurvival55^(1/TimeToEmerge55) 
    
    pup.dr55=1/TimeToEmerge55 
    
    egg.surv=EggDailySurvival55^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS55=larv.surv55*(1-larv.dr55) 
    
    LT55=larv.surv55*larv.dr55 
    
    PS55=pup.surv55*(1-pup.dr55) 
    
    PT55=pup.surv55*pup.dr55 
    
    LarvaSurvival56=LSurRatevec * TreatmentMat56
    
    TimeToPupate56=LDevRatevec
    
    PupaSurvival56=PSurRatevec
    
    TimeToEmerge56=PDevRatevec
    
    EggDailySurvival56=EggDailySurvival
    
    larv.surv56=LarvaSurvival56^(1/TimeToPupate56) 
    
    larv.dr56=1/TimeToPupate56 
    
    pup.surv56=PupaSurvival56^(1/TimeToEmerge56) 
    
    pup.dr56=1/TimeToEmerge56 
    
    egg.surv=EggDailySurvival56^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS56=larv.surv56*(1-larv.dr56) 
    
    LT56=larv.surv56*larv.dr56 
    
    PS56=pup.surv56*(1-pup.dr56) 
    
    PT56=pup.surv56*pup.dr56 
    
    LarvaSurvival57=LSurRatevec * TreatmentMat57
    
    TimeToPupate57=LDevRatevec
    
    PupaSurvival57=PSurRatevec
    
    TimeToEmerge57=PDevRatevec
    
    EggDailySurvival57=EggDailySurvival
    
    larv.surv57=LarvaSurvival57^(1/TimeToPupate57) 
    
    larv.dr57=1/TimeToPupate57 
    
    pup.surv57=PupaSurvival57^(1/TimeToEmerge57) 
    
    pup.dr57=1/TimeToEmerge57 
    
    egg.surv=EggDailySurvival57^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS57=larv.surv57*(1-larv.dr57) 
    
    LT57=larv.surv57*larv.dr57 
    
    PS57=pup.surv57*(1-pup.dr57) 
    
    PT57=pup.surv57*pup.dr57 
    
    LarvaSurvival58=LSurRatevec * TreatmentMat58
    
    TimeToPupate58=LDevRatevec
    
    PupaSurvival58=PSurRatevec
    
    TimeToEmerge58=PDevRatevec
    
    EggDailySurvival58=EggDailySurvival
    
    larv.surv58=LarvaSurvival58^(1/TimeToPupate58) 
    
    larv.dr58=1/TimeToPupate58 
    
    pup.surv58=PupaSurvival58^(1/TimeToEmerge58) 
    
    pup.dr58=1/TimeToEmerge58 
    
    egg.surv=EggDailySurvival58^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS58=larv.surv58*(1-larv.dr58) 
    
    LT58=larv.surv58*larv.dr58 
    
    PS58=pup.surv58*(1-pup.dr58) 
    
    PT58=pup.surv58*pup.dr58 
    
    LarvaSurvival59=LSurRatevec * TreatmentMat59
    
    TimeToPupate59=LDevRatevec
    
    PupaSurvival59=PSurRatevec
    
    TimeToEmerge59=PDevRatevec
    
    EggDailySurvival59=EggDailySurvival
    
    larv.surv59=LarvaSurvival59^(1/TimeToPupate59) 
    
    larv.dr59=1/TimeToPupate59 
    
    pup.surv59=PupaSurvival59^(1/TimeToEmerge59) 
    
    pup.dr59=1/TimeToEmerge59 
    
    egg.surv=EggDailySurvival59^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS59=larv.surv59*(1-larv.dr59) 
    
    LT59=larv.surv59*larv.dr59 
    
    PS59=pup.surv59*(1-pup.dr59) 
    
    PT59=pup.surv59*pup.dr59 
    
    LarvaSurvival60=LSurRatevec * TreatmentMat60
    
    TimeToPupate60=LDevRatevec
    
    PupaSurvival60=PSurRatevec
    
    TimeToEmerge60=PDevRatevec
    
    EggDailySurvival60=EggDailySurvival
    
    larv.surv60=LarvaSurvival60^(1/TimeToPupate60) 
    
    larv.dr60=1/TimeToPupate60 
    
    pup.surv60=PupaSurvival60^(1/TimeToEmerge60) 
    
    pup.dr60=1/TimeToEmerge60 
    
    egg.surv=EggDailySurvival60^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS60=larv.surv60*(1-larv.dr60) 
    
    LT60=larv.surv60*larv.dr60 
    
    PS60=pup.surv60*(1-pup.dr60) 
    
    PT60=pup.surv60*pup.dr60 
    
    LarvaSurvival61=LSurRatevec * TreatmentMat61
    
    TimeToPupate61=LDevRatevec
    
    PupaSurvival61=PSurRatevec
    
    TimeToEmerge61=PDevRatevec
    
    EggDailySurvival61=EggDailySurvival
    
    larv.surv61=LarvaSurvival61^(1/TimeToPupate61) 
    
    larv.dr61=1/TimeToPupate61 
    
    pup.surv61=PupaSurvival61^(1/TimeToEmerge61) 
    
    pup.dr61=1/TimeToEmerge61 
    
    egg.surv=EggDailySurvival61^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS61=larv.surv61*(1-larv.dr61) 
    
    LT61=larv.surv61*larv.dr61 
    
    PS61=pup.surv61*(1-pup.dr61) 
    
    PT61=pup.surv61*pup.dr61 
    
    LarvaSurvival62=LSurRatevec * TreatmentMat62
    
    TimeToPupate62=LDevRatevec
    
    PupaSurvival62=PSurRatevec
    
    TimeToEmerge62=PDevRatevec
    
    EggDailySurvival62=EggDailySurvival
    
    larv.surv62=LarvaSurvival62^(1/TimeToPupate62) 
    
    larv.dr62=1/TimeToPupate62 
    
    pup.surv62=PupaSurvival62^(1/TimeToEmerge62) 
    
    pup.dr62=1/TimeToEmerge62 
    
    egg.surv=EggDailySurvival62^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS62=larv.surv62*(1-larv.dr62) 
    
    LT62=larv.surv62*larv.dr62 
    
    PS62=pup.surv62*(1-pup.dr62) 
    
    PT62=pup.surv62*pup.dr62 
    
    LarvaSurvival63=LSurRatevec * TreatmentMat63
    
    TimeToPupate63=LDevRatevec
    
    PupaSurvival63=PSurRatevec
    
    TimeToEmerge63=PDevRatevec
    
    EggDailySurvival63=EggDailySurvival
    
    larv.surv63=LarvaSurvival63^(1/TimeToPupate63) 
    
    larv.dr63=1/TimeToPupate63 
    
    pup.surv63=PupaSurvival63^(1/TimeToEmerge63) 
    
    pup.dr63=1/TimeToEmerge63 
    
    egg.surv=EggDailySurvival63^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS63=larv.surv63*(1-larv.dr63) 
    
    LT63=larv.surv63*larv.dr63 
    
    PS63=pup.surv63*(1-pup.dr63) 
    
    PT63=pup.surv63*pup.dr63 
    
    LarvaSurvival64=LSurRatevec * TreatmentMat64
    
    TimeToPupate64=LDevRatevec
    
    PupaSurvival64=PSurRatevec
    
    TimeToEmerge64=PDevRatevec
    
    EggDailySurvival64=EggDailySurvival
    
    larv.surv64=LarvaSurvival64^(1/TimeToPupate64) 
    
    larv.dr64=1/TimeToPupate64 
    
    pup.surv64=PupaSurvival64^(1/TimeToEmerge64) 
    
    pup.dr64=1/TimeToEmerge64 
    
    egg.surv=EggDailySurvival64^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS64=larv.surv64*(1-larv.dr64) 
    
    LT64=larv.surv64*larv.dr64 
    
    PS64=pup.surv64*(1-pup.dr64) 
    
    PT64=pup.surv64*pup.dr64 
    
    LarvaSurvival65=LSurRatevec * TreatmentMat65
    
    TimeToPupate65=LDevRatevec
    
    PupaSurvival65=PSurRatevec
    
    TimeToEmerge65=PDevRatevec
    
    EggDailySurvival65=EggDailySurvival
    
    larv.surv65=LarvaSurvival65^(1/TimeToPupate65) 
    
    larv.dr65=1/TimeToPupate65 
    
    pup.surv65=PupaSurvival65^(1/TimeToEmerge65) 
    
    pup.dr65=1/TimeToEmerge65 
    
    egg.surv=EggDailySurvival65^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS65=larv.surv65*(1-larv.dr65) 
    
    LT65=larv.surv65*larv.dr65 
    
    PS65=pup.surv65*(1-pup.dr65) 
    
    PT65=pup.surv65*pup.dr65 
    
    LarvaSurvival66=LSurRatevec * TreatmentMat66
    
    TimeToPupate66=LDevRatevec
    
    PupaSurvival66=PSurRatevec
    
    TimeToEmerge66=PDevRatevec
    
    EggDailySurvival66=EggDailySurvival
    
    larv.surv66=LarvaSurvival66^(1/TimeToPupate66) 
    
    larv.dr66=1/TimeToPupate66 
    
    pup.surv66=PupaSurvival66^(1/TimeToEmerge66) 
    
    pup.dr66=1/TimeToEmerge66 
    
    egg.surv=EggDailySurvival66^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS66=larv.surv66*(1-larv.dr66) 
    
    LT66=larv.surv66*larv.dr66 
    
    PS66=pup.surv66*(1-pup.dr66) 
    
    PT66=pup.surv66*pup.dr66 
    
    LarvaSurvival67=LSurRatevec * TreatmentMat67
    
    TimeToPupate67=LDevRatevec
    
    PupaSurvival67=PSurRatevec
    
    TimeToEmerge67=PDevRatevec
    
    EggDailySurvival67=EggDailySurvival
    
    larv.surv67=LarvaSurvival67^(1/TimeToPupate67) 
    
    larv.dr67=1/TimeToPupate67 
    
    pup.surv67=PupaSurvival67^(1/TimeToEmerge67) 
    
    pup.dr67=1/TimeToEmerge67 
    
    egg.surv=EggDailySurvival67^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS67=larv.surv67*(1-larv.dr67) 
    
    LT67=larv.surv67*larv.dr67 
    
    PS67=pup.surv67*(1-pup.dr67) 
    
    PT67=pup.surv67*pup.dr67 
    
    LarvaSurvival68=LSurRatevec * TreatmentMat68
    
    TimeToPupate68=LDevRatevec
    
    PupaSurvival68=PSurRatevec
    
    TimeToEmerge68=PDevRatevec
    
    EggDailySurvival68=EggDailySurvival
    
    larv.surv68=LarvaSurvival68^(1/TimeToPupate68) 
    
    larv.dr68=1/TimeToPupate68 
    
    pup.surv68=PupaSurvival68^(1/TimeToEmerge68) 
    
    pup.dr68=1/TimeToEmerge68 
    
    egg.surv=EggDailySurvival68^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS68=larv.surv68*(1-larv.dr68) 
    
    LT68=larv.surv68*larv.dr68 
    
    PS68=pup.surv68*(1-pup.dr68) 
    
    PT68=pup.surv68*pup.dr68 
    
    LarvaSurvival69=LSurRatevec * TreatmentMat69
    
    TimeToPupate69=LDevRatevec
    
    PupaSurvival69=PSurRatevec
    
    TimeToEmerge69=PDevRatevec
    
    EggDailySurvival69=EggDailySurvival
    
    larv.surv69=LarvaSurvival69^(1/TimeToPupate69) 
    
    larv.dr69=1/TimeToPupate69 
    
    pup.surv69=PupaSurvival69^(1/TimeToEmerge69) 
    
    pup.dr69=1/TimeToEmerge69 
    
    egg.surv=EggDailySurvival69^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS69=larv.surv69*(1-larv.dr69) 
    
    LT69=larv.surv69*larv.dr69 
    
    PS69=pup.surv69*(1-pup.dr69) 
    
    PT69=pup.surv69*pup.dr69 
    
    LarvaSurvival70=LSurRatevec * TreatmentMat70
    
    TimeToPupate70=LDevRatevec
    
    PupaSurvival70=PSurRatevec
    
    TimeToEmerge70=PDevRatevec
    
    EggDailySurvival70=EggDailySurvival
    
    larv.surv70=LarvaSurvival70^(1/TimeToPupate70) 
    
    larv.dr70=1/TimeToPupate70 
    
    pup.surv70=PupaSurvival70^(1/TimeToEmerge70) 
    
    pup.dr70=1/TimeToEmerge70 
    
    egg.surv=EggDailySurvival70^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS70=larv.surv70*(1-larv.dr70) 
    
    LT70=larv.surv70*larv.dr70 
    
    PS70=pup.surv70*(1-pup.dr70) 
    
    PT70=pup.surv70*pup.dr70 
    
    LarvaSurvival71=LSurRatevec * TreatmentMat71
    
    TimeToPupate71=LDevRatevec
    
    PupaSurvival71=PSurRatevec
    
    TimeToEmerge71=PDevRatevec
    
    EggDailySurvival71=EggDailySurvival
    
    larv.surv71=LarvaSurvival71^(1/TimeToPupate71) 
    
    larv.dr71=1/TimeToPupate71 
    
    pup.surv71=PupaSurvival71^(1/TimeToEmerge71) 
    
    pup.dr71=1/TimeToEmerge71 
    
    egg.surv=EggDailySurvival71^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS71=larv.surv71*(1-larv.dr71) 
    
    LT71=larv.surv71*larv.dr71 
    
    PS71=pup.surv71*(1-pup.dr71) 
    
    PT71=pup.surv71*pup.dr71 
    
    LarvaSurvival72=LSurRatevec * TreatmentMat72
    
    TimeToPupate72=LDevRatevec
    
    PupaSurvival72=PSurRatevec
    
    TimeToEmerge72=PDevRatevec
    
    EggDailySurvival72=EggDailySurvival
    
    larv.surv72=LarvaSurvival72^(1/TimeToPupate72) 
    
    larv.dr72=1/TimeToPupate72 
    
    pup.surv72=PupaSurvival72^(1/TimeToEmerge72) 
    
    pup.dr72=1/TimeToEmerge72 
    
    egg.surv=EggDailySurvival72^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS72=larv.surv72*(1-larv.dr72) 
    
    LT72=larv.surv72*larv.dr72 
    
    PS72=pup.surv72*(1-pup.dr72) 
    
    PT72=pup.surv72*pup.dr72 
    
    LarvaSurvival73=LSurRatevec * TreatmentMat73
    
    TimeToPupate73=LDevRatevec
    
    PupaSurvival73=PSurRatevec
    
    TimeToEmerge73=PDevRatevec
    
    EggDailySurvival73=EggDailySurvival
    
    larv.surv73=LarvaSurvival73^(1/TimeToPupate73) 
    
    larv.dr73=1/TimeToPupate73 
    
    pup.surv73=PupaSurvival73^(1/TimeToEmerge73) 
    
    pup.dr73=1/TimeToEmerge73 
    
    egg.surv=EggDailySurvival73^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS73=larv.surv73*(1-larv.dr73) 
    
    LT73=larv.surv73*larv.dr73 
    
    PS73=pup.surv73*(1-pup.dr73) 
    
    PT73=pup.surv73*pup.dr73 
    
    LarvaSurvival74=LSurRatevec * TreatmentMat74
    
    TimeToPupate74=LDevRatevec
    
    PupaSurvival74=PSurRatevec
    
    TimeToEmerge74=PDevRatevec
    
    EggDailySurvival74=EggDailySurvival
    
    larv.surv74=LarvaSurvival74^(1/TimeToPupate74) 
    
    larv.dr74=1/TimeToPupate74 
    
    pup.surv74=PupaSurvival74^(1/TimeToEmerge74) 
    
    pup.dr74=1/TimeToEmerge74 
    
    egg.surv=EggDailySurvival74^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS74=larv.surv74*(1-larv.dr74) 
    
    LT74=larv.surv74*larv.dr74 
    
    PS74=pup.surv74*(1-pup.dr74) 
    
    PT74=pup.surv74*pup.dr74 
    
    LarvaSurvival75=LSurRatevec * TreatmentMat75
    
    TimeToPupate75=LDevRatevec
    
    PupaSurvival75=PSurRatevec
    
    TimeToEmerge75=PDevRatevec
    
    EggDailySurvival75=EggDailySurvival
    
    larv.surv75=LarvaSurvival75^(1/TimeToPupate75) 
    
    larv.dr75=1/TimeToPupate75 
    
    pup.surv75=PupaSurvival75^(1/TimeToEmerge75) 
    
    pup.dr75=1/TimeToEmerge75 
    
    egg.surv=EggDailySurvival75^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS75=larv.surv75*(1-larv.dr75) 
    
    LT75=larv.surv75*larv.dr75 
    
    PS75=pup.surv75*(1-pup.dr75) 
    
    PT75=pup.surv75*pup.dr75 
    
    LarvaSurvival76=LSurRatevec * TreatmentMat76
    
    TimeToPupate76=LDevRatevec
    
    PupaSurvival76=PSurRatevec
    
    TimeToEmerge76=PDevRatevec
    
    EggDailySurvival76=EggDailySurvival
    
    larv.surv76=LarvaSurvival76^(1/TimeToPupate76) 
    
    larv.dr76=1/TimeToPupate76 
    
    pup.surv76=PupaSurvival76^(1/TimeToEmerge76) 
    
    pup.dr76=1/TimeToEmerge76 
    
    egg.surv=EggDailySurvival76^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS76=larv.surv76*(1-larv.dr76) 
    
    LT76=larv.surv76*larv.dr76 
    
    PS76=pup.surv76*(1-pup.dr76) 
    
    PT76=pup.surv76*pup.dr76 
    
    LarvaSurvival77=LSurRatevec * TreatmentMat77
    
    TimeToPupate77=LDevRatevec
    
    PupaSurvival77=PSurRatevec
    
    TimeToEmerge77=PDevRatevec
    
    EggDailySurvival77=EggDailySurvival
    
    larv.surv77=LarvaSurvival77^(1/TimeToPupate77) 
    
    larv.dr77=1/TimeToPupate77 
    
    pup.surv77=PupaSurvival77^(1/TimeToEmerge77) 
    
    pup.dr77=1/TimeToEmerge77 
    
    egg.surv=EggDailySurvival77^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS77=larv.surv77*(1-larv.dr77) 
    
    LT77=larv.surv77*larv.dr77 
    
    PS77=pup.surv77*(1-pup.dr77) 
    
    PT77=pup.surv77*pup.dr77 
    
    LarvaSurvival78=LSurRatevec * TreatmentMat78
    
    TimeToPupate78=LDevRatevec
    
    PupaSurvival78=PSurRatevec
    
    TimeToEmerge78=PDevRatevec
    
    EggDailySurvival78=EggDailySurvival
    
    larv.surv78=LarvaSurvival78^(1/TimeToPupate78) 
    
    larv.dr78=1/TimeToPupate78 
    
    pup.surv78=PupaSurvival78^(1/TimeToEmerge78) 
    
    pup.dr78=1/TimeToEmerge78 
    
    egg.surv=EggDailySurvival78^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS78=larv.surv78*(1-larv.dr78) 
    
    LT78=larv.surv78*larv.dr78 
    
    PS78=pup.surv78*(1-pup.dr78) 
    
    PT78=pup.surv78*pup.dr78 
    
    LarvaSurvival79=LSurRatevec * TreatmentMat79
    
    TimeToPupate79=LDevRatevec
    
    PupaSurvival79=PSurRatevec
    
    TimeToEmerge79=PDevRatevec
    
    EggDailySurvival79=EggDailySurvival
    
    larv.surv79=LarvaSurvival79^(1/TimeToPupate79) 
    
    larv.dr79=1/TimeToPupate79 
    
    pup.surv79=PupaSurvival79^(1/TimeToEmerge79) 
    
    pup.dr79=1/TimeToEmerge79 
    
    egg.surv=EggDailySurvival79^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS79=larv.surv79*(1-larv.dr79) 
    
    LT79=larv.surv79*larv.dr79 
    
    PS79=pup.surv79*(1-pup.dr79) 
    
    PT79=pup.surv79*pup.dr79 
    
    LarvaSurvival80=LSurRatevec * TreatmentMat80
    
    TimeToPupate80=LDevRatevec
    
    PupaSurvival80=PSurRatevec
    
    TimeToEmerge80=PDevRatevec
    
    EggDailySurvival80=EggDailySurvival
    
    larv.surv80=LarvaSurvival80^(1/TimeToPupate80) 
    
    larv.dr80=1/TimeToPupate80 
    
    pup.surv80=PupaSurvival80^(1/TimeToEmerge80) 
    
    pup.dr80=1/TimeToEmerge80 
    
    egg.surv=EggDailySurvival80^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS80=larv.surv80*(1-larv.dr80) 
    
    LT80=larv.surv80*larv.dr80 
    
    PS80=pup.surv80*(1-pup.dr80) 
    
    PT80=pup.surv80*pup.dr80 
    
    LarvaSurvival81=LSurRatevec * TreatmentMat81
    
    TimeToPupate81=LDevRatevec
    
    PupaSurvival81=PSurRatevec
    
    TimeToEmerge81=PDevRatevec
    
    EggDailySurvival81=EggDailySurvival
    
    larv.surv81=LarvaSurvival81^(1/TimeToPupate81) 
    
    larv.dr81=1/TimeToPupate81 
    
    pup.surv81=PupaSurvival81^(1/TimeToEmerge81) 
    
    pup.dr81=1/TimeToEmerge81 
    
    egg.surv=EggDailySurvival81^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS81=larv.surv81*(1-larv.dr81) 
    
    LT81=larv.surv81*larv.dr81 
    
    PS81=pup.surv81*(1-pup.dr81) 
    
    PT81=pup.surv81*pup.dr81 
    
    LarvaSurvival82=LSurRatevec * TreatmentMat82
    
    TimeToPupate82=LDevRatevec
    
    PupaSurvival82=PSurRatevec
    
    TimeToEmerge82=PDevRatevec
    
    EggDailySurvival82=EggDailySurvival
    
    larv.surv82=LarvaSurvival82^(1/TimeToPupate82) 
    
    larv.dr82=1/TimeToPupate82 
    
    pup.surv82=PupaSurvival82^(1/TimeToEmerge82) 
    
    pup.dr82=1/TimeToEmerge82 
    
    egg.surv=EggDailySurvival82^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS82=larv.surv82*(1-larv.dr82) 
    
    LT82=larv.surv82*larv.dr82 
    
    PS82=pup.surv82*(1-pup.dr82) 
    
    PT82=pup.surv82*pup.dr82 
    
    LarvaSurvival83=LSurRatevec * TreatmentMat83
    
    TimeToPupate83=LDevRatevec
    
    PupaSurvival83=PSurRatevec
    
    TimeToEmerge83=PDevRatevec
    
    EggDailySurvival83=EggDailySurvival
    
    larv.surv83=LarvaSurvival83^(1/TimeToPupate83) 
    
    larv.dr83=1/TimeToPupate83 
    
    pup.surv83=PupaSurvival83^(1/TimeToEmerge83) 
    
    pup.dr83=1/TimeToEmerge83 
    
    egg.surv=EggDailySurvival83^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS83=larv.surv83*(1-larv.dr83) 
    
    LT83=larv.surv83*larv.dr83 
    
    PS83=pup.surv83*(1-pup.dr83) 
    
    PT83=pup.surv83*pup.dr83 
    
    LarvaSurvival84=LSurRatevec * TreatmentMat84
    
    TimeToPupate84=LDevRatevec
    
    PupaSurvival84=PSurRatevec
    
    TimeToEmerge84=PDevRatevec
    
    EggDailySurvival84=EggDailySurvival
    
    larv.surv84=LarvaSurvival84^(1/TimeToPupate84) 
    
    larv.dr84=1/TimeToPupate84 
    
    pup.surv84=PupaSurvival84^(1/TimeToEmerge84) 
    
    pup.dr84=1/TimeToEmerge84 
    
    egg.surv=EggDailySurvival84^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS84=larv.surv84*(1-larv.dr84) 
    
    LT84=larv.surv84*larv.dr84 
    
    PS84=pup.surv84*(1-pup.dr84) 
    
    PT84=pup.surv84*pup.dr84 
    
    LarvaSurvival85=LSurRatevec * TreatmentMat85
    
    TimeToPupate85=LDevRatevec
    
    PupaSurvival85=PSurRatevec
    
    TimeToEmerge85=PDevRatevec
    
    EggDailySurvival85=EggDailySurvival
    
    larv.surv85=LarvaSurvival85^(1/TimeToPupate85) 
    
    larv.dr85=1/TimeToPupate85 
    
    pup.surv85=PupaSurvival85^(1/TimeToEmerge85) 
    
    pup.dr85=1/TimeToEmerge85 
    
    egg.surv=EggDailySurvival85^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS85=larv.surv85*(1-larv.dr85) 
    
    LT85=larv.surv85*larv.dr85 
    
    PS85=pup.surv85*(1-pup.dr85) 
    
    PT85=pup.surv85*pup.dr85 
    
    LarvaSurvival86=LSurRatevec * TreatmentMat86
    
    TimeToPupate86=LDevRatevec
    
    PupaSurvival86=PSurRatevec
    
    TimeToEmerge86=PDevRatevec
    
    EggDailySurvival86=EggDailySurvival
    
    larv.surv86=LarvaSurvival86^(1/TimeToPupate86) 
    
    larv.dr86=1/TimeToPupate86 
    
    pup.surv86=PupaSurvival86^(1/TimeToEmerge86) 
    
    pup.dr86=1/TimeToEmerge86 
    
    egg.surv=EggDailySurvival86^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS86=larv.surv86*(1-larv.dr86) 
    
    LT86=larv.surv86*larv.dr86 
    
    PS86=pup.surv86*(1-pup.dr86) 
    
    PT86=pup.surv86*pup.dr86 
    
    LarvaSurvival87=LSurRatevec * TreatmentMat87
    
    TimeToPupate87=LDevRatevec
    
    PupaSurvival87=PSurRatevec
    
    TimeToEmerge87=PDevRatevec
    
    EggDailySurvival87=EggDailySurvival
    
    larv.surv87=LarvaSurvival87^(1/TimeToPupate87) 
    
    larv.dr87=1/TimeToPupate87 
    
    pup.surv87=PupaSurvival87^(1/TimeToEmerge87) 
    
    pup.dr87=1/TimeToEmerge87 
    
    egg.surv=EggDailySurvival87^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS87=larv.surv87*(1-larv.dr87) 
    
    LT87=larv.surv87*larv.dr87 
    
    PS87=pup.surv87*(1-pup.dr87) 
    
    PT87=pup.surv87*pup.dr87 
    
    LarvaSurvival88=LSurRatevec * TreatmentMat88
    
    TimeToPupate88=LDevRatevec
    
    PupaSurvival88=PSurRatevec
    
    TimeToEmerge88=PDevRatevec
    
    EggDailySurvival88=EggDailySurvival
    
    larv.surv88=LarvaSurvival88^(1/TimeToPupate88) 
    
    larv.dr88=1/TimeToPupate88 
    
    pup.surv88=PupaSurvival88^(1/TimeToEmerge88) 
    
    pup.dr88=1/TimeToEmerge88 
    
    egg.surv=EggDailySurvival88^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS88=larv.surv88*(1-larv.dr88) 
    
    LT88=larv.surv88*larv.dr88 
    
    PS88=pup.surv88*(1-pup.dr88) 
    
    PT88=pup.surv88*pup.dr88 
    
    LarvaSurvival89=LSurRatevec * TreatmentMat89
    
    TimeToPupate89=LDevRatevec
    
    PupaSurvival89=PSurRatevec
    
    TimeToEmerge89=PDevRatevec
    
    EggDailySurvival89=EggDailySurvival
    
    larv.surv89=LarvaSurvival89^(1/TimeToPupate89) 
    
    larv.dr89=1/TimeToPupate89 
    
    pup.surv89=PupaSurvival89^(1/TimeToEmerge89) 
    
    pup.dr89=1/TimeToEmerge89 
    
    egg.surv=EggDailySurvival89^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS89=larv.surv89*(1-larv.dr89) 
    
    LT89=larv.surv89*larv.dr89 
    
    PS89=pup.surv89*(1-pup.dr89) 
    
    PT89=pup.surv89*pup.dr89 
    
    LarvaSurvival90=LSurRatevec * TreatmentMat90
    
    TimeToPupate90=LDevRatevec
    
    PupaSurvival90=PSurRatevec
    
    TimeToEmerge90=PDevRatevec
    
    EggDailySurvival90=EggDailySurvival
    
    larv.surv90=LarvaSurvival90^(1/TimeToPupate90) 
    
    larv.dr90=1/TimeToPupate90 
    
    pup.surv90=PupaSurvival90^(1/TimeToEmerge90) 
    
    pup.dr90=1/TimeToEmerge90 
    
    egg.surv=EggDailySurvival90^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS90=larv.surv90*(1-larv.dr90) 
    
    LT90=larv.surv90*larv.dr90 
    
    PS90=pup.surv90*(1-pup.dr90) 
    
    PT90=pup.surv90*pup.dr90 
    
    LarvaSurvival91=LSurRatevec * TreatmentMat91
    
    TimeToPupate91=LDevRatevec
    
    PupaSurvival91=PSurRatevec
    
    TimeToEmerge91=PDevRatevec
    
    EggDailySurvival91=EggDailySurvival
    
    larv.surv91=LarvaSurvival91^(1/TimeToPupate91) 
    
    larv.dr91=1/TimeToPupate91 
    
    pup.surv91=PupaSurvival91^(1/TimeToEmerge91) 
    
    pup.dr91=1/TimeToEmerge91 
    
    egg.surv=EggDailySurvival91^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS91=larv.surv91*(1-larv.dr91) 
    
    LT91=larv.surv91*larv.dr91 
    
    PS91=pup.surv91*(1-pup.dr91) 
    
    PT91=pup.surv91*pup.dr91 
    
    LarvaSurvival92=LSurRatevec * TreatmentMat92
    
    TimeToPupate92=LDevRatevec
    
    PupaSurvival92=PSurRatevec
    
    TimeToEmerge92=PDevRatevec
    
    EggDailySurvival92=EggDailySurvival
    
    larv.surv92=LarvaSurvival92^(1/TimeToPupate92) 
    
    larv.dr92=1/TimeToPupate92 
    
    pup.surv92=PupaSurvival92^(1/TimeToEmerge92) 
    
    pup.dr92=1/TimeToEmerge92 
    
    egg.surv=EggDailySurvival92^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS92=larv.surv92*(1-larv.dr92) 
    
    LT92=larv.surv92*larv.dr92 
    
    PS92=pup.surv92*(1-pup.dr92) 
    
    PT92=pup.surv92*pup.dr92 
    
    LarvaSurvival93=LSurRatevec * TreatmentMat93
    
    TimeToPupate93=LDevRatevec
    
    PupaSurvival93=PSurRatevec
    
    TimeToEmerge93=PDevRatevec
    
    EggDailySurvival93=EggDailySurvival
    
    larv.surv93=LarvaSurvival93^(1/TimeToPupate93) 
    
    larv.dr93=1/TimeToPupate93 
    
    pup.surv93=PupaSurvival93^(1/TimeToEmerge93) 
    
    pup.dr93=1/TimeToEmerge93 
    
    egg.surv=EggDailySurvival93^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS93=larv.surv93*(1-larv.dr93) 
    
    LT93=larv.surv93*larv.dr93 
    
    PS93=pup.surv93*(1-pup.dr93) 
    
    PT93=pup.surv93*pup.dr93 
    
    LarvaSurvival94=LSurRatevec * TreatmentMat94
    
    TimeToPupate94=LDevRatevec
    
    PupaSurvival94=PSurRatevec
    
    TimeToEmerge94=PDevRatevec
    
    EggDailySurvival94=EggDailySurvival
    
    larv.surv94=LarvaSurvival94^(1/TimeToPupate94) 
    
    larv.dr94=1/TimeToPupate94 
    
    pup.surv94=PupaSurvival94^(1/TimeToEmerge94) 
    
    pup.dr94=1/TimeToEmerge94 
    
    egg.surv=EggDailySurvival94^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS94=larv.surv94*(1-larv.dr94) 
    
    LT94=larv.surv94*larv.dr94 
    
    PS94=pup.surv94*(1-pup.dr94) 
    
    PT94=pup.surv94*pup.dr94 
    
    LarvaSurvival95=LSurRatevec * TreatmentMat95
    
    TimeToPupate95=LDevRatevec
    
    PupaSurvival95=PSurRatevec
    
    TimeToEmerge95=PDevRatevec
    
    EggDailySurvival95=EggDailySurvival
    
    larv.surv95=LarvaSurvival95^(1/TimeToPupate95) 
    
    larv.dr95=1/TimeToPupate95 
    
    pup.surv95=PupaSurvival95^(1/TimeToEmerge95) 
    
    pup.dr95=1/TimeToEmerge95 
    
    egg.surv=EggDailySurvival95^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS95=larv.surv95*(1-larv.dr95) 
    
    LT95=larv.surv95*larv.dr95 
    
    PS95=pup.surv95*(1-pup.dr95) 
    
    PT95=pup.surv95*pup.dr95 
    
    LarvaSurvival96=LSurRatevec * TreatmentMat96
    
    TimeToPupate96=LDevRatevec
    
    PupaSurvival96=PSurRatevec
    
    TimeToEmerge96=PDevRatevec
    
    EggDailySurvival96=EggDailySurvival
    
    larv.surv96=LarvaSurvival96^(1/TimeToPupate96) 
    
    larv.dr96=1/TimeToPupate96 
    
    pup.surv96=PupaSurvival96^(1/TimeToEmerge96) 
    
    pup.dr96=1/TimeToEmerge96 
    
    egg.surv=EggDailySurvival96^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS96=larv.surv96*(1-larv.dr96) 
    
    LT96=larv.surv96*larv.dr96 
    
    PS96=pup.surv96*(1-pup.dr96) 
    
    PT96=pup.surv96*pup.dr96 
    
    LarvaSurvival97=LSurRatevec * TreatmentMat97
    
    TimeToPupate97=LDevRatevec
    
    PupaSurvival97=PSurRatevec
    
    TimeToEmerge97=PDevRatevec
    
    EggDailySurvival97=EggDailySurvival
    
    larv.surv97=LarvaSurvival97^(1/TimeToPupate97) 
    
    larv.dr97=1/TimeToPupate97 
    
    pup.surv97=PupaSurvival97^(1/TimeToEmerge97) 
    
    pup.dr97=1/TimeToEmerge97 
    
    egg.surv=EggDailySurvival97^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS97=larv.surv97*(1-larv.dr97) 
    
    LT97=larv.surv97*larv.dr97 
    
    PS97=pup.surv97*(1-pup.dr97) 
    
    PT97=pup.surv97*pup.dr97 
    
    LarvaSurvival98=LSurRatevec * TreatmentMat98
    
    TimeToPupate98=LDevRatevec
    
    PupaSurvival98=PSurRatevec
    
    TimeToEmerge98=PDevRatevec
    
    EggDailySurvival98=EggDailySurvival
    
    larv.surv98=LarvaSurvival98^(1/TimeToPupate98) 
    
    larv.dr98=1/TimeToPupate98 
    
    pup.surv98=PupaSurvival98^(1/TimeToEmerge98) 
    
    pup.dr98=1/TimeToEmerge98 
    
    egg.surv=EggDailySurvival98^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS98=larv.surv98*(1-larv.dr98) 
    
    LT98=larv.surv98*larv.dr98 
    
    PS98=pup.surv98*(1-pup.dr98) 
    
    PT98=pup.surv98*pup.dr98 
    
    LarvaSurvival99=LSurRatevec * TreatmentMat99
    
    TimeToPupate99=LDevRatevec
    
    PupaSurvival99=PSurRatevec
    
    TimeToEmerge99=PDevRatevec
    
    EggDailySurvival99=EggDailySurvival
    
    larv.surv99=LarvaSurvival99^(1/TimeToPupate99) 
    
    larv.dr99=1/TimeToPupate99 
    
    pup.surv99=PupaSurvival99^(1/TimeToEmerge99) 
    
    pup.dr99=1/TimeToEmerge99 
    
    egg.surv=EggDailySurvival99^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS99=larv.surv99*(1-larv.dr99) 
    
    LT99=larv.surv99*larv.dr99 
    
    PS99=pup.surv99*(1-pup.dr99) 
    
    PT99=pup.surv99*pup.dr99 
    
    LarvaSurvival100=LSurRatevec * TreatmentMat100
    
    TimeToPupate100=LDevRatevec
    
    PupaSurvival100=PSurRatevec
    
    TimeToEmerge100=PDevRatevec
    
    EggDailySurvival100=EggDailySurvival
    
    larv.surv100=LarvaSurvival100^(1/TimeToPupate100) 
    
    larv.dr100=1/TimeToPupate100 
    
    pup.surv100=PupaSurvival100^(1/TimeToEmerge100) 
    
    pup.dr100=1/TimeToEmerge100 
    
    egg.surv=EggDailySurvival100^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS100=larv.surv100*(1-larv.dr100) 
    
    LT100=larv.surv100*larv.dr100 
    
    PS100=pup.surv100*(1-pup.dr100) 
    
    PT100=pup.surv100*pup.dr100 
    
    LarvaSurvival101=LSurRatevec * TreatmentMat101
    
    TimeToPupate101=LDevRatevec
    
    PupaSurvival101=PSurRatevec
    
    TimeToEmerge101=PDevRatevec
    
    EggDailySurvival101=EggDailySurvival
    
    larv.surv101=LarvaSurvival101^(1/TimeToPupate101) 
    
    larv.dr101=1/TimeToPupate101 
    
    pup.surv101=PupaSurvival101^(1/TimeToEmerge101) 
    
    pup.dr101=1/TimeToEmerge101 
    
    egg.surv=EggDailySurvival101^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS101=larv.surv101*(1-larv.dr101) 
    
    LT101=larv.surv101*larv.dr101 
    
    PS101=pup.surv101*(1-pup.dr101) 
    
    PT101=pup.surv101*pup.dr101 
    
    LarvaSurvival102=LSurRatevec * TreatmentMat102
    
    TimeToPupate102=LDevRatevec
    
    PupaSurvival102=PSurRatevec
    
    TimeToEmerge102=PDevRatevec
    
    EggDailySurvival102=EggDailySurvival
    
    larv.surv102=LarvaSurvival102^(1/TimeToPupate102) 
    
    larv.dr102=1/TimeToPupate102 
    
    pup.surv102=PupaSurvival102^(1/TimeToEmerge102) 
    
    pup.dr102=1/TimeToEmerge102 
    
    egg.surv=EggDailySurvival102^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS102=larv.surv102*(1-larv.dr102) 
    
    LT102=larv.surv102*larv.dr102 
    
    PS102=pup.surv102*(1-pup.dr102) 
    
    PT102=pup.surv102*pup.dr102 
    
    LarvaSurvival103=LSurRatevec * TreatmentMat103
    
    TimeToPupate103=LDevRatevec
    
    PupaSurvival103=PSurRatevec
    
    TimeToEmerge103=PDevRatevec
    
    EggDailySurvival103=EggDailySurvival
    
    larv.surv103=LarvaSurvival103^(1/TimeToPupate103) 
    
    larv.dr103=1/TimeToPupate103 
    
    pup.surv103=PupaSurvival103^(1/TimeToEmerge103) 
    
    pup.dr103=1/TimeToEmerge103 
    
    egg.surv=EggDailySurvival103^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS103=larv.surv103*(1-larv.dr103) 
    
    LT103=larv.surv103*larv.dr103 
    
    PS103=pup.surv103*(1-pup.dr103) 
    
    PT103=pup.surv103*pup.dr103 
    
    LarvaSurvival104=LSurRatevec * TreatmentMat104
    
    TimeToPupate104=LDevRatevec
    
    PupaSurvival104=PSurRatevec
    
    TimeToEmerge104=PDevRatevec
    
    EggDailySurvival104=EggDailySurvival
    
    larv.surv104=LarvaSurvival104^(1/TimeToPupate104) 
    
    larv.dr104=1/TimeToPupate104 
    
    pup.surv104=PupaSurvival104^(1/TimeToEmerge104) 
    
    pup.dr104=1/TimeToEmerge104 
    
    egg.surv=EggDailySurvival104^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS104=larv.surv104*(1-larv.dr104) 
    
    LT104=larv.surv104*larv.dr104 
    
    PS104=pup.surv104*(1-pup.dr104) 
    
    PT104=pup.surv104*pup.dr104 
    
    LarvaSurvival105=LSurRatevec * TreatmentMat105
    
    TimeToPupate105=LDevRatevec
    
    PupaSurvival105=PSurRatevec
    
    TimeToEmerge105=PDevRatevec
    
    EggDailySurvival105=EggDailySurvival
    
    larv.surv105=LarvaSurvival105^(1/TimeToPupate105) 
    
    larv.dr105=1/TimeToPupate105 
    
    pup.surv105=PupaSurvival105^(1/TimeToEmerge105) 
    
    pup.dr105=1/TimeToEmerge105 
    
    egg.surv=EggDailySurvival105^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS105=larv.surv105*(1-larv.dr105) 
    
    LT105=larv.surv105*larv.dr105 
    
    PS105=pup.surv105*(1-pup.dr105) 
    
    PT105=pup.surv105*pup.dr105 
    
    LarvaSurvival106=LSurRatevec * TreatmentMat106
    
    TimeToPupate106=LDevRatevec
    
    PupaSurvival106=PSurRatevec
    
    TimeToEmerge106=PDevRatevec
    
    EggDailySurvival106=EggDailySurvival
    
    larv.surv106=LarvaSurvival106^(1/TimeToPupate106) 
    
    larv.dr106=1/TimeToPupate106 
    
    pup.surv106=PupaSurvival106^(1/TimeToEmerge106) 
    
    pup.dr106=1/TimeToEmerge106 
    
    egg.surv=EggDailySurvival106^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS106=larv.surv106*(1-larv.dr106) 
    
    LT106=larv.surv106*larv.dr106 
    
    PS106=pup.surv106*(1-pup.dr106) 
    
    PT106=pup.surv106*pup.dr106 
    
    LarvaSurvival107=LSurRatevec * TreatmentMat107
    
    TimeToPupate107=LDevRatevec
    
    PupaSurvival107=PSurRatevec
    
    TimeToEmerge107=PDevRatevec
    
    EggDailySurvival107=EggDailySurvival
    
    larv.surv107=LarvaSurvival107^(1/TimeToPupate107) 
    
    larv.dr107=1/TimeToPupate107 
    
    pup.surv107=PupaSurvival107^(1/TimeToEmerge107) 
    
    pup.dr107=1/TimeToEmerge107 
    
    egg.surv=EggDailySurvival107^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS107=larv.surv107*(1-larv.dr107) 
    
    LT107=larv.surv107*larv.dr107 
    
    PS107=pup.surv107*(1-pup.dr107) 
    
    PT107=pup.surv107*pup.dr107 
    
    LarvaSurvival109=LSurRatevec * TreatmentMat109
    
    TimeToPupate109=LDevRatevec
    
    PupaSurvival109=PSurRatevec
    
    TimeToEmerge109=PDevRatevec
    
    EggDailySurvival109=EggDailySurvival
    
    larv.surv109=LarvaSurvival109^(1/TimeToPupate109) 
    
    larv.dr109=1/TimeToPupate109 
    
    pup.surv109=PupaSurvival109^(1/TimeToEmerge109) 
    
    pup.dr109=1/TimeToEmerge109 
    
    egg.surv=EggDailySurvival109^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS109=larv.surv109*(1-larv.dr109) 
    
    LT109=larv.surv109*larv.dr109 
    
    PS109=pup.surv109*(1-pup.dr109) 
    
    PT109=pup.surv109*pup.dr109 
    
    LarvaSurvival110=LSurRatevec * TreatmentMat110
    
    TimeToPupate110=LDevRatevec
    
    PupaSurvival110=PSurRatevec
    
    TimeToEmerge110=PDevRatevec
    
    EggDailySurvival110=EggDailySurvival
    
    larv.surv110=LarvaSurvival110^(1/TimeToPupate110) 
    
    larv.dr110=1/TimeToPupate110 
    
    pup.surv110=PupaSurvival110^(1/TimeToEmerge110) 
    
    pup.dr110=1/TimeToEmerge110 
    
    egg.surv=EggDailySurvival110^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS110=larv.surv110*(1-larv.dr110) 
    
    LT110=larv.surv110*larv.dr110 
    
    PS110=pup.surv110*(1-pup.dr110) 
    
    PT110=pup.surv110*pup.dr110 
    
    LarvaSurvival111=LSurRatevec * TreatmentMat111
    
    TimeToPupate111=LDevRatevec
    
    PupaSurvival111=PSurRatevec
    
    TimeToEmerge111=PDevRatevec
    
    EggDailySurvival111=EggDailySurvival
    
    larv.surv111=LarvaSurvival111^(1/TimeToPupate111) 
    
    larv.dr111=1/TimeToPupate111 
    
    pup.surv111=PupaSurvival111^(1/TimeToEmerge111) 
    
    pup.dr111=1/TimeToEmerge111 
    
    egg.surv=EggDailySurvival111^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS111=larv.surv111*(1-larv.dr111) 
    
    LT111=larv.surv111*larv.dr111 
    
    PS111=pup.surv111*(1-pup.dr111) 
    
    PT111=pup.surv111*pup.dr111 
    
    LarvaSurvival112=LSurRatevec * TreatmentMat112
    
    TimeToPupate112=LDevRatevec
    
    PupaSurvival112=PSurRatevec
    
    TimeToEmerge112=PDevRatevec
    
    EggDailySurvival112=EggDailySurvival
    
    larv.surv112=LarvaSurvival112^(1/TimeToPupate112) 
    
    larv.dr112=1/TimeToPupate112 
    
    pup.surv112=PupaSurvival112^(1/TimeToEmerge112) 
    
    pup.dr112=1/TimeToEmerge112 
    
    egg.surv=EggDailySurvival112^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS112=larv.surv112*(1-larv.dr112) 
    
    LT112=larv.surv112*larv.dr112 
    
    PS112=pup.surv112*(1-pup.dr112) 
    
    PT112=pup.surv112*pup.dr112 
    
    LarvaSurvival113=LSurRatevec * TreatmentMat113
    
    TimeToPupate113=LDevRatevec
    
    PupaSurvival113=PSurRatevec
    
    TimeToEmerge113=PDevRatevec
    
    EggDailySurvival113=EggDailySurvival
    
    larv.surv113=LarvaSurvival113^(1/TimeToPupate113) 
    
    larv.dr113=1/TimeToPupate113 
    
    pup.surv113=PupaSurvival113^(1/TimeToEmerge113) 
    
    pup.dr113=1/TimeToEmerge113 
    
    egg.surv=EggDailySurvival113^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS113=larv.surv113*(1-larv.dr113) 
    
    LT113=larv.surv113*larv.dr113 
    
    PS113=pup.surv113*(1-pup.dr113) 
    
    PT113=pup.surv113*pup.dr113 
    
    LarvaSurvival114=LSurRatevec * TreatmentMat114
    
    TimeToPupate114=LDevRatevec
    
    PupaSurvival114=PSurRatevec
    
    TimeToEmerge114=PDevRatevec
    
    EggDailySurvival114=EggDailySurvival
    
    larv.surv114=LarvaSurvival114^(1/TimeToPupate114) 
    
    larv.dr114=1/TimeToPupate114 
    
    pup.surv114=PupaSurvival114^(1/TimeToEmerge114) 
    
    pup.dr114=1/TimeToEmerge114 
    
    egg.surv=EggDailySurvival114^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS114=larv.surv114*(1-larv.dr114) 
    
    LT114=larv.surv114*larv.dr114 
    
    PS114=pup.surv114*(1-pup.dr114) 
    
    PT114=pup.surv114*pup.dr114 
    
    LarvaSurvival115=LSurRatevec * TreatmentMat115
    
    TimeToPupate115=LDevRatevec
    
    PupaSurvival115=PSurRatevec
    
    TimeToEmerge115=PDevRatevec
    
    EggDailySurvival115=EggDailySurvival
    
    larv.surv115=LarvaSurvival115^(1/TimeToPupate115) 
    
    larv.dr115=1/TimeToPupate115 
    
    pup.surv115=PupaSurvival115^(1/TimeToEmerge115) 
    
    pup.dr115=1/TimeToEmerge115 
    
    egg.surv=EggDailySurvival115^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS115=larv.surv115*(1-larv.dr115) 
    
    LT115=larv.surv115*larv.dr115 
    
    PS115=pup.surv115*(1-pup.dr115) 
    
    PT115=pup.surv115*pup.dr115 
    
    LarvaSurvival116=LSurRatevec * TreatmentMat116
    
    TimeToPupate116=LDevRatevec
    
    PupaSurvival116=PSurRatevec
    
    TimeToEmerge116=PDevRatevec
    
    EggDailySurvival116=EggDailySurvival
    
    larv.surv116=LarvaSurvival116^(1/TimeToPupate116) 
    
    larv.dr116=1/TimeToPupate116 
    
    pup.surv116=PupaSurvival116^(1/TimeToEmerge116) 
    
    pup.dr116=1/TimeToEmerge116 
    
    egg.surv=EggDailySurvival116^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS116=larv.surv116*(1-larv.dr116) 
    
    LT116=larv.surv116*larv.dr116 
    
    PS116=pup.surv116*(1-pup.dr116) 
    
    PT116=pup.surv116*pup.dr116 
    
    LarvaSurvival117=LSurRatevec * TreatmentMat117
    
    TimeToPupate117=LDevRatevec
    
    PupaSurvival117=PSurRatevec
    
    TimeToEmerge117=PDevRatevec
    
    EggDailySurvival117=EggDailySurvival
    
    larv.surv117=LarvaSurvival117^(1/TimeToPupate117) 
    
    larv.dr117=1/TimeToPupate117 
    
    pup.surv117=PupaSurvival117^(1/TimeToEmerge117) 
    
    pup.dr117=1/TimeToEmerge117 
    
    egg.surv=EggDailySurvival117^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS117=larv.surv117*(1-larv.dr117) 
    
    LT117=larv.surv117*larv.dr117 
    
    PS117=pup.surv117*(1-pup.dr117) 
    
    PT117=pup.surv117*pup.dr117 
    
    LarvaSurvival118=LSurRatevec * TreatmentMat118
    
    TimeToPupate118=LDevRatevec
    
    PupaSurvival118=PSurRatevec
    
    TimeToEmerge118=PDevRatevec
    
    EggDailySurvival118=EggDailySurvival
    
    larv.surv118=LarvaSurvival118^(1/TimeToPupate118) 
    
    larv.dr118=1/TimeToPupate118 
    
    pup.surv118=PupaSurvival118^(1/TimeToEmerge118) 
    
    pup.dr118=1/TimeToEmerge118 
    
    egg.surv=EggDailySurvival118^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS118=larv.surv118*(1-larv.dr118) 
    
    LT118=larv.surv118*larv.dr118 
    
    PS118=pup.surv118*(1-pup.dr118) 
    
    PT118=pup.surv118*pup.dr118 
    
    LarvaSurvival119=LSurRatevec * TreatmentMat119
    
    TimeToPupate119=LDevRatevec
    
    PupaSurvival119=PSurRatevec
    
    TimeToEmerge119=PDevRatevec
    
    EggDailySurvival119=EggDailySurvival
    
    larv.surv119=LarvaSurvival119^(1/TimeToPupate119) 
    
    larv.dr119=1/TimeToPupate119 
    
    pup.surv119=PupaSurvival119^(1/TimeToEmerge119) 
    
    pup.dr119=1/TimeToEmerge119 
    
    egg.surv=EggDailySurvival119^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS119=larv.surv119*(1-larv.dr119) 
    
    LT119=larv.surv119*larv.dr119 
    
    PS119=pup.surv119*(1-pup.dr119) 
    
    PT119=pup.surv119*pup.dr119 
    
    LarvaSurvival120=LSurRatevec * TreatmentMat120
    
    TimeToPupate120=LDevRatevec
    
    PupaSurvival120=PSurRatevec
    
    TimeToEmerge120=PDevRatevec
    
    EggDailySurvival120=EggDailySurvival
    
    larv.surv120=LarvaSurvival120^(1/TimeToPupate120) 
    
    larv.dr120=1/TimeToPupate120 
    
    pup.surv120=PupaSurvival120^(1/TimeToEmerge120) 
    
    pup.dr120=1/TimeToEmerge120 
    
    egg.surv=EggDailySurvival120^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS120=larv.surv120*(1-larv.dr120) 
    
    LT120=larv.surv120*larv.dr120 
    
    PS120=pup.surv120*(1-pup.dr120) 
    
    PT120=pup.surv120*pup.dr120 
    
    LarvaSurvival121=LSurRatevec * TreatmentMat121
    
    TimeToPupate121=LDevRatevec
    
    PupaSurvival121=PSurRatevec
    
    TimeToEmerge121=PDevRatevec
    
    EggDailySurvival121=EggDailySurvival
    
    larv.surv121=LarvaSurvival121^(1/TimeToPupate121) 
    
    larv.dr121=1/TimeToPupate121 
    
    pup.surv121=PupaSurvival121^(1/TimeToEmerge121) 
    
    pup.dr121=1/TimeToEmerge121 
    
    egg.surv=EggDailySurvival121^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS121=larv.surv121*(1-larv.dr121) 
    
    LT121=larv.surv121*larv.dr121 
    
    PS121=pup.surv121*(1-pup.dr121) 
    
    PT121=pup.surv121*pup.dr121 
    
    LarvaSurvival122=LSurRatevec * TreatmentMat122
    
    TimeToPupate122=LDevRatevec
    
    PupaSurvival122=PSurRatevec
    
    TimeToEmerge122=PDevRatevec
    
    EggDailySurvival122=EggDailySurvival
    
    larv.surv122=LarvaSurvival122^(1/TimeToPupate122) 
    
    larv.dr122=1/TimeToPupate122 
    
    pup.surv122=PupaSurvival122^(1/TimeToEmerge122) 
    
    pup.dr122=1/TimeToEmerge122 
    
    egg.surv=EggDailySurvival122^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS122=larv.surv122*(1-larv.dr122) 
    
    LT122=larv.surv122*larv.dr122 
    
    PS122=pup.surv122*(1-pup.dr122) 
    
    PT122=pup.surv122*pup.dr122 
    
    LarvaSurvival123=LSurRatevec * TreatmentMat123
    
    TimeToPupate123=LDevRatevec
    
    PupaSurvival123=PSurRatevec
    
    TimeToEmerge123=PDevRatevec
    
    EggDailySurvival123=EggDailySurvival
    
    larv.surv123=LarvaSurvival123^(1/TimeToPupate123) 
    
    larv.dr123=1/TimeToPupate123 
    
    pup.surv123=PupaSurvival123^(1/TimeToEmerge123) 
    
    pup.dr123=1/TimeToEmerge123 
    
    egg.surv=EggDailySurvival123^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS123=larv.surv123*(1-larv.dr123) 
    
    LT123=larv.surv123*larv.dr123 
    
    PS123=pup.surv123*(1-pup.dr123) 
    
    PT123=pup.surv123*pup.dr123 
    
    LarvaSurvival124=LSurRatevec * TreatmentMat124
    
    TimeToPupate124=LDevRatevec
    
    PupaSurvival124=PSurRatevec
    
    TimeToEmerge124=PDevRatevec
    
    EggDailySurvival124=EggDailySurvival
    
    larv.surv124=LarvaSurvival124^(1/TimeToPupate124) 
    
    larv.dr124=1/TimeToPupate124 
    
    pup.surv124=PupaSurvival124^(1/TimeToEmerge124) 
    
    pup.dr124=1/TimeToEmerge124 
    
    egg.surv=EggDailySurvival124^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS124=larv.surv124*(1-larv.dr124) 
    
    LT124=larv.surv124*larv.dr124 
    
    PS124=pup.surv124*(1-pup.dr124) 
    
    PT124=pup.surv124*pup.dr124 
    
    LarvaSurvival125=LSurRatevec * TreatmentMat125
    
    TimeToPupate125=LDevRatevec
    
    PupaSurvival125=PSurRatevec
    
    TimeToEmerge125=PDevRatevec
    
    EggDailySurvival125=EggDailySurvival
    
    larv.surv125=LarvaSurvival125^(1/TimeToPupate125) 
    
    larv.dr125=1/TimeToPupate125 
    
    pup.surv125=PupaSurvival125^(1/TimeToEmerge125) 
    
    pup.dr125=1/TimeToEmerge125 
    
    egg.surv=EggDailySurvival125^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS125=larv.surv125*(1-larv.dr125) 
    
    LT125=larv.surv125*larv.dr125 
    
    PS125=pup.surv125*(1-pup.dr125) 
    
    PT125=pup.surv125*pup.dr125 
    
    LarvaSurvival126=LSurRatevec * TreatmentMat126
    
    TimeToPupate126=LDevRatevec
    
    PupaSurvival126=PSurRatevec
    
    TimeToEmerge126=PDevRatevec
    
    EggDailySurvival126=EggDailySurvival
    
    larv.surv126=LarvaSurvival126^(1/TimeToPupate126) 
    
    larv.dr126=1/TimeToPupate126 
    
    pup.surv126=PupaSurvival126^(1/TimeToEmerge126) 
    
    pup.dr126=1/TimeToEmerge126 
    
    egg.surv=EggDailySurvival126^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS126=larv.surv126*(1-larv.dr126) 
    
    LT126=larv.surv126*larv.dr126 
    
    PS126=pup.surv126*(1-pup.dr126) 
    
    PT126=pup.surv126*pup.dr126 
    
    LarvaSurvival127=LSurRatevec * TreatmentMat127
    
    TimeToPupate127=LDevRatevec
    
    PupaSurvival127=PSurRatevec
    
    TimeToEmerge127=PDevRatevec
    
    EggDailySurvival127=EggDailySurvival
    
    larv.surv127=LarvaSurvival127^(1/TimeToPupate127) 
    
    larv.dr127=1/TimeToPupate127 
    
    pup.surv127=PupaSurvival127^(1/TimeToEmerge127) 
    
    pup.dr127=1/TimeToEmerge127 
    
    egg.surv=EggDailySurvival127^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS127=larv.surv127*(1-larv.dr127) 
    
    LT127=larv.surv127*larv.dr127 
    
    PS127=pup.surv127*(1-pup.dr127) 
    
    PT127=pup.surv127*pup.dr127 
    
    LarvaSurvival128=LSurRatevec * TreatmentMat128
    
    TimeToPupate128=LDevRatevec
    
    PupaSurvival128=PSurRatevec
    
    TimeToEmerge128=PDevRatevec
    
    EggDailySurvival128=EggDailySurvival
    
    larv.surv128=LarvaSurvival128^(1/TimeToPupate128) 
    
    larv.dr128=1/TimeToPupate128 
    
    pup.surv128=PupaSurvival128^(1/TimeToEmerge128) 
    
    pup.dr128=1/TimeToEmerge128 
    
    egg.surv=EggDailySurvival128^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS128=larv.surv128*(1-larv.dr128) 
    
    LT128=larv.surv128*larv.dr128 
    
    PS128=pup.surv128*(1-pup.dr128) 
    
    PT128=pup.surv128*pup.dr128 
    
    LarvaSurvival129=LSurRatevec * TreatmentMat129
    
    TimeToPupate129=LDevRatevec
    
    PupaSurvival129=PSurRatevec
    
    TimeToEmerge129=PDevRatevec
    
    EggDailySurvival129=EggDailySurvival
    
    larv.surv129=LarvaSurvival129^(1/TimeToPupate129) 
    
    larv.dr129=1/TimeToPupate129 
    
    pup.surv129=PupaSurvival129^(1/TimeToEmerge129) 
    
    pup.dr129=1/TimeToEmerge129 
    
    egg.surv=EggDailySurvival129^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS129=larv.surv129*(1-larv.dr129) 
    
    LT129=larv.surv129*larv.dr129 
    
    PS129=pup.surv129*(1-pup.dr129) 
    
    PT129=pup.surv129*pup.dr129 
    
    LarvaSurvival130=LSurRatevec * TreatmentMat130
    
    TimeToPupate130=LDevRatevec
    
    PupaSurvival130=PSurRatevec
    
    TimeToEmerge130=PDevRatevec
    
    EggDailySurvival130=EggDailySurvival
    
    larv.surv130=LarvaSurvival130^(1/TimeToPupate130) 
    
    larv.dr130=1/TimeToPupate130 
    
    pup.surv130=PupaSurvival130^(1/TimeToEmerge130) 
    
    pup.dr130=1/TimeToEmerge130 
    
    egg.surv=EggDailySurvival130^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS130=larv.surv130*(1-larv.dr130) 
    
    LT130=larv.surv130*larv.dr130 
    
    PS130=pup.surv130*(1-pup.dr130) 
    
    PT130=pup.surv130*pup.dr130 
    
    LarvaSurvival131=LSurRatevec * TreatmentMat131
    
    TimeToPupate131=LDevRatevec
    
    PupaSurvival131=PSurRatevec
    
    TimeToEmerge131=PDevRatevec
    
    EggDailySurvival131=EggDailySurvival
    
    larv.surv131=LarvaSurvival131^(1/TimeToPupate131) 
    
    larv.dr131=1/TimeToPupate131 
    
    pup.surv131=PupaSurvival131^(1/TimeToEmerge131) 
    
    pup.dr131=1/TimeToEmerge131 
    
    egg.surv=EggDailySurvival131^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS131=larv.surv131*(1-larv.dr131) 
    
    LT131=larv.surv131*larv.dr131 
    
    PS131=pup.surv131*(1-pup.dr131) 
    
    PT131=pup.surv131*pup.dr131 
    
    LarvaSurvival132=LSurRatevec * TreatmentMat132
    
    TimeToPupate132=LDevRatevec
    
    PupaSurvival132=PSurRatevec
    
    TimeToEmerge132=PDevRatevec
    
    EggDailySurvival132=EggDailySurvival
    
    larv.surv132=LarvaSurvival132^(1/TimeToPupate132) 
    
    larv.dr132=1/TimeToPupate132 
    
    pup.surv132=PupaSurvival132^(1/TimeToEmerge132) 
    
    pup.dr132=1/TimeToEmerge132 
    
    egg.surv=EggDailySurvival132^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS132=larv.surv132*(1-larv.dr132) 
    
    LT132=larv.surv132*larv.dr132 
    
    PS132=pup.surv132*(1-pup.dr132) 
    
    PT132=pup.surv132*pup.dr132 
    
    LarvaSurvival133=LSurRatevec * TreatmentMat133
    
    TimeToPupate133=LDevRatevec
    
    PupaSurvival133=PSurRatevec
    
    TimeToEmerge133=PDevRatevec
    
    EggDailySurvival133=EggDailySurvival
    
    larv.surv133=LarvaSurvival133^(1/TimeToPupate133) 
    
    larv.dr133=1/TimeToPupate133 
    
    pup.surv133=PupaSurvival133^(1/TimeToEmerge133) 
    
    pup.dr133=1/TimeToEmerge133 
    
    egg.surv=EggDailySurvival133^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS133=larv.surv133*(1-larv.dr133) 
    
    LT133=larv.surv133*larv.dr133 
    
    PS133=pup.surv133*(1-pup.dr133) 
    
    PT133=pup.surv133*pup.dr133 
    
    LarvaSurvival134=LSurRatevec * TreatmentMat134
    
    TimeToPupate134=LDevRatevec
    
    PupaSurvival134=PSurRatevec
    
    TimeToEmerge134=PDevRatevec
    
    EggDailySurvival134=EggDailySurvival
    
    larv.surv134=LarvaSurvival134^(1/TimeToPupate134) 
    
    larv.dr134=1/TimeToPupate134 
    
    pup.surv134=PupaSurvival134^(1/TimeToEmerge134) 
    
    pup.dr134=1/TimeToEmerge134 
    
    egg.surv=EggDailySurvival134^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS134=larv.surv134*(1-larv.dr134) 
    
    LT134=larv.surv134*larv.dr134 
    
    PS134=pup.surv134*(1-pup.dr134) 
    
    PT134=pup.surv134*pup.dr134 
    
    LarvaSurvival135=LSurRatevec * TreatmentMat135
    
    TimeToPupate135=LDevRatevec
    
    PupaSurvival135=PSurRatevec
    
    TimeToEmerge135=PDevRatevec
    
    EggDailySurvival135=EggDailySurvival
    
    larv.surv135=LarvaSurvival135^(1/TimeToPupate135) 
    
    larv.dr135=1/TimeToPupate135 
    
    pup.surv135=PupaSurvival135^(1/TimeToEmerge135) 
    
    pup.dr135=1/TimeToEmerge135 
    
    egg.surv=EggDailySurvival135^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS135=larv.surv135*(1-larv.dr135) 
    
    LT135=larv.surv135*larv.dr135 
    
    PS135=pup.surv135*(1-pup.dr135) 
    
    PT135=pup.surv135*pup.dr135 
    
    LarvaSurvival136=LSurRatevec * TreatmentMat136
    
    TimeToPupate136=LDevRatevec
    
    PupaSurvival136=PSurRatevec
    
    TimeToEmerge136=PDevRatevec
    
    EggDailySurvival136=EggDailySurvival
    
    larv.surv136=LarvaSurvival136^(1/TimeToPupate136) 
    
    larv.dr136=1/TimeToPupate136 
    
    pup.surv136=PupaSurvival136^(1/TimeToEmerge136) 
    
    pup.dr136=1/TimeToEmerge136 
    
    egg.surv=EggDailySurvival136^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS136=larv.surv136*(1-larv.dr136) 
    
    LT136=larv.surv136*larv.dr136 
    
    PS136=pup.surv136*(1-pup.dr136) 
    
    PT136=pup.surv136*pup.dr136 
    
    LarvaSurvival137=LSurRatevec * TreatmentMat137
    
    TimeToPupate137=LDevRatevec
    
    PupaSurvival137=PSurRatevec
    
    TimeToEmerge137=PDevRatevec
    
    EggDailySurvival137=EggDailySurvival
    
    larv.surv137=LarvaSurvival137^(1/TimeToPupate137) 
    
    larv.dr137=1/TimeToPupate137 
    
    pup.surv137=PupaSurvival137^(1/TimeToEmerge137) 
    
    pup.dr137=1/TimeToEmerge137 
    
    egg.surv=EggDailySurvival137^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS137=larv.surv137*(1-larv.dr137) 
    
    LT137=larv.surv137*larv.dr137 
    
    PS137=pup.surv137*(1-pup.dr137) 
    
    PT137=pup.surv137*pup.dr137 
    
    LarvaSurvival138=LSurRatevec * TreatmentMat138
    
    TimeToPupate138=LDevRatevec
    
    PupaSurvival138=PSurRatevec
    
    TimeToEmerge138=PDevRatevec
    
    EggDailySurvival138=EggDailySurvival
    
    larv.surv138=LarvaSurvival138^(1/TimeToPupate138) 
    
    larv.dr138=1/TimeToPupate138 
    
    pup.surv138=PupaSurvival138^(1/TimeToEmerge138) 
    
    pup.dr138=1/TimeToEmerge138 
    
    egg.surv=EggDailySurvival138^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS138=larv.surv138*(1-larv.dr138) 
    
    LT138=larv.surv138*larv.dr138 
    
    PS138=pup.surv138*(1-pup.dr138) 
    
    PT138=pup.surv138*pup.dr138 
    
    LarvaSurvival139=LSurRatevec * TreatmentMat139
    
    TimeToPupate139=LDevRatevec
    
    PupaSurvival139=PSurRatevec
    
    TimeToEmerge139=PDevRatevec
    
    EggDailySurvival139=EggDailySurvival
    
    larv.surv139=LarvaSurvival139^(1/TimeToPupate139) 
    
    larv.dr139=1/TimeToPupate139 
    
    pup.surv139=PupaSurvival139^(1/TimeToEmerge139) 
    
    pup.dr139=1/TimeToEmerge139 
    
    egg.surv=EggDailySurvival139^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS139=larv.surv139*(1-larv.dr139) 
    
    LT139=larv.surv139*larv.dr139 
    
    PS139=pup.surv139*(1-pup.dr139) 
    
    PT139=pup.surv139*pup.dr139 
    
    LarvaSurvival140=LSurRatevec * TreatmentMat140
    
    TimeToPupate140=LDevRatevec
    
    PupaSurvival140=PSurRatevec
    
    TimeToEmerge140=PDevRatevec
    
    EggDailySurvival140=EggDailySurvival
    
    larv.surv140=LarvaSurvival140^(1/TimeToPupate140) 
    
    larv.dr140=1/TimeToPupate140 
    
    pup.surv140=PupaSurvival140^(1/TimeToEmerge140) 
    
    pup.dr140=1/TimeToEmerge140 
    
    egg.surv=EggDailySurvival140^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS140=larv.surv140*(1-larv.dr140) 
    
    LT140=larv.surv140*larv.dr140 
    
    PS140=pup.surv140*(1-pup.dr140) 
    
    PT140=pup.surv140*pup.dr140 
    
    LarvaSurvival141=LSurRatevec * TreatmentMat141
    
    TimeToPupate141=LDevRatevec
    
    PupaSurvival141=PSurRatevec
    
    TimeToEmerge141=PDevRatevec
    
    EggDailySurvival141=EggDailySurvival
    
    larv.surv141=LarvaSurvival141^(1/TimeToPupate141) 
    
    larv.dr141=1/TimeToPupate141 
    
    pup.surv141=PupaSurvival141^(1/TimeToEmerge141) 
    
    pup.dr141=1/TimeToEmerge141 
    
    egg.surv=EggDailySurvival141^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS141=larv.surv141*(1-larv.dr141) 
    
    LT141=larv.surv141*larv.dr141 
    
    PS141=pup.surv141*(1-pup.dr141) 
    
    PT141=pup.surv141*pup.dr141 
    
    LarvaSurvival142=LSurRatevec * TreatmentMat142
    
    TimeToPupate142=LDevRatevec
    
    PupaSurvival142=PSurRatevec
    
    TimeToEmerge142=PDevRatevec
    
    EggDailySurvival142=EggDailySurvival
    
    larv.surv142=LarvaSurvival142^(1/TimeToPupate142) 
    
    larv.dr142=1/TimeToPupate142 
    
    pup.surv142=PupaSurvival142^(1/TimeToEmerge142) 
    
    pup.dr142=1/TimeToEmerge142 
    
    egg.surv=EggDailySurvival142^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS142=larv.surv142*(1-larv.dr142) 
    
    LT142=larv.surv142*larv.dr142 
    
    PS142=pup.surv142*(1-pup.dr142) 
    
    PT142=pup.surv142*pup.dr142 
    
    LarvaSurvival143=LSurRatevec * TreatmentMat143
    
    TimeToPupate143=LDevRatevec
    
    PupaSurvival143=PSurRatevec
    
    TimeToEmerge143=PDevRatevec
    
    EggDailySurvival143=EggDailySurvival
    
    larv.surv143=LarvaSurvival143^(1/TimeToPupate143) 
    
    larv.dr143=1/TimeToPupate143 
    
    pup.surv143=PupaSurvival143^(1/TimeToEmerge143) 
    
    pup.dr143=1/TimeToEmerge143 
    
    egg.surv=EggDailySurvival143^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS143=larv.surv143*(1-larv.dr143) 
    
    LT143=larv.surv143*larv.dr143 
    
    PS143=pup.surv143*(1-pup.dr143) 
    
    PT143=pup.surv143*pup.dr143 
    
    LarvaSurvival144=LSurRatevec * TreatmentMat144
    
    TimeToPupate144=LDevRatevec
    
    PupaSurvival144=PSurRatevec
    
    TimeToEmerge144=PDevRatevec
    
    EggDailySurvival144=EggDailySurvival
    
    larv.surv144=LarvaSurvival144^(1/TimeToPupate144) 
    
    larv.dr144=1/TimeToPupate144 
    
    pup.surv144=PupaSurvival144^(1/TimeToEmerge144) 
    
    pup.dr144=1/TimeToEmerge144 
    
    egg.surv=EggDailySurvival144^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS144=larv.surv144*(1-larv.dr144) 
    
    LT144=larv.surv144*larv.dr144 
    
    PS144=pup.surv144*(1-pup.dr144) 
    
    PT144=pup.surv144*pup.dr144 
    
    LarvaSurvival145=LSurRatevec * TreatmentMat145
    
    TimeToPupate145=LDevRatevec
    
    PupaSurvival145=PSurRatevec
    
    TimeToEmerge145=PDevRatevec
    
    EggDailySurvival145=EggDailySurvival
    
    larv.surv145=LarvaSurvival145^(1/TimeToPupate145) 
    
    larv.dr145=1/TimeToPupate145 
    
    pup.surv145=PupaSurvival145^(1/TimeToEmerge145) 
    
    pup.dr145=1/TimeToEmerge145 
    
    egg.surv=EggDailySurvival145^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS145=larv.surv145*(1-larv.dr145) 
    
    LT145=larv.surv145*larv.dr145 
    
    PS145=pup.surv145*(1-pup.dr145) 
    
    PT145=pup.surv145*pup.dr145 
    
    LarvaSurvival146=LSurRatevec * TreatmentMat146
    
    TimeToPupate146=LDevRatevec
    
    PupaSurvival146=PSurRatevec
    
    TimeToEmerge146=PDevRatevec
    
    EggDailySurvival146=EggDailySurvival
    
    larv.surv146=LarvaSurvival146^(1/TimeToPupate146) 
    
    larv.dr146=1/TimeToPupate146 
    
    pup.surv146=PupaSurvival146^(1/TimeToEmerge146) 
    
    pup.dr146=1/TimeToEmerge146 
    
    egg.surv=EggDailySurvival146^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS146=larv.surv146*(1-larv.dr146) 
    
    LT146=larv.surv146*larv.dr146 
    
    PS146=pup.surv146*(1-pup.dr146) 
    
    PT146=pup.surv146*pup.dr146 
    
    LarvaSurvival147=LSurRatevec * TreatmentMat147
    
    TimeToPupate147=LDevRatevec
    
    PupaSurvival147=PSurRatevec
    
    TimeToEmerge147=PDevRatevec
    
    EggDailySurvival147=EggDailySurvival
    
    larv.surv147=LarvaSurvival147^(1/TimeToPupate147) 
    
    larv.dr147=1/TimeToPupate147 
    
    pup.surv147=PupaSurvival147^(1/TimeToEmerge147) 
    
    pup.dr147=1/TimeToEmerge147 
    
    egg.surv=EggDailySurvival147^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS147=larv.surv147*(1-larv.dr147) 
    
    LT147=larv.surv147*larv.dr147 
    
    PS147=pup.surv147*(1-pup.dr147) 
    
    PT147=pup.surv147*pup.dr147 
    
    LarvaSurvival148=LSurRatevec * TreatmentMat148
    
    TimeToPupate148=LDevRatevec
    
    PupaSurvival148=PSurRatevec
    
    TimeToEmerge148=PDevRatevec
    
    EggDailySurvival148=EggDailySurvival
    
    larv.surv148=LarvaSurvival148^(1/TimeToPupate148) 
    
    larv.dr148=1/TimeToPupate148 
    
    pup.surv148=PupaSurvival148^(1/TimeToEmerge148) 
    
    pup.dr148=1/TimeToEmerge148 
    
    egg.surv=EggDailySurvival148^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS148=larv.surv148*(1-larv.dr148) 
    
    LT148=larv.surv148*larv.dr148 
    
    PS148=pup.surv148*(1-pup.dr148) 
    
    PT148=pup.surv148*pup.dr148 
    
    LarvaSurvival149=LSurRatevec * TreatmentMat149
    
    TimeToPupate149=LDevRatevec
    
    PupaSurvival149=PSurRatevec
    
    TimeToEmerge149=PDevRatevec
    
    EggDailySurvival149=EggDailySurvival
    
    larv.surv149=LarvaSurvival149^(1/TimeToPupate149) 
    
    larv.dr149=1/TimeToPupate149 
    
    pup.surv149=PupaSurvival149^(1/TimeToEmerge149) 
    
    pup.dr149=1/TimeToEmerge149 
    
    egg.surv=EggDailySurvival149^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS149=larv.surv149*(1-larv.dr149) 
    
    LT149=larv.surv149*larv.dr149 
    
    PS149=pup.surv149*(1-pup.dr149) 
    
    PT149=pup.surv149*pup.dr149 
    
    LarvaSurvival150=LSurRatevec * TreatmentMat150
    
    TimeToPupate150=LDevRatevec
    
    PupaSurvival150=PSurRatevec
    
    TimeToEmerge150=PDevRatevec
    
    EggDailySurvival150=EggDailySurvival
    
    larv.surv150=LarvaSurvival150^(1/TimeToPupate150) 
    
    larv.dr150=1/TimeToPupate150 
    
    pup.surv150=PupaSurvival150^(1/TimeToEmerge150) 
    
    pup.dr150=1/TimeToEmerge150 
    
    egg.surv=EggDailySurvival150^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS150=larv.surv150*(1-larv.dr150) 
    
    LT150=larv.surv150*larv.dr150 
    
    PS150=pup.surv150*(1-pup.dr150) 
    
    PT150=pup.surv150*pup.dr150 
    
    LarvaSurvival151=LSurRatevec * TreatmentMat151
    
    TimeToPupate151=LDevRatevec
    
    PupaSurvival151=PSurRatevec
    
    TimeToEmerge151=PDevRatevec
    
    EggDailySurvival151=EggDailySurvival
    
    larv.surv151=LarvaSurvival151^(1/TimeToPupate151) 
    
    larv.dr151=1/TimeToPupate151 
    
    pup.surv151=PupaSurvival151^(1/TimeToEmerge151) 
    
    pup.dr151=1/TimeToEmerge151 
    
    egg.surv=EggDailySurvival151^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS151=larv.surv151*(1-larv.dr151) 
    
    LT151=larv.surv151*larv.dr151 
    
    PS151=pup.surv151*(1-pup.dr151) 
    
    PT151=pup.surv151*pup.dr151 
    
    LarvaSurvival152=LSurRatevec * TreatmentMat152
    
    TimeToPupate152=LDevRatevec
    
    PupaSurvival152=PSurRatevec
    
    TimeToEmerge152=PDevRatevec
    
    EggDailySurvival152=EggDailySurvival
    
    larv.surv152=LarvaSurvival152^(1/TimeToPupate152) 
    
    larv.dr152=1/TimeToPupate152 
    
    pup.surv152=PupaSurvival152^(1/TimeToEmerge152) 
    
    pup.dr152=1/TimeToEmerge152 
    
    egg.surv=EggDailySurvival152^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS152=larv.surv152*(1-larv.dr152) 
    
    LT152=larv.surv152*larv.dr152 
    
    PS152=pup.surv152*(1-pup.dr152) 
    
    PT152=pup.surv152*pup.dr152 
    
    LarvaSurvival153=LSurRatevec * TreatmentMat153
    
    TimeToPupate153=LDevRatevec
    
    PupaSurvival153=PSurRatevec
    
    TimeToEmerge153=PDevRatevec
    
    EggDailySurvival153=EggDailySurvival
    
    larv.surv153=LarvaSurvival153^(1/TimeToPupate153) 
    
    larv.dr153=1/TimeToPupate153 
    
    pup.surv153=PupaSurvival153^(1/TimeToEmerge153) 
    
    pup.dr153=1/TimeToEmerge153 
    
    egg.surv=EggDailySurvival153^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS153=larv.surv153*(1-larv.dr153) 
    
    LT153=larv.surv153*larv.dr153 
    
    PS153=pup.surv153*(1-pup.dr153) 
    
    PT153=pup.surv153*pup.dr153 
    
    LarvaSurvival154=LSurRatevec * TreatmentMat154
    
    TimeToPupate154=LDevRatevec
    
    PupaSurvival154=PSurRatevec
    
    TimeToEmerge154=PDevRatevec
    
    EggDailySurvival154=EggDailySurvival
    
    larv.surv154=LarvaSurvival154^(1/TimeToPupate154) 
    
    larv.dr154=1/TimeToPupate154 
    
    pup.surv154=PupaSurvival154^(1/TimeToEmerge154) 
    
    pup.dr154=1/TimeToEmerge154 
    
    egg.surv=EggDailySurvival154^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS154=larv.surv154*(1-larv.dr154) 
    
    LT154=larv.surv154*larv.dr154 
    
    PS154=pup.surv154*(1-pup.dr154) 
    
    PT154=pup.surv154*pup.dr154 
    
    LarvaSurvival155=LSurRatevec * TreatmentMat155
    
    TimeToPupate155=LDevRatevec
    
    PupaSurvival155=PSurRatevec
    
    TimeToEmerge155=PDevRatevec
    
    EggDailySurvival155=EggDailySurvival
    
    larv.surv155=LarvaSurvival155^(1/TimeToPupate155) 
    
    larv.dr155=1/TimeToPupate155 
    
    pup.surv155=PupaSurvival155^(1/TimeToEmerge155) 
    
    pup.dr155=1/TimeToEmerge155 
    
    egg.surv=EggDailySurvival155^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS155=larv.surv155*(1-larv.dr155) 
    
    LT155=larv.surv155*larv.dr155 
    
    PS155=pup.surv155*(1-pup.dr155) 
    
    PT155=pup.surv155*pup.dr155 
    
    LarvaSurvival156=LSurRatevec * TreatmentMat156
    
    TimeToPupate156=LDevRatevec
    
    PupaSurvival156=PSurRatevec
    
    TimeToEmerge156=PDevRatevec
    
    EggDailySurvival156=EggDailySurvival
    
    larv.surv156=LarvaSurvival156^(1/TimeToPupate156) 
    
    larv.dr156=1/TimeToPupate156 
    
    pup.surv156=PupaSurvival156^(1/TimeToEmerge156) 
    
    pup.dr156=1/TimeToEmerge156 
    
    egg.surv=EggDailySurvival156^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS156=larv.surv156*(1-larv.dr156) 
    
    LT156=larv.surv156*larv.dr156 
    
    PS156=pup.surv156*(1-pup.dr156) 
    
    PT156=pup.surv156*pup.dr156 
    
    LarvaSurvival157=LSurRatevec * TreatmentMat157
    
    TimeToPupate157=LDevRatevec
    
    PupaSurvival157=PSurRatevec
    
    TimeToEmerge157=PDevRatevec
    
    EggDailySurvival157=EggDailySurvival
    
    larv.surv157=LarvaSurvival157^(1/TimeToPupate157) 
    
    larv.dr157=1/TimeToPupate157 
    
    pup.surv157=PupaSurvival157^(1/TimeToEmerge157) 
    
    pup.dr157=1/TimeToEmerge157 
    
    egg.surv=EggDailySurvival157^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS157=larv.surv157*(1-larv.dr157) 
    
    LT157=larv.surv157*larv.dr157 
    
    PS157=pup.surv157*(1-pup.dr157) 
    
    PT157=pup.surv157*pup.dr157 
    
    LarvaSurvival158=LSurRatevec * TreatmentMat158
    
    TimeToPupate158=LDevRatevec
    
    PupaSurvival158=PSurRatevec
    
    TimeToEmerge158=PDevRatevec
    
    EggDailySurvival158=EggDailySurvival
    
    larv.surv158=LarvaSurvival158^(1/TimeToPupate158) 
    
    larv.dr158=1/TimeToPupate158 
    
    pup.surv158=PupaSurvival158^(1/TimeToEmerge158) 
    
    pup.dr158=1/TimeToEmerge158 
    
    egg.surv=EggDailySurvival158^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS158=larv.surv158*(1-larv.dr158) 
    
    LT158=larv.surv158*larv.dr158 
    
    PS158=pup.surv158*(1-pup.dr158) 
    
    PT158=pup.surv158*pup.dr158 
    
    LarvaSurvival159=LSurRatevec * TreatmentMat159
    
    TimeToPupate159=LDevRatevec
    
    PupaSurvival159=PSurRatevec
    
    TimeToEmerge159=PDevRatevec
    
    EggDailySurvival159=EggDailySurvival
    
    larv.surv159=LarvaSurvival159^(1/TimeToPupate159) 
    
    larv.dr159=1/TimeToPupate159 
    
    pup.surv159=PupaSurvival159^(1/TimeToEmerge159) 
    
    pup.dr159=1/TimeToEmerge159 
    
    egg.surv=EggDailySurvival159^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS159=larv.surv159*(1-larv.dr159) 
    
    LT159=larv.surv159*larv.dr159 
    
    PS159=pup.surv159*(1-pup.dr159) 
    
    PT159=pup.surv159*pup.dr159 
    
    LarvaSurvival160=LSurRatevec * TreatmentMat160
    
    TimeToPupate160=LDevRatevec
    
    PupaSurvival160=PSurRatevec
    
    TimeToEmerge160=PDevRatevec
    
    EggDailySurvival160=EggDailySurvival
    
    larv.surv160=LarvaSurvival160^(1/TimeToPupate160) 
    
    larv.dr160=1/TimeToPupate160 
    
    pup.surv160=PupaSurvival160^(1/TimeToEmerge160) 
    
    pup.dr160=1/TimeToEmerge160 
    
    egg.surv=EggDailySurvival160^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS160=larv.surv160*(1-larv.dr160) 
    
    LT160=larv.surv160*larv.dr160 
    
    PS160=pup.surv160*(1-pup.dr160) 
    
    PT160=pup.surv160*pup.dr160 
    
    LarvaSurvival161=LSurRatevec * TreatmentMat161
    
    TimeToPupate161=LDevRatevec
    
    PupaSurvival161=PSurRatevec
    
    TimeToEmerge161=PDevRatevec
    
    EggDailySurvival161=EggDailySurvival
    
    larv.surv161=LarvaSurvival161^(1/TimeToPupate161) 
    
    larv.dr161=1/TimeToPupate161 
    
    pup.surv161=PupaSurvival161^(1/TimeToEmerge161) 
    
    pup.dr161=1/TimeToEmerge161 
    
    egg.surv=EggDailySurvival161^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS161=larv.surv161*(1-larv.dr161) 
    
    LT161=larv.surv161*larv.dr161 
    
    PS161=pup.surv161*(1-pup.dr161) 
    
    PT161=pup.surv161*pup.dr161 
    
    LarvaSurvival162=LSurRatevec * TreatmentMat162
    
    TimeToPupate162=LDevRatevec
    
    PupaSurvival162=PSurRatevec
    
    TimeToEmerge162=PDevRatevec
    
    EggDailySurvival162=EggDailySurvival
    
    larv.surv162=LarvaSurvival162^(1/TimeToPupate162) 
    
    larv.dr162=1/TimeToPupate162 
    
    pup.surv162=PupaSurvival162^(1/TimeToEmerge162) 
    
    pup.dr162=1/TimeToEmerge162 
    
    egg.surv=EggDailySurvival162^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS162=larv.surv162*(1-larv.dr162) 
    
    LT162=larv.surv162*larv.dr162 
    
    PS162=pup.surv162*(1-pup.dr162) 
    
    PT162=pup.surv162*pup.dr162 
    
    LarvaSurvival163=LSurRatevec * TreatmentMat163
    
    TimeToPupate163=LDevRatevec
    
    PupaSurvival163=PSurRatevec
    
    TimeToEmerge163=PDevRatevec
    
    EggDailySurvival163=EggDailySurvival
    
    larv.surv163=LarvaSurvival163^(1/TimeToPupate163) 
    
    larv.dr163=1/TimeToPupate163 
    
    pup.surv163=PupaSurvival163^(1/TimeToEmerge163) 
    
    pup.dr163=1/TimeToEmerge163 
    
    egg.surv=EggDailySurvival163^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS163=larv.surv163*(1-larv.dr163) 
    
    LT163=larv.surv163*larv.dr163 
    
    PS163=pup.surv163*(1-pup.dr163) 
    
    PT163=pup.surv163*pup.dr163 
    
    LarvaSurvival164=LSurRatevec * TreatmentMat164
    
    TimeToPupate164=LDevRatevec
    
    PupaSurvival164=PSurRatevec
    
    TimeToEmerge164=PDevRatevec
    
    EggDailySurvival164=EggDailySurvival
    
    larv.surv164=LarvaSurvival164^(1/TimeToPupate164) 
    
    larv.dr164=1/TimeToPupate164 
    
    pup.surv164=PupaSurvival164^(1/TimeToEmerge164) 
    
    pup.dr164=1/TimeToEmerge164 
    
    egg.surv=EggDailySurvival164^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS164=larv.surv164*(1-larv.dr164) 
    
    LT164=larv.surv164*larv.dr164 
    
    PS164=pup.surv164*(1-pup.dr164) 
    
    PT164=pup.surv164*pup.dr164 
    
    LarvaSurvival165=LSurRatevec * TreatmentMat165
    
    TimeToPupate165=LDevRatevec
    
    PupaSurvival165=PSurRatevec
    
    TimeToEmerge165=PDevRatevec
    
    EggDailySurvival165=EggDailySurvival
    
    larv.surv165=LarvaSurvival165^(1/TimeToPupate165) 
    
    larv.dr165=1/TimeToPupate165 
    
    pup.surv165=PupaSurvival165^(1/TimeToEmerge165) 
    
    pup.dr165=1/TimeToEmerge165 
    
    egg.surv=EggDailySurvival165^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS165=larv.surv165*(1-larv.dr165) 
    
    LT165=larv.surv165*larv.dr165 
    
    PS165=pup.surv165*(1-pup.dr165) 
    
    PT165=pup.surv165*pup.dr165 
    
    LarvaSurvival166=LSurRatevec * TreatmentMat166
    
    TimeToPupate166=LDevRatevec
    
    PupaSurvival166=PSurRatevec
    
    TimeToEmerge166=PDevRatevec
    
    EggDailySurvival166=EggDailySurvival
    
    larv.surv166=LarvaSurvival166^(1/TimeToPupate166) 
    
    larv.dr166=1/TimeToPupate166 
    
    pup.surv166=PupaSurvival166^(1/TimeToEmerge166) 
    
    pup.dr166=1/TimeToEmerge166 
    
    egg.surv=EggDailySurvival166^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS166=larv.surv166*(1-larv.dr166) 
    
    LT166=larv.surv166*larv.dr166 
    
    PS166=pup.surv166*(1-pup.dr166) 
    
    PT166=pup.surv166*pup.dr166 
    
    LarvaSurvival167=LSurRatevec * TreatmentMat167
    
    TimeToPupate167=LDevRatevec
    
    PupaSurvival167=PSurRatevec
    
    TimeToEmerge167=PDevRatevec
    
    EggDailySurvival167=EggDailySurvival
    
    larv.surv167=LarvaSurvival167^(1/TimeToPupate167) 
    
    larv.dr167=1/TimeToPupate167 
    
    pup.surv167=PupaSurvival167^(1/TimeToEmerge167) 
    
    pup.dr167=1/TimeToEmerge167 
    
    egg.surv=EggDailySurvival167^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS167=larv.surv167*(1-larv.dr167) 
    
    LT167=larv.surv167*larv.dr167 
    
    PS167=pup.surv167*(1-pup.dr167) 
    
    PT167=pup.surv167*pup.dr167 
    
    LarvaSurvival168=LSurRatevec * TreatmentMat168
    
    TimeToPupate168=LDevRatevec
    
    PupaSurvival168=PSurRatevec
    
    TimeToEmerge168=PDevRatevec
    
    EggDailySurvival168=EggDailySurvival
    
    larv.surv168=LarvaSurvival168^(1/TimeToPupate168) 
    
    larv.dr168=1/TimeToPupate168 
    
    pup.surv168=PupaSurvival168^(1/TimeToEmerge168) 
    
    pup.dr168=1/TimeToEmerge168 
    
    egg.surv=EggDailySurvival168^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS168=larv.surv168*(1-larv.dr168) 
    
    LT168=larv.surv168*larv.dr168 
    
    PS168=pup.surv168*(1-pup.dr168) 
    
    PT168=pup.surv168*pup.dr168 
    
    LarvaSurvival169=LSurRatevec * TreatmentMat169
    
    TimeToPupate169=LDevRatevec
    
    PupaSurvival169=PSurRatevec
    
    TimeToEmerge169=PDevRatevec
    
    EggDailySurvival169=EggDailySurvival
    
    larv.surv169=LarvaSurvival169^(1/TimeToPupate169) 
    
    larv.dr169=1/TimeToPupate169 
    
    pup.surv169=PupaSurvival169^(1/TimeToEmerge169) 
    
    pup.dr169=1/TimeToEmerge169 
    
    egg.surv=EggDailySurvival169^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS169=larv.surv169*(1-larv.dr169) 
    
    LT169=larv.surv169*larv.dr169 
    
    PS169=pup.surv169*(1-pup.dr169) 
    
    PT169=pup.surv169*pup.dr169 
    
    LarvaSurvival170=LSurRatevec * TreatmentMat170
    
    TimeToPupate170=LDevRatevec
    
    PupaSurvival170=PSurRatevec
    
    TimeToEmerge170=PDevRatevec
    
    EggDailySurvival170=EggDailySurvival
    
    larv.surv170=LarvaSurvival170^(1/TimeToPupate170) 
    
    larv.dr170=1/TimeToPupate170 
    
    pup.surv170=PupaSurvival170^(1/TimeToEmerge170) 
    
    pup.dr170=1/TimeToEmerge170 
    
    egg.surv=EggDailySurvival170^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS170=larv.surv170*(1-larv.dr170) 
    
    LT170=larv.surv170*larv.dr170 
    
    PS170=pup.surv170*(1-pup.dr170) 
    
    PT170=pup.surv170*pup.dr170 
    
    LarvaSurvival171=LSurRatevec * TreatmentMat171
    
    TimeToPupate171=LDevRatevec
    
    PupaSurvival171=PSurRatevec
    
    TimeToEmerge171=PDevRatevec
    
    EggDailySurvival171=EggDailySurvival
    
    larv.surv171=LarvaSurvival171^(1/TimeToPupate171) 
    
    larv.dr171=1/TimeToPupate171 
    
    pup.surv171=PupaSurvival171^(1/TimeToEmerge171) 
    
    pup.dr171=1/TimeToEmerge171 
    
    egg.surv=EggDailySurvival171^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS171=larv.surv171*(1-larv.dr171) 
    
    LT171=larv.surv171*larv.dr171 
    
    PS171=pup.surv171*(1-pup.dr171) 
    
    PT171=pup.surv171*pup.dr171 
    
    LarvaSurvival172=LSurRatevec * TreatmentMat172
    
    TimeToPupate172=LDevRatevec
    
    PupaSurvival172=PSurRatevec
    
    TimeToEmerge172=PDevRatevec
    
    EggDailySurvival172=EggDailySurvival
    
    larv.surv172=LarvaSurvival172^(1/TimeToPupate172) 
    
    larv.dr172=1/TimeToPupate172 
    
    pup.surv172=PupaSurvival172^(1/TimeToEmerge172) 
    
    pup.dr172=1/TimeToEmerge172 
    
    egg.surv=EggDailySurvival172^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS172=larv.surv172*(1-larv.dr172) 
    
    LT172=larv.surv172*larv.dr172 
    
    PS172=pup.surv172*(1-pup.dr172) 
    
    PT172=pup.surv172*pup.dr172 
    
    LarvaSurvival173=LSurRatevec * TreatmentMat173
    
    TimeToPupate173=LDevRatevec
    
    PupaSurvival173=PSurRatevec
    
    TimeToEmerge173=PDevRatevec
    
    EggDailySurvival173=EggDailySurvival
    
    larv.surv173=LarvaSurvival173^(1/TimeToPupate173) 
    
    larv.dr173=1/TimeToPupate173 
    
    pup.surv173=PupaSurvival173^(1/TimeToEmerge173) 
    
    pup.dr173=1/TimeToEmerge173 
    
    egg.surv=EggDailySurvival173^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS173=larv.surv173*(1-larv.dr173) 
    
    LT173=larv.surv173*larv.dr173 
    
    PS173=pup.surv173*(1-pup.dr173) 
    
    PT173=pup.surv173*pup.dr173 
    
    LarvaSurvival174=LSurRatevec * TreatmentMat174
    
    TimeToPupate174=LDevRatevec
    
    PupaSurvival174=PSurRatevec
    
    TimeToEmerge174=PDevRatevec
    
    EggDailySurvival174=EggDailySurvival
    
    larv.surv174=LarvaSurvival174^(1/TimeToPupate174) 
    
    larv.dr174=1/TimeToPupate174 
    
    pup.surv174=PupaSurvival174^(1/TimeToEmerge174) 
    
    pup.dr174=1/TimeToEmerge174 
    
    egg.surv=EggDailySurvival174^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS174=larv.surv174*(1-larv.dr174) 
    
    LT174=larv.surv174*larv.dr174 
    
    PS174=pup.surv174*(1-pup.dr174) 
    
    PT174=pup.surv174*pup.dr174 
    
    LarvaSurvival175=LSurRatevec * TreatmentMat175
    
    TimeToPupate175=LDevRatevec
    
    PupaSurvival175=PSurRatevec
    
    TimeToEmerge175=PDevRatevec
    
    EggDailySurvival175=EggDailySurvival
    
    larv.surv175=LarvaSurvival175^(1/TimeToPupate175) 
    
    larv.dr175=1/TimeToPupate175 
    
    pup.surv175=PupaSurvival175^(1/TimeToEmerge175) 
    
    pup.dr175=1/TimeToEmerge175 
    
    egg.surv=EggDailySurvival175^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS175=larv.surv175*(1-larv.dr175) 
    
    LT175=larv.surv175*larv.dr175 
    
    PS175=pup.surv175*(1-pup.dr175) 
    
    PT175=pup.surv175*pup.dr175 
    
    LarvaSurvival176=LSurRatevec * TreatmentMat176
    
    TimeToPupate176=LDevRatevec
    
    PupaSurvival176=PSurRatevec
    
    TimeToEmerge176=PDevRatevec
    
    EggDailySurvival176=EggDailySurvival
    
    larv.surv176=LarvaSurvival176^(1/TimeToPupate176) 
    
    larv.dr176=1/TimeToPupate176 
    
    pup.surv176=PupaSurvival176^(1/TimeToEmerge176) 
    
    pup.dr176=1/TimeToEmerge176 
    
    egg.surv=EggDailySurvival176^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS176=larv.surv176*(1-larv.dr176) 
    
    LT176=larv.surv176*larv.dr176 
    
    PS176=pup.surv176*(1-pup.dr176) 
    
    PT176=pup.surv176*pup.dr176 
    
    LarvaSurvival177=LSurRatevec * TreatmentMat177
    
    TimeToPupate177=LDevRatevec
    
    PupaSurvival177=PSurRatevec
    
    TimeToEmerge177=PDevRatevec
    
    EggDailySurvival177=EggDailySurvival
    
    larv.surv177=LarvaSurvival177^(1/TimeToPupate177) 
    
    larv.dr177=1/TimeToPupate177 
    
    pup.surv177=PupaSurvival177^(1/TimeToEmerge177) 
    
    pup.dr177=1/TimeToEmerge177 
    
    egg.surv=EggDailySurvival177^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS177=larv.surv177*(1-larv.dr177) 
    
    LT177=larv.surv177*larv.dr177 
    
    PS177=pup.surv177*(1-pup.dr177) 
    
    PT177=pup.surv177*pup.dr177 
    
    LarvaSurvival178=LSurRatevec * TreatmentMat178
    
    TimeToPupate178=LDevRatevec
    
    PupaSurvival178=PSurRatevec
    
    TimeToEmerge178=PDevRatevec
    
    EggDailySurvival178=EggDailySurvival
    
    larv.surv178=LarvaSurvival178^(1/TimeToPupate178) 
    
    larv.dr178=1/TimeToPupate178 
    
    pup.surv178=PupaSurvival178^(1/TimeToEmerge178) 
    
    pup.dr178=1/TimeToEmerge178 
    
    egg.surv=EggDailySurvival178^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS178=larv.surv178*(1-larv.dr178) 
    
    LT178=larv.surv178*larv.dr178 
    
    PS178=pup.surv178*(1-pup.dr178) 
    
    PT178=pup.surv178*pup.dr178 
    
    LarvaSurvival179=LSurRatevec * TreatmentMat179
    
    TimeToPupate179=LDevRatevec
    
    PupaSurvival179=PSurRatevec
    
    TimeToEmerge179=PDevRatevec
    
    EggDailySurvival179=EggDailySurvival
    
    larv.surv179=LarvaSurvival179^(1/TimeToPupate179) 
    
    larv.dr179=1/TimeToPupate179 
    
    pup.surv179=PupaSurvival179^(1/TimeToEmerge179) 
    
    pup.dr179=1/TimeToEmerge179 
    
    egg.surv=EggDailySurvival179^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS179=larv.surv179*(1-larv.dr179) 
    
    LT179=larv.surv179*larv.dr179 
    
    PS179=pup.surv179*(1-pup.dr179) 
    
    PT179=pup.surv179*pup.dr179 
    
    LarvaSurvival180=LSurRatevec * TreatmentMat180
    
    TimeToPupate180=LDevRatevec
    
    PupaSurvival180=PSurRatevec
    
    TimeToEmerge180=PDevRatevec
    
    EggDailySurvival180=EggDailySurvival
    
    larv.surv180=LarvaSurvival180^(1/TimeToPupate180) 
    
    larv.dr180=1/TimeToPupate180 
    
    pup.surv180=PupaSurvival180^(1/TimeToEmerge180) 
    
    pup.dr180=1/TimeToEmerge180 
    
    egg.surv=EggDailySurvival180^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS180=larv.surv180*(1-larv.dr180) 
    
    LT180=larv.surv180*larv.dr180 
    
    PS180=pup.surv180*(1-pup.dr180) 
    
    PT180=pup.surv180*pup.dr180 
    
    LarvaSurvival181=LSurRatevec * TreatmentMat181
    
    TimeToPupate181=LDevRatevec
    
    PupaSurvival181=PSurRatevec
    
    TimeToEmerge181=PDevRatevec
    
    EggDailySurvival181=EggDailySurvival
    
    larv.surv181=LarvaSurvival181^(1/TimeToPupate181) 
    
    larv.dr181=1/TimeToPupate181 
    
    pup.surv181=PupaSurvival181^(1/TimeToEmerge181) 
    
    pup.dr181=1/TimeToEmerge181 
    
    egg.surv=EggDailySurvival181^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS181=larv.surv181*(1-larv.dr181) 
    
    LT181=larv.surv181*larv.dr181 
    
    PS181=pup.surv181*(1-pup.dr181) 
    
    PT181=pup.surv181*pup.dr181 
    
    LarvaSurvival182=LSurRatevec * TreatmentMat182
    
    TimeToPupate182=LDevRatevec
    
    PupaSurvival182=PSurRatevec
    
    TimeToEmerge182=PDevRatevec
    
    EggDailySurvival182=EggDailySurvival
    
    larv.surv182=LarvaSurvival182^(1/TimeToPupate182) 
    
    larv.dr182=1/TimeToPupate182 
    
    pup.surv182=PupaSurvival182^(1/TimeToEmerge182) 
    
    pup.dr182=1/TimeToEmerge182 
    
    egg.surv=EggDailySurvival182^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS182=larv.surv182*(1-larv.dr182) 
    
    LT182=larv.surv182*larv.dr182 
    
    PS182=pup.surv182*(1-pup.dr182) 
    
    PT182=pup.surv182*pup.dr182 
    
    LarvaSurvival183=LSurRatevec * TreatmentMat183
    
    TimeToPupate183=LDevRatevec
    
    PupaSurvival183=PSurRatevec
    
    TimeToEmerge183=PDevRatevec
    
    EggDailySurvival183=EggDailySurvival
    
    larv.surv183=LarvaSurvival183^(1/TimeToPupate183) 
    
    larv.dr183=1/TimeToPupate183 
    
    pup.surv183=PupaSurvival183^(1/TimeToEmerge183) 
    
    pup.dr183=1/TimeToEmerge183 
    
    egg.surv=EggDailySurvival183^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS183=larv.surv183*(1-larv.dr183) 
    
    LT183=larv.surv183*larv.dr183 
    
    PS183=pup.surv183*(1-pup.dr183) 
    
    PT183=pup.surv183*pup.dr183 
    
    LarvaSurvival184=LSurRatevec * TreatmentMat184
    
    TimeToPupate184=LDevRatevec
    
    PupaSurvival184=PSurRatevec
    
    TimeToEmerge184=PDevRatevec
    
    EggDailySurvival184=EggDailySurvival
    
    larv.surv184=LarvaSurvival184^(1/TimeToPupate184) 
    
    larv.dr184=1/TimeToPupate184 
    
    pup.surv184=PupaSurvival184^(1/TimeToEmerge184) 
    
    pup.dr184=1/TimeToEmerge184 
    
    egg.surv=EggDailySurvival184^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS184=larv.surv184*(1-larv.dr184) 
    
    LT184=larv.surv184*larv.dr184 
    
    PS184=pup.surv184*(1-pup.dr184) 
    
    PT184=pup.surv184*pup.dr184 
    
    LarvaSurvival185=LSurRatevec * TreatmentMat185
    
    TimeToPupate185=LDevRatevec
    
    PupaSurvival185=PSurRatevec
    
    TimeToEmerge185=PDevRatevec
    
    EggDailySurvival185=EggDailySurvival
    
    larv.surv185=LarvaSurvival185^(1/TimeToPupate185) 
    
    larv.dr185=1/TimeToPupate185 
    
    pup.surv185=PupaSurvival185^(1/TimeToEmerge185) 
    
    pup.dr185=1/TimeToEmerge185 
    
    egg.surv=EggDailySurvival185^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS185=larv.surv185*(1-larv.dr185) 
    
    LT185=larv.surv185*larv.dr185 
    
    PS185=pup.surv185*(1-pup.dr185) 
    
    PT185=pup.surv185*pup.dr185 
    
    LarvaSurvival186=LSurRatevec * TreatmentMat186
    
    TimeToPupate186=LDevRatevec
    
    PupaSurvival186=PSurRatevec
    
    TimeToEmerge186=PDevRatevec
    
    EggDailySurvival186=EggDailySurvival
    
    larv.surv186=LarvaSurvival186^(1/TimeToPupate186) 
    
    larv.dr186=1/TimeToPupate186 
    
    pup.surv186=PupaSurvival186^(1/TimeToEmerge186) 
    
    pup.dr186=1/TimeToEmerge186 
    
    egg.surv=EggDailySurvival186^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS186=larv.surv186*(1-larv.dr186) 
    
    LT186=larv.surv186*larv.dr186 
    
    PS186=pup.surv186*(1-pup.dr186) 
    
    PT186=pup.surv186*pup.dr186 
    
    LarvaSurvival187=LSurRatevec * TreatmentMat187
    
    TimeToPupate187=LDevRatevec
    
    PupaSurvival187=PSurRatevec
    
    TimeToEmerge187=PDevRatevec
    
    EggDailySurvival187=EggDailySurvival
    
    larv.surv187=LarvaSurvival187^(1/TimeToPupate187) 
    
    larv.dr187=1/TimeToPupate187 
    
    pup.surv187=PupaSurvival187^(1/TimeToEmerge187) 
    
    pup.dr187=1/TimeToEmerge187 
    
    egg.surv=EggDailySurvival187^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS187=larv.surv187*(1-larv.dr187) 
    
    LT187=larv.surv187*larv.dr187 
    
    PS187=pup.surv187*(1-pup.dr187) 
    
    PT187=pup.surv187*pup.dr187 
    
    LarvaSurvival188=LSurRatevec * TreatmentMat188
    
    TimeToPupate188=LDevRatevec
    
    PupaSurvival188=PSurRatevec
    
    TimeToEmerge188=PDevRatevec
    
    EggDailySurvival188=EggDailySurvival
    
    larv.surv188=LarvaSurvival188^(1/TimeToPupate188) 
    
    larv.dr188=1/TimeToPupate188 
    
    pup.surv188=PupaSurvival188^(1/TimeToEmerge188) 
    
    pup.dr188=1/TimeToEmerge188 
    
    egg.surv=EggDailySurvival188^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS188=larv.surv188*(1-larv.dr188) 
    
    LT188=larv.surv188*larv.dr188 
    
    PS188=pup.surv188*(1-pup.dr188) 
    
    PT188=pup.surv188*pup.dr188 
    
    LarvaSurvival189=LSurRatevec * TreatmentMat189
    
    TimeToPupate189=LDevRatevec
    
    PupaSurvival189=PSurRatevec
    
    TimeToEmerge189=PDevRatevec
    
    EggDailySurvival189=EggDailySurvival
    
    larv.surv189=LarvaSurvival189^(1/TimeToPupate189) 
    
    larv.dr189=1/TimeToPupate189 
    
    pup.surv189=PupaSurvival189^(1/TimeToEmerge189) 
    
    pup.dr189=1/TimeToEmerge189 
    
    egg.surv=EggDailySurvival189^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS189=larv.surv189*(1-larv.dr189) 
    
    LT189=larv.surv189*larv.dr189 
    
    PS189=pup.surv189*(1-pup.dr189) 
    
    PT189=pup.surv189*pup.dr189 
    
    LarvaSurvival190=LSurRatevec * TreatmentMat190
    
    TimeToPupate190=LDevRatevec
    
    PupaSurvival190=PSurRatevec
    
    TimeToEmerge190=PDevRatevec
    
    EggDailySurvival190=EggDailySurvival
    
    larv.surv190=LarvaSurvival190^(1/TimeToPupate190) 
    
    larv.dr190=1/TimeToPupate190 
    
    pup.surv190=PupaSurvival190^(1/TimeToEmerge190) 
    
    pup.dr190=1/TimeToEmerge190 
    
    egg.surv=EggDailySurvival190^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS190=larv.surv190*(1-larv.dr190) 
    
    LT190=larv.surv190*larv.dr190 
    
    PS190=pup.surv190*(1-pup.dr190) 
    
    PT190=pup.surv190*pup.dr190 
    
    LarvaSurvival191=LSurRatevec * TreatmentMat191
    
    TimeToPupate191=LDevRatevec
    
    PupaSurvival191=PSurRatevec
    
    TimeToEmerge191=PDevRatevec
    
    EggDailySurvival191=EggDailySurvival
    
    larv.surv191=LarvaSurvival191^(1/TimeToPupate191) 
    
    larv.dr191=1/TimeToPupate191 
    
    pup.surv191=PupaSurvival191^(1/TimeToEmerge191) 
    
    pup.dr191=1/TimeToEmerge191 
    
    egg.surv=EggDailySurvival191^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS191=larv.surv191*(1-larv.dr191) 
    
    LT191=larv.surv191*larv.dr191 
    
    PS191=pup.surv191*(1-pup.dr191) 
    
    PT191=pup.surv191*pup.dr191 
    
    LarvaSurvival192=LSurRatevec * TreatmentMat192
    
    TimeToPupate192=LDevRatevec
    
    PupaSurvival192=PSurRatevec
    
    TimeToEmerge192=PDevRatevec
    
    EggDailySurvival192=EggDailySurvival
    
    larv.surv192=LarvaSurvival192^(1/TimeToPupate192) 
    
    larv.dr192=1/TimeToPupate192 
    
    pup.surv192=PupaSurvival192^(1/TimeToEmerge192) 
    
    pup.dr192=1/TimeToEmerge192 
    
    egg.surv=EggDailySurvival192^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS192=larv.surv192*(1-larv.dr192) 
    
    LT192=larv.surv192*larv.dr192 
    
    PS192=pup.surv192*(1-pup.dr192) 
    
    PT192=pup.surv192*pup.dr192 
    
    LarvaSurvival193=LSurRatevec * TreatmentMat193
    
    TimeToPupate193=LDevRatevec
    
    PupaSurvival193=PSurRatevec
    
    TimeToEmerge193=PDevRatevec
    
    EggDailySurvival193=EggDailySurvival
    
    larv.surv193=LarvaSurvival193^(1/TimeToPupate193) 
    
    larv.dr193=1/TimeToPupate193 
    
    pup.surv193=PupaSurvival193^(1/TimeToEmerge193) 
    
    pup.dr193=1/TimeToEmerge193 
    
    egg.surv=EggDailySurvival193^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS193=larv.surv193*(1-larv.dr193) 
    
    LT193=larv.surv193*larv.dr193 
    
    PS193=pup.surv193*(1-pup.dr193) 
    
    PT193=pup.surv193*pup.dr193 
    
    LarvaSurvival194=LSurRatevec * TreatmentMat194
    
    TimeToPupate194=LDevRatevec
    
    PupaSurvival194=PSurRatevec
    
    TimeToEmerge194=PDevRatevec
    
    EggDailySurvival194=EggDailySurvival
    
    larv.surv194=LarvaSurvival194^(1/TimeToPupate194) 
    
    larv.dr194=1/TimeToPupate194 
    
    pup.surv194=PupaSurvival194^(1/TimeToEmerge194) 
    
    pup.dr194=1/TimeToEmerge194 
    
    egg.surv=EggDailySurvival194^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS194=larv.surv194*(1-larv.dr194) 
    
    LT194=larv.surv194*larv.dr194 
    
    PS194=pup.surv194*(1-pup.dr194) 
    
    PT194=pup.surv194*pup.dr194 
    
    LarvaSurvival195=LSurRatevec * TreatmentMat195
    
    TimeToPupate195=LDevRatevec
    
    PupaSurvival195=PSurRatevec
    
    TimeToEmerge195=PDevRatevec
    
    EggDailySurvival195=EggDailySurvival
    
    larv.surv195=LarvaSurvival195^(1/TimeToPupate195) 
    
    larv.dr195=1/TimeToPupate195 
    
    pup.surv195=PupaSurvival195^(1/TimeToEmerge195) 
    
    pup.dr195=1/TimeToEmerge195 
    
    egg.surv=EggDailySurvival195^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS195=larv.surv195*(1-larv.dr195) 
    
    LT195=larv.surv195*larv.dr195 
    
    PS195=pup.surv195*(1-pup.dr195) 
    
    PT195=pup.surv195*pup.dr195 
    
    LarvaSurvival196=LSurRatevec * TreatmentMat196
    
    TimeToPupate196=LDevRatevec
    
    PupaSurvival196=PSurRatevec
    
    TimeToEmerge196=PDevRatevec
    
    EggDailySurvival196=EggDailySurvival
    
    larv.surv196=LarvaSurvival196^(1/TimeToPupate196) 
    
    larv.dr196=1/TimeToPupate196 
    
    pup.surv196=PupaSurvival196^(1/TimeToEmerge196) 
    
    pup.dr196=1/TimeToEmerge196 
    
    egg.surv=EggDailySurvival196^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS196=larv.surv196*(1-larv.dr196) 
    
    LT196=larv.surv196*larv.dr196 
    
    PS196=pup.surv196*(1-pup.dr196) 
    
    PT196=pup.surv196*pup.dr196 
    
    LarvaSurvival197=LSurRatevec * TreatmentMat197
    
    TimeToPupate197=LDevRatevec
    
    PupaSurvival197=PSurRatevec
    
    TimeToEmerge197=PDevRatevec
    
    EggDailySurvival197=EggDailySurvival
    
    larv.surv197=LarvaSurvival197^(1/TimeToPupate197) 
    
    larv.dr197=1/TimeToPupate197 
    
    pup.surv197=PupaSurvival197^(1/TimeToEmerge197) 
    
    pup.dr197=1/TimeToEmerge197 
    
    egg.surv=EggDailySurvival197^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS197=larv.surv197*(1-larv.dr197) 
    
    LT197=larv.surv197*larv.dr197 
    
    PS197=pup.surv197*(1-pup.dr197) 
    
    PT197=pup.surv197*pup.dr197 
    
    LarvaSurvival198=LSurRatevec * TreatmentMat198
    
    TimeToPupate198=LDevRatevec
    
    PupaSurvival198=PSurRatevec
    
    TimeToEmerge198=PDevRatevec
    
    EggDailySurvival198=EggDailySurvival
    
    larv.surv198=LarvaSurvival198^(1/TimeToPupate198) 
    
    larv.dr198=1/TimeToPupate198 
    
    pup.surv198=PupaSurvival198^(1/TimeToEmerge198) 
    
    pup.dr198=1/TimeToEmerge198 
    
    egg.surv=EggDailySurvival198^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS198=larv.surv198*(1-larv.dr198) 
    
    LT198=larv.surv198*larv.dr198 
    
    PS198=pup.surv198*(1-pup.dr198) 
    
    PT198=pup.surv198*pup.dr198 
    
    LarvaSurvival199=LSurRatevec * TreatmentMat199
    
    TimeToPupate199=LDevRatevec
    
    PupaSurvival199=PSurRatevec
    
    TimeToEmerge199=PDevRatevec
    
    EggDailySurvival199=EggDailySurvival
    
    larv.surv199=LarvaSurvival199^(1/TimeToPupate199) 
    
    larv.dr199=1/TimeToPupate199 
    
    pup.surv199=PupaSurvival199^(1/TimeToEmerge199) 
    
    pup.dr199=1/TimeToEmerge199 
    
    egg.surv=EggDailySurvival199^(1/TimeToHatch) 
    
    egg.dr=1/TimeToHatch 
    
    ES=egg.surv*(1-egg.dr) 
    
    ET=egg.surv*egg.dr 
    
    LS199=larv.surv199*(1-larv.dr199) 
    
    LT199=larv.surv199*larv.dr199 
    
    PS199=pup.surv199*(1-pup.dr199) 
    
    PT199=pup.surv199*pup.dr199 
    
    StartEgg<-StartEggs #This is to put 80 eggs in each playa lake. 
    for (i in 1:NumWet)
      assign(paste("Nproj",i, sep=""), matrix(c(StartEgg, rep(0, 4*simulation-1)), nrow = 4)) 
    
    NLDoCommand(1, "go") 
    
    for (i in 2:simulation) { 
      
      NLDoCommand(1, "go")
      
      
      TM1 = matrix(c(ES,0,                   0, 0,
                     ET,LS1[i], 0, 0,
                     0,LT1[i], PS1, 0,
                     0,   0,                         PT1, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs1<-paste("count eggs with [playa = 1 ]")
      
      Juv1<-NLReport(CountEggs1)
      
      Nproj1[1, i-1] = Juv1+Nproj1[1,i-1]
      
      Nproj1[1:4,i] = (TM1%*%Nproj1[1:4 ,i-1])
      
      Adults1=floor(Nproj1[4, i])
      
      NoAdults1=paste("ask patches[if pxcor = 262 and pycor = 304 [ sprout-mosqs", Adults1," ]]")
      
      NLCommand(NoAdults1)
      
      Nproj1[4,i]=0
      
      Nproj1=ifelse(Nproj1>0.1, Nproj1, 0)
      
      
      TM2 = matrix(c(ES,0,                   0, 0,
                     ET,LS2[i], 0, 0,
                     0,LT2[i], PS2, 0,
                     0,   0,                         PT2, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs2<-paste("count eggs with [playa = 2 ]")
      
      Juv2<-NLReport(CountEggs2)
      
      Nproj2[1, i-1] = Juv2+Nproj2[1,i-1]
      
      Nproj2[1:4,i] = (TM2%*%Nproj2[1:4 ,i-1])
      
      Adults2=floor(Nproj2[4, i])
      
      NoAdults2=paste("ask patches[if pxcor = 218 and pycor = 277 [ sprout-mosqs", Adults2," ]]")
      
      NLCommand(NoAdults2)
      
      Nproj2[4,i]=0
      
      Nproj2=ifelse(Nproj2>0.1, Nproj2, 0)
      
      
      TM3 = matrix(c(ES,0,                   0, 0,
                     ET,LS3[i], 0, 0,
                     0,LT3[i], PS3, 0,
                     0,   0,                         PT3, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs3<-paste("count eggs with [playa = 3 ]")
      
      Juv3<-NLReport(CountEggs3)
      
      Nproj3[1, i-1] = Juv3+Nproj3[1,i-1]
      
      Nproj3[1:4,i] = (TM3%*%Nproj3[1:4 ,i-1])
      
      Adults3=floor(Nproj3[4, i])
      
      NoAdults3=paste("ask patches[if pxcor = 326 and pycor = 132 [ sprout-mosqs", Adults3," ]]")
      
      NLCommand(NoAdults3)
      
      Nproj3[4,i]=0
      
      Nproj3=ifelse(Nproj3>0.1, Nproj3, 0)
      
      
      TM4 = matrix(c(ES,0,                   0, 0,
                     ET,LS4[i], 0, 0,
                     0,LT4[i], PS4, 0,
                     0,   0,                         PT4, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs4<-paste("count eggs with [playa = 4 ]")
      
      Juv4<-NLReport(CountEggs4)
      
      Nproj4[1, i-1] = Juv4+Nproj4[1,i-1]
      
      Nproj4[1:4,i] = (TM4%*%Nproj4[1:4 ,i-1])
      
      Adults4=floor(Nproj4[4, i])
      
      NoAdults4=paste("ask patches[if pxcor = 84 and pycor = 168 [ sprout-mosqs", Adults4," ]]")
      
      NLCommand(NoAdults4)
      
      Nproj4[4,i]=0
      
      Nproj4=ifelse(Nproj4>0.1, Nproj4, 0)
      
      
      TM5 = matrix(c(ES,0,                   0, 0,
                     ET,LS5[i], 0, 0,
                     0,LT5[i], PS5, 0,
                     0,   0,                         PT5, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs5<-paste("count eggs with [playa = 5 ]")
      
      Juv5<-NLReport(CountEggs5)
      
      Nproj5[1, i-1] = Juv5+Nproj5[1,i-1]
      
      Nproj5[1:4,i] = (TM5%*%Nproj5[1:4 ,i-1])
      
      Adults5=floor(Nproj5[4, i])
      
      NoAdults5=paste("ask patches[if pxcor = 128 and pycor = 56 [ sprout-mosqs", Adults5," ]]")
      
      NLCommand(NoAdults5)
      
      Nproj5[4,i]=0
      
      Nproj5=ifelse(Nproj5>0.1, Nproj5, 0)
      
      
      TM6 = matrix(c(ES,0,                   0, 0,
                     ET,LS6[i], 0, 0,
                     0,LT6[i], PS6, 0,
                     0,   0,                         PT6, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs6<-paste("count eggs with [playa = 6 ]")
      
      Juv6<-NLReport(CountEggs6)
      
      Nproj6[1, i-1] = Juv6+Nproj6[1,i-1]
      
      Nproj6[1:4,i] = (TM6%*%Nproj6[1:4 ,i-1])
      
      Adults6=floor(Nproj6[4, i])
      
      NoAdults6=paste("ask patches[if pxcor = 77 and pycor = 110 [ sprout-mosqs", Adults6," ]]")
      
      NLCommand(NoAdults6)
      
      Nproj6[4,i]=0
      
      Nproj6=ifelse(Nproj6>0.1, Nproj6, 0)
      
      
      TM7 = matrix(c(ES,0,                   0, 0,
                     ET,LS7[i], 0, 0,
                     0,LT7[i], PS7, 0,
                     0,   0,                         PT7, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs7<-paste("count eggs with [playa = 7 ]")
      
      Juv7<-NLReport(CountEggs7)
      
      Nproj7[1, i-1] = Juv7+Nproj7[1,i-1]
      
      Nproj7[1:4,i] = (TM7%*%Nproj7[1:4 ,i-1])
      
      Adults7=floor(Nproj7[4, i])
      
      NoAdults7=paste("ask patches[if pxcor = 272 and pycor = 276 [ sprout-mosqs", Adults7," ]]")
      
      NLCommand(NoAdults7)
      
      Nproj7[4,i]=0
      
      Nproj7=ifelse(Nproj7>0.1, Nproj7, 0)
      
      
      TM8 = matrix(c(ES,0,                   0, 0,
                     ET,LS8[i], 0, 0,
                     0,LT8[i], PS8, 0,
                     0,   0,                         PT8, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs8<-paste("count eggs with [playa = 8 ]")
      
      Juv8<-NLReport(CountEggs8)
      
      Nproj8[1, i-1] = Juv8+Nproj8[1,i-1]
      
      Nproj8[1:4,i] = (TM8%*%Nproj8[1:4 ,i-1])
      
      Adults8=floor(Nproj8[4, i])
      
      NoAdults8=paste("ask patches[if pxcor = 355 and pycor = 277 [ sprout-mosqs", Adults8," ]]")
      
      NLCommand(NoAdults8)
      
      Nproj8[4,i]=0
      
      Nproj8=ifelse(Nproj8>0.1, Nproj8, 0)
      
      
      TM9 = matrix(c(ES,0,                   0, 0,
                     ET,LS9[i], 0, 0,
                     0,LT9[i], PS9, 0,
                     0,   0,                         PT9, 1)
                   , nr = 4, byrow=T) 
      
      
      
      CountEggs9<-paste("count eggs with [playa = 9 ]")
      
      Juv9<-NLReport(CountEggs9)
      
      Nproj9[1, i-1] = Juv9+Nproj9[1,i-1]
      
      Nproj9[1:4,i] = (TM9%*%Nproj9[1:4 ,i-1])
      
      Adults9=floor(Nproj9[4, i])
      
      NoAdults9=paste("ask patches[if pxcor = 298 and pycor = 288 [ sprout-mosqs", Adults9," ]]")
      
      NLCommand(NoAdults9)
      
      Nproj9[4,i]=0
      
      Nproj9=ifelse(Nproj9>0.1, Nproj9, 0)
      
      
      TM10 = matrix(c(ES,0,                   0, 0,
                      ET,LS10[i], 0, 0,
                      0,LT10[i], PS10, 0,
                      0,   0,                         PT10, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs10<-paste("count eggs with [playa = 10 ]")
      
      Juv10<-NLReport(CountEggs10)
      
      Nproj10[1, i-1] = Juv10+Nproj10[1,i-1]
      
      Nproj10[1:4,i] = (TM10%*%Nproj10[1:4 ,i-1])
      
      Adults10=floor(Nproj10[4, i])
      
      NoAdults10=paste("ask patches[if pxcor = 287 and pycor = 295 [ sprout-mosqs", Adults10," ]]")
      
      NLCommand(NoAdults10)
      
      Nproj10[4,i]=0
      
      Nproj10=ifelse(Nproj10>0.1, Nproj10, 0)
      
      
      TM11 = matrix(c(ES,0,                   0, 0,
                      ET,LS11[i], 0, 0,
                      0,LT11[i], PS11, 0,
                      0,   0,                         PT11, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs11<-paste("count eggs with [playa = 11 ]")
      
      Juv11<-NLReport(CountEggs11)
      
      Nproj11[1, i-1] = Juv11+Nproj11[1,i-1]
      
      Nproj11[1:4,i] = (TM11%*%Nproj11[1:4 ,i-1])
      
      Adults11=floor(Nproj11[4, i])
      
      NoAdults11=paste("ask patches[if pxcor = 116 and pycor = 58 [ sprout-mosqs", Adults11," ]]")
      
      NLCommand(NoAdults11)
      
      Nproj11[4,i]=0
      
      Nproj11=ifelse(Nproj11>0.1, Nproj11, 0)
      
      
      TM12 = matrix(c(ES,0,                   0, 0,
                      ET,LS12[i], 0, 0,
                      0,LT12[i], PS12, 0,
                      0,   0,                         PT12, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs12<-paste("count eggs with [playa = 12 ]")
      
      Juv12<-NLReport(CountEggs12)
      
      Nproj12[1, i-1] = Juv12+Nproj12[1,i-1]
      
      Nproj12[1:4,i] = (TM12%*%Nproj12[1:4 ,i-1])
      
      Adults12=floor(Nproj12[4, i])
      
      NoAdults12=paste("ask patches[if pxcor = 67 and pycor = 74 [ sprout-mosqs", Adults12," ]]")
      
      NLCommand(NoAdults12)
      
      Nproj12[4,i]=0
      
      Nproj12=ifelse(Nproj12>0.1, Nproj12, 0)
      
      
      TM13 = matrix(c(ES,0,                   0, 0,
                      ET,LS13[i], 0, 0,
                      0,LT13[i], PS13, 0,
                      0,   0,                         PT13, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs13<-paste("count eggs with [playa = 13 ]")
      
      Juv13<-NLReport(CountEggs13)
      
      Nproj13[1, i-1] = Juv13+Nproj13[1,i-1]
      
      Nproj13[1:4,i] = (TM13%*%Nproj13[1:4 ,i-1])
      
      Adults13=floor(Nproj13[4, i])
      
      NoAdults13=paste("ask patches[if pxcor = 152 and pycor = 94 [ sprout-mosqs", Adults13," ]]")
      
      NLCommand(NoAdults13)
      
      Nproj13[4,i]=0
      
      Nproj13=ifelse(Nproj13>0.1, Nproj13, 0)
      
      
      TM14 = matrix(c(ES,0,                   0, 0,
                      ET,LS14[i], 0, 0,
                      0,LT14[i], PS14, 0,
                      0,   0,                         PT14, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs14<-paste("count eggs with [playa = 14 ]")
      
      Juv14<-NLReport(CountEggs14)
      
      Nproj14[1, i-1] = Juv14+Nproj14[1,i-1]
      
      Nproj14[1:4,i] = (TM14%*%Nproj14[1:4 ,i-1])
      
      Adults14=floor(Nproj14[4, i])
      
      NoAdults14=paste("ask patches[if pxcor = 72 and pycor = 100 [ sprout-mosqs", Adults14," ]]")
      
      NLCommand(NoAdults14)
      
      Nproj14[4,i]=0
      
      Nproj14=ifelse(Nproj14>0.1, Nproj14, 0)
      
      
      TM15 = matrix(c(ES,0,                   0, 0,
                      ET,LS15[i], 0, 0,
                      0,LT15[i], PS15, 0,
                      0,   0,                         PT15, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs15<-paste("count eggs with [playa = 15 ]")
      
      Juv15<-NLReport(CountEggs15)
      
      Nproj15[1, i-1] = Juv15+Nproj15[1,i-1]
      
      Nproj15[1:4,i] = (TM15%*%Nproj15[1:4 ,i-1])
      
      Adults15=floor(Nproj15[4, i])
      
      NoAdults15=paste("ask patches[if pxcor = 58 and pycor = 112 [ sprout-mosqs", Adults15," ]]")
      
      NLCommand(NoAdults15)
      
      Nproj15[4,i]=0
      
      Nproj15=ifelse(Nproj15>0.1, Nproj15, 0)
      
      
      TM16 = matrix(c(ES,0,                   0, 0,
                      ET,LS16[i], 0, 0,
                      0,LT16[i], PS16, 0,
                      0,   0,                         PT16, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs16<-paste("count eggs with [playa = 16 ]")
      
      Juv16<-NLReport(CountEggs16)
      
      Nproj16[1, i-1] = Juv16+Nproj16[1,i-1]
      
      Nproj16[1:4,i] = (TM16%*%Nproj16[1:4 ,i-1])
      
      Adults16=floor(Nproj16[4, i])
      
      NoAdults16=paste("ask patches[if pxcor = 78 and pycor = 126 [ sprout-mosqs", Adults16," ]]")
      
      NLCommand(NoAdults16)
      
      Nproj16[4,i]=0
      
      Nproj16=ifelse(Nproj16>0.1, Nproj16, 0)
      
      
      TM17 = matrix(c(ES,0,                   0, 0,
                      ET,LS17[i], 0, 0,
                      0,LT17[i], PS17, 0,
                      0,   0,                         PT17, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs17<-paste("count eggs with [playa = 17 ]")
      
      Juv17<-NLReport(CountEggs17)
      
      Nproj17[1, i-1] = Juv17+Nproj17[1,i-1]
      
      Nproj17[1:4,i] = (TM17%*%Nproj17[1:4 ,i-1])
      
      Adults17=floor(Nproj17[4, i])
      
      NoAdults17=paste("ask patches[if pxcor = 45 and pycor = 136 [ sprout-mosqs", Adults17," ]]")
      
      NLCommand(NoAdults17)
      
      Nproj17[4,i]=0
      
      Nproj17=ifelse(Nproj17>0.1, Nproj17, 0)
      
      
      TM18 = matrix(c(ES,0,                   0, 0,
                      ET,LS18[i], 0, 0,
                      0,LT18[i], PS18, 0,
                      0,   0,                         PT18, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs18<-paste("count eggs with [playa = 18 ]")
      
      Juv18<-NLReport(CountEggs18)
      
      Nproj18[1, i-1] = Juv18+Nproj18[1,i-1]
      
      Nproj18[1:4,i] = (TM18%*%Nproj18[1:4 ,i-1])
      
      Adults18=floor(Nproj18[4, i])
      
      NoAdults18=paste("ask patches[if pxcor = 56 and pycor = 67 [ sprout-mosqs", Adults18," ]]")
      
      NLCommand(NoAdults18)
      
      Nproj18[4,i]=0
      
      Nproj18=ifelse(Nproj18>0.1, Nproj18, 0)
      
      
      TM19 = matrix(c(ES,0,                   0, 0,
                      ET,LS19[i], 0, 0,
                      0,LT19[i], PS19, 0,
                      0,   0,                         PT19, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs19<-paste("count eggs with [playa = 19 ]")
      
      Juv19<-NLReport(CountEggs19)
      
      Nproj19[1, i-1] = Juv19+Nproj19[1,i-1]
      
      Nproj19[1:4,i] = (TM19%*%Nproj19[1:4 ,i-1])
      
      Adults19=floor(Nproj19[4, i])
      
      NoAdults19=paste("ask patches[if pxcor = 99 and pycor = 63 [ sprout-mosqs", Adults19," ]]")
      
      NLCommand(NoAdults19)
      
      Nproj19[4,i]=0
      
      Nproj19=ifelse(Nproj19>0.1, Nproj19, 0)
      
      
      TM20 = matrix(c(ES,0,                   0, 0,
                      ET,LS20[i], 0, 0,
                      0,LT20[i], PS20, 0,
                      0,   0,                         PT20, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs20<-paste("count eggs with [playa = 20 ]")
      
      Juv20<-NLReport(CountEggs20)
      
      Nproj20[1, i-1] = Juv20+Nproj20[1,i-1]
      
      Nproj20[1:4,i] = (TM20%*%Nproj20[1:4 ,i-1])
      
      Adults20=floor(Nproj20[4, i])
      
      NoAdults20=paste("ask patches[if pxcor = 278 and pycor = 354 [ sprout-mosqs", Adults20," ]]")
      
      NLCommand(NoAdults20)
      
      Nproj20[4,i]=0
      
      Nproj20=ifelse(Nproj20>0.1, Nproj20, 0)
      
      
      TM21 = matrix(c(ES,0,                   0, 0,
                      ET,LS21[i], 0, 0,
                      0,LT21[i], PS21, 0,
                      0,   0,                         PT21, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs21<-paste("count eggs with [playa = 21 ]")
      
      Juv21<-NLReport(CountEggs21)
      
      Nproj21[1, i-1] = Juv21+Nproj21[1,i-1]
      
      Nproj21[1:4,i] = (TM21%*%Nproj21[1:4 ,i-1])
      
      Adults21=floor(Nproj21[4, i])
      
      NoAdults21=paste("ask patches[if pxcor = 75 and pycor = 81 [ sprout-mosqs", Adults21," ]]")
      
      NLCommand(NoAdults21)
      
      Nproj21[4,i]=0
      
      Nproj21=ifelse(Nproj21>0.1, Nproj21, 0)
      
      
      TM22 = matrix(c(ES,0,                   0, 0,
                      ET,LS22[i], 0, 0,
                      0,LT22[i], PS22, 0,
                      0,   0,                         PT22, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs22<-paste("count eggs with [playa = 22 ]")
      
      Juv22<-NLReport(CountEggs22)
      
      Nproj22[1, i-1] = Juv22+Nproj22[1,i-1]
      
      Nproj22[1:4,i] = (TM22%*%Nproj22[1:4 ,i-1])
      
      Adults22=floor(Nproj22[4, i])
      
      NoAdults22=paste("ask patches[if pxcor = 148 and pycor = 95 [ sprout-mosqs", Adults22," ]]")
      
      NLCommand(NoAdults22)
      
      Nproj22[4,i]=0
      
      Nproj22=ifelse(Nproj22>0.1, Nproj22, 0)
      
      
      TM23 = matrix(c(ES,0,                   0, 0,
                      ET,LS23[i], 0, 0,
                      0,LT23[i], PS23, 0,
                      0,   0,                         PT23, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs23<-paste("count eggs with [playa = 23 ]")
      
      Juv23<-NLReport(CountEggs23)
      
      Nproj23[1, i-1] = Juv23+Nproj23[1,i-1]
      
      Nproj23[1:4,i] = (TM23%*%Nproj23[1:4 ,i-1])
      
      Adults23=floor(Nproj23[4, i])
      
      NoAdults23=paste("ask patches[if pxcor = 103 and pycor = 34 [ sprout-mosqs", Adults23," ]]")
      
      NLCommand(NoAdults23)
      
      Nproj23[4,i]=0
      
      Nproj23=ifelse(Nproj23>0.1, Nproj23, 0)
      
      
      TM24 = matrix(c(ES,0,                   0, 0,
                      ET,LS24[i], 0, 0,
                      0,LT24[i], PS24, 0,
                      0,   0,                         PT24, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs24<-paste("count eggs with [playa = 24 ]")
      
      Juv24<-NLReport(CountEggs24)
      
      Nproj24[1, i-1] = Juv24+Nproj24[1,i-1]
      
      Nproj24[1:4,i] = (TM24%*%Nproj24[1:4 ,i-1])
      
      Adults24=floor(Nproj24[4, i])
      
      NoAdults24=paste("ask patches[if pxcor = 95 and pycor = 57 [ sprout-mosqs", Adults24," ]]")
      
      NLCommand(NoAdults24)
      
      Nproj24[4,i]=0
      
      Nproj24=ifelse(Nproj24>0.1, Nproj24, 0)
      
      
      TM25 = matrix(c(ES,0,                   0, 0,
                      ET,LS25[i], 0, 0,
                      0,LT25[i], PS25, 0,
                      0,   0,                         PT25, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs25<-paste("count eggs with [playa = 25 ]")
      
      Juv25<-NLReport(CountEggs25)
      
      Nproj25[1, i-1] = Juv25+Nproj25[1,i-1]
      
      Nproj25[1:4,i] = (TM25%*%Nproj25[1:4 ,i-1])
      
      Adults25=floor(Nproj25[4, i])
      
      NoAdults25=paste("ask patches[if pxcor = 27 and pycor = 28 [ sprout-mosqs", Adults25," ]]")
      
      NLCommand(NoAdults25)
      
      Nproj25[4,i]=0
      
      Nproj25=ifelse(Nproj25>0.1, Nproj25, 0)
      
      
      TM26 = matrix(c(ES,0,                   0, 0,
                      ET,LS26[i], 0, 0,
                      0,LT26[i], PS26, 0,
                      0,   0,                         PT26, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs26<-paste("count eggs with [playa = 26 ]")
      
      Juv26<-NLReport(CountEggs26)
      
      Nproj26[1, i-1] = Juv26+Nproj26[1,i-1]
      
      Nproj26[1:4,i] = (TM26%*%Nproj26[1:4 ,i-1])
      
      Adults26=floor(Nproj26[4, i])
      
      NoAdults26=paste("ask patches[if pxcor = 221 and pycor = 172 [ sprout-mosqs", Adults26," ]]")
      
      NLCommand(NoAdults26)
      
      Nproj26[4,i]=0
      
      Nproj26=ifelse(Nproj26>0.1, Nproj26, 0)
      
      
      TM27 = matrix(c(ES,0,                   0, 0,
                      ET,LS27[i], 0, 0,
                      0,LT27[i], PS27, 0,
                      0,   0,                         PT27, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs27<-paste("count eggs with [playa = 27 ]")
      
      Juv27<-NLReport(CountEggs27)
      
      Nproj27[1, i-1] = Juv27+Nproj27[1,i-1]
      
      Nproj27[1:4,i] = (TM27%*%Nproj27[1:4 ,i-1])
      
      Adults27=floor(Nproj27[4, i])
      
      NoAdults27=paste("ask patches[if pxcor = 303 and pycor = 253 [ sprout-mosqs", Adults27," ]]")
      
      NLCommand(NoAdults27)
      
      Nproj27[4,i]=0
      
      Nproj27=ifelse(Nproj27>0.1, Nproj27, 0)
      
      
      TM28 = matrix(c(ES,0,                   0, 0,
                      ET,LS28[i], 0, 0,
                      0,LT28[i], PS28, 0,
                      0,   0,                         PT28, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs28<-paste("count eggs with [playa = 28 ]")
      
      Juv28<-NLReport(CountEggs28)
      
      Nproj28[1, i-1] = Juv28+Nproj28[1,i-1]
      
      Nproj28[1:4,i] = (TM28%*%Nproj28[1:4 ,i-1])
      
      Adults28=floor(Nproj28[4, i])
      
      NoAdults28=paste("ask patches[if pxcor = 282 and pycor = 258 [ sprout-mosqs", Adults28," ]]")
      
      NLCommand(NoAdults28)
      
      Nproj28[4,i]=0
      
      Nproj28=ifelse(Nproj28>0.1, Nproj28, 0)
      
      
      TM29 = matrix(c(ES,0,                   0, 0,
                      ET,LS29[i], 0, 0,
                      0,LT29[i], PS29, 0,
                      0,   0,                         PT29, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs29<-paste("count eggs with [playa = 29 ]")
      
      Juv29<-NLReport(CountEggs29)
      
      Nproj29[1, i-1] = Juv29+Nproj29[1,i-1]
      
      Nproj29[1:4,i] = (TM29%*%Nproj29[1:4 ,i-1])
      
      Adults29=floor(Nproj29[4, i])
      
      NoAdults29=paste("ask patches[if pxcor = 284 and pycor = 313 [ sprout-mosqs", Adults29," ]]")
      
      NLCommand(NoAdults29)
      
      Nproj29[4,i]=0
      
      Nproj29=ifelse(Nproj29>0.1, Nproj29, 0)
      
      
      TM30 = matrix(c(ES,0,                   0, 0,
                      ET,LS30[i], 0, 0,
                      0,LT30[i], PS30, 0,
                      0,   0,                         PT30, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs30<-paste("count eggs with [playa = 30 ]")
      
      Juv30<-NLReport(CountEggs30)
      
      Nproj30[1, i-1] = Juv30+Nproj30[1,i-1]
      
      Nproj30[1:4,i] = (TM30%*%Nproj30[1:4 ,i-1])
      
      Adults30=floor(Nproj30[4, i])
      
      NoAdults30=paste("ask patches[if pxcor = 278 and pycor = 280 [ sprout-mosqs", Adults30," ]]")
      
      NLCommand(NoAdults30)
      
      Nproj30[4,i]=0
      
      Nproj30=ifelse(Nproj30>0.1, Nproj30, 0)
      
      
      TM31 = matrix(c(ES,0,                   0, 0,
                      ET,LS31[i], 0, 0,
                      0,LT31[i], PS31, 0,
                      0,   0,                         PT31, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs31<-paste("count eggs with [playa = 31 ]")
      
      Juv31<-NLReport(CountEggs31)
      
      Nproj31[1, i-1] = Juv31+Nproj31[1,i-1]
      
      Nproj31[1:4,i] = (TM31%*%Nproj31[1:4 ,i-1])
      
      Adults31=floor(Nproj31[4, i])
      
      NoAdults31=paste("ask patches[if pxcor = 288 and pycor = 280 [ sprout-mosqs", Adults31," ]]")
      
      NLCommand(NoAdults31)
      
      Nproj31[4,i]=0
      
      Nproj31=ifelse(Nproj31>0.1, Nproj31, 0)
      
      
      TM32 = matrix(c(ES,0,                   0, 0,
                      ET,LS32[i], 0, 0,
                      0,LT32[i], PS32, 0,
                      0,   0,                         PT32, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs32<-paste("count eggs with [playa = 32 ]")
      
      Juv32<-NLReport(CountEggs32)
      
      Nproj32[1, i-1] = Juv32+Nproj32[1,i-1]
      
      Nproj32[1:4,i] = (TM32%*%Nproj32[1:4 ,i-1])
      
      Adults32=floor(Nproj32[4, i])
      
      NoAdults32=paste("ask patches[if pxcor = 263 and pycor = 249 [ sprout-mosqs", Adults32," ]]")
      
      NLCommand(NoAdults32)
      
      Nproj32[4,i]=0
      
      Nproj32=ifelse(Nproj32>0.1, Nproj32, 0)
      
      
      TM33 = matrix(c(ES,0,                   0, 0,
                      ET,LS33[i], 0, 0,
                      0,LT33[i], PS33, 0,
                      0,   0,                         PT33, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs33<-paste("count eggs with [playa = 33 ]")
      
      Juv33<-NLReport(CountEggs33)
      
      Nproj33[1, i-1] = Juv33+Nproj33[1,i-1]
      
      Nproj33[1:4,i] = (TM33%*%Nproj33[1:4 ,i-1])
      
      Adults33=floor(Nproj33[4, i])
      
      NoAdults33=paste("ask patches[if pxcor = 265 and pycor = 274 [ sprout-mosqs", Adults33," ]]")
      
      NLCommand(NoAdults33)
      
      Nproj33[4,i]=0
      
      Nproj33=ifelse(Nproj33>0.1, Nproj33, 0)
      
      
      TM34 = matrix(c(ES,0,                   0, 0,
                      ET,LS34[i], 0, 0,
                      0,LT34[i], PS34, 0,
                      0,   0,                         PT34, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs34<-paste("count eggs with [playa = 34 ]")
      
      Juv34<-NLReport(CountEggs34)
      
      Nproj34[1, i-1] = Juv34+Nproj34[1,i-1]
      
      Nproj34[1:4,i] = (TM34%*%Nproj34[1:4 ,i-1])
      
      Adults34=floor(Nproj34[4, i])
      
      NoAdults34=paste("ask patches[if pxcor = 281 and pycor = 267 [ sprout-mosqs", Adults34," ]]")
      
      NLCommand(NoAdults34)
      
      Nproj34[4,i]=0
      
      Nproj34=ifelse(Nproj34>0.1, Nproj34, 0)
      
      
      TM35 = matrix(c(ES,0,                   0, 0,
                      ET,LS35[i], 0, 0,
                      0,LT35[i], PS35, 0,
                      0,   0,                         PT35, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs35<-paste("count eggs with [playa = 35 ]")
      
      Juv35<-NLReport(CountEggs35)
      
      Nproj35[1, i-1] = Juv35+Nproj35[1,i-1]
      
      Nproj35[1:4,i] = (TM35%*%Nproj35[1:4 ,i-1])
      
      Adults35=floor(Nproj35[4, i])
      
      NoAdults35=paste("ask patches[if pxcor = 298 and pycor = 267 [ sprout-mosqs", Adults35," ]]")
      
      NLCommand(NoAdults35)
      
      Nproj35[4,i]=0
      
      Nproj35=ifelse(Nproj35>0.1, Nproj35, 0)
      
      
      TM36 = matrix(c(ES,0,                   0, 0,
                      ET,LS36[i], 0, 0,
                      0,LT36[i], PS36, 0,
                      0,   0,                         PT36, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs36<-paste("count eggs with [playa = 36 ]")
      
      Juv36<-NLReport(CountEggs36)
      
      Nproj36[1, i-1] = Juv36+Nproj36[1,i-1]
      
      Nproj36[1:4,i] = (TM36%*%Nproj36[1:4 ,i-1])
      
      Adults36=floor(Nproj36[4, i])
      
      NoAdults36=paste("ask patches[if pxcor = 310 and pycor = 287 [ sprout-mosqs", Adults36," ]]")
      
      NLCommand(NoAdults36)
      
      Nproj36[4,i]=0
      
      Nproj36=ifelse(Nproj36>0.1, Nproj36, 0)
      
      
      TM37 = matrix(c(ES,0,                   0, 0,
                      ET,LS37[i], 0, 0,
                      0,LT37[i], PS37, 0,
                      0,   0,                         PT37, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs37<-paste("count eggs with [playa = 37 ]")
      
      Juv37<-NLReport(CountEggs37)
      
      Nproj37[1, i-1] = Juv37+Nproj37[1,i-1]
      
      Nproj37[1:4,i] = (TM37%*%Nproj37[1:4 ,i-1])
      
      Adults37=floor(Nproj37[4, i])
      
      NoAdults37=paste("ask patches[if pxcor = 228 and pycor = 268 [ sprout-mosqs", Adults37," ]]")
      
      NLCommand(NoAdults37)
      
      Nproj37[4,i]=0
      
      Nproj37=ifelse(Nproj37>0.1, Nproj37, 0)
      
      
      TM38 = matrix(c(ES,0,                   0, 0,
                      ET,LS38[i], 0, 0,
                      0,LT38[i], PS38, 0,
                      0,   0,                         PT38, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs38<-paste("count eggs with [playa = 38 ]")
      
      Juv38<-NLReport(CountEggs38)
      
      Nproj38[1, i-1] = Juv38+Nproj38[1,i-1]
      
      Nproj38[1:4,i] = (TM38%*%Nproj38[1:4 ,i-1])
      
      Adults38=floor(Nproj38[4, i])
      
      NoAdults38=paste("ask patches[if pxcor = 203 and pycor = 306 [ sprout-mosqs", Adults38," ]]")
      
      NLCommand(NoAdults38)
      
      Nproj38[4,i]=0
      
      Nproj38=ifelse(Nproj38>0.1, Nproj38, 0)
      
      
      TM39 = matrix(c(ES,0,                   0, 0,
                      ET,LS39[i], 0, 0,
                      0,LT39[i], PS39, 0,
                      0,   0,                         PT39, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs39<-paste("count eggs with [playa = 39 ]")
      
      Juv39<-NLReport(CountEggs39)
      
      Nproj39[1, i-1] = Juv39+Nproj39[1,i-1]
      
      Nproj39[1:4,i] = (TM39%*%Nproj39[1:4 ,i-1])
      
      Adults39=floor(Nproj39[4, i])
      
      NoAdults39=paste("ask patches[if pxcor = 225 and pycor = 280 [ sprout-mosqs", Adults39," ]]")
      
      NLCommand(NoAdults39)
      
      Nproj39[4,i]=0
      
      Nproj39=ifelse(Nproj39>0.1, Nproj39, 0)
      
      
      TM40 = matrix(c(ES,0,                   0, 0,
                      ET,LS40[i], 0, 0,
                      0,LT40[i], PS40, 0,
                      0,   0,                         PT40, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs40<-paste("count eggs with [playa = 40 ]")
      
      Juv40<-NLReport(CountEggs40)
      
      Nproj40[1, i-1] = Juv40+Nproj40[1,i-1]
      
      Nproj40[1:4,i] = (TM40%*%Nproj40[1:4 ,i-1])
      
      Adults40=floor(Nproj40[4, i])
      
      NoAdults40=paste("ask patches[if pxcor = 259 and pycor = 276 [ sprout-mosqs", Adults40," ]]")
      
      NLCommand(NoAdults40)
      
      Nproj40[4,i]=0
      
      Nproj40=ifelse(Nproj40>0.1, Nproj40, 0)
      
      
      TM41 = matrix(c(ES,0,                   0, 0,
                      ET,LS41[i], 0, 0,
                      0,LT41[i], PS41, 0,
                      0,   0,                         PT41, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs41<-paste("count eggs with [playa = 41 ]")
      
      Juv41<-NLReport(CountEggs41)
      
      Nproj41[1, i-1] = Juv41+Nproj41[1,i-1]
      
      Nproj41[1:4,i] = (TM41%*%Nproj41[1:4 ,i-1])
      
      Adults41=floor(Nproj41[4, i])
      
      NoAdults41=paste("ask patches[if pxcor = 196 and pycor = 189 [ sprout-mosqs", Adults41," ]]")
      
      NLCommand(NoAdults41)
      
      Nproj41[4,i]=0
      
      Nproj41=ifelse(Nproj41>0.1, Nproj41, 0)
      
      
      TM42 = matrix(c(ES,0,                   0, 0,
                      ET,LS42[i], 0, 0,
                      0,LT42[i], PS42, 0,
                      0,   0,                         PT42, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs42<-paste("count eggs with [playa = 42 ]")
      
      Juv42<-NLReport(CountEggs42)
      
      Nproj42[1, i-1] = Juv42+Nproj42[1,i-1]
      
      Nproj42[1:4,i] = (TM42%*%Nproj42[1:4 ,i-1])
      
      Adults42=floor(Nproj42[4, i])
      
      NoAdults42=paste("ask patches[if pxcor = 198 and pycor = 298 [ sprout-mosqs", Adults42," ]]")
      
      NLCommand(NoAdults42)
      
      Nproj42[4,i]=0
      
      Nproj42=ifelse(Nproj42>0.1, Nproj42, 0)
      
      
      TM43 = matrix(c(ES,0,                   0, 0,
                      ET,LS43[i], 0, 0,
                      0,LT43[i], PS43, 0,
                      0,   0,                         PT43, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs43<-paste("count eggs with [playa = 43 ]")
      
      Juv43<-NLReport(CountEggs43)
      
      Nproj43[1, i-1] = Juv43+Nproj43[1,i-1]
      
      Nproj43[1:4,i] = (TM43%*%Nproj43[1:4 ,i-1])
      
      Adults43=floor(Nproj43[4, i])
      
      NoAdults43=paste("ask patches[if pxcor = 94 and pycor = 186 [ sprout-mosqs", Adults43," ]]")
      
      NLCommand(NoAdults43)
      
      Nproj43[4,i]=0
      
      Nproj43=ifelse(Nproj43>0.1, Nproj43, 0)
      
      
      TM44 = matrix(c(ES,0,                   0, 0,
                      ET,LS44[i], 0, 0,
                      0,LT44[i], PS44, 0,
                      0,   0,                         PT44, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs44<-paste("count eggs with [playa = 44 ]")
      
      Juv44<-NLReport(CountEggs44)
      
      Nproj44[1, i-1] = Juv44+Nproj44[1,i-1]
      
      Nproj44[1:4,i] = (TM44%*%Nproj44[1:4 ,i-1])
      
      Adults44=floor(Nproj44[4, i])
      
      NoAdults44=paste("ask patches[if pxcor = 140 and pycor = 137 [ sprout-mosqs", Adults44," ]]")
      
      NLCommand(NoAdults44)
      
      Nproj44[4,i]=0
      
      Nproj44=ifelse(Nproj44>0.1, Nproj44, 0)
      
      
      TM45 = matrix(c(ES,0,                   0, 0,
                      ET,LS45[i], 0, 0,
                      0,LT45[i], PS45, 0,
                      0,   0,                         PT45, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs45<-paste("count eggs with [playa = 45 ]")
      
      Juv45<-NLReport(CountEggs45)
      
      Nproj45[1, i-1] = Juv45+Nproj45[1,i-1]
      
      Nproj45[1:4,i] = (TM45%*%Nproj45[1:4 ,i-1])
      
      Adults45=floor(Nproj45[4, i])
      
      NoAdults45=paste("ask patches[if pxcor = 416 and pycor = 33 [ sprout-mosqs", Adults45," ]]")
      
      NLCommand(NoAdults45)
      
      Nproj45[4,i]=0
      
      Nproj45=ifelse(Nproj45>0.1, Nproj45, 0)
      
      
      TM46 = matrix(c(ES,0,                   0, 0,
                      ET,LS46[i], 0, 0,
                      0,LT46[i], PS46, 0,
                      0,   0,                         PT46, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs46<-paste("count eggs with [playa = 46 ]")
      
      Juv46<-NLReport(CountEggs46)
      
      Nproj46[1, i-1] = Juv46+Nproj46[1,i-1]
      
      Nproj46[1:4,i] = (TM46%*%Nproj46[1:4 ,i-1])
      
      Adults46=floor(Nproj46[4, i])
      
      NoAdults46=paste("ask patches[if pxcor = 407 and pycor = 49 [ sprout-mosqs", Adults46," ]]")
      
      NLCommand(NoAdults46)
      
      Nproj46[4,i]=0
      
      Nproj46=ifelse(Nproj46>0.1, Nproj46, 0)
      
      
      TM47 = matrix(c(ES,0,                   0, 0,
                      ET,LS47[i], 0, 0,
                      0,LT47[i], PS47, 0,
                      0,   0,                         PT47, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs47<-paste("count eggs with [playa = 47 ]")
      
      Juv47<-NLReport(CountEggs47)
      
      Nproj47[1, i-1] = Juv47+Nproj47[1,i-1]
      
      Nproj47[1:4,i] = (TM47%*%Nproj47[1:4 ,i-1])
      
      Adults47=floor(Nproj47[4, i])
      
      NoAdults47=paste("ask patches[if pxcor = 254 and pycor = 80 [ sprout-mosqs", Adults47," ]]")
      
      NLCommand(NoAdults47)
      
      Nproj47[4,i]=0
      
      Nproj47=ifelse(Nproj47>0.1, Nproj47, 0)
      
      
      TM48 = matrix(c(ES,0,                   0, 0,
                      ET,LS48[i], 0, 0,
                      0,LT48[i], PS48, 0,
                      0,   0,                         PT48, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs48<-paste("count eggs with [playa = 48 ]")
      
      Juv48<-NLReport(CountEggs48)
      
      Nproj48[1, i-1] = Juv48+Nproj48[1,i-1]
      
      Nproj48[1:4,i] = (TM48%*%Nproj48[1:4 ,i-1])
      
      Adults48=floor(Nproj48[4, i])
      
      NoAdults48=paste("ask patches[if pxcor = 402 and pycor = 64 [ sprout-mosqs", Adults48," ]]")
      
      NLCommand(NoAdults48)
      
      Nproj48[4,i]=0
      
      Nproj48=ifelse(Nproj48>0.1, Nproj48, 0)
      
      
      TM49 = matrix(c(ES,0,                   0, 0,
                      ET,LS49[i], 0, 0,
                      0,LT49[i], PS49, 0,
                      0,   0,                         PT49, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs49<-paste("count eggs with [playa = 49 ]")
      
      Juv49<-NLReport(CountEggs49)
      
      Nproj49[1, i-1] = Juv49+Nproj49[1,i-1]
      
      Nproj49[1:4,i] = (TM49%*%Nproj49[1:4 ,i-1])
      
      Adults49=floor(Nproj49[4, i])
      
      NoAdults49=paste("ask patches[if pxcor = 288 and pycor = 249 [ sprout-mosqs", Adults49," ]]")
      
      NLCommand(NoAdults49)
      
      Nproj49[4,i]=0
      
      Nproj49=ifelse(Nproj49>0.1, Nproj49, 0)
      
      
      TM50 = matrix(c(ES,0,                   0, 0,
                      ET,LS50[i], 0, 0,
                      0,LT50[i], PS50, 0,
                      0,   0,                         PT50, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs50<-paste("count eggs with [playa = 50 ]")
      
      Juv50<-NLReport(CountEggs50)
      
      Nproj50[1, i-1] = Juv50+Nproj50[1,i-1]
      
      Nproj50[1:4,i] = (TM50%*%Nproj50[1:4 ,i-1])
      
      Adults50=floor(Nproj50[4, i])
      
      NoAdults50=paste("ask patches[if pxcor = 349 and pycor = 230 [ sprout-mosqs", Adults50," ]]")
      
      NLCommand(NoAdults50)
      
      Nproj50[4,i]=0
      
      Nproj50=ifelse(Nproj50>0.1, Nproj50, 0)
      
      
      TM51 = matrix(c(ES,0,                   0, 0,
                      ET,LS51[i], 0, 0,
                      0,LT51[i], PS51, 0,
                      0,   0,                         PT51, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs51<-paste("count eggs with [playa = 51 ]")
      
      Juv51<-NLReport(CountEggs51)
      
      Nproj51[1, i-1] = Juv51+Nproj51[1,i-1]
      
      Nproj51[1:4,i] = (TM51%*%Nproj51[1:4 ,i-1])
      
      Adults51=floor(Nproj51[4, i])
      
      NoAdults51=paste("ask patches[if pxcor = 388 and pycor = 254 [ sprout-mosqs", Adults51," ]]")
      
      NLCommand(NoAdults51)
      
      Nproj51[4,i]=0
      
      Nproj51=ifelse(Nproj51>0.1, Nproj51, 0)
      
      
      TM52 = matrix(c(ES,0,                   0, 0,
                      ET,LS52[i], 0, 0,
                      0,LT52[i], PS52, 0,
                      0,   0,                         PT52, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs52<-paste("count eggs with [playa = 52 ]")
      
      Juv52<-NLReport(CountEggs52)
      
      Nproj52[1, i-1] = Juv52+Nproj52[1,i-1]
      
      Nproj52[1:4,i] = (TM52%*%Nproj52[1:4 ,i-1])
      
      Adults52=floor(Nproj52[4, i])
      
      NoAdults52=paste("ask patches[if pxcor = 85 and pycor = 104 [ sprout-mosqs", Adults52," ]]")
      
      NLCommand(NoAdults52)
      
      Nproj52[4,i]=0
      
      Nproj52=ifelse(Nproj52>0.1, Nproj52, 0)
      
      
      TM53 = matrix(c(ES,0,                   0, 0,
                      ET,LS53[i], 0, 0,
                      0,LT53[i], PS53, 0,
                      0,   0,                         PT53, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs53<-paste("count eggs with [playa = 53 ]")
      
      Juv53<-NLReport(CountEggs53)
      
      Nproj53[1, i-1] = Juv53+Nproj53[1,i-1]
      
      Nproj53[1:4,i] = (TM53%*%Nproj53[1:4 ,i-1])
      
      Adults53=floor(Nproj53[4, i])
      
      NoAdults53=paste("ask patches[if pxcor = 120 and pycor = 73 [ sprout-mosqs", Adults53," ]]")
      
      NLCommand(NoAdults53)
      
      Nproj53[4,i]=0
      
      Nproj53=ifelse(Nproj53>0.1, Nproj53, 0)
      
      
      TM54 = matrix(c(ES,0,                   0, 0,
                      ET,LS54[i], 0, 0,
                      0,LT54[i], PS54, 0,
                      0,   0,                         PT54, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs54<-paste("count eggs with [playa = 54 ]")
      
      Juv54<-NLReport(CountEggs54)
      
      Nproj54[1, i-1] = Juv54+Nproj54[1,i-1]
      
      Nproj54[1:4,i] = (TM54%*%Nproj54[1:4 ,i-1])
      
      Adults54=floor(Nproj54[4, i])
      
      NoAdults54=paste("ask patches[if pxcor = 52 and pycor = 169 [ sprout-mosqs", Adults54," ]]")
      
      NLCommand(NoAdults54)
      
      Nproj54[4,i]=0
      
      Nproj54=ifelse(Nproj54>0.1, Nproj54, 0)
      
      
      TM55 = matrix(c(ES,0,                   0, 0,
                      ET,LS55[i], 0, 0,
                      0,LT55[i], PS55, 0,
                      0,   0,                         PT55, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs55<-paste("count eggs with [playa = 55 ]")
      
      Juv55<-NLReport(CountEggs55)
      
      Nproj55[1, i-1] = Juv55+Nproj55[1,i-1]
      
      Nproj55[1:4,i] = (TM55%*%Nproj55[1:4 ,i-1])
      
      Adults55=floor(Nproj55[4, i])
      
      NoAdults55=paste("ask patches[if pxcor = 123 and pycor = 196 [ sprout-mosqs", Adults55," ]]")
      
      NLCommand(NoAdults55)
      
      Nproj55[4,i]=0
      
      Nproj55=ifelse(Nproj55>0.1, Nproj55, 0)
      
      
      TM56 = matrix(c(ES,0,                   0, 0,
                      ET,LS56[i], 0, 0,
                      0,LT56[i], PS56, 0,
                      0,   0,                         PT56, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs56<-paste("count eggs with [playa = 56 ]")
      
      Juv56<-NLReport(CountEggs56)
      
      Nproj56[1, i-1] = Juv56+Nproj56[1,i-1]
      
      Nproj56[1:4,i] = (TM56%*%Nproj56[1:4 ,i-1])
      
      Adults56=floor(Nproj56[4, i])
      
      NoAdults56=paste("ask patches[if pxcor = 147 and pycor = 159 [ sprout-mosqs", Adults56," ]]")
      
      NLCommand(NoAdults56)
      
      Nproj56[4,i]=0
      
      Nproj56=ifelse(Nproj56>0.1, Nproj56, 0)
      
      
      TM57 = matrix(c(ES,0,                   0, 0,
                      ET,LS57[i], 0, 0,
                      0,LT57[i], PS57, 0,
                      0,   0,                         PT57, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs57<-paste("count eggs with [playa = 57 ]")
      
      Juv57<-NLReport(CountEggs57)
      
      Nproj57[1, i-1] = Juv57+Nproj57[1,i-1]
      
      Nproj57[1:4,i] = (TM57%*%Nproj57[1:4 ,i-1])
      
      Adults57=floor(Nproj57[4, i])
      
      NoAdults57=paste("ask patches[if pxcor = 354 and pycor = 242 [ sprout-mosqs", Adults57," ]]")
      
      NLCommand(NoAdults57)
      
      Nproj57[4,i]=0
      
      Nproj57=ifelse(Nproj57>0.1, Nproj57, 0)
      
      
      TM58 = matrix(c(ES,0,                   0, 0,
                      ET,LS58[i], 0, 0,
                      0,LT58[i], PS58, 0,
                      0,   0,                         PT58, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs58<-paste("count eggs with [playa = 58 ]")
      
      Juv58<-NLReport(CountEggs58)
      
      Nproj58[1, i-1] = Juv58+Nproj58[1,i-1]
      
      Nproj58[1:4,i] = (TM58%*%Nproj58[1:4 ,i-1])
      
      Adults58=floor(Nproj58[4, i])
      
      NoAdults58=paste("ask patches[if pxcor = 319 and pycor = 301 [ sprout-mosqs", Adults58," ]]")
      
      NLCommand(NoAdults58)
      
      Nproj58[4,i]=0
      
      Nproj58=ifelse(Nproj58>0.1, Nproj58, 0)
      
      
      TM59 = matrix(c(ES,0,                   0, 0,
                      ET,LS59[i], 0, 0,
                      0,LT59[i], PS59, 0,
                      0,   0,                         PT59, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs59<-paste("count eggs with [playa = 59 ]")
      
      Juv59<-NLReport(CountEggs59)
      
      Nproj59[1, i-1] = Juv59+Nproj59[1,i-1]
      
      Nproj59[1:4,i] = (TM59%*%Nproj59[1:4 ,i-1])
      
      Adults59=floor(Nproj59[4, i])
      
      NoAdults59=paste("ask patches[if pxcor = 383 and pycor = 266 [ sprout-mosqs", Adults59," ]]")
      
      NLCommand(NoAdults59)
      
      Nproj59[4,i]=0
      
      Nproj59=ifelse(Nproj59>0.1, Nproj59, 0)
      
      
      TM60 = matrix(c(ES,0,                   0, 0,
                      ET,LS60[i], 0, 0,
                      0,LT60[i], PS60, 0,
                      0,   0,                         PT60, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs60<-paste("count eggs with [playa = 60 ]")
      
      Juv60<-NLReport(CountEggs60)
      
      Nproj60[1, i-1] = Juv60+Nproj60[1,i-1]
      
      Nproj60[1:4,i] = (TM60%*%Nproj60[1:4 ,i-1])
      
      Adults60=floor(Nproj60[4, i])
      
      NoAdults60=paste("ask patches[if pxcor = 396 and pycor = 23 [ sprout-mosqs", Adults60," ]]")
      
      NLCommand(NoAdults60)
      
      Nproj60[4,i]=0
      
      Nproj60=ifelse(Nproj60>0.1, Nproj60, 0)
      
      
      TM61 = matrix(c(ES,0,                   0, 0,
                      ET,LS61[i], 0, 0,
                      0,LT61[i], PS61, 0,
                      0,   0,                         PT61, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs61<-paste("count eggs with [playa = 61 ]")
      
      Juv61<-NLReport(CountEggs61)
      
      Nproj61[1, i-1] = Juv61+Nproj61[1,i-1]
      
      Nproj61[1:4,i] = (TM61%*%Nproj61[1:4 ,i-1])
      
      Adults61=floor(Nproj61[4, i])
      
      NoAdults61=paste("ask patches[if pxcor = 112 and pycor = 32 [ sprout-mosqs", Adults61," ]]")
      
      NLCommand(NoAdults61)
      
      Nproj61[4,i]=0
      
      Nproj61=ifelse(Nproj61>0.1, Nproj61, 0)
      
      
      TM62 = matrix(c(ES,0,                   0, 0,
                      ET,LS62[i], 0, 0,
                      0,LT62[i], PS62, 0,
                      0,   0,                         PT62, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs62<-paste("count eggs with [playa = 62 ]")
      
      Juv62<-NLReport(CountEggs62)
      
      Nproj62[1, i-1] = Juv62+Nproj62[1,i-1]
      
      Nproj62[1:4,i] = (TM62%*%Nproj62[1:4 ,i-1])
      
      Adults62=floor(Nproj62[4, i])
      
      NoAdults62=paste("ask patches[if pxcor = 294 and pycor = 52 [ sprout-mosqs", Adults62," ]]")
      
      NLCommand(NoAdults62)
      
      Nproj62[4,i]=0
      
      Nproj62=ifelse(Nproj62>0.1, Nproj62, 0)
      
      
      TM63 = matrix(c(ES,0,                   0, 0,
                      ET,LS63[i], 0, 0,
                      0,LT63[i], PS63, 0,
                      0,   0,                         PT63, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs63<-paste("count eggs with [playa = 63 ]")
      
      Juv63<-NLReport(CountEggs63)
      
      Nproj63[1, i-1] = Juv63+Nproj63[1,i-1]
      
      Nproj63[1:4,i] = (TM63%*%Nproj63[1:4 ,i-1])
      
      Adults63=floor(Nproj63[4, i])
      
      NoAdults63=paste("ask patches[if pxcor = 365 and pycor = 72 [ sprout-mosqs", Adults63," ]]")
      
      NLCommand(NoAdults63)
      
      Nproj63[4,i]=0
      
      Nproj63=ifelse(Nproj63>0.1, Nproj63, 0)
      
      
      TM64 = matrix(c(ES,0,                   0, 0,
                      ET,LS64[i], 0, 0,
                      0,LT64[i], PS64, 0,
                      0,   0,                         PT64, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs64<-paste("count eggs with [playa = 64 ]")
      
      Juv64<-NLReport(CountEggs64)
      
      Nproj64[1, i-1] = Juv64+Nproj64[1,i-1]
      
      Nproj64[1:4,i] = (TM64%*%Nproj64[1:4 ,i-1])
      
      Adults64=floor(Nproj64[4, i])
      
      NoAdults64=paste("ask patches[if pxcor = 390 and pycor = 58 [ sprout-mosqs", Adults64," ]]")
      
      NLCommand(NoAdults64)
      
      Nproj64[4,i]=0
      
      Nproj64=ifelse(Nproj64>0.1, Nproj64, 0)
      
      
      TM65 = matrix(c(ES,0,                   0, 0,
                      ET,LS65[i], 0, 0,
                      0,LT65[i], PS65, 0,
                      0,   0,                         PT65, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs65<-paste("count eggs with [playa = 65 ]")
      
      Juv65<-NLReport(CountEggs65)
      
      Nproj65[1, i-1] = Juv65+Nproj65[1,i-1]
      
      Nproj65[1:4,i] = (TM65%*%Nproj65[1:4 ,i-1])
      
      Adults65=floor(Nproj65[4, i])
      
      NoAdults65=paste("ask patches[if pxcor = 403 and pycor = 256 [ sprout-mosqs", Adults65," ]]")
      
      NLCommand(NoAdults65)
      
      Nproj65[4,i]=0
      
      Nproj65=ifelse(Nproj65>0.1, Nproj65, 0)
      
      
      TM66 = matrix(c(ES,0,                   0, 0,
                      ET,LS66[i], 0, 0,
                      0,LT66[i], PS66, 0,
                      0,   0,                         PT66, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs66<-paste("count eggs with [playa = 66 ]")
      
      Juv66<-NLReport(CountEggs66)
      
      Nproj66[1, i-1] = Juv66+Nproj66[1,i-1]
      
      Nproj66[1:4,i] = (TM66%*%Nproj66[1:4 ,i-1])
      
      Adults66=floor(Nproj66[4, i])
      
      NoAdults66=paste("ask patches[if pxcor = 440 and pycor = 28 [ sprout-mosqs", Adults66," ]]")
      
      NLCommand(NoAdults66)
      
      Nproj66[4,i]=0
      
      Nproj66=ifelse(Nproj66>0.1, Nproj66, 0)
      
      
      TM67 = matrix(c(ES,0,                   0, 0,
                      ET,LS67[i], 0, 0,
                      0,LT67[i], PS67, 0,
                      0,   0,                         PT67, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs67<-paste("count eggs with [playa = 67 ]")
      
      Juv67<-NLReport(CountEggs67)
      
      Nproj67[1, i-1] = Juv67+Nproj67[1,i-1]
      
      Nproj67[1:4,i] = (TM67%*%Nproj67[1:4 ,i-1])
      
      Adults67=floor(Nproj67[4, i])
      
      NoAdults67=paste("ask patches[if pxcor = 443 and pycor = 19 [ sprout-mosqs", Adults67," ]]")
      
      NLCommand(NoAdults67)
      
      Nproj67[4,i]=0
      
      Nproj67=ifelse(Nproj67>0.1, Nproj67, 0)
      
      
      TM68 = matrix(c(ES,0,                   0, 0,
                      ET,LS68[i], 0, 0,
                      0,LT68[i], PS68, 0,
                      0,   0,                         PT68, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs68<-paste("count eggs with [playa = 68 ]")
      
      Juv68<-NLReport(CountEggs68)
      
      Nproj68[1, i-1] = Juv68+Nproj68[1,i-1]
      
      Nproj68[1:4,i] = (TM68%*%Nproj68[1:4 ,i-1])
      
      Adults68=floor(Nproj68[4, i])
      
      NoAdults68=paste("ask patches[if pxcor = 371 and pycor = 56 [ sprout-mosqs", Adults68," ]]")
      
      NLCommand(NoAdults68)
      
      Nproj68[4,i]=0
      
      Nproj68=ifelse(Nproj68>0.1, Nproj68, 0)
      
      
      TM69 = matrix(c(ES,0,                   0, 0,
                      ET,LS69[i], 0, 0,
                      0,LT69[i], PS69, 0,
                      0,   0,                         PT69, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs69<-paste("count eggs with [playa = 69 ]")
      
      Juv69<-NLReport(CountEggs69)
      
      Nproj69[1, i-1] = Juv69+Nproj69[1,i-1]
      
      Nproj69[1:4,i] = (TM69%*%Nproj69[1:4 ,i-1])
      
      Adults69=floor(Nproj69[4, i])
      
      NoAdults69=paste("ask patches[if pxcor = 404 and pycor = 156 [ sprout-mosqs", Adults69," ]]")
      
      NLCommand(NoAdults69)
      
      Nproj69[4,i]=0
      
      Nproj69=ifelse(Nproj69>0.1, Nproj69, 0)
      
      
      TM70 = matrix(c(ES,0,                   0, 0,
                      ET,LS70[i], 0, 0,
                      0,LT70[i], PS70, 0,
                      0,   0,                         PT70, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs70<-paste("count eggs with [playa = 70 ]")
      
      Juv70<-NLReport(CountEggs70)
      
      Nproj70[1, i-1] = Juv70+Nproj70[1,i-1]
      
      Nproj70[1:4,i] = (TM70%*%Nproj70[1:4 ,i-1])
      
      Adults70=floor(Nproj70[4, i])
      
      NoAdults70=paste("ask patches[if pxcor = 349 and pycor = 299 [ sprout-mosqs", Adults70," ]]")
      
      NLCommand(NoAdults70)
      
      Nproj70[4,i]=0
      
      Nproj70=ifelse(Nproj70>0.1, Nproj70, 0)
      
      
      TM71 = matrix(c(ES,0,                   0, 0,
                      ET,LS71[i], 0, 0,
                      0,LT71[i], PS71, 0,
                      0,   0,                         PT71, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs71<-paste("count eggs with [playa = 71 ]")
      
      Juv71<-NLReport(CountEggs71)
      
      Nproj71[1, i-1] = Juv71+Nproj71[1,i-1]
      
      Nproj71[1:4,i] = (TM71%*%Nproj71[1:4 ,i-1])
      
      Adults71=floor(Nproj71[4, i])
      
      NoAdults71=paste("ask patches[if pxcor = 391 and pycor = 240 [ sprout-mosqs", Adults71," ]]")
      
      NLCommand(NoAdults71)
      
      Nproj71[4,i]=0
      
      Nproj71=ifelse(Nproj71>0.1, Nproj71, 0)
      
      
      TM72 = matrix(c(ES,0,                   0, 0,
                      ET,LS72[i], 0, 0,
                      0,LT72[i], PS72, 0,
                      0,   0,                         PT72, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs72<-paste("count eggs with [playa = 72 ]")
      
      Juv72<-NLReport(CountEggs72)
      
      Nproj72[1, i-1] = Juv72+Nproj72[1,i-1]
      
      Nproj72[1:4,i] = (TM72%*%Nproj72[1:4 ,i-1])
      
      Adults72=floor(Nproj72[4, i])
      
      NoAdults72=paste("ask patches[if pxcor = 334 and pycor = 303 [ sprout-mosqs", Adults72," ]]")
      
      NLCommand(NoAdults72)
      
      Nproj72[4,i]=0
      
      Nproj72=ifelse(Nproj72>0.1, Nproj72, 0)
      
      
      TM73 = matrix(c(ES,0,                   0, 0,
                      ET,LS73[i], 0, 0,
                      0,LT73[i], PS73, 0,
                      0,   0,                         PT73, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs73<-paste("count eggs with [playa = 73 ]")
      
      Juv73<-NLReport(CountEggs73)
      
      Nproj73[1, i-1] = Juv73+Nproj73[1,i-1]
      
      Nproj73[1:4,i] = (TM73%*%Nproj73[1:4 ,i-1])
      
      Adults73=floor(Nproj73[4, i])
      
      NoAdults73=paste("ask patches[if pxcor = 317 and pycor = 315 [ sprout-mosqs", Adults73," ]]")
      
      NLCommand(NoAdults73)
      
      Nproj73[4,i]=0
      
      Nproj73=ifelse(Nproj73>0.1, Nproj73, 0)
      
      
      TM74 = matrix(c(ES,0,                   0, 0,
                      ET,LS74[i], 0, 0,
                      0,LT74[i], PS74, 0,
                      0,   0,                         PT74, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs74<-paste("count eggs with [playa = 74 ]")
      
      Juv74<-NLReport(CountEggs74)
      
      Nproj74[1, i-1] = Juv74+Nproj74[1,i-1]
      
      Nproj74[1:4,i] = (TM74%*%Nproj74[1:4 ,i-1])
      
      Adults74=floor(Nproj74[4, i])
      
      NoAdults74=paste("ask patches[if pxcor = 291 and pycor = 310 [ sprout-mosqs", Adults74," ]]")
      
      NLCommand(NoAdults74)
      
      Nproj74[4,i]=0
      
      Nproj74=ifelse(Nproj74>0.1, Nproj74, 0)
      
      
      TM75 = matrix(c(ES,0,                   0, 0,
                      ET,LS75[i], 0, 0,
                      0,LT75[i], PS75, 0,
                      0,   0,                         PT75, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs75<-paste("count eggs with [playa = 75 ]")
      
      Juv75<-NLReport(CountEggs75)
      
      Nproj75[1, i-1] = Juv75+Nproj75[1,i-1]
      
      Nproj75[1:4,i] = (TM75%*%Nproj75[1:4 ,i-1])
      
      Adults75=floor(Nproj75[4, i])
      
      NoAdults75=paste("ask patches[if pxcor = 426 and pycor = 323 [ sprout-mosqs", Adults75," ]]")
      
      NLCommand(NoAdults75)
      
      Nproj75[4,i]=0
      
      Nproj75=ifelse(Nproj75>0.1, Nproj75, 0)
      
      
      TM76 = matrix(c(ES,0,                   0, 0,
                      ET,LS76[i], 0, 0,
                      0,LT76[i], PS76, 0,
                      0,   0,                         PT76, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs76<-paste("count eggs with [playa = 76 ]")
      
      Juv76<-NLReport(CountEggs76)
      
      Nproj76[1, i-1] = Juv76+Nproj76[1,i-1]
      
      Nproj76[1:4,i] = (TM76%*%Nproj76[1:4 ,i-1])
      
      Adults76=floor(Nproj76[4, i])
      
      NoAdults76=paste("ask patches[if pxcor = 250 and pycor = 317 [ sprout-mosqs", Adults76," ]]")
      
      NLCommand(NoAdults76)
      
      Nproj76[4,i]=0
      
      Nproj76=ifelse(Nproj76>0.1, Nproj76, 0)
      
      
      TM77 = matrix(c(ES,0,                   0, 0,
                      ET,LS77[i], 0, 0,
                      0,LT77[i], PS77, 0,
                      0,   0,                         PT77, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs77<-paste("count eggs with [playa = 77 ]")
      
      Juv77<-NLReport(CountEggs77)
      
      Nproj77[1, i-1] = Juv77+Nproj77[1,i-1]
      
      Nproj77[1:4,i] = (TM77%*%Nproj77[1:4 ,i-1])
      
      Adults77=floor(Nproj77[4, i])
      
      NoAdults77=paste("ask patches[if pxcor = 264 and pycor = 319 [ sprout-mosqs", Adults77," ]]")
      
      NLCommand(NoAdults77)
      
      Nproj77[4,i]=0
      
      Nproj77=ifelse(Nproj77>0.1, Nproj77, 0)
      
      
      TM78 = matrix(c(ES,0,                   0, 0,
                      ET,LS78[i], 0, 0,
                      0,LT78[i], PS78, 0,
                      0,   0,                         PT78, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs78<-paste("count eggs with [playa = 78 ]")
      
      Juv78<-NLReport(CountEggs78)
      
      Nproj78[1, i-1] = Juv78+Nproj78[1,i-1]
      
      Nproj78[1:4,i] = (TM78%*%Nproj78[1:4 ,i-1])
      
      Adults78=floor(Nproj78[4, i])
      
      NoAdults78=paste("ask patches[if pxcor = 208 and pycor = 319 [ sprout-mosqs", Adults78," ]]")
      
      NLCommand(NoAdults78)
      
      Nproj78[4,i]=0
      
      Nproj78=ifelse(Nproj78>0.1, Nproj78, 0)
      
      
      TM79 = matrix(c(ES,0,                   0, 0,
                      ET,LS79[i], 0, 0,
                      0,LT79[i], PS79, 0,
                      0,   0,                         PT79, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs79<-paste("count eggs with [playa = 79 ]")
      
      Juv79<-NLReport(CountEggs79)
      
      Nproj79[1, i-1] = Juv79+Nproj79[1,i-1]
      
      Nproj79[1:4,i] = (TM79%*%Nproj79[1:4 ,i-1])
      
      Adults79=floor(Nproj79[4, i])
      
      NoAdults79=paste("ask patches[if pxcor = 440 and pycor = 241 [ sprout-mosqs", Adults79," ]]")
      
      NLCommand(NoAdults79)
      
      Nproj79[4,i]=0
      
      Nproj79=ifelse(Nproj79>0.1, Nproj79, 0)
      
      
      TM80 = matrix(c(ES,0,                   0, 0,
                      ET,LS80[i], 0, 0,
                      0,LT80[i], PS80, 0,
                      0,   0,                         PT80, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs80<-paste("count eggs with [playa = 80 ]")
      
      Juv80<-NLReport(CountEggs80)
      
      Nproj80[1, i-1] = Juv80+Nproj80[1,i-1]
      
      Nproj80[1:4,i] = (TM80%*%Nproj80[1:4 ,i-1])
      
      Adults80=floor(Nproj80[4, i])
      
      NoAdults80=paste("ask patches[if pxcor = 426 and pycor = 314 [ sprout-mosqs", Adults80," ]]")
      
      NLCommand(NoAdults80)
      
      Nproj80[4,i]=0
      
      Nproj80=ifelse(Nproj80>0.1, Nproj80, 0)
      
      
      TM81 = matrix(c(ES,0,                   0, 0,
                      ET,LS81[i], 0, 0,
                      0,LT81[i], PS81, 0,
                      0,   0,                         PT81, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs81<-paste("count eggs with [playa = 81 ]")
      
      Juv81<-NLReport(CountEggs81)
      
      Nproj81[1, i-1] = Juv81+Nproj81[1,i-1]
      
      Nproj81[1:4,i] = (TM81%*%Nproj81[1:4 ,i-1])
      
      Adults81=floor(Nproj81[4, i])
      
      NoAdults81=paste("ask patches[if pxcor = 233 and pycor = 164 [ sprout-mosqs", Adults81," ]]")
      
      NLCommand(NoAdults81)
      
      Nproj81[4,i]=0
      
      Nproj81=ifelse(Nproj81>0.1, Nproj81, 0)
      
      
      TM82 = matrix(c(ES,0,                   0, 0,
                      ET,LS82[i], 0, 0,
                      0,LT82[i], PS82, 0,
                      0,   0,                         PT82, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs82<-paste("count eggs with [playa = 82 ]")
      
      Juv82<-NLReport(CountEggs82)
      
      Nproj82[1, i-1] = Juv82+Nproj82[1,i-1]
      
      Nproj82[1:4,i] = (TM82%*%Nproj82[1:4 ,i-1])
      
      Adults82=floor(Nproj82[4, i])
      
      NoAdults82=paste("ask patches[if pxcor = 399 and pycor = 216 [ sprout-mosqs", Adults82," ]]")
      
      NLCommand(NoAdults82)
      
      Nproj82[4,i]=0
      
      Nproj82=ifelse(Nproj82>0.1, Nproj82, 0)
      
      
      TM83 = matrix(c(ES,0,                   0, 0,
                      ET,LS83[i], 0, 0,
                      0,LT83[i], PS83, 0,
                      0,   0,                         PT83, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs83<-paste("count eggs with [playa = 83 ]")
      
      Juv83<-NLReport(CountEggs83)
      
      Nproj83[1, i-1] = Juv83+Nproj83[1,i-1]
      
      Nproj83[1:4,i] = (TM83%*%Nproj83[1:4 ,i-1])
      
      Adults83=floor(Nproj83[4, i])
      
      NoAdults83=paste("ask patches[if pxcor = 392 and pycor = 282 [ sprout-mosqs", Adults83," ]]")
      
      NLCommand(NoAdults83)
      
      Nproj83[4,i]=0
      
      Nproj83=ifelse(Nproj83>0.1, Nproj83, 0)
      
      
      TM84 = matrix(c(ES,0,                   0, 0,
                      ET,LS84[i], 0, 0,
                      0,LT84[i], PS84, 0,
                      0,   0,                         PT84, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs84<-paste("count eggs with [playa = 84 ]")
      
      Juv84<-NLReport(CountEggs84)
      
      Nproj84[1, i-1] = Juv84+Nproj84[1,i-1]
      
      Nproj84[1:4,i] = (TM84%*%Nproj84[1:4 ,i-1])
      
      Adults84=floor(Nproj84[4, i])
      
      NoAdults84=paste("ask patches[if pxcor = 406 and pycor = 301 [ sprout-mosqs", Adults84," ]]")
      
      NLCommand(NoAdults84)
      
      Nproj84[4,i]=0
      
      Nproj84=ifelse(Nproj84>0.1, Nproj84, 0)
      
      
      TM85 = matrix(c(ES,0,                   0, 0,
                      ET,LS85[i], 0, 0,
                      0,LT85[i], PS85, 0,
                      0,   0,                         PT85, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs85<-paste("count eggs with [playa = 85 ]")
      
      Juv85<-NLReport(CountEggs85)
      
      Nproj85[1, i-1] = Juv85+Nproj85[1,i-1]
      
      Nproj85[1:4,i] = (TM85%*%Nproj85[1:4 ,i-1])
      
      Adults85=floor(Nproj85[4, i])
      
      NoAdults85=paste("ask patches[if pxcor = 220 and pycor = 179 [ sprout-mosqs", Adults85," ]]")
      
      NLCommand(NoAdults85)
      
      Nproj85[4,i]=0
      
      Nproj85=ifelse(Nproj85>0.1, Nproj85, 0)
      
      
      TM86 = matrix(c(ES,0,                   0, 0,
                      ET,LS86[i], 0, 0,
                      0,LT86[i], PS86, 0,
                      0,   0,                         PT86, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs86<-paste("count eggs with [playa = 86 ]")
      
      Juv86<-NLReport(CountEggs86)
      
      Nproj86[1, i-1] = Juv86+Nproj86[1,i-1]
      
      Nproj86[1:4,i] = (TM86%*%Nproj86[1:4 ,i-1])
      
      Adults86=floor(Nproj86[4, i])
      
      NoAdults86=paste("ask patches[if pxcor = 233 and pycor = 255 [ sprout-mosqs", Adults86," ]]")
      
      NLCommand(NoAdults86)
      
      Nproj86[4,i]=0
      
      Nproj86=ifelse(Nproj86>0.1, Nproj86, 0)
      
      
      TM87 = matrix(c(ES,0,                   0, 0,
                      ET,LS87[i], 0, 0,
                      0,LT87[i], PS87, 0,
                      0,   0,                         PT87, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs87<-paste("count eggs with [playa = 87 ]")
      
      Juv87<-NLReport(CountEggs87)
      
      Nproj87[1, i-1] = Juv87+Nproj87[1,i-1]
      
      Nproj87[1:4,i] = (TM87%*%Nproj87[1:4 ,i-1])
      
      Adults87=floor(Nproj87[4, i])
      
      NoAdults87=paste("ask patches[if pxcor = 221 and pycor = 178 [ sprout-mosqs", Adults87," ]]")
      
      NLCommand(NoAdults87)
      
      Nproj87[4,i]=0
      
      Nproj87=ifelse(Nproj87>0.1, Nproj87, 0)
      
      
      TM88 = matrix(c(ES,0,                   0, 0,
                      ET,LS88[i], 0, 0,
                      0,LT88[i], PS88, 0,
                      0,   0,                         PT88, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs88<-paste("count eggs with [playa = 88 ]")
      
      Juv88<-NLReport(CountEggs88)
      
      Nproj88[1, i-1] = Juv88+Nproj88[1,i-1]
      
      Nproj88[1:4,i] = (TM88%*%Nproj88[1:4 ,i-1])
      
      Adults88=floor(Nproj88[4, i])
      
      NoAdults88=paste("ask patches[if pxcor = 211 and pycor = 125 [ sprout-mosqs", Adults88," ]]")
      
      NLCommand(NoAdults88)
      
      Nproj88[4,i]=0
      
      Nproj88=ifelse(Nproj88>0.1, Nproj88, 0)
      
      
      TM89 = matrix(c(ES,0,                   0, 0,
                      ET,LS89[i], 0, 0,
                      0,LT89[i], PS89, 0,
                      0,   0,                         PT89, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs89<-paste("count eggs with [playa = 89 ]")
      
      Juv89<-NLReport(CountEggs89)
      
      Nproj89[1, i-1] = Juv89+Nproj89[1,i-1]
      
      Nproj89[1:4,i] = (TM89%*%Nproj89[1:4 ,i-1])
      
      Adults89=floor(Nproj89[4, i])
      
      NoAdults89=paste("ask patches[if pxcor = 145 and pycor = 124 [ sprout-mosqs", Adults89," ]]")
      
      NLCommand(NoAdults89)
      
      Nproj89[4,i]=0
      
      Nproj89=ifelse(Nproj89>0.1, Nproj89, 0)
      
      
      TM90 = matrix(c(ES,0,                   0, 0,
                      ET,LS90[i], 0, 0,
                      0,LT90[i], PS90, 0,
                      0,   0,                         PT90, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs90<-paste("count eggs with [playa = 90 ]")
      
      Juv90<-NLReport(CountEggs90)
      
      Nproj90[1, i-1] = Juv90+Nproj90[1,i-1]
      
      Nproj90[1:4,i] = (TM90%*%Nproj90[1:4 ,i-1])
      
      Adults90=floor(Nproj90[4, i])
      
      NoAdults90=paste("ask patches[if pxcor = 211 and pycor = 127 [ sprout-mosqs", Adults90," ]]")
      
      NLCommand(NoAdults90)
      
      Nproj90[4,i]=0
      
      Nproj90=ifelse(Nproj90>0.1, Nproj90, 0)
      
      
      TM91 = matrix(c(ES,0,                   0, 0,
                      ET,LS91[i], 0, 0,
                      0,LT91[i], PS91, 0,
                      0,   0,                         PT91, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs91<-paste("count eggs with [playa = 91 ]")
      
      Juv91<-NLReport(CountEggs91)
      
      Nproj91[1, i-1] = Juv91+Nproj91[1,i-1]
      
      Nproj91[1:4,i] = (TM91%*%Nproj91[1:4 ,i-1])
      
      Adults91=floor(Nproj91[4, i])
      
      NoAdults91=paste("ask patches[if pxcor = 218 and pycor = 180 [ sprout-mosqs", Adults91," ]]")
      
      NLCommand(NoAdults91)
      
      Nproj91[4,i]=0
      
      Nproj91=ifelse(Nproj91>0.1, Nproj91, 0)
      
      
      TM92 = matrix(c(ES,0,                   0, 0,
                      ET,LS92[i], 0, 0,
                      0,LT92[i], PS92, 0,
                      0,   0,                         PT92, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs92<-paste("count eggs with [playa = 92 ]")
      
      Juv92<-NLReport(CountEggs92)
      
      Nproj92[1, i-1] = Juv92+Nproj92[1,i-1]
      
      Nproj92[1:4,i] = (TM92%*%Nproj92[1:4 ,i-1])
      
      Adults92=floor(Nproj92[4, i])
      
      NoAdults92=paste("ask patches[if pxcor = 203 and pycor = 185 [ sprout-mosqs", Adults92," ]]")
      
      NLCommand(NoAdults92)
      
      Nproj92[4,i]=0
      
      Nproj92=ifelse(Nproj92>0.1, Nproj92, 0)
      
      
      TM93 = matrix(c(ES,0,                   0, 0,
                      ET,LS93[i], 0, 0,
                      0,LT93[i], PS93, 0,
                      0,   0,                         PT93, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs93<-paste("count eggs with [playa = 93 ]")
      
      Juv93<-NLReport(CountEggs93)
      
      Nproj93[1, i-1] = Juv93+Nproj93[1,i-1]
      
      Nproj93[1:4,i] = (TM93%*%Nproj93[1:4 ,i-1])
      
      Adults93=floor(Nproj93[4, i])
      
      NoAdults93=paste("ask patches[if pxcor = 85 and pycor = 48 [ sprout-mosqs", Adults93," ]]")
      
      NLCommand(NoAdults93)
      
      Nproj93[4,i]=0
      
      Nproj93=ifelse(Nproj93>0.1, Nproj93, 0)
      
      
      TM94 = matrix(c(ES,0,                   0, 0,
                      ET,LS94[i], 0, 0,
                      0,LT94[i], PS94, 0,
                      0,   0,                         PT94, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs94<-paste("count eggs with [playa = 94 ]")
      
      Juv94<-NLReport(CountEggs94)
      
      Nproj94[1, i-1] = Juv94+Nproj94[1,i-1]
      
      Nproj94[1:4,i] = (TM94%*%Nproj94[1:4 ,i-1])
      
      Adults94=floor(Nproj94[4, i])
      
      NoAdults94=paste("ask patches[if pxcor = 30 and pycor = 66 [ sprout-mosqs", Adults94," ]]")
      
      NLCommand(NoAdults94)
      
      Nproj94[4,i]=0
      
      Nproj94=ifelse(Nproj94>0.1, Nproj94, 0)
      
      
      TM95 = matrix(c(ES,0,                   0, 0,
                      ET,LS95[i], 0, 0,
                      0,LT95[i], PS95, 0,
                      0,   0,                         PT95, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs95<-paste("count eggs with [playa = 95 ]")
      
      Juv95<-NLReport(CountEggs95)
      
      Nproj95[1, i-1] = Juv95+Nproj95[1,i-1]
      
      Nproj95[1:4,i] = (TM95%*%Nproj95[1:4 ,i-1])
      
      Adults95=floor(Nproj95[4, i])
      
      NoAdults95=paste("ask patches[if pxcor = 53 and pycor = 76 [ sprout-mosqs", Adults95," ]]")
      
      NLCommand(NoAdults95)
      
      Nproj95[4,i]=0
      
      Nproj95=ifelse(Nproj95>0.1, Nproj95, 0)
      
      
      TM96 = matrix(c(ES,0,                   0, 0,
                      ET,LS96[i], 0, 0,
                      0,LT96[i], PS96, 0,
                      0,   0,                         PT96, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs96<-paste("count eggs with [playa = 96 ]")
      
      Juv96<-NLReport(CountEggs96)
      
      Nproj96[1, i-1] = Juv96+Nproj96[1,i-1]
      
      Nproj96[1:4,i] = (TM96%*%Nproj96[1:4 ,i-1])
      
      Adults96=floor(Nproj96[4, i])
      
      NoAdults96=paste("ask patches[if pxcor = 41 and pycor = 108 [ sprout-mosqs", Adults96," ]]")
      
      NLCommand(NoAdults96)
      
      Nproj96[4,i]=0
      
      Nproj96=ifelse(Nproj96>0.1, Nproj96, 0)
      
      
      TM97 = matrix(c(ES,0,                   0, 0,
                      ET,LS97[i], 0, 0,
                      0,LT97[i], PS97, 0,
                      0,   0,                         PT97, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs97<-paste("count eggs with [playa = 97 ]")
      
      Juv97<-NLReport(CountEggs97)
      
      Nproj97[1, i-1] = Juv97+Nproj97[1,i-1]
      
      Nproj97[1:4,i] = (TM97%*%Nproj97[1:4 ,i-1])
      
      Adults97=floor(Nproj97[4, i])
      
      NoAdults97=paste("ask patches[if pxcor = 58 and pycor = 38 [ sprout-mosqs", Adults97," ]]")
      
      NLCommand(NoAdults97)
      
      Nproj97[4,i]=0
      
      Nproj97=ifelse(Nproj97>0.1, Nproj97, 0)
      
      
      TM98 = matrix(c(ES,0,                   0, 0,
                      ET,LS98[i], 0, 0,
                      0,LT98[i], PS98, 0,
                      0,   0,                         PT98, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs98<-paste("count eggs with [playa = 98 ]")
      
      Juv98<-NLReport(CountEggs98)
      
      Nproj98[1, i-1] = Juv98+Nproj98[1,i-1]
      
      Nproj98[1:4,i] = (TM98%*%Nproj98[1:4 ,i-1])
      
      Adults98=floor(Nproj98[4, i])
      
      NoAdults98=paste("ask patches[if pxcor = 155 and pycor = 37 [ sprout-mosqs", Adults98," ]]")
      
      NLCommand(NoAdults98)
      
      Nproj98[4,i]=0
      
      Nproj98=ifelse(Nproj98>0.1, Nproj98, 0)
      
      
      TM99 = matrix(c(ES,0,                   0, 0,
                      ET,LS99[i], 0, 0,
                      0,LT99[i], PS99, 0,
                      0,   0,                         PT99, 1)
                    , nr = 4, byrow=T) 
      
      
      
      CountEggs99<-paste("count eggs with [playa = 99 ]")
      
      Juv99<-NLReport(CountEggs99)
      
      Nproj99[1, i-1] = Juv99+Nproj99[1,i-1]
      
      Nproj99[1:4,i] = (TM99%*%Nproj99[1:4 ,i-1])
      
      Adults99=floor(Nproj99[4, i])
      
      NoAdults99=paste("ask patches[if pxcor = 210 and pycor = 96 [ sprout-mosqs", Adults99," ]]")
      
      NLCommand(NoAdults99)
      
      Nproj99[4,i]=0
      
      Nproj99=ifelse(Nproj99>0.1, Nproj99, 0)
      
      
      TM100 = matrix(c(ES,0,                   0, 0,
                       ET,LS100[i], 0, 0,
                       0,LT100[i], PS100, 0,
                       0,   0,                         PT100, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs100<-paste("count eggs with [playa = 100 ]")
      
      Juv100<-NLReport(CountEggs100)
      
      Nproj100[1, i-1] = Juv100+Nproj100[1,i-1]
      
      Nproj100[1:4,i] = (TM100%*%Nproj100[1:4 ,i-1])
      
      Adults100=floor(Nproj100[4, i])
      
      NoAdults100=paste("ask patches[if pxcor = 140 and pycor = 100 [ sprout-mosqs", Adults100," ]]")
      
      NLCommand(NoAdults100)
      
      Nproj100[4,i]=0
      
      Nproj100=ifelse(Nproj100>0.1, Nproj100, 0)
      
      
      TM101 = matrix(c(ES,0,                   0, 0,
                       ET,LS101[i], 0, 0,
                       0,LT101[i], PS101, 0,
                       0,   0,                         PT101, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs101<-paste("count eggs with [playa = 101 ]")
      
      Juv101<-NLReport(CountEggs101)
      
      Nproj101[1, i-1] = Juv101+Nproj101[1,i-1]
      
      Nproj101[1:4,i] = (TM101%*%Nproj101[1:4 ,i-1])
      
      Adults101=floor(Nproj101[4, i])
      
      NoAdults101=paste("ask patches[if pxcor = 157 and pycor = 106 [ sprout-mosqs", Adults101," ]]")
      
      NLCommand(NoAdults101)
      
      Nproj101[4,i]=0
      
      Nproj101=ifelse(Nproj101>0.1, Nproj101, 0)
      
      
      TM102 = matrix(c(ES,0,                   0, 0,
                       ET,LS102[i], 0, 0,
                       0,LT102[i], PS102, 0,
                       0,   0,                         PT102, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs102<-paste("count eggs with [playa = 102 ]")
      
      Juv102<-NLReport(CountEggs102)
      
      Nproj102[1, i-1] = Juv102+Nproj102[1,i-1]
      
      Nproj102[1:4,i] = (TM102%*%Nproj102[1:4 ,i-1])
      
      Adults102=floor(Nproj102[4, i])
      
      NoAdults102=paste("ask patches[if pxcor = 96 and pycor = 92 [ sprout-mosqs", Adults102," ]]")
      
      NLCommand(NoAdults102)
      
      Nproj102[4,i]=0
      
      Nproj102=ifelse(Nproj102>0.1, Nproj102, 0)
      
      
      TM103 = matrix(c(ES,0,                   0, 0,
                       ET,LS103[i], 0, 0,
                       0,LT103[i], PS103, 0,
                       0,   0,                         PT103, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs103<-paste("count eggs with [playa = 103 ]")
      
      Juv103<-NLReport(CountEggs103)
      
      Nproj103[1, i-1] = Juv103+Nproj103[1,i-1]
      
      Nproj103[1:4,i] = (TM103%*%Nproj103[1:4 ,i-1])
      
      Adults103=floor(Nproj103[4, i])
      
      NoAdults103=paste("ask patches[if pxcor = 95 and pycor = 79 [ sprout-mosqs", Adults103," ]]")
      
      NLCommand(NoAdults103)
      
      Nproj103[4,i]=0
      
      Nproj103=ifelse(Nproj103>0.1, Nproj103, 0)
      
      
      TM104 = matrix(c(ES,0,                   0, 0,
                       ET,LS104[i], 0, 0,
                       0,LT104[i], PS104, 0,
                       0,   0,                         PT104, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs104<-paste("count eggs with [playa = 104 ]")
      
      Juv104<-NLReport(CountEggs104)
      
      Nproj104[1, i-1] = Juv104+Nproj104[1,i-1]
      
      Nproj104[1:4,i] = (TM104%*%Nproj104[1:4 ,i-1])
      
      Adults104=floor(Nproj104[4, i])
      
      NoAdults104=paste("ask patches[if pxcor = 238 and pycor = 316 [ sprout-mosqs", Adults104," ]]")
      
      NLCommand(NoAdults104)
      
      Nproj104[4,i]=0
      
      Nproj104=ifelse(Nproj104>0.1, Nproj104, 0)
      
      
      TM105 = matrix(c(ES,0,                   0, 0,
                       ET,LS105[i], 0, 0,
                       0,LT105[i], PS105, 0,
                       0,   0,                         PT105, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs105<-paste("count eggs with [playa = 105 ]")
      
      Juv105<-NLReport(CountEggs105)
      
      Nproj105[1, i-1] = Juv105+Nproj105[1,i-1]
      
      Nproj105[1:4,i] = (TM105%*%Nproj105[1:4 ,i-1])
      
      Adults105=floor(Nproj105[4, i])
      
      NoAdults105=paste("ask patches[if pxcor = 233 and pycor = 311 [ sprout-mosqs", Adults105," ]]")
      
      NLCommand(NoAdults105)
      
      Nproj105[4,i]=0
      
      Nproj105=ifelse(Nproj105>0.1, Nproj105, 0)
      
      
      TM106 = matrix(c(ES,0,                   0, 0,
                       ET,LS106[i], 0, 0,
                       0,LT106[i], PS106, 0,
                       0,   0,                         PT106, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs106<-paste("count eggs with [playa = 106 ]")
      
      Juv106<-NLReport(CountEggs106)
      
      Nproj106[1, i-1] = Juv106+Nproj106[1,i-1]
      
      Nproj106[1:4,i] = (TM106%*%Nproj106[1:4 ,i-1])
      
      Adults106=floor(Nproj106[4, i])
      
      NoAdults106=paste("ask patches[if pxcor = 178 and pycor = 286 [ sprout-mosqs", Adults106," ]]")
      
      NLCommand(NoAdults106)
      
      Nproj106[4,i]=0
      
      Nproj106=ifelse(Nproj106>0.1, Nproj106, 0)
      
      
      TM107 = matrix(c(ES,0,                   0, 0,
                       ET,LS107[i], 0, 0,
                       0,LT107[i], PS107, 0,
                       0,   0,                         PT107, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs107<-paste("count eggs with [playa = 107 ]")
      
      Juv107<-NLReport(CountEggs107)
      
      Nproj107[1, i-1] = Juv107+Nproj107[1,i-1]
      
      Nproj107[1:4,i] = (TM107%*%Nproj107[1:4 ,i-1])
      
      Adults107=floor(Nproj107[4, i])
      
      NoAdults107=paste("ask patches[if pxcor = 80 and pycor = 252 [ sprout-mosqs", Adults107," ]]")
      
      NLCommand(NoAdults107)
      
      Nproj107[4,i]=0
      
      Nproj107=ifelse(Nproj107>0.1, Nproj107, 0)
      
      
      TM109 = matrix(c(ES,0,                   0, 0,
                       ET,LS109[i], 0, 0,
                       0,LT109[i], PS109, 0,
                       0,   0,                         PT109, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs109<-paste("count eggs with [playa = 109 ]")
      
      Juv109<-NLReport(CountEggs109)
      
      Nproj109[1, i-1] = Juv109+Nproj109[1,i-1]
      
      Nproj109[1:4,i] = (TM109%*%Nproj109[1:4 ,i-1])
      
      Adults109=floor(Nproj109[4, i])
      
      NoAdults109=paste("ask patches[if pxcor = 155 and pycor = 108 [ sprout-mosqs", Adults109," ]]")
      
      NLCommand(NoAdults109)
      
      Nproj109[4,i]=0
      
      Nproj109=ifelse(Nproj109>0.1, Nproj109, 0)
      
      
      TM110 = matrix(c(ES,0,                   0, 0,
                       ET,LS110[i], 0, 0,
                       0,LT110[i], PS110, 0,
                       0,   0,                         PT110, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs110<-paste("count eggs with [playa = 110 ]")
      
      Juv110<-NLReport(CountEggs110)
      
      Nproj110[1, i-1] = Juv110+Nproj110[1,i-1]
      
      Nproj110[1:4,i] = (TM110%*%Nproj110[1:4 ,i-1])
      
      Adults110=floor(Nproj110[4, i])
      
      NoAdults110=paste("ask patches[if pxcor = 145 and pycor = 115 [ sprout-mosqs", Adults110," ]]")
      
      NLCommand(NoAdults110)
      
      Nproj110[4,i]=0
      
      Nproj110=ifelse(Nproj110>0.1, Nproj110, 0)
      
      
      TM111 = matrix(c(ES,0,                   0, 0,
                       ET,LS111[i], 0, 0,
                       0,LT111[i], PS111, 0,
                       0,   0,                         PT111, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs111<-paste("count eggs with [playa = 111 ]")
      
      Juv111<-NLReport(CountEggs111)
      
      Nproj111[1, i-1] = Juv111+Nproj111[1,i-1]
      
      Nproj111[1:4,i] = (TM111%*%Nproj111[1:4 ,i-1])
      
      Adults111=floor(Nproj111[4, i])
      
      NoAdults111=paste("ask patches[if pxcor = 142 and pycor = 122 [ sprout-mosqs", Adults111," ]]")
      
      NLCommand(NoAdults111)
      
      Nproj111[4,i]=0
      
      Nproj111=ifelse(Nproj111>0.1, Nproj111, 0)
      
      
      TM112 = matrix(c(ES,0,                   0, 0,
                       ET,LS112[i], 0, 0,
                       0,LT112[i], PS112, 0,
                       0,   0,                         PT112, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs112<-paste("count eggs with [playa = 112 ]")
      
      Juv112<-NLReport(CountEggs112)
      
      Nproj112[1, i-1] = Juv112+Nproj112[1,i-1]
      
      Nproj112[1:4,i] = (TM112%*%Nproj112[1:4 ,i-1])
      
      Adults112=floor(Nproj112[4, i])
      
      NoAdults112=paste("ask patches[if pxcor = 105 and pycor = 111 [ sprout-mosqs", Adults112," ]]")
      
      NLCommand(NoAdults112)
      
      Nproj112[4,i]=0
      
      Nproj112=ifelse(Nproj112>0.1, Nproj112, 0)
      
      
      TM113 = matrix(c(ES,0,                   0, 0,
                       ET,LS113[i], 0, 0,
                       0,LT113[i], PS113, 0,
                       0,   0,                         PT113, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs113<-paste("count eggs with [playa = 113 ]")
      
      Juv113<-NLReport(CountEggs113)
      
      Nproj113[1, i-1] = Juv113+Nproj113[1,i-1]
      
      Nproj113[1:4,i] = (TM113%*%Nproj113[1:4 ,i-1])
      
      Adults113=floor(Nproj113[4, i])
      
      NoAdults113=paste("ask patches[if pxcor = 161 and pycor = 117 [ sprout-mosqs", Adults113," ]]")
      
      NLCommand(NoAdults113)
      
      Nproj113[4,i]=0
      
      Nproj113=ifelse(Nproj113>0.1, Nproj113, 0)
      
      
      TM114 = matrix(c(ES,0,                   0, 0,
                       ET,LS114[i], 0, 0,
                       0,LT114[i], PS114, 0,
                       0,   0,                         PT114, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs114<-paste("count eggs with [playa = 114 ]")
      
      Juv114<-NLReport(CountEggs114)
      
      Nproj114[1, i-1] = Juv114+Nproj114[1,i-1]
      
      Nproj114[1:4,i] = (TM114%*%Nproj114[1:4 ,i-1])
      
      Adults114=floor(Nproj114[4, i])
      
      NoAdults114=paste("ask patches[if pxcor = 213 and pycor = 143 [ sprout-mosqs", Adults114," ]]")
      
      NLCommand(NoAdults114)
      
      Nproj114[4,i]=0
      
      Nproj114=ifelse(Nproj114>0.1, Nproj114, 0)
      
      
      TM115 = matrix(c(ES,0,                   0, 0,
                       ET,LS115[i], 0, 0,
                       0,LT115[i], PS115, 0,
                       0,   0,                         PT115, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs115<-paste("count eggs with [playa = 115 ]")
      
      Juv115<-NLReport(CountEggs115)
      
      Nproj115[1, i-1] = Juv115+Nproj115[1,i-1]
      
      Nproj115[1:4,i] = (TM115%*%Nproj115[1:4 ,i-1])
      
      Adults115=floor(Nproj115[4, i])
      
      NoAdults115=paste("ask patches[if pxcor = 173 and pycor = 179 [ sprout-mosqs", Adults115," ]]")
      
      NLCommand(NoAdults115)
      
      Nproj115[4,i]=0
      
      Nproj115=ifelse(Nproj115>0.1, Nproj115, 0)
      
      
      TM116 = matrix(c(ES,0,                   0, 0,
                       ET,LS116[i], 0, 0,
                       0,LT116[i], PS116, 0,
                       0,   0,                         PT116, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs116<-paste("count eggs with [playa = 116 ]")
      
      Juv116<-NLReport(CountEggs116)
      
      Nproj116[1, i-1] = Juv116+Nproj116[1,i-1]
      
      Nproj116[1:4,i] = (TM116%*%Nproj116[1:4 ,i-1])
      
      Adults116=floor(Nproj116[4, i])
      
      NoAdults116=paste("ask patches[if pxcor = 160 and pycor = 159 [ sprout-mosqs", Adults116," ]]")
      
      NLCommand(NoAdults116)
      
      Nproj116[4,i]=0
      
      Nproj116=ifelse(Nproj116>0.1, Nproj116, 0)
      
      
      TM117 = matrix(c(ES,0,                   0, 0,
                       ET,LS117[i], 0, 0,
                       0,LT117[i], PS117, 0,
                       0,   0,                         PT117, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs117<-paste("count eggs with [playa = 117 ]")
      
      Juv117<-NLReport(CountEggs117)
      
      Nproj117[1, i-1] = Juv117+Nproj117[1,i-1]
      
      Nproj117[1:4,i] = (TM117%*%Nproj117[1:4 ,i-1])
      
      Adults117=floor(Nproj117[4, i])
      
      NoAdults117=paste("ask patches[if pxcor = 189 and pycor = 192 [ sprout-mosqs", Adults117," ]]")
      
      NLCommand(NoAdults117)
      
      Nproj117[4,i]=0
      
      Nproj117=ifelse(Nproj117>0.1, Nproj117, 0)
      
      
      TM118 = matrix(c(ES,0,                   0, 0,
                       ET,LS118[i], 0, 0,
                       0,LT118[i], PS118, 0,
                       0,   0,                         PT118, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs118<-paste("count eggs with [playa = 118 ]")
      
      Juv118<-NLReport(CountEggs118)
      
      Nproj118[1, i-1] = Juv118+Nproj118[1,i-1]
      
      Nproj118[1:4,i] = (TM118%*%Nproj118[1:4 ,i-1])
      
      Adults118=floor(Nproj118[4, i])
      
      NoAdults118=paste("ask patches[if pxcor = 153 and pycor = 177 [ sprout-mosqs", Adults118," ]]")
      
      NLCommand(NoAdults118)
      
      Nproj118[4,i]=0
      
      Nproj118=ifelse(Nproj118>0.1, Nproj118, 0)
      
      
      TM119 = matrix(c(ES,0,                   0, 0,
                       ET,LS119[i], 0, 0,
                       0,LT119[i], PS119, 0,
                       0,   0,                         PT119, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs119<-paste("count eggs with [playa = 119 ]")
      
      Juv119<-NLReport(CountEggs119)
      
      Nproj119[1, i-1] = Juv119+Nproj119[1,i-1]
      
      Nproj119[1:4,i] = (TM119%*%Nproj119[1:4 ,i-1])
      
      Adults119=floor(Nproj119[4, i])
      
      NoAdults119=paste("ask patches[if pxcor = 213 and pycor = 244 [ sprout-mosqs", Adults119," ]]")
      
      NLCommand(NoAdults119)
      
      Nproj119[4,i]=0
      
      Nproj119=ifelse(Nproj119>0.1, Nproj119, 0)
      
      
      TM120 = matrix(c(ES,0,                   0, 0,
                       ET,LS120[i], 0, 0,
                       0,LT120[i], PS120, 0,
                       0,   0,                         PT120, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs120<-paste("count eggs with [playa = 120 ]")
      
      Juv120<-NLReport(CountEggs120)
      
      Nproj120[1, i-1] = Juv120+Nproj120[1,i-1]
      
      Nproj120[1:4,i] = (TM120%*%Nproj120[1:4 ,i-1])
      
      Adults120=floor(Nproj120[4, i])
      
      NoAdults120=paste("ask patches[if pxcor = 212 and pycor = 209 [ sprout-mosqs", Adults120," ]]")
      
      NLCommand(NoAdults120)
      
      Nproj120[4,i]=0
      
      Nproj120=ifelse(Nproj120>0.1, Nproj120, 0)
      
      
      TM121 = matrix(c(ES,0,                   0, 0,
                       ET,LS121[i], 0, 0,
                       0,LT121[i], PS121, 0,
                       0,   0,                         PT121, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs121<-paste("count eggs with [playa = 121 ]")
      
      Juv121<-NLReport(CountEggs121)
      
      Nproj121[1, i-1] = Juv121+Nproj121[1,i-1]
      
      Nproj121[1:4,i] = (TM121%*%Nproj121[1:4 ,i-1])
      
      Adults121=floor(Nproj121[4, i])
      
      NoAdults121=paste("ask patches[if pxcor = 173 and pycor = 215 [ sprout-mosqs", Adults121," ]]")
      
      NLCommand(NoAdults121)
      
      Nproj121[4,i]=0
      
      Nproj121=ifelse(Nproj121>0.1, Nproj121, 0)
      
      
      TM122 = matrix(c(ES,0,                   0, 0,
                       ET,LS122[i], 0, 0,
                       0,LT122[i], PS122, 0,
                       0,   0,                         PT122, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs122<-paste("count eggs with [playa = 122 ]")
      
      Juv122<-NLReport(CountEggs122)
      
      Nproj122[1, i-1] = Juv122+Nproj122[1,i-1]
      
      Nproj122[1:4,i] = (TM122%*%Nproj122[1:4 ,i-1])
      
      Adults122=floor(Nproj122[4, i])
      
      NoAdults122=paste("ask patches[if pxcor = 211 and pycor = 215 [ sprout-mosqs", Adults122," ]]")
      
      NLCommand(NoAdults122)
      
      Nproj122[4,i]=0
      
      Nproj122=ifelse(Nproj122>0.1, Nproj122, 0)
      
      
      TM123 = matrix(c(ES,0,                   0, 0,
                       ET,LS123[i], 0, 0,
                       0,LT123[i], PS123, 0,
                       0,   0,                         PT123, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs123<-paste("count eggs with [playa = 123 ]")
      
      Juv123<-NLReport(CountEggs123)
      
      Nproj123[1, i-1] = Juv123+Nproj123[1,i-1]
      
      Nproj123[1:4,i] = (TM123%*%Nproj123[1:4 ,i-1])
      
      Adults123=floor(Nproj123[4, i])
      
      NoAdults123=paste("ask patches[if pxcor = 30 and pycor = 117 [ sprout-mosqs", Adults123," ]]")
      
      NLCommand(NoAdults123)
      
      Nproj123[4,i]=0
      
      Nproj123=ifelse(Nproj123>0.1, Nproj123, 0)
      
      
      TM124 = matrix(c(ES,0,                   0, 0,
                       ET,LS124[i], 0, 0,
                       0,LT124[i], PS124, 0,
                       0,   0,                         PT124, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs124<-paste("count eggs with [playa = 124 ]")
      
      Juv124<-NLReport(CountEggs124)
      
      Nproj124[1, i-1] = Juv124+Nproj124[1,i-1]
      
      Nproj124[1:4,i] = (TM124%*%Nproj124[1:4 ,i-1])
      
      Adults124=floor(Nproj124[4, i])
      
      NoAdults124=paste("ask patches[if pxcor = 83 and pycor = 134 [ sprout-mosqs", Adults124," ]]")
      
      NLCommand(NoAdults124)
      
      Nproj124[4,i]=0
      
      Nproj124=ifelse(Nproj124>0.1, Nproj124, 0)
      
      
      TM125 = matrix(c(ES,0,                   0, 0,
                       ET,LS125[i], 0, 0,
                       0,LT125[i], PS125, 0,
                       0,   0,                         PT125, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs125<-paste("count eggs with [playa = 125 ]")
      
      Juv125<-NLReport(CountEggs125)
      
      Nproj125[1, i-1] = Juv125+Nproj125[1,i-1]
      
      Nproj125[1:4,i] = (TM125%*%Nproj125[1:4 ,i-1])
      
      Adults125=floor(Nproj125[4, i])
      
      NoAdults125=paste("ask patches[if pxcor = 159 and pycor = 193 [ sprout-mosqs", Adults125," ]]")
      
      NLCommand(NoAdults125)
      
      Nproj125[4,i]=0
      
      Nproj125=ifelse(Nproj125>0.1, Nproj125, 0)
      
      
      TM126 = matrix(c(ES,0,                   0, 0,
                       ET,LS126[i], 0, 0,
                       0,LT126[i], PS126, 0,
                       0,   0,                         PT126, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs126<-paste("count eggs with [playa = 126 ]")
      
      Juv126<-NLReport(CountEggs126)
      
      Nproj126[1, i-1] = Juv126+Nproj126[1,i-1]
      
      Nproj126[1:4,i] = (TM126%*%Nproj126[1:4 ,i-1])
      
      Adults126=floor(Nproj126[4, i])
      
      NoAdults126=paste("ask patches[if pxcor = 117 and pycor = 182 [ sprout-mosqs", Adults126," ]]")
      
      NLCommand(NoAdults126)
      
      Nproj126[4,i]=0
      
      Nproj126=ifelse(Nproj126>0.1, Nproj126, 0)
      
      
      TM127 = matrix(c(ES,0,                   0, 0,
                       ET,LS127[i], 0, 0,
                       0,LT127[i], PS127, 0,
                       0,   0,                         PT127, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs127<-paste("count eggs with [playa = 127 ]")
      
      Juv127<-NLReport(CountEggs127)
      
      Nproj127[1, i-1] = Juv127+Nproj127[1,i-1]
      
      Nproj127[1:4,i] = (TM127%*%Nproj127[1:4 ,i-1])
      
      Adults127=floor(Nproj127[4, i])
      
      NoAdults127=paste("ask patches[if pxcor = 110 and pycor = 185 [ sprout-mosqs", Adults127," ]]")
      
      NLCommand(NoAdults127)
      
      Nproj127[4,i]=0
      
      Nproj127=ifelse(Nproj127>0.1, Nproj127, 0)
      
      
      TM128 = matrix(c(ES,0,                   0, 0,
                       ET,LS128[i], 0, 0,
                       0,LT128[i], PS128, 0,
                       0,   0,                         PT128, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs128<-paste("count eggs with [playa = 128 ]")
      
      Juv128<-NLReport(CountEggs128)
      
      Nproj128[1, i-1] = Juv128+Nproj128[1,i-1]
      
      Nproj128[1:4,i] = (TM128%*%Nproj128[1:4 ,i-1])
      
      Adults128=floor(Nproj128[4, i])
      
      NoAdults128=paste("ask patches[if pxcor = 75 and pycor = 237 [ sprout-mosqs", Adults128," ]]")
      
      NLCommand(NoAdults128)
      
      Nproj128[4,i]=0
      
      Nproj128=ifelse(Nproj128>0.1, Nproj128, 0)
      
      
      TM129 = matrix(c(ES,0,                   0, 0,
                       ET,LS129[i], 0, 0,
                       0,LT129[i], PS129, 0,
                       0,   0,                         PT129, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs129<-paste("count eggs with [playa = 129 ]")
      
      Juv129<-NLReport(CountEggs129)
      
      Nproj129[1, i-1] = Juv129+Nproj129[1,i-1]
      
      Nproj129[1:4,i] = (TM129%*%Nproj129[1:4 ,i-1])
      
      Adults129=floor(Nproj129[4, i])
      
      NoAdults129=paste("ask patches[if pxcor = 41 and pycor = 362 [ sprout-mosqs", Adults129," ]]")
      
      NLCommand(NoAdults129)
      
      Nproj129[4,i]=0
      
      Nproj129=ifelse(Nproj129>0.1, Nproj129, 0)
      
      
      TM130 = matrix(c(ES,0,                   0, 0,
                       ET,LS130[i], 0, 0,
                       0,LT130[i], PS130, 0,
                       0,   0,                         PT130, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs130<-paste("count eggs with [playa = 130 ]")
      
      Juv130<-NLReport(CountEggs130)
      
      Nproj130[1, i-1] = Juv130+Nproj130[1,i-1]
      
      Nproj130[1:4,i] = (TM130%*%Nproj130[1:4 ,i-1])
      
      Adults130=floor(Nproj130[4, i])
      
      NoAdults130=paste("ask patches[if pxcor = 198 and pycor = 340 [ sprout-mosqs", Adults130," ]]")
      
      NLCommand(NoAdults130)
      
      Nproj130[4,i]=0
      
      Nproj130=ifelse(Nproj130>0.1, Nproj130, 0)
      
      
      TM131 = matrix(c(ES,0,                   0, 0,
                       ET,LS131[i], 0, 0,
                       0,LT131[i], PS131, 0,
                       0,   0,                         PT131, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs131<-paste("count eggs with [playa = 131 ]")
      
      Juv131<-NLReport(CountEggs131)
      
      Nproj131[1, i-1] = Juv131+Nproj131[1,i-1]
      
      Nproj131[1:4,i] = (TM131%*%Nproj131[1:4 ,i-1])
      
      Adults131=floor(Nproj131[4, i])
      
      NoAdults131=paste("ask patches[if pxcor = 189 and pycor = 129 [ sprout-mosqs", Adults131," ]]")
      
      NLCommand(NoAdults131)
      
      Nproj131[4,i]=0
      
      Nproj131=ifelse(Nproj131>0.1, Nproj131, 0)
      
      
      TM132 = matrix(c(ES,0,                   0, 0,
                       ET,LS132[i], 0, 0,
                       0,LT132[i], PS132, 0,
                       0,   0,                         PT132, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs132<-paste("count eggs with [playa = 132 ]")
      
      Juv132<-NLReport(CountEggs132)
      
      Nproj132[1, i-1] = Juv132+Nproj132[1,i-1]
      
      Nproj132[1:4,i] = (TM132%*%Nproj132[1:4 ,i-1])
      
      Adults132=floor(Nproj132[4, i])
      
      NoAdults132=paste("ask patches[if pxcor = 162 and pycor = 120 [ sprout-mosqs", Adults132," ]]")
      
      NLCommand(NoAdults132)
      
      Nproj132[4,i]=0
      
      Nproj132=ifelse(Nproj132>0.1, Nproj132, 0)
      
      
      TM133 = matrix(c(ES,0,                   0, 0,
                       ET,LS133[i], 0, 0,
                       0,LT133[i], PS133, 0,
                       0,   0,                         PT133, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs133<-paste("count eggs with [playa = 133 ]")
      
      Juv133<-NLReport(CountEggs133)
      
      Nproj133[1, i-1] = Juv133+Nproj133[1,i-1]
      
      Nproj133[1:4,i] = (TM133%*%Nproj133[1:4 ,i-1])
      
      Adults133=floor(Nproj133[4, i])
      
      NoAdults133=paste("ask patches[if pxcor = 411 and pycor = 250 [ sprout-mosqs", Adults133," ]]")
      
      NLCommand(NoAdults133)
      
      Nproj133[4,i]=0
      
      Nproj133=ifelse(Nproj133>0.1, Nproj133, 0)
      
      
      TM134 = matrix(c(ES,0,                   0, 0,
                       ET,LS134[i], 0, 0,
                       0,LT134[i], PS134, 0,
                       0,   0,                         PT134, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs134<-paste("count eggs with [playa = 134 ]")
      
      Juv134<-NLReport(CountEggs134)
      
      Nproj134[1, i-1] = Juv134+Nproj134[1,i-1]
      
      Nproj134[1:4,i] = (TM134%*%Nproj134[1:4 ,i-1])
      
      Adults134=floor(Nproj134[4, i])
      
      NoAdults134=paste("ask patches[if pxcor = 361 and pycor = 233 [ sprout-mosqs", Adults134," ]]")
      
      NLCommand(NoAdults134)
      
      Nproj134[4,i]=0
      
      Nproj134=ifelse(Nproj134>0.1, Nproj134, 0)
      
      
      TM135 = matrix(c(ES,0,                   0, 0,
                       ET,LS135[i], 0, 0,
                       0,LT135[i], PS135, 0,
                       0,   0,                         PT135, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs135<-paste("count eggs with [playa = 135 ]")
      
      Juv135<-NLReport(CountEggs135)
      
      Nproj135[1, i-1] = Juv135+Nproj135[1,i-1]
      
      Nproj135[1:4,i] = (TM135%*%Nproj135[1:4 ,i-1])
      
      Adults135=floor(Nproj135[4, i])
      
      NoAdults135=paste("ask patches[if pxcor = 420 and pycor = 244 [ sprout-mosqs", Adults135," ]]")
      
      NLCommand(NoAdults135)
      
      Nproj135[4,i]=0
      
      Nproj135=ifelse(Nproj135>0.1, Nproj135, 0)
      
      
      TM136 = matrix(c(ES,0,                   0, 0,
                       ET,LS136[i], 0, 0,
                       0,LT136[i], PS136, 0,
                       0,   0,                         PT136, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs136<-paste("count eggs with [playa = 136 ]")
      
      Juv136<-NLReport(CountEggs136)
      
      Nproj136[1, i-1] = Juv136+Nproj136[1,i-1]
      
      Nproj136[1:4,i] = (TM136%*%Nproj136[1:4 ,i-1])
      
      Adults136=floor(Nproj136[4, i])
      
      NoAdults136=paste("ask patches[if pxcor = 238 and pycor = 178 [ sprout-mosqs", Adults136," ]]")
      
      NLCommand(NoAdults136)
      
      Nproj136[4,i]=0
      
      Nproj136=ifelse(Nproj136>0.1, Nproj136, 0)
      
      
      TM137 = matrix(c(ES,0,                   0, 0,
                       ET,LS137[i], 0, 0,
                       0,LT137[i], PS137, 0,
                       0,   0,                         PT137, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs137<-paste("count eggs with [playa = 137 ]")
      
      Juv137<-NLReport(CountEggs137)
      
      Nproj137[1, i-1] = Juv137+Nproj137[1,i-1]
      
      Nproj137[1:4,i] = (TM137%*%Nproj137[1:4 ,i-1])
      
      Adults137=floor(Nproj137[4, i])
      
      NoAdults137=paste("ask patches[if pxcor = 268 and pycor = 156 [ sprout-mosqs", Adults137," ]]")
      
      NLCommand(NoAdults137)
      
      Nproj137[4,i]=0
      
      Nproj137=ifelse(Nproj137>0.1, Nproj137, 0)
      
      
      TM138 = matrix(c(ES,0,                   0, 0,
                       ET,LS138[i], 0, 0,
                       0,LT138[i], PS138, 0,
                       0,   0,                         PT138, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs138<-paste("count eggs with [playa = 138 ]")
      
      Juv138<-NLReport(CountEggs138)
      
      Nproj138[1, i-1] = Juv138+Nproj138[1,i-1]
      
      Nproj138[1:4,i] = (TM138%*%Nproj138[1:4 ,i-1])
      
      Adults138=floor(Nproj138[4, i])
      
      NoAdults138=paste("ask patches[if pxcor = 211 and pycor = 359 [ sprout-mosqs", Adults138," ]]")
      
      NLCommand(NoAdults138)
      
      Nproj138[4,i]=0
      
      Nproj138=ifelse(Nproj138>0.1, Nproj138, 0)
      
      
      TM139 = matrix(c(ES,0,                   0, 0,
                       ET,LS139[i], 0, 0,
                       0,LT139[i], PS139, 0,
                       0,   0,                         PT139, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs139<-paste("count eggs with [playa = 139 ]")
      
      Juv139<-NLReport(CountEggs139)
      
      Nproj139[1, i-1] = Juv139+Nproj139[1,i-1]
      
      Nproj139[1:4,i] = (TM139%*%Nproj139[1:4 ,i-1])
      
      Adults139=floor(Nproj139[4, i])
      
      NoAdults139=paste("ask patches[if pxcor = 164 and pycor = 316 [ sprout-mosqs", Adults139," ]]")
      
      NLCommand(NoAdults139)
      
      Nproj139[4,i]=0
      
      Nproj139=ifelse(Nproj139>0.1, Nproj139, 0)
      
      
      TM140 = matrix(c(ES,0,                   0, 0,
                       ET,LS140[i], 0, 0,
                       0,LT140[i], PS140, 0,
                       0,   0,                         PT140, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs140<-paste("count eggs with [playa = 140 ]")
      
      Juv140<-NLReport(CountEggs140)
      
      Nproj140[1, i-1] = Juv140+Nproj140[1,i-1]
      
      Nproj140[1:4,i] = (TM140%*%Nproj140[1:4 ,i-1])
      
      Adults140=floor(Nproj140[4, i])
      
      NoAdults140=paste("ask patches[if pxcor = 242 and pycor = 282 [ sprout-mosqs", Adults140," ]]")
      
      NLCommand(NoAdults140)
      
      Nproj140[4,i]=0
      
      Nproj140=ifelse(Nproj140>0.1, Nproj140, 0)
      
      
      TM141 = matrix(c(ES,0,                   0, 0,
                       ET,LS141[i], 0, 0,
                       0,LT141[i], PS141, 0,
                       0,   0,                         PT141, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs141<-paste("count eggs with [playa = 141 ]")
      
      Juv141<-NLReport(CountEggs141)
      
      Nproj141[1, i-1] = Juv141+Nproj141[1,i-1]
      
      Nproj141[1:4,i] = (TM141%*%Nproj141[1:4 ,i-1])
      
      Adults141=floor(Nproj141[4, i])
      
      NoAdults141=paste("ask patches[if pxcor = 57 and pycor = 173 [ sprout-mosqs", Adults141," ]]")
      
      NLCommand(NoAdults141)
      
      Nproj141[4,i]=0
      
      Nproj141=ifelse(Nproj141>0.1, Nproj141, 0)
      
      
      TM142 = matrix(c(ES,0,                   0, 0,
                       ET,LS142[i], 0, 0,
                       0,LT142[i], PS142, 0,
                       0,   0,                         PT142, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs142<-paste("count eggs with [playa = 142 ]")
      
      Juv142<-NLReport(CountEggs142)
      
      Nproj142[1, i-1] = Juv142+Nproj142[1,i-1]
      
      Nproj142[1:4,i] = (TM142%*%Nproj142[1:4 ,i-1])
      
      Adults142=floor(Nproj142[4, i])
      
      NoAdults142=paste("ask patches[if pxcor = 111 and pycor = 161 [ sprout-mosqs", Adults142," ]]")
      
      NLCommand(NoAdults142)
      
      Nproj142[4,i]=0
      
      Nproj142=ifelse(Nproj142>0.1, Nproj142, 0)
      
      
      TM143 = matrix(c(ES,0,                   0, 0,
                       ET,LS143[i], 0, 0,
                       0,LT143[i], PS143, 0,
                       0,   0,                         PT143, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs143<-paste("count eggs with [playa = 143 ]")
      
      Juv143<-NLReport(CountEggs143)
      
      Nproj143[1, i-1] = Juv143+Nproj143[1,i-1]
      
      Nproj143[1:4,i] = (TM143%*%Nproj143[1:4 ,i-1])
      
      Adults143=floor(Nproj143[4, i])
      
      NoAdults143=paste("ask patches[if pxcor = 210 and pycor = 59 [ sprout-mosqs", Adults143," ]]")
      
      NLCommand(NoAdults143)
      
      Nproj143[4,i]=0
      
      Nproj143=ifelse(Nproj143>0.1, Nproj143, 0)
      
      
      TM144 = matrix(c(ES,0,                   0, 0,
                       ET,LS144[i], 0, 0,
                       0,LT144[i], PS144, 0,
                       0,   0,                         PT144, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs144<-paste("count eggs with [playa = 144 ]")
      
      Juv144<-NLReport(CountEggs144)
      
      Nproj144[1, i-1] = Juv144+Nproj144[1,i-1]
      
      Nproj144[1:4,i] = (TM144%*%Nproj144[1:4 ,i-1])
      
      Adults144=floor(Nproj144[4, i])
      
      NoAdults144=paste("ask patches[if pxcor = 222 and pycor = 91 [ sprout-mosqs", Adults144," ]]")
      
      NLCommand(NoAdults144)
      
      Nproj144[4,i]=0
      
      Nproj144=ifelse(Nproj144>0.1, Nproj144, 0)
      
      
      TM145 = matrix(c(ES,0,                   0, 0,
                       ET,LS145[i], 0, 0,
                       0,LT145[i], PS145, 0,
                       0,   0,                         PT145, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs145<-paste("count eggs with [playa = 145 ]")
      
      Juv145<-NLReport(CountEggs145)
      
      Nproj145[1, i-1] = Juv145+Nproj145[1,i-1]
      
      Nproj145[1:4,i] = (TM145%*%Nproj145[1:4 ,i-1])
      
      Adults145=floor(Nproj145[4, i])
      
      NoAdults145=paste("ask patches[if pxcor = 235 and pycor = 91 [ sprout-mosqs", Adults145," ]]")
      
      NLCommand(NoAdults145)
      
      Nproj145[4,i]=0
      
      Nproj145=ifelse(Nproj145>0.1, Nproj145, 0)
      
      
      TM146 = matrix(c(ES,0,                   0, 0,
                       ET,LS146[i], 0, 0,
                       0,LT146[i], PS146, 0,
                       0,   0,                         PT146, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs146<-paste("count eggs with [playa = 146 ]")
      
      Juv146<-NLReport(CountEggs146)
      
      Nproj146[1, i-1] = Juv146+Nproj146[1,i-1]
      
      Nproj146[1:4,i] = (TM146%*%Nproj146[1:4 ,i-1])
      
      Adults146=floor(Nproj146[4, i])
      
      NoAdults146=paste("ask patches[if pxcor = 212 and pycor = 370 [ sprout-mosqs", Adults146," ]]")
      
      NLCommand(NoAdults146)
      
      Nproj146[4,i]=0
      
      Nproj146=ifelse(Nproj146>0.1, Nproj146, 0)
      
      
      TM147 = matrix(c(ES,0,                   0, 0,
                       ET,LS147[i], 0, 0,
                       0,LT147[i], PS147, 0,
                       0,   0,                         PT147, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs147<-paste("count eggs with [playa = 147 ]")
      
      Juv147<-NLReport(CountEggs147)
      
      Nproj147[1, i-1] = Juv147+Nproj147[1,i-1]
      
      Nproj147[1:4,i] = (TM147%*%Nproj147[1:4 ,i-1])
      
      Adults147=floor(Nproj147[4, i])
      
      NoAdults147=paste("ask patches[if pxcor = 121 and pycor = 292 [ sprout-mosqs", Adults147," ]]")
      
      NLCommand(NoAdults147)
      
      Nproj147[4,i]=0
      
      Nproj147=ifelse(Nproj147>0.1, Nproj147, 0)
      
      
      TM148 = matrix(c(ES,0,                   0, 0,
                       ET,LS148[i], 0, 0,
                       0,LT148[i], PS148, 0,
                       0,   0,                         PT148, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs148<-paste("count eggs with [playa = 148 ]")
      
      Juv148<-NLReport(CountEggs148)
      
      Nproj148[1, i-1] = Juv148+Nproj148[1,i-1]
      
      Nproj148[1:4,i] = (TM148%*%Nproj148[1:4 ,i-1])
      
      Adults148=floor(Nproj148[4, i])
      
      NoAdults148=paste("ask patches[if pxcor = 343 and pycor = 302 [ sprout-mosqs", Adults148," ]]")
      
      NLCommand(NoAdults148)
      
      Nproj148[4,i]=0
      
      Nproj148=ifelse(Nproj148>0.1, Nproj148, 0)
      
      
      TM149 = matrix(c(ES,0,                   0, 0,
                       ET,LS149[i], 0, 0,
                       0,LT149[i], PS149, 0,
                       0,   0,                         PT149, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs149<-paste("count eggs with [playa = 149 ]")
      
      Juv149<-NLReport(CountEggs149)
      
      Nproj149[1, i-1] = Juv149+Nproj149[1,i-1]
      
      Nproj149[1:4,i] = (TM149%*%Nproj149[1:4 ,i-1])
      
      Adults149=floor(Nproj149[4, i])
      
      NoAdults149=paste("ask patches[if pxcor = 360 and pycor = 339 [ sprout-mosqs", Adults149," ]]")
      
      NLCommand(NoAdults149)
      
      Nproj149[4,i]=0
      
      Nproj149=ifelse(Nproj149>0.1, Nproj149, 0)
      
      
      TM150 = matrix(c(ES,0,                   0, 0,
                       ET,LS150[i], 0, 0,
                       0,LT150[i], PS150, 0,
                       0,   0,                         PT150, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs150<-paste("count eggs with [playa = 150 ]")
      
      Juv150<-NLReport(CountEggs150)
      
      Nproj150[1, i-1] = Juv150+Nproj150[1,i-1]
      
      Nproj150[1:4,i] = (TM150%*%Nproj150[1:4 ,i-1])
      
      Adults150=floor(Nproj150[4, i])
      
      NoAdults150=paste("ask patches[if pxcor = 246 and pycor = 249 [ sprout-mosqs", Adults150," ]]")
      
      NLCommand(NoAdults150)
      
      Nproj150[4,i]=0
      
      Nproj150=ifelse(Nproj150>0.1, Nproj150, 0)
      
      
      TM151 = matrix(c(ES,0,                   0, 0,
                       ET,LS151[i], 0, 0,
                       0,LT151[i], PS151, 0,
                       0,   0,                         PT151, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs151<-paste("count eggs with [playa = 151 ]")
      
      Juv151<-NLReport(CountEggs151)
      
      Nproj151[1, i-1] = Juv151+Nproj151[1,i-1]
      
      Nproj151[1:4,i] = (TM151%*%Nproj151[1:4 ,i-1])
      
      Adults151=floor(Nproj151[4, i])
      
      NoAdults151=paste("ask patches[if pxcor = 241 and pycor = 244 [ sprout-mosqs", Adults151," ]]")
      
      NLCommand(NoAdults151)
      
      Nproj151[4,i]=0
      
      Nproj151=ifelse(Nproj151>0.1, Nproj151, 0)
      
      
      TM152 = matrix(c(ES,0,                   0, 0,
                       ET,LS152[i], 0, 0,
                       0,LT152[i], PS152, 0,
                       0,   0,                         PT152, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs152<-paste("count eggs with [playa = 152 ]")
      
      Juv152<-NLReport(CountEggs152)
      
      Nproj152[1, i-1] = Juv152+Nproj152[1,i-1]
      
      Nproj152[1:4,i] = (TM152%*%Nproj152[1:4 ,i-1])
      
      Adults152=floor(Nproj152[4, i])
      
      NoAdults152=paste("ask patches[if pxcor = 249 and pycor = 279 [ sprout-mosqs", Adults152," ]]")
      
      NLCommand(NoAdults152)
      
      Nproj152[4,i]=0
      
      Nproj152=ifelse(Nproj152>0.1, Nproj152, 0)
      
      
      TM153 = matrix(c(ES,0,                   0, 0,
                       ET,LS153[i], 0, 0,
                       0,LT153[i], PS153, 0,
                       0,   0,                         PT153, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs153<-paste("count eggs with [playa = 153 ]")
      
      Juv153<-NLReport(CountEggs153)
      
      Nproj153[1, i-1] = Juv153+Nproj153[1,i-1]
      
      Nproj153[1:4,i] = (TM153%*%Nproj153[1:4 ,i-1])
      
      Adults153=floor(Nproj153[4, i])
      
      NoAdults153=paste("ask patches[if pxcor = 211 and pycor = 84 [ sprout-mosqs", Adults153," ]]")
      
      NLCommand(NoAdults153)
      
      Nproj153[4,i]=0
      
      Nproj153=ifelse(Nproj153>0.1, Nproj153, 0)
      
      
      TM154 = matrix(c(ES,0,                   0, 0,
                       ET,LS154[i], 0, 0,
                       0,LT154[i], PS154, 0,
                       0,   0,                         PT154, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs154<-paste("count eggs with [playa = 154 ]")
      
      Juv154<-NLReport(CountEggs154)
      
      Nproj154[1, i-1] = Juv154+Nproj154[1,i-1]
      
      Nproj154[1:4,i] = (TM154%*%Nproj154[1:4 ,i-1])
      
      Adults154=floor(Nproj154[4, i])
      
      NoAdults154=paste("ask patches[if pxcor = 312 and pycor = 30 [ sprout-mosqs", Adults154," ]]")
      
      NLCommand(NoAdults154)
      
      Nproj154[4,i]=0
      
      Nproj154=ifelse(Nproj154>0.1, Nproj154, 0)
      
      
      TM155 = matrix(c(ES,0,                   0, 0,
                       ET,LS155[i], 0, 0,
                       0,LT155[i], PS155, 0,
                       0,   0,                         PT155, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs155<-paste("count eggs with [playa = 155 ]")
      
      Juv155<-NLReport(CountEggs155)
      
      Nproj155[1, i-1] = Juv155+Nproj155[1,i-1]
      
      Nproj155[1:4,i] = (TM155%*%Nproj155[1:4 ,i-1])
      
      Adults155=floor(Nproj155[4, i])
      
      NoAdults155=paste("ask patches[if pxcor = 111 and pycor = 107 [ sprout-mosqs", Adults155," ]]")
      
      NLCommand(NoAdults155)
      
      Nproj155[4,i]=0
      
      Nproj155=ifelse(Nproj155>0.1, Nproj155, 0)
      
      
      TM156 = matrix(c(ES,0,                   0, 0,
                       ET,LS156[i], 0, 0,
                       0,LT156[i], PS156, 0,
                       0,   0,                         PT156, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs156<-paste("count eggs with [playa = 156 ]")
      
      Juv156<-NLReport(CountEggs156)
      
      Nproj156[1, i-1] = Juv156+Nproj156[1,i-1]
      
      Nproj156[1:4,i] = (TM156%*%Nproj156[1:4 ,i-1])
      
      Adults156=floor(Nproj156[4, i])
      
      NoAdults156=paste("ask patches[if pxcor = 387 and pycor = 339 [ sprout-mosqs", Adults156," ]]")
      
      NLCommand(NoAdults156)
      
      Nproj156[4,i]=0
      
      Nproj156=ifelse(Nproj156>0.1, Nproj156, 0)
      
      
      TM157 = matrix(c(ES,0,                   0, 0,
                       ET,LS157[i], 0, 0,
                       0,LT157[i], PS157, 0,
                       0,   0,                         PT157, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs157<-paste("count eggs with [playa = 157 ]")
      
      Juv157<-NLReport(CountEggs157)
      
      Nproj157[1, i-1] = Juv157+Nproj157[1,i-1]
      
      Nproj157[1:4,i] = (TM157%*%Nproj157[1:4 ,i-1])
      
      Adults157=floor(Nproj157[4, i])
      
      NoAdults157=paste("ask patches[if pxcor = 403 and pycor = 358 [ sprout-mosqs", Adults157," ]]")
      
      NLCommand(NoAdults157)
      
      Nproj157[4,i]=0
      
      Nproj157=ifelse(Nproj157>0.1, Nproj157, 0)
      
      
      TM158 = matrix(c(ES,0,                   0, 0,
                       ET,LS158[i], 0, 0,
                       0,LT158[i], PS158, 0,
                       0,   0,                         PT158, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs158<-paste("count eggs with [playa = 158 ]")
      
      Juv158<-NLReport(CountEggs158)
      
      Nproj158[1, i-1] = Juv158+Nproj158[1,i-1]
      
      Nproj158[1:4,i] = (TM158%*%Nproj158[1:4 ,i-1])
      
      Adults158=floor(Nproj158[4, i])
      
      NoAdults158=paste("ask patches[if pxcor = 320 and pycor = 370 [ sprout-mosqs", Adults158," ]]")
      
      NLCommand(NoAdults158)
      
      Nproj158[4,i]=0
      
      Nproj158=ifelse(Nproj158>0.1, Nproj158, 0)
      
      
      TM159 = matrix(c(ES,0,                   0, 0,
                       ET,LS159[i], 0, 0,
                       0,LT159[i], PS159, 0,
                       0,   0,                         PT159, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs159<-paste("count eggs with [playa = 159 ]")
      
      Juv159<-NLReport(CountEggs159)
      
      Nproj159[1, i-1] = Juv159+Nproj159[1,i-1]
      
      Nproj159[1:4,i] = (TM159%*%Nproj159[1:4 ,i-1])
      
      Adults159=floor(Nproj159[4, i])
      
      NoAdults159=paste("ask patches[if pxcor = 441 and pycor = 259 [ sprout-mosqs", Adults159," ]]")
      
      NLCommand(NoAdults159)
      
      Nproj159[4,i]=0
      
      Nproj159=ifelse(Nproj159>0.1, Nproj159, 0)
      
      
      TM160 = matrix(c(ES,0,                   0, 0,
                       ET,LS160[i], 0, 0,
                       0,LT160[i], PS160, 0,
                       0,   0,                         PT160, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs160<-paste("count eggs with [playa = 160 ]")
      
      Juv160<-NLReport(CountEggs160)
      
      Nproj160[1, i-1] = Juv160+Nproj160[1,i-1]
      
      Nproj160[1:4,i] = (TM160%*%Nproj160[1:4 ,i-1])
      
      Adults160=floor(Nproj160[4, i])
      
      NoAdults160=paste("ask patches[if pxcor = 341 and pycor = 364 [ sprout-mosqs", Adults160," ]]")
      
      NLCommand(NoAdults160)
      
      Nproj160[4,i]=0
      
      Nproj160=ifelse(Nproj160>0.1, Nproj160, 0)
      
      
      TM161 = matrix(c(ES,0,                   0, 0,
                       ET,LS161[i], 0, 0,
                       0,LT161[i], PS161, 0,
                       0,   0,                         PT161, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs161<-paste("count eggs with [playa = 161 ]")
      
      Juv161<-NLReport(CountEggs161)
      
      Nproj161[1, i-1] = Juv161+Nproj161[1,i-1]
      
      Nproj161[1:4,i] = (TM161%*%Nproj161[1:4 ,i-1])
      
      Adults161=floor(Nproj161[4, i])
      
      NoAdults161=paste("ask patches[if pxcor = 425 and pycor = 290 [ sprout-mosqs", Adults161," ]]")
      
      NLCommand(NoAdults161)
      
      Nproj161[4,i]=0
      
      Nproj161=ifelse(Nproj161>0.1, Nproj161, 0)
      
      
      TM162 = matrix(c(ES,0,                   0, 0,
                       ET,LS162[i], 0, 0,
                       0,LT162[i], PS162, 0,
                       0,   0,                         PT162, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs162<-paste("count eggs with [playa = 162 ]")
      
      Juv162<-NLReport(CountEggs162)
      
      Nproj162[1, i-1] = Juv162+Nproj162[1,i-1]
      
      Nproj162[1:4,i] = (TM162%*%Nproj162[1:4 ,i-1])
      
      Adults162=floor(Nproj162[4, i])
      
      NoAdults162=paste("ask patches[if pxcor = 385 and pycor = 47 [ sprout-mosqs", Adults162," ]]")
      
      NLCommand(NoAdults162)
      
      Nproj162[4,i]=0
      
      Nproj162=ifelse(Nproj162>0.1, Nproj162, 0)
      
      
      TM163 = matrix(c(ES,0,                   0, 0,
                       ET,LS163[i], 0, 0,
                       0,LT163[i], PS163, 0,
                       0,   0,                         PT163, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs163<-paste("count eggs with [playa = 163 ]")
      
      Juv163<-NLReport(CountEggs163)
      
      Nproj163[1, i-1] = Juv163+Nproj163[1,i-1]
      
      Nproj163[1:4,i] = (TM163%*%Nproj163[1:4 ,i-1])
      
      Adults163=floor(Nproj163[4, i])
      
      NoAdults163=paste("ask patches[if pxcor = 442 and pycor = 229 [ sprout-mosqs", Adults163," ]]")
      
      NLCommand(NoAdults163)
      
      Nproj163[4,i]=0
      
      Nproj163=ifelse(Nproj163>0.1, Nproj163, 0)
      
      
      TM164 = matrix(c(ES,0,                   0, 0,
                       ET,LS164[i], 0, 0,
                       0,LT164[i], PS164, 0,
                       0,   0,                         PT164, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs164<-paste("count eggs with [playa = 164 ]")
      
      Juv164<-NLReport(CountEggs164)
      
      Nproj164[1, i-1] = Juv164+Nproj164[1,i-1]
      
      Nproj164[1:4,i] = (TM164%*%Nproj164[1:4 ,i-1])
      
      Adults164=floor(Nproj164[4, i])
      
      NoAdults164=paste("ask patches[if pxcor = 443 and pycor = 240 [ sprout-mosqs", Adults164," ]]")
      
      NLCommand(NoAdults164)
      
      Nproj164[4,i]=0
      
      Nproj164=ifelse(Nproj164>0.1, Nproj164, 0)
      
      
      TM165 = matrix(c(ES,0,                   0, 0,
                       ET,LS165[i], 0, 0,
                       0,LT165[i], PS165, 0,
                       0,   0,                         PT165, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs165<-paste("count eggs with [playa = 165 ]")
      
      Juv165<-NLReport(CountEggs165)
      
      Nproj165[1, i-1] = Juv165+Nproj165[1,i-1]
      
      Nproj165[1:4,i] = (TM165%*%Nproj165[1:4 ,i-1])
      
      Adults165=floor(Nproj165[4, i])
      
      NoAdults165=paste("ask patches[if pxcor = 441 and pycor = 288 [ sprout-mosqs", Adults165," ]]")
      
      NLCommand(NoAdults165)
      
      Nproj165[4,i]=0
      
      Nproj165=ifelse(Nproj165>0.1, Nproj165, 0)
      
      
      TM166 = matrix(c(ES,0,                   0, 0,
                       ET,LS166[i], 0, 0,
                       0,LT166[i], PS166, 0,
                       0,   0,                         PT166, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs166<-paste("count eggs with [playa = 166 ]")
      
      Juv166<-NLReport(CountEggs166)
      
      Nproj166[1, i-1] = Juv166+Nproj166[1,i-1]
      
      Nproj166[1:4,i] = (TM166%*%Nproj166[1:4 ,i-1])
      
      Adults166=floor(Nproj166[4, i])
      
      NoAdults166=paste("ask patches[if pxcor = 347 and pycor = 317 [ sprout-mosqs", Adults166," ]]")
      
      NLCommand(NoAdults166)
      
      Nproj166[4,i]=0
      
      Nproj166=ifelse(Nproj166>0.1, Nproj166, 0)
      
      
      TM167 = matrix(c(ES,0,                   0, 0,
                       ET,LS167[i], 0, 0,
                       0,LT167[i], PS167, 0,
                       0,   0,                         PT167, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs167<-paste("count eggs with [playa = 167 ]")
      
      Juv167<-NLReport(CountEggs167)
      
      Nproj167[1, i-1] = Juv167+Nproj167[1,i-1]
      
      Nproj167[1:4,i] = (TM167%*%Nproj167[1:4 ,i-1])
      
      Adults167=floor(Nproj167[4, i])
      
      NoAdults167=paste("ask patches[if pxcor = 210 and pycor = 372 [ sprout-mosqs", Adults167," ]]")
      
      NLCommand(NoAdults167)
      
      Nproj167[4,i]=0
      
      Nproj167=ifelse(Nproj167>0.1, Nproj167, 0)
      
      
      TM168 = matrix(c(ES,0,                   0, 0,
                       ET,LS168[i], 0, 0,
                       0,LT168[i], PS168, 0,
                       0,   0,                         PT168, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs168<-paste("count eggs with [playa = 168 ]")
      
      Juv168<-NLReport(CountEggs168)
      
      Nproj168[1, i-1] = Juv168+Nproj168[1,i-1]
      
      Nproj168[1:4,i] = (TM168%*%Nproj168[1:4 ,i-1])
      
      Adults168=floor(Nproj168[4, i])
      
      NoAdults168=paste("ask patches[if pxcor = 50 and pycor = 166 [ sprout-mosqs", Adults168," ]]")
      
      NLCommand(NoAdults168)
      
      Nproj168[4,i]=0
      
      Nproj168=ifelse(Nproj168>0.1, Nproj168, 0)
      
      
      TM169 = matrix(c(ES,0,                   0, 0,
                       ET,LS169[i], 0, 0,
                       0,LT169[i], PS169, 0,
                       0,   0,                         PT169, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs169<-paste("count eggs with [playa = 169 ]")
      
      Juv169<-NLReport(CountEggs169)
      
      Nproj169[1, i-1] = Juv169+Nproj169[1,i-1]
      
      Nproj169[1:4,i] = (TM169%*%Nproj169[1:4 ,i-1])
      
      Adults169=floor(Nproj169[4, i])
      
      NoAdults169=paste("ask patches[if pxcor = 402 and pycor = 270 [ sprout-mosqs", Adults169," ]]")
      
      NLCommand(NoAdults169)
      
      Nproj169[4,i]=0
      
      Nproj169=ifelse(Nproj169>0.1, Nproj169, 0)
      
      
      TM170 = matrix(c(ES,0,                   0, 0,
                       ET,LS170[i], 0, 0,
                       0,LT170[i], PS170, 0,
                       0,   0,                         PT170, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs170<-paste("count eggs with [playa = 170 ]")
      
      Juv170<-NLReport(CountEggs170)
      
      Nproj170[1, i-1] = Juv170+Nproj170[1,i-1]
      
      Nproj170[1:4,i] = (TM170%*%Nproj170[1:4 ,i-1])
      
      Adults170=floor(Nproj170[4, i])
      
      NoAdults170=paste("ask patches[if pxcor = 166 and pycor = 224 [ sprout-mosqs", Adults170," ]]")
      
      NLCommand(NoAdults170)
      
      Nproj170[4,i]=0
      
      Nproj170=ifelse(Nproj170>0.1, Nproj170, 0)
      
      
      TM171 = matrix(c(ES,0,                   0, 0,
                       ET,LS171[i], 0, 0,
                       0,LT171[i], PS171, 0,
                       0,   0,                         PT171, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs171<-paste("count eggs with [playa = 171 ]")
      
      Juv171<-NLReport(CountEggs171)
      
      Nproj171[1, i-1] = Juv171+Nproj171[1,i-1]
      
      Nproj171[1:4,i] = (TM171%*%Nproj171[1:4 ,i-1])
      
      Adults171=floor(Nproj171[4, i])
      
      NoAdults171=paste("ask patches[if pxcor = 174 and pycor = 260 [ sprout-mosqs", Adults171," ]]")
      
      NLCommand(NoAdults171)
      
      Nproj171[4,i]=0
      
      Nproj171=ifelse(Nproj171>0.1, Nproj171, 0)
      
      
      TM172 = matrix(c(ES,0,                   0, 0,
                       ET,LS172[i], 0, 0,
                       0,LT172[i], PS172, 0,
                       0,   0,                         PT172, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs172<-paste("count eggs with [playa = 172 ]")
      
      Juv172<-NLReport(CountEggs172)
      
      Nproj172[1, i-1] = Juv172+Nproj172[1,i-1]
      
      Nproj172[1:4,i] = (TM172%*%Nproj172[1:4 ,i-1])
      
      Adults172=floor(Nproj172[4, i])
      
      NoAdults172=paste("ask patches[if pxcor = 167 and pycor = 184 [ sprout-mosqs", Adults172," ]]")
      
      NLCommand(NoAdults172)
      
      Nproj172[4,i]=0
      
      Nproj172=ifelse(Nproj172>0.1, Nproj172, 0)
      
      
      TM173 = matrix(c(ES,0,                   0, 0,
                       ET,LS173[i], 0, 0,
                       0,LT173[i], PS173, 0,
                       0,   0,                         PT173, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs173<-paste("count eggs with [playa = 173 ]")
      
      Juv173<-NLReport(CountEggs173)
      
      Nproj173[1, i-1] = Juv173+Nproj173[1,i-1]
      
      Nproj173[1:4,i] = (TM173%*%Nproj173[1:4 ,i-1])
      
      Adults173=floor(Nproj173[4, i])
      
      NoAdults173=paste("ask patches[if pxcor = 46 and pycor = 146 [ sprout-mosqs", Adults173," ]]")
      
      NLCommand(NoAdults173)
      
      Nproj173[4,i]=0
      
      Nproj173=ifelse(Nproj173>0.1, Nproj173, 0)
      
      
      TM174 = matrix(c(ES,0,                   0, 0,
                       ET,LS174[i], 0, 0,
                       0,LT174[i], PS174, 0,
                       0,   0,                         PT174, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs174<-paste("count eggs with [playa = 174 ]")
      
      Juv174<-NLReport(CountEggs174)
      
      Nproj174[1, i-1] = Juv174+Nproj174[1,i-1]
      
      Nproj174[1:4,i] = (TM174%*%Nproj174[1:4 ,i-1])
      
      Adults174=floor(Nproj174[4, i])
      
      NoAdults174=paste("ask patches[if pxcor = 419 and pycor = 340 [ sprout-mosqs", Adults174," ]]")
      
      NLCommand(NoAdults174)
      
      Nproj174[4,i]=0
      
      Nproj174=ifelse(Nproj174>0.1, Nproj174, 0)
      
      
      TM175 = matrix(c(ES,0,                   0, 0,
                       ET,LS175[i], 0, 0,
                       0,LT175[i], PS175, 0,
                       0,   0,                         PT175, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs175<-paste("count eggs with [playa = 175 ]")
      
      Juv175<-NLReport(CountEggs175)
      
      Nproj175[1, i-1] = Juv175+Nproj175[1,i-1]
      
      Nproj175[1:4,i] = (TM175%*%Nproj175[1:4 ,i-1])
      
      Adults175=floor(Nproj175[4, i])
      
      NoAdults175=paste("ask patches[if pxcor = 192 and pycor = 100 [ sprout-mosqs", Adults175," ]]")
      
      NLCommand(NoAdults175)
      
      Nproj175[4,i]=0
      
      Nproj175=ifelse(Nproj175>0.1, Nproj175, 0)
      
      
      TM176 = matrix(c(ES,0,                   0, 0,
                       ET,LS176[i], 0, 0,
                       0,LT176[i], PS176, 0,
                       0,   0,                         PT176, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs176<-paste("count eggs with [playa = 176 ]")
      
      Juv176<-NLReport(CountEggs176)
      
      Nproj176[1, i-1] = Juv176+Nproj176[1,i-1]
      
      Nproj176[1:4,i] = (TM176%*%Nproj176[1:4 ,i-1])
      
      Adults176=floor(Nproj176[4, i])
      
      NoAdults176=paste("ask patches[if pxcor = 168 and pycor = 125 [ sprout-mosqs", Adults176," ]]")
      
      NLCommand(NoAdults176)
      
      Nproj176[4,i]=0
      
      Nproj176=ifelse(Nproj176>0.1, Nproj176, 0)
      
      
      TM177 = matrix(c(ES,0,                   0, 0,
                       ET,LS177[i], 0, 0,
                       0,LT177[i], PS177, 0,
                       0,   0,                         PT177, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs177<-paste("count eggs with [playa = 177 ]")
      
      Juv177<-NLReport(CountEggs177)
      
      Nproj177[1, i-1] = Juv177+Nproj177[1,i-1]
      
      Nproj177[1:4,i] = (TM177%*%Nproj177[1:4 ,i-1])
      
      Adults177=floor(Nproj177[4, i])
      
      NoAdults177=paste("ask patches[if pxcor = 157 and pycor = 130 [ sprout-mosqs", Adults177," ]]")
      
      NLCommand(NoAdults177)
      
      Nproj177[4,i]=0
      
      Nproj177=ifelse(Nproj177>0.1, Nproj177, 0)
      
      
      TM178 = matrix(c(ES,0,                   0, 0,
                       ET,LS178[i], 0, 0,
                       0,LT178[i], PS178, 0,
                       0,   0,                         PT178, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs178<-paste("count eggs with [playa = 178 ]")
      
      Juv178<-NLReport(CountEggs178)
      
      Nproj178[1, i-1] = Juv178+Nproj178[1,i-1]
      
      Nproj178[1:4,i] = (TM178%*%Nproj178[1:4 ,i-1])
      
      Adults178=floor(Nproj178[4, i])
      
      NoAdults178=paste("ask patches[if pxcor = 177 and pycor = 125 [ sprout-mosqs", Adults178," ]]")
      
      NLCommand(NoAdults178)
      
      Nproj178[4,i]=0
      
      Nproj178=ifelse(Nproj178>0.1, Nproj178, 0)
      
      
      TM179 = matrix(c(ES,0,                   0, 0,
                       ET,LS179[i], 0, 0,
                       0,LT179[i], PS179, 0,
                       0,   0,                         PT179, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs179<-paste("count eggs with [playa = 179 ]")
      
      Juv179<-NLReport(CountEggs179)
      
      Nproj179[1, i-1] = Juv179+Nproj179[1,i-1]
      
      Nproj179[1:4,i] = (TM179%*%Nproj179[1:4 ,i-1])
      
      Adults179=floor(Nproj179[4, i])
      
      NoAdults179=paste("ask patches[if pxcor = 429 and pycor = 354 [ sprout-mosqs", Adults179," ]]")
      
      NLCommand(NoAdults179)
      
      Nproj179[4,i]=0
      
      Nproj179=ifelse(Nproj179>0.1, Nproj179, 0)
      
      
      TM180 = matrix(c(ES,0,                   0, 0,
                       ET,LS180[i], 0, 0,
                       0,LT180[i], PS180, 0,
                       0,   0,                         PT180, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs180<-paste("count eggs with [playa = 180 ]")
      
      Juv180<-NLReport(CountEggs180)
      
      Nproj180[1, i-1] = Juv180+Nproj180[1,i-1]
      
      Nproj180[1:4,i] = (TM180%*%Nproj180[1:4 ,i-1])
      
      Adults180=floor(Nproj180[4, i])
      
      NoAdults180=paste("ask patches[if pxcor = 211 and pycor = 372 [ sprout-mosqs", Adults180," ]]")
      
      NLCommand(NoAdults180)
      
      Nproj180[4,i]=0
      
      Nproj180=ifelse(Nproj180>0.1, Nproj180, 0)
      
      
      TM181 = matrix(c(ES,0,                   0, 0,
                       ET,LS181[i], 0, 0,
                       0,LT181[i], PS181, 0,
                       0,   0,                         PT181, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs181<-paste("count eggs with [playa = 181 ]")
      
      Juv181<-NLReport(CountEggs181)
      
      Nproj181[1, i-1] = Juv181+Nproj181[1,i-1]
      
      Nproj181[1:4,i] = (TM181%*%Nproj181[1:4 ,i-1])
      
      Adults181=floor(Nproj181[4, i])
      
      NoAdults181=paste("ask patches[if pxcor = 400 and pycor = 369 [ sprout-mosqs", Adults181," ]]")
      
      NLCommand(NoAdults181)
      
      Nproj181[4,i]=0
      
      Nproj181=ifelse(Nproj181>0.1, Nproj181, 0)
      
      
      TM182 = matrix(c(ES,0,                   0, 0,
                       ET,LS182[i], 0, 0,
                       0,LT182[i], PS182, 0,
                       0,   0,                         PT182, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs182<-paste("count eggs with [playa = 182 ]")
      
      Juv182<-NLReport(CountEggs182)
      
      Nproj182[1, i-1] = Juv182+Nproj182[1,i-1]
      
      Nproj182[1:4,i] = (TM182%*%Nproj182[1:4 ,i-1])
      
      Adults182=floor(Nproj182[4, i])
      
      NoAdults182=paste("ask patches[if pxcor = 334 and pycor = 320 [ sprout-mosqs", Adults182," ]]")
      
      NLCommand(NoAdults182)
      
      Nproj182[4,i]=0
      
      Nproj182=ifelse(Nproj182>0.1, Nproj182, 0)
      
      
      TM183 = matrix(c(ES,0,                   0, 0,
                       ET,LS183[i], 0, 0,
                       0,LT183[i], PS183, 0,
                       0,   0,                         PT183, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs183<-paste("count eggs with [playa = 183 ]")
      
      Juv183<-NLReport(CountEggs183)
      
      Nproj183[1, i-1] = Juv183+Nproj183[1,i-1]
      
      Nproj183[1:4,i] = (TM183%*%Nproj183[1:4 ,i-1])
      
      Adults183=floor(Nproj183[4, i])
      
      NoAdults183=paste("ask patches[if pxcor = 187 and pycor = 85 [ sprout-mosqs", Adults183," ]]")
      
      NLCommand(NoAdults183)
      
      Nproj183[4,i]=0
      
      Nproj183=ifelse(Nproj183>0.1, Nproj183, 0)
      
      
      TM184 = matrix(c(ES,0,                   0, 0,
                       ET,LS184[i], 0, 0,
                       0,LT184[i], PS184, 0,
                       0,   0,                         PT184, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs184<-paste("count eggs with [playa = 184 ]")
      
      Juv184<-NLReport(CountEggs184)
      
      Nproj184[1, i-1] = Juv184+Nproj184[1,i-1]
      
      Nproj184[1:4,i] = (TM184%*%Nproj184[1:4 ,i-1])
      
      Adults184=floor(Nproj184[4, i])
      
      NoAdults184=paste("ask patches[if pxcor = 426 and pycor = 345 [ sprout-mosqs", Adults184," ]]")
      
      NLCommand(NoAdults184)
      
      Nproj184[4,i]=0
      
      Nproj184=ifelse(Nproj184>0.1, Nproj184, 0)
      
      
      TM185 = matrix(c(ES,0,                   0, 0,
                       ET,LS185[i], 0, 0,
                       0,LT185[i], PS185, 0,
                       0,   0,                         PT185, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs185<-paste("count eggs with [playa = 185 ]")
      
      Juv185<-NLReport(CountEggs185)
      
      Nproj185[1, i-1] = Juv185+Nproj185[1,i-1]
      
      Nproj185[1:4,i] = (TM185%*%Nproj185[1:4 ,i-1])
      
      Adults185=floor(Nproj185[4, i])
      
      NoAdults185=paste("ask patches[if pxcor = 395 and pycor = 332 [ sprout-mosqs", Adults185," ]]")
      
      NLCommand(NoAdults185)
      
      Nproj185[4,i]=0
      
      Nproj185=ifelse(Nproj185>0.1, Nproj185, 0)
      
      
      TM186 = matrix(c(ES,0,                   0, 0,
                       ET,LS186[i], 0, 0,
                       0,LT186[i], PS186, 0,
                       0,   0,                         PT186, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs186<-paste("count eggs with [playa = 186 ]")
      
      Juv186<-NLReport(CountEggs186)
      
      Nproj186[1, i-1] = Juv186+Nproj186[1,i-1]
      
      Nproj186[1:4,i] = (TM186%*%Nproj186[1:4 ,i-1])
      
      Adults186=floor(Nproj186[4, i])
      
      NoAdults186=paste("ask patches[if pxcor = 246 and pycor = 310 [ sprout-mosqs", Adults186," ]]")
      
      NLCommand(NoAdults186)
      
      Nproj186[4,i]=0
      
      Nproj186=ifelse(Nproj186>0.1, Nproj186, 0)
      
      
      TM187 = matrix(c(ES,0,                   0, 0,
                       ET,LS187[i], 0, 0,
                       0,LT187[i], PS187, 0,
                       0,   0,                         PT187, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs187<-paste("count eggs with [playa = 187 ]")
      
      Juv187<-NLReport(CountEggs187)
      
      Nproj187[1, i-1] = Juv187+Nproj187[1,i-1]
      
      Nproj187[1:4,i] = (TM187%*%Nproj187[1:4 ,i-1])
      
      Adults187=floor(Nproj187[4, i])
      
      NoAdults187=paste("ask patches[if pxcor = 163 and pycor = 111 [ sprout-mosqs", Adults187," ]]")
      
      NLCommand(NoAdults187)
      
      Nproj187[4,i]=0
      
      Nproj187=ifelse(Nproj187>0.1, Nproj187, 0)
      
      
      TM188 = matrix(c(ES,0,                   0, 0,
                       ET,LS188[i], 0, 0,
                       0,LT188[i], PS188, 0,
                       0,   0,                         PT188, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs188<-paste("count eggs with [playa = 188 ]")
      
      Juv188<-NLReport(CountEggs188)
      
      Nproj188[1, i-1] = Juv188+Nproj188[1,i-1]
      
      Nproj188[1:4,i] = (TM188%*%Nproj188[1:4 ,i-1])
      
      Adults188=floor(Nproj188[4, i])
      
      NoAdults188=paste("ask patches[if pxcor = 168 and pycor = 284 [ sprout-mosqs", Adults188," ]]")
      
      NLCommand(NoAdults188)
      
      Nproj188[4,i]=0
      
      Nproj188=ifelse(Nproj188>0.1, Nproj188, 0)
      
      
      TM189 = matrix(c(ES,0,                   0, 0,
                       ET,LS189[i], 0, 0,
                       0,LT189[i], PS189, 0,
                       0,   0,                         PT189, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs189<-paste("count eggs with [playa = 189 ]")
      
      Juv189<-NLReport(CountEggs189)
      
      Nproj189[1, i-1] = Juv189+Nproj189[1,i-1]
      
      Nproj189[1:4,i] = (TM189%*%Nproj189[1:4 ,i-1])
      
      Adults189=floor(Nproj189[4, i])
      
      NoAdults189=paste("ask patches[if pxcor = 169 and pycor = 184 [ sprout-mosqs", Adults189," ]]")
      
      NLCommand(NoAdults189)
      
      Nproj189[4,i]=0
      
      Nproj189=ifelse(Nproj189>0.1, Nproj189, 0)
      
      
      TM190 = matrix(c(ES,0,                   0, 0,
                       ET,LS190[i], 0, 0,
                       0,LT190[i], PS190, 0,
                       0,   0,                         PT190, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs190<-paste("count eggs with [playa = 190 ]")
      
      Juv190<-NLReport(CountEggs190)
      
      Nproj190[1, i-1] = Juv190+Nproj190[1,i-1]
      
      Nproj190[1:4,i] = (TM190%*%Nproj190[1:4 ,i-1])
      
      Adults190=floor(Nproj190[4, i])
      
      NoAdults190=paste("ask patches[if pxcor = 216 and pycor = 143 [ sprout-mosqs", Adults190," ]]")
      
      NLCommand(NoAdults190)
      
      Nproj190[4,i]=0
      
      Nproj190=ifelse(Nproj190>0.1, Nproj190, 0)
      
      
      TM191 = matrix(c(ES,0,                   0, 0,
                       ET,LS191[i], 0, 0,
                       0,LT191[i], PS191, 0,
                       0,   0,                         PT191, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs191<-paste("count eggs with [playa = 191 ]")
      
      Juv191<-NLReport(CountEggs191)
      
      Nproj191[1, i-1] = Juv191+Nproj191[1,i-1]
      
      Nproj191[1:4,i] = (TM191%*%Nproj191[1:4 ,i-1])
      
      Adults191=floor(Nproj191[4, i])
      
      NoAdults191=paste("ask patches[if pxcor = 287 and pycor = 348 [ sprout-mosqs", Adults191," ]]")
      
      NLCommand(NoAdults191)
      
      Nproj191[4,i]=0
      
      Nproj191=ifelse(Nproj191>0.1, Nproj191, 0)
      
      
      TM192 = matrix(c(ES,0,                   0, 0,
                       ET,LS192[i], 0, 0,
                       0,LT192[i], PS192, 0,
                       0,   0,                         PT192, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs192<-paste("count eggs with [playa = 192 ]")
      
      Juv192<-NLReport(CountEggs192)
      
      Nproj192[1, i-1] = Juv192+Nproj192[1,i-1]
      
      Nproj192[1:4,i] = (TM192%*%Nproj192[1:4 ,i-1])
      
      Adults192=floor(Nproj192[4, i])
      
      NoAdults192=paste("ask patches[if pxcor = 175 and pycor = 228 [ sprout-mosqs", Adults192," ]]")
      
      NLCommand(NoAdults192)
      
      Nproj192[4,i]=0
      
      Nproj192=ifelse(Nproj192>0.1, Nproj192, 0)
      
      
      TM193 = matrix(c(ES,0,                   0, 0,
                       ET,LS193[i], 0, 0,
                       0,LT193[i], PS193, 0,
                       0,   0,                         PT193, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs193<-paste("count eggs with [playa = 193 ]")
      
      Juv193<-NLReport(CountEggs193)
      
      Nproj193[1, i-1] = Juv193+Nproj193[1,i-1]
      
      Nproj193[1:4,i] = (TM193%*%Nproj193[1:4 ,i-1])
      
      Adults193=floor(Nproj193[4, i])
      
      NoAdults193=paste("ask patches[if pxcor = 246 and pycor = 242 [ sprout-mosqs", Adults193," ]]")
      
      NLCommand(NoAdults193)
      
      Nproj193[4,i]=0
      
      Nproj193=ifelse(Nproj193>0.1, Nproj193, 0)
      
      
      TM194 = matrix(c(ES,0,                   0, 0,
                       ET,LS194[i], 0, 0,
                       0,LT194[i], PS194, 0,
                       0,   0,                         PT194, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs194<-paste("count eggs with [playa = 194 ]")
      
      Juv194<-NLReport(CountEggs194)
      
      Nproj194[1, i-1] = Juv194+Nproj194[1,i-1]
      
      Nproj194[1:4,i] = (TM194%*%Nproj194[1:4 ,i-1])
      
      Adults194=floor(Nproj194[4, i])
      
      NoAdults194=paste("ask patches[if pxcor = 215 and pycor = 233 [ sprout-mosqs", Adults194," ]]")
      
      NLCommand(NoAdults194)
      
      Nproj194[4,i]=0
      
      Nproj194=ifelse(Nproj194>0.1, Nproj194, 0)
      
      
      TM195 = matrix(c(ES,0,                   0, 0,
                       ET,LS195[i], 0, 0,
                       0,LT195[i], PS195, 0,
                       0,   0,                         PT195, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs195<-paste("count eggs with [playa = 195 ]")
      
      Juv195<-NLReport(CountEggs195)
      
      Nproj195[1, i-1] = Juv195+Nproj195[1,i-1]
      
      Nproj195[1:4,i] = (TM195%*%Nproj195[1:4 ,i-1])
      
      Adults195=floor(Nproj195[4, i])
      
      NoAdults195=paste("ask patches[if pxcor = 141 and pycor = 287 [ sprout-mosqs", Adults195," ]]")
      
      NLCommand(NoAdults195)
      
      Nproj195[4,i]=0
      
      Nproj195=ifelse(Nproj195>0.1, Nproj195, 0)
      
      
      TM196 = matrix(c(ES,0,                   0, 0,
                       ET,LS196[i], 0, 0,
                       0,LT196[i], PS196, 0,
                       0,   0,                         PT196, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs196<-paste("count eggs with [playa = 196 ]")
      
      Juv196<-NLReport(CountEggs196)
      
      Nproj196[1, i-1] = Juv196+Nproj196[1,i-1]
      
      Nproj196[1:4,i] = (TM196%*%Nproj196[1:4 ,i-1])
      
      Adults196=floor(Nproj196[4, i])
      
      NoAdults196=paste("ask patches[if pxcor = 444 and pycor = 242 [ sprout-mosqs", Adults196," ]]")
      
      NLCommand(NoAdults196)
      
      Nproj196[4,i]=0
      
      Nproj196=ifelse(Nproj196>0.1, Nproj196, 0)
      
      
      TM197 = matrix(c(ES,0,                   0, 0,
                       ET,LS197[i], 0, 0,
                       0,LT197[i], PS197, 0,
                       0,   0,                         PT197, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs197<-paste("count eggs with [playa = 197 ]")
      
      Juv197<-NLReport(CountEggs197)
      
      Nproj197[1, i-1] = Juv197+Nproj197[1,i-1]
      
      Nproj197[1:4,i] = (TM197%*%Nproj197[1:4 ,i-1])
      
      Adults197=floor(Nproj197[4, i])
      
      NoAdults197=paste("ask patches[if pxcor = 331 and pycor = 312 [ sprout-mosqs", Adults197," ]]")
      
      NLCommand(NoAdults197)
      
      Nproj197[4,i]=0
      
      Nproj197=ifelse(Nproj197>0.1, Nproj197, 0)
      
      
      TM198 = matrix(c(ES,0,                   0, 0,
                       ET,LS198[i], 0, 0,
                       0,LT198[i], PS198, 0,
                       0,   0,                         PT198, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs198<-paste("count eggs with [playa = 198 ]")
      
      Juv198<-NLReport(CountEggs198)
      
      Nproj198[1, i-1] = Juv198+Nproj198[1,i-1]
      
      Nproj198[1:4,i] = (TM198%*%Nproj198[1:4 ,i-1])
      
      Adults198=floor(Nproj198[4, i])
      
      NoAdults198=paste("ask patches[if pxcor = 66 and pycor = 77 [ sprout-mosqs", Adults198," ]]")
      
      NLCommand(NoAdults198)
      
      Nproj198[4,i]=0
      
      Nproj198=ifelse(Nproj198>0.1, Nproj198, 0)
      
      
      TM199 = matrix(c(ES,0,                   0, 0,
                       ET,LS199[i], 0, 0,
                       0,LT199[i], PS199, 0,
                       0,   0,                         PT199, 1)
                     , nr = 4, byrow=T) 
      
      
      
      CountEggs199<-paste("count eggs with [playa = 199 ]")
      
      Juv199<-NLReport(CountEggs199)
      
      Nproj199[1, i-1] = Juv199+Nproj199[1,i-1]
      
      Nproj199[1:4,i] = (TM199%*%Nproj199[1:4 ,i-1])
      
      Adults199=floor(Nproj199[4, i])
      
      NoAdults199=paste("ask patches[if pxcor = 289 and pycor = 117 [ sprout-mosqs", Adults199," ]]")
      
      NLCommand(NoAdults199)
      
      Nproj199[4,i]=0
      
      Nproj199=ifelse(Nproj199>0.1, Nproj199, 0)
      
      NLCommand("ask eggs[die]")  #eggs at the end of each iteration to die because they've already been added into the matrix models 
      
      ##################
      #Don't paste below this line 
      ###############
      
      NumberMosqs[i]=NLReport("count mosqs") 
      
      Mosqs<- NLGetAgentSet(c("xcor", "ycor"), "mosqs")  #get x and y coordinates of mosquitoes created turtles
      mosqloc<-Mosqs  #grabs x and y coordinates of Mosqs
      mosqloc$xcor<-ifelse(mosqloc$xcor<0, 0, floor(mosqloc$xcor))#grounds all coordinates to base coordinate
      mosqloc$ycor<-ifelse(mosqloc$ycor<0, 0, floor(mosqloc$ycor))#grounds all coordinates to base coordinate
      mosqloc$count<-rep(1, length(mosqloc[,1]))  #this and the next line count the number of mosquitoes per xcor and ycor coordinate
      mosqag<-aggregate(mosqloc$count, by=list(mosqloc$xcor, mosqloc$ycor), FUN="sum")
      
      names(mosqag)<-c("xcor", "ycor", "NoMosq")
      
      mosrasterlist[[i]]<-mosqag
      
    }
    
    #mosrasterlist1[[k]]<-mosrasterlist
    Wetlands<-NLGetAgentSet(c("xcor", "ycor"), "wetlands") #get x and y coordinates of created wetlands
    parameterset<-c(NLReport("Adultdaily-survival"),
                         NLReport("GC1Length"),
                         NLReport("GCsubLength"),
                         NLReport("fecundity"),
                         NLReport("avg.daily.dis"),
                         NLReport("long.dis.max"),
                         NLReport("longdis.disp.prob"),
                         LarvaSurvival1[1],
                         TimeToPupate1,
                         PupaSurvival1,
                         TimeToEmerge1,
                         EggDailySurvival1) 
    
    
#}
#  return(list(mosrasterlist1, NumberMosqs, Wetlands, parameterset))
  return(list(mosrasterlist, NumberMosqs, Wetlands, parameterset))
  NLQuit()
    }
```


#########################Model Running Section 

library(parallel)
library(doParallel)
NLQuit()
#outputfile="Control_Conditions_60Days_2_6_17_100runs"
outputfile="Treatment_Conditions_60days_2_9_17_200runs"
modelpath="C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/MosquitoModel_11_3_2016_Generalized_for_Distribution.nlogo"

VarMat=VarMat1

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20 ))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile, "_1", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_2", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile, "_3", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_4", ".RData", sep=""))
stopCluster(cl)


cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_5", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_6", ".RData", sep=""))
stopCluster(cl)


cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_7", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_8", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_9", ".RData", sep=""))
stopCluster(cl)

cl<-makeCluster(detectCores())
registerDoParallel(cl, cores=4)
system.time(output1<-foreach ( k = 1:20, .packages=c("RNetLogo")) %dopar% MosqMod(VarMat[k,],NoSimulations=20))
setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(output1, file=paste(outputfile,"_10", ".RData", sep=""))
stopCluster(cl)

rm(output1)


########
totaldata<-NULL
for (i in 1:10) {
load(paste("Control_Conditions_60Days_2_6_17_100runs_", i,".RData", sep="")) 
totaldata<-append(totaldata, output1)
rm(output1)}



totaldata<-NULL
for (i in 1:10) {
  load(paste("Treatment_Conditions_60Days_2_9_17_200runs_", i,".RData", sep="")) 
  totaldata<-append(totaldata, output1)
  rm(output1)}


#####



##3.1 minutes for each simulation; 


The below section plots the count of mosquitoes on a daily basis for all model runs
```{r}

MosCount<-matrix(0,nrow=60, ncol=length(totaldata))
for (i in 1:200) {
outputlist<-unlist(totaldata[[i]][2])
MosCount[,i]=outputlist
}


RowMeans1<-rowMeans(MosCount)

#dev.new()
matplot(MosCount, col="black",type="l",xlab="Time in Days", ylab="Number of Adults", main="Number of Adults Over Time", lty=1, sub="Nearest Wetland Oviposition Behavior, Landcover-based Movement")
lines(RowMeans1, col="red", lwd=2)

###

library(RNetLogo)

##Need to have the model open, in order to get some of the stuff out of it. 
##Need to run model without roads first because model with roads distorts the landscape so
#that we don't get all 199 wetlands 
nl.path<-"C:/Program Files (x86)/NetLogo 5.2.0"  #NetLogo file path on this computer
NLStart(nl.path, gui=F)  #Starting NetLogo from the filepate. Note, for multiple runs, turn off the the GUI for faster computation 
NLLoadModel("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/MosquitoModel_11_3_2016_Generalized_for_Distribution.nlogo")
NLLoadModel("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/MosquitoModel_2_8_2017_Generalized_for_Distribution_roads.nlogo")

NLQuit()

You can use this code to create rasters as needed from the mosquito output data. You have to specify them as results from the function. In the case below, the mosquito xy data is in the first result slot, the second model run, and the 10th time step. 


mosqag<-ModelRun[[1]][[2]]

mosgridID<-seq(1, ((x.minmax[2]+1)*(y.minmax[2]+1)),1) #this creates an overall grid

LandscapeRas<-NLGetPatches(c("pxcor", "pycor"),patchset="patches", as.data.frame=T)
MosLandscape<-merge(LandscapeRas, mosqag, by.x=c("pxcor", "pycor"), by.y=c("xcor", "ycor"), all=TRUE)

MosLandscape[is.na(MosLandscape)]<-0
mosraster<-raster(ncol=x.minmax[2]+1, nrow=y.minmax[2]+1, xmn=0, xmx=x.minmax[2], ymn=0, ymx=y.minmax[2])
values(mosraster)<-MosLandscape$NoMosq
head(MosLandscape)

```

You can use the code below to create rasters from multiple model runs from particular time steps. In the current coding, its pulling day 30 from each of the runs.  
```{r}
LandscapeRas<-NLGetPatches(c("pxcor", "pycor"),patchset="patches", as.data.frame=T)
rasterlist<-list()

for (i in 1:length(totaldata)){
modelrun<-totaldata[[i]][[1]][[40]]
pycor<-rep(seq(0,y.minmax[2],1), x.minmax[2]+1) ##These replicates the dimensions of the map
pxcor<-rep(seq(0,x.minmax[2],1), y.minmax[2]+1)
MosquitoTemplate<-data.frame(pxcor[order(pxcor)], pycor)
names(MosquitoTemplate)<-c("pxcor", "pycor")
MosLandscape<-merge(MosquitoTemplate, modelrun, by.x=c("pxcor", "pycor"), by.y=c("xcor", "ycor"), all=TRUE)
MosLandscape<-MosLandscape[order(-MosLandscape$pycor),] # this is necessary to line it up with the ordering of values in the raster
MosLandscape[is.na(MosLandscape)]<-0 ;# this sets NA's to 0's
mosraster<-raster(ncol=x.minmax[2]+1, nrow=y.minmax[2]+1, xmn=0, xmx=x.minmax[2], ymn=0, ymx=y.minmax[2])
values(mosraster)<-MosLandscape$NoMosq
rasterlist[[i]]<-mosraster
}

setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
save(rasterlist, file="Control_raster_list_2_7_17.RData")

setwd("C:/Users/danidaws/Documents/Master Documents/Research/NetLogo/Current RNetLogo_Model work/ModelScenarios/Work_2_5_2017")
load("Control_raster_list_2_7_17.RData")

```

library(raster)

#This section will plot the locations of mosquitoes in space, on top of a landcover map 
#This section creates a raster of landcover. 
NLCommand("resize-world 0 456 0 382") # this lines up the NetLogo world with the LandSat Data. 
NLCommand("setup")

x.minmax<-NLReport("(list min-pxcor max-pxcor)") #gets x extent
y.minmax<-NLReport("(list min-pycor max-pycor)") #gets y extent

locations<-NLReport("[ (list xcor ycor idnumber) ] of wetlands") #Se
wllocmap<-data.frame(matrix(unlist(locations), nrow=length(locations), byrow=T))
colnames(wllocmap)<-c("xcor", "ycor", "IDnumber") 
wllocations<-wllocmap[order(wllocmap$IDnumber),] #a list of the coordinates of all the the wetlands
NumWet<-length(wllocations[,1]) # counts the number of wetlands

##Setting wetland centroid numbers and their xcor and ycor's##
xcor<- wllocations$xcor
ycor<-wllocations$ycor   
WLSimNo<-wllocations$IDnumber  #This pulls the ID number

length(xcor)


HabitatPatch<-NLGetPatches(c("Habitat", "pxcor", "pycor", "popdensity", "vzltrap", "roadlayer"),patchset="patches", as.data.frame=T)
lcraster<-raster(ncol=x.minmax[2]+1, nrow=y.minmax[2]+1, xmn=0, xmx=x.minmax[2], ymn=0, ymx=y.minmax[2])
values(lcraster)<-HabitatPatch$Habitat
roadraster<-raster(ncol=x.minmax[2]+1, nrow=y.minmax[2]+1, xmn=0, xmx=x.minmax[2], ymn=0, ymx=y.minmax[2])
roadvalues<-HabitatPatch$roadlayer
roadvalues[is.na(roadvalues)]=0
roadvalues<-ifelse(roadvalues>0, 1,0)
values(roadraster)<-roadvalues

rasterlist
#MosquitoMap; this creates a mean mosquito value raster from the collection of rasters created above
meanraster<-mean(stack(rasterlist))

plot(meanraster, breaks=c(0.005, 0.01,0.015, 1.8), col=c("green", "blue", "orange", "red"), main="Number of Adult Mosquitoes per Grid Cell", xlab="xcor", ylab="ycor", legend=TRUE, interpolate=F)
#writeRaster(meanraster, "controlraster_2_7_17.asc", "ascii")

#This plots the landcover map and location of wetlands under the mosquitoes  
plot(lcraster, alpha=0.6, legend=F, col=terrain.colors(5), add=TRUE)
points(xcor, ycor,col="blue", cex=0.5) #plots the locations of wetlands


#This plots the landcover map and location of wetlands under the mosquitoes  
plot(roadraster, alpha=0.6, legend=F, add=T)
points(xcor, ycor,col="blue", cex=0.5) #plots the locations of wetlands



```

This will write out raster's 
```{r}
#Writing Rasters out of NetLogo
writeRaster(lcraster, "lcraster.asc", "ascii")
writeRaster(vzltrapraster, "vzltrapraster.asc", "ascii")



This is function that will calculate the relative contact risk based on Human Pop Density and Mosquito Density
The function outputs an image, as well as a raster and reclassification matrix on which to calculate statistics. You can alter the function if you want to write the raster to a file. 

#RiskSurface<-function(surface, Scenario){
  
  pdraster<-raster(ncol=x.minmax[2]+1, nrow=y.minmax[2]+1, xmn=0, xmx=x.minmax[2], ymn=0, ymx=y.minmax[2])
  values(pdraster)<-HabitatPatch$popdensity
  pdraster[is.na(pdraster[])] <- 0 #this takes care of some NA values
  popval<-getValues(pdraster)
  popval<-popval[popval>0]
  popdec<-quantile(popval, probs=c(0.2, 0.4, 0.6, 0.8, 1.0),type=7)
  #This reclassifies the pop raster into 20% quantiles
  m <- c(-10, 0, 0,  
         0, popdec[1], 1,
         popdec[1], popdec[2], 2,
         popdec[2], popdec[3], 3,
         popdec[3], popdec[4], 4,
         popdec[4], popdec[5], 5)
  
  
  rclmat_hum <- matrix(m, ncol=3, byrow=TRUE)
  rcpdraster <- reclassify(pdraster, rcl= rclmat_hum)
  plot(rcpdraster)
  
  ####mosquito part#####
  #Mosquito Surfaces
  #This reclassifies the mosquito raster into 20% quantiles
  surface<-meanraster
  mosrasval<-getValues(surface)
  
  probs<-c(seq(2,10,2)/100)
  
  m <- c(-1, 0, 0,  
         0, probs[1] ,1,  
         probs[1], probs[2], 2,
         probs[2], probs[3], 3,
         probs[3], probs[4], 4,
         probs[4], max(mosrasval), 5)
  rclmat_mos <- matrix(m, ncol=3, byrow=TRUE)
  mosras_reclass<-reclassify(surface, rcl=rclmat_mos)
  plot(mosras_reclass, col=c("white", "green", "yellow", "orange", "red"),  main="Mosquito Population Index")
  mosplot<-plot(lcraster, col=terrain.colors(5), legend=F, alpha=0.5, add=TRUE)
  CombinedRasters<-mosras_reclass + rcpdraster 
  m <- c(-1, 0, 0,  
         0, 5 ,1,  
         5, 6, 2,
         6, 7, 3,
         7, 8, 4,
         8, 10, 5)
    rimat_mos <- matrix(m, ncol=3, byrow=TRUE)
  ri_reclass<-reclassify(CombinedRasters, rcl=rimat_mos)
  #dev.new()
  riskplot<-plot(ri_reclass, col=c("white", "black", "green", "yellow", "orange", "red"), xlab="xcor", ylab="ycor", main="Relative Contact Risk")
  rivalues<-getValues(ri_reclass)
  #writeRaster(ri_reclass, "ri_reclass.asc", "ascii", overwrite=TRUE)
  #writeRaster(mosras_reclass, "mosrass_reclass.asc", "ascii",overwrite=TRUE)
  #writeRaster(lcraster, "lcraster.asc", "ascii",overwrite=TRUE)
  
 # return(list(mosplot,riskplot, rivalues, ri_reclass))
  
#}

```

Run the Function here
```{r}
RiskSurface(meanraster, "test")

```




###Select locations to treat based on risk 
points(xcor, ycor,col="blue", cex=0.5) #plots the points in R

WLxy<-cbind(xcor, ycor)
WLri<-extract(ri_reclass, WLxy, buffer=6)

WLri_High<-lapply(WLri, function(x) 5%in%x) #this selects wetlands that are near the higheset risk areas(areas with risk 5)
WLri_MedHigh<-lapply(WLri, function(x) c(5%in%x | 4%in%x)) #this selects wetlands that are near the higheset risk areas(areas with risk 5)

WLrivec_high<-unlist(WLri_High)
WLridf_high<-data.frame(WLrivec_high, c(seq(1:107), seq(109, 199))) ##Note, WL 108 is missing. 
###High Risk Wetlands 

#Now, we need to kno which ones are within 100 M of road.

WLroads<-extract(roadraster, WLxy, buffer=0.85)
WLroads_close<-lapply(WLroads, function(x) 1%in%x) #this selects wetlands that are near the higheset risk areas(areas with risk 5)
WLroads_close_vec<-unlist(WLroads_close)
WLroads_close_df<-data.frame(WLroads_close_vec, c(seq(1:107), seq(109, 199))) ##Note, WL 108 is missing.

##Combinining High Risk WL with WL near roads
wllocations1<-cbind(wllocations, WLridf_high,WLroads_close_df)

WL_highrisk_nearroads<-wllocations1[c(wllocations1[,4]=="TRUE" & wllocations1[,6]=="TRUE"),] 
length(WL_highrisk_nearroads[,1])

WL_highrisk_nearroads$IDnumber
#These are the WL near a high risk cell(720m), and that are near roads(100m) #36 Wetlands
  4   5   6  14  17  55  56  61  68  89  91 100 101 107 109 110 111 112 113 114 115 116 122 128 131 132 136 142 144
  150 155 175 178 183 187 190
