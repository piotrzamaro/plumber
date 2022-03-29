library(plumber)
library(dplyr)
library(ggplot2)
library(gapminder)
library(IPDfromKM)
library(survminer)

#* Returns countries that satisfy condition
#* @param nrisk_arm0
#* @param nrisk_arm1
#* @param time_risk
#* @param max_y
#* @param label_arm0
#* @param label_arm1
#* @get /km_table
#* @get /km_plot

function(nrisk_arm0, nrisk_arm1, time_risk,max_y,label_arm0,label_arm1){
  
  
  preprocess_arm0  =  preprocess(getpoints_arm0,           
                                 trisk = time_risk,
                                 nrisk = nrisk_arm0,       
                                 totalpts = nrisk_arm0[1], 
                                 maxy = max_y)             
  
  preprocess_arm1 = preprocess(getpoints_arm1,
                               trisk = time_risk,
                               nrisk = nrisk_arm1,
                               totalpts = nrisk_arm1[1],
                               maxy = max_y)
  
  
  getIPD_arm0 <-   getIPD(preprocess_arm0,
                          armID = 1)       
  getIPD_arm1 <-  getIPD(preprocess_arm1, 
                         armID = 2)
  
  ipd <- rbind( 
    data.frame(getIPD_arm0$IPD, "arm" = "0", "group" = label_arm0), 
    data.frame(getIPD_arm1$IPD, "arm" = "1", "group" = label_arm1))
  
  
  
}