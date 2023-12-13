
#------- cleaning and setting preferences and loading DEM ----------------------------- 
{
  rm(list=ls()) # clean history 
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  source('functions/Viewshed.R')
  setEnv()
  # Sys.setenv(TZ = 'UTC')
}

#enter the geographical coordinates of two diagonal corners of the region
box <- data.frame(lat=c(32.595899,31.311894),lon=c(34.841037, 35.618320))
DEM_name<-"DEM_Files/Israel_DEM_File.tif" # DEM on geographic grid
ANTfilename <- "ANT_Table_Files/All_Lon_Lat_25%.csv"

# ------ Antenna Positions ---------------
DEM <- getDEM(type="file",box,filename=DEM_name,resoluton = 100) # retrieve and cut DEM from "tif" file
ANTS.df <- setANTS(ANTfilename,DEM)
viewSetup(ANTS.df,DEM)

# ------ Calculating viewshed for all Antennas serial version --------------------
ANTlist <-as.character(ANTS.df$ID)
transAlts <-  c(2) 
Layername <- "Res100_25%_2023Nov_21_RC"
SerialComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F) # terrestrial

# ------ Calculating viewshed for all Antennas parallel version--------------------
#detectCores()
#registerDoParallel(4)
#ParallelComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F)

# ------  substituting / adding a single layer in a LOSLayers file

listLOSFiles()
str_name <- "Res100_25%_2023Nov_21_RC_final"
str_name_new <- "Res100_25%_2023Nov_21_RC_new"
str_name_out <- "Res100_25%_2023Nov_21_RC_out"
listAnts(str_name)

source('functions/ShinyViewshed.R')

