### NC habitat file generation for BeeHAVE

###03 Habitat File

#Edited by E. Paulukonis April 2022

forage_data<-read.csv(file = file.path(root_data_in, "forage_data.csv"))
lc_names<-read.csv(file = file.path(root_data_in, "LC_crosswalk.csv"))

output<-list()
for(lc in 1:length(list_clump)){

dar<-list_clump[[lc]] #individual LC class
  
  #patch area 
  habitatfile<-as.data.frame(tapply(area(dar), dar[], sum))
  names(habitatfile)<-'patch_area'
  habitatfile$patch<-row.names(habitatfile)
  
  #day
  habitatfile$day<-NA
  
  #patch distance (units based on projection)
  distp <- disaggregate(rasterToPolygons(dar,dissolve=T))# convert to vector to get distance, use dissolve to keep raster values combined and disaggregate to keep individual features
  distance_p<-as.vector(gDistance(apiary_d, distp, byid=TRUE))
  habitatfile$distance_m<-distance_p
  habitatfile$distance_m<-ifelse(habitatfile$distance_m == 0, 0.000001, habitatfile$distance_m)
  

  #x and y correlation
  habitatfile$xcor<- 0 
  habitatfile$ycor<- 0 
  
  #detection probability (decay with distance function, Becher et al. 2016)
  habitatfile$calculatedDetectionProb_per_t<-rip<-exp(1)^(-0.00073*habitatfile$distance_m)
  habitatfile$modelledDetectionProb_per_trip <-0
  
  #foraging time
  habitatfile$pollenGathering_s<- 600
  habitatfile$nectarGathering_s<- 1200
  
  #nectar and pollen pesticide content 
  habitatfile$nectarPesticide_ng_per_g<- 0 
  habitatfile$pollenPesticide_ng_per_g<- 0
  
  #foraging class
  names_classes<- lc_names[lc_names$CDLName == names(dar),1]
  habitatfile$patchType<-paste(names_classes, habitatfile$patch, sep="") 
  
  #split into n items by patch
  list_by_patch<-split(habitatfile,f=habitatfile$patch) 
  
  list_by_patch_all<-list()
  for(patch in 1:length(list_by_patch)){
    n=365
    x = do.call("rbind", replicate(n, list_by_patch[[patch]], simplify = FALSE))
    x$day<-1:365
    x$Pollen_g_m2<-0
    x$Nectar_ml_per_m2<-0
    x$concentration<-0
    row.names(x)<-x$day
    
    #first day of spring and first day of fall 
    for(i in 79:265){
      x[i,13]<-forage_data[1,2]
      x[i,14]<-forage_data[2,2]
      x[i,15]<-forage_data[3,2]
        }
    list_by_patch_all[[patch]]<-x
  }
  
  landcover_patches_all<-
  
output[[lc]]<-list_by_patch_all
  
}


