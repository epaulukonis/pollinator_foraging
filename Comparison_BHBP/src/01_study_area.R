### Habitat generation file for BEEHAVE and BeePop+ comparison

###01 Study Area

#Edited by E. Paulukonis April 2022

import_start_time <- Sys.time()
print("stepping into 01_study_area.R")

#### Import study area data ----
# input state and species data
print(list.files(path=root_data_in, all.files=TRUE, full.names=FALSE)) #landcover data
lc_data_sf <- raster(file.path(root_data_in, list.files(path=root_data_in, pattern='.tif$', all.files=TRUE, full.names=FALSE)))
plot(lc_data_sf) #check that it's the right state/file

ap_df<-read.csv(file.path(root_data_in,"ap_locs.csv"))
coordinates(ap_df)=~Long+Lat #get coordinates
proj4string(ap_df)=CRS("+init=epsg:4326") # set it to lat-long
ap_pts = spTransform(ap_df,crs(lc_data_sf)) #transform to match NC habitat
ap_pts$ID<- 1:length(unique(ap_pts$Apiary))


# verify that they overlap visually
plot(lc_data_sf) 
plot(ap_pts, add=T)
names_ap<-ap_pts@data$Apiary
ID<-1:length(names_ap)
names_ap<-cbind(names_ap, ID)

#### Proportional landscapes -----
##In this section, we'll create a data-frame that contains the IDs to examine the proportion of  study area land cover in each buffer 


#depending on distance, use this 
mile1 = 1609.34
mile3 = 4828.02
mile5 = 8046.7


lc_buffer <- raster::extract(lc_data_sf,       # raster layer
                           ap_pts,            # SPDF 
                           buffer = mile1,   # define mile buffer
                           df=TRUE)         # return a data frame

lc_buffer<-as.data.frame(lc_buffer)
colnames(lc_buffer)[2]<-'Class'
lc_buffer<-merge(lc_buffer, names_ap, by="ID", all=T) #join to get apiary name
lc_buffer$Class<-ifelse(lc_buffer$Class < 100 & lc_buffer$Class != 37 | lc_buffer$Class > 200, 1, lc_buffer$Class)#we'll convert all crops into a single cultivated crop, so change everything below 100 and above 200
lc_buffer$Class<-ifelse(lc_buffer$Class == 176 | lc_buffer$Class == 37, 176, lc_buffer$Class) # recombine hay and pasture into 176
lc_buffer_n<-lc_buffer %>% group_by(ID, Class, names_ap) %>% summarize(count=n()) #summarize the number of pixels associated with each buffered apiary location, and their class, multiply by 900 to get area
lc_buffer_n<-lc_buffer_n %>%  #get total area of land cover within each 1 mile radius
  group_by(ID) %>% 
  mutate(AreaTotal=sum(count)*900)
lc_buffer_n$AreaClass<-lc_buffer_n$count*900
lc_buffer_n$prop<-(lc_buffer_n$AreaClass/lc_buffer_n$AreaTotal)*100

#look at average lc proportion by apiary
average_lc<-lc_buffer_n %>%
  group_by(Class) %>%
  summarise(avg = mean(prop), std= sd(prop)) 
average_lc

#plot the apiaries, 
plot(ap_pts, pch=16, add=T)
plot(lc_data_sf, add=T) 
apiaries<-unique(lc_buffer_n$names_ap)
text(ap_pts, labels=apiaries, pos = 4, offset = 0.7)


#write a csv to output proportional data, if needed
write.csv(lc_buffer_n, paste0(root_data_out,"/landcover_data_1m.csv")) # comment out accorinding to which distance 
# write.csv(lc_buffer_n, paste0(root_data_out,"/landcover_data_3m.csv"))
# write.csv(lc_buffer_n, paste0(root_data_out,"/landcover_data_5m.csv"))

