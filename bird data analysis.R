library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(MASS)
library(plyr)
library(stringr)


birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/bird data - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata$data.Site_ID[birddata$data.Site_ID=="Bcz 931"]="BCZ931"
birddata$data.Site_ID[birddata$data.Site_ID=="BGL 711"]="BGL711"
birddata$data.Site_ID[birddata$data.Site_ID=="BGO 694"]="BGO694"
birddata$data.Site_ID[birddata$data.Site_ID=="BHG 797"]="BHG797"
birddata$data.Site_ID[birddata$data.Site_ID=="BHY 708"]="BHY708"
birddata$data.Site_ID[birddata$data.Site_ID=="BID 721"]="BID721"
birddata$data.Site_ID[birddata$data.Site_ID=="BIM 638"]="BIM638"
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)


#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                      "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"))
extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/bird data - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)
#CUCE
#colnames(bird2)[1]<- "Key_1"
#CUCE1 <- join( birddata4[34],bird2[1:2], by = "Key_1")
#colnames(CUCE1)[2]<- "CUCE1"
#colnames(bird2)[1]<- "Key_2"
#CUCE2 <- join( birddata4[35],bird2[1:2], by = "Key_2") 
#colnames(CUCE2)[2]<- "CUCE2"
#colnames(bird2)[1]<- "Key_3"
#CUCE3 <- join( birddata4[36],bird2[1:2], by = "Key_3")  
#colnames(CUCE3)[2]<- "CUCE3"
#colnames(bird2)[1]<- "Key_4"
#CUCE4 <- join( birddata4[37],bird2[1:2], by = "Key_4")   
#colnames(CUCE4)[2]<- "CUCE4"

#CUCE<- cbind(CUCE1[2], CUCE2[2],CUCE3[2],CUCE4[2])


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
colnames(allbird1)[2:11] <- str_c( colnames(allbird1)[2:11] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:11] <- str_c( colnames(allbird2)[2:11] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:11] <- str_c( colnames(allbird3)[2:11] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:11] <- str_c( colnames(allbird4)[2:11] , ".4")


allbird<- cbind(allbird1[2:11], allbird2[2:11],allbird3[2:11],allbird4[2:11])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)


#4habirds
bird4ha<- read.csv("D:/Jobin IISER/8th Sem/bird data/bird data - data-Bird_data_4ha.csv")
bird4ha<- distinct(bird4ha)

fourhabird2<- dcast(bird4ha, PARENT_KEY ~  data.Bird_data_4ha.Select_bird_4ha, value.var ="data.Bird_data_4ha.Count_4ha", sum )



colnames(fourhabird2)[1]<- "Key_1"
fourhabird1 <- join( birddata4[34],fourhabird2, by = "Key_1")
library(stringr)
colnames(fourhabird1)[3:4] <- str_c( colnames(fourhabird1)[3:4] , "_1")

colnames(fourhabird2)[1]<- "Key_2"
fourhabird3 <- join( birddata4[35],fourhabird2, by = "Key_2")
colnames(fourhabird3)[3:4] <- str_c( colnames(fourhabird3)[3:4] , "_2")

colnames(fourhabird2)[1]<- "Key_3"
fourhabird4 <- join( birddata4[36],fourhabird2, by = "Key_3")
colnames(fourhabird4)[3:4] <- str_c( colnames(fourhabird4)[3:4] , "_3")

colnames(fourhabird2)[1]<- "Key_4"
fourhabird5 <- join( birddata4[37],fourhabird2, by = "Key_4")
colnames(fourhabird5)[3:4] <- str_c( colnames(fourhabird5)[3:4] , "_4")

fourhectarebird<- cbind(fourhabird1[3:4], fourhabird3[3:4],fourhabird4[3:4],fourhabird5[3:4])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird,fourhectarebird)


allbirdtotal$POHO.1 <- with(allbirdtotal, ifelse(is.na(POHO.1), NA_real_, ifelse(is.na(POHO_1), 0+POHO.1, POHO.1 + POHO_1)))
allbirdtotal$POHO.2 <- with(allbirdtotal, ifelse(is.na(POHO.2), NA_real_, ifelse(is.na(POHO_2), 0+POHO.2, POHO.2 + POHO_2)))
allbirdtotal$POHO.3 <- with(allbirdtotal, ifelse(is.na(POHO.3), NA_real_, ifelse(is.na(POHO_3), 0+POHO.3, POHO.3 + POHO_3)))
allbirdtotal$POHO.4 <- with(allbirdtotal, ifelse(is.na(POHO.4), NA_real_, ifelse(is.na(POHO_4), 0+POHO.4, POHO.4 + POHO_4)))


allbirdtotal$PYJO.1 <- with(allbirdtotal, ifelse(is.na(PYJO.1), NA_real_, ifelse(is.na(PYJO_1), 0+PYJO.1, PYJO.1 + PYJO_1)))
allbirdtotal$PYJO.2 <- with(allbirdtotal, ifelse(is.na(PYJO.2), NA_real_, ifelse(is.na(PYJO_2), 0+PYJO.2, PYJO.2 + PYJO_2)))
allbirdtotal$PYJO.3 <- with(allbirdtotal, ifelse(is.na(PYJO.3), NA_real_, ifelse(is.na(PYJO_3), 0+PYJO.3, PYJO.3 + PYJO_3)))
allbirdtotal$PYJO.4 <- with(allbirdtotal, ifelse(is.na(PYJO.4), NA_real_, ifelse(is.na(PYJO_4), 0+PYJO.4, PYJO.4 + PYJO_4)))


allbirdtotal$POHO_1<-NULL
allbirdtotal$POHO_2<-NULL
allbirdtotal$POHO_3<-NULL
allbirdtotal$POHO_4<-NULL
allbirdtotal$PYJO_1<-NULL
allbirdtotal$PYJO_2<-NULL
allbirdtotal$PYJO_3<-NULL
allbirdtotal$PYJO_4<-NULL


write.csv(allbirdtotal, "D:/Jobin IISER/8th Sem/bird data/jobin-allbirdtotal.csv")


####Saeed data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/saeed/Odk birds Saeed - Sheet2.csv")
sort(unique(birddata$data.Site_ID))


birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)


#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"))
extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/saeed/Odk birds Saeed - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird1[2:9], allbird2[2:9],allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "saeed-allbirdtotal.csv")

####Saeed data2####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/saeed2/Odk birds Saeed1 - Sheet1.csv")
sort(unique(birddata$data.Site_ID))
birddata$data.Site_ID[birddata$data.Site_ID=="AVC626"]="AVC625"
birddata$data.Plot_Size[birddata$data.Site_ID=="AVP621"& birddata$data.Plot_Size=="4hectare"]="1hectare"
birddata$data.Plot_Size[birddata$data.Site_ID=="AXQ695"& birddata$data.Plot_Size=="4hectare"]="1hectare"
birddata$data.Plot_Size[birddata$data.Site_ID=="AYB688"& birddata$data.Plot_Size=="4hectare"]="1hectare"



sort(unique(birddata$data.Site_ID))


birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/saeed2/Odk birds Saeed1 - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:11] <- str_c( colnames(allbird1)[2:11] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:11] <- str_c( colnames(allbird2)[2:11] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:11] <- str_c( colnames(allbird3)[2:11] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:11] <- str_c( colnames(allbird4)[2:11] , ".4")


allbird<- cbind(allbird1[2:11], allbird2[2:11],allbird3[2:11],allbird4[2:11])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "saeed2-allbirdtotal.csv")



####joseph data2####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/joseph/Joseph ODK Bird - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/joseph/Joseph ODK Bird - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird1[2:9], allbird2[2:9],allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "joseph-allbirdtotal.csv")



####mikhail data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/mikhail/Mikhail ODK bird - Sheet2.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/mikhail/Mikhail ODK bird - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird1[2:9], allbird2[2:9],allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "mikhail-allbirdtotal.csv")


####mikhail data2####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/mikhail2/Mikhail ODK bird 1 - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/mikhail2/Mikhail ODK bird 1 - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:10] <- str_c( colnames(allbird1)[2:10] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:10] <- str_c( colnames(allbird2)[2:10] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:10] <- str_c( colnames(allbird3)[2:10] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:10] <- str_c( colnames(allbird4)[2:10] , ".4")


allbird<- cbind(allbird1[2:10], allbird2[2:10],allbird3[2:10],allbird4[2:10])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "mikhail2-allbirdtotal.csv")


####vivek data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/Vivek/Vivek bird odk valparai - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/Vivek/Vivek bird odk valparai - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:10] <- str_c( colnames(allbird1)[2:10] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:10] <- str_c( colnames(allbird2)[2:10] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:10] <- str_c( colnames(allbird3)[2:10] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:10] <- str_c( colnames(allbird4)[2:10] , ".4")


allbird<- cbind(allbird1[2:10], allbird2[2:10],allbird3[2:10],allbird4[2:10])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

#4habirds
bird4ha<- read.csv("D:/Jobin IISER/8th Sem/bird data/Vivek/Vivek bird odk valparai - data-Bird_data_4ha.csv")
bird4ha<- distinct(bird4ha)

fourhabird2<- dcast(bird4ha, PARENT_KEY ~  data.Bird_data_4ha.Select_bird_4ha, value.var ="data.Bird_data_4ha.Count_4ha", sum )



colnames(fourhabird2)[1]<- "Key_1"
fourhabird1 <- join( birddata4[34],fourhabird2, by = "Key_1")
library(stringr)
colnames(fourhabird1)[2:3] <- str_c( colnames(fourhabird1)[2:3] , "_1")

colnames(fourhabird2)[1]<- "Key_2"
fourhabird3 <- join( birddata4[35],fourhabird2, by = "Key_2")
colnames(fourhabird3)[2:3] <- str_c( colnames(fourhabird3)[2:3] , "_2")

colnames(fourhabird2)[1]<- "Key_3"
fourhabird4 <- join( birddata4[36],fourhabird2, by = "Key_3")
colnames(fourhabird4)[2:3] <- str_c( colnames(fourhabird4)[2:3] , "_3")

colnames(fourhabird2)[1]<- "Key_4"
fourhabird5 <- join( birddata4[37],fourhabird2, by = "Key_4")
colnames(fourhabird5)[2:3] <- str_c( colnames(fourhabird5)[2:3] , "_4")

fourhectarebird<- cbind(fourhabird1[2:3], fourhabird3[2:3],fourhabird4[2:3],fourhabird5[2:3])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird,fourhectarebird)


allbirdtotal$POHO.1 <- with(allbirdtotal, ifelse(is.na(POHO.1), NA_real_, ifelse(is.na(POHO_1), 0+POHO.1, POHO.1 + POHO_1)))
allbirdtotal$POHO.2 <- with(allbirdtotal, ifelse(is.na(POHO.2), NA_real_, ifelse(is.na(POHO_2), 0+POHO.2, POHO.2 + POHO_2)))
allbirdtotal$POHO.3 <- with(allbirdtotal, ifelse(is.na(POHO.3), NA_real_, ifelse(is.na(POHO_3), 0+POHO.3, POHO.3 + POHO_3)))
allbirdtotal$POHO.4 <- with(allbirdtotal, ifelse(is.na(POHO.4), NA_real_, ifelse(is.na(POHO_4), 0+POHO.4, POHO.4 + POHO_4)))


allbirdtotal$PYJO.1 <- with(allbirdtotal, ifelse(is.na(PYJO.1), NA_real_, ifelse(is.na(PYJO_1), 0+PYJO.1, PYJO.1 + PYJO_1)))
allbirdtotal$PYJO.2 <- with(allbirdtotal, ifelse(is.na(PYJO.2), NA_real_, ifelse(is.na(PYJO_2), 0+PYJO.2, PYJO.2 + PYJO_2)))
allbirdtotal$PYJO.3 <- with(allbirdtotal, ifelse(is.na(PYJO.3), NA_real_, ifelse(is.na(PYJO_3), 0+PYJO.3, PYJO.3 + PYJO_3)))
allbirdtotal$PYJO.4 <- with(allbirdtotal, ifelse(is.na(PYJO.4), NA_real_, ifelse(is.na(PYJO_4), 0+PYJO.4, PYJO.4 + PYJO_4)))


allbirdtotal$POHO_1<-NULL
allbirdtotal$POHO_2<-NULL
allbirdtotal$POHO_3<-NULL
allbirdtotal$POHO_4<-NULL
allbirdtotal$PYJO_1<-NULL
allbirdtotal$PYJO_2<-NULL
allbirdtotal$PYJO_3<-NULL
allbirdtotal$PYJO_4<-NULL


write.csv(allbirdtotal, "Vivek-allbirdtotal.csv")


####sonia data2####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/sonia/sonia odk birds - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/sonia/sonia odk birds - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[26],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[27],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[28],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird1[2:9], allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(26:28)], allbird)

write.csv(allbirdtotal, "sonia-allbirdtotal.csv")


####arathi data1####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/arathi/arathi bird ODK - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/arathi/arathi bird ODK - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird1[2:9], allbird2[2:9],allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "arathi-allbirdtotal.csv")



####abhishaek data1####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/abhishaek/abhishaek odk bird valparai - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/abhishaek/abhishaek odk bird valparai - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )


library(plyr)


#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[26],bird2, by = "Key_1")
library(stringr)
colnames(allbird1)[2:9] <- str_c( colnames(allbird1)[2:9] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[27],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[28],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")




allbird<- cbind(allbird1[2:9], allbird2[2:9],allbird3[2:9])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

write.csv(allbirdtotal, "abhishaek-allbirdtotal.csv")




####meghana data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/meghana/bird data meghana - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/meghana/bird data meghana - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )




#bird

colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[26],bird2, by = "Key_2") 
colnames(allbird2)[2:9] <- str_c( colnames(allbird2)[2:9] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[27],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[28],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird2[2:9],allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(26:28)], allbird)

write.csv(allbirdtotal, "D:/Jobin IISER/8th Sem/bird data/meghana/meghana-allbirdtotal.csv")


#### roshan data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/roshan/Roshan bird data - Sheet1 (1).csv")
sort(unique(birddata$data.Site_ID))

birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/roshan/Roshan bird data - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )




#bird

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[18],bird2, by = "Key_3")  
colnames(allbird3)[2:9] <- str_c( colnames(allbird3)[2:9] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[19],bird2, by = "Key_4")  
colnames(allbird4)[2:9] <- str_c( colnames(allbird4)[2:9] , ".4")


allbird<- cbind(allbird3[2:9],allbird4[2:9])

allbirdtotal<- cbind(birddata4[,-c(18:19)], allbird)

write.csv(allbirdtotal, "D:/Jobin IISER/8th Sem/bird data/roshan/roshan-allbirdtotal.csv")



####JP data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/JP/JP bird data - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata$data.Site_ID[birddata$data.Site_ID=="BDX 959"]="BDX959"


birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "1ha_exit_time",
                                                                       "4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/JP/JP bird data - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )



#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
colnames(allbird1)[2:7] <- str_c( colnames(allbird1)[2:7] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:7] <- str_c( colnames(allbird2)[2:7] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:7] <- str_c( colnames(allbird3)[2:7] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:7] <- str_c( colnames(allbird4)[2:7] , ".4")


allbird<- cbind(allbird1[2:7], allbird2[2:7],allbird3[2:7],allbird4[2:7])
allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

#4habirds
bird4ha<- read.csv("D:/Jobin IISER/8th Sem/bird data/JP/JP bird data - data-Bird_data_4ha.csv")
bird4ha<- distinct(bird4ha)

fourhabird2<- dcast(bird4ha, PARENT_KEY ~  data.Bird_data_4ha.Select_bird_4ha, value.var ="data.Bird_data_4ha.Count_4ha", sum )



colnames(fourhabird2)[1]<- "Key_1"
fourhabird1 <- join( birddata4[34],fourhabird2, by = "Key_1")
colnames(fourhabird1)[2:3] <- str_c( colnames(fourhabird1)[2:3] , ".1")

colnames(fourhabird2)[1]<- "Key_2"
fourhabird3 <- join( birddata4[35],fourhabird2, by = "Key_2")
colnames(fourhabird3)[2:3] <- str_c( colnames(fourhabird3)[2:3] , ".2")

colnames(fourhabird2)[1]<- "Key_3"
fourhabird4 <- join( birddata4[36],fourhabird2, by = "Key_3")
colnames(fourhabird4)[2:3] <- str_c( colnames(fourhabird4)[2:3] , ".3")

colnames(fourhabird2)[1]<- "Key_4"
fourhabird5 <- join( birddata4[37],fourhabird2, by = "Key_4")
colnames(fourhabird5)[2:3] <- str_c( colnames(fourhabird5)[2:3] , ".4")

fourhectarebird<- cbind(fourhabird1[2:3], fourhabird3[2:3],fourhabird4[2:3],fourhabird5[2:3])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird,fourhectarebird)



allbirdtotal$POHO_1<-NULL
allbirdtotal$POHO_2<-NULL
allbirdtotal$POHO_3<-NULL
allbirdtotal$POHO_4<-NULL
allbirdtotal$PYJO_1<-NULL
allbirdtotal$PYJO_2<-NULL
allbirdtotal$PYJO_3<-NULL
allbirdtotal$PYJO_4<-NULL


write.csv(allbirdtotal, "D:/Jobin IISER/8th Sem/bird data/JP/JP-allbirdtotal.csv")

####Jobin2 data####
birddata<- read.csv("D:/Jobin IISER/8th Sem/bird data/jobin 2/2 bird data - Sheet1.csv")
sort(unique(birddata$data.Site_ID))

birddata$data.Site_ID[birddata$data.Site_ID=="BDZ 967"]="BDZ967"
birddata$data.Site_ID[birddata$data.Site_ID=="BFP 955"]="BFP955"
birddata$data.Site_ID[birddata$data.Site_ID=="BGL 711"]="BGL711"
birddata$data.Site_ID[birddata$data.Site_ID=="BGO 694"]="BGO694"
birddata$data.Site_ID[birddata$data.Site_ID=="BGT 732"]="BGT732"
birddata$data.Site_ID[birddata$data.Site_ID=="BGZ 816"]="BGZ816"
birddata$data.Site_ID[birddata$data.Site_ID=="BHE 943"]="BHE943"
birddata$data.Site_ID[birddata$data.Site_ID=="BHH 723"]="BHH723"
birddata$data.Site_ID[birddata$data.Site_ID=="BHH 724"]="BHH723"
birddata$data.Site_ID[birddata$data.Site_ID=="BHH 732"]="BHH732"
birddata$data.Site_ID[birddata$data.Site_ID=="BID 721"]="BID721"
birddata$data.Site_ID[birddata$data.Site_ID=="BIL 650"]="BIL650"
birddata$data.Site_ID[birddata$data.Site_ID=="BIS 646"]="BIS646"
birddata$data.Site_ID[birddata$data.Site_ID=="NM72."]="NM72"
birddata$data.Site_ID[birddata$data.Site_ID=="NN_54"]="NN54"
birddata$data.Site_ID[birddata$data.Site_ID=="NN54."]="NN54"
birddata$data.Site_ID[birddata$data.Site_ID=="NO61."]="NO61"
birddata$data.Site_ID[birddata$data.Site_ID=="NV  73"]="NV73"
birddata$data.Site_ID[birddata$data.Site_ID=="NV73."]="NV73"
birddata$data.Site_ID[birddata$data.Site_ID=="BMY9582"]="BMY958"





birddata<-birddata%>%separate(data.Lat_Long, c('Lat', 'Lon'), sep = ",")
birddata1<-birddata[,c(2,3,4,5,8:14, 15, 19, 23, 24)]
colnames(birddata1)[1] <- "Site_ID"
colnames(birddata1)[4] <- "Altitude"
colnames(birddata1)[5] <- "Plot_Size"
colnames(birddata1)[6] <- "Replicate"
colnames(birddata1)[7] <- "Date"
colnames(birddata1)[8] <- "Weather"
colnames(birddata1)[9] <- "Wind"
colnames(birddata1)[10] <- "Observer"
colnames(birddata1)[11] <- "Name_Observer"
colnames(birddata1)[12] <- "Start_time"
colnames(birddata1)[13] <- "1ha_exit_time"
colnames(birddata1)[14] <- "4ha_exit_time"
colnames(birddata1)[15] <- "Key"

# Remove duplicate rows of the dataframe
birddata1<- distinct(birddata1)

write.csv(birddata1, "D:/Jobin IISER/8th Sem/bird data/jobin 2/birddata1.csv")
birddata1<- read.csv("D:/Jobin IISER/8th Sem/bird data/jobin 2/birddata1.csv")

#reshaping data frame
birddata3<- dcast(setDT(birddata1), Site_ID ~ Replicate, value.var = c("Date","Weather","Wind", "Observer",
                                                                       "Name_Observer","Start_time", "X1ha_exit_time",
                                                                       "X4ha_exit_time", "Key"), fun.aggregate = NULL)

extra<-birddata1[,c(1,2,3,4,5)]%>%group_by(Site_ID)%>%slice(1)

birddata4 <- merge(as.data.frame(birddata3),extra,by=c("Site_ID","Site_ID"))


bird1<- read.csv("D:/Jobin IISER/8th Sem/bird data/jobin 2/2 bird data - data-Bird_data.csv")
bird1<- distinct(bird1)

bird2<- dcast(bird1, PARENT_KEY ~ data.Bird_data.Select_bird, value.var ="data.Bird_data.Count", sum )



#bird
colnames(bird2)[1]<- "Key_1"
allbird1 <- join( birddata4[34],bird2, by = "Key_1")
colnames(allbird1)[2:11] <- str_c( colnames(allbird1)[2:11] , ".1")


colnames(bird2)[1]<- "Key_2"
allbird2 <- join( birddata4[35],bird2, by = "Key_2") 
colnames(allbird2)[2:11] <- str_c( colnames(allbird2)[2:11] , ".2")

colnames(bird2)[1]<- "Key_3"
allbird3 <- join( birddata4[36],bird2, by = "Key_3")  
colnames(allbird3)[2:11] <- str_c( colnames(allbird3)[2:11] , ".3")

colnames(bird2)[1]<- "Key_4"
allbird4 <- join( birddata4[37],bird2, by = "Key_4")  
colnames(allbird4)[2:11] <- str_c( colnames(allbird4)[2:11] , ".4")


allbird<- cbind(allbird1[2:11], allbird2[2:11],allbird3[2:11],allbird4[2:11])
allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird)

#4habirds
bird4ha<- read.csv("D:/Jobin IISER/8th Sem/bird data/jobin 2/2 bird data - data-Bird_data_4ha.csv")
bird4ha<- distinct(bird4ha)

fourhabird2<- dcast(bird4ha, PARENT_KEY ~  X3d9d.4368.b7c, value.var ="data.Bird_data_4ha.Count_4ha", sum )



colnames(fourhabird2)[1]<- "Key_1"
fourhabird1 <- join( birddata4[34],fourhabird2, by = "Key_1")
library(stringr)
colnames(fourhabird1)[3:4] <- str_c( colnames(fourhabird1)[3:4] , "_1")

colnames(fourhabird2)[1]<- "Key_2"
fourhabird3 <- join( birddata4[35],fourhabird2, by = "Key_2")
colnames(fourhabird3)[3:4] <- str_c( colnames(fourhabird3)[3:4] , "_2")

colnames(fourhabird2)[1]<- "Key_3"
fourhabird4 <- join( birddata4[36],fourhabird2, by = "Key_3")
colnames(fourhabird4)[3:4] <- str_c( colnames(fourhabird4)[3:4] , "_3")

colnames(fourhabird2)[1]<- "Key_4"
fourhabird5 <- join( birddata4[37],fourhabird2, by = "Key_4")
colnames(fourhabird5)[3:4] <- str_c( colnames(fourhabird5)[3:4] , "_4")

fourhectarebird<- cbind(fourhabird1[3:4], fourhabird3[3:4],fourhabird4[3:4],fourhabird5[3:4])

allbirdtotal<- cbind(birddata4[,-c(34:37)], allbird,fourhectarebird)


allbirdtotal$POHO.1 <- with(allbirdtotal, ifelse(is.na(POHO.1), NA_real_, ifelse(is.na(POHO_1), 0+POHO.1, POHO.1 + POHO_1)))
allbirdtotal$POHO.2 <- with(allbirdtotal, ifelse(is.na(POHO.2), NA_real_, ifelse(is.na(POHO_2), 0+POHO.2, POHO.2 + POHO_2)))
allbirdtotal$POHO.3 <- with(allbirdtotal, ifelse(is.na(POHO.3), NA_real_, ifelse(is.na(POHO_3), 0+POHO.3, POHO.3 + POHO_3)))
allbirdtotal$POHO.4 <- with(allbirdtotal, ifelse(is.na(POHO.4), NA_real_, ifelse(is.na(POHO_4), 0+POHO.4, POHO.4 + POHO_4)))


allbirdtotal$PYJO.1 <- with(allbirdtotal, ifelse(is.na(PYJO.1), NA_real_, ifelse(is.na(PYJO_1), 0+PYJO.1, PYJO.1 + PYJO_1)))
allbirdtotal$PYJO.2 <- with(allbirdtotal, ifelse(is.na(PYJO.2), NA_real_, ifelse(is.na(PYJO_2), 0+PYJO.2, PYJO.2 + PYJO_2)))
allbirdtotal$PYJO.3 <- with(allbirdtotal, ifelse(is.na(PYJO.3), NA_real_, ifelse(is.na(PYJO_3), 0+PYJO.3, PYJO.3 + PYJO_3)))
allbirdtotal$PYJO.4 <- with(allbirdtotal, ifelse(is.na(PYJO.4), NA_real_, ifelse(is.na(PYJO_4), 0+PYJO.4, PYJO.4 + PYJO_4)))


allbirdtotal$POHO_1<-NULL
allbirdtotal$POHO_2<-NULL
allbirdtotal$POHO_3<-NULL
allbirdtotal$POHO_4<-NULL
allbirdtotal$PYJO_1<-NULL
allbirdtotal$PYJO_2<-NULL
allbirdtotal$PYJO_3<-NULL
allbirdtotal$PYJO_4<-NULL



write.csv(allbirdtotal, "D:/Jobin IISER/8th Sem/bird data/jobin 2/jobin2-allbirdtotal.csv")

