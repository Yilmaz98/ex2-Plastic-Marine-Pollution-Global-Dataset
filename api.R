require(rfigshare)
require(xlsx)

#get the data

data.uri = fs_download(1015289)
#authentication:
#email: omercofman@gmail.com
#password: rabbit333
data.file.name ="pollution.xlsx"
download.file(data.uri, data.file.name, mode='wb')
data.xlsx = read.xlsx(data.file.name, sheetName = "Sheet1")

#originize the data

#remove extra header
data.xlsx = data.xlsx[-c(1),]
#remove unintresting columns
drops = c("Date","Source","Info","Sea.State","WD4..g.km.2..","NA.","NA..1","NA..2","NA..3","NA..4","Comments","CD4....km.2.")
data.xlsx = data.xlsx[, !(names(data.xlsx) %in% drops)]
#rename the columns
names(data.xlsx) = c("Latitude", "Longitude", "cd1", "cd2", "cd3", "wd1", "wd2", "wd3")
data.xlsx = transform(data.xlsx,
                      cd1=as.numeric(levels(cd1))[cd1],
                      cd2=as.numeric(levels(cd1))[cd2],
                      cd3=as.numeric(levels(cd3))[cd3],
                      wd1=as.numeric(levels(wd1))[wd1],
                      wd2=as.numeric(levels(wd2))[wd2],
                      wd3=as.numeric(levels(wd3))[wd3]);
#replace NA values with 0
data.xlsx[is.na(data.xlsx)] = 0
write.csv(data.xlsx, data.file.name)
data=data.xlsx


#get a clue
head(data);

summary(data)

require(ggmap)
par(mfrow=c(1,2))
map1 = get_map(location=c(lon=100+(mean(data$Longitude)),lat=mean(data$Latitude)),zoom=2,maptype="watercolor",scale=2)
map2 = get_map(location=c(lon=(mean(data$Longitude)-85),lat=mean(data$Latitude)),zoom=2,maptype="watercolor",scale=2)

ggmap(map1) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd1*data$wd1/.677/100),colour="darkred") + guides(fill=F,alpha=F,size=F)
ggmap(map2) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd1*data$wd1/.677/100),colour="darkred") + guides(fill=F,alpha=F,size=F)

ggmap(map1) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd2*data$wd2/2.875/100),colour="darkred") + guides(fill=F,alpha=F,size=F)
ggmap(map2) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd2*data$wd2/2.875/100),colour="darkred") + guides(fill=F,alpha=F,size=F)

ggmap(map1) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd3*data$wd3/100/100),colour="darkred") + guides(fill=F,alpha=F,size=F)
ggmap(map2) + geom_point(data=data,aes(x=Longitude,y=Latitude,alph=0.1),size=log(data$cd3*data$wd3/100/100),colour="darkred") + guides(fill=F,alpha=F,size=F)


data.1 = data[data$Longitude<200 & data$Longitude>150,]
data.2 = data[data$Longitude<150 & data$Longitude>50,]
data.3 = data[data$Longitude<50 & data$Longitude>30,]
data.4 = data[data$Longitude<30 & data$Longitude>-80,]
data.5 = data[data$Longitude<-80 & data$Longitude>-200,]

mean(data.1$cd1*data.1$wd1/.677 + data.1$cd2*data.1$wd2/2.875 + data.1$cd3*data.1$wd3/100)
mean(data.2$cd1*data.2$wd1/.677 + data.2$cd2*data.2$wd2/2.875 + data.2$cd3*data.2$wd3/100)
mean(data.3$cd1*data.3$wd1/.677 + data.3$cd2*data.3$wd2/2.875 + data.3$cd3*data.3$wd3/100)
mean(data.4$cd1*data.4$wd1/.677 + data.4$cd2*data.4$wd2/2.875 + data.4$cd3*data.4$wd3/100)
mean(data.5$cd1*data.5$wd1/.677 + data.5$cd2*data.5$wd2/2.875 + data.5$cd3*data.5$wd3/100)
sum(data.1$cd2)
sum(data.2$cd2)
sum(data.3$cd2)
sum(data.4$cd2)
sum(data.5$cd2)





