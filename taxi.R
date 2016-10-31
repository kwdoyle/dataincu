library(data.table)
library(ggmap)


# Jan data
taxi <- fread("yellow_tripdata_2015-01.csv")




pickup_lat <- summary(taxi$pickup_latitude)
dropoff_lat <- summary(taxi$dropoff_latitude)
pickup_lon <- summary(taxi$pickup_longitude)
dropoff_lon <- summary(taxi$dropoff_longitude)

# cutoff stuff below 1st Qu. & above 3rd Qu.
taxi <- taxi[-c(which(taxi$pickup_latitude <= pickup_lat[2]),
                which(taxi$dropoff_latitude <= dropoff_lat[2]),
                which(taxi$pickup_latitude >= pickup_lat[5]),
                which(taxi$dropoff_latitude >= dropoff_lat[5]),
                which(taxi$pickup_longitude <= pickup_lon[2]),
                which(taxi$dropoff_longitude <= dropoff_lon[2]),
                which(taxi$pickup_longitude >= pickup_lon[5]),
                which(taxi$dropoff_longitude >= dropoff_lon[5])), ]




# July data

taxi2 <- fread("yellow_tripdata_2015-07.csv")


pickup_lat <- summary(taxi2$pickup_latitude)
dropoff_lat <- summary(taxi2$dropoff_latitude)
pickup_lon <- summary(taxi2$pickup_longitude)
dropoff_lon <- summary(taxi2$dropoff_longitude)

# cutoff stuff below 1st Qu. & above 3rd Qu.
taxi2 <- taxi2[-c(which(taxi2$pickup_latitude <= pickup_lat[2]),
                which(taxi2$dropoff_latitude <= dropoff_lat[2]),
                which(taxi2$pickup_latitude >= pickup_lat[5]),
                which(taxi2$dropoff_latitude >= dropoff_lat[5]),
                which(taxi2$pickup_longitude <= pickup_lon[2]),
                which(taxi2$dropoff_longitude <= dropoff_lon[2]),
                which(taxi2$pickup_longitude >= pickup_lon[5]),
                which(taxi2$dropoff_longitude >= dropoff_lon[5])), ]








## Find all uniqe days


# January
# these aren't in order
dates_pickup <- as.Date(taxi$tpep_pickup_datetime)

taxi <- cbind(taxi, pickupdate=dates_pickup)

# order dates in Jan
taxi <- taxi[order(taxi$pickupdate, decreasing=F), ]

# reassign this in order
dates_pickup <- as.Date(taxi$tpep_pickup_datetime)



# July
dates_pickup2 <- as.Date(taxi2$tpep_pickup_datetime)

taxi2 <- cbind(taxi2, pickupdate=dates_pickup2)



uniqdays <- unique(dates_pickup)
uniqdays2 <- unique(dates_pickup2)


for (i in 1:length(uniqdays)) {
  assign(paste("Jan", i, sep=""), taxi[c(which(taxi$pickupdate==uniqdays[i])), ])
}

for (i in 1:length(uniqdays2)) {
  assign(paste("Jul", i, sep=""), taxi2[c(which(taxi2$pickupdate==uniqdays2[i])), ])
}



############# Makes heat map - pickup Jan1 ################

map <- get_map(location = c(lon=mean(Jan1$pickup_longitude), 
                                   lat=mean(Jan1$pickup_latitude)), zoom=14,
                      maptype="roadmap", scale=2)



p <- ggmap(map)


p + geom_density2d(data=Jan1, 
                  aes(x=Jan1$pickup_longitude, y=Jan1$pickup_latitude), size = 0.3) +
  stat_density2d(data = Jan1, 
                 aes(x = Jan1$pickup_longitude, y = Jan1$pickup_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Pickup Locations: Jan 1")





# dropoff heatmap Jan1
map2 <- get_map(location = c(lon=mean(Jan1$dropoff_longitude), 
                            lat=mean(Jan1$dropoff_latitude)), zoom=14,
               maptype="roadmap", scale=2)


q <- ggmap(map2)


q + geom_density2d(data=Jan1, 
                   aes(x=Jan1$dropoff_longitude, y=Jan1$dropoff_latitude), size = 0.3) +
  stat_density2d(data = Jan1, 
                 aes(x = Jan1$dropoff_longitude, y = Jan1$dropoff_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Dropoff Locations: Jan 1")





# Jan 31
map3 <- get_map(location = c(lon=mean(Jan31$pickup_longitude), 
                            lat=mean(Jan31$pickup_latitude)), zoom=14,
               maptype="roadmap", scale=2)


r <- ggmap(map3)


r + geom_density2d(data=Jan31, 
                   aes(x=Jan31$pickup_longitude, y=Jan31$pickup_latitude), size = 0.3) +
  stat_density2d(data = Jan31, 
                 aes(x = Jan31$pickup_longitude, y = Jan31$pickup_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Pickup Locations: Jan 31")



# dropoff heatmap Jan31
map4 <- get_map(location = c(lon=mean(Jan31$dropoff_longitude), 
                             lat=mean(Jan31$dropoff_latitude)), zoom=14,
                maptype="roadmap", scale=2)


z <- ggmap(map4)


z + geom_density2d(data=Jan31, 
                   aes(x=Jan31$dropoff_longitude, y=Jan31$dropoff_latitude), size = 0.3) +
  stat_density2d(data = Jan31, 
                 aes(x = Jan31$dropoff_longitude, y = Jan31$dropoff_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Dropoff Locations: Jan 31")






# July
############# Makes heat map - pickup Jul4 ################

map5 <- get_map(location = c(lon=mean(Jul4$pickup_longitude), 
                            lat=mean(Jul4$pickup_latitude)), zoom=14,
               maptype="roadmap", scale=2)



t <- ggmap(map5)


t + geom_density2d(data=Jul4, 
                   aes(x=Jul4$pickup_longitude, y=Jul4$pickup_latitude), size = 0.3) +
  stat_density2d(data = Jul4, 
                 aes(x = Jul4$pickup_longitude, y = Jul4$pickup_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Pickup Locations: Jul 4")




# dropoff heatmap Jul4
map6 <- get_map(location = c(lon=mean(Jul4$dropoff_longitude), 
                             lat=mean(Jul4$dropoff_latitude)), zoom=14,
                maptype="roadmap", scale=2)


k <- ggmap(map6)


k + geom_density2d(data=Jul4, 
                   aes(x=Jul4$dropoff_longitude, y=Jul4$dropoff_latitude), size = 0.3) +
  stat_density2d(data = Jul4, 
                 aes(x = Jul4$dropoff_longitude, y = Jul4$dropoff_latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle("Dropoff Locations: Jul 4")
