data_attr <- read.csv("att_mat.csv",header = T, na.strings = "no_value", encoding = "UTF-8")
is.logical(data_attr$RestaurantsDelivery)
data_attr$RestaurantsDelivery[data_attr$RestaurantsDelivery=="None"] <- NA
#data_attr$RestaurantsDelivery[is.na(data_attr$RestaurantsDelivery)] <- as.logical(data_attr$RestaurantsDelivery)
logicid <- c(1:6, 10,11,14,15)
for(i in logicid){
  data_attr[[i]][is.na(data_attr[[i]])] <- as.logical(data_attr[[i]][is.na(data_attr[[i]])])
}


address <- read.csv("burger_noreview.csv", encoding = "UTF-8",
                    colClasses = c("NULL","NULL",NA,NA,NA,NA,"NULL",NA,"NULL","NULL"))

data_origin <- cbind(data_attr,address)[,c(1:15,20)]
data <- dplyr::distinct(data)
data <- data[,c(1:15,20)]

colnames(data_origin) <- c("Delivery", "TakeOut","AcceptsCreditCards","Reservation","HasTV","GoodForGroups","PriceRange","NoiseLevel","Alcohol", "OutdoorSeats","GoodForGroups.1","Attire","WiFi","GoodForKids","BikeParking","stars_y")

model_origin <- rpart(stars_y~.,data = data_origin)
rpart.plot(model_origin,cex = 0.8)
