library(readxl)
library(stringr)
burgers = read_excel("burger_noreview.xlsx")
attributes = burgers$attributes
att_mat = matrix("no_value",nrow = nrow(burgers),ncol=15)
colnames(att_mat) = c("RestaurantsDelivery","RestaurantsTakeOut","BusinessAcceptsCreditCards",
                      "RestaurantsReservations","HasTV","RestaurantsGoodForGroups",
                      "RestaurantsPriceRange2","NoiseLevel","Alcohol",
                      "OutdoorSeating","RestaurantsGoodForGroups","RestaurantsAttire",
                      "WiFi","GoodForKids","BikeParking")
for (i in 1:nrow(burgers)){
  if (is.na(attributes[i])){next}
  # RestaurantsDelivery
  if (str_detect(attributes[i],"'RestaurantsDelivery': '([a-zA-z_]+)'")){
    att_mat[i,1]=sub("^.*'RestaurantsDelivery': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsTakeOut
  if (str_detect(attributes[i],"'RestaurantsTakeOut': '([a-zA-z_]+)'")){
    att_mat[i,2]=sub("^.*'RestaurantsTakeOut': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # BusinessAcceptsCreditCards
  if (str_detect(attributes[i],"'BusinessAcceptsCreditCards': '([a-zA-z_]+)'")){
    att_mat[i,3]=sub("^.*'BusinessAcceptsCreditCards': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsReservations
  if (str_detect(attributes[i],"'RestaurantsReservations': '([a-zA-z_]+)'")){
    att_mat[i,4]=sub("^.*'RestaurantsReservations': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # HasTV
  if (str_detect(attributes[i],"'HasTV': '([a-zA-z_]+)'")){
    att_mat[i,5]=sub("^.*'HasTV': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsGoodForGroups
  if (str_detect(attributes[i],"'RestaurantsGoodForGroups': '([a-zA-z_]+)'")){
    att_mat[i,6]=sub("^.*'RestaurantsGoodForGroups': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsPriceRange2
  if (str_detect(attributes[i],"'RestaurantsPriceRange2': '([0-9]+)'")){
    att_mat[i,7]=sub("^.*'RestaurantsPriceRange2': '([0-9]+)'.*$",'\\1',attributes[i])
  }
  # NoiseLevel
  if (str_detect(attributes[i],"'NoiseLevel': \"u'([a-zA-z_]+)'")){
    att_mat[i,8]=sub("^.*'NoiseLevel': \"u'([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # Alcohol
  if (str_detect(attributes[i],"'Alcohol': \"u'([a-zA-z_]+)'")){
    att_mat[i,9]=sub("^.*'Alcohol': \"u'([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # OutdoorSeating
  if (str_detect(attributes[i],"'OutdoorSeating': '([a-zA-z_]+)'")){
    att_mat[i,10]=sub("^.*'OutdoorSeating': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsGoodForGroups
  if (str_detect(attributes[i],"'RestaurantsGoodForGroups': '([a-zA-z_]+)'")){
    att_mat[i,11]=sub("^.*'RestaurantsGoodForGroups': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # RestaurantsAttire
  if (str_detect(attributes[i],"'RestaurantsAttire': \"u'([a-zA-z_]+)'")){
    att_mat[i,12]=sub("^.*'RestaurantsAttire': \"u'([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # WiFi
  if (str_detect(attributes[i],"'WiFi': \"u'([a-zA-z_]+)'")){
    att_mat[i,13]=sub("^.*'WiFi': \"u'([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # GoodForKids
  if (str_detect(attributes[i],"'GoodForKids': '([a-zA-z_]+)'")){
    att_mat[i,14]=sub("^.*'GoodForKids': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
  # BikeParking
  if (str_detect(attributes[i],"'BikeParking': '([a-zA-z_]+)'")){
    att_mat[i,15]=sub("^.*'BikeParking': '([a-zA-z_]+)'.*$",'\\1',attributes[i])
  }
} 



