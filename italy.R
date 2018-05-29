
################ PRE_PROCESSING ###################
legend = read.csv("pollution_legend.csv", header = F)
legend$V8 = as.character(legend$V1)

#Getting the revelant csv files
reqd_data = list()
for(i in 1:nrow(legend)){
  reqd_data[[i]] = read.csv(paste0("pollution-mi/",list.files(path = "./pollution-mi/", pattern = legend$V8[i])), header = F)
}

#Separating the Date and Time and merging with Legend file
fin_data = list()
for(i in 1:length(reqd_data)){
  out = strsplit(as.character(reqd_data[[i]]$V2)," ") 
  reqd_data[[i]] = data.frame(reqd_data[[i]], do.call(rbind, out))
  reqd_data[[i]]$V2 = NULL
  fin_data[[i]] = merge(reqd_data[[i]],legend, by = "V1")
}

#Separating the pollutants measured across hours and measured across days
withtime = list()
without_time = list()
x = lapply(fin_data, function(x) ncol(x) == 11)

#Collecting the data into separate lists
withtime = fin_data[unlist(x)]
without_time = fin_data[!unlist(x)]

#Converting lists into a consolidated dataframe
withtime = do.call(rbind, withtime)
without_time = do.call(rbind, without_time)

#Naming the variables 
colnames(withtime) = c("id","measure","date","time","street","lat","long","type","unit","format","id_char")
colnames(without_time) = c("id","measure","date","street","lat","long","type","unit","format","id_char")

#Standardizing the pollution measurements to a single unit
withtime$unit = as.character(withtime$unit)
withtime$measure[which(withtime$unit == "ppb")] = withtime$measure[which(withtime$unit == "ppb")]*0.0409*28
withtime$measure[which(withtime$unit == "mg/m3")] = withtime$measure[which(withtime$unit == "mg/m3")]*1000
withtime$unit[which(withtime$unit == "ppb")] = "µg/m3"
withtime$unit[which(withtime$unit == "mg/m3")] = "µg/m3"

#Combining the type of pollutant and unit of measurement
withtime$type_and_unit = paste(withtime$type, withtime$unit, sep = "-")
without_time$type_and_unit = paste(without_time$type, without_time$unit, sep = "-")

#Separating the pollutants varying across days and hours
subset_ugm = subset(withtime, unit == "µg/m")
subset_ugm$time = NULL
without_time = rbind(without_time, subset_ugm)
withtime = withtime[-which(withtime$unit == "µg/m"),]

#Cleaning of data and spelling errors
withtime$type = as.character(withtime$type)
withtime$type[withtime$type == "Nitrogene Dioxide"]= "Nitrogen Dioxide"

without_time$type = as.character(without_time$type)
without_time$type[without_time$type == "Sulfur Dioxide"]= "Sulphur Dioxide"

withtime$type_and_unit = paste(withtime$type,withtime$unit, sep = "-")
without_time$type_and_unit = paste(without_time$type,without_time$unit, sep = "-")

# Normalizing of Data 
withtime$normal_measure = ave(withtime$measure, withtime$type, FUN = function(x) (x-min(x))/diff(range(x)))
without_time$normal_measure = ave(without_time$measure, without_time$type, FUN = function(x) (x-min(x))/diff(range(x)))

#Saving the data as CSV files
write.csv(withtime, "2014_milan_poll_withtime.csv")
write.csv(without_time, "2014_milan_poll_without_time.csv")

