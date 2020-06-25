####################################################################
#LOGISTIC REGRESSION - Predicting the minutes a flight might get delay 
####################################################################

# predictflights<-multmerge("C:/Users/narra/Documents/ProjectRShinyGroup10/Project-RShiny-Group10/predictionData")
# write.csv(predictflights,'predict_flights.csv')
predictflights <- read.csv("predict_flights.csv")
predictflights<-subset(predictflights, ORIGIN_CITY_NAME %in% c('Seattle, WA'))
predictflights$HOUR<-predictflights$CRS_DEP_TIME/100


flights <- select(predictflights, DEP_DELAY, HOUR,DAY_OF_MONTH,
                  MONTH, TAIL_NUM, OP_UNIQUE_CARRIER, ORIGIN_CITY_NAME,
                  DISTANCE, AIR_TIME)

ontime <- na.omit(flights)

ontime_sorted <- ontime[,c("DEP_DELAY", "MONTH", "OP_UNIQUE_CARRIER", "HOUR","DISTANCE","AIR_TIME")]
ontime_sorted$MONTH <- as.factor(ontime_sorted$MONTH)

# Select data to put into training


training_index <- createDataPartition(ontime_sorted$DEP_DELAY, p=0.75, list=FALSE)

# Create training & testing dataset

training_data <- ontime_sorted[training_index,] 
testing_data <- ontime_sorted[-training_index,] 


log_reg_mod <- train(DEP_DELAY ~ ., data = training_data, method = "glm",
                     trControl=trainControl(method = "cv", number = 5))
#naccuracy<-RMSE(as.numeric(as.character(log_reg_mod)),testing_data[,1])
