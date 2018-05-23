
airlineForecast <- function(trainingDataFile, validationDataFile) {
  
  tdf <- read.csv(trainingDataFile)
  vdf <- read.csv(validationDataFile)
  
  # NUMBER DAYS UNTIL DEPARTURE
  tdf$daystodepart <- as.numeric(as.Date(as.character(tdf$departure_date), format="%m/%d/%Y")) - 
                      as.numeric(as.Date(as.character(tdf$booking_date), format="%m/%d/%Y"))
  
  # DAY OF THE WEEK OF DEPARTURE
  tdf$departure_day <- weekdays(as.Date(as.character(tdf$departure_date), format="%m/%d/%Y"))
  
  # SUBSET: Days until departure is less than 28
  tdf <- tdf[which(tdf$daystodepart <= 28), ]
  
  # CREATE A VECTOR OF MULTIPLIERS
  # each multiplier is calculated as the cum_bookings of the day after divided by the cum_bookings current day
  multipliers <- c()
  for (i in 1:nrow(tdf)) {
      multipliers[i] <- tdf[i+1, 3] / tdf[i, 3]
  }
  tdf$multipliers <- multipliers
  
  # REMOVE MULTIPLIERS FROM daystodepart-0, which correspond to multipliers of less than 1.
  # cum_bookings never decreases unless we are moving on to a new departure date.
  tdf <- tdf[which(tdf$multipliers >= 1), ]
  
  # AVERAGE MULTIPLIER FOR EVERY COMBINATION OF dayoftheweek AND daystodepart
  # 203 rows = 28 daystodepart * 7 daysoftheweek
  df.multi <- aggregate(data.frame(tdf$multipliers),
                        by=list(tdf$departure_day, tdf$daystodepart),
                        FUN=mean)
  
  # CONVERT DATA FRAME TO TABLE
  multi <- xtabs(df.multi[ ,3] ~ df.multi[ ,2] + df.multi[ ,1], df.multi)
  
  # REORDER TABLE BY DAYS OF THE WEEK
  multi <- multi[ ,c("Monday",
                     "Tuesday",
                     "Wednesday",
                     "Thursday",
                     "Friday",
                     "Saturday",
                     "Sunday")]
  
  # CREATE SEPARATE MODELS FOR EACH DEPARTURE DAY OF THE WEEK
  leavemon <- multi[ ,1]
  leavetue <- multi[ ,2]
  leavewed <- multi[ ,3]
  leavethu <- multi[ ,4]
  leavefri <- multi[ ,5]
  leavesat <- multi[ ,6]
  leavesun <- multi[ ,7]
  
  # CREATE A LIST OF THOSE MODELS
  multilist <- list(leavemon,
                    leavetue,
                    leavewed,
                    leavethu,
                    leavefri,
                    leavesat,
                    leavesun)
  
  # CREATE LIST CALCULATING CUMULATIVE MULTIPLIERS
  # for each specific combination of daytodepart (1-28) and dayofweek (Mon-Sun)
  day <- list()
  for (i in 1:length(multilist)) {
    cum.multi <- c()
    for (j in 1:length(multilist[[i]])) {
      x <- prod(multilist[[i]][1:j])
      cum.multi <- c(cum.multi, x)
    }
    day[[i]] <- cum.multi
  }
  
  # PREPARE VALIDATION SET
  vdf$daystodepart <- as.numeric(as.Date(as.character(vdf$departure_date), format="%m/%d/%Y")) - 
                     as.numeric(as.Date(as.character(vdf$booking_date), format="%m/%d/%Y"))
  vdf$departure_day <- weekdays(as.Date(as.character(vdf$departure_date), format="%m/%d/%Y"))
  vdf <- vdf[which(vdf$daystodepart != 0), ]
  vdf <- vdf[order(vdf$departure_date, -vdf$daystodepart), ]
  
  # CREATE VECTOR FOR PREDICTED VALUES
  # Determines which day of departure and # of daystodepart and points to the corresponding cum multipliers
  # multiplies them out to get predicted values.
  predicted <- c()
  model <- c()
  for (i in 1:nrow(vdf)) {
    if        (vdf[i, 7] == "Monday") {
      index<- 1
    } else if (vdf[i, 7] == "Tuesday") {
      index<- 2
    } else if (vdf[i, 7] == "Wednesday") {
      index<- 3
    } else if (vdf[i, 7] == "Thursday") {
      index<- 4
    } else if (vdf[i, 7] == "Friday") {
      index<- 5
    } else if (vdf[i, 7] == "Saturday") {
      index<- 6
    } else if (vdf[i, 7] == "Sunday") {
      index<- 7
    }
    predicted[i] <- day[[index]][vdf[i, 6]] * vdf[i, 3]
  }
  
  vdf$predicted <- predicted
  
  # CALCULATE MASE
  MASE.values <- vdf[which(vdf$daystodepart >= 1 & vdf$daystodepart <= 7), ]
  MASE <- (sum(abs(MASE.values$final_demand - MASE.values$predicted))) /
          (sum(abs(MASE.values$final_demand - MASE.values$naive_forecast)))
  MASE
  
  # PREPARE DATA FOR OUTPUT
  result.1 <- subset(vdf, select = c(departure_date,
                                     booking_date,
                                     predicted))
  result<- list(result.1,
                MASE)
  names(result)<- c("vdf with Predictions",
                    "MASE")
  return(result)
}

airlineForecast("airline_booking_trainingData.csv",
                "airline_booking_validationData.csv")