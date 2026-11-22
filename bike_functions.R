
#' Sort Inventory
#' 
#' @description Calculates inventory at each trip timepoint given bikesharing dataset with columns 
#' start_station, end_station, start_time, and end_time
#' @param data tidy dataframe with columns start_station, end_station, start_time, and end_time
#' @return df with sorted inventory at each trip timepoint

sort_inventory <- function(bikes){
  inventory <- data.frame()
  j <- 1
  
  for (i in seq_len(nrow(bikes))){
    start_st <- bikes[i, "start_station"]
    end_st <- bikes[i, "end_station"]
    start_tm <- bikes[i, "start_time"] %>% sub("\\..*", "", .)
    end_tm <- bikes[i, "end_time"] %>% sub("\\..*", "", .)
    
    if(start_st == "R"){
      inventory[j, "station"] <- end_st
      inventory[j, "time"] <- end_tm
      inventory[j, "inventory_change"] <- 1
      inventory[j, "event_type"] <- "R"
      
      j <- j +1
    }
    # realized this is pointless because there are 0 end stations marked R?
    # but i already wrote it so
    else if(end_st == "R"){
      inventory[j, "station"] <- start_st
      inventory[j, "time"] <- start_tm
      inventory[j, "inventory_change"] <- (-1)
      inventory[j, "event_type"] <- "R"
      
      j <- j +1
    }
    else{
      inventory[j, "station"] <- start_st
      inventory[j, "time"] <- start_tm
      inventory[j, "inventory_change"] <- (-1)
      inventory[j, "event_type"] <- "C"
      inventory[j + 1, "station"] <- end_st
      inventory[j + 1, "time"] <- end_tm
      inventory[j + 1, "inventory_change"] <- 1
      inventory[j + 1, "event_type"] <- "C"
      
      j <- j + 2
    }
  }
  
  sorted_inventory <- inventory[order(inventory$time, inventory$event_type),]
  
  stations <- unique(sorted_inventory$station)
  st_inventory <- setNames(rep(0, length(stations)), stations)
  sorted_inventory[0,"st_inventory"] <- 0
  
  for(i in seq_len(nrow(sorted_inventory))){
    st <- as.character(sorted_inventory$station[i])
    st_inventory[st] <- st_inventory[st] + sorted_inventory$inventory_change[i]
    sorted_inventory$station_inventory[i] <- st_inventory[st]
  }
  
  sorted_inventory %<>% select(-c(st_inventory))
  sorted_inventory %<>% mutate(time = hour(ymd_hms(as.character(time))))
  return(sorted_inventory)
}


#' Estimated Arrivals
#' 
#' @description Estimates arrival rate per hour for each unique trip start/end given 
#' bikeshare data, window_start date/time, and window_end date/time
#'
#' @param data tidy dataframe with columns X (trip i), start_station, end_station, and start_time
#' @param window_start date time in %Y-m%-%d %H:%M:%S format
#' @param window_end date time in %Y-m%-%d %H:%M:%S format
#' @return df with one row per unique trip start/end stations and columns origin, destination, 
#' hour (0--23), observed_rate, availability, and rate

arrivals <- function(data, window_start, window_end){
  day_dif <- as.numeric(difftime(window_end, window_start, units = "days"))
  result <- data %>% filter(start_time >= window_start, start_time <= window_end) %>%
    transmute(origin = as.character(start_station), destination = as.character(end_station), 
              hour = hour(start_time))
  station_pairs_total <- as.data.frame(table(result$origin, result$destination)) %>% 
    transmute(origin = as.character(Var1), destination = as.character(Var2), total_trips = Freq)
  station_pairs_hour <- as.data.frame(table(result$origin, result$destination, result$hour)) %>% 
    transmute(origin = as.character(Var1), destination = as.character(Var2), hour = as.integer(Var3), 
              observed_rate = (Freq/day_dif))
  result <- left_join(result, station_pairs_hour)
  result <- left_join(result, station_pairs_total)
  inventory <- sort_inventory(data) %>% transmute(origin = station, hour = time, 
                                                  availability = station_inventory)
  result <- left_join(result, inventory)
  
  avail_prop <- result %>% group_by(availability, origin, hour) %>%
    summarise(n = n(), .groups = "drop_last") %>% mutate(prop = n / sum(n))
  
  result$rate <- result$observed_rate/avail_prop$prop
  
  result <- result %>% mutate(rate = na_if(availability, 0)) %>% select(-c(total_trips))
  
  result %<>% distinct()
  
  return(result)
}
