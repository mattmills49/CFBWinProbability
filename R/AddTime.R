#' Fill in missing time remaining for CFB Stats plays
#' 
#' This function estimates the time remaining of all plays in the CFB Stats 
#' play file. It does this by extrapolating the values between instances where
#' there are times listed in the file. 
#' @param plays a data frame containing plays from the CFB Stats play.csv file
#' @return A data frame with a new column called New.Clock with the estimated time remaining of each play
#' @examples
#' plays <- readin("play", 2010:2014)
#' newtime <- AddTime(plays)
#' ggplot2::qplot(x = New.Clock, data = newtime)
#' 
AddTime <- function(plays){
  clock <- plays$Clock # creates a vector of all the clock values
  previousclock <- 9000 # initializes the previous clock so it won't catch as NA the first time
  newclock <- rep(0,length(clock))
  currentclock <- clock[1]
  newclock[1] <- 900
  clockcount <- 1
  for(i in 2:length(clock)){ # starts at two because we have already handled play 1
    if(is.na(clock[i])){ # if there is no clock info we add a counter to record the play
      clockcount <- clockcount + 1
    }
    if(!is.na(clock[i])){ # if there is clock info we have three options:
      #1.) it is a new quarter
      #2.) it is a repeat clock like attempt and kickoff, same time but a new play
      #3.) there was a new time listed and we have to roll up time to the plays inbetween
      if(clock[i] == 900){
        clockvalues <- seq(currentclock, 10, length.out = clockcount) # creates the sequence of times from last recorded clock to 10 seconds
        for(time in 1:clockcount){ # this will take the times just created and assign them to the previous plays so that it fills all the "missing" times
          newclock[i - time] <- clockvalues[clockcount - time + 1]
        }
        clockcount <- 1
        currentclock <- 900
        newclock[i] <- 900
      }
      if(clock[i] != 900){
        if(clock[i] == currentclock){ # this is a special case for scores and kickoffs
          newclock[i] <- currentclock
          clockcount <- 1
        }
        if(clock[i] != currentclock){
          clockcount <- clockcount + 1
          previousclock <- currentclock
          currentclock <- clock[i]
          clockvalues <- seq(previousclock, currentclock, length.out = clockcount)
          for(time in 1:clockcount){
            newclock[i - time + 1] <- clockvalues[clockcount - time + 1]
          }
          clockcount <- 1
        }
      }
    }
  }
  plays$New.Clock <- newclock
  return(plays)
}