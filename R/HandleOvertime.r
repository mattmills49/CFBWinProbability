#' Structure plays correctly for games that went into overtime
#' 
#' This function takes in overtime plays and gets them in the same structure as
#' plays in regulation. This function is called from the DataPrep function. 
#' @param overtime a data frame containing overtime plays from the CFB Stats play.csv file
#' @return A data frame with the overtime plays. 
#' @examples


HandleOvertime <- function(overtime){
  overtime$Down <- with(overtime, ifelse(is.na(Down), 5 ,Down))
  overtime$Distance <- with(overtime, ifelse(is.na(Distance), 0, Distance))
  for(i in 2:nrow(overtime)){ 
    if(is.na(overtime[i,"Drive.Number"])){
      overtime[i,"Drive.Number"] <- overtime[i-1,"Drive.Number"]}
  }
  games <- unique(overtime$Game.Code)
  fixedot <- data.frame()
  for(game in games){
    gameot <- filter(overtime,Game.Code == game)
    first.drive.num <- gameot[1,"Drive.Number"]
    gameot$OT.Drive.Number <- with(gameot,ifelse((first.drive.num - Drive.Number) %% 2 == 0,1,2))
    fixedot <- rbind(fixedot,gameot)
  }
  clock <- 0
  fixedot$Seconds.Remaining <- 0
  currgame <- fixedot$Game.Code[1]
  for(i in 1:nrow(fixedot)){
    if(fixedot$Game.Code[i] == currgame){
      fixedot$Seconds.Remaining[i] <- clock - 25
      clock <- clock - 25
    }
    if(fixedot$Game.Code[i] != currgame){
      currgame <- fixedot$Game.Code[i]
      clock <- 0
      fixedot$Seconds.Remaining[i] <- clock - 25
    }
  }
  return(fixedot)
}