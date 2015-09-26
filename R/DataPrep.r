#' Format data to be used in regression fitting
#' 
#' This function takes in a data frame of plays read in from the CFB Stats
#' play by play file after the time of each play has been added. The function
#' determines a winner of each game, adjusts the down, distance, and spot
#' for kickoffs, determines if the game is against an FCS opponent, adds other
#' indicator variables, and handles overtime plays. 
#' @param plays a data frame containing plays from the CFB Stats play.csv file
#' @param yearvec the numeric vector of years to read in the data for
#' @param return.ot logical indicator on what to do with overtime plays
#' @return A list containing the plays in regulation. If \code{return.ot} is \code{TRUE} then the list contains a second data frame for overtime plays
#' @importFrom magrittr `%>%`
#' @examples
#' plays <- readin("play", 2010:2014)
#' newtime <- AddTime(plays)
#' cleandata <- DataPrep(newtime, 2010:2014, return.ot = T)
#' head(cleandata[[1]])
#' head(cleandata[[2]])

DataPrep <- function(plays, yearvec, return.ot = F){
  conf <- readin("conference", yearvec)
  teams <- readin("team", yearvec)
  confinfo <- dplyr::inner_join(teams, conf, by = c("Year", "Conference.Code"))
  plays$Down[plays$Play.Type == "KICKOFF"] <- 5
  plays$Td.Penalty <- with(plays, ifelse(Play.Type == "PENALTY" & Spot == 3 & Distance == 3 & Down == 1, 1, 0)) # handles some plays where there is a penatly on a PAT
  gameinfo <- readin("game", yearvec)
  statsinfo <- readin("team-game-statistics", yearvec)
  # Add in the score of each game
  gamescores <- dplyr::left_join(gameinfo, statsinfo %>% dplyr::select(Game.Code, Team.Code, Points, Year), by = c("Year" = "Year", "Game.Code" = "Game.Code", "Home.Team.Code" = "Team.Code")) %>% dplyr::left_join(statsinfo %>% dplyr::select(Game.Code, Team.Code, Points, Year), by = c("Year" = "Year", "Game.Code" = "Game.Code", "Visit.Team.Code" = "Team.Code"))
  names(gamescores)[8:9] <- c("Home.Team.Points","Visit.Team.Points")
  gamescores <- dplyr::inner_join(gamescores, confinfo %>% dplyr::select(Year, Team.Code, Subdivision), by = c("Year" = "Year", "Visit.Team.Code" = "Team.Code"))
  gamescores$Home.Team.Win <- with(gamescores, (ifelse(Home.Team.Points > Visit.Team.Points, 1, 0)))
  gamescores$FCS <- with(gamescores, ifelse(Subdivision == "FCS", 1, 0))
  justplays <- dplyr::left_join(plays, gamescores %>% select(-Date,-Stadium.Code),by = c("Year", "Game.Code"))
  justplays$Distance[justplays$Play.Type == "KICKOFF"] <- 0
  justplays$Spot[justplays$Play.Type == "KICKOFF"] <- 0
  justplays$Is.Attempt <- with(justplays,ifelse(Play.Type == "KICKOFF", 1, 0))
  justplays$Red.Zone <- with(justplays,ifelse(Spot <= 20, 1, 0))
  justplays$FG.Range <- with(justplays,ifelse(Spot <= 35, 1, 0))
  justplays$Win <- with(justplays, ifelse(Offense.Team.Code == Home.Team.Code, ifelse(Home.Team.Win == 1, 1, 0), ifelse(Home.Team.Win == 1, 0, 1)))
  justplays$Is.Home <- with(justplays, ifelse(Offense.Team.Code == Home.Team.Code & Site == "TEAM", 1, 0))
  justplays$Is.Neutral <- with(justplays, ifelse(Site == "NEUTRAL", 1, 0))
  justplays$Lead <- justplays$Offense.Points - justplays$Defense.Points
  ot <- dplyr::filter(justplays, Period.Number == 5)
  justplays <- dplyr::filter(justplays, Play.Type != "TIMEOUT", Play.Type != "ATTEMPT", !is.na(Down), Td.Penalty == 0, Period.Number != 5)
  justplays$Two.Min <- with(justplays, ifelse(Period.Number == 2 | Period.Number == 4, ifelse(New.Clock <= 120, 1, 0), 0))
  justplays$One.P <- with(justplays, ifelse(Lead <= 8 & Lead >= -8, 1, 0))
  justplays$Two.P <- with(justplays, ifelse(Lead <= 16 & Lead >= -16, 1, 0))
  justplays$FG.P <- with(justplays, ifelse(Lead <= 3 & Lead >= -3, 1, 0))
  justplays$Seconds.Remaining <- with(justplays, ifelse(Period.Number == 1, New.Clock + 2700, ifelse(Period.Number == 2, New.Clock + 1800, ifelse(Period.Number == 3, New.Clock + 900, New.Clock))))
  justplays$Seconds.Remaining <- with(justplays, ifelse(Seconds.Remaining == 0, 1, Seconds.Remaining))
  if(return.ot){
    if(nrow(ot) > 0) overtime <- HandleOvertime(ot)
    if(nrow(ot) == 0) overtime <- "none"
    return(list(justplays,overtime))
  }
  if(!return.ot){
    return(list(justplays))
  }
}