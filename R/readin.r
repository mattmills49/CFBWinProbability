#' Read in data from multiple years of CFB Stats data
#' 
#' The function accepts a base filename from the CFB Stats data files and a
#' vector containing the years to read the data in. If there are multiple years
#' to read in the years are stacked and an extra variable is added called 
#' \code{Year} that contains the year of that data row. The package assumes 
#' the CFB Stats data are stored in a location with different folders for
#' each year and the name of the folders are \code{YYYY data}, e.g. 
#' 2009 data/play.csv . The function also replaces column names with a space
#' with a \code{.}. 
#' 
#' @param file The base name of the file to read in.
#' @param yearvec A numeric vector of years of data to read in at once.
#' @param folder_loc The location on your local drive of where the CFB Stats data is stored. 
#' @return A data frame containing the data file for all years. An additional column has been added with the year of each data row. 
#' @examples
#' plays <- readin("play", 2010:2014)
#' table(plays$Year)

readin <- function(file, yearvec, folder_loc = "~/Documents/CFB Data/"){
  cfb_data <- c()
  for(year in yearvec){
    info <- readr::read_csv(paste0(folder_loc, year, " data/",file,".csv"), col_names = T, progress = F)
    names(info) <- stringr::str_replace_all(names(info), pattern = " ", replacement = ".")
    info$Year <- year
    cfb_data <- dplyr::bind_rows(data, info)
  }
  return(cfb_data)
} 
