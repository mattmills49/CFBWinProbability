% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/AddTime.R
\name{AddTime}
\alias{AddTime}
\title{Fill in missing time remaining for CFB Stats plays}
\usage{
AddTime(plays)
}
\arguments{
\item{plays}{a data frame containing plays from the CFB Stats play.csv file}
}
\value{
A data frame with a new column called New.Clock with the estimated time remaining of each play
}
\description{
This function estimates the time remaining of all plays in the CFB Stats
play file. It does this by extrapolating the values between instances where
there are times listed in the file.
}
\examples{
plays <- readin("play", 2010:2014)
newtime <- AddTime(plays)
ggplot2::qplot(x = New.Clock, data = newtime)
}

