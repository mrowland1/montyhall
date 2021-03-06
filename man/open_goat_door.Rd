% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{\preformatted{	Open a Goat Door
}}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
the function returns a number between 1 and 3 to represent one of the three doors
}
\description{
\preformatted{Time to open one door that contains a "goat", 
this door cannot be the door that was selected
	by the contestant as their first pick. 
}
}
\details{
\preformatted{	If the contestant originally selected the car as their pick, 
	then either of the two remaining doors can be opened as 
	each would contain a goat. If the contestant chose a goat,
	then only the other remaining goat door can be opened. 
}
}
\examples{
	open_goat_door()

}
