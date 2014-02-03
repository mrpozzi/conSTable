\name{Objective Functions}
\alias{rrmseObj}
\alias{rmseObj}

\title{
Objective functions
}
\description{
Two examples of objective functions to minimize.
be minimized.
}
\usage{
rrmseObj(obsTable)
rmseObj(obsTable)
}

\arguments{
  \item{obsTable}{
A table assumed reliable we want to minimize the MSE-distance from
while enforcing balancedness.
}
}
\details{
This are popular choices for an objective but they also work as a
template for the user to write his own. the argument \code{objFun} in
\code{\link{sampleTables}} has to be a function which gets a table as
input and outputs a \code{''numeric''} value. 
}
\value{
 A function which takes a sampled table as input and computes the MSE or RMSE between the input table and \code{obsTable}.
}

\author{
Luca Pozzi, Marco Garieri, Natalia Golini

Maintainer: Luca Pozzi 
\email{luca.pozzi@fao.org}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sampleTables}}
}
\examples{
data("Italy2006FBsheet")
objF <- rrmseObj(Scenario)
objF(Scenario+1)
}
\keyword{objective function }
\keyword{sampling}
