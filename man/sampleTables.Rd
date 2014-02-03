\name{sampleTables}
\alias{sampleTables}

\title{
Function for constrained table sampling.
}
\description{
~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sampleTables(n0, muTab, bounds, controlCol = NULL, controlRow = NULL, nIter = 100, N = 10000, sdev = 5, verbose = TRUE, objFun = function(tab) {
    -colSums(tab)[1]
}, fixed = c(), fixedRows = NULL, keepArgs = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n0}{
%%     ~~Describe \code{n0} here~~
}
  \item{muTab}{
%%     ~~Describe \code{muTab} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{nIter}{
%%     ~~Describe \code{nIter} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{controlCol}{
%%     ~~Describe \code{controlCol} here~~
}
  \item{controlRow}{
%%     ~~Describe \code{controlRow} here~~
}
  \item{sdev}{
%%     ~~Describe \code{sdev} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{objFun}{
%%     ~~Describe \code{objFun} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{fixedRows}{
%%     ~~Describe \code{fixedRows} here~~
}
  \item{keepArgs}{
%%     ~~Describe \code{keepArgs} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An object of class  \code{\link{conTa}} with the results from the sampling.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Luca Pozzi, Marco Garieri, Natalia Golini

Maintainer: Luca Pozzi 
\email{luca.pozzi@fao.org}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{conTa}}
}
\examples{

data("Italy2006FBsheet")
controlCol <- rbind(lowCol,uppCol)
system.time(tab <- sampleTables(n0,muTab,bounds,controlCol,verbose=FALSE))


}

\keyword{ Sampling }
\keyword{ contingency tables }
