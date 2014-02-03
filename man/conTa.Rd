\name{conTa-class}
%\Rdversion{1.1}
\docType{class}
\alias{conTa-class}
\alias{conTa}

\title{Class "conTa"}
\description{
An CONstrained TAble object
}
\section{Objects from the Class}{
The suggested way to construct objects of class \code{conTa} is to use
the constructor functions \code{sampleTables(n0, muTab, bounds,controlCol)}
}

\section{Slots}{
  \describe{
    \item{\code{bestTab}:}{A \code{"matrix"}  with the table which
        minimizes the objective function.}
      \item{\code{tables}:}{A \code{"list"}  with the sampled tables.}
        \item{\code{iters}:}{The \code{"integer"} number of iterations.}
          \item{\code{objective}:}{The \code{"numeric"} value of the
              objective function.}
      \item{\code{call}:}{The \code{"call"} to the function.}
    \item{\code{args}:}{A \code{"list"} with the arguments the function has been called with.}
  }
}

\section{Methods}{
  \describe{

\item{\code{show}}{\code{signature(object = "conTa")}: the show method.}
  }                                           
}

\author{
Luca Pozzi, Marco Garieri, Natalia Golini

Maintainer: Luca Pozzi 
\email{luca.pozzi@fao.org}
}
\seealso{
  The package vignette COMING SOON.%\code{demoARAMIS}.
}
\examples{
showClass("conTa")
}
\keyword{classes}
