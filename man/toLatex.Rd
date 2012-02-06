\name{toLatex.xtable}
\alias{toLatex.xtable}
\title{Convert Table to Latex}
\description{Function creating a LaTeX representation of an object of class \code{xtable}.}
\usage{
	\method{toLatex}{xtable}(object, ...)}
\arguments{
  \item{object}{An object of class \code{"xtable"}.}
  \item{...}{Other arguments to \code{print.xtable}.}  
}
\details{
  This function creates a LaTeX representation of an object of class \code{"xtable"}.  This is a method for the generic \code{"toLatex"} in the core R package \code{"utils"}. 
}
\author{Charles Roosen \email{roosen@gmail.com} with contributions and suggestions from many others (see source code).}
\seealso{\code{\link{print.xtable}}}

\keyword{toLatex}
