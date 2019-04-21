\name{print.xtableMatharray}
\alias{print.xtableMatharray}
\title{Print Math Array}
\description{
  For an object of class \code{"xtableMatharray"}, returns the LaTeX
  commands to produce an array.
}
\usage{
\method{print}{xtableMatharray}(x,
  print.results = TRUE,
  format.args = getOption("xtable.format.args", NULL),
  scalebox = getOption("xtable.scalebox", NULL),
  comment = FALSE,
  timestamp = NULL,
  ...)
}
\arguments{
  \item{x}{An object of class \code{"xtableMatharray"}.}
  \item{print.results}{If \code{TRUE}, the generated table is printed to
    standard output.  Set this to \code{FALSE} if you will just be using
    the character vector that is returned invisibly.
    Default value is \code{TRUE}.}
  \item{format.args}{List of arguments for the \code{formatC} function.
    For example, standard German number separators can be specified as
    \code{format.args=list(big.mark = "'", decimal.mark = ",")}. The
    arguments \code{digits} and \code{format} should not be included in
    this list. See details for function \code{\link{print.xtable}}.
    Default value is \code{NULL}.}
  \item{scalebox}{If not \code{NULL}, a \code{scalebox} clause will be
    added around the tabular environment with the specified value used
    as the scaling factor.
    Default value is \code{NULL}.}
  \item{comment}{If \code{TRUE}, the version and timestamp comment is
    included.  Default value is \code{FALSE}. }
  \item{timestamp}{Timestamp to include in LaTeX comment.  Set this
    to \code{NULL} to exclude the timestamp. Default value is \code{NULL}. }
  \item{...}{Additional arguments.  (Currently ignored.) }
}
\details{
 This command prints an array of numbers which may be included in a
 mathematical expression in a LaTeX document created using \pkg{Sweave}
 or \pkg{knitr}. Internally it calls \code{print.data.frame} but with
 special values for the arguments, namely that the tabular environment
 is \code{array}, row names and column names are not included, and there
 are no horizontal lines. Note that the default values for the arguments
 \code{comment} and \code{timestamp} are different to the default values
 for \code{print.xtable}, the justification being that comments would
 make the resulting LaTeX harder to read.
}
\value{
  A character vector containing the LaTeX code for incorporating an
  array in a mathematical expression.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}.
}
\seealso{
  \code{\link{xtableMatharray}}, \code{\link{print.xtable}}
}

\examples{
V <- matrix(c(1.140380e-03,  3.010497e-05,  7.334879e-05,
              3.010497e-05,  3.320683e-04, -5.284854e-05,
              7.334879e-05, -5.284854e-05,  3.520928e-04), nrow = 3)
### Simple test of print.xtableMatharray
print.xtableMatharray(xtable(V, display = rep("E", 4)))

class(V) <- c("xtableMatharray")
class(V)

### Test without any additional arguments
mth <- xtableMatharray(V)
str(mth)
print(mth)

### Test with arguments to xtable
mth <- xtableMatharray(V, display = rep("E", 4))
str(mth)
print(mth)

mth <- xtableMatharray(V, digits = 6)
str(mth)
print(mth)

### Test with additional print.xtableMatharray arguments
mth <- xtableMatharray(V, digits = 6)
str(mth)
print(mth, format.args = list(decimal.mark = ","))
print(mth, scalebox = 0.5)
print(mth, comment = TRUE)
print(mth, timestamp = "2000-01-01")
print(mth, comment = TRUE, timestamp = "2000-01-01")
}

\keyword{print}
