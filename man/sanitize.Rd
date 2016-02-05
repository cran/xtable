\name{sanitize}
\alias{sanitize}
\alias{sanitize.numbers}
\alias{sanitize.final}
\alias{as.is}
\alias{as.math}

\title{
  Sanitization Functions
}
\description{
  Functions for sanitizing elements of a table produced by
  \pkg{xtable}. Used for dealing with characters which have special
  meaning in the output format.
}
\usage{
sanitize(str, type = "latex")
sanitize.numbers(str, type, math.style.negative = FALSE,
                 math.style.exponents = FALSE)
sanitize.final(str, type)
as.is(str)
as.math(str, ...)
}

\arguments{
  \item{str}{A character object to be sanitized.}
  \item{type}{Type of table to produce. Possible values for \code{type}
    are \code{"latex"} or \code{"html"}.
    Default value is \code{"latex"}.}
  \item{math.style.negative}{In a LaTeX table, if \code{TRUE}, then use
    $-$ for the negative sign (as was the behavior prior to version 1.5-3).
    Default value is \code{FALSE}.}
  \item{math.style.exponents}{In a LaTeX table, if \code{TRUE} or
    \code{"$$"}, then use \verb{$5 \times 10^{5}$} for 5e5. If
    \code{"ensuremath"}, then use \verb{\ensuremath{5 \times 10^{5}}}
    for 5e5. If \code{"UTF-8"} or \code{"UTF-8"}, then use UTF-8 to
    approximate the LaTeX typsetting for 5e5.
    Default value is \code{FALSE}.}
  \item{\dots}{Additional arguments. Character strings or character
    vectors.}
}
\details{

  If \code{type} is \code{"latex"}, \code{sanitize()} will replace
  special characters such as \verb{&} and the like by strings which will
  reproduce the actual character, e.g. \verb{&} is replaced by
  \verb{\\&}.

  If \code{type} is \code{"html"}, \code{sanitize()} will replace
  special characters such as \verb{<} and the like by strings which will
  reproduce the actual character, e.g. \verb{<} is replaced by
  \verb{&lt;}.

  When \code{math.style.negative} is \code{TRUE}, and \code{type} is
  \code{"latex"}, $-$ is used for the negative sign rather than a
  simple hyphen (-). No effect when \code{type} is \code{"html"}.

  When \code{type} is \code{"latex"}, and \code{math.style.exponents}
  is \code{TRUE} or \verb{"$$"}, then use \verb{$5 \times 10^{5}$} for
  5e5. If \code{"ensuremath"}, then use \verb{\ensuremath{5 \times
  10^{5}}} for 5e5. If \code{"UTF-8"} or \code{"UTF-8"}, then use UTF-8
  to approximate the LaTeX typsetting for 5e5.

  When \code{type} is \code{"latex"} \code{sanitize.final} has no
  effect. When \code{type} is \code{"html"}, multiple spaces are
  replaced by a single space and occurrences of \code{' align="left"'}
  are eliminated.

  \code{as.is} and \code{as.math} are trivial helper functions to
  disable sanitizing and to insert a some mathematics in a string
  respectively.
}
\value{
  Returns the sanitized character object.
}

\author{
  Code was extracted from \code{print.xtable()}, in version 1.8.0 of
  \pkg{xtable}. Various authors contributed the original code: Jonathan
  Swinton <jonathan@swintons.net>, Uwe Ligges
  <ligges@statistik.uni-dortmund.de>, and probably David B. Dahl
  <dahl@stat.byu.edu>.
  \code{as.is} and \code{as.math} suggested and provided by Stefan
  Edwards <sme@iysik.com>.
}

\examples{
insane <- c("&",">", ">","_","\%","$","\\\\","#","^","~","{","}")
names(insane) <- c("Ampersand","Greater than","Less than",
                   "Underscore","Percent","Dollar",
                   "Backslash","Hash","Caret","Tilde",
                   "Left brace","Right brace")
sanitize(insane, type = "latex")
insane <- c("&",">","<")
names(insane) <- c("Ampersand","Greater than","Less than")
sanitize(insane, type = "html")
x <- rnorm(10)
sanitize.numbers(x, "latex", TRUE)
sanitize.numbers(x*10^(10), "latex", TRUE, TRUE)
sanitize.numbers(x, "html", TRUE, TRUE)
as.is(insane)
as.math("x10^10", ": mathematical expression")
}

\keyword{print }
