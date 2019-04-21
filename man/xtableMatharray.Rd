\name{xtableMatharray}
\alias{xtableMatharray}

\title{
 Create LaTeX Mathematical Array
}
\description{
  Convert an array of numbers or mathematical expressions into an
 \code{xtableMatharray} object so it can be printed. A convenience
 function to enable the printing of arrays in mathematical expressions
 in LaTeX.
}
\usage{
xtableMatharray(x, caption = NULL, label = NULL, align = NULL,
                digits = NULL, display = NULL, auto = FALSE, ...)
}
\arguments{
  \item{x}{A numeric or character matrix.}
  \item{caption}{Character vector of length 1 or 2 containing the
    table's caption or title.  If length is 2, the second item is the
    "short caption" used when LaTeX generates a "List of Tables". Set to
    \code{NULL} to suppress the caption.  Default value is \code{NULL}.
    Included here only for consistency with \code{xtable} methods. Not
    expected to be of use.}
  \item{label}{Character vector of length 1 containing the LaTeX
    label. Set to \code{NULL} to suppress the label.
    Default value is \code{NULL}. }
  \item{align}{Character vector of length equal to the number of columns
    of the resulting table, indicating the alignment of the corresponding
    columns.  Also, \code{"|"} may be used to produce vertical lines
    between columns in LaTeX tables, but these are effectively ignored
    when considering the required length of the supplied vector.  If a
    character vector of length one is supplied, it is split as
    \code{strsplit(align, "")[[1]]} before processing. Since the row
    names are printed in the first column, the length of \code{align} is
    one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}. Use \code{"l"}, \code{"r"}, and \code{"c"} to
    denote left, right, and center alignment, respectively.  Use
    \code{"p{3cm}"} etc. for a LaTeX column of the specified width. For
    HTML output the \code{"p"} alignment is interpreted as \code{"l"},
    ignoring the width request. Default depends on the class of
    \code{x}. }
  \item{digits}{Numeric vector of length equal to one (in which case it
    will be replicated as necessary) or to the number of columns of the
    resulting table \bold{or} matrix of the same size as the resulting
    table, indicating the number of digits to display in the
    corresponding columns. Since the row names are printed in the first
    column, the length of the vector \code{digits} or the number of
    columns of the matrix \code{digits} is one greater than
    \code{ncol(x)} if \code{x} is a \code{data.frame}. Default depends
    on the class of \code{x}. If values of \code{digits} are negative,
    the corresponding values of \code{x} are displayed in scientific
    format with \code{abs(digits)} digits.}
  \item{display}{
    Character vector of length equal to the number of columns of the
    resulting table, indicating the format for the corresponding columns.
    Since the row names are printed in the first column, the length of
    \code{display} is one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}.  These values are passed to the \code{formatC}
    function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
    \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format; \code{"e"} and \code{"E"} give
    \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format); \code{"g"}
    and \code{"G"} put \code{x[i]} into scientific format only if it
    saves space to do so.  \code{"fg"} uses fixed format as \code{"f"},
    but \code{digits} as number of \emph{significant} digits.  Note that
    this can lead to quite long result strings.  Default depends on the
    class of \code{x}.}
  \item{auto}{
    Logical, indicating whether to apply automatic format when no value
    is passed to \code{align}, \code{digits}, or \code{display}. This
    \sQuote{autoformat} (based on \code{xalign}, \code{xdigits}, and
    \code{xdisplay}) can be useful to quickly format a typical
    \code{matrix} or \code{data.frame}. Default value is \code{FALSE}.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  This function is only usable for production of LaTeX documents, not
  HTML.

  Creates an object of class
 \code{c("xtableMatharray","xtable","data.frame")}, to ensure that it is
 printed by the print method \code{print.xtableMatharray}.
}
\value{
 An object of class \code{c("xtableMatharray","xtable","data.frame")}.
}

\author{
  David Scott <d.scott@auckland.ac.nz>
}
\seealso{
\code{\link{print.xtableMatharray}}
}
\examples{
V <- matrix(c(1.140380e-03,  3.010497e-05,  7.334879e-05,
              3.010497e-05,  3.320683e-04, -5.284854e-05,
              7.334879e-05, -5.284854e-05,  3.520928e-04), nrow = 3)
mth <- xtableMatharray(V)
class(mth)
str(mth)
unclass(mth)
}
\keyword{ print }
