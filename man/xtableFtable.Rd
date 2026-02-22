\name{xtableFtable}
\alias{xtableFtable}
\alias{print.xtableFtable}

\title{
  Create and Export Flat Tables
}
\description{
  \code{xtableFtable} creates an object which contains information about
  a flat table which can be used by \code{print.xtableFtable} to produce
  a character string which when included in a document produces a nicely
  formatted flat table.
}
\usage{
xtableFtable(x, caption = NULL, label = NULL,
             align = NULL, digits = 0, display = NULL,
             quote = FALSE,
             method = c("non.compact", "row.compact",
                         "col.compact", "compact"),
             lsep = " $\\\\vert$ ", ...)

\method{print}{xtableFtable}(x,
  type = getOption("xtable.type", "latex"),
  file = getOption("xtable.file", ""),
  append = getOption("xtable.append", FALSE),
  floating = getOption("xtable.floating", TRUE),
  floating.environment = getOption("xtable.floating.environment", "table"),
  table.placement = getOption("xtable.table.placement", "ht"),
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),
  latex.environments = getOption("xtable.latex.environments", c("center")),
  tabular.environment = getOption("xtable.tabular.environment", "tabular"),
  size = getOption("xtable.size", NULL),
  hline.after = getOption("xtable.hline.after", NULL),
  NA.string = getOption("xtable.NA.string", ""),
  only.contents = getOption("xtable.only.contents", FALSE),
  add.to.row = getOption("xtable.add.to.row", NULL),
  sanitize.text.function = getOption("xtable.sanitize.text.function", as.is),
  sanitize.rownames.function = getOption("xtable.sanitize.rownames.function",
                                         sanitize.text.function),
  sanitize.colnames.function = getOption("xtable.sanitize.colnames.function",
                                         sanitize.text.function),
  math.style.negative = getOption("xtable.math.style.negative", FALSE),
  math.style.exponents = getOption("xtable.math.style.exponents", FALSE),
  html.table.attributes = getOption("xtable.html.table.attributes",
                                    "border=1"),
  print.results = getOption("xtable.print.results", TRUE),
  format.args = getOption("xtable.format.args", NULL),
  rotate.rownames = getOption("xtable.rotate.rownames", FALSE),
  rotate.colnames = getOption("xtable.rotate.colnames", FALSE),
  booktabs = getOption("xtable.booktabs", FALSE),
  scalebox = getOption("xtable.scalebox", NULL),
  width = getOption("xtable.width", NULL),
  comment = getOption("xtable.comment", TRUE),
  timestamp = getOption("xtable.timestamp", date()),
  ...)
}

\arguments{
  \item{x}{For \code{xtableFtable}, an object of class
  \code{"ftable"}. For \code{print.xtableFtable}, an object of class
  \code{c("xtableFtable", "ftable")}.}
  \item{caption}{Character vector of length 1 or 2 containing the
    table's caption or title.  If length is 2, the second item is the
    "short caption" used when LaTeX generates a "List of Tables". Set to
    \code{NULL} to suppress the caption.  Default value is \code{NULL}. }
  \item{label}{Character vector of length 1 containing the LaTeX label
    or HTML anchor. Set to \code{NULL} to suppress the label.  Default
    value is \code{NULL}. }
  \item{align}{Character vector of length equal to the number of columns
    of the resulting table, indicating the alignment of the corresponding
    columns.  Also, \code{"|"} may be used to produce vertical lines
    between columns in LaTeX tables, but these are effectively ignored
    when considering the required length of the supplied vector.  If a
    character vector of length one is supplied, it is split as
    \code{strsplit(align, "")[[1]]} before processing. For a flat table,
    the number of columns is the number of columns of data, plus the
    number of row variables in the table, plus one for the row names,
    even though row names are not printed.
    Use \code{"l"}, \code{"r"}, and \code{"c"} to
    denote left, right, and center alignment, respectively.  Use
    \code{"p{3cm}"} etc. for a LaTeX column of the specified width. For
    HTML output the \code{"p"} alignment is interpreted as \code{"l"},
    ignoring the width request.
    If \code{NULL} all row variable labels will be left aligned,
    separated from the data columns by a vertical line, and all data
    columns will be right aligned. The actual length of \code{align}
    depends on the value of \code{method}.}
  \item{digits}{
    Numeric vector of length equal to one (in which case it will be
    replicated as necessary) or to the number of columns in the
    resulting table. Since data in the table consists of
    counts, the default is 0. If the value of \code{digits} is negative, the
    corresponding columns are displayed in scientific format
    with \code{abs(digits)} digits.}
  \item{display}{
    Character vector of length equal to the number of columns of the
    resulting table, indicating the format for the corresponding columns.
    These values are passed to the \code{formatC}
    function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
    \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format; \code{"e"} and \code{"E"} give
    \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format); \code{"g"}
    and \code{"G"} put \code{x[i]} into scientific format only if it
    saves space to do so.  \code{"fg"} uses fixed format as \code{"f"},
    but \code{digits} as number of \emph{significant} digits.  Note that
    this can lead to quite long result strings.
    If \code{NULL} all row variable names and labels will have format
    \code{"s"}, and all data columns will have format \code{"d"}. The
    actual length of \code{display} depends on the value of
    \code{method}.}
  \item{quote}{A character string giving the set of quoting characters
    for \code{format.ftable} used in \code{print.xtableFtable}. To
    disable quoting altogether, use \code{quote=""}.}
  \item{method}{String specifying how the \code{"xtableFtable"} object is
    printed in the \code{print} method.  Can be abbreviated.  Available
    methods are (see the examples in \code{\link{print.ftable}}):
    \describe{
      \item{"non.compact"}{the default representation of an
        \code{"ftable"} object.}
      \item{"row.compact"}{a row-compact version without empty cells
        below the column labels.}
      \item{"col.compact"}{a column-compact version without empty cells
        to the right of the row labels.}
      \item{"compact"}{a row- and column-compact version.  This may imply
        a row and a column label sharing the same cell.  They are then
        separated by the string \code{lsep}.}
    }
  }
  \item{lsep}{Only for \code{method = "compact"}, the separation string
    for row and column labels.}
  \item{type}{Type of table to produce. Possible values for \code{type}
    are \code{"latex"} or \code{"html"}.
    Default value is \code{"latex"} and is the only type implemented so far.}
  \item{file}{Name of file where the resulting code should be saved.  If
    \code{file=""}, output is displayed on screen.  Note that the
    function also (invisibly) returns a character vector of the results
    (which can be helpful for post-processing).
    Default value is \code{""}.}
  \item{append}{If \code{TRUE} and \code{file!=""}, code will be
    appended to \code{file} instead of overwriting \code{file}.
    Default value is \code{FALSE}.}
  \item{floating}{If \code{TRUE} and \code{type="latex"}, the resulting
    table will be a floating table (using, for example,
    \code{\\begin\{table\}} and \code{\\end\{table\}}).  See
    \code{floating.environment} below.
    Default value is \code{TRUE}. }
  \item{floating.environment}{If \code{floating=TRUE} and
    \code{type="latex"}, the resulting table uses the specified floating
    environment. Possible values include \code{"table"}, \code{"table*"},
    and other floating environments defined in LaTeX packages.
    Default value is \code{"table"}.}
  \item{table.placement}{If \code{floating=TRUE} and
    \code{type="latex"}, the floating table will have placement given by
    \code{table.placement} where \code{table.placement} must be
    \code{NULL} or contain only elements of
    \{"h","t","b","p","!","H"\}.
    Default value is \code{"ht"}.}
  \item{caption.placement}{The caption will be placed at the bottom
    of the table if \code{caption.placement} is \code{"bottom"} and at
    the top of the table if it equals \code{"top"}.
    Default value is \code{"bottom"}.}
  \item{caption.width}{The caption will be placed in a \code{"parbox"}
    of the specified width if \code{caption.width} is not \code{NULL} and
	\code{type="latex"}. Default value is \code{NULL}.}
  \item{latex.environments}{If \code{floating=TRUE} and
    \code{type="latex"}, the specified LaTeX environments (provided as
    a character vector) will enclose the tabular environment.
    Default value is \code{"center"}. }
  \item{tabular.environment}{When \code{type="latex"}, the tabular
    environment that will be used.
    When working with tables that extend more than one page, using
    \code{tabular.environment="longtable"} with the corresponding
    LaTeX package (see Fairbairns, 2005) allows one to typeset them
    uniformly. Note that \code{floating} should be set to
    \code{FALSE} when using the \code{longtable} environment.
    Default value is \code{"tabular"}.}
  \item{size}{A character vector that is inserted just before the
    tabular environment starts. This can be used to set the font size
    and a variety of other table settings. Initial backslashes are
    automatically prefixed, if not supplied by user.
    Default value is \code{NULL}. }
  \item{hline.after}{When \code{type="latex"}, a vector of numbers
    between -1 and \code{nrow(x)}, inclusive, indicating the rows after
    which a horizontal line should appear.  Repeated values are
    allowed. If \code{NULL} the default is to draw a line before before
    starting the table, after the column variable names and labels, and
    at the end of the table.}
  \item{NA.string}{String to be used for missing values in table
    entries.
    Default value is \code{""}.}
 \item{only.contents}{If \code{TRUE} only the rows of the
    table are printed.
    Default value is \code{FALSE}. }
  \item{add.to.row}{A list of two components. The first component (which
    should be called 'pos') is a list that contains the position of rows on
    which extra commands should be added at the end. The second
    component (which should be called 'command') is a character vector
    of the same length as the first component, which contains the command
    that should be added at the end of the specified rows.
    Default value is \code{NULL}, i.e. do not add commands.}
  \item{sanitize.text.function}{Since the table entries are counts no
    sanitization is necessary. The default is \code{as.is}, which is the
    function which makes no changes. This also applies to the labels for
    the row and column variables since these are also part of the table
    which is printed using a call to \code{print.xtable}.}
  \item{sanitize.rownames.function}{Like the
    \code{sanitize.text.function}, but applicable to row names.
    The default uses the \code{sanitize.text.function}. }
  \item{sanitize.colnames.function}{Like the
    \code{sanitize.text.function}, but applicable to column names.
    The default uses the \code{sanitize.text.function}. }
  \item{math.style.negative}{In a LaTeX table, if \code{TRUE}, then use
    $-$ for the negative sign (as was the behavior prior to version 1.5-3).
    Default value is \code{FALSE}.}
  \item{math.style.exponents}{In a LaTeX table, if \code{TRUE} or
    \code{"$$"}, then use \verb{$5 \times 10^{5}$} for 5e5. If
    \code{"ensuremath"}, then use \verb{\\ensuremath{5 \times 10^{5}}}
    for 5e5. If \code{"UTF-8"} or \code{"UTF-8"}, then use UTF-8 to
    approximate the LaTeX typesetting for 5e5.
    Default value is \code{FALSE}.}
  \item{html.table.attributes}{In an HTML table, attributes associated
    with the \code{<TABLE>} tag.
    Default value is \code{"border=1"}.}
  \item{print.results}{If \code{TRUE}, the generated table is printed to
    standard output.  Set this to \code{FALSE} if you will just be using
    the character vector that is returned invisibly.
  Default value is \code{TRUE}.}
  \item{format.args}{List of arguments for the \code{formatC} function.
    For example, standard German number separators can be specified as
    \code{format.args=list(big.mark = "'", decimal.mark =
      ","))}. The arguments \code{digits} and \code{format} should not be
    included in this list.
    Default value is \code{NULL}.}
  \item{rotate.rownames}{If \code{TRUE}, the row names and labels, and
    column variable names are displayed vertically in LaTeX.
    Default value is \code{FALSE}.}
  \item{rotate.colnames}{If \code{TRUE}, the column names and labels,
    and row variable names are displayed vertically in LaTeX.
    Default value is \code{FALSE}.}
  \item{booktabs}{If \code{TRUE}, the \code{toprule}, \code{midrule} and
    \code{bottomrule} commands from the LaTeX "booktabs" package are used
    rather than \code{hline} for the horizontal line tags. }
  \item{scalebox}{If not \code{NULL}, a \code{scalebox} clause will be
    added around the tabular environment with the specified value used
    as the scaling factor.
    Default value is \code{NULL}.}
  \item{width}{If not \code{NULL}, the specified value is included in
    parentheses between the tabular environment \code{begin} tag and the
    alignment specification.  This allows specification of the table
    width when using tabular environments such as \code{tabular*} and
    \code{tabularx}.  Note that table width specification is not
    supported with the \code{tabular} or \code{longtable} environments.
    Default value is \code{NULL}.}
  \item{comment}{If \code{TRUE}, the version and timestamp comment is
    included.  Default value is \code{TRUE}. }
  \item{timestamp}{Timestamp to include in LaTeX comment.  Set this
    to \code{NULL} to exclude the timestamp. Default value is
    \code{date()}. }
  \item{...}{Additional arguments.  (Currently ignored.) }
}
\details{
  \code{xtableFtable} carries out some calculations to determine the
    number of rows and columns of names and labels which will be in the
    table when formatted as a flat table, which depends on the value of
    \code{method}. It uses the results of those calculations to set
    sensible values for \code{align} and \code{display} if these have
    not been supplied. It attaches attributes to the resulting object
    which specify details of the function call which are needed when
    printing the resulting object which is of class
    \code{c("xtableFtable", "ftable")}.

    \code{print.xtableFtable} uses the attributes attached to an object
    of class \code{c("xtableFtable", "ftable")} to create a suitable
    character matrix object for subsequent printing. Formatting is
    carried out by changing the class of the \code{c("xtableFtable",
    "ftable")} to \code{"ftable"} then using the generic \code{format}
    to invoke \code{format.ftable}, from the \pkg{stats} package. The
    matrix object produced is then printed via a call to
    \code{print.xtable}.

    Note that at present there is no code for \code{type = "html"}.
}
\value{
  For \code{xtableFtable} an object of class \code{c("xtableFtable",
    "ftable")}, with attributes
  \item{ftableCaption}{the value of the \code{caption} argument}
  \item{ftableLabel}{the value of the \code{label} argument}
  \item{ftableAlign}{the value of the \code{label} argument}
  \item{ftableDigits}{the value of the \code{digits} argument or the
    default value if \code{digits = NULL}}
  \item{quote}{the value of the \code{quote} argument}
  \item{ftableDisplay}{the value of the \code{display} argument or the
    default value if \code{align = NULL}}
  \item{method}{the value of the \code{method} argument}
  \item{lsep}{the value of the \code{lsep} argument}
  \item{nChars}{a vector of length 2 giving the number of character rows
    and the number of character columns}

  For \code{print.xtableFtable} a character string which will produce a
  formatted table when included in a LaTeX document.

}
\references{
  The TeX FAQ, \emph{Tables longer than a single page.}
  \url{https://texfaq.org/FAQ-longtab}
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}.
}
\note{
  The functions \code{xtableFtable} and \code{print.xtableFtable} are
  new and their behaviour may change in the future based on user
  experience and recommendations.

  It is not recommended that users change the values of \code{align},
  \code{digits} or \code{align}. First of all, alternative values have
  not been tested. Secondly, it is most likely that to determine
  appropriate values for these arguments, users will have to investigate
  the code for \code{xtableFtable} and/or \code{print.xtableFtable}.
}

\seealso{
  \code{\link{ftable}}, \code{\link{print.ftable}}

  \code{\link{xtable}}, \code{\link{caption}}, \code{\link{label}},
  \code{\link{align}}, \code{\link{digits}}, \code{\link{display}},
  \code{\link{formatC}}
}
\examples{
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl, levels = c("4","6","8"),
                     labels = c("four","six","eight"))
tbl <- ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear,
              row.vars = c(2, 4),
              dnn = c("Cylinders", "V/S", "Transmission", "Gears"))
xftbl <- xtableFtable(tbl, method = "compact")
print.xtableFtable(xftbl, booktabs = TRUE)
xftbl <- xtableFtable(tbl, method = "row.compact")
print.xtableFtable(xftbl, rotate.colnames = TRUE,
                   rotate.rownames = TRUE)
}

\keyword{ category }
\keyword{ print }
