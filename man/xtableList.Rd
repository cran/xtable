\name{xtableList}
\alias{xtableList}
\alias{print.xtableList}

\title{
  Create and Export Lists of Tables
}
\description{
  \code{xtableList} creates an object from a list of tables, which can
  be used by \code{print.xtableList} to produce a composite table
  containing the information from the individual tables.
}
\usage{
xtableList(x, caption = NULL, label = NULL,
           align = NULL, digits = NULL, display = NULL, ...)

\method{print}{xtableList}(x,
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
  hline.after = NULL,
  NA.string = getOption("xtable.NA.string", ""),
  include.rownames = getOption("xtable.include.rownames", TRUE),
  colnames.format = "single",
  only.contents = getOption("xtable.only.contents", FALSE),
  add.to.row = NULL,
  sanitize.text.function = getOption("xtable.sanitize.text.function", NULL),
  sanitize.rownames.function = getOption("xtable.sanitize.rownames.function",
                                         sanitize.text.function),
  sanitize.colnames.function = getOption("xtable.sanitize.colnames.function",
                                         sanitize.text.function),
  sanitize.subheadings.function =
    getOption("xtable.sanitize.subheadings.function",
              sanitize.text.function),
  sanitize.message.function =
    getOption("xtable.sanitize.message.function",
              sanitize.text.function),
  math.style.negative = getOption("xtable.math.style.negative", FALSE),
  math.style.exponents = getOption("xtable.math.style.exponents", FALSE),
  html.table.attributes = getOption("xtable.html.table.attributes", "border=1"),
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
  \item{x}{
    For \code{xtableList}, a list of \R objects all of the same class,
    being a class found among \code{methods(xtable)}. The list may also
    have attributes \code{"subheadings"} and \code{"message"}. The
    attribute \code{"subheadings"} should be a character vector of the
    same length as the list \code{x}. The attribute \code{"message"}
    should be a character vector of any length.
    For \code{print.xtableList}, an object of class \code{xtableList}
    produced by a call to \code{xtableList}.}
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
    \code{strsplit(align, "")[[1]]} before processing. Since the row
    names are printed in the first column, the length of \code{align} is
    one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}. Use \code{"l"}, \code{"r"}, and \code{"c"} to
    denote left, right, and center alignment, respectively.  Use
    \code{"p{3cm}"} etc. for a LaTeX column of the specified width. For
    HTML output the \code{"p"} alignment is interpreted as \code{"l"},
    ignoring the width request. Default depends on the class of
    \code{x}. }
  \item{digits}{
    Either \code{NULL}, or a vector of length one, or a vector of length
    equal to the number of columns in the resulting table, indicating
    the number of digits to display in the corresponding columns, or a
    list if length equal to the number of R objects making up \code{x},
    all members being vectors of the same length, either length one or
    of length equal to the number of columns in the resulting table. See
    details for further information.}
  \item{display}{
    Either \code{NULL}, or a vector of length one, or a vector of length
    equal to the number of columns in the resulting table, indicating
    the format of the corresponding columns, or a
    list if length equal to the number of R objects making up \code{x},
    all members being vectors of the same length, either length one or
    of length equal to the number of columns in the resulting table. See
    details for further information.}
  \item{type}{Type of table to produce. Possible values for \code{type}
    are \code{"latex"} or \code{"html"}.
    Default value is \code{"latex"}.}
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
    between -1 and the number of rows in the resulting table, inclusive,
    indicating the rows after which a horizontal line should
    appear. Determining row numbers is not straightforward since some
    lines in the resulting table don't enter into the count.  The
    default depends on the value of \code{col.names.format}.}
  \item{NA.string}{String to be used for missing values in table
    entries.
    Default value is \code{""}.}
  \item{include.rownames}{If \code{TRUE} the rows names are
    printed.
    Default value is \code{TRUE}.}
  \item{colnames.format}{Either \code{"single"} or \code{"multiple"}.
    Default is \code{"single"}.}
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
  \item{sanitize.text.function}{All non-numeric entries (except row and
    column names) are sanitized in an attempt to remove characters which
    have special meaning for the output format. If
    \code{sanitize.text.function} is not \code{NULL}, it should
    be a function taking a character vector and returning one, and will
    be used for the sanitization instead of the default internal
    function.
    Default value is \code{NULL}.}
  \item{sanitize.rownames.function}{Like the
    \code{sanitize.text.function}, but applicable to row names.
    The default uses the \code{sanitize.text.function}. }
  \item{sanitize.colnames.function}{Like the
    \code{sanitize.text.function}, but applicable to column names.
    The default uses the \code{sanitize.text.function}. }
  \item{sanitize.subheadings.function}{Like the
    \code{sanitize.text.function}, but applicable to subheadings.
    The default uses the \code{sanitize.text.function}. }
  \item{sanitize.message.function}{Like the
    \code{sanitize.text.function}, but applicable to the message.
    The default uses the \code{sanitize.text.function}. }
  \item{math.style.negative}{In a LaTeX table, if \code{TRUE}, then use
    $-$ for the negative sign (as was the behavior prior to version 1.5-3).
    Default value is \code{FALSE}.}
  \item{math.style.exponents}{In a LaTeX table, if \code{TRUE} or
    \code{"$$"}, then use \verb{$5 \times 10^{5}$} for 5e5. If
    \code{"ensuremath"}, then use \verb{\\ensuremath{5 \times 10^{5}}}
    for 5e5. If \code{"UTF-8"} or \code{"UTF-8"}, then use UTF-8 to
    approximate the LaTeX typsetting for 5e5.
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
    included in this list. See details.
    Default value is \code{NULL}.}
  \item{rotate.rownames}{If \code{TRUE}, the row names are displayed
    vertically in LaTeX.
    Default value is \code{FALSE}.}
  \item{rotate.colnames}{If \code{TRUE}, the column names are displayed
    vertically in LaTeX.
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
  \item{\dots}{Additional arguments.  (Currently ignored.)}
}
\details{
  \code{xtableList} produces an object suitable for printing using
  \code{print.xtableList}.

  The elements of the list \code{x} supplied to \code{xtableList} must
  all have the same structure. When these list items are submitted to
  \code{xtable} the resulting table must have the same number of columns
  with the same column names and type of data.

  The values supplied to arguments \code{digits} and \code{display},
  must be composed of elements as specified in those same arguments for
  the function \code{\link{xtable}}. See the help for
  \code{\link{xtable}} for details.

  \code{print.xtableList} produces tables in two different formats
  depending on the value of \code{col.names.format}. If
  \code{col.names.format = "single"}, the resulting table has only a
  single heading row. If \code{col.names.format = "multiple"} there is a
  heading row for each of the subtables making up the complete table.

  By default if \code{col.names.format = "single"}, there are horizontal
  lines above and below the heading row, and at the end of each
  subtable. If \code{col.names.format = "multiple"}, there are
  horizontal lines above and below each appearance of the heading row,
  and at the end of each subtable.

  If \code{"subheadings"} is not \code{NULL}, the individual elements of
  this vector (which can include newlines \verb{\\n}) produce a heading
  line or lines for the subtables. When \code{col.names.format =
    "multiple"} these subheadings appear \emph{above} the heading rows.

  If \code{"message"} is not \code{NULL} the vector produces a line or
  lines at the end of the table.

  Consult the vignette \sQuote{The \code{xtableList} Gallery} to see
  the behaviour of these functions.

  Note that at present there is no code for \code{type = "html"}.
}
\value{
  \code{xtableList} produces an object of class
  \code{"xtableList"}. An object of this class is a list of
  \code{"xtable"} objects with some additional attributes. Each element
  of the list can have a \code{"subheading"} attribute. The list can
  also have a \code{"message"} attribute.

  \code{print.xtableList} produces a character string containing LaTeX
  markup which produces a composite table in a LaTeX document.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}.
}
\seealso{
  \code{\link{xtable}}, \code{\link{print.xtable}},
  \code{\link{formatC}}

  \code{\link{caption}}, \code{\link{label}}, \code{\link{align}},
  \code{\link{digits}}, \code{\link{display}}
}
\examples{
data(mtcars)
mtcars <- mtcars[, 1:6]
mtcarsList <- split(mtcars, f = mtcars$cyl)
attr(mtcarsList, "subheadings") <- paste0("Number of cylinders = ",
                                          names(mtcarsList))
attr(mtcarsList, "message") <- c("Line 1 of Message",
                                 "Line 2 of Message")
xList <- xtableList(mtcarsList)
print.xtableList(xList)
print.xtableList(xList, colnames.format = "multiple")
}
\keyword{ print }
