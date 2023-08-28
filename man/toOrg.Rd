\name{toOrg}
\alias{toOrg}
\alias{print.org}
\alias{toOrg.data.frame}
\alias{toOrg.Date}
\alias{toOrg.POSIXt}
\title{
  Generate Org-mode Markup
}
\description{
  Transform R objects into Org-mode objects.
}
\usage{
toOrg(x, ...)

\method{print}{org}(x, \ldots)

\method{toOrg}{data.frame}(x, row.names = NULL, \ldots)

\method{toOrg}{Date}(x, inactive = FALSE, \ldots)

\method{toOrg}{POSIXt}(x, inactive = FALSE, \ldots)
}
\arguments{
  \item{x}{
    an object
  }
  \item{row.names}{

    \code{NULL}, logical or character. If \code{TRUE},
    \code{\link{row.names}} of \code{x} are added as the first column,
    with column name "\code{row.names}". If a character string, the
    string is used as the column name. See Examples.

    If \code{NULL}, \code{\link{row.names}} are added when they are not
    \code{1, 2, \ldots} (i.e. row numbers).

    If \code{FALSE}, \code{\link{row.names}} are not added.

  }
  \item{inactive}{
    logical: use inactive timestamps? See
    \url{http://orgmode.org/manual/Creating-timestamps.html} .
  }
  \item{\dots}{
    other arguments
  }
}
\details{

  Transforms an object \code{x} into character vectors with Org
  markup. Most useful when \code{x} is a \link{data.frame}.

  \code{toOrg} is meant for snippets of code, not for producing whole
  Org documents.

  When you work with \code{POSIXt}, make sure that a
  potential timezone does not cause trouble: Org does
  not support timezones.

}
\value{

  A character vector, usually with class \code{org}. In some cases,
  class \code{\link{character}} is additionally attached.

  To save it to a file, use \code{\link{writeLines}}.

}
\references{
  Org mode manual \url{http://orgmode.org/manual/index.html}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[utils]{toLatex}}, function \code{as.orgtable} in
  \pkg{microplot}
}
\examples{
toOrg(data.frame(a = 1:3, row.names = LETTERS[1:3]))
## =>  | row.names | a |
##     |-----------+---|
##     | A         | 1 |
##     | B         | 2 |
##     | C         | 3 |

toOrg(data.frame(a = 1:3))
## =>  | a |
##     |---|
##     | 1 |
##     | 2 |
##     | 3 |

toOrg(data.frame(a = 1:3), row.names = TRUE)
## =>  | row.names | a |
##     |-----------+---|
##     | 1         | 1 |
##     | 2         | 2 |
##     | 3         | 3 |


toOrg(data.frame(a = 1:5), row.names = "row numbers")
## =>  | row numbers | a |
##     |-------------+---|
##     | 1           | 1 |
##     | 2           | 2 |
##     | 3           | 3 |
##     | 4           | 4 |
##     | 5           | 5 |

\dontrun{
writeLines(toOrg(data.frame(a = 1:3)), "~/Desktop/my_table.org")}

## Dates/Times
toOrg(as.Date("2015-01-01"))                   ## <2015-01-01 Thu>
toOrg(as.Date("2015-01-01"), inactive = TRUE)  ## [2015-01-01 Thu]
toOrg(Sys.time())                              ## <2017-03-20 Mon 13:23:18>

## Convert Org dates to Date

## see ?strptime: Each input string is processed as far as
##                necessary for the format specified: any
##                trailing characters are ignored.
d <- toOrg(as.Date("2015-01-01"))
as.Date(d, "<\%Y-\%m-\%d")

}
