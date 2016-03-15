\name{toOrg}
\alias{toOrg}
\alias{print.org}
\alias{toOrg.data.frame}
\alias{toOrg.Date}
\title{
  Generate Org-mode Code
}
\description{
  Transform R objects into org-mode objects
}
\usage{
toOrg(x, ...)

\method{print}{org}(x, \ldots)

\method{toOrg}{data.frame}(x, row.names = NULL, \ldots)

\method{toOrg}{Date}(x, \ldots)

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
  \item{\dots}{
    other arguments
  }
}
\details{

  Transforms an object \code{x} into text with Org markup. Useful mostly
  when \code{x} is a \link{data.frame}.
  
}
\value{

  A character vector of class \code{org}. To save it to a file, use
  \code{\link{writeLines}}.

}
\references{
  Org mode manual \url{http://orgmode.org/manual/index.html}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{toLatex}}
}
\examples{
toOrg(data.frame(a = 1:5, row.names = LETTERS[1:5]))

toOrg(data.frame(a = 1:5))
toOrg(data.frame(a = 1:5), row.names = TRUE)
toOrg(data.frame(a = 1:5), row.names = "row numbers")

\dontrun{
writeLines(toOrg(data.frame(a = 1:5)), "~/Desktop/my_table.org")
}

## convert Org dates to Date
d <- toOrg(as.Date("2015-01-01"))  ## <2015-01-01 Thu>

## see ?strptime: Each input string is processed as far as
##                necessary for the format specified: any
##                trailing characters are ignored.

as.Date(d, "<\%Y-\%m-\%d")
}
