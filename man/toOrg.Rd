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

\method{toOrg}{data.frame}(x, \ldots)

\method{toOrg}{Date}(x, \ldots)

}
\arguments{
  \item{x}{
    an object
  }
  \item{\dots}{
    other arguments
  }
}
\details{

  Transforms an object x into text with Org markup. Useful mostly for
  data.frames.
  
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
toOrg(data.frame(a = 1:5))

\dontrun{
writeLines(toOrg(data.frame(a = 1:5)), "~/Desktop/my_table.org")}
}
