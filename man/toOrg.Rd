\name{toOrg}
\alias{toOrg}
\alias{print.org}
\title{
  Generate Org-mode Code
}
\description{
  Transform R objects into org-mode objects
}
\usage{
toOrg(x, ...)

\method{print}{org}(\dots, recursive = FALSE)

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
  Transforms an object x into text.
  
}
\value{
  A character vector of class \code{org}.
}
\references{
  Org mode manual
}
\author{
  Enrico Schumann
}
%% \note{
%% %%  ~~further notes~~
%% }
%% %% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  toLatex
}
\examples{
toOrg(data.frame(a = 1:5))
}
