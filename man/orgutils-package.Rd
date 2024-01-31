\name{orgutils-package}
\alias{orgutils-package}
\alias{org}
\docType{package}
\title{
  Org Utils
}
\description{
  Helper functions to interact with Org files: read Org tables, convert
  R objects to Org markup.
}
\details{

  Org mode is a major mode for Emacs; see
  \url{https://orgmode.org/manual/Summary.html#Summary} for a
  summary of what it does.

  The \pkg{orgutils} package provides helper functions for
  interacting with Org files (reading Org tables, convert R
  objects to Org markup) without Emacs. Since Org syntax is
  very human-readable, such conversions are useful also, for
  instance, in plain-text emails or reports.

  There are several other packages that help you work with Org
  files as well, such as \pkg{orgR} or \pkg{ascii}.

}
\author{
  Enrico Schumann <es@enricoschumann.net>
}
\references{
  Org mode manual \url{https://orgmode.org/}
}
\keyword{ package }
\seealso{
  \code{\link{toOrg}}, \code{\link{readOrg}}
}
