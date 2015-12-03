\name{readOrg}
\alias{readOrg}
\title{
  Read Org Tables
}
\description{
  Read an org table
}
\usage{
readOrg(file, header = TRUE, dec = ".", comment.char = "",
        encoding = "", strip.white = TRUE,
        table.name = NULL, \dots)
}
\arguments{
  \item{file}{
    character
}
  \item{header}{
    logical
}
  \item{dec}{
    character
}
\item{comment.char}{
  character
}
  \item{encoding}{
    the encoding of the file
}
  \item{strip.white}{
    logical
}
  \item{table.name}{
    character: the name of the table to read
}
  \item{\dots}{
    further arguments
}
}
\details{

  When header is \code{TRUE}, readOrg will read the first 5 lines of the
  file and try to find an org table header. Formating instructions such
  as \code{<5>} are discarded. Then the function uses
  \code{\link{read.csv}} to read the remainder of the file.

  When \code{table.name} is specified, the function looks for a line that
  starts with \code{#+NAME: <table.name>} and reads the table that
  follows that line.
  
}
\value{
  A data.frame.
}
\references{
  Org manual \url{http://orgmode.org/manual/index.html}
}
\author{
  Enrico Schumann
}
\seealso{
\code{\link{read.csv}}
}
\examples{
## TODO
}
