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
        encoding = "unknown", strip.white = TRUE, \dots)
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
  \item{\dots}{
    further arguments
}
}
\details{
  When header is TRUE, readOrg will read the first 5 lines of file and
  try to find an org table header. Formating instructions (such as
  \code{<5>}) are discarded.
  
  Then the function uses  \code{\link{read.csv}} to read the remainder
  of the file.
}
\value{
  A data.frame.
}
\references{
  Org manual
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
