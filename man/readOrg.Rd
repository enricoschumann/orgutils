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
    character: the name of the table to read (a regular expression)
}
  \item{\dots}{
    further arguments
}
}
\details{

  When \code{header} is \code{TRUE}, \code{readOrg} will read the first
  10 lines of the file (or the table, when \code{table.name} is
  specified) and try to find an org table header. Formating instructions
  such as \code{<5>} are discarded. Then the function uses
  \code{\link{read.csv}} to read the remainder of the file/table.

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
\dontrun{
## create an org file with a  table and read the table
tmp <-
"#+TITLE: A Table

Next comes a table.

#+name: test_table
| a | b |
|---+---|
| 1 | 2 |
| 3 | 4 |

That was a table.
"

fname <- tempfile("testfile", fileext = ".org")
writeLines(tmp, fname)

require("org")
readOrg(fname, table.name = "test_table")
}
}
