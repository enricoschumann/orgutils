\name{readOrg}
\alias{readOrg}
\title{
  Read Org Tables
}
\description{
  Read an Org table from a file.
}
\usage{
readOrg(file, header = TRUE, dec = ".", comment.char = "",
        encoding = "", strip.white = TRUE,
        stringsAsFactors = FALSE,
        table.name = NULL, text, \dots)
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
  \item{stringsAsFactors}{

    logical: note that the default \code{FALSE} differs
    from read.csv

  }
  \item{table.name}{

    character: the name of the table to read (a regular
    expression)

  }
  \item{text}{

    character: if \code{file} is not supplied,
    \code{text} is read via
    \code{\link{textConnection}}

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

  For empty files, \code{readOrg} behaves like \link[utils]{read.csv}:
  when completely empty, it fails; when headers are found, a zero-row
  \link{data.frame} is returned.
  
}
\value{
  A \code{\link{data.frame}}.
}
\references{
  Org manual \url{http://orgmode.org/manual/index.html}
}
\author{
  Enrico Schumann
}
\seealso{
\code{\link[utils]{read.csv}}
}
\examples{
\dontrun{
## create an Org file with a table and read the table
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
