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
        table.name = NULL, text,
        table.missing = NULL, \dots,
        strip.format = TRUE,
        strip.horiz.rules = TRUE,
        collapse.header = FALSE)
}
\arguments{
  \item{file}{
    character
  }
  \item{header}{%

    logical: If \code{TRUE}, and \code{collapse.header} is
    \code{FALSE}, the first row of the table is used for
    column names (\code{strip.horiz.rules} determines
    whether initial rules are removed first).

  }
  \item{dec}{
    character; see \code{\link{read.table}}
  }
  \item{comment.char}{
    character; see \code{\link{read.table}}
  }
  \item{encoding}{
    string; see \code{\link{read.table}}
  }
  \item{strip.white}{
    logical; see \code{\link{read.table}}
  }
  \item{strip.format}{
    logical: strip rows of format instructions, such as
    \code{<c>}
  }
  \item{strip.horiz.rules}{
    logical: string horizontal rules from table
  }
  \item{collapse.header}{
    logical: if \code{TRUE}, all rows before the first
    horizontal rule are considered table headers (as defined
    in the Org manual)
  }
  \item{stringsAsFactors}{

    logical: note that the default \code{FALSE} differs
    from read.csv

  }
  \item{table.name}{

    character: a regex; the name of the table to read.

  }
  \item{text}{

    character: if \code{file} is not supplied,
    \code{text} is read via \code{\link{textConnection}}

  }
  \item{table.missing}{

    what to do if a table specified by \code{table.name}
    is not found.  Default is to return \code{NULL}. Set
    to string \code{"stop"} to throw an error.

  }
  \item{\dots}{
    further arguments
  }
}
\details{

  Org tables are very human-readable plain-text tables
  that look like
\preformatted{%
| Column1 | Column2 |
|---------+---------|
|       1 |       2 |
|       3 |       4 |
}

  A line that starts with \sQuote{\code{|}} (after optional
  whitespace) is considered a table row; a line that starts
  with \sQuote{\code{|-}} (after optional whitespace) is a
  horizontal rule.  Rows before the first horizontal rule
  are header lines (see the Org manual).

  Depending on the settings of \code{strip.format} and
  \code{strip.horiz.rules}, format instructions such as
  \code{<5>} and are discarded.  Then the function uses
  \code{\link[utils]{read.csv}} to read the remainder of
  the file/table.

  When \code{table.name} is specified, the function looks
  for a line that starts with \code{#+NAME: <table.name>}
  and reads the table that follows that line.

  For empty files, \code{readOrg} behaves like
  \link[utils]{read.csv}: when completely empty, it fails;
  when headers are found, a zero-row \link{data.frame} is
  returned.

}
\value{
  a \code{\link{data.frame}}
}
\references{
  Org manual \url{https://orgmode.org/manual/Tables.html}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[utils]{read.csv}}
}
\examples{
\donttest{
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

library("orgutils")
readOrg(fname, table.name = "test_table")
}
}
