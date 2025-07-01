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
        strip.sep.lines = TRUE,
        collapse.header = FALSE)
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
  \item{strip.format}{
    logical
  }
  \item{strip.sep.lines}{
    logical
  }
  \item{collapse.header}{
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
  \item{table.missing}{

    what to do if a table specified by
    \code{table.name} is not found.  Default is to
    return \code{NULL}. Set to string \code{"stop"} to
    throw an error.

  }
  \item{\dots}{
    further arguments
  }
}
\details{

Org tables are plain-text tables that look like
\preformatted{%
| Column1 | Column2 |
|---------+---------|
|       1 |       2 |
|       3 |       4 |
}

A line that starts with \sQuote{\code{|}} (after optional
whitespace) is considered a table row.  A line that starts
with \sQuote{\code{|-}} (after optional whitespace) is a
horizontal rule.  Rows before the first horizontal rule are
header lines.

When \code{header} is \code{TRUE}, \code{readOrg} will read
the first \code{max.header} lines of the file (or the table,
when \code{table.name} is specified) and try to find an org
table header. Formating instructions such as \code{<5>} are
discarded. Then the function uses \code{\link{read.csv}} to
read the remainder of the file/table.

When \code{table.name} is specified, the function looks for
a line that starts with \code{#+NAME: <table.name>} and
reads the table that follows that line.

For empty files, \code{readOrg} behaves like
\link[utils]{read.csv}: when completely empty, it fails;
when headers are found, a zero-row \link{data.frame} is
returned.

}
\value{
  A \code{\link{data.frame}}.
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
