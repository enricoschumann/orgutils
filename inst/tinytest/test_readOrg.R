## ERROR
## empty file: readOrg stops, like 'read.table'
## read.table("orgtable0.org")
fn <- "orgtable0.org"
expect_error(readOrg(fn))


fn <- "orgtable00.org"
expect_error(readOrg(fn))
expect_true(is.null(readOrg(fn, table.name = "A")))
expect_error(readOrg(fn, table.name = "A", table.missing = "stop"))


fn <- "orgtable1.org"
expect_equal(readOrg(fn),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- "orgtable2.org"
expect_equal(readOrg(fn),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- "orgtable3.org"
expect_equal(readOrg(fn, table.name = "table3"),
            structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                           col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))



fn <- "orgtable4.org"
expect_equal(readOrg(fn, table.name = "table4"),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- "orgtable5.org"
expect_equal(readOrg(fn, table.name = "table5"),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- "orgtable6.org"
expect_equal(readOrg(fn, table.name = "table6"),
             structure(list(col1 = c(1L, 4L, 7L),
                            col2 = c(2L, 5L, 8L),
                            col3 = c(3L, 6L, 9L)),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -3L)))



## an empty org table
fn <- "orgtable7.org"
expect_equal(readOrg(fn),
             structure(list(col1 = character(0),
                            col2 = character(0),
                            col3 = character(0)),
                       .Names = c("col1", "col2", "col3"),
                       row.names = integer(0),
                       class = "data.frame"))


## empty with title
fn <- "orgtable8.org"
expect_equal(readOrg(fn),
             structure(list(col1 = character(0),
                            col2 = character(0),
                            col3 = character(0)),
                       .Names = c("col1", "col2", "col3"),
                       row.names = integer(0),
                       class = "data.frame"))

## with title
fn <- "orgtable9.org"
expect_equal(readOrg(fn),
             structure(list(col1 = 1L,
                            col2 = 2L,
                            col3 = 3L),
                       .Names = c("col1", "col2", "col3"),
                       row.names = c(NA, -1L),
                       class = "data.frame"))


## pass on argument "dec"
fn <- "orgtable10.org"
expect_equal(readOrg(fn, dec = ","),
             structure(list(col1 = c(1.1, 4.4),
                            col2 = c(2.2, 5.5),
                            col3 = c(3.3, 6.6)),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))


## named table
fn <- "orgtable11.org"
expect_equal(readOrg(fn, table.name = "table$"),
             structure(list(C1 = c( "w7",  "w8"),
                            C2 = c( "w9", "w10"),
                            C3 = c("w11", "w12")),
                       class = "data.frame",
                       row.names = c(NA, -2L)))

## ... ERROR
## there are two tables whose name starts with table1:
## the function stop()s
expect_error(readOrg(fn, table.name = "table1"))
## ==> specify table name via regex
## t <- readOrg(fn, table.name = "table1$")




fn <- "orgtable12.org"
## named table
x <- readOrg(fn, table.name = "tab1", header = FALSE)
expect_equal(x[[1]], c("x", "3"))
expect_equal(x[[2]], c("y", "4"))

x <- readOrg(fn, table.name = "tab1", header = TRUE)
expect_equal(x[[1]], 3)
expect_equal(x[[2]], 4)
expect_equal(colnames(x), c("x", "y"))

x <- readOrg(fn, table.name = "tab2", header = FALSE)
expect_equal(x[[1]], c("x", "3"))
expect_equal(x[[2]], c("y", "4"))

x <- readOrg(fn, table.name = "tab2", header = TRUE)
expect_equal(x[[1]], 3)
expect_equal(x[[2]], 4)
expect_equal(colnames(x), c("x", "y"))


x <- readOrg(fn, table.name = "tab3", header = FALSE)
expect_equal(x[[1]], c("x", "3"))
expect_equal(x[[2]], c("y", "4"))

x <- readOrg(fn, table.name = "tab3", header = TRUE)



## many empty lines
fn <- "orgtable13.org"
x <- readOrg(fn, header = TRUE)

## several header lines
fn <- "orgtable14.org"
x <- readOrg(fn, header = TRUE, collapse.header = TRUE)
expect_equivalent(unlist(x[1, ]), c(1, 2))
x <- readOrg(fn, header = TRUE, collapse.header = FALSE)




fn <- "orgtable15.org"
readOrg(fn, table.name = "T1")
##   A X
## 1 1 2

readOrg(fn, table.name = "T1", strip.horiz.rules = FALSE)

readOrg(fn, table.name = "T1", strip.horiz.rules = FALSE,
        header = FALSE)


##  #+NAME: T2
##  |---+---|
##  |---+---|
##  |---+---|
##  |---+---|
##  |---+---|
##  |---+---|

x <- readOrg(fn, table.name = "T2")
## structure(list(), names = character(0),
##           row.names = integer(0), class = "data.frame")
expect_equal(nrow(x), 0)

x <- readOrg(fn, table.name = "T2", strip.horiz.rules = FALSE)
expect_equal(nrow(x), 5) ## one row is used for headers

x <- readOrg(fn, table.name = "T2", strip.horiz.rules = FALSE,
             header = FALSE)
expect_equal(nrow(x), 6) ## no row is used for headers



## #+NAME: T3
## | 1 | 2 |

x <- readOrg(fn, table.name = "T3", header = TRUE)
expect_equal(nrow(x), 0)

x <- readOrg(fn, table.name = "T3", header = FALSE)
expect_equal(nrow(x), 1)
expect_equal(x[[1]], 1)
expect_equal(x[[2]], 2)



## #+NAME: T4
## | 1 | 2 |
## |---+---|
## |---+---|
## |---+---|
## |---+---|
## |---+---|
x <- readOrg(fn, table.name = "T4")
expect_equal(nrow(x), 0) ## 1 | 2 is used as headers

x <- readOrg(fn, table.name = "T4", header = FALSE)
expect_equal(nrow(x), 1)
expect_equal(x[[1]], 1)
expect_equal(x[[2]], 2)

x <- readOrg(fn, table.name = "T4",
             strip.horiz.rules = FALSE)
expect_equal(colnames(x), c("1", "2"))
expect_equal(x[[1]], rep("---", 5))
expect_equal(x[[2]], rep("---", 5))
