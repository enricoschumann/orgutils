
## ERROR
## empty file: readOrg stops, like 'read.table'
## read.table(system.file("unitTests/orgtable0.org", package = "orgutils"))
fn <- system.file("unitTests/orgtable0.org", package = "orgutils")
expect_error(readOrg(fn))


fn <- system.file("unitTests/orgtable00.org", package = "orgutils")
expect_error(readOrg(fn))
expect_true(is.null(readOrg(fn, table.name = "A")))
expect_error(readOrg(fn, table.name = "A", table.missing = "stop"))


fn <- system.file("unitTests/orgtable1.org", package = "orgutils")
expect_equal(readOrg(fn),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- system.file("unitTests/orgtable2.org", package = "orgutils")
expect_equal(readOrg(fn),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- system.file("unitTests/orgtable3.org", package = "orgutils")
expect_equal(readOrg(fn, table.name = "table3"),
            structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                           col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))



fn <- system.file("unitTests/orgtable4.org", package = "orgutils")
expect_equal(readOrg(fn, table.name = "table4"),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- system.file("unitTests/orgtable5.org", package = "orgutils")
expect_equal(readOrg(fn, table.name = "table5"),
             structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                            col3 = c("3", "test")),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))



fn <- system.file("unitTests/orgtable6.org", package = "orgutils")
expect_equal(readOrg(fn, table.name = "table6"),
             structure(list(col1 = c(1L, 4L, 7L),
                            col2 = c(2L, 5L, 8L),
                            col3 = c(3L, 6L, 9L)),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -3L)))



## an empty org table
fn <- system.file("unitTests/orgtable7.org", package = "orgutils")
expect_equal(readOrg(fn),
             structure(list(col1 = character(0),
                            col2 = character(0),
                            col3 = character(0)),
                       .Names = c("col1", "col2", "col3"),
                       row.names = integer(0),
                       class = "data.frame"))


## empty with title
fn <- system.file("unitTests/orgtable8.org", package = "orgutils")
## debug(readOrg)
expect_equal(readOrg(fn),
             structure(list(col1 = character(0),
                            col2 = character(0),
                            col3 = character(0)),
                       .Names = c("col1", "col2", "col3"),
                       row.names = integer(0),
                       class = "data.frame"))

## with title
fn <- system.file("unitTests/orgtable9.org", package = "orgutils")
expect_equal(readOrg(fn),
             structure(list(col1 = 1L,
                            col2 = 2L,
                            col3 = 3L),
                       .Names = c("col1", "col2", "col3"),
                       row.names = c(NA, -1L),
                       class = "data.frame"))


## pass on argument "dec"
fn <- system.file("unitTests/orgtable10.org", package = "orgutils")
expect_equal(readOrg(fn, dec = ","),
             structure(list(col1 = c(1.1, 4.4),
                            col2 = c(2.2, 5.5),
                            col3 = c(3.3, 6.6)),
                       .Names = c("col1", "col2", "col3"),
                       class = "data.frame",
                       row.names = c(NA, -2L)))


## named table
fn <- system.file("unitTests/orgtable11.org", package = "orgutils")
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




fn <- system.file("unitTests/orgtable12.org", package = "orgutils")
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
fn <- system.file("unitTests/orgtable13.org", package = "orgutils")
x <- readOrg(fn, header = TRUE)

## several header lines
fn <- system.file("unitTests/orgtable14.org", package = "orgutils")
x <- readOrg(fn, header = TRUE, collapse.header = TRUE)
expect_equivalent(unlist(x[1, ]), c(1, 2))
x <- readOrg(fn, header = TRUE, collapse.header = FALSE)
