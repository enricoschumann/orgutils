## -*- truncate-lines: t; -*-

test.readOrg <- function() {
    require("org")

    t1 <- readOrg("~/Packages/org/inst/unitTests/orgtable1.org")
    checkEquals(t1,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))


    t2 <- readOrg("~/Packages/org/inst/unitTests/orgtable2.org")
    checkEquals(t2,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))

    
    t3 <- readOrg("~/Packages/org/inst/unitTests/orgtable3.org",
                  table.name = "table3")
    checkEquals(t3,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))
    

    t4 <- readOrg("~/Packages/org/inst/unitTests/orgtable4.org",
                  table.name = "table4")
    checkEquals(t4,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))

}
