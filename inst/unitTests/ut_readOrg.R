## -*- truncate-lines: t; -*-

test.readOrg <- function() {
    require("org")
    require("RUnit")

    ## empty file: readOrg stops, like 'read.table'
    ## read.table("~/Packages/org/inst/unitTests/orgtable0.org")
    checkException(readOrg("~/Packages/org/inst/unitTests/orgtable0.org"),
                   silent = TRUE)

    
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

    
    t5 <- readOrg("~/Packages/org/inst/unitTests/orgtable5.org",
                  table.name = "table5")
    checkEquals(t5,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))

    
    t6 <- readOrg("~/Packages/org/inst/unitTests/orgtable6.org",
                      table.name = "table6")
    checkEquals(t6,
                structure(list(col1 = c(1L, 4L, 7L),
                               col2 = c(2L, 5L, 8L),
                               col3 = c(3L, 6L, 9L)),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -3L)))


    ## an empty org table
    t7 <- readOrg("~/Packages/org/inst/unitTests/orgtable7.org")
    checkEquals(t7,
                structure(list(col1 = character(0),
                               col2 = character(0),
                               col3 = character(0)),
                          .Names = c("col1", "col2", "col3"),
                          row.names = integer(0),
                          class = "data.frame"))
    
}
