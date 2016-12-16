## -*- truncate-lines: t; -*-

test.readOrg <- function() {

    ## empty file: readOrg stops, like 'read.table'
    ## read.table("~/Packages/orgutils/inst/unitTests/orgtable0.org")
    checkException(readOrg("~/Packages/orgutils/inst/unitTests/orgtable0.org"),
                   silent = TRUE)


    
    t1 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable1.org")
    checkEquals(t1,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))


    
    t2 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable2.org")
    checkEquals(t2,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))


    
    t3 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable3.org",
                  table.name = "table3")
    checkEquals(t3,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))
    

    
    t4 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable4.org",
                  table.name = "table4")
    checkEquals(t4,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))

    
    
    t5 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable5.org",
                  table.name = "table5")
    checkEquals(t5,
                structure(list(col1 = c(1L, 4L), col2 = c(2L, 5L),
                               col3 = c("3", "test")),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -2L)))

    
    
    t6 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable6.org",
                      table.name = "table6")
    checkEquals(t6,
                structure(list(col1 = c(1L, 4L, 7L),
                               col2 = c(2L, 5L, 8L),
                               col3 = c(3L, 6L, 9L)),
                          .Names = c("col1", "col2", "col3"),
                          class = "data.frame",
                          row.names = c(NA, -3L)))

    

    ## an empty org table
    t7 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable7.org")
    checkEquals(t7,
                structure(list(col1 = character(0),
                               col2 = character(0),
                               col3 = character(0)),
                          .Names = c("col1", "col2", "col3"),
                          row.names = integer(0),
                          class = "data.frame"))

    
    ## empty with title
    t8 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable8.org")
    checkEquals(t8,
                structure(list(col1 = character(0),
                               col2 = character(0),
                               col3 = character(0)),
                          .Names = c("col1", "col2", "col3"),
                          row.names = integer(0),
                          class = "data.frame"))

    ## with title
    t9 <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable9.org")
    checkEquals(t9,
                structure(list(col1 = 1L,
                               col2 = 2L,
                               col3 = 3L),
                          .Names = c("col1", "col2", "col3"),
                          row.names = c(NA, -1L),
                          class = "data.frame"))
    
}




test.toOrg <- function() {

    df <- data.frame(x = 1:3, row.names = letters[1:3])
    tbl <- structure(c("| row.names | x |",
                       "|-----------+---|",
                       "| a         | 1 |", 
                       "| b         | 2 |",
                       "| c         | 3 |"),
              class = "org")

    checkEquals(tbl, toOrg(df))
    checkEquals(tbl, toOrg(df), TRUE)

    
    
    df <- data.frame(x = 1:3)
    tbl <- structure(c("| x |",
                       "|---|",
                       "| 1 |", 
                       "| 2 |",
                       "| 3 |"),
              class = "org")
    
    checkEquals(tbl, toOrg(df))


    
    df <- data.frame(x = 1:3)
    tbl <- structure(c("| row.names | x |",
                       "|-----------+---|",
                       "| 1         | 1 |", 
                       "| 2         | 2 |",
                       "| 3         | 3 |"),
              class = "org")
    checkEquals(tbl, toOrg(data.frame(x = 1:3), TRUE))


    
    checkEquals(toOrg(as.Date("2016-1-1")),
                structure(paste0("<2016-01-01 ",
                                 format(as.Date("2016-01-01"),
                                        "%a"),
                                 ">"),
                          class = c("org", "character")))

    checkEquals(toOrg(as.Date("2016-1-1"), inactive = TRUE),                      
                structure(paste0("[2016-01-01 ",
                                 format(as.Date("2016-01-01"),
                                        "%a"),
                                 "]"),
                          class = c("org", "character")))    
}
