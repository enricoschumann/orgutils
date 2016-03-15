## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

test.toOrg <- function() {

    require("org")
    require("RUnit")

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
    
}
