df <- data.frame(x = 1:3, row.names = letters[1:3])
tbl <- structure(c("| row.names | x |",
                   "|-----------+---|",
                   "| a         | 1 |",
                   "| b         | 2 |",
                   "| c         | 3 |"),
                 class = "org")

expect_equal(tbl, toOrg(df))
expect_equal(tbl, toOrg(df), TRUE)



df <- data.frame(x = 1:3)
tbl <- structure(c("| x |",
                   "|---|",
                   "| 1 |",
                   "| 2 |",
                   "| 3 |"),
                 class = "org")

expect_equal(tbl, toOrg(df))



df <- data.frame(x = 1:3)
tbl <- structure(c("| row.names | x |",
                   "|-----------+---|",
                   "| 1         | 1 |",
                   "| 2         | 2 |",
                   "| 3         | 3 |"),
                 class = "org")
expect_equal(tbl, toOrg(data.frame(x = 1:3), TRUE))


## Date
expect_equal(toOrg(as.Date("2016-1-1")),
             structure(paste0("<2016-01-01 ",
                              format(as.Date("2016-01-01"),
                                     "%a"),
                              ">"),
                       class = c("org", "character")))

expect_equal(toOrg(as.Date("2016-1-1"), inactive = TRUE),
             structure(paste0("[2016-01-01 ",
                              format(as.Date("2016-01-01"),
                                     "%a"),
                              "]"),
                       class = c("org", "character")))

## POSIXt
##   The test may fail since it depends on finding
##   the system's local timezone.
times <- as.POSIXct(c("2016-1-1 10:00:00",
                      "2016-1-1 11:00:00"),
                    tz = Sys.timezone(location = TRUE))
expect_equal(toOrg(times),
             structure(paste("<2016-01-01",
                             format(times, "%a"),
                             c("10:00:00>", "11:00:00>")),
                       class = c("org", "character")))

times <- as.POSIXlt(times)
expect_equal(toOrg(times),
            structure(paste("<2016-01-01",
                            format(times, "%a"),
                            c("10:00:00>", "11:00:00>")),
                      class = c("org", "character")))
