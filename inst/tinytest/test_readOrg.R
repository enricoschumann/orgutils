t <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable11.org",
              table.name = "table$")
expect_equal(t,
            structure(list(C1 = c("w7", "w8"),
                           C2 = c("w9", "w10"),
                           C3 = c("w11", "w12")),
                      class = "data.frame",
                      row.names = c(NA, -2L)))
## ERROR
## t <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable11.org",
##               table.name = "table1")

t <- readOrg("~/Packages/orgutils/inst/unitTests/orgtable11.org",
              table.name = "table1$")
