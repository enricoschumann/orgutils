if (require("RUnit", quietly = TRUE)) {
    localTesting <- TRUE
    pkg <- "org"
    require(pkg)
    path <- if (localTesting)
        paste0("~/Packages/", pkg, "/inst/unitTests") else
        system.file("unitTests", package = "org")

    myTestSuite <- defineTestSuite(pkg, dirs = path,
                                   testFileRegexp = "ut_.+")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "test_results"),
                      ".txt", sep = ""))
}
