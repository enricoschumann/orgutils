if (require("RUnit", quietly = TRUE)) {
    localTesting <- TRUE
    require("org")
    if (localTesting)
        path <- "~/Packages/NMOF/inst/unitTests" else
    path <- system.file("unitTests", package = "org")

    myTestSuite <- defineTestSuite("org",
                                   dirs = path,
                                   testFileRegexp = "unitTests.+")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
