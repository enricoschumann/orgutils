if (require("RUnit", quietly = TRUE)) {
    localTesting <- TRUE
    pkg <- "org"
    require(pkg)
    if (localTesting)
        path <- paste0("~/Packages/", pkg, "/inst/unitTests" else
    path <- system.file("unitTests", package = "org")

    myTestSuite <- defineTestSuite(pkg, dirs = path,
                                   testFileRegexp = "unitTests.+")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
