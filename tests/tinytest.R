if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("orgutils",
                                               color = interactive(),
                                               verbose = 1)
