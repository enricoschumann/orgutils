toOrg <- function(x, ... )
    UseMethod("toOrg")

toOrg.data.frame <- function(x, ...) {
    type <- unname(unlist(lapply(x, mode)))
    
    x <- rbind(colnames(x), x)
    for (i in seq_len(ncol(x))) {
        just <- if (type[i] == "character")
            "left" else "right"
        if (i == 1L)
            res <- paste("|", format(x[[1L]], justify = just))
        else
            res <- paste(res, "|", format(x[[i]], justify = just))
    }
    res <- paste(res, "|")

    header <- res[1]
    rule <- paste(rep("-", nchar(header)), collapse = "")
    substr(rule, 1, 1) <- "|"
    substr(rule, nchar(rule), nchar(rule)) <- "|"

    pos <- gregexpr("|", header, fixed = TRUE)[[1]]
    pos <- pos[-c(1, length(pos))]
    for (i in seq_along(pos)) {
        substr(rule, pos[i], pos[i]) <- "+"
    }
    res <- c(res[1], rule, res[-1])
    class(res) <- "org"
    res
}

toOrg.Date <- function(x, ...) {
    strftime(x, "<%Y-%m-%d %a>")
}

print.org <- function(x, ...) {
    cat(x, sep = "\n")
    invisible(x)
}

readOrg <-function (file, header = TRUE, 
                    dec = ".", comment.char = "",
                    encoding = "", strip.white = TRUE,
                    table.name = NULL, ...) {

    txt <- readLines(file)
    
    if (!is.null(table.name)) {

        start <- grep(paste0("^#\\+name: ", table.name), txt, ignore.case = TRUE)
        if (length(start) > 1L)
            stop("several tables with the same name -- see lines ",
                    paste(start, collapse = ", "))
        if (!length(start))
            stop("table ", sQuote(table.name), " not found")
        start <- start + 1L
        end <- grep("^ *[^|]|^\\s*$", txt[start:length(txt)], perl = TRUE)
        if (!length(end))
            end <- length(txt) else end <- start + min(end) - 2L
        ## end <- min(end[end > start]) - 1L
        txt <- txt[start:end]
        
    }

    sep <- 0
    if (header) {
        head <- readLines(textConnection(txt), n = 10, encoding = encoding)
        line <- min(grep("^ *\\| [^<]", head))
        sep <- max(grep("^\\s*\\|-", head, perl = TRUE))
        headers <- trim(strsplit(head[line], "|",
                                    fixed = TRUE)[[1]][-1])
    }
    if (sep > 0)
        txt <- txt[-(1:sep)]
    res <- read.csv(textConnection(txt), header = FALSE, sep = "|",
                    stringsAsFactors = FALSE, fileEncoding = encoding,
                    strip.white = strip.white, ...)
    res <- res[ , c(-1L, -length(res))] ## drop first and last column
    if (header)
        colnames(res) <- headers
    res
}
## file <- "~/Packages/org/inst/unitTests/orgtable3.org"
## file <- "~/projects3/Fund_Replication/funds.org"
## table.name  <- "allinstruments"
