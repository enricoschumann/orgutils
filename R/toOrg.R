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

    if (!is.null(table.name)) {
        txt <- readLines(file)
        start <- grep(paste0("^#\\+name: ", table.name), txt, ignore.case = TRUE)
        if (!length(start))
            stop("table ", sQuote(table.name), " not found")
        end <- grep("^ *[^|]|^ *$", txt)
        end <- min(end[end > start]) - 1L
        start <- start + 1L
        txt <- txt[start:end]
        file <- txt
    }

    sep <- 0
    if (header) {
        head <- readLines(textConnection(file), n = 5, encoding = encoding)
        line <- min(grep("^ *\\| [^<]", head))
        sep <- max(grep("^\\s*\\|-", head, perl = TRUE))
        headers <- trim(strsplit(head[line], "|",
                                    fixed = TRUE)[[1]][-1])
    }
    txt <- read.csv(textConnection(file), header = FALSE,
                    skip = sep, sep = "|",
                    stringsAsFactors = FALSE, fileEncoding = encoding,
                    strip.white = strip.white, ...)
    txt <- txt[ , c(-1, -length(txt))]
    if (header)
        colnames(txt) <- headers
    txt
}
## file <- "~/projects3/Fund_Replication/funds.org"
## table.name  <- "allinstruments"
