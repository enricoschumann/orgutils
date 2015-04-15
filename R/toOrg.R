## helper function; may be moved to texttools
rmspace <- function (s) {
    s <- gsub("^\\s*", "", s, perl = TRUE)
    gsub("\\s*$", "", s, perl = TRUE)
}

toOrg <- function(x, ... ){
    UseMethod("toOrg")
}

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

print.org <- function(x, ...) {
    cat(x, sep = "\n")
    invisible(x)
}

readOrg <-function (file, header = TRUE, 
                     dec = ".", comment.char = "",
                     encoding = "unknown", ...) {
    if (header) {
        head <- readLines(file, n = 5, encoding = encoding)
        sep <- max(grep("^\\s*\\|-", head, perl = TRUE))
        headers <- rmspace(strsplit(head[sep - 1], "|",
                                    fixed = TRUE)[[1]][-1])
    }
    txt <- read.csv(file, header = FALSE, skip = sep, sep = "|",
                    stringsAsFactors = FALSE, fileEncoding = encoding)
    txt[ , c(-1, -length(txt))]
}
