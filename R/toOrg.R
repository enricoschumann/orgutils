toOrg <- function(x, ... )
    UseMethod("toOrg")

toOrg.data.frame <- function(x, row.names = NULL, ...) {
    is.f <- unlist(lapply(x, function(i) inherits(i, "factor") || inherits(i, "Date")))
    if (any(is.f)) {
        is.f <- which(is.f)
        for (i in seq_along(is.f))
            x[[ is.f[i] ]] <- as.character(x[[ is.f[i] ]])
    }

    type <- unname(unlist(lapply(x, mode)))

    rn <- if ( (is.null(row.names) &&
                nrow(x) > 0L &&
                any(attr(x, "row.names") != seq_len(nrow(x)))) ||
               isTRUE(row.names) ||
               is.character(row.names) )
              
              c(if (is.character(row.names)) row.names else "row.names",
                row.names(x))
                                                 
          else 
              NULL
    x <- rbind(colnames(x), x)
    if (!is.null(rn)) {
        x <- cbind(rn, x)
        type <- c("character", type)
    }
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

toOrg.Date <- function(x, inactive = FALSE,...) {
    res <- if (inactive)
               strftime(x, "[%Y-%m-%d %a]")
           else
               strftime(x, "<%Y-%m-%d %a>")
    class(res) <- c("org", "character")
    res
}

toOrg.POSIXt <- function(x, inactive = FALSE,...) {
    res <- if (inactive)
               strftime(x, "[%Y-%m-%d %a %H:%M:%S]")
           else
               strftime(x, "<%Y-%m-%d %a %H:%M:%S>")
    class(res) <- c("org", "character")
    res
}

print.org <- function(x, ...) {
    cat(x, sep = "\n")
    invisible(x)
}

readOrg <-function (file, header = TRUE,
                    dec = ".", comment.char = "",
                    encoding = "", strip.white = TRUE,
                    stringsAsFactors = FALSE,
                    table.name = NULL, text, ...) {

    if (missing(file) && !missing(text)) {
        txt <- text
        txt <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
    } else 
        txt <- readLines(file, encoding = encoding)
    
    if (length(txt) == 0L)
        stop("no lines available in input")
    
    if (!is.null(table.name)) {

        start <- grep(paste0("^#\\+name: ", table.name),
                      txt, ignore.case = TRUE)
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
        head <- txt[1:10]
        line <- min(grep("^ *\\| [^<]", head)) ## ignore format instructions, such as '<10>'
        sep <- min(grep("^\\s*\\|-", head, perl = TRUE))
        headers <- trim(strsplit(head[line], "|",
                                    fixed = TRUE)[[1]][-1])
    }
    if (sep > 0)
        txt <- txt[-(1:sep)]

    ## drop horizontal lines |--------------|
    txt <- txt[!grepl("^\\s*\\|-", txt, perl = TRUE)]

    if (length(txt) && any(txt != "")) {
        res <- read.csv(textConnection(txt), header = FALSE, sep = "|",
                        dec = dec,
                        stringsAsFactors = stringsAsFactors,
                        fileEncoding = encoding,
                        strip.white = strip.white, ...)
        
        ## drop first and last column
        res <- res[ , c(-1L, -length(res))]
    } else {
        res <- vector("list", length = length(headers))
        for (i in seq_along(res))
            res[[i]] <- character(0L)
        res <- as.data.frame(res, stringsAsFactors = FALSE)
    }
        
    if (header) {
            colnames(res) <- headers
    }
    res
}
