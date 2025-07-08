toOrg <- function(x, ... )
    UseMethod("toOrg")



toOrg.data.frame <- function(x, row.names = NULL, ...) {
    is.f <- unlist(lapply(x, function(i) inherits(i, "factor") ||
                                         inherits(i, "Date")))
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
                    dec = ".",
                    comment.char = "",
                    encoding = "",
                    strip.white = TRUE,
                    stringsAsFactors = FALSE,
                    table.name = NULL, text,
                    table.missing = NULL, ...,
                    strip.format = TRUE,
                    strip.horiz.rules = TRUE,
                    collapse.header = FALSE) {

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
        if (!length(start)) {
            if (is.null(table.missing))
                return(invisible(NULL))
            else
                stop("table ", sQuote(table.name), " not found")
        }
        start <- start + 1L
        end <- grep("^ *[^|]|^\\s*$", txt[start:length(txt)], perl = TRUE)
        if (!length(end))
            end <- length(txt) else end <- start + min(end) - 2L
        txt <- txt[start:end]

    } else {
        start <- grep("^\\s*[|]", txt)
        if (length(start))
            start <- min(start)
        end <- grep("^ *[^|]|^\\s*$", txt[start:length(txt)], perl = TRUE)
        if (!length(end))
            end <- length(txt) else end <- start + min(end) - 2L
        txt <- txt[start:end]
    }

    if (collapse.header)
        header <- TRUE

    ## DROP FORMAT  <lrc> <N> <lrcN>
    ## FIXME use more specific rx; drop column groups
    if (strip.format)
        txt <- txt[!grepl("^\\s*[|]\\s*<[0-9lcr]+>[0-9<>rlc| ]*$",
                          txt, perl = TRUE)]


    ## HORIZONTAL RULES
    hr <- grepl("^\\s*\\|-", txt, perl = TRUE)

    ##   !collapse.header && strip.horiz.rules
    ##   ==> remove all rules
    if (!collapse.header && strip.horiz.rules)
        txt <- txt[!hr]


    ##    collapse.header && strip.horiz.rules
    ##   ==> remove only leading rules
    if ( collapse.header && strip.horiz.rules) {
        cr <- which(!hr) ## content rows
        if (!length(cr))
            txt <- character(0)
        else if (min(cr) > 1)
            txt <- txt[-seq_len(min(cr) - 1)]
    }

    if (!strip.horiz.rules) {
        txt[hr] <- gsub("[+]", "|", txt[hr])
    }

    if (!header) {
        h <- 0

    } else {

        if (collapse.header) {
            h <- grep("^\\s*\\|-", txt, perl = TRUE)
            if (length(h))
                h <- min(h) - 1 else h <- 1
        } else
            h <- 1

        headers <- txt[ seq_len(h)]
        txt <-     txt[-seq_len(h)    ]

        headers <- strsplit(headers, "|", fixed = TRUE)
        headers <- lapply(headers, trimws)
        headers <- do.call(paste, headers)[-1L]

        if (collapse.header && strip.horiz.rules)
            ## drop remaining horizontal lines
            txt <- txt[!grepl("^\\s*\\|-", txt, perl = TRUE)]
    }

    if (length(txt) && any(txt != "")) {
        res <- read.csv(textConnection(txt),
                        header = FALSE, sep = "|",
                        dec = dec,
                        stringsAsFactors = stringsAsFactors,
                        fileEncoding = encoding,
                        strip.white = strip.white, ...)

        ## drop first and last column
        res <- res[, c(-1L, -length(res))]

    } else {
        res <- vector("list", length = length(headers))
        for (i in seq_along(res))
            res[[i]] <- character(0L)
        res <- as.data.frame(res, stringsAsFactors = FALSE)
    }

    if (header && length(headers))
        colnames(res) <- headers

    res
}



cleanOrg <- function(file,
                     clock = TRUE,
                     state.changes = TRUE,
                     dry.run = FALSE,
                     overwrite = TRUE,
                     ...) {

    txt <- readLines(file, ...)
    i <- 0
    j <- 0
    if (clock) {
        i <- grep(paste0(" *CLOCK: *[[<][0-9][0-9][0-9][0-9]-",
                         "[0-9][0-9]-[0-9][0-9].*"), txt,
                  perl = TRUE)
    } else
        i <- 0
    if (state.changes) {
        j <- grep(paste0(' *- *State *".*" *from *".*" *',
                         '[<[][0-9][0-9][0-9][0-9]-',
                         '[0-9][0-9]-[0-9][0-9].*'), txt,
                  perl = TRUE)
    } else
        j <- 0

    ij <- sort(unique(c(i,j)))
    ij <- ij[ij > 0]
    if (dry.run) {
        df <- data.frame("Removed lines" = txt[ij],
                         stringsAsFactors = FALSE)
        row.names(df) <- as.character(ij)
        df
    } else if (overwrite) {
        writeLines(txt[-ij], file)
    } else
        txt[-ij]
}

.readOrg <-function (file, header = TRUE,
                     dec = ".", comment.char = "",
                     encoding = "", strip.white = TRUE,
                     stringsAsFactors = FALSE,
                     table.name = NULL, text,
                     table.missing = NULL,
                     file.filter = "org$", ...) {

    files <- file
    isdir <- file.info(files)$isdir

    more.files <- NULL
    for (d in files[isdir]) {
        more.files <- c(more.files, dir(d, full.names = TRUE))
    }

    files <- files[!isdir]
    files <- c(files, more.files)
    if (is.character(file.filter))
        files <- files[grepl(file.filter, files)]

    all_files <- list()
    for (file in files) {

        all_files[[file]] <- .readOrg1(file)
    }

}

.readOrg1 <-
function(file,
         TODO.states = c("TODO", "DONE"), ...) {

    txt <- readLines(file, ...)

    start <- grep("^[*][*][^*]", txt)
    end <- c(start[-length(start)] + diff(start) - 1, length(txt))

    df <- data.frame(start, end)

    ans <- vector("list", nrow(df))

    for (i in seq_len(nrow(df)))
        ans[[i]] <- txt[ df[[1]][i]:df[[2]][i] ]

    .entry <- function(s) {

        txt <- s

        if (grepl(":[a-zA-Z]:$", s[1L])) {
            tags <- gsub(".* (+[a-zA-Z:]+:)$", "\\1", s[1])
            tags <- strsplit(tags, ":", fixed = TRUE)[[1]]
            tags <- tags[tags != ""]
        } else
            tags <- NULL


        title <- gsub(paste0("^[*]+ (",
                             paste0(TODO.states, collapse = "|"),
                             " )?"), "", s[1])
        title <- gsub(" +[a-zA-Z:]+:$", "", title)
        s <- s[-1]

        i <- grep("^\\s+created: ", s)
        if (length(i)) {
            created <- as.Date(gsub(".*(\\d{4}-\\d{2}.\\d{2}).*", "\\1", s[i]))
            s <- s[-i]
        } else
            created <- NA

        i <- grep("DEADLINE: ", s)
        if (length(i)) {
            deadline <- as.Date(gsub(".*(\\d{4}-\\d{2}.\\d{2}).*", "\\1", s[i]))
            s <- s[-i]
        } else
            deadline <- NA

        list(title = title,
             created = created,
             deadline = deadline,
             scheduled = NA,
             tags = tags,
             body = s,
             source = txt)

    }

    lapply(ans, .entry)
}
