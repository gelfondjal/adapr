#' Digest files from (digest package)
#' @description Compute file hash without checking file.access
#' @return The filehash
#' @export
Digest <- function (object, algo = c("md5", "sha1", "crc32", "sha256", 
                                     "sha512"), serialize = TRUE, file = FALSE, length = Inf, 
                    skip = "auto", ascii = FALSE, raw = FALSE) 
{
  algo <- match.arg(algo)
  if (is.infinite(length)) {
    length <- -1
  }
  if (is.character(file) && missing(object)) {
    object <- file
    file <- TRUE
  }
  if (serialize && !file) {
    object <- serialize(object, connection = NULL, ascii = ascii)
    if (any(!is.na(pmatch(skip, "auto")))) {
      if (ascii) {
        skip <- which(object[1:30] == as.raw(10))[4]
      }
      else {
        skip <- 14
      }
    }
  }
  else if (!is.character(object) && !inherits(object, "raw")) {
    stop("Argument object must be of type character or raw vector if serialize is FALSE")
  }
  if (file && !is.character(object)) 
    stop("file=TRUE can only be used with a character object")
  algoint <- switch(algo, md5 = 1, sha1 = 2, crc32 = 3, sha256 = 4, 
                    sha512 = 5)
  if (file) {
    algoint <- algoint + 100
    object <- path.expand(object)
    if (!file.exists(object)) {
      stop("The file does not exist: ", object)
    }
    if (!isTRUE(!file.info(object)$isdir)) {
      stop("The specified pathname is not a file: ", object)
    }
#    if (file.access(object, 4)) {
#      stop("The specified file is not readable: ", object)
#    }
  }
  if (is.character(skip)) 
    skip <- 0
  val <- .Call("digest", object, as.integer(algoint), as.integer(length), 
               as.integer(skip), as.integer(raw), PACKAGE = "digest")
  return(val)
}
    
