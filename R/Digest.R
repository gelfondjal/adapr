#' Digest files from (digest package)
#' @description Compute file hash without checking file.access
#' @param object R object or file
#' @param algo digest hash algorithm
#' @param serialize FALSE allows hash comparison of known output
#' @param file logical TRUE iff object is file 
#' @param length Size of object/file to hash
#' @param skip How many input bytes to skip for computing hash
#' @param ascii ASCII or binary compression
#' @param raw logical digest output in binary form
#' @details Uses digest from package "digest". Authors Dirk Eddelbuettel edd@debian.org for the R interface; Antoine Lucas for the integration of crc32; Jarek Tuszynski for the file-based operations; Henrik Bengtsson and Simon Urbanek for improved serialization patches; Christophe Devine for the hash function implementations for sha-1, sha-256 and md5; Jean-loup Gailly and Mark Adler for crc32; Hannes Muehleisen for the integration of sha-512; Jim Hester for the integration of xxhash32, xxhash64 and murmur32.
#' @return The filehash
#' @export
Digest <- function (object=NULL, algo=checkAdaprHashAlgo(),serialize = FALSE, file = TRUE, length = Inf, 
                    skip = "auto", ascii = FALSE, raw = FALSE) 
{
	
  #equire(digest)
  
  object0 <- object 
  algo0 <- algo
  serialize0 <- serialize
  file0 <- file
  length0 <- length
  skip0 <- skip
  ascii0 <- ascii
  raw0 <- raw
  
  val <- digest::digest(algo=algo0 , serialize = serialize0 , file = file0, length = length0, 
                 skip = skip0, ascii = ascii0 , raw = raw0 ,errormode="warn")
  
  
  
  return(val)
}
    

#' Checks or changes the specified adapr hash algorithm (adaprHashAlgo option)
#' @param hashAlgorithm characters specifying adaprHash algorithm if changing
#' @return value is specified algorithm or default algorithm
#' @details Current default is sha1. If algorithm not recognized then will not change option.
#' @export
#' 
checkAdaprHashAlgo <- function(hashAlgorithm=""){
  
  defaultAlgorithm <- "sha1"
  
  knownAlgos = c("md5", "sha1", "crc32", "sha256", "sha512")
  
  test <- is.null(options()$adaprHashAlgo)
  
  if(test){
    if(hashAlgorithm==""){
      options(adaprHashAlgo = defaultAlgorithm)
      return(defaultAlgorithm)
    }
  }
  
  if(hashAlgorithm!=""){
  
    if(hashAlgorithm %in% knownAlgos){
      options(adaprHashAlgo = hashAlgorithm)
    }else{
      hashAlgorithm <- defaultAlgorithm
      options(adaprHashAlgo = hashAlgorithm)
    }
    
  }else{
    
    hashAlgorithm <- options()$adaprHashAlgo
  }
  
  return(hashAlgorithm)
  
}

