#' Create creation DocID based on true random number
#' @param nhash Number of hashes to avoid hash collision
#' @param gitTF Logical whether to include git information
#' @param gitRepo path to git repository (defaults to working directory)
#' @return data.frame
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' docIDborn()
#'

docIDborn <- function(nhash=5,gitTF=FALSE,gitRepo=getwd()) {

  nhash <- as.integer(nhash)

  docIDs <- random::randomNumbers(n=nhash, min=1, max=1e6,col=1)
  user <- witchUser()
  time <- Sys.time()

  idFrame <- list(docIDs,Creator=user,DesignedOn = time)

  idFrame <- data.frame(docID=digest::digest(idFrame),Creator=user,DesignedOn = time)

  idFrame$gitInfo <- gitSummary(gitRepo,gitTF)

  rownames(idFrame) <- NULL

  return(idFrame)
}
#' Retrieve or identify a username.
#' @param userName String to specify user's name
#' @param setUser logical to set user as userName
#' @return character string for git summary
#' @details Accesses the options()$WitchUser variable or the system user name.
#' @export
#' @examples
#' witchUser()

witchUser <- function(userName="Anonymous",setUser=FALSE){

  if(setUser==TRUE){
    options(whichUser=setUser)
    return(setUser)
  }

  if(is.null(options()$whichUser)){
    return(Sys.info()["user"])
  }else{
      return(options()$whichUser)
    }


}

#' Create creation Git summary string with git2r function
#' @param gitRepo path to git repository (defaults to working directory)
#' @param gitTF logical to return git (if TRUE) or empty string (if FALSE)
#' @return character string for git summary
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' gitSummary()
#'


gitSummary <- function(gitRepo="",gitTF=FALSE){

  gitSummary <- ""
  
  if(gitTF){
  try({
    repo <- git2r::repository(gitRepo)

    committed <- git2r::last_commit(repo )

    gitSummary <- utils::capture.output(print(committed))
  })
    return(gitSummary)

  }else{

    return(gitSummary)
  }
  return(gitSummary)

}


#' sourceFinder creates a filename and hash at runtime
#' @param parentFile0 path to parent file that creates docutment
#' @return data.frame with parent file name and its hash (sha1)
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' sourceFinder(parentFile0=parent.frame(2)$ofile)
#'
sourceFinder <- function(parentFile0=parent.frame(2)$ofile){

  parentFile <- ifelse(is.null(parentFile0),"Interactive",parentFile0)

  pHash <- ifelse(is.null(parentFile0),"",digest::digest(file=parentFile,serialize = FALSE,algo="sha1"))

  parentInfo <- data.frame(parentFile=basename(parentFile),parentHash=pHash)

  rownames(parentInfo) <- NULL
  
return(parentInfo)

}

#' Create compute DocID with system user and compute time
#' @param gitTF Logical whether to include git information
#' @param gitRepo path to git repository (defaults to working directory)
#' @param parentFile filename of source file
#' @param ... arguments to sourceFinder
#' @return data.frame
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' docIDcomputed()
#'


docIDcomputed <- function(gitTF=TRUE,gitRepo=getwd(),parentFile=parent.frame(2)$ofile) {

  user <- witchUser()
  time <- Sys.time()

  idFrame <- data.frame(ComputedBy=user,ComputeTime = time)
  
  
  idFrame$gitInfo <- gitSummary(gitRepo,gitTF)

  idFrame <- cbind(idFrame,sourceFinder(parentFile0 = parentFile))
  
  rownames(idFrame) <- NULL

  return(idFrame)
}


#' Create character strings for DocID with system user and compute time
#' @param bornTF Logical to print the born on/create time (TRUE) or the compute time (FALSE-default)
#' @param ... extra arguments to docIDborn or computed to git repository
#' @return data.frame
#' @export
#' @examples
#'
#' # Requires internet connection to access true random numbers
#' printDocId()
#'
#'
printDocId <- function(bornTF=FALSE,...){
  if(bornTF){
    return(knitr::kable(docIDborn(...)))
  }else{
    return(knitr::kable(docIDcomputed(...)))
    }
}

#' Generate string vector to render creation docID
#' @param bornTF Logical to print the born on/create time (TRUE) or the compute time (FALSE-default)
#' @param ... extra arguments to docIDborn or computed to git repository
#' @return Character vector for docID
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' newlinesDocID()
#'
#'
newlinesDocID <- function(bornTF=TRUE,...){
  return(paste("\n\n",printDocId(bornTF,...),"\n\n",collapse="\n"))

}


#' Generate code to render creation compute time docID
#' @return Character vector for code chunk for computing docID
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' codeChunkDocId()
#'
#'
codeChunkDocId <- function(){
  
  codeOut <- c("```\n\n\n",
  "```{r,echo=FALSE,message=FALSE,warning=FALSE,include=TRUE}\n",
  
  "printDocId(bornTF = FALSE,gitTF=TRUE))\n",  
  "```\n\n\n")
  
  return(codeOut)
  
  
}
