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

  if(gitTF){

    repo <- git2r::repository(gitRepo)

    committed <- git2r::last_commit(repo )

    gitSummary <- utils::capture.output(print(committed))

    return(gitSummary)

  }else{

    return("")
  }


}

#' Create compute DocID with system user and compute time
#' @param gitTF Logical whether to include git information
#' @param gitRepo path to git repository (defaults to working directory)
#' @return data.frame
#' @export
#' @examples
#'
#' # Requires internet connection to access GitHub.
#' docIDcomputed()
#'


docIDcomputed <- function(gitTF=FALSE,gitRepo=getwd()) {

  user <- witchUser()
  time <- Sys.time()

  idFrame <- data.frame(ComputedBy=user,ComputeTime = time)

  idFrame$gitInfo <- gitSummary(gitRepo,gitTF)


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
#' # Requires internet connection to access GitHub.
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
#' printDocId()
#'
#'
newlinesDocID <- function(bornTF=TRUE,...){
  return(paste("\n\n",printDocId(bornTF,...),"\n\n",collapse="\n"))

}




