#' Find path for git executable
#' @param git_binary_name git binary name 
#' @return git executable
#' @export
#' 
git_path <- function(git_binary_name = NULL) {
  # Use user supplied path
  if (!is.null(git_binary_name)) {
    if (!file.exists(git_binary_name)) {
      stop("Path ", git_binary_name, " does not exist", .call = FALSE)
    }
    return(git_binary_name)
  }
  
  # Look on path
  git_path <- Sys.which("git")[[1]]
  if (git_path != "") return(git_path)
  
  # On Windows, look in common locations
  if (.Platform$OS.type == "windows") {
    look_in <- c(
      "C:/Program Files/Git/bin/git.exe",
      "C:/Program Files (x86)/Git/bin/git.exe"
    )
    found <- file.exists(look_in)
    if (any(found)) return(look_in[found][1])
  }
  
  stop("Git does not seem to be installed on your system.", call. = FALSE)
}


