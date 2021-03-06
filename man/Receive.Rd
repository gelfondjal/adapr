% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pass_recieve.R
\name{Receive}
\alias{Receive}
\title{Simple way to read R object into script produced from another.}
\usage{
Receive(varname, Rscript = "", output = FALSE)
}
\arguments{
\item{varname}{String of R object name from other R script.}

\item{Rscript}{R script that created the R object to read in.}

\item{output}{Default FALSE logical indicating whether to return object object or create object of name varname.}
}
\value{
NULL or value of varname
}
\description{
Simple way to read R object into script produced from another.
}
\details{
Creates object in environment called varname. Don't need to specify Rscript.

Adds '.rda' to object name and reads from Result file. Works with 'Pass'.
}
\examples{
\dontrun{
# Requires internet connection to access GitHub.
Recieve("myObject") # Creates myObject in workspace
newName <- Recieve("myObject",output=TRUE) # Returns value of 'myObject'
Receive("myObject","myRscript.R")
} 
}
