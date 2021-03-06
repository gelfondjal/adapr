% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess_write_fnc.R
\name{guessWriteFcn}
\alias{guessWriteFcn}
\title{Return function for writing common file types}
\usage{
guessWriteFcn(filename)
}
\arguments{
\item{filename}{file to be written}
}
\value{
function for writing file
}
\description{
Return function for writing common file types
}
\details{
Uses the file suffix to return write.csv, png, pdf, save, saveRDS
}
\examples{
\dontrun{
 identical(utils::write.csv,guessWriteFcn("data.csv"))
} 
}
