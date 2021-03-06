% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_hyperlink.R
\name{makeRelativeHyperlink}
\alias{makeRelativeHyperlink}
\title{Makes HTML hyper link relative to a directory by clipping off directory name}
\usage{
makeRelativeHyperlink(directory.to.clip, files, links)
}
\arguments{
\item{directory.to.clip}{character of directory to remove from file path}

\item{files}{character vector of filenames}

\item{links}{description of links}
}
\value{
link command vector
}
\description{
Makes HTML hyper link relative to a directory by clipping off directory name
}
\details{
Used in making HTML files
}
\examples{

 makeRelativeHyperlink("myPath","myPath/pathToFile","click here to my path")

}
