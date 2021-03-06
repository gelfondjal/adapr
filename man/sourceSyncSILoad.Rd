% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source_sync_si_load.R
\name{sourceSyncSILoad}
\alias{sourceSyncSILoad}
\title{Synchronize project by IDENTIFYING necessary R scripts}
\usage{
sourceSyncSILoad(source_info)
}
\arguments{
\item{source_info}{Project information within source_info list}
}
\value{
Data.frame with sources needed to synchronize with run times
}
\description{
Synchronize project by IDENTIFYING necessary R scripts
}
\details{
Not usually direct use. See syncProject() and syncTestProject().
}
\examples{
\dontrun{
source_info <- create_source_file_dir("adaprHome","tree_controller.R")
sourceSyncSILoad(source_info)
} 
}
