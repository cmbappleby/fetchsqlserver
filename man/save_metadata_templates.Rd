% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{save_metadata_templates}
\alias{save_metadata_templates}
\title{Save core metadata templates}
\usage{
save_metadata_templates(data_pkg_folder = NULL)
}
\arguments{
\item{data_pkg_folder}{(optional) A string of the folder path for the data
package where data tables, attribute tables, categorical variable tables,
and metadata files will be saved. If not provided, the user will be prompted
to select a folder.}
}
\description{
Core metadata files (abstract,
additional info, methods, intellectual rights, personnel, keywords, and
possibly custom units) are saved to the data package folder for EML metadata
creation. You should
check the core metadata files to ensure they filled out properly and contain
all the information necessary for the data package before running
\code{\link{create_eml_metadata}}.
}
