% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{missing_values_all_tbls}
\alias{missing_values_all_tbls}
\title{Add missing value info to all attribute tables in the data_list}
\usage{
missing_values_all_tbls(data_list)
}
\arguments{
\item{data_list}{A large list from
\code{\link{fetch_sql_server_data()}}.}
}
\value{
A large list (data_list) with the updated attribute table(s).
}
\description{
Adds generic missing value information to all the attribute tables in a
data_list if a column contains NA or null values. Uses
\code{\link{missing_values()}}, which checks to see if the missingValueCode
is \code{NA} or \code{NULL}. If it is, it is assumed that no custom values
have been entered for missingValueCode AND missingValueCodeExplanation. As a
result, this function can be used to fill in the remaining \code{NA} and
\code{NULL} column missing value information with the generic info after
customizing applicable missing value information.
}
\seealso{
\code{\link{missing_values()}}
}
