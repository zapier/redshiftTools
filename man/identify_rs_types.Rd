% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redshift-utils.R
\name{identify_rs_types}
\alias{identify_rs_types}
\title{Identify Corresponding Redshift Types}
\usage{
identify_rs_types(.data, character_length = NA_real_)
}
\arguments{
\item{.data}{\code{data.frame}}

\item{character_length}{The length you want for your VARCHAR, by default we'll just use 10\% more than the max value we see in the provided data.}
}
\value{
character vector
}
\description{
Identify Corresponding Redshift Types
}
