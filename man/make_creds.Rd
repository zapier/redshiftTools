% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{make_creds}
\alias{make_creds}
\title{Make Credentials}
\usage{
make_creds(access_key = NULL, secret_key = NULL)
}
\arguments{
\item{access_key}{character (optional)}

\item{secret_key}{character (optional)}
}
\value{
character string to append to provide creds to Redshift
}
\description{
Uses access_key and secret_key if provided,
otherwise use REDSHIFT_ROLE env var if available,
otherwise use AWS_ACCESS_KEY and AWS_SECRET_ACCESS_KEY env var,
otherwise error.
}
\details{
REDSHIFT_ROLE should be the full resource address, e.g. arn:aws:iam::###:role/{name}
}
