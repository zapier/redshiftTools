% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redshift-utils.R
\name{query_details}
\alias{query_details}
\title{Query details}
\usage{
query_details(con, query_id = NULL)
}
\arguments{
\item{con}{Redshift Connection}

\item{query_id}{(optional) integer

Provided a query_id it will return the details of a single query;
otherwise it will return the details of all queries available to the user associated with the con.
Available details include:
* query_text <chr>, query_execution_time <dbl>, usename <chr>
This function is particularly useful for extracting the full text of any given query.}
}
\value{
data.frame
}
\description{
Query details
}
