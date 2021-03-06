% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redshift-transaction.R
\name{transaction}
\alias{transaction}
\title{Issue commands within a single transaction block.}
\usage{
transaction(.data, .dbcon, .function_sequence)
}
\arguments{
\item{.data}{a data.frame}

\item{.dbcon}{a DBI connection}

\item{.function_sequence}{a list of functions to apply, BEWARE the order matters. They'll be run head to tail.}
}
\value{
boolean
}
\description{
Issue commands within a single transaction block.
}
\examples{
\dontrun{
transaction(
.data = mtcars,
.dbcon = rs$con,
.function_sequence = list(
 function(...) { rs_create_table(table_name = "mtcars", ...) },
 function(...) { rs_upsert_table(table_name = "mtcars", ...) },
 function(...) { rs_replace_table(table_name = "mtcars", ...) }
 )
)
}

}
