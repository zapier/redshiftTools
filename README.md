<!-- README.md is generated from README.Rmd. Please edit that file -->

# redshiftTools

This is an R Package meant to easen the uploading of bulk data into
Amazon Redshift.

## Installation

To install this package, you’ll need to execute these commands:

``` r
    install.packages(c('devtools', 'httr'))
    devtools::install_github("RcppCore/Rcpp")
    devtools::install_github("rstats-db/DBI")
    devtools::install_github("rstats-db/RPostgres")
    install.packages("aws.s3", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))
    devtools::install_github("sicarul/redshiftTools")
```

## Usage

You’ll have available now 2 functions: `rs_replace_table` and
`rs_upsert_table`, both of these functions are called with almost the
same parameters, except on upsert you can specify with which keys to
search for matching rows.

For example, suppose we have a table to load with 2 integer columns, we
could use the following code:

``` r
    library("aws.s3")
    library(RPostgres)
    library(redshiftTools)
    
    a=data.frame(a=seq(1,10000), b=seq(10000,1))
    n=head(a,n=10)
    n$b=n$a
    nx=rbind(n, data.frame(a=seq(5:10), b=seq(10:5)))
    
    con <- dbConnect(RPostgres::Postgres(), dbname="dbname",
    host='my-redshift-url.amazon.com', port='5439',
    user='myuser', password='mypassword',sslmode='require')
    
    b=rs_replace_table(a, dbcon=con, tableName='mytable', bucket="mybucket", split_files=4)
    c=rs_upsert_table(nx, dbcon=con, tableName = 'mytable', split_files=4, bucket="mybucket", keys=c('a'))
```

## Changelog

### 0.2.2

-   Updated code to meet {dbplyr}’s change in class from
    `dbplyr::in_schema()`
-   BREAKING CHANGE: standardized function call args from `tableName` to
    `table_name`
-   Updated code wrapped in transaction() so that errors bubble up
-   {redshiftTools} tests had been broken for a while, did some bug
    fixes o that they pass
-   Because we now make good use of schema other than ‘public’ earlier
    introspection code using `pg_table_def` stopped working unless we
    were to update the user’s search path. In order to avoid that, we
    now use `information_schema.columns` for that. :ht: @ Jessica
    Holliday.
