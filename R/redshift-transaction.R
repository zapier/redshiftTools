#' Issue commands within a single transaction block.
#'
#' @param .data a data.frame
#' @param .dbcon a DBI connection
#' @param .function_sequence a list of functions to apply, BEWARE the order matters. They'll be run head to tail.
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' transaction(
#' .data = mtcars,
#' .dbcon = rs$con,
#' .function_sequence = list(
#'  function(...) { rs_create_table(table_name = "mtcars", ...) },
#'  function(...) { rs_upsert_table(table_name = "mtcars", ...) },
#'  function(...) { rs_replace_table(table_name = "mtcars", ...) }
#'  )
#' )
#' }
#'
transaction <- function(.data, .dbcon, .function_sequence) {
  result <- tryCatch(
    {
      message("Beginning transaction")
      if ("pqConnection" %in% class(.dbcon)) {
        DBI::dbBegin(.dbcon)
        warning("pqConnection is going to give you a bad time")
      } else {
        DBI::dbGetQuery(.dbcon, "BEGIN;")
      }

      lapply(.function_sequence, function(.f) {
        .f(.data, .dbcon, use_transaction = FALSE)
      })

      message("Committing changes")
      DBI::dbExecute(.dbcon, "COMMIT;")
      TRUE
    },
    error = function(e) {
      message(e$message)
      DBI::dbExecute(.dbcon, "ROLLBACK;")
      message("Rollback complete")
      result <- FALSE
      attr(result, "error") <- e
      stop(glue("A redshift error occured: {e$message}"))
      return(result)
    }
  )
  if (is.null(result)) {
    stop("A redshift error occured, the result of the transaction was NULL - which is unexpected")
  }
  if (!isTRUE(result)) {
    stop("A redshift error occured, the result of the transaction was FALSE - which is unexpected")
  }
  return(result)
}
