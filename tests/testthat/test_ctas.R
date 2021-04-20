context("ctas()")

test_that(
  "ctas works for temp table", {
  ttn <- temp_table_name()
  zapieR::rs_db("select 1 as foo") %>%
    ctas(ttn, temp = TRUE)
  expect_equivalent(zapieR::rs_db(ttn) %>%
                      collect, data.frame(foo=1L))
  DBI::dbExecute(zapieR::rs_db()$con, glue::glue("drop table {ttn}"))
})

test_that(
  "ctas works for non-temp table", {
    ttn <- temp_table_name()
    zapieR::rs_db("select 1 as foo") %>%
      ctas(ttn, temp = FALSE)
    expect_equivalent(zapieR::rs_db(ttn) %>%
                        collect, data.frame(foo=1L))
    DBI::dbExecute(zapieR::rs_db()$con, glue::glue("drop table {ttn}"))
  })

test_that(
  "ctas works for view", {
    ttn <- temp_table_name()
    ttvn <- temp_table_name()
    zapieR::rs_db("select 1 as foo") %>%
      ctas(ttn, temp = FALSE)
    zapieR::rs_db("select * from {{ttn}}", list(ttn=ttn)) %>%
      ctas(ttvn, temp = FALSE, view = TRUE)
    expect_equivalent(zapieR::rs_db("select * from {{ttvn}}", list(ttvn=ttvn)) %>%
                        collect, data.frame(foo=1L))
    DBI::dbExecute(zapieR::rs_db()$con, glue::glue("drop table {ttn} CASCADE"))
    DBI::dbExecute(zapieR::rs_db()$con, glue::glue("drop view if exists {ttvn}"))
  })

test_that(
  "ctas works for in_schema", {
    zapieR::rs_db("select * from iris") %>%
      ctas(dbplyr::in_schema("staging","redshiftToolsIris"), temp = FALSE)
    expect_gte(zapieR::rs_db("Select count(*) as n from staging.redshiftToolsIris") %>% pull(n),1)
  })


