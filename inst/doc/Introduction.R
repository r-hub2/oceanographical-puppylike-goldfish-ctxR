params <-
list(my_css = "css/rmdformats.css")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = (nchar(Sys.getenv('CTX_API_KEY')) > 0)
)
library(httptest)
start_vignette("1")

## ----setup, echo=FALSE--------------------------------------------------------
#  if (!library(ctxR, logical.return = TRUE)){
#    devtools::load_all()
#  }

## ----setup-print, echo = FALSE------------------------------------------------
#  # Redefining the knit_print method to truncate character values to 25 characters
#  # in each column and to truncate the columns in the print call to prevent
#  # wrapping tables with several columns.
#  #library(ctxR)
#  knit_print.data.table = function(x, ...) {
#    y <- data.table::copy(x)
#    y <- y[, lapply(.SD, function(t){
#      if (is.character(t)){
#        t <- strtrim(t, 25)
#      }
#      return(t)
#    })]
#    print(y, trunc.cols = TRUE)
#  }
#  
#  registerS3method(
#    "knit_print", "data.table", knit_print.data.table,
#    envir = asNamespace("knitr")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  if (!library(devtools, logical.return = TRUE)){
#    install.packages(devtools)
#    library(devtools)}
#  
#  devtools::install_github("USEPA/ctxR")

## ----echo = FALSE-------------------------------------------------------------
#  my_key <- ctx_key()

## ----eval = FALSE, api-key----------------------------------------------------
#  my_key <- 'YOUR_CTX_API_key'

## ----register-ctxR, eval=FALSE------------------------------------------------
#  # This stores the key in the current session
#  register_ctx_api_key(key = '<YOUR API KEY>')
#  
#  # This stores the key across multiple sessions and only needs to be run once. If the key changes, rerun this with the new key.
#  register_ctx_api_key(key = '<YOUR API KEY>', write = TRUE)

## ----display-hide-key---------------------------------------------------------
#  # To show the API key
#  ctxR_show_api_key()
#  getOption('ctxR')$display_api_key
#  
#  # To hide the API key
#  ctxR_hide_api_key()
#  getOption('ctxR')$display_api_key

## ----ctx-key, eval = FALSE----------------------------------------------------
#  ctx_key()

## ----bpa-chem-details---------------------------------------------------------
#  bpa_details <- get_chemical_details(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_details),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----bpa-chem-info------------------------------------------------------------
#  bpa_info <- get_chem_info(DTXSID = "DTXSID7020182")

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_info),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----bpa-experimental---------------------------------------------------------
#  bpa_info_experimental <- get_chem_info(DTXSID = "DTXSID7020182", type = 'experimental')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_info_experimental),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----hazard-------------------------------------------------------------------
#  bpa_hazard <- get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_hazard_info),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----human-hazard-------------------------------------------------------------
#  bpa_human_hazard <- get_human_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_human_hazard),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----ecotox-hazard------------------------------------------------------------
#  bpa_eco_hazard <- get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_eco_hazard),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----bioactivity-dtxsid-------------------------------------------------------
#  bpa_bioactivity <- get_bioactivity_details(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(bpa_bioactivity),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----bioactivity-aeid---------------------------------------------------------
#  assay_id_search <- get_bioactivity_details(AEID = 42)

## ----echo=FALSE---------------------------------------------------------------
#  htmlTable::htmlTable(head(assay_id_search),
#          align = 'l',
#          align.header = 'l',
#          rnames = FALSE  ,
#          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----breakdown, echo = FALSE, results = 'hide'--------------------------------
#  # This chunk will be hidden in the final product. It serves to undo defining the
#  # custom print function to prevent unexpected behavior after this module during
#  # the final knitting process
#  
#  knit_print.data.table = knitr::normal_print
#  
#  registerS3method(
#    "knit_print", "data.table", knit_print.data.table,
#    envir = asNamespace("knitr")
#  )

## ----include=FALSE------------------------------------------------------------
#  end_vignette()

