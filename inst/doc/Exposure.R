params <-
list(my_css = "css/rmdformats.css")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(httptest)
start_vignette("5")

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
#if (!library(ctxR, logical.return = TRUE)){
  devtools::load_all()
#}
old_options <- options("width")

## ----setup-print, echo = FALSE------------------------------------------------
# Redefining the knit_print method to truncate character values to 25 characters
# in each column and to truncate the columns in the print call to prevent 
# wrapping tables with several columns.
#library(ctxR)
knit_print.data.table = function(x, ...) {
  y <- data.table::copy(x)
  y <- y[, lapply(.SD, function(t){
    if (is.character(t)){
      t <- strtrim(t, 25)
    }
    return(t)
  })]
  print(y, trunc.cols = TRUE)
}

registerS3method(
  "knit_print", "data.table", knit_print.data.table,
  envir = asNamespace("knitr")
)

## ----exposure functional use--------------------------------------------------
exp_fun_use <- get_exposure_functional_use(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_fun_use))  %>%
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## -----------------------------------------------------------------------------
exp_fun_use_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_fun_use_prob))

## -----------------------------------------------------------------------------
exp_fun_use_cat <- get_exposure_functional_use_category()

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_fun_use_cat))

## -----------------------------------------------------------------------------
exp_prod_dat <- get_exposure_product_data(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_prod_dat))%>%
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## -----------------------------------------------------------------------------
exp_prod_data_puc <- get_exposure_product_data_puc()

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_prod_data_puc))

## -----------------------------------------------------------------------------
bpa_httk <- get_httk_data(DTXSID = 'DTXSID7020182')
head(bpa_httk)

## -----------------------------------------------------------------------------
exp_list_tags <- get_exposure_list_presence_tags()

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_list_tags))

## -----------------------------------------------------------------------------
exp_list_tags_dat <- get_exposure_list_presence_tags_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(exp_list_tags_dat))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## -----------------------------------------------------------------------------
bpa_general_exposure <- get_general_exposure_prediction(DTXSID = 'DTXSID7020182')
head(bpa_general_exposure)

## -----------------------------------------------------------------------------
bpa_demographic_exposure <- get_demographic_exposure_prediction(DTXSID = 'DTXSID7020182')
bpa_demographic_exposure

## -----------------------------------------------------------------------------
bpa_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')
caf_prob <- get_exposure_functional_use_probability(DTXSID = 'DTXSID0020232')

bpa_caf_prob <- get_exposure_functional_use_probability_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))

## ----echo=FALSE---------------------------------------------------------------
bpa_prob

## ----echo=FALSE---------------------------------------------------------------
caf_prob

## ----echo=FALSE---------------------------------------------------------------
bpa_caf_prob

## ----breakdown, echo = FALSE, results = 'hide'--------------------------------
# This chunk will be hidden in the final product. It serves to undo defining the
# custom print function to prevent unexpected behavior after this module during
# the final knitting process and restores original option values.

knit_print.data.table = knitr::normal_print
  
registerS3method(
  "knit_print", "data.table", knit_print.data.table,
  envir = asNamespace("knitr")
)

options(old_options)

## ----include=FALSE------------------------------------------------------------
end_vignette()

