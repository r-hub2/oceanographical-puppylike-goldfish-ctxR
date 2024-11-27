params <-
list(my_css = "css/rmdformats.css")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(httptest)
start_vignette("4")

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
if (!library(ctxR, logical.return = TRUE)){
  devtools::load_all()
}
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


## ----ctxR annotation by aeid, message=FALSE-----------------------------------
assay <- get_annotation_by_aeid(AEID = "891")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(assay))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## ----ctxR annotation by aeid batch, message=FALSE-----------------------------
assays <- get_annotation_by_aeid_batch(AEID = c(759,700,891))
# return is in list form by aeid, convert to table for output
assays <- data.table::rbindlist(assays)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(assays))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%", height="400px")

## ----ctxR all assays, message=FALSE, eval=FALSE-------------------------------
#  all_assays <- get_all_assays()

## ----ctxR summary by aeid, message=FALSE--------------------------------------
summary <- get_bioactivity_summary(AEID = "891")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(summary))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## ----ctxR summary by aeid batch, message=FALSE, results= FALSE----------------
summary <- get_bioactivity_summary_batch(AEID = c(759,700,891))

## ----ctxR data by spid, message=FALSE, results = FALSE------------------------
# By spid
spid_data <- get_bioactivity_details(SPID = 'TP0000904H05')

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(spid_data))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%", height="400px")

## ----ctxR data by m4id, message=FALSE, results = FALSE------------------------
# By m4id
m4id_data <- get_bioactivity_details(m4id = 739695)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(m4id_data))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%", height="400px")

## ----ctxR data by dtxsid, message=FALSE, results = FALSE----------------------
# By DTXSID
dtxsid_data <- get_bioactivity_details(DTXSID = "DTXSID30944145")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(dtxsid_data))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%", height="400px")

## ----ctxR data by aeid, message=FALSE, results = FALSE------------------------
# By aeid
aeid_data <- get_bioactivity_details(AEID = 704)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(aeid_data))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%",  height="400px")

## ----ctxR data by aeid batch, message=FALSE, eval=FALSE-----------------------
#  aeid_data_batch <- get_bioactivity_details_batch(AEID = c(759,700,891))

## ----breakdown, echo = FALSE, results = 'hide'--------------------------------
# This chunk will be hidden in the final product. It serves to undo defining the
# custom print function to prevent unexpected behavior after this module during
# the final knitting process

knit_print.data.table = knitr::normal_print
  
registerS3method(
  "knit_print", "data.table", knit_print.data.table,
  envir = asNamespace("knitr")
)

options(old_options)

## ----include=FALSE------------------------------------------------------------
end_vignette()

