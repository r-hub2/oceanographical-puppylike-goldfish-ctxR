params <-
list(my_css = "css/rmdformats.css")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(httptest)
start_vignette("2")

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
if (!library(ctxR, logical.return = TRUE)){
  devtools::load_all()
}
old_options <- options("width")

## ----echo=FALSE, warning=FALSE------------------------------------------------
# Used to visualize data in a variety of plot designs
library(ggplot2)
library(gridExtra)

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

## ----ctxR dtxsid data chemical, message=FALSE, eval=FALSE---------------------
# chemical_details_by_dtxsid <- get_chemical_details(DTXSID = 'DTXSID7020182')

## ----ctxR dtxcid data chemical, message=FALSE, eval=FALSE---------------------
# chemical_details_by_dtxcid <- get_chemical_details(DTXCID = 'DTXCID30182')

## ----ctxR batch data chemical, message=FALSE, eval=FALSE----------------------
# vector_dtxsid<- c("DTXSID7020182", "DTXSID9020112", "DTXSID8021430")
# chemical_details_by_batch_dtxsid <- get_chemical_details_batch(DTXSID = vector_dtxsid)
# 
# vector_dtxcid <- c("DTXCID30182", "DTXCID801430", "DTXCID90112")
# chemical_details_by_batch_dtxcid <- get_chemical_details_batch(DTXCID = vector_dtxcid)

## ----ctxr dtxsid check, message=FALSE, eval=FALSE-----------------------------
# dtxsid_check_true <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182')
# dtxsid_check_false <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182f')

## ----ctxr dtxsid check batch, message=FALSE, eval=FALSE-----------------------
# vector_dtxsid_and_non_dtxsid <- c('DTXSID7020182F', 'DTXSID7020182', 'DTXSID0020232F')
# dtxsid_checks <- check_existence_by_dtxsid_batch(DTXSID = vector_dtxsid_and_non_dtxsid)

## ----ctxR property range chemical, message=FALSE, eval=FALSE------------------
# chemical_by_property_range <- get_chemical_by_property_range(start = 1.311,
#                                          end = 1.313,
#                                          property = 'Density')

## ----ctxR info chemical, message=FALSE, eval=FALSE----------------------------
# chemical_info <- get_chem_info(DTXSID = 'DTXSID7020182')

## ----ctxR fate data chemical, message=FALSE, eval=FALSE-----------------------
# fate_by_dtxsid <- get_fate_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----ctxR starting value chemical, message=FALSE, eval=FALSE------------------
# search_starts_with <- chemical_starts_with(word = 'DTXSID70201')

## ----ctxR exact value chemical, message=FALSE, eval=FALSE---------------------
# search_exact <- chemical_equal(word = 'DTXSID7020182')

## ----ctxR substring value chemical, message=FALSE, eval=FALSE-----------------
# search_contains <- chemical_contains(word = 'DTXSID702018')

## ----ctxR mass range ms ready chemical, message=FALSE, eval=FALSE-------------
# msready_by_mass <- get_msready_by_mass(start = 200.9,
#                               end = 200.95)

## ----ctxR chemical formula ms ready chemical, message=FALSE, eval=FALSE-------
# msready_by_formula <- get_msready_by_formula(formula = 'C16H24N2O5S')

## ----ctxR dtxcid ms ready chemical, message=FALSE, eval=FALSE-----------------
# msready_by_dtxcid <- get_msready_by_dtxcid(DTXCID = 'DTXCID30182')

## ----ctxR types of chemical lists, message=FALSE, eval=FALSE------------------
# get_all_list_types()

## ----ctxR all list types chemical, message=FALSE, eval=FALSE------------------
# chemical_lists_by_type <- get_chemical_lists_by_type(type =  'federal')

## ----ctxR list by name chemical, message=FALSE, eval=FALSE--------------------
# public_chemical_list_by_name <- get_public_chemical_list_by_name(listname = 'CCL4')

## ----ctxR lists containing chemical, message=FALSE, eval=FALSE----------------
# lists_containing_chemical <- get_lists_containing_chemical(DTXSID = 'DTXSID7020182')

## ----ctxR chemicals-in-list-start, message=FALSE, eval=FALSE------------------
# chemicals_in_ccl4_start <- get_chemicals_in_list_start(list_name = 'CCL4', word = 'Bi')

## ----ctxR chemicals-in-list-exact, message=FALSE, eval=FALSE------------------
# chemicals_in_ccl4_exact <- get_chemicals_in_list_exact(list_name = 'BIOSOLIDS2021', word = 'Bisphenol A')

## ----ctxR chemicals-in-list-contain, message=FALSE, eval=FALSE----------------
# chemicals_in_ccl4_contain <- get_chemicals_in_list_contain(list_name = 'CCL4', word = 'Bis')

## ----ctxR chemical in list chemical, message=FALSE, eval=FALSE----------------
# chemicals_in_list <- get_chemicals_in_list(list_name = 'CCL4')

## ----ctxR mrv by dtxsid dtxcid chemical, message=FALSE, eval=FALSE------------
# chemical_mrv_by_dtxsid <- get_chemical_mrv(DTXSID = 'DTXSID7020182')
# chemical_mrv_by_dtxcid <- get_chemical_mrv(DTXCID = 'DTXCID30182')

## ----ctxR mol by dtxsid dtxcid chemical, message=FALSE, eval=FALSE------------
# chemical_mol_by_dtxsid <- get_chemical_mol(DTXSID = 'DTXSID7020182')
# chemical_mol_by_dtxcid <- get_chemical_mol(DTXCID = 'DTXCID30182')

## ----ctxR image by dtxsid dtxcid chemical, message=FALSE, eval=FALSE----------
# chemical_image_by_dtxsid <- get_chemical_image(DTXSID = 'DTXSID7020182')
# chemical_image_by_dtxcid <- get_chemical_image(DTXCID = 'DTXCID30182')
# chemical_image_by_smiles <- get_chemical_image(SMILES = 'CC(C)(C1=CC=C(O)C=C1)C1=CC=C(O)C=C1')
# 
# countcolors::plotArrayAsImage(chemical_image_by_dtxsid)
# countcolors::plotArrayAsImage(chemical_image_by_dtxcid)
# countcolors::plotArrayAsImage(chemical_image_by_smiles)

## ----ctxR synonym by dtxsid chemical, message=FALSE, eval=FALSE---------------
# chemical_synonym <- get_chemical_synonym(DTXSID = 'DTXSID7020182')

## -------------------------------------------------------------------------------------------------
options(width = 100)
ccl4_information <- get_public_chemical_list_by_name('CCL4')
print(ccl4_information, trunc.cols = TRUE)

natadb_information <- get_public_chemical_list_by_name('NATADB')
print(natadb_information, trunc.cols = TRUE)

## -------------------------------------------------------------------------------------------------
ccl4 <- get_chemicals_in_list('ccl4')
ccl4 <- data.table::as.data.table(ccl4)

natadb <- get_chemicals_in_list('NATADB')
natadb <- data.table::as.data.table(natadb)

## ----eval=FALSE-----------------------------------------------------------------------------------
# dim(ccl4)
# dim(natadb)
# colnames(ccl4)
# head(ccl4, 1)

## -------------------------------------------------------------------------------------------------
ccl4_phys_chem <- get_chem_info_batch(ccl4$dtxsid)
natadb_phys_chem <- get_chem_info_batch(natadb$dtxsid)

## ----eval=FALSE-----------------------------------------------------------------------------------
# dim(ccl4_phys_chem)
# colnames(ccl4_phys_chem)

## -------------------------------------------------------------------------------------------------
ccl4_phys_chem[, unique(propertyId)]
ccl4_phys_chem[, unique(propType)]

## -------------------------------------------------------------------------------------------------
ccl4_phys_chem[propertyId == 'boiling-point', .(Mean = mean(value))]
ccl4_phys_chem[propertyId == 'boiling-point', .(Mean = mean(value)),
               by = .(propType)]

ccl4_phys_chem[propertyId == 'melting-point', .(Mean = mean(value))]
ccl4_phys_chem[propertyId == 'melting-point', .(Mean = mean(value)),
               by = .(propType)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
head(ccl4_phys_chem[dtxsid == ccl4$dtxsid[[25]], ])
ccl4_phys_chem[dtxsid == ccl4$dtxsid[[25]], .(propType, value, unit),
               by = .(propertyId)]
ccl4_phys_chem[dtxsid == ccl4$dtxsid[[25]], .(value, unit), 
               by = .(propertyId, propType)]

ccl4_phys_chem[dtxsid == ccl4$dtxsid[[25]], .(Mean_value = sapply(.SD, mean)),
               by = .(propertyId, unit), .SDcols = c("value")]
ccl4_phys_chem[dtxsid == ccl4$dtxsid[[25]], .(Mean_value = sapply(.SD, mean)), 
               by = .(propertyId, unit, propType), 
               .SDcols = c("value")][order(propertyId)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_vapor_all <- ccl4_phys_chem[propertyId %in% 'vapor-pressure', 
                                 .(mean_vapor_pressure = sapply(.SD, mean)), 
                                 .SDcols = c('value'), by = .(dtxsid)]
natadb_vapor_all <- natadb_phys_chem[propertyId %in% 'vapor-pressure', 
                                     .(mean_vapor_pressure = sapply(.SD, mean)),
                                     .SDcols = c('value'), by = .(dtxsid)]
ccl4_vapor_grouped <- ccl4_phys_chem[propertyId %in% 'vapor-pressure', 
                                     .(mean_vapor_pressure = sapply(.SD, mean)),
                                     .SDcols = c('value'), 
                                     by = .(dtxsid, propType)]
natadb_vapor_grouped <- natadb_phys_chem[propertyId %in% 'vapor-pressure', 
                                         .(mean_vapor_pressure = 
                                             sapply(.SD, mean)), 
                                         .SDcols = c('value'), 
                                         by = .(dtxsid, propType)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
summary(ccl4_vapor_all)
summary(ccl4_vapor_grouped)
summary(natadb_vapor_all)
summary(natadb_vapor_grouped)

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_vapor_all[, log_transform_mean_vapor_pressure := log(mean_vapor_pressure)]
ccl4_vapor_grouped[, log_transform_mean_vapor_pressure := 
                     log(mean_vapor_pressure)]
natadb_vapor_all[, log_transform_mean_vapor_pressure := 
                   log(mean_vapor_pressure)]
natadb_vapor_grouped[, log_transform_mean_vapor_pressure := 
                       log(mean_vapor_pressure)]

## ----fig.align='center', echo=FALSE, eval=FALSE---------------------------------------------------
# ggplot(ccl4_vapor_all, aes(log_transform_mean_vapor_pressure)) +
#   geom_boxplot() +
#   coord_flip()
# ggplot(ccl4_vapor_grouped, aes(propType, log_transform_mean_vapor_pressure)) +
#   geom_boxplot()

## ----fig.align='center', echo=FALSE, eval=FALSE---------------------------------------------------
# ggplot(natadb_vapor_all, aes(log_transform_mean_vapor_pressure)) +
#   geom_boxplot() + coord_flip()
# ggplot(natadb_vapor_grouped, aes(propType, log_transform_mean_vapor_pressure)) +
#   geom_boxplot()

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_vapor_grouped[, set := 'CCL4']
natadb_vapor_grouped[, set := 'NATADB']

all_vapor_grouped <- rbind(ccl4_vapor_grouped, natadb_vapor_grouped)

vapor_box <- ggplot(all_vapor_grouped, 
                    aes(set, log_transform_mean_vapor_pressure)) + 
                    geom_boxplot(aes(color = propType))
vapor <- ggplot(all_vapor_grouped, aes(log_transform_mean_vapor_pressure)) +
                     geom_boxplot((aes(color = set))) + 
                     coord_flip()

## ----fig.align='center', class.source="scroll-200", echo=FALSE------------------------------------
gridExtra::grid.arrange(vapor_box, vapor, ncol=2)

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_hlc_all <- ccl4_phys_chem[propertyId %in% 'henrys-law', 
                               .(mean_hlc = sapply(.SD, mean)), 
                               .SDcols = c('value'), by = .(dtxsid)]
natadb_hlc_all <- natadb_phys_chem[propertyId %in% 'henrys-law', 
                                   .(mean_hlc = sapply(.SD, mean)), 
                                   .SDcols = c('value'), by = .(dtxsid)]
ccl4_hlc_grouped <- ccl4_phys_chem[propertyId %in% 'henrys-law', 
                                   .(mean_hlc = sapply(.SD, mean)), 
                                   .SDcols = c('value'), 
                                   by = .(dtxsid, propType)]
natadb_hlc_grouped <- natadb_phys_chem[propertyId %in% 'henrys-law', 
                                       .(mean_hlc = sapply(.SD, mean)), 
                                       .SDcols = c('value'), 
                                       by = .(dtxsid, propType)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
summary(ccl4_hlc_all)
summary(ccl4_hlc_grouped)
summary(natadb_hlc_all)
summary(natadb_hlc_grouped)

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_hlc_all[, log_transform_mean_hlc := log(mean_hlc)]
ccl4_hlc_grouped[, log_transform_mean_hlc := log(mean_hlc)]

natadb_hlc_all[, log_transform_mean_hlc := log(mean_hlc)]
natadb_hlc_grouped[, log_transform_mean_hlc := log(mean_hlc)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_hlc_grouped[, set := 'CCL4']
natadb_hlc_grouped[, set := 'NATADB']

all_hlc_grouped <- rbind(ccl4_hlc_grouped, natadb_hlc_grouped)

hlc_box <- ggplot(all_hlc_grouped, aes(set, log_transform_mean_hlc)) + 
  geom_boxplot(aes(color = propType))

hlc <- ggplot(all_hlc_grouped, aes(log_transform_mean_hlc)) +
  geom_boxplot(aes(color = set)) +
  coord_flip()

## ----fig.align='center',class.source="scroll-200", echo=FALSE-------------------------------------
gridExtra::grid.arrange(hlc_box, hlc, ncol=2)

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_boiling_all <- ccl4_phys_chem[propertyId %in% 'boiling-point', 
                                   .(mean_boiling_point = sapply(.SD, mean)), 
                                   .SDcols = c('value'), by = .(dtxsid)]
natadb_boiling_all <- natadb_phys_chem[propertyId %in% 'boiling-point', 
                                       .(mean_boiling_point = 
                                           sapply(.SD, mean)), 
                                       .SDcols = c('value'), by = .(dtxsid)]
ccl4_boiling_grouped <- ccl4_phys_chem[propertyId %in% 'boiling-point', 
                                       .(mean_boiling_point = 
                                           sapply(.SD, mean)), 
                                       .SDcols = c('value'), 
                                       by = .(dtxsid, propType)]
natadb_boiling_grouped <- natadb_phys_chem[propertyId %in% 'boiling-point', 
                                           .(mean_boiling_point = 
                                               sapply(.SD, mean)), 
                                           .SDcols = c('value'), 
                                           by = .(dtxsid, propType)]

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
summary(ccl4_boiling_all)
summary(ccl4_boiling_grouped)
summary(natadb_boiling_all)
summary(natadb_boiling_grouped)

## ----fig.align='center',class.source="scroll-300",message=FALSE-----------------------------------
ccl4_boiling_grouped[, set := 'CCL4']
natadb_boiling_grouped[, set := 'NATADB']

all_boiling_grouped <- rbind(ccl4_boiling_grouped, natadb_boiling_grouped)

boiling_box <- ggplot(all_boiling_grouped, aes(set, mean_boiling_point)) + 
  geom_boxplot(aes(color = propType))
boiling <- ggplot(all_boiling_grouped, aes(mean_boiling_point)) +
  geom_boxplot(aes(color = set)) + 
  coord_flip()

## ----fig.align='center',class.source="scroll-200", echo=FALSE-------------------------------------
gridExtra::grid.arrange(boiling_box, boiling, ncol=2)

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

