#' @title parsing the raw experimental data
#'
#' @description  This combines head with a number of columns
#' @param filename tsv filename
#' @param lipid_class filter data by lipid class e.g. FFA or LPL
#' @param selected_chains filter data by selected_chains e.g. 10:0 etc
#' @param treat_groups filter data by treatment groups
#' @param columns declare columns which contain experimental subject data e.g. M1, M2, etc for mouse 1 etc.
#' @param filter_method apply outlier removal e.g. mf+replacemedian for median filtering and median replacment by default.
#' @param filter_cutoff outlier removal cutoff which is 2 by default
#' @import tidyverse
#' @import purrr
#' @import magrittr
#' @export parse_raw
#' @return NULL


setClass("lipids",

         representation(
           filename = 'character',
           data = 'tbl',
           n = 'numeric',
           treatments = 'vector',
           chains = 'vector',
           filter_method = 'character',
           filter_cutoff = 'numeric'
         )
)


setMethod("show", "lipids",
          function(object){

            cat('File Summary \n')
            cat(" Class                 : ", unique(object@data$TYPE), "\n", sep = "")
            cat(" Array Size            :", nrow(object@data), 'x', ncol(object@data), '\n')
            cat(" Sample Size           :", object@n, '\n')
            cat(" Filter Method         :", object@filter_method, '\n')
            cat(" Filter Cutoff         :", object@filter_cutoff, '\n')
            cat(" Selected Treatments   :", object@treatments, '\n')
            cat(" Selected Chains       :", object@chains, '\n')

          })


parse_raw = function(filename,
                     lipid_class = 'FFA',
                     selected_chains=NULL,
                     treat_groups = NULL,
                     columns,
                     filter_method = "mf+replacemedian",
                     filter_cutoff = 2
){


  default_settings = read.table(file=filename,
                                header=T,
                                stringsAsFactors = F) %>%
    distinct(CHAIN, TREATMENT)


  chains = if(is.null(selected_chains)) unique(default_settings$CHAIN) else selected_chains
  treatments = if(is.null(treat_groups)) unique(default_settings$TREATMENT) else treat_groups

  data = read.table(file=filename, header=T, stringsAsFactors = F) %>%
    as.tbl() %>%
    dplyr::filter(TYPE == lipid_class, CHAIN %in% chains, TREATMENT %in% treatments) %>%
    group_by(CHAIN, REGION, TREATMENT) %>%
    nest() %>%
    dplyr::mutate(data = map(data, function(df) {

      reject_outliers(df, method=filter_method, cutoff=filter_cutoff, columns = columns)

    })) %>%
    unnest(data) %>%
    sat_lookup %>%
    rowwise() %>%
    dplyr::mutate(

      FILT.MEAN = median(sapply(columns, function(i) get(i))),

      FILT.SEM = sd(sapply(columns, function(i) get(i)), na.rm=T)/sqrt(length(columns))

    ) %>%
    arrange(CHAIN, TREATMENT)





  "Create class instance"

  return_object = new('lipids',
                      filename = filename,
                      data = data,
                      n = length(columns),
                      chains =  chains,
                      treatments = treatments,
                      filter_method = filter_method,
                      filter_cutoff = filter_cutoff
  )

  return(return_object)

}
