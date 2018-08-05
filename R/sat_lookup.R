#' @title saturation look up
#'
#' @description  Look up the saturation value / colour / order
#' @param df enter data frame
#' @param keys key by which to lookup e.g. CHAIN or SATURATION
#' @import tidyverse
#' @import purrr
#' @import magrittr
#' @export sat_lookup
#' @return NULL


sat_lookup = function(df, keys = 'CHAIN'){

  if(keys == 'CHAIN'){
    df %>%
      left_join(

        colours %>%
          dplyr::mutate(CHAIN = gsub("_", ":", CHAIN)) %>%
          distinct(SATURATION, CHAIN,  CHAIN_ORDER, CHAIN_COLOUR) %>%
          dplyr::rename("ORDER" = 'CHAIN_ORDER',
                        "COLOUR" = "CHAIN_COLOUR"),

        by = keys

      ) %>%
      dplyr::mutate(id = 1:n())

  } else if(keys == 'SATURATION'){

    df %>%
      left_join(

        colours %>%
          distinct(SATURATION, SATURATION_ORDER, SATURATION_COLOUR) %>%
          dplyr::rename("ORDER" = 'SATURATION_ORDER',
                        "COLOUR" = "SATURATION_COLOUR"),

        by = keys

      ) %>%
      dplyr::mutate(id = 1:n())

  }

}
