#' @title saturation look up
#'
#' @description  Look up the saturation value / colour / order
#' @param df enter data frame
#' @param columns key by which to lookup e.g. CHAIN or SATURATION
#' @param cutoff key by which to lookup e.g. CHAIN or SATURATION
#' @param method key by which to lookup e.g. CHAIN or SATURATION
#' @import tidyverse
#' @import purrr
#' @import magrittr
#' @export reject_outliers
#' @return NULL

reject_outliers = function(df, columns, cutoff = 2, method = 'mf+replacemedian'){

  dff = df[, columns] # needs to become columns

  if (method == 'mf+noreplace'){

    d = abs(dff - median(as.matrix(dff)))
    mdev = median(as.matrix(d))
    s = d/ifelse(!is.null(mdev), mdev, 1)
    dff[s > cutoff] = NA

    df[, columns] = dff
    df

  } else if (method == 'mf+replacemedian'){

    d = abs(dff - median(as.matrix(dff)))
    mdev = median(as.matrix(d))
    s = d/ifelse(!is.null(mdev), mdev, 1)
    filtdf = as.matrix(dff)[s < cutoff]
    dff[s > cutoff] = median(filtdf)

    df[, columns] = dff
    df

  } else if (method == 'none'){

    df

  }
}
