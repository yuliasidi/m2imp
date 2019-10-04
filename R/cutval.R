#' @title calculates a cut-off value to be used in the confidence interval
#' @description calcualtes cut-off value to be used in the confidence itnerval
#' @param alpha numeric, Default: 0.025, a one-sided level of alpha to be used
#' @param df_m2 numeric, degrees of freedom from margin imputation
#' @param lambda numeric, ratio between degrees of freedom between margin
#' imputation and subject level imputation, if subject level data was not
#' imputed, then set lambda as zero
#' @return numeric, cut-off value
#' @examples
#' cutval(0.025, 50, 1)
#' @seealso
#'  \code{\link[stats]{Normal}}
#' @rdname cutval
#' @export
#' @importFrom  stats qnorm
cutval <- function(alpha = 0.025, df_m2, lambda){

  talpha <- stats::qnorm(1-alpha)

  r1 <- (1 + lambda)/16 * (talpha^2 + 5)

  r2 <- (1 + lambda^2)/1536 * (37*talpha^4 + 200*talpha^2 + 171) -
    lambda/256 * (9*talpha^4 - 24*talpha^2 + 7)

  r3 <- (1 + lambda^3)/8192 *(81*talpha^6 + 349*talpha^4 - 293*talpha^2 - 1153) -
    lambda*(1 + lambda)/24576 * (231*talpha^6 - 773*talpha^4 - 499*talpha^2 - 2871)

  val <- talpha* (1 + 1/df_m2*r1 + 1/df_m2^2*r2 + 1/df_m2^3*r3)

  return(val)
}

