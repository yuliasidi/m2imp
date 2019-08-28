#' @title sample size calcukation for difference in binomial proportions
#' @description sample size calcukation for difference in binomial proportions
#' using Farrington-Manning method
#' @param alpha numeric, desired alpha level (one-sided)
#' @param power numeric, desirerd power level
#' @param pc numeric, assumed proportion of events in group 'c'
#' @param pt numeric, assumed proportion of events in group 't'
#' @param m2 numerric, pre-specified margin
#' @param pc_r numeric, restricted under the null proportion of events in group
#' 'c'
#' @param pt_r numeric, restricted under the null proportion of events in group
#' 't't
#' @return numeric
#' @details function returns sample size per group assumimg 1:1 allocation ratio
#' @examples
#' ss_fm(0.025, 0.8, 0.8, 0.8, 0.1, 0.78, 0.78 - 0.1)
#' @seealso
#'  \code{\link[stats]{Normal}}
#' @rdname ss_fm
#' @export
#' @importFrom stats qnorm
ss_fm <- function(alpha, power, pc, pt, m2, pc_r, pt_r ){

 n_arm <- floor((stats::qnorm(1-alpha)*sqrt((pc_r*(1-pc_r)+pt_r*(1-pt_r))) +
                       stats::qnorm(power)*sqrt(pc*(1-pc)+pt*(1-pt)))^2/(pc-pt-m2)^2)
  return(n_arm)
}
