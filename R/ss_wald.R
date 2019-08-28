#' @title sample size calcukation for difference in binomial proportions
#' @description sample size calcukation for difference in binomial proportions
#' using Wald method
#' @param alpha numeric, desired alpha level (one-sided)
#' @param power numeric, desirerd power level
#' @param pc numeric, assumed proportion of events in group 'c'
#' @param pt numeric, assumed proportion of events in group 't'
#' @param m2 numerric, pre-specified margin
#' @return numeric
#' @details function returns sample size per group assumimg 1:1 allocation ratio
#' @examples
#' ss_wald(0.025, 0.8, 0.8, 0.8, 0.1)
#' @rdname ss_wald
#' @export
ss_wald <- function(alpha, power, pc, pt, m2){

  n_arm <- floor((qnorm(1-alpha) + qnorm(power))^2*(pc*(1 - pc) + pt*(1 - pt))/(pc - pt - m2)^2)

  return(n_arm)
}
