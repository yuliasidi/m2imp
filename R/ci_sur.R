#' @title confidence interval for difference in binomial proportions
#' incorporating clinical experts survey data for clinical non-inferiority
#' margin
#' @description calculates confidence interval for difference in binomial
#' proportions using Wald method, while incorporrating clinical experts survey
#' data in regards to non-inferiority margin
#' @param dt_sur tibble, summarised survey data
#' @param dt_ct tibble, clinical trial data
#' @param type string, Default: c("all", "obs", "mi", "sing"), type of the
#' surrvey data used
#' @return tibble, inlcudes lower/upper confidence interval bounds, and
#' non-inferiority decision
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[bin2mi]{p2_mle}}
#'  \code{\link[tidyr]{unnest}}
#'  \code{\link[stats]{Normal}}
#' @rdname ci_sur
#' @export
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom bin2mi p2_mle
#' @importFrom tidyr unnest
#' @importFrom stats qnorm
ci_sur <- function(dt_sur, dt_ct, type = c('all', 'obs', 'mi', 'sing')){

  out <- dt_sur%>%
    dplyr::mutate(p_sum = purrr::pmap(list(m2 = mean_l), bin2mi::p2_mle, dt = dt_ct))%>%
    tidyr::unnest()%>%
    dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + stats::qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                  ci_l = phat_d - (1 - mean_l)*m1 - stats::qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                  ni_des = ifelse(ci_u < 0, 1, 0),
                  sur = type)

  return(out)
}

