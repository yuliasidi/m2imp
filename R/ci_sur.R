#' @title confidence interval for difference in binomial proportions
#' incorporating clinical experts survey data for clinical non-inferiority
#' margin
#' @description calculates confidence interval for difference in binomial
#' proportions using Wald method, while incorporrating clinical experts survey
#' data in regards to non-inferiority margin
#' @param dt_sur tibble, summarised survey data
#' @param dt_ct tibble, clinical trial data
#' @param type string, Default: c("all", "obs", "mi", "sing"), type of the
#' survey data used
#' @param subj_miss logic, Default: FALSE, detemines whether subject level data
#' are incomplete
#' @param num_ms numeric, Default: 10, number of imputations for subject level
#' data
#' @param x_inc logic, Default: TRUE, indicates whether covariate should
#' be included in the multiple imputation
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
#' @importFrom distr Td q Norm
#' @importFrom purrr pmap
#' @importFrom bin2mi p2_mle
#' @importFrom tidyr unnest
#' @importFrom stats qnorm
ci_sur <- function(dt_sur, dt_ct, type = c('all', 'obs', 'mi', 'sing'), subj_miss = FALSE, num_ms = 10,
                   x_inc = TRUE){

  if (type!='mi'){


    out <- dt_sur%>%
      dplyr::mutate(p_sum = purrr::pmap(list(m2 = mean_l), bin2mi::p2_mle,
                                        dt = dt_ct%>%
                                          dplyr::filter(is.na(y)==FALSE)))%>%
      tidyr::unnest()%>%
      dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + stats::qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                    ci_l = phat_d - (1 - mean_l)*m1 - stats::qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                    ni_des = ifelse(ci_u < 0, 1, 0),
                    sur = type,
                    cutv = stats::qnorm(1-alpha))

  }

  if (type=='mi' & !subj_miss){

    tvar1 <- distr::Norm(0,1)
    tvar2 <- distr::Td(dt_sur$v)
    tvar <- tvar1 - tvar2

    tval <- distr::q(tvar)(1 - alpha)/sqrt(2)

    out <- dt_sur%>%
      dplyr::mutate(p_sum = purrr::pmap(list(m2 = mean_l), bin2mi::p2_mle,
                                        dt = dt_ct))%>%
      tidyr::unnest()%>%
      dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + tval*sqrt(var_d + m1*sd_l^2),
                    ci_l = phat_d - (1 - mean_l)*m1 - tval*sqrt(var_d + m1*sd_l^2),
                    ni_des = ifelse(ci_u < 0, 1, 0),
                    sur = type,
                    cutv = tval)
  }

    if(type=='mi' & subj_miss){

        subj_mi_res <- subj_mi(dt_ct, num_ms = num_ms, x_inc = x_inc)
        tvar1 <- distr::Td(subj_mi_res$v)
        tvar2 <- distr::Td(dt_sur$v)
        tvar <- tvar1 - tvar2

        tval <- distr::q(tvar)(1 - alpha)/sqrt(2)

      out <- dt_sur%>%
        dplyr::mutate(phat_d = subj_mi_res$qbar,
                      var_d = subj_mi_res$t,
                      u_subj = subj_mi_res$t,
                      b_subj = subj_mi_res$b,
                      v_subj = subj_mi_res$v)%>%
        dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + tval*sqrt(var_d + m1*sd_l^2),
                      ci_l = phat_d - (1 - mean_l)*m1 - tval*sqrt(var_d + m1*sd_l^2),
                      ni_des = ifelse(ci_u < 0, 1, 0),
                      sur = type,
                      cutv = tval)

    }

  return(out)
}

