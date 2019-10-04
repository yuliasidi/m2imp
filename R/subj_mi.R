#' @title multiple imputation for subject level data
#' @description multiply imputes unobserved subject level data using mice
#' @param dt tibble, subject level data
#' @param num_ms numeric, Default: 10, number of imputed datasets
#' @param maxit numeris, Default: 20, number of maximum iterations per
#' imputation
#' @return tibble, contains summary of the multiply imputed responses
#' @details the function reads a partially observed subject level data,
#' then multiple imputes it using mice with logistic model
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stats]{cor}}
#' @rdname subj_mi
#' @import mice
#' @importFrom tibble tibble
#' @importFrom purrr pmap_dfr
#' @import dplyr
#' @importFrom stats var

subj_mi <- function(dt, num_ms = 10, maxit = 20){

  dtc <- dt%>%dplyr::filter(trt=='c')%>%dplyr::mutate(y = as.factor(y))
  dtt <- dt%>%dplyr::filter(trt=='t')%>%dplyr::mutate(y = as.factor(y))

  predMc <- mice::make.predictorMatrix(data=dtc)
  predMc[, "trt"] <- 0

  predMt <- mice::make.predictorMatrix(data=dtt)
  predMt[, "trt"] <- 0

  mice_outc <- mice::mice(
    data = dtc,
    m=num_ms,
    maxit = maxit,
    method = "logreg",
    predictorMatrix=predMc,
    printFlag = FALSE
  )

  mice_outt <- mice::mice(
    data = dtt,
    m=num_ms,
    maxit = maxit,
    method = "logreg",
    predictorMatrix=predMt,
    printFlag = FALSE
  )

  tmp <- tibble::tibble(i = seq(1, num_ms,1))

  dt_mice <- purrr::pmap_dfr(tmp, .f=function(i){

    dtc_comp <- mice::complete(mice_outc, i)
    dtt_comp <- mice::complete(mice_outt, i)

    dt <- dplyr::bind_rows(dtc_comp, dtt_comp)

  }, .id = "i")

  dt_mice <- dt_mice %>%
    dplyr::mutate(y = as.numeric(y) - 1)

  perimp <- dt_mice%>%
    dplyr::group_by(i,trt)%>%
    dplyr::summarise(phat = mean(y), n_obs = dplyr::n())%>%
    tidyr::spread(key = 'trt', value = 'phat')%>%
    dplyr::rename(phat_c = c, phat_t = t)%>%
    dplyr::mutate(phat_d = phat_c - phat_t,
                  u = phat_c*(1 - phat_c)/n_obs + phat_t*(1 - phat_t)/n_obs)
  out <-
    perimp%>%
    ungroup()%>%
    dplyr::summarise(qbar = mean(phat_d),
                     ubar = mean(u),
                     b = stats::var(phat_d))%>%
    dplyr::mutate(t = ubar + (1 + 1/num_ms)*b,
                  v = (num_ms - 1)*(1 + ubar/(b*(1 + 1/num_ms)))^2)

  return(out)

}
