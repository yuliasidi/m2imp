#' @title Runs MI via norm package
#' @description Runs MI via norm package
#' @param dt_in tibble, to be imputed
#' @param num_m numeric, Default: 5, number of imputations
#' @param i numeric, the seed is defined as 666*i
#' @param n_iter numeric, number of iterations to be used in data
#' augmentation procedure
#' @return tibble of MI summary: qbar, ubar, b, v and t
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stats]{cor}}
#' @rdname norm_run
#' @export
#' @import norm
#' @import dplyr
#' @importFrom  tibble tibble as_tibble
#' @importFrom purrr pmap_dfr
#' @importFrom stats var
norm_run <- function(dt_in, num_m = 5, i, n_iter){

  norm::rngseed(seed = 666*i)
  data1 <- as.matrix(dt_in%>%
                       dplyr::select(x, lambda))
  s <- norm::prelim.norm(data1)


  imp_n <- tibble::tibble(i = seq(1, num_m, 1))

  # if (chain == 'one') {

  thetahat <- norm::em.norm(s, showits = FALSE)
  thetahat <- norm::da.norm(s, thetahat, steps = 1, showits = FALSE)

  xx <- 2
  diff <- matrix(NA_real_,nrow = n_iter,ncol = 4)
  diff[1,c(1,2)] <- 100
  thetahat_old <- thetahat

  while( xx < n_iter & (diff[xx-1,1] > 1e-4 || diff[xx-1,2] > 1e-5)){
    thetahat <- norm::da.norm(s, thetahat_old, steps = 1, showits = FALSE)
    param_out_old <- norm::getparam.norm(s, thetahat_old)
    param_out <- norm::getparam.norm(s, thetahat)

    diff[xx,1] <- abs(param_out$mu[2] - param_out_old$mu[2])
    diff[xx,2] <- abs(param_out$sigma[2,2] - param_out_old$sigma[2,2])
    diff[xx,3] <- param_out$mu[2]
    diff[xx,4] <- param_out$sigma[2,2]

    thetahat_old <- thetahat
    xx <- xx + 1
  }

  # return(thetahat)
  # }

  mi_sum <- purrr::pmap_dfr(imp_n, .f=function(i){


    # if (chain == 'multiple') {
    #
    #   thetahat <- norm::em.norm(s, showits = FALSE)
    #   thetahat <- norm::da.norm(s, thetahat, steps = n_iter, showits = FALSE)
    #   return(thetahat)
    # }

    dt_mi <- norm::imp.norm(s, thetahat, data1)%>%
      tibble::as_tibble()%>%
      dplyr::bind_cols(dt_in%>%
                         dplyr::select(-c(lambda, x)))
    dt_mi%>%
      dplyr::summarise(qhat = mean(lambda), u = stats::var(lambda)/dplyr::n())
  },

  .id = "i")

  out <- mi_sum%>%
    dplyr::summarise(qbar = mean(qhat),
                     ubar = mean(u),
                     b = stats::var(qhat))%>%
    dplyr::mutate(t = ubar + (1 + 1/num_m)*b,
                  v = (num_m - 1)*(1 + ubar/(b*(1 + 1/num_m)))^2,
                  n_l = length(dt_in$ph_id))

  out_new <- list(out, diff)%>%purrr::set_names('out', 'diff')
  return(out_new)

}
