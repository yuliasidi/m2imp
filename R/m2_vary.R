#' @title Variable margin overall point estimate and variance derivation
#' @description Calculates overall point estimates and its variance using a
#' vector of margins
#' @param lambda numeric, fraction of control treatment effect preservation
#' @param m1 numeric, statistical margin
#' @param pc numeric, estimated proportion of events in group 'c'
#' @param pt numeric, estimated proportion of events in group 't'
#' @param pd_var numeric, estimated variance for difference in propotions
#' between groups 'c' and 't'
#' @param nc numeric, number of observations in group 'c'
#' @param nt numeric, number of observations in group 't'
#' @param method chararcter with the followin two options: "wald", "fm"
#' @return list
#' @details DETAILS
#' @examples
#' lambda <- c(0.60, 0.63, 0.65)
#' m2_vary(lambda, m1 = 0.23, pc = 0.8, pt = 0.7, pd_var = 0.004, nc = 100, nt = 100, method = 'wald')
#' @seealso
#'  \code{\link[stats]{cor}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'  \code{\link[bin2mi]{p_rmle}}
#' @rdname m2_vary
#' @export
#' @importFrom stats var
#' @importFrom purrr map_dbl set_names
#' @importFrom bin2mi p_rmle
m2_vary <- function(lambda, m1, pc, pt, pd_var, nc, nt, method = c('wald', 'fm')){

  m2 <- (1-lambda)*m1

  mean_lambda <- mean(m2)
  var_lambda <- stats::var(m2)
  num_k <- length(m2)

  qbar <- pc - pt - mean_lambda

  if (method == 'wald'){
    ubar <- pd_var + var_lambda
  }

  if (method == 'fm'){
    pc_rmle <- purrr::map_dbl(m2, bin2mi::p_rmle, nt = nt, nc = nc, pc = pc,  pt = pt)
    pt_rmle <- pc_rmle - m2
    uk <- pc_rmle*(1 - pc_rmle)/nc + pt_rmle*(1 - pt_rmle)/nt
    ubar <- mean(uk) + var_lambda
  }

  b <- var_lambda
  t <- ubar + (1 + 1/num_k)*b
  v <- floor((num_k - 1)*(1 + ubar/((1+1/num_k)*b))^2)

  out <- list(qbar, ubar, b, t, v)%>%purrr::set_names("qbar", "ubar", 'b', 't', 'v')

  return(out)
}
