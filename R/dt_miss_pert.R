#' @title Impose missingness
#' @description imposes missingness per each group seperately
#' @param dt tibble, data to impose missingness on
#' @param do_ratec numeric, target drop out rate for group 'c'
#' @param do_ratet numeric, target drop out rate for group 't'
#' @param bxmc numeric, x coefficient for affecting missingness in group 'c'
#' @param bxmt numeric, x coefficient for affecting missingness in group 't'
#' @return tibble
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{n}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{select}}
#'  \code{\link[stats]{Uniform}}
#' @rdname dt_miss_pert
#' @export
#' @import dplyr
#' @importFrom stats runif
dt_miss_pert <- function(dt, do_ratec, do_ratet, bxmc, bxmt){

  am_c <- log(do_ratec/(1 - do_ratec)) - bxmc*mean(dt$x[dt$trt=='c'])
  am_t <- log(do_ratet/(1 - do_ratet)) - bxmt*mean(dt$x[dt$trt=='t'])

  dtmc <- dt%>%
    dplyr::filter(trt=='c')%>%
    dplyr::mutate(pm = 1/(1 + exp(- am_c - bxmc * x)),
                  pthresh = stats::runif(dplyr::n()),
                  r = ifelse(pm > pthresh, 1, 0),
                  y = ifelse(r==0, y, NA))

  dtmt <- dt%>%
    dplyr::filter(trt=='t')%>%
    dplyr::mutate(pm = 1/(1 + exp(- am_t - bxmt * x)),
                  pthresh = stats::runif(dplyr::n()),
                  r = ifelse(pm > pthresh, 1, 0),
                  y = ifelse(r==0, y, NA))

  dtm <- dplyr::bind_rows(dtmc, dtmt)%>%
    dplyr::select(-c(pm, pthresh))

  dtm
}
