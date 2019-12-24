#' @title multiple imputation to set a margin
#' @description multiply imputes unobserved fraction for preservation of control
#' treatment effect over placebo, which is used to set up a clincal margin (M2)
#' for non-inferiority trial
#' @param dt tibble, contains clinical experts survey information
#' @param num_m numeric, Default: 10, number of imputed datasets
#' @param maxit numeris, Default: 20, number of maximum iterations per
#' imputation
#' @param mi_method characgter, Default: "norm", method used for mice mi
#' @param use_pckg character, 'mice' or 'norm', package to be used in MI
#' @param i numeric, the seed is defined as 666*i (norm package)
#' @param n_iter numeric, Default: 100, number of iterations to be used in data
#' augmentation procedure (norm package)
#' @param chain character, Default 'one', 'one' or 'multiple' chains for data
#' augmentation
#' @return tibble, contains summary of the multiply imputed responses
#' @details the function reads a partially observed survey data, then multiple
#' imputes it using mice with normal model
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stats]{cor}}
#' @rdname m2_mi
#' @export
#' @import mice
#' @importFrom tibble tibble
#' @importFrom purrr pmap_dfr
#' @import dplyr
#' @importFrom stats var
m2_mi <- function(dt, num_m = 10, maxit = 20, mi_method = 'norm',
                  use_pckg = 'mice', i, n_iter = 100, chain = 'one'){

  if (use_pckg == 'mice'){

    predM <- mice::make.predictorMatrix(data=dt)
    predM[, "ph_id"] <- 0
    predM[, "r"] <- 0

    mice_out <- mice::mice(
      data = dt,
      m=num_m,
      maxit = maxit,
      method = mi_method,
      predictorMatrix=predM,
      printFlag = FALSE
    )


    tmp <- tibble::tibble(i = seq(1, num_m,1))

    dt_mice <- purrr::pmap_dfr(tmp, .f=function(i){
      dt <- mice::complete(mice_out, i)
    }, .id = "i")

    out <-
      dt_mice%>%
      dplyr::group_by(i)%>%
      dplyr::summarise(qhat = mean(lambda), u = stats::var(lambda)/dplyr::n())%>%
      dplyr::ungroup()%>%
      dplyr::summarise(qbar = mean(qhat),
                       ubar = mean(u),
                       b = stats::var(qhat))%>%
      dplyr::mutate(t = ubar + (1 + 1/num_m)*b,
                    v = (num_m - 1)*(1 + ubar/(b*(1 + 1/num_m)))^2,
                    n_l = length(dt$ph_id))

  }

  if (use_pckg == 'norm'){

    out <- norm_run(dt_in = dt, num_m = num_m, i = i, n_iter = n_iter,
                    chain = chain)


    }


  return(out)

}

