logit = function(x) log(x) - log1p(-x)

#' Summary statistics for each day of the duration estimate
#' 
#' @param estimate The estimate to use, either "hakki" or "original"
#' @param summary_fun The function to use to summarise the posterior samples
#' @param ... Additional arguments to pass to the summary function
#' @importFrom rlang .data
#' @export
ataccc_posterior_summary_stats = function(estimate = "hakki", summary_fun = ggdist::mean_qi, ...) {
    ataccc_posterior_samples(estimate) |>
        group_by(.data[["time"]]) |>
        summary_fun(...)
}

#' The posterior mean of the survival time for each posterior sample
#' 
#' @param estimate The estimate to use, either "hakki" or "original"
#' @importFrom rlang .data
#' @export 
ataccc_posterior_means = function(estimate = "hakki") {
    ataccc_posterior_samples(estimate) |>
    group_by(.data[[".draw"]]) |>
    summarise(Mean_surv = sum(.data[["S"]]))
}

#' @importFrom stats rbeta
#' @importFrom rlang .data
logit_hazard_matrix = function(estimate) {
    ataccc_posterior_samples(estimate) |>
        filter(between(.data[["time"]], 1, 40)) |>
        mutate(lambda = if_else(.data[["lambda"]] <= 0, rbeta(n(), 0.5, 1e7), .data[["lambda"]])) |>
        pivot_wider(id_cols = .data[[".draw"]], values_from = .data[["lambda"]], names_from = .data[["time"]]) |>
        select(!.data[[".draw"]]) |>
        as.matrix() |>
        logit()
}

#' A vector of the mean of logit(hazard), starting from time 1
#' 
#' @param estimate The estimate to use, either "hakki" or "original"
#' @export
ataccc_logit_hazard_mean = function(estimate = "hakki") {
    logit_hazard_matrix(estimate) |>
        colMeans()
}

#' A vector of the covariance of logit(hazard), starting from time 1
#' 
#' @param estimate The estimate to use, either "hakki" or "original"
#' @export
ataccc_logit_hazard_covar = function(estimate = "hakki") {
    logit_hazard_matrix(estimate) |>
        stats::cov()
}