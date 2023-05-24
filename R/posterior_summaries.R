logit = function(x) log(x) - log1p(-x)

#' @export
ataccc_posterior_summary_stats = function(estimate = "hakki", summary_fun = ggdist::mean_qi, ...) {
    ataccc_posterior_samples(estimate) |>
        group_by(time) |>
        summary_fun(...)
}

#' @export 
ataccc_posterior_means = function(estimate = "hakki", summary_fun = ggdist::mean_qi, ...) {
    ataccc_posterior_samples(estimate) |>
    group_by(.draw) |>
    summarise(Mean_surv = sum(S))
}

#' @importFrom stats rbeta
logit_hazard_matrix = function(estimate) {
    ataccc_posterior_samples(estimate) |>
        filter(between(time, 1, 40)) |>
        mutate(lambda = if_else(lambda <= 0, rbeta(n(), 0.5, 1e7), lambda)) |>
        pivot_wider(id_cols = .draw, values_from = lambda, names_from = time) |>
        select(!.draw) |>
        as.matrix() |>
        logit()
}

#' A vector of the mean of logit(hazard), starting from time 1
#' @export
ataccc_logit_hazard_mean = function(estimate = "hakki") {
    logit_hazard_matrix(estimate) |>
        colMeans()
}

#' A vector of the covariance of logit(hazard), starting from time 1
#' @export
ataccc_logit_hazard_covar = function(estimate = "hakki") {
    logit_hazard_matrix(estimate) |>
        stats::cov()
}