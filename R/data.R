#' Get the posterior samples of the duration estimates
#' 
#' @param estimate The estimate to use, either "hakki" or "original"
#' @export 
ataccc_posterior_samples = function(estimate = "hakki") {
    switch(
        estimate,
        hakki = hakki_duration,
        original = original_duration
    )
}