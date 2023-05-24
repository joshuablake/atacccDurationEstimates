#' @export 
ataccc_posterior_samples = function(estimate = "hakki") {
    switch(
        estimate,
        hakki = hakki_duration,
        original = original_duration
    )
}