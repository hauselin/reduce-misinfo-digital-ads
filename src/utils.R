library(glue)

prop_reached <- c(0.5132, 0.6429, 0.6254)


format_number <- function(x, digits = 2) {
    return(format(round(x, digits), nsmall = digits))
}

format_p <- function(x, digits = 3) {
    if (x < .001) {
        return("p < .001")
    } else {
        pval <- format(round(x, digits), nsmall = digits)
        pval <- gsub("0.", ".", pval, fixed = TRUE)
        return(glue("p = {pval}"))
    }
}

summ_metagen <- function(model, digits = 3) {
    b <- format_number(model$TE.fixed, digits = digits)
    se <- format_number(model$seTE.fixed, digits = digits)
    lowci <- format_number(model$lower.fixed, digits = digits)
    upci <- format_number(model$upper.fixed, digits = digits)
    pval <- format_p(model$pval.fixed, digits = digits)
    glue("b = {b} [{lowci}, {upci}], {pval}")
}

