#' print strumming patterns
#'
#' Take beats & strums dataframe as input and print each beat and its associated strum to screen
#'
#' @param df Dataframe of beats and strums
#' @return NULL
#' @examples
#' print_strum(beats_strums)
#'
print_strum <- function(df) {
  apply(df, 1, function(line) {
    cat(line[['beat']])
    cat("\n")
    cat(line[['strum']])
    cat("\n")
    cat("\n")
  })
}
