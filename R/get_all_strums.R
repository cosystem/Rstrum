#' Get all strumming patterns
#'
#' @param notetype Type of note. Currently supports 4 (1/4), 8 (1/8) and 16 (1/16) notes
#' @param n_min Minimal number of beats
#' @param dlm Delimiter for printing
#' @return A tibble containing all beats and strums. First column is beats. Second column is the associated strums
#' @examples
#' get_all_strums(8, 4, '|')
#' get_all_strums(16, 7, '|')

get_all_strums <- function(notetype, n_min, dlm) {
  make_strum <- function(type, n) replicate(n %/% 2, type)

  make_beats <- function(n) {
    inner <- function(init, out) {
      if (init == n %/% 2) paste(out, '+')
      else inner(init+1, paste(out, '+', as.character(init+1)))
    }
    paste(unlist(str_split(inner(1,'1'), ' ')), collapse = '|')
  }


  combine_strum <- function(downs, ups) {
    unlist(str_split(paste(downs, ups), ' '))
  }

  get_blank_pos <- function(notetype, n_min) {
    purrr::flatten(lapply(c(1:(notetype-n_min)), function(n) combn(c(1:notetype), n, simplify = F)))
  }

  add_blank <- function(L, pos_list) {
    lapply(pos_list, function(pos) multi_replace(L, " ", pos))
  }

  blank_pos <- get_blank_pos(notetype, n_min)

  downs <- make_strum("D", notetype)
  ups <- make_strum("U", notetype)

  strums <- combine_strum(downs, ups)
  beats <- make_beats(notetype)

  add_dlm <- function(L, dlm) {
    sapply(L, function(l) paste(l, collapse = dlm))
  }

  printable_strums <- strums %>% add_blank(blank_pos) %>% add_dlm(dlm)
  printable_beats <- replicate(length(printable_strums), beats %>% add_dlm(dlm))

  tibble(beat=printable_beats, strum=printable_strums)

}
