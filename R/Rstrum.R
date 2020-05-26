library("stringr")
library("tibble")
library("dplyr")

multi_replace <- function(v, r, ks, split=TRUE, dlm='') {
  if(length(v) == 1 & split) {
    vs <- unlist(strsplit(v, dlm))
  } else {
    vs <- v
  }
  ks_i <- as.integer(ks)
  if (length(ks_i) == 0) {
    v
  } else if (length(ks_i) == 1) {
    if (ks_i[[1]] > length(vs) | ks_i[[1]] < 1) {
      stop("Invalid parameter: ks=", as.character(ks_i[[1]]), ". Valid range: 1-", as.character(length(v)))
    } else if (ks_i[[1]] == 1) {
      c(r, vs[-1])
    } else if (ks_i[[1]] == length(vs)) {
      c(vs[-length(vs)], r)
    } else {
      c(vs[1:(ks_i[[1]]-1)], r, vs[(ks_i[[1]]+1):length(vs)])
      }
  } else {
    multi_replace(multi_replace(vs, r, ks_i[[1]]), r, ks_i[-1])
  }
}



multi_replace_v <- function(v, r, ks) {
  ks <- as.integer(ks)
  if (length(ks) == 0) {
    v
  } else if (length(ks) == 1) {
    if (ks[[1]] > length(v) | ks[[1]] < 1) {
      stop("Invalid parameter: ks=", as.character(ks[[1]]), ". Valid range: 1-", as.character(length(v)))
    } else if (ks[[1]] == 1) {
      c(r, v[-1])
    } else if (ks[[1]] == length(v)) {
      c(v[-length(v)], r)
    } else {
      c(v[1:(ks[[1]]-1)], r, v[(ks[[1]]+1):length(v)])
      }
  } else {
    multi_replace_v(multi_replace_v(v, r, ks[[1]]), r, ks[-1])
  }
}

multi_replace_s <- function(s, r, ks) paste0(multi_replace_v(unlist(strsplit(s, '')), r, ks), collapse = '')

multi_replace <- function(v_or_s, r, ks) {
  if (length(v_or_s) == 1) {
    multi_replace_s(v_or_s, r, ks)
  } else if (length(v_or_s) > 1) {
    multi_replace_v(v_or_s, r, ks)
  } else {
    NULL
  }
}


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



