n <-  8
n_note <- 3
n_remove <- n - n_note

make_strum <- function(type, n) replicate(n/2, type)

downs <- make_strum("D", 8)
ups <- make_strum("U", 8)

make_beats <- function(n) {
  inner <- function(init, out) {
    if (init == n) paste(out, '+')
    else inner(init+1, paste(out, '+', as.character(init+1)))
  }
  unlist(str_split(inner(1,'1'), ' '))
}

all_combine <- function(v, n) {
  
}


combine_strum <- function(downs, ups) {
  unlist(str_split(paste(downs, ups), ' '))
}

gen_patterns <- function(type, n, minn) {
  n_remove_max <- n - minn
  
} 
out=list()
mystr <- make_beats(8)
for (L in combn(c(1:8), 7, simplify = F)) {
  for (i in L) {
    mystr[[i]] <- ""
    out <- c(out, mystr)
    mystr <- make_beats(8)
  }
}

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


