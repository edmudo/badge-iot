LCSS = function(a, b, delta=0, epsilon=1) {
  print(paste(nrow(a), nrow(b)))
  print(a)
  print(b)
  if (all(is.na(a)) || all(is.na(b))) {
    return(0)
  }
  
  # handle when there are missing values
  if (is.na(a[nrow(a),1])) {
    mem.lookup = mem[nrow(a), nrow(b)+1]
    if (mem.lookup != -1) {
      return(mem.lookup)
    } else {
      a = na.trim(a)
      retv = LCSS(a, b, delta, epsilon)
      mem[nrow(a), nrow(b)+1] <<- retv
      return(retv)
    }
  }
  
  if (is.na(b[nrow(b),1])) {
    mem.lookup = mem[nrow(a)+1, nrow(b)]
    if (mem.lookup != -1) {
      return(mem.lookup)
    } else {
      b = na.trim(b)
      retv = LCSS(a, b, delta, epsilon)
      mem[nrow(a)+1, nrow(b)] <<- retv
      return(retv)
    }
  }
  # end handle missing values
  
  if (abs(a[nrow(a), 1] - b[nrow(b), 1]) < epsilon
      && abs(a[nrow(a), 2] - b[nrow(b), 2]) < epsilon
      && abs(nrow(a) - nrow(b)) <= delta) {
    mem.lookup = mem[nrow(a), nrow(b)]
    if (mem.lookup != -1) {
      return(mem.lookup)
    } else {
      retv = 1 + LCSS(a[-nrow(a),,drop=F], b[-nrow(b),,drop=F], delta, epsilon)
      mem[nrow(a), nrow(b)] <<- retv
      return(retv)
    }
  }
  
  mem.lookup.1 = mem[nrow(a), nrow(b)+1]
  mem.lookup.2 = mem[nrow(a)+1, nrow(b)]
  if (mem.lookup.1 != -1) {
    val.1 = mem.lookup.1
  } else {
    val.1 = LCSS(a[-nrow(a),,drop=F], b, delta, epsilon)
    mem[nrow(a), nrow(b)+1] <<- val.1
  }
  if (mem.lookup.2 != -1) {
    val.2 = mem.lookup.2
  } else {
    val.2 = LCSS(a, b[-nrow(b),,drop=F], delta, epsilon)
    mem[nrow(a)+1, nrow(b)] <<- val.2
  }
  return(max(val.1, val.2))
}

LCSS_similarity = function(a, b, delta, epsilon) {
  bound.upper = Vectorize(
    function(x, ref) {
      row_idx = seq(max(x - delta, 1), min(x + delta, nrow(b)))
      max(ref[row_idx] + epsilon, na.rm=T)
    },
    c('x')
  )
  bound.lower = Vectorize(
    function(x, ref) {
      row_idx = seq(max(x - delta, 1), min(x + delta, nrow(b)))
      min(ref[row_idx] - epsilon, na.rm=T)
    },
    c('x')
  )
  print(paste('len', nrow(a), nrow(b)))
  mem <<- matrix(rep(-1, (nrow(a)+1) * (nrow(b)+1)), nrow=nrow(a)+1)
  if (nrow(a) == nrow(b)) {
    mask = (as.data.frame.matrix(cbind(a, b)) %>%
              mutate(high.a.x=bound.upper(row_number(), V1), low.a.x=bound.lower(row_number(), V1),
                     high.a.y=bound.upper(row_number(), V2), low.a.y=bound.lower(row_number(), V2),
                     mask = V3 <= high.a.x & V3 >= low.a.x & V4 <= high.a.y & V4 >= low.a.y))$mask
    tmp.a <<- a[mask,]
    tmp.b <<- b[mask,]
    # print(paste('score', nrow(a[mask,]), nrow(b[mask,]), LCSS(a[mask,], b[mask,], delta, epsilon)))
    LCSS(a[mask,], b[mask,], delta, epsilon)/min(nrow(as.data.frame.matrix(a) %>% drop_na()), nrow(as.data.frame.matrix(b) %>% drop_na()))
  } else {
    # print(paste('score', nrow(a), nrow(b)))
    LCSS(a, b, delta, epsilon)/min(nrow(a), nrow(b))
  }
}