library('biOps')
library('cluster')

docr.read.labels <- function(filename="t10k-labels-idx1-ubyte", limit=1000) {
 f = file(filename, "rb")
  magic = readBin(f, integer(), signed=FALSE, n=3, size=1)
  stopifnot(c(0,0,8) == c(0, 0, 8))
  type = readBin(f, integer(), signed=FALSE, n=1, size=1)
  stopifnot(type == 1)
  count = readBin(f, integer(), signed=FALSE, n=1, size=4, endian="big")
  count = min(count, limit)
  all = readBin(f, integer(), signed=FALSE, n=(count), size=1, endian="big")
  a = array(all[1:(1*limit)], c(1,limit))
  l = list()
  for (i in 1:dim(a)[3]) l[[i]] = t(a[,,i])
  l
}

reader <- function(filename="t10k-images-idx3-ubyte", limit=1000) {
  f = file(filename, "rb")
  magic = readBin(f, integer(), signed=FALSE, n=3, size=1)
  stopifnot(c(0,0,8) == c(0, 0, 8))
  type = readBin(f, integer(), signed=FALSE, n=1, size=1)
  stopifnot(type == 3)
  count = readBin(f, integer(), signed=FALSE, n=1, size=4, endian="big")
  count = min(count, limit)
  w = readBin(f, integer(), signed=FALSE, n=1, size=4, endian="big")
  h = readBin(f, integer(), signed=FALSE, n=1, size=4, endian="big")
  all = readBin(f, integer(), signed=FALSE, n=(count*w*h), size=1, endian="big")
  a = array(all[1:(28*28*limit)], c(28,28,limit))
  l = list()
  for (i in 1:dim(a)[3]) l[[i]] = t(a[,,i])
  l
}

contourify <- function(img) {
  id = imagedata(img)
  idc = imgCanny(id, 1.2)
  #edged = array(idc, dim(idc))
  c = which(idc==0, arr.ind=TRUE)
  smallified = pam(dist(c), 30)$medoids
  c[smallified,]
}

docr.learn <- function() {
  digits = reader()
  contours = lapply(digits, contourify)
  gcntr <<- 0
  invisible(lapply(contours, create.estimator))
}

create.estimator <- function(c) {
  res = pairlist()
  l = (dim(c)[1])
  gcntr <<- gcntr + 1
  print(gcntr)
  for (i in 1:l) {
    res[[i]] = list(c[i,], create.shape.context(i, c))
  }
  res
}

create.shape.context <- function(i, c) {
  x = c[i,]
  rel = t(t(c) - x)
  conv = function(r) {
    a = atan2(r[2], r[1])+pi
    d = sqrt(sum((r^2)))
    ## print(r)
    ## print(c(a,d,log(d+1)))
    c(a,log(d+1))
  }
  res = t(apply(rel, 1, conv))
  ## print(cbind(c,rel,res))
  h = myhist2d(res, nbins=c(12,5), x.range=c(0,2*pi), y.range=c(0,3.703719), show=FALSE)
  h / sum(h)
}


estimator.distance <- function(ae, be) {
  dd = matrix(0, ncol=length(ae), nrow=length(be))
  for (i in 1:length(ae)) {
    for (j in i:length(be)) {
      dd[j,i] <- shape.context.distance(ae[[i]][[2]], be[[j]][[2]])
    }
  }
  dd
}

shape.context.distance <- function(a, b) {
  d = (a-b)^2 / (a+b)
  d[is.nan(d)] <- 0
  sum(d)
}


myhist2d <- function (x, y = NULL, nbins = 200, same.scale = FALSE,
                      x.range = NULL, y.range = NULL, na.rm = TRUE,
                      show = TRUE, col = c("black", heat.colors(12)), ...)
{
  if (is.null(y)) {
    if (ncol(x) != 2)
      stop("If y is ommitted, x must be a 2 column matirx")
    y <- x[, 2]
    x <- x[, 1]
  }
  if (length(nbins) == 1)
    nbins <- rep(nbins, 2)
  nas <- is.na(x) | is.na(y)
  if (na.rm) {
    x <- x[!nas]
    y <- y[!nas]
  }
  else stop("missinig values not permitted if na.rm=FALSE")
  if (same.scale) {
    x.cuts <- seq(from = min(x, y), to = max(x, y), length = nbins[1] +
                  1, labels = FALSE)
    y.cuts <- seq(from = min(x, y), to = max(x, y), length = nbins[2] +
                  1, labels = FALSE)
  }
  else {
    if (is.null(x.range)) {
      x.cuts <- seq(from = min(x), to = max(x), length = nbins[1] +
                    1, labels = FALSE)
    } else {
      x.cuts <- seq(from = x.range[1], to = x.range[2],
                    length = nbins[1] + 1, labels = FALSE)
    }
    if (is.null(y.range)) {
      y.cuts <- seq(from = min(y), to = max(y), length = nbins[2] +
                    1, labels = FALSE)
    } else {
      y.cuts <- seq(from = y.range[1], to = y.range[2],
                    length = nbins[2] + 1, labels = FALSE)
    }
  }
  index.x <- cut(x, x.cuts, include.lowest = TRUE)
  index.y <- cut(y, y.cuts, include.lowest = TRUE)
  m <- matrix(0, nrow = nbins[1], ncol = nbins[2],
              dimnames = list(levels(index.x), levels(index.y))
              )
  for (i in 1:length(index.x)) {
    m[index.x[i], index.y[i]] <- m[index.x[i], index.y[i]] + 1
  }
  ## xvals <- x.cuts[1:nbins[1]]
  ## yvals <- y.cuts[1:nbins[2]]
  ## if (show)
  ##   image(xvals, yvals, m, col = col, ...)
  ## list(counts = m, x = xvals, y = yvals)
  m
}
