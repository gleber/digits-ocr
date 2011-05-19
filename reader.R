library('biOps')
library('cluster')
library('clue')
library('knnflex')
library('corpcor')

docr.read.labels <- function(filename="train-labels-idx1-ubyte", limit=1000) {
  f = file(filename, "rb")
  magic = readBin(f, integer(), signed=FALSE, n=3, size=1)
  stopifnot(c(0,0,8) == c(0, 0, 8))
  type = readBin(f, integer(), signed=FALSE, n=1, size=1)
  stopifnot(type == 1)
  count = readBin(f, integer(), signed=FALSE, n=1, size=4, endian="big")
  count = min(count, limit)
  readBin(f, integer(), signed=FALSE, n=(count), size=1, endian="big")
}

docr.read.images <- function(filename="train-images-idx3-ubyte", limit=1000) {
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

distmat <- function(x, y, f) {
  lx = length(x)
  ly = length(y)
  dd = matrix(0, ncol=lx, nrow=ly)
  for (i in 1:lx) {
    for (j in 1:ly) {
      dd[j,i] = f(x[[i]], y[[j]])
    }
  }
  dd
}

contourify <- function(img, k=40) {
  id = imagedata(img)
  idc = imgCanny(id, 1.2)
  c = which(idc==0, arr.ind=TRUE)
  ## plot(c)
  l = dim(c)[1]
  if (l > k) {
    ## print(dim(as.matrix(dist(c))))
    smallified = pam(dist(c), k=k)$medoids
  } else {
    smallified = 1:k
    c = c[rep(seq(nrow(c)), length.out=k), ]
  }
  dim(smallified) = k
  c[smallified,]
}

docr.test <- function(cl, limit=200) {
  lbls = docr.read.labels("t10k-labels-idx1-ubyte", limit=limit)
  digits = docr.read.images("t10k-images-idx3-ubyte", limit=limit)
  good = 0
  bad = 0
  for (i in 1:limit) {
    cor.lbl = lbls[[i]]
    dig = digits[[i]]
    r = docr.predict(cl, dig)
    pr.lbl = which.max(unlist(r)) - 1
    if (cor.lbl == pr.lbl) {
      good = good + 1
    } else {
      bad = bad + 1
    }
    print(c(good, bad, good / (good + bad)))
  }
}

docr.learn <- function(digits=NULL, lbls=NULL, limit=200, k=3, contours=NULL, estimators=NULL, cache=TRUE) {
  if (cache) {
    print("Using cache")
    if (is.null(digits) && exists("docr.last.digits") && length(docr.last.digits) == limit) {
      digits = docr.last.digits
    }
    if (is.null(lbls) && exists("docr.last.lbls") && length(docr.last.lbls) == limit) {
      lbls = docr.last.lbls
    }
    if (is.null(contours) && exists("docr.last.contours") && length(docr.last.contours) == limit) {
      contours = docr.last.contours
    }
    if (is.null(estimators) && exists("docr.last.estimators") && length(docr.last.estimators) == limit) {
      estimators = docr.last.estimators
    }
  }

  if (is.null(digits) || is.null(lbls)) {
    digits = docr.read.images(limit=limit)
    lbls = docr.read.labels(limit=limit)
    docr.last.lbls <<- lbls
    docr.last.digits <<- digits
  }
  stopifnot(length(lbls) == length(digits))
  stopifnot(limit == length(digits))
  if (is.null(estimators)) {
    if (is.null(contours)) {
      print("Contourifying")
      contours = lapply(digits, contourify)
      print("done")
      docr.last.contours <<- contours
    }
    gcntr <<- 0
    print("Creating estimators")
    estimators = lapply(contours, create.estimator)
    print("done")
    rm("gcntr")
    docr.last.estimators <<- estimators
  }
  pr = split.and.prototype(estimators, lbls, k=k)
  docr.last.prototypes <<- pr
  cl = docr.prepare.classifier(pr)
  docr.last.classifier <<- cl
  invisible(cl)
}

docr.prepare.classifier <- function(prototypes) {
  n = names(prototypes)
  labels = list()
  pse = list()
  for (lbl in n) {
    for (p in prototypes[[lbl]]) {
      labels[[length(labels)+1]] = lbl
      pse[[length(pse)+1]] = p
    }
  }
  dist = create.shapes.distmat(pse)
  list(as.dist(dist), pse, 1:(length(pse)), labels)
}

docr.predict <- function(classifier, img, k=10) {
  cont = contourify(img)
  se = create.estimator(cont)

  dm = classifier[[1]]
  tc = classifier[[2]]
  tr = classifier[[3]]
  tl = classifier[[4]]
  edm = rbind(cbind(as.matrix(dm), 0), 0)
  mh = dim(edm)[1]
  mw = dim(edm)[2]
  for (i in 1:(mw-1)) {
    edm[i,mh] <- edm[mh,i] <- estimator.distance(tc[[i]], se)
  }
  knn.probability(tr, dim(edm)[1], unlist(tl), edm, k=k)
}

split.and.prototype <- function(estimators, lbls, k=5) {
  spl = split(estimators,lbls)
  for (i in names(spl)) { print(c(i, length(spl[[i]]))) }
  prototypes = list()
  for (key in names(spl)) {
    curdig = spl[[key]]
    estimators.distmat = create.shapes.distmat(curdig)
    meds = pam(as.dist(estimators.distmat), k)$medoids
    prototypes[[key]] = curdig[meds]
  }
  docr.last.prototypes <<- prototypes
  invisible(prototypes)
}

create.estimator <- function(c) {
  res = list()
  l = (dim(c)[1])
  if (exists("gcntr")) {
    print(gcntr)
    gcntr <<- gcntr + 1
  }
  d = dist(c)
  docr.last.est.dist <<- d
  a = mean(d)
  center = colMeans(c)
  print(center)
  for (i in 1:l) {
    res[[i]] = list(c[i,], create.shape.context(i, c, a, center=center))
  }
  res
}

create.shape.context <- function(i, c, alpha=NULL, center=NULL) {
  ad = 0
  x = c[i,]
  if (is.null(alpha)) {
    d = dist(c)
    alpha = mean(d)
  }
  if (!is.null(center)) {
    ad = center - x
    ad = atan2(ad[2], ad[1])
  }
  ## print(ad)
  rel = t(t(c) - x)
  conv = function(r) {
    a = (atan2(r[2], r[1]) - ad + 2*pi) %% (2*pi)
    d = sqrt(sum((r^2))) / alpha
    c(a, log10(d))
  }
  res = t(apply(rel, 1, conv))
  docr.last.shc.d <<- res
  h = myhist2d(res,
    nbins=c(12,5),
    x.range=c(0,2*pi),
    y.range=c(log10(0.125), log10(2)),
    show=FALSE)
  h / sum(h)
}

create.shapes.distmat <- function(shest) {
  l = length(shest)
  mm = l * (l+1) / 2
  dd = matrix(0, ncol=l, nrow=l)
  l = length(shest)
  c = 0
  for (i in 1:l) {
    for (j in i:l) {
      ## print(c(i,j))
      c = c + 1
      ## print(c(c,mm))
      dd[i,j] <- dd[j,i] <- estimator.distance(shest[[i]], shest[[j]])
    }
  }
  dd
}

match.contour.points <- function(ae, be, ae.coords=NULL, be.coords=NULL, only.ind=TRUE, df=shape.context.distance, df.args=list()) {
  l = length(ae)
  dd = matrix(0, ncol=l, nrow=l)
  for (i in 1:l) {
    for (j in i:l) {
      d = do.call(df, c(list(ae[[i]], be[[j]]), df.args))
      ## d = d + sqrt(sum((ae[[i]][[1]] - be[[j]][[1]])^2))
      dd[i,j] <- dd[j,i] <- d
    }
  }
  docr.last.medm <<- dd
  ass = solve_LSAP(dd)
  docr.last.ass <<- ass
  if (only.ind) {
    return(ass)
  }
  if (is.null(ae.coords)) {
    ae.coords = matrix(unlist(sapply(ae, "[", 1)), ncol=2, byrow=TRUE)
  }
  if (all(unlist(ass) == 1:l)) {
    return(list(ae.coords, ae.coords))
  }
  if (is.null(be.coords)) {
    be.coords = matrix(unlist(sapply(be, "[", 1)), ncol=2, byrow=TRUE)
  }
  be.assed = be.coords[ass,]
  return(list(ae.coords, be.assed))
}

estimator.distance <- function(ae, be) {
  ae.coords = matrix(unlist(sapply(ae, "[", 1)), ncol=2, byrow=TRUE)
  be.coords = matrix(unlist(sapply(be, "[", 1)), ncol=2, byrow=TRUE)
  ae.center = colMeans(ae.coords)
  be.center = colMeans(be.coords)

  match = match.contour.points(ae, be, only.ind=FALSE, df.args=list(ae.center, be.center))
  ## ae.coords = match[[1]]
  be.assed = match[[2]]
  if (all(ae.coords == be.assed)) {
    return(0);
  }
  p = ae.coords
  q = be.assed
  ## print(cbind(p, q))
  pp = cbind(1, p)
  qq = cbind(1, q)
  qqp = pseudoinverse(qq)
  aa = t(qqp %*% pp)
  o = colMeans(p - q)

  be.trans = t(aa %*% t(cbind(1, p)) + o)[,2:3]
  docr.last.be.trans <<- be.trans
  mean(sqrt(rowSums((ae.coords - be.trans) ^ 2)))
  ## ifelse(r < (.Machine$double.eps * 10 * l), 0, r)
}

shape.context.distance <- function(a, b, a.center, b.center) {
  a.d = a.center - a[[1]]
  b.d = b.center - b[[1]]
  a.a = atan2(a.d[2], a.d[1])
  b.a = atan2(b.d[2], b.d[1])

  Ctan = 0.5 * (1 - cos(a.a - b.a))
  
  a = a[[2]]
  b = b[[2]]
  d = (a-b)^2 / (a+b)
  d[is.nan(d)] <- 0
  Csc = 0.5 * sum(d)

  0.9 * Csc + 0.1 * Ctan
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
