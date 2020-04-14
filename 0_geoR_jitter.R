##
## Miscelaneous geoR functions to jitter duplicate coords in a dataframe
## Copied from https://github.com/cran/geoR/blob/master/R/geoRmisc.R
## on 4/22/2019

"jitterDupCoords" <-
  function(x, ...)
  {
    UseMethod("jitterDupCoords")
  }

"jitterDupCoords.default" <-
  function(x, ...){
    ## checking input
    if(!is.matrix(x) && !is.data.frame(x) || ncol(x) != 2)
      stop("jitterCoords.default: coords must be a matrix or data-frame with two columns")
    ind <- dup.coords(x, simplify=FALSE, USE.NAMES=FALSE)
    i <- 1
    while(i <= length(ind)){
      x[ind[[i]],] <- jitter2d(coords=x[ind[[i]],], ...)
      i <- i+1
    }
    return(x)
  }

"jitterDupCoords.geodata" <-
  function(x, ...){
    x$jitter.Random.seed <- .Random.seed
    x$coords <- jitterDupCoords.default(x=x$coords, ...)
    return(x)
  }


"jitter2d" <-
  function(coords, max, min = 0.2*max,
           fix.one = TRUE, which.fix=c("random", "first", "last")){
    ## checking input
    if(!is.matrix(coords) && !is.data.frame(coords) || ncol(coords) != 2)
      stop("jitterCoords: coords must be a matrix or data-frame with two columns")
    nc <- nrow(coords)
    if(mode(which.fix) != "numeric"){
      which.fix <- match.arg(which.fix)
      which.fix <- switch(which.fix,
                          random = sample(1:nc, 1),
                          first = 1,
                          last = nc)
    }
    if(length(which.fix) > 1)
      stop("jitterCoords: which.fix must be a single element vector")
    ## number of coordinates to be jittered
    if(fix.one) nc <- nc-1
    ## random displacements of the coordinates
    angle <- runif(nc, min = 0, max = 2 * pi)
    d <- sqrt(runif(nc, min = min^2, max = max^2))
    coords[-which.fix,] <- coords[-which.fix,] + cbind(d * cos(angle), d * sin(angle))
    return(coords)
  }

"dup.coords" <-
  function(x, ...)
  {
    UseMethod("dup.coords")
  }

"dup.coords.default" <-
  function(x, ...)
  {
    ap1 <- unclass(factor(paste("x",x[,1],"y",x[,2], sep="")))
    ap2 <- table(ap1)
    ap2 <- ap2[ap2 > 1]
    takecoords <- function(n){
      if(!is.null(rownames(x))) rownames(x[ap1 == n,])
      else (1:nrow(x))[ap1 == n]
    }
    res <- sapply(as.numeric(names(ap2)), takecoords, ...)
    if(length(res) == 0) res <- NULL
    if(!is.null(res)) class(res) <- "duplicated.coords"
    return(res)
  }

"dup.coords.geodata" <- "duplicated.geodata" <-
  function(x, incomparables, ...)
  {
    xdf <- as.data.frame.geodata(x)
    dc <- dup.coords.default(x$coords)
    if(is.null(dc)) return(dc)
    else{
      return(data.frame(dup=factor(rep(1:length(dc), sapply(dc, length))),
                        xdf[unlist(dc),]))
    }
  }

