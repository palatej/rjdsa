if (! isGeneric("saDecomposition" )){
  setGeneric(name="saDecomposition", def = function( object, id, ... ){standardGeneric("saDecomposition")})
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_LP_Properties",
  contains = "JD3_ProcResults"
)


#' Title
#'
#' @param JD3_LP_Properties
#'
#' @return
#' @export
#'
#' @examples
setMethod("show", "JD3_LP_Properties", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    show(proc_data(object@internal, "sweights"))
  }
})

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_X11Plus",
  contains = "JD3_ProcResults"
)

#' Title
#'
#' @param JD3_X11Plus
#'
#' @return
#' @export
#'
#' @examples
setMethod("show", "JD3_X11Plus", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("X11", "\n")
  }
})

#' Title
#'
#' @param JD3_X11Plus
#'
#' @return
#' @export
#'
#' @examples
setMethod("predict", "JD3_X11Plus", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("X11Plus", "\n")
  }
})

#' Title
#'
#' @param JD3_X11Plus
#'
#' @return
#' @export
#'
#' @examples
setMethod("saDecomposition", "JD3_X11Plus", function(object){
  if (is.jnull(object@internal)){
    return (NULL)
  }else{
    y<-proc_data(object@internal, "y")
    sa<-proc_data(object@internal, "d11")
    trend<-proc_data(object@internal, "d12")
    seas<-proc_data(object@internal, "d10")
    irr<-proc_data(object@internal, "d13")
    return (cbind(y, sa, trend, seas, irr))
  }
})

#' Title
#'
#' @param horizon
#' @param degree
#' @param kernel
#' @param endpoints
#' @param ic
#'
#' @return
#' @export
#'
#' @examples
localPolynomialProperties<-function(horizon, degree=3, kernel=c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"), endpoints=c("DAF", "CC", "LC", "QL", "CQ", "CN"), ic=4.5){
  d<-2/(sqrt(pi)*ic)
  kernel=match.arg(kernel)
  endpoints=match.arg(endpoints)
  jrslt<-.jcall("demetra/saexperimental/r/LocalPolynomialFilters", "Ldemetra/saexperimental/r/FiltersToolkit$FiniteFilters;", "filterProperties", as.integer(horizon), as.integer(degree), kernel, endpoints, d)
  new (Class = "JD3_LP_Properties", internal = jrslt)
}

#' Title
#'
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
filter.weights<-function(filter){
  w0<-result(filter, "aweights(0)")
  w<-result(filter, "sweights")
  nw<-length(w)
  len=(nw-1)/2
  colfunc <- colorRampPalette(c("black", "lightgray"))
  cols=colfunc(len)
  plot(w0, type="l", xlim=c(1, nw), ylim=c(min(w0, w), max(w0, w)), col=cols[len])
  for (i in 1:(len-1)){
    name<-paste0("aweights(", i, ")")
    wcur<-result(filter, name)
    lines(wcur, col=cols[len-i])
  }
  lines(w, col="magenta")
}

#' Title
#'
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
filter.gain<-function(filter){
  f<-result(filter, "sweights")
  w0<-result(filter, "again(0)")
  w<-result(filter, "sgain")
  nw<-length(f)
  len=(nw-1)/2
  colfunc <- colorRampPalette(c("black", "lightgray"))
  cols=colfunc(len)
  plot(w0, type="l", ylim=c(min(w0, w), max(w0, w)), col=cols[len])
  for (i in 1:(len-1)){
    name<-paste0("again(", i, ")")
    wcur<-result(filter, name)
    lines(wcur, col=cols[len-i])
  }
  lines(w, col="magenta")
}

#' Title
#'
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
filter.phase<-function(filter){
  f<-result(filter, "sweights")
  w0<-result(filter, "aphase(0)")
  nw<-length(f)
  len=(nw-1)/2
  colfunc <- colorRampPalette(c("black", "lightgray"))
  cols=colfunc(len)
  plot(w0[1:120], type="l", ylim=c(min(0,-1.2*min(w0[1:120])), 1.2*max(w0[1:120])), col=cols[len])
  for (i in 1:(len-1)){
    name<-paste0("aphase(", i, ")")
    wcur<-result(filter, name)
    lines(wcur[1:120], col=cols[len-i])
  }
}

#' Title
#'
#' @param data
#' @param period
#' @param mul
#' @param trend.horizon
#' @param trend.degree
#' @param trend.kernel
#' @param trend.asymmetric.degree
#' @param trend.asymmetric.params
#' @param trend.asymmetric.timeliness
#' @param trend.asymmetric.passBand
#' @param seas.horizon
#' @param seas.kernel
#' @param extreme.lsig
#' @param extreme.usig
#'
#' @return
#' @export
#'
#' @examples
x11.lp<-function(data, period, mul=T, trend.horizon=6, trend.degree=2,
                 trend.kernel=c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov", "Trapezoidal"),
                 trend.asymmetric.degree=0, trend.asymmetric.params=2/(sqrt(pi)*3.5), trend.asymmetric.timeliness=0, trend.asymmetric.passBand=pi/8,
                 seas.horizon=3, seas.kernel=c("Trapezoidal", "Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov"),
                 extreme.lsig=1.5, extreme.usig=2.5){
  tkernel=match.arg(trend.kernel)
  skernel=match.arg(seas.kernel)
  if (is.null(trend.asymmetric.params)){
    jparams=.jnull("[D;")
  }else{
    jparams=.jarray(trend.asymmetric.params)
  }

  jrslt<-.jcall("demetra/saexperimental/r/X11Decomposition", "Ldemetra/saexperimental/r/X11Decomposition$Results;", "lpX11", as.double(data), as.integer(period), mul,
                as.integer(trend.horizon), as.integer(trend.degree), tkernel,
                as.integer(trend.asymmetric.degree), jparams, trend.asymmetric.timeliness, trend.asymmetric.passBand,
                as.integer(seas.horizon), skernel, extreme.lsig, extreme.usig)
  return (new (Class = "JD3_X11Plus", internal = jrslt))
}


#' Title
#'
#' @param data
#' @param period
#' @param mul
#' @param trend.horizon
#' @param trend.degree
#' @param trend.kernel
#' @param rkhs.trend.bandWith
#' @param rkhs.asymmetric.criterion
#' @param rkhs.asymmetric.bandWith
#' @param rkhs.asymmetric.passBand
#' @param seas.horizon
#' @param seas.kernel
#' @param extreme.lsig
#' @param extreme.usig
#'
#' @return
#' @export
#'
#' @examples
x11.rkhs<-function(data, period, mul=T, trend.horizon=6, trend.degree=2,
                 trend.kernel=c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov", "Trapezoidal"),
                 rkhs.trend.bandWith=T, rkhs.asymmetric.criterion=c("FrequencyResponse", "Accuracy", "Smoothness", "Timeliness"), rkhs.asymmetric.bandWith=T,rkhs.asymmetric.passBand=pi/8,
                 seas.horizon=3, seas.kernel=c("Trapezoidal", "Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov"),
                 extreme.lsig=1.5, extreme.usig=2.5){
  tkernel=match.arg(trend.kernel)
  skernel=match.arg(seas.kernel)
  criterion=match.arg(rkhs.asymmetric.criterion)
  if (is.null(trend.asymmetric.params)){
    jparams=.jnull("[D;")
  }else{
    jparams=.jarray(trend.asymmetric.params)
  }

  jrslt<-.jcall("demetra/saexperimental/r/X11Decomposition", "Ldemetra/saexperimental/r/X11Decomposition$Results;", "rkhsX11", as.double(data), as.integer(period), mul,
                as.integer(trend.horizon), as.integer(trend.degree), tkernel, rkhs.trend.bandWith,
                criterion, rkhs.asymmetric.bandWith, trend.asymmetric.passBand,
                as.integer(seas.horizon), skernel, extreme.lsig, extreme.usig)
  return (new (Class = "JD3_X11Plus", internal = jrslt))
}
