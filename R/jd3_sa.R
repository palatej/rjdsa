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

