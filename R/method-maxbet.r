#' @title MAXBET 
#' 
#' @description Maxbet method
#' 
#' @details
#' The list of specifications must contain the following elements:
#' \code{tol}, \code{maxiter}, \code{crit}, \code{display}, \code{init}
#' 
#' @param data a numeric matrix or data frame
#' @param blocks a list of vectors defining the blocks
#' @param maxdim maximum number of dimensions
#' @param specs list with algorithm specifications
#' @return an object of class \code{"maxbet"} with the following elements:
#' @return \item{model}{list with block model elements}
#' @return \item{iterations}{list of iteration values}
#' @return \item{crietrion}{list of fit criterion values}
#' @return \item{specs}{list with specifications}
#' @return \item{residuals}{blockmatrix of residuals}
#' @return \item{all_names}{list of names}
#' @references 
#' Hanafi M., Ten Berge J.M.F. (2003) Global Optimality
#' of the Successive Maxbet Algorithm. 
#' \emph{Psychometrika}, \bold{68}, No. 1, 97-103.
#' 
#' van de Geer J.P. (1984) Linear relations among k sets of variables.
#' \emph{Psychometrika}, \bold{49}, 79-94.
#' 
#' @export
#' @examples
#' # load wines
#' data(wines)
#' 
#' # center wines
#' wines = scale(wines, scale=FALSE)
#' 
#' # wines as block-matrix
#' wine_blocks = list(1:4, 5:7, 8:11, 12:14)
#' 
#' # apply maxbet method
#' mb = MAXBET(wines, wine_blocks, maxdim=2)
#' mb
#' 
#' # basic summary
#' summary(mb)
MAXBET <- function(data, blocks, maxdim = 2, specs = NULL)
{
  # check arguments
  data = check_data(data)
  blocks = check_blocks(blocks, data)
  specs = check_specs(specs)
  # get names
  all_names = get_all_names(data, blocks, maxdim)
  
  # output containers
  models = vector("list", maxdim)
  iterations = rep(0, maxdim)
  criterion = vector("list", maxdim)
  W_loads = vector("list", maxdim)
  
  # convert data into blockmatrix
  X = blockification(data, blocks)
  
  for (i in 1:maxdim) 
  {
    proc = maxbet_proc(X, specs)
    models[[i]] = proc$factorization
    iterations[i] = proc$iters
    criterion[[i]] = proc$fit_crit
    # deflation
    def = deflation(X, proc$factorization, option="loadings")
    X = def$X_residuals
    W_loads[[i]] = def$W_projection
  }
  # output
  structure(
    list(model = models,
         iterations = iterations,
         criterion = criterion,
         specs = proc$specs,
         residuals = X,
         all_names = all_names),
    class = "maxbet")
}


#' @S3method print maxbet
print.maxbet <- function(x, ...)
{
  cat("\nMaxBet Method\n")
  cat(rep("-", 43), sep="")
  cat("\n$model        ", "block model")
  cat("\n$iterations   ", "iteration values")
  cat("\n$criterion    ", "fit criterion")
  cat("\n$specs        ", "specifications")
  cat("\n$residuals    ", "residuals")
  cat("\n$all_names    ", "list of names\n")
  cat(rep("-", 43), sep = "")
  invisible(x)
}


#' @S3method summary maxbet
summary.maxbet <- function(object, ...) 
{
  init = object$specs$init
  if (is.null(init)) init = "NULL"
  structure(
    list(
      num_obs = length(object$all_names$obs_names),
      num_vars = length(object$all_names$var_names),
      num_blocks = length(object$all_names$block_names),
      num_dims = length(object$all_names$dim_names),
      maxiter = object$specs$maxiter,
      tolerance = object$specs$tol,
      fit_crit = object$specs$crit,
      init = init),
    class = "summary.maxbet")
}


#' @S3method print summary.maxbet
print.summary.maxbet <- function(x, ...)
{
  cat(rep("-", 35), sep="")
  cat("\nModel Information", "\n")
  cat(rep("-", 35), sep="")
  cat("\n")
  cat("1  Procedure (method)   ", "maxbet", "\n")
  cat("2  Num of Observations  ", x$num_obs, "\n")
  cat("3  Num of Variables     ", x$num_vars, "\n")
  cat("4  Num of Blocks        ", x$num_blocks, "\n")
  cat("5  Max Num Dimensions   ", x$num_dims, "\n")
  cat("6  Max Num Iterations   ", x$maxiter, "\n")
  cat("7  Tolerance            ", x$tolerance, "\n")
  cat("8  Fit Criterion        ", x$fit_crit, "\n")
  cat("9  Initialization       ", x$init, "\n")
  
  invisible(x)
}

