#' @title Block Model
#' 
#' @description takes a maxbet object and returns the list
#' with model componenets
#' 
#' @param x a \code{"maxbet"} object 
#' @param ... further arguments are ignored
#' @return list with \code{Values}, \code{Left} and \code{Right} vectors
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
#' 
#' # convert to block-model
#' bmodel(mb)
bmodel <- function(x, ...) {
  UseMethod("bmodel", x)  
}

#' @S3method bmodel default
bmodel.default <- function(x, ...)
{
  if (!is(x, "maxbet"))
    stop("\n'bmodel()' requires a maxbet object")
}


#' @S3method bmodel maxbet
bmodel.maxbet <- function(x, ...)
{
  get_model(x$model, x$all_names)
}


# internal function
# convert factorization list into maxbet model
get_model <- function(mb_model, all_names) 
{
  # mb_model is a factorization list for each dimension
  num_dim = length(mb_model)
  num_obs = nrow(mb_model[[1]]$Left)
  num_blocks = length(mb_model[[1]]$values)
  
  ## singular values as blockmatrix
  singular_values = lapply(mb_model, function(x) x$values)
  Values = do.call("rbind", singular_values)
  #  Values = blockmatrix(Values, num_dim, colparts(mb_model[[1]]$Left))
  Values = blockmatrix(Values, rep(1,num_dim), colparts(mb_model[[1]]$Left))
  dimnames(Values) = list(all_names$dim_names, all_names$block_names)
  
  ## Right vectors as blockmatrix
  right_vectors = lapply(mb_model, function(x) x$Right)
  Right = do.call("cbind", right_vectors)
  Right = blockmatrix(Right, rowparts(mb_model[[1]]$Right), num_dim)
  dimnames(Right) = list(all_names$var_names, all_names$dim_names)
  
  ## Left vectors as blockmatrix
  left_vectors = lapply(mb_model, function(x) x$Left)
  # left vectors in matrix format
  Left = matrix(0, num_obs, (num_blocks * num_dim))
  aux = 1
  for (k in 1:num_blocks) {
    for (d in 1:num_dim) {
      Left[,aux] = left_vectors[[d]][,k]
      aux = aux + 1 
    }
  }
  Left = blockmatrix(Left, num_obs, rep(num_dim, num_blocks))
  left_colnames = paste(rep(all_names$block_names, 
                            each = length(all_names$dim_names)),
                        all_names$dim_names, sep = "_")
  dimnames(Left) = list(all_names$obs_names, left_colnames)
  
  # output
  list(Values = Values,
       Left = Left, 
       Right = Right)
}
