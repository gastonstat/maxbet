#' @title Dataframe of Scores from maxbet object
#' 
#' @description 
#' This function constructs a dataframe of scores
#' from a \code{"maxbet"} object containing a list of models 
#' (i.e. factorization results)
#' 
#' @param maxbet_obj maxbet multiblock model
#' @return A dataframe (ready to use with ggplot)
#' @seealso \code{\link{get_loadings}}
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
#' wmbc = MAXBET(wines, wine_blocks, 2, specs=NULL)
#' get_scores(wmbc)
get_scores <- function(maxbet_obj) 
{
  mb_model = maxbet_obj$model
  num_dims = length(mb_model)
  labels = maxbet_obj$all_names
  num_blocks = length(labels$block_names)
  num_obs = length(labels$obs_names)
  
  # list with scores
  scores_list = vector("list", num_dims)
  for (i in 1:num_dims) {
    scores_list[[i]] = mb_model[[i]]$Left
  }
  # scores in data frame
  scores_df = data.frame(
    rep(labels$obs_names, num_blocks),
    rep(labels$block_names, each = num_obs),
    do.call("cbind", lapply(scores_list, as.vector))
  )
  # add names
  names(scores_df) = c("Name", "Block", labels$dim_names)
  rownames(scores_df) = 1:nrow(scores_df)
  # output
  scores_df
}


#' @title Dataframe of Scores from models list
#' 
#' @description 
#' Internal function that constructs a dataframe of scores
#' from a \code{"maxbet"} object containing a list of models 
#' (i.e. factorization results)
#' 
#' @param maxbet_obj maxbet multiblock model
#' @param dims vector of dimensions
#' @return A dataframe ready for autoplot ggplot
#' @export
#' @keywords internal
scores_to_dataframe <- function(maxbet_obj, dims)
{
  # build data frame
  scores_df = get_scores(maxbet_obj)
  # select scores according to dims
  scores_df[,c(1, 2, dims + 2)]
}





scorify <- function(maxbet_obj, dims)
{
  mb_model = maxbet_obj$model
  num_dims = length(mb_model)
  labels = maxbet_obj$all_names
  num_blocks = length(labels$block_names)
  num_obs = length(labels$obs_names)
  
  # list with scores
  scores_list = vector("list", num_dims)
  for (i in 1:num_dims) {
    scores_list[[i]] = mb_model[[i]]$Left
  }
  # select scores according to dims
  all_scores = do.call("cbind", lapply(scores_list, as.vector))
  # scores in data frame
  scores_df = data.frame(
    rep(labels$obs_names, num_blocks),
    rep(labels$block_names, each = num_obs),
    all_scores[,dims]
  )
  # add names
  names(scores_df) = c("Name", "Block", labels$dim_names[dims])
  rownames(scores_df) = 1:nrow(scores_df)
  # output
  scores_df
}
