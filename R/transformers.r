#' @title Building block matrix from a data set
#' 
#' @details
#' Internal function \code{blockification}.
#' 
#' @param data matrix from which manifest variables are extracted
#' @param blocks list (with numeric elements) indicating the set of manifest 
#' variables that form each block
#' @return blockmatrix of selected variables
#' @keywords internal
#' @export
blockification <- function(data, blocks)
{
  # building blockmatrix
  blockmatrix(data[,unlist(blocks)], 
              rowparts = nrow(data), 
              colparts = lengths(blocks))
}


# @title Blockmatrix of scores to dataframe
# 
# @description 
# Internal function that converts a Blockmatrix of scores into a dataframe
# 
# @param score_bmatrix blockmatrix of scores
# @param all_names list with elements names
# @return A dataframe ready for ggplots
# @export
# @keywords internal
# @examples
# # create a list of scores
# A = matrix(rnorm(15), 5, 3)
# B = matrix(rnorm(15), 5, 3)
# scores = list(A=A, B=B)
# scores
# 
# # from list to blockmatrix
# scores_to_df(scores)
scores_to_df <- function(score_bmatrix, all_names)
{
  # data frame
  scores_df = data.frame(
    rep(all_names$obs_names, nblocks(score_bmatrix)),
    rep(all_names$block_names, each = nrow(score_bmatrix)),
    do.call("rbind", separate(score_bmatrix))
  )
  # add names
  names(scores_df) = c("Name", "Block", all_names$dim_names)
  rownames(scores_df) = 1:nrow(scores_df)
  # output
  scores_df
}
