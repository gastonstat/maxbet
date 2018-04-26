#' @title Get All Names
#' 
#' @description Get names of observations, columns, variables,
#' blocks and dimensions
#' 
#' @param data a numeric matrix or data frame
#' @param blocks a list of vectors indicating the blocks of variables
#' @param maxdim maximum number of dimensions
#' @keywords internal
get_all_names <- function(data, blocks, maxdim) 
{
  obs_names = get_rownames(data)
  col_names = get_colnames(data)
  var_names = get_varnames(col_names, blocks)
  block_names = get_blocknames(blocks)
  dim_names = paste("Dim", 1:maxdim, sep=".")
  # output
  list(
    obs_names = obs_names,
    col_names = col_names,
    var_names = var_names,
    block_names = block_names,
    dim_names = dim_names
  )
}


#' @title Get Row Names
#' 
#' @description Get names of observations
#' 
#' @param dataset a numeric matrix or data frame
#' @keywords internal
get_rownames <- function(dataset) 
{
  if (lacks_rownames(dataset)) {
    row_names = 1L:nrow(dataset)
  } else {
    row_names = rownames(dataset)
  }
  # output
  row_names
}


#' @title Get Column Names
#' 
#' @description Get names of columns
#' 
#' @param dataset a numeric matrix or data frame
#' @keywords internal
get_colnames <- function(dataset) 
{
  if (lacks_colnames(dataset)) {
    col_names = 1L:ncol(dataset)
  } else {
    col_names = colnames(dataset)
  }
  # output
  col_names
}


#' @title Get Varibale Names
#' 
#' @description Get names of variables
#' 
#' @param names_vector (column) names from where to extract selected variables
#' @param block_list a list of vectors indicating the blocks of variables
#' @keywords internal
get_varnames <- function(names_vector, block_list) {
  names_vector[unlist(block_list)]
}


#' @title Get Block Names
#' 
#' @description Get names of blocks
#' 
#' @param block_list a list of vectors indicating the blocks of variables
#' @keywords internal
get_blocknames <- function(block_list) {
  if (lacks_names(block_list)) {
    block_names = paste("Block", 1L:length(block_list), sep="")  
  } else {
    block_names = names(block_list)
  }
  # output
  block_names
}
