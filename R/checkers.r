#' @title Check Data
#' 
#' @details
#' Internal function. \code{check_data} is called by \code{MAXBET}.
#'
#' @param data numeric matrix or data frame containing the variables.
#' @return validated Data 
#' @keywords internal
#' @export
check_data <- function(data)
{
  if (!is_numeric_tabular(data))
    stop("\n'data' must be a numeric data frame or matrix")

  if (is.data.frame(data))
    data = as.matrix(data)
  
  if (nrow(data) == 1)
    stop("\nCannot work with only one row in 'data'")
  
  if (ncol(data) == 1)
    stop("\nCannot work with only one column in 'data'")
  
  # output
  data
}


#' @title Check well defined blocks
#' 
#' @details
#' Internal function. \code{check_blocks} is called by \code{MAXBET}.
#'
#' @param blocks list defining the blocks of manifest variables. Elements in
#' \code{blocks} must have the same mode: all "numeric" or all "character".
#' @param data matrix or data frame from where to extract manifest variables.
#' @return validated blocks (output in numeric format)
#' @keywords internal
#' @export
check_blocks <- function(blocks, data)
{
  if (!is.list(blocks))
    stop("\n'blocks' must be a list.")
  
  # no duplicated elements within each block
  mvs_duplicated = unlist(lapply(blocks, duplicated))
  if (any(mvs_duplicated))
    stop("\nWrong 'blocks'. Duplicated variables in a block are not allowed")
  
  # all elements in blocks of same mode
  mvs_mode = unique(unlist(lapply(blocks, mode)))
  if (length(mvs_mode) > 1)
    stop("\nAll elements in 'blocks' must have the same mode")
  
  # check indices inside columns range of Data
  if (mvs_mode == "numeric") {
    blocks_in_data = match(unlist(blocks), 1:ncol(data))
    if (any(is.na(blocks_in_data)))
      stop("\nIndices in 'blocks' outside the number of columns in 'data'")
  }
  
  # convert character blocks to numeric blocks
  if (mvs_mode == "character") {
    data_names = colnames(data)
    matched_names = match(unlist(blocks), data_names)
    if (any(is.na(matched_names))) {
      bad_names = unlist(blocks)[is.na(matched_names)]
      stop(sprintf("\nUnrecognized name in 'blocks': '%s'", bad_names))        
    }
    blocks = lapply(blocks, function(x, y) match(x, y), data_names)
  }
  
  # output
  blocks
}


#' @title Check Specifications
#' 
#' @description Check list of specifications
#' 
#' @param specs list of specifications
#' @keywords internal
#' @export
#' @examples
#' # good list of specs
#' specs1 = list(tol = 0.001, maxiter = 20, crit = 1, 
#'    display = TRUE, init = NULL)
#'    
#' # bad list of specs
#' specs2 = list(tol = 0.001, maxter = 20, crit = 1, 
#'    display = TRUE, hello = NULL)
#' 
#' # check_specs(specs1)  # TRUE
#' # check_specs(specs2)  # error
check_specs <- function(specs)
{
  if (is.null(specs)) {
    specs = list(
      tol = 0.001, 
      maxiter = 20, 
      crit = 1, 
      display = FALSE, 
      init = NULL)
  } else {
    if(!is.list(specs))
      stop("\n'specs' must be a list")
    
    # stop if any bad specification names
    spec_names = c("tol", "maxiter", "crit", "display", "init")
    bad_specs <- !(spec_names == names(specs))
    if (any(bad_specs))
      stop(paste("\nUnrecognized specification: ", names(specs)[bad_specs]))
    
    # checking each argument
    if (!is_positive_decimal(specs$tol))
      stop("\n'specs$tol' must be a positive decimal")
    
    if (!is_positive_integer(specs$maxiter))
      stop("\n'specs$maxiter' must be a positive integer")
    
    if (!is.logical(specs$crit))
      stop("\n'specs$crit' must be either TRUE or FALSE")
    
    if (!is.logical(specs$display))
      stop("\n'specs$display' must be either TRUE or FALSE")
    
    if (!is.null(specs$init)) {
      if (!is.numeric(specs$init))
        stop("\n'specs$init' must be numeric")
    }
  }
  # if everything is fine
  specs
}
