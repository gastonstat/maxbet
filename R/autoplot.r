#' @title Fortify method for Maxbet
#' 
#' @description Extract information from a MaxBet object into a data.frame
#'
#' @param model an object resulting from MaxBet
#' @param data the original data used to compute MaxBet, 
#' to be concatenated to the output when extracting observations; 
#' when \code{NULL}, the default, the data will be extracted from 
#' the MaxBet object when possible (not for \code{\link{MAXBET}})
#' @param what to extract: \code{"scores"} or \code{"loadings"}
#' @param dims the dimensions to extract; two is usual for plotting
#' @param ... pass-through argument
#'
#' @return
#' A data.frame containing the following elements
#'   \item{Dim#}{the scores (for observations) or loadings (for variables)}
#'   \item{Name}{name of the observation}
#'   \item{Block}{label of the corresponding block}
#' @export
#' @method fortify maxbet
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
#' 
#' # scores
#' fortify(wmbc, what="scores")
#' 
#' # loadings
#' #fortify(wmbc, what="loadings")
fortify.maxbet <- 
function(model, data = NULL, what = "scores", dims = c(1,2), ...) 
{
  # Method for maxbet
  # NB: those objects do not contain the original data, 
  # so data is NULL by default
  
  # Checks
  if (any(dims > length(model$all_names$dim_names))) {
    stop("\nAt least one of the dimensions does not exist")
  }
  if (length(dims) < 1 || length(dims) > 2) {
    stop("\nYou must choose two dimensions")
  }  
  
  # Scores
  if (what == "scores") {
    res = scores_to_dataframe(model, dims)
  }
  else if (what == "loadings") {
    res = loadings_to_dataframe(model, dims)
  }
  
  return(res)
}



#' @title Automatic ggplot for MAXBET
#' 
#' @description autoplot for maxbet objects using \code{ggplot} style
#' 
#' @param object a \code{"maxbet"} object 
#' @param what to plot (\code{"scores"} or \code{"loadings"})
#' @param block whether to plot all blocks (\code{block = NULL}) or 
#' just one of them (eg \code{block = 1})
#' @param mapping a call to aes() specifying additional mappings 
#' between variables and plot aesthetics; 
#' by default, positions in x and y are mapped to the scores 
#'
#' @param data the original data used to compute maxbet, 
#' to be concatenated to the output when extracting observations. 
#' This allows to map original data columns to aesthetics of the plot, 
#' even if those columns were not used in the PCA. 
#' When \code{NULL} the data used in maxbet will be extracted from the maxbet 
#' object, when possible (not for \code{\link{MAXBET}})
#'
#' @param dims the scores to extract; two are necessary to produce a plot
#' @param ... passed to the various geoms; 
#' can be used to \emph{set} further aesthetics
#'
#' @export
#' @method autoplot maxbet
#' @examples
#' # load wines
#' data(wines)
#' wines = scale(wines, scale=FALSE)
#' 
#' # list of blocks
#' blocks = list(Blok1 = 1:4, Blok2 = 5:7, Blok3 = 8:11, Blok4 = 12:14)
#' 
#' # apply maxbet
#' mbwines = MAXBET(wines, blocks=blocks, maxdim = 2)
#' 
#' # plot scores
#' autoplot(mbwines, what="scores") +
#' ggtitle("Scores Plot")
#' 
#' # plot loadings
#' #autoplot(mbwines, what="loadings", mapping=aes(colour=Block)) +
#' #ggtitle("Loadings Plot")
autoplot.maxbet <- function(object, what = "scores", block = NULL, 
         mapping = aes(), data = NULL, dims = c(1, 2), ...) 
{
  # check arguments
  block = check_block_plot(block, object$all_names$block_names)
  if (length(dims) != 2) {
    stop("\nYou must choose exactly two dimensions to plot")
  }
  
  # prepare the appropriate plots
  fort_data <- fortify.maxbet(model=object, data=data, what=what, dims=dims)
#  ggblock(data=fort_data, block=block, mapping=mapping, ...)
  
  # Construct default aesthetic mappings
  Dims = grep("Dim", names(fort_data), value=TRUE)
  # map x/y position to Dims
  mapping = c(mapping, aes_string(x=Dims[1L], y=Dims[2L]))
  class(mapping) = "uneval"
  
  # NULLing out global variables for nonstandard evaluation
  # (otherwise 'R CMD check package_name' will complain)
  Name <- Block <- NULL
  
  # Construct plot
  if (is.null(block))
  {
    p = ggplot(fort_data, mapping=mapping) +
      geom_point(...) +
      geom_hline(yintercept=0, colour="gray70") +
      geom_vline(xintercept=0, colour="gray70") +
      geom_text(aes(label=Name), ...) +
      facet_wrap(~ Block)  
  } else {
    block_data = subset(fort_data, grepl(block, fort_data$Block))
    p = ggplot(block_data, mapping=mapping) +
      geom_point(...) +
      geom_hline(yintercept=0, colour="gray70") +
      geom_vline(xintercept=0, colour="gray70") +
      geom_text(aes(label=Name), ...)
  }
  return(p)
}



#' @title Check block argument for plotting
#' 
#' @details
#' Internal function. \code{check_block_plot} is called 
#' by \code{autoplot.maxbet}.
#'
#' @param block whether to plot all blocks (\code{block = NULL}) or 
#' just one of them (eg \code{block = 1})
#' @param block_names vector of block names
#' @return validated block argument 
#' @keywords internal
#' @export
check_block_plot <- function(block, block_names) 
{
  if (!is.null(block)) 
  {
    if (length(block) > 1L) {
      warning("Only the first element in 'block' is used")
      block = block[1L]
    }
    if (is.character(block)) {
      bad_block <- !(block %in% block_names)
      if (bad_block)
        stop(paste("\nblock", block, "is not recognized"))
    } else {
      # block as numeric index
      if (!(block %in% seq_along(block_names)))
        stop("\nnumber of 'block' is out of limits")
      block = block_names[block]
    }
  }
  # output
  block
}



# Not implemented yet!!!
# wrapper function around ggplot for autoplot maxbet
ggblock <- function(data, block, mapping, ...) 
{  
  # Construct default aesthetic mappings
  Dims = grep("Dim", names(data), value=TRUE)
  # map x/y position to Dims
  mapping = c(mapping, aes_string(x=Dims[1L], y=Dims[2L]))
  class(mapping) = "uneval"
  # NULLing out global variables for nonstandard evaluation
  # (otherwise 'R CMD check package_name' will complain)
  Name <- Block <- NULL
  
  # Construct plot
  if (is.null(block))
  {
    p = ggplot(data, mapping=mapping) +
      geom_point(...) +
      geom_hline(yintercept=0, colour="gray70") +
      geom_vline(xintercept=0, colour="gray70") +
      geom_text(aes(label=Name), ...) +
      facet_grid(. ~ Block)  
#      facet_wrap(~ Block)  
  } else {
    p = ggplot(subset(data, Block==block), mapping=mapping) +
      geom_point(...) +
      geom_hline(yintercept=0, colour="gray70") +
      geom_vline(xintercept=0, colour="gray70") +
      geom_text(aes(label=Name), ...)
  }
  
  return(p)
}



