#' @title Barycentric plot for MAXBET
#' 
#' @description barycentric for maxbet objects using \code{ggplot} style
#' 
#' @param object a \code{"maxbet"} object 
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
#' @examples
#' # load wines
#' data(wines)
#' wines = scale(wines, scale=FALSE)
#' 
#' # list of blocks
#' blocks = list(Blok1 = 1:4, Blok2 = 5:7, Blok3 = 8:11, Blok4 = 12:14)
#' 
#' # apply maxbet
#' mb = MAXBET(wines, blocks=blocks, maxdim = 2)
#' 
#' # plot scores
#' baryplot(mb) +
#' ggtitle("Barycentric Plot")
baryplot <- 
function(object, mapping = aes(), data = NULL, dims = c(1, 2), ...) 
{
  # check arguments
  if (length(dims) != 2) {
    stop("\nYou must choose exactly two dimensions to plot")
  }
  
  # prepare the appropriate plots
  fort_data <- fortify.maxbet(model=object, data=data, what="scores", dims=dims)

  # NULLing out global variables for nonstandard evaluation
  # (otherwise 'R CMD check package_name' will complain)
  Name <- centroidx <- centroidy <- x <- y <- NULL

  # centroids (barycenters)
  centroids = data.frame(
    Name = object$all_names$obs_names,
    x = tapply(fort_data[,3], fort_data$Name, mean),
    y = tapply(fort_data[,4], fort_data$Name, mean))
    
  # add centroids to Scores
  num_blocks = length(object$all_names$block_names)
  fort_data$centroidx = rep(centroids$x, num_blocks)
  fort_data$centroidy = rep(centroids$y, num_blocks)
  
  # Construct default aesthetic mappings
  Dims = grep("Dim", names(fort_data), value=TRUE)
  # map x/y position to Dims
  mapping = c(mapping, aes_string(x=Dims[1L], y=Dims[2L]))
  class(mapping) = "uneval"

  
  # Construct plot
  p = ggplot(fort_data, mapping=mapping) +
    geom_hline(yintercept=0, colour="gray70") +
    geom_vline(xintercept=0, colour="gray70") +
    geom_segment(aes(xend=centroidx, yend=centroidy, color=Name), alpha=0.5) + 
    geom_point(data = centroids, aes(x=x, y=y, color=Name), 
               size = 5, alpha=0.5) +
    geom_point(aes(color = Name), size=3) 
  # output
  return(p)
}
