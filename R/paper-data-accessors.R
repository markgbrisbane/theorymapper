#' Returns the yaml data from a local paper
#'
#' @param DOI digital object identifier as character
#'
#' @export
paper.data <- function(DOI){
  if(local.paper_data.exists(DOI)){
    paper_data <- read_yaml(local.paper_data_path(DOI))

    paper_data$meta <-
    map(paper_data$meta, function(x){
      if(is.null(x)){
        NA
      }else{
        x
      }
    })
    return(paper_data)
  } else {
    stop(paste0("\nPaper with DOI:  ", DOI,"\nIsn't stored locally. Stopping now."))
  }
}


#' Find paper in local directory by name
#'
#' @param value name to search for
#' @param by currently can only search by name
#' @param only_downloaded currently can only search directories with a downloaded .pdf
#'
#' @export
#' @import purrr
paper.find <- function(value, by="name", only_downloaded=TRUE){
  if(only_downloaded){
    results <- map(local.papers.downloaded(), function(x){
      if(x$meta$title == value){
        return(x)
      } else {
        return(NA)
      }
    })
    results %>% compact()
  }
}
