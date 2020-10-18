#' Fetching metadata for a doi from COCI
#'
#' Fetches reference, citation and meta information from the OpenCitations
#' COCI project, see here: https://opencitations.net/index/coci
#'
#' @param DOI digital object identifier as character
#'
#' @export
#' @import citecorp
#' @import yaml
doi.fetch_metadata <- function(DOI){
  paper.references <- oc_coci_refs(DOI)
  paper.citations <- oc_coci_cites(DOI)
  paper.meta <- oc_coci_meta(DOI)
  paper_data <- list(meta=list(title=paper.meta[["title"]],
                             author=paper.meta[["author"]],
                             citation_count=paper.meta[["citation_count"]],
                             journal=paper.meta[["source_title"]],
                             year=paper.meta[["year"]],
                             volume=paper.meta[["volume"]],
                             doi=paper.meta[["doi"]],
                             link=paper.meta[["oa_link"]]),
                   references=paper.references[["cited"]],
                   cited_by=paper.citations[["citing"]])

  write_yaml(paper_data, local.paper_path(DOI, filename="paper_data.yaml"))
  return(paper_data)
}

#' Fetches the citations/references/meta for all references in a DOI
#'
#' @param DOI digital object identifier as character
#' @param download_paper logical, whether to download the .pdf too
#'
#' @export
#' @import yaml
#' @import progress
#' @import purrr
paper.fetch_references_data <- function(DOI, download_paper=FALSE){
  if(local.paper_data.exists(DOI)){
    paper_metadata <- paper.data(DOI)
  } else {
    message("DOI metadata not found in local files, fetching first.")
    paper_metadata <- paper.fetch_data(DOI, download_paper=FALSE, auto_open_paper=FALSE)
  }
  if(length(paper_metadata$references) == 0){
    message("No references found for...\n",
            paper_metadata$meta$title,"\n\n",
            "Skipping")
    return(NA)
  } else{
    message("Processing all references for...\n",
            paper_metadata$meta$title,"\n\n",
            "Total references in CICO:  ", length(paper_metadata$references))
    pb <-
    progress_bar$new(
      format = "  processing... [:bar] :percent in :elapsed",
      total = length(paper_metadata$references),
      show_after=0,
      clear = TRUE, width= 80)
    pb$tick(0)

    references <- map(paper_metadata$references,
                      paper.fetch_data,
                      download_paper=download_paper, progress=pb, auto_open_paper=FALSE)
    message("\nDone!")
    return(references)
  }
}

#' Fetches the citations/references/meta for all works citing a DOI
#'
#' @param DOI digital object identifier as a string
#' @param download_paper whether to download the paper .pdf as well
#'
#' @export
#' @import yaml
#' @import progress
#' @import purrr
paper.fetch_citations_data <- function(DOI, download_paper=FALSE){
  if(local.paper_data.exists(DOI)){
    paper_metadata <- paper.data(DOI)
  } else {
    message("DOI metadata not found in local files, fetching first.")
    paper_metadata <- paper.fetch_data(DOI, download_paper=FALSE, auto_open_paper=FALSE)
  }
  if(length(paper_metadata$cited_by) == 0){
    message("No citations found for...\n",
            paper_metadata$meta$title,"\n\n",
            "Skipping")
    return(NA)
  } else{
    message("Processing all citations for...\n",
            paper_metadata$meta$title,"\n\n",
            "Total references in CICO:  ", length(paper_metadata$cited_by))
  pb <-
  progress_bar$new(
    format = "  processing... [:bar] :percent in :elapsed",
    total = length(paper_metadata$cited_by),
    show_after=0,
    clear = TRUE, width= 80)
  pb$tick(0)

  citations<- map(paper_metadata$cited_by, paper.fetch_data, download_paper=download_paper, progress=pb)

  message("\nDone!")
  return(citations)
  }
}

#' Get metadata for a paper
#'
#' @param DOI digital object identifier as a character
#' @param download_paper whether or not to include the paper .pdf as well
#' @param auto_open_paper whether to open the paper as soon as it's downloaded, if download_paper is true
#' @param progress a progress bar object, to be ticked after completion
#'
#' @export
#' @import yaml
#' @import progress
paper.fetch_data <- function(DOI, download_paper=TRUE, auto_open_paper=TRUE, progress=NULL){
  if(local.paper_data.exists(DOI)){
    if( ! is.null(progress)){
      progress$tick()
    }
    return(paper.data(DOI))
  } else {
    if(download_paper == FALSE){
    } else if(auto_open_paper==TRUE){
      if( ! paper.download_and_open(DOI)){
        file.create(local.paper_path(DOI=DOI, "COULD NOT FIND TO DOWNLOAD"))
      }
    } else {
      if( ! paper.download(DOI)){
        file.create(local.paper_path(DOI=DOI, "COULD NOT FIND TO DOWNLOAD"))
      }
    }
    metadata <- doi.fetch_metadata(DOI)
    if( ! is.null(progress)){
      progress$tick()
    }
    return(metadata)
  }
}


