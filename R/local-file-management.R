#' Returns the path to a paper .pdf
#'
#' @param DOI digital object identifier as character
#' @param filename filename to save as as character eg. paper_name.pdf
#'
#' @export
#' @import stringr
local.paper_path <- function(DOI, filename=NULL){
  local_dir <- local.paper_dir(DOI)
  if( ! dir.exists(local_dir)){
    dir.create(path=local_dir, recursive=TRUE)
  }
  if(is.null(filename)){
    DOI.encoded <- str_replace_all(DOI, "/", ":")
    return_path <- paste0(local_dir,"/", DOI.encoded,".pdf")
  } else {
    return_path <- paste0(local_dir,"/", filename)
  }
  return(return_path)
}

#' Returns the directory of a paper
#'
#' @param DOI digital object identifier
#'
#' @export
#' @import stringr
#' @import here
local.paper_dir <- function(DOI){
  DOI.prefix.publisher <- str_extract(DOI, "(?<=10\\.)([0-9]+)(?=/)")
  DOI.object_id <- str_extract(DOI, "(?<=/).*")
  DOI.object_id.encoded <- str_replace_all(DOI.object_id, "/", ":")

  #journal_dir <- existing_dir_with(DOI.prefix.publisher, here("data"))
  #if(identical(journal_dir, character(0))){
  #  journal_dir <- DOI.prefix.publisher
  #}
  prefix_dir <- DOI.prefix.publisher

  here("data", prefix_dir, DOI.object_id.encoded)
}

#' Returns path to paper_data.yaml file
#'
#' @param DOI digital object identifier as character
#'
#' @export
local.paper_data_path <- function(DOI){
  local_dir <- local.paper_dir(DOI)
  if( ! dir.exists(local_dir)){
    dir.create(path=local_dir, recursive=TRUE)
  }
  return_path <- paste0(local_dir,"/", "paper_data.yaml")
  return(return_path)
}

#' Checks whether local paper data exists
#'
#' @param DOI digital object identifier as character
#'
#' @export
local.paper_data.exists <- function(DOI){
  file.exists(local.paper_data_path(DOI))
}

#' Checks whether local paper .pdf exists
#'
#' @param DOI digital object identifier as character
#'
#' @export
local.paper.exists <- function(DOI){
  file.exists(local.paper_path(DOI))
}

#' Returns data for the papers you've downloaded
#'
#' @export
#' @import stringr
#' @import magrittr
#' @import here
#' @import yaml
local.papers.downloaded <- function(){
  all_data_files <- dir(here("data"), recursive=TRUE)
  downloaded_paper_data <-
  all_data_files %>%
    str_subset("pdf$") %>%
    str_replace("/[^/]+\\.pdf", "/paper_data.yaml")
  downloaded_paper_data.full_path <- here("data", downloaded_paper_data)
  map(downloaded_paper_data.full_path, read_yaml)
}
