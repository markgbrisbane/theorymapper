#' Returns a URL for accessing a pdf
#'
#' If your institution has a forwarding URL which allows direct access to .pdf
#' papers, by appending a URL to the paper DOI, set that URL to the environmental
#' variable INSTITUTION_PDF_PREFIX and this method will produce links to .pdfs
#' from DOIs
#'
#' @param DOI Digital Object Identifier as a character
#'
#' @export
paper.institution_doi_url <- function(DOI){
  if(Sys.getenv("INSTITUTION_PDF_PREFIX") == ""){
    if(exists("INSTITUTION_PDF_PREFIX")){
      prefix = INSTITUTION_PDF_PREFIX
    } else {
      stop("You need to set INSTITUTION_PDF_PREFIX in either your R env, or your machine's env.")
    }
  } else{
    prefix = Sys.getenv("INSTITUTION_PDF_PREFIX")
  }
  paste0(prefix, DOI)
}

#' Downloads a paper
#'
#' This requires a the INSTITUTION_PDF_PREFIX env variable set according to paper.institution_doi_url
#' Visits a institution/DOI url, searches for an iframe object with an embedded pdf according to a
#' given xpath, and then downloads the paper to a location set with download_path
#'
#' @param DOI digital object identifier as character
#' @param download_path full path to download .pdf to as a character
#' @param iframe_xpath xpath attribute to find the .pdf in, as a character with escapes if necessary
#'
#' @import rvest
#' @import xml2
#' @import stringr
#' @import magrittr
#' @export
#'
paper.download <- function(DOI, download_path=local.paper_path(DOI), iframe_xpath = "//*[@id=\'pdf\']"){
  if(local.paper.exists(DOI)){
    message(paste0("Paper .pdf was already downloaded! Find it at:\n",local.paper_path(DOI)))
  } else {
    pdf_url_from_iframe<-
      paper.institution_doi_url(DOI) %>%
      html_session() %>%
      read_html() %>%
      html_nodes(xpath=iframe_xpath) %>%
      html_attr("src") %>%
      str_replace("(?<=\\.pdf).*$", "") %>%
      str_replace("^//", "https://")
    download.file(pdf_url_from_iframe, download_path)
  }
  return(TRUE)
}


#' Downloads and opens a paper
#'
#' See paper.download function for details
#'
#' @param DOI digital object identifier as character
#' @param download_path full path to download .pdf to as a character
#'
#' @export
paper.download_and_open <- function(DOI, download_path=local.paper_path(DOI)){
  paper.download(DOI, download_path)
  system(paste0('open "', download_path, '"'))
  return(TRUE)
}
