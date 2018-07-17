#' Get Wikipedia Text Content from Multiple Pages
#' 
#' A wrapper around WikipediR::get_page_content with some soft cleaning of the content
#' and automatic handling of pages with a redirect (see details section). Furthmore,
#' the function does not stop if input includes eroneous page names and simply skips these.
#' 
#' The content cleaning includes:
#' - removal of "non-text" sections (See_also|Notes_and_References|Notes|References|Further_reading|External_links)
#' - removal of html tags, line breaks, reference and other bracket content (e.g., [edit]) 
#' - harmonization of blanks
#' 
#' If a redirect page is hit, the function simply discards this page (and associated name) and turns to the
#' redirected page.
#'
#' @param page_names The names of the Wiki pages to retreive content of (e.g., "Main_Page").
#' @param language By default \code{"en"}.
#' @param project By default \code{"Wikpedia"}.
#' @param rm_bracket_length Maximum length (number of characters) of bracket content to be removed.
#'                          Edged brackets and enclosed content with equal or lower length are removed.
#'                          By default \code{50}.
#'
#' @return A character vector with Wiki content.
#' @export
#'
#' @examples
#'  
#' content = get_wiki_content(c("S_(programming_language)", "Eco-sufficiency", "Energy star ratings"))
#' # [1] "S_(programming_language)"
#' # [1] "Eco-sufficiency"
#' # [1] "Energy star ratings"
#' # [1] "Energy Star"
#' 
#' # notice that "energy star ratings" is actually a page with a redirect
#' # the function replaces it by the respective redirect page
#' 
#' str(content)
#' # Named chr [1:3] "SParadigm multi-paradigm: imperative, object orientedDeveloper Rick Becker, Allan Wilks, John ChambersFirst&#16"| __truncated__ ...
#' # - attr(*, "names")= chr [1:3] "S_(programming_language)" "Eco-sufficiency" "Energy star ratings"

get_wiki_content = function(page_names, language = "en", project = "wikipedia", rm_bracket_length = 50) {
  require(WikipediR)
  # basic function for one page name
  get_page_content = function(x) {
    print(x)
    x = page_content(language, project, page_name = x)
    x =  x$parse$text$`*`
    #remove final "non-text" sections
    x = gsub(paste0("<span class=\\\"mw-headline\\\" id=\\\"", "(See_also|Notes_and_References|Notes|References|Further_reading|External_links)", "\\\">.*$"), "", x, perl = F)
    #remove html tags
    x = gsub("<[^>]*>", "", x, perl = T)
    #remove line breaks
    x = gsub("\n", " ", x, fixed = T)
    #harmonize blanks
    x = gsub("\\s+", " ", x, perl  = T)
    x = gsub("\\s+$|^\\s+", "", x, perl  = T)
    x =  gsub("\"", "", x)
    # remove reference indices and some other wiki things like [edit]
    x = gsub(paste0("\\[.{1,", rm_bracket_length,"}\\]"), "", x, perl = T)
    if (grepl("^Redirect to", x)) {
      redirect =  gsub("^Redirect to:| This page is a redirect.*$", "", x, perl = T)
      redirect = gsub("\\s+$", "", redirect, perl = T)
      if (redirect == x) {
        return(NULL)
      } else {
        x = get_page_content(redirect)
      }
    }
    x
  }
  sapply(page_names, function(x) {
    tryCatch({get_page_content(x)}, error = function(e){cat(x, " - ERROR:",conditionMessage(e), "\n")})
  }, USE.NAMES = TRUE)
}