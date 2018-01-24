#' Uncontrtact negations, e.g., "don't" to "do not"
#'
#' @param x The \code{string} whose negations shall be uncontracted.
#' @param negation_forms A \code{matrix} with two columns of mode \code{character}
#'                       The first column contains the contracted form, the second the uncontracted form.
#'                       The default is "default", which considers the following negations:
#'                       "isn.t","aren.t","wasn.t","weren.t","hasn.t"  , "haven.t",
#'                       "hadn.t"  , "doesn.t" , "don.t" ,  "didn.t" , "won.t" ,  "wouldn.t",
#'                       "shan.t" ,  "shouldn.t" ,"can.t" ,  "couldn.t" ,"mustn.t" , "cannot"
#' @return The \code{string} with uncontracted negations.
#' @export
#'
#' @examples
#' txt <- c("I don`t want this.",  "I won`t accept it.")
#' uncontract_negations(txt)
#' #[1] "I do not want this."   "I will not accept it."

uncontract_negations <- function(x, negation_forms = "default") {
  if (negation_forms == "default") {
 neg <- matrix(c(contracted_negations = c("isn.t","aren.t","wasn.t","weren.t","hasn.t"  , "haven.t"
                                       ,"hadn.t"  , "doesn.t" , "don.t" ,  "didn.t" , "won.t" ,  "wouldn.t"
                                       ,"shan.t" ,  "shouldn.t" ,"can.t" ,  "couldn.t" ,"mustn.t" , "cannot")
             ,uncontracted_negations = c("is not" ,  "are not" , "was not" , "were not" , "has not" ,  "have not"
                                        ,"had not"  , "does not" , "do not" ,  "did not" , "will not"  , "would not"
                                        ,"shall not" , "should not", "can not" ,  "could not" , "must not", "can not")),ncol = 2)
  }
  stringi::stri_replace_all_regex(x, paste0("\\b", neg[,1] , "\\b"), neg[,2], vectorize_all = FALSE)
}
