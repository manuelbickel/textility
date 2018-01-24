#' Subset rows of a data.table in a memory efficient way
#'
#' @param DT The \code{data.table} to be subsetted.
#' @param idxs An \code{integer} vector with row indices for the subset. For a conditional subsets see example below.
#' @param del.idxs FALSE by default to get a positive and return the selected rows. Set to FALSE for deleting rows.
#'
#' @return A positive (or negative) subset of the input \code{data.table}.
#' @export
#'
#' @examples
#' #conditional deletion of rows
#' DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
#' #introduce numbering id if relevant
#' DT[, id := 1:nrow(DT)]
#' delete_letters <- c("a", "c")
#' delete_idxs <- which(DT[,x] %in% delete_letters)
#' DT_subset <- subset_rows_DT(DT = DT, idxs = delete_idxs, delete_idxs = TRUE)
#' DT_subset
#' #    x y v id
#' # 1: b 1 1  1
#' # 2: b 3 2  2
#' # 3: b 6 3  3

subset_rows_DT <- function(DT, idxs, delete_idxs = FALSE) {
  #slightly adapted version as proposed here
  #https://github.com/Rdatatable/data.table/issues/635
  #https://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-data-table
  if (delete_idxs == TRUE) {
    idxs <- (1:nrow(DT))[!(1:nrow(DT) %in% idxs)]
  }
  cols = names(DT)
  DT.subset = data.table(DT[[1]][idxs])
  setnames(DT.subset, cols[1])
  for (col in cols[2:length(cols)]){
    DT.subset[, (col) := DT[[col]][idxs]]
    DT[, (col) := NULL] #delete
  }
  return(DT.subset)
}
