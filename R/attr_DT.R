

#' attr_DT: Workaround to use data.table attributes that are stable during chaining, etc.
#'
#' This is a workaround with reference to https://github.com/Rdatatable/data.table/issues/995: retain attributes while chaining.
#' Instead of storing attributes within the data.table object DT,
#' they are stored and may be accessed via a separate data.table object DT.attributes that lives in the .GlobalEnv.
#' The function allows to access attributes by name or assign values to them in a similar style like base::attr.
#'
#' @param DT A \code{data.table} object existing in the calling environment (function tested for \code{.GlobalEnv}).
#' @param j A column name to name the attribute that shall be accessed or associated with a value.
#' @param value The value to be assigned to \code{j}, may be a single numeric value, vector, etc. or also an object such as a data.table.
#'              All values are internally encapsulated in a \code{list}.
#' @param delete By default \code{FALSE}.
#'               Set to \code{TRUE} if the attribute specified by \code{j} shall be deleted instead of being created or associated with a new value.
#'
#' @return A \code{data.table} is generated in the \code{.GlobalEnv} named DT.attributes that stores the attributes.
#' @export
#'
#' @examples
#'
#' myDT <- data.table(x = c(1:3), y = (letters[1:3]))
#' my_attr1 <- data.table(A1 = c(9:10), A2 = c(letters[9:10]))
#' my_attr2 <- "attribute2"
#'
#' attr_DT(myDT, j = "name_myattr1", value = my_attr1)
#' attr_DT(myDT, j = "name_myattr2", value = my_attr2)
#' myDT.attributes
#' #    name_myattr1 name_myattr2
#' # 1: <data.table>   attribute2
#' attr_DT(myDT, j = "name_myattr1")
#' #    A1 A2
#' # 1:  9  i
#' # 2: 10  j

attr_DT <- function(DT, j, value = NULL, delete = FALSE) {
  if (is.null(value) & delete == FALSE) {
    call_get_attr <- paste0(substitute(DT), ".attributes$", j, "[[1]]")
    return(eval(parse(text = call_get_attr)))
  } else if ((is.null(value) & delete == TRUE)) {
    DT.attributes <- paste0(substitute(DT), ".attributes")
    call_delete_attr <- paste0(DT.attributes, "[,", j,":= NULL]")
    eval(parse(text = call_delete_attr))
    if (ncol(eval(parse(text = DT.attributes))) == 0) {
      eval(parse(text = paste0("rm(",DT.attributes, ", envir = .GlobalEnv)")))
    }
  } else {
    DT.attributes <- paste0(substitute(DT), ".attributes")
    call_make_attr <- paste0(DT.attributes, "[,", j,":= list(list(value))]")
    if (!exists(DT.attributes)) {
      #initial
      assign(DT.attributes, data.table(init = integer(1L)),envir = .GlobalEnv)
      eval(parse(text =  call_make_attr))
      #delete init
      eval(parse(text = paste0(DT.attributes, "[,init:= NULL]")))
    } else {
      eval(parse(text =  call_make_attr))
    }
  }
}
