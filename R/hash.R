#' One way hash a vector
#'
#' @param x vector of numbers or characters
#' @param type hashing algorithm to use
#' @param n_char number of characters to return
#' @return one way hashed vector of the same length
#' @examples
#' hash("This is secret stuff")
#' hash(c("Also handles", NA, "NA values"))
#' @export
hash <- function(x, type = "md5", n_char = NULL){

  vec_hash <- Vectorize(digest::digest, USE.NAMES = FALSE)

  x[!is.na(x)] <- vec_hash(x[!is.na(x)])

  if(!is.null(n_char)){

    return(substr(x, 1, n_char))
  }

  return(x)
}
