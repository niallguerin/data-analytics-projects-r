# Assignment: 10
# Name: Niall Guerin
# Student ID: 18235079
# Web Resources: http://r-pkgs.had.co.nz/check.html, http://r-pkgs.had.co.nz/description.html (to resolve warning for testthat)
#' The constructor for qfifo
#' @return An S3 object of class qfifo.
#' @examples
#' q <- qfifo()
#' @export
qfifo <- function(){
  structure(list(qelements=list()), class ="qfifo")
}

#' Add a value to the queue
#'
#' @param q is the current queue object
#' @param val is the value to be added to the queue
#'
#' @return The updated queue object
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
add <- function(q,val){
  UseMethod("add")
}

#' @export
add.qfifo <-function(q,val){
  q$qelements[length(q$qelements)+1] <- val
  q
}

#' Return the top value in the queue
#'
#' @param q is the current queue object
#'
#' @return The top of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q, 1234)
#' v <- top(q)
top <- function(q){
  UseMethod("top")
}

#' @export
top.qfifo <- function(q){
  # get the top value from the queue - first item in the queue
  if(length(q$qelements) == 0)
    stop("No elements in the queue")
  q$qelements[[1]]
}

#' Delete the top element from the queue
#'
#' @param q is the current queue object
#'
#' @return The modified queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q, 1234)
#' q <- add(q, 5678)
#' q <- process(q)
process <- function(q){
  UseMethod("process")
}

#' @export
process.qfifo <- function(q){
  # get the first element in the queue (first-in-first-out) and delete
  if(length(q$qelements) == 0)
    stop("No elements on the queue to remove")
  q$qelements[1] <- NULL
  q
}
