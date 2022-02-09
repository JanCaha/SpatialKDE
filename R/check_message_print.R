check_message_print <- function(){
  print_message <- getOption("SpatialKDE.suppres_message")

  if (is.null(print_message)){
    return(TRUE)
  } else {
    if(isFALSE(print_message)){
      return(TRUE)
    }
    else if(isTRUE(print_message)){
      return(FALSE)
    }
  }

  return(TRUE)
}
