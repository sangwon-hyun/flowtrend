# Generated from _main.Rmd: do not edit by hand

#' Parse command line arguments and assigns the values of them. |args| is meant
#' to just be additional command line arguments.
#'
#' @param args Argument.
#'
#' @return Nothing.
#' @export
parse_args <- function(args, verbose=FALSE){
  args = sapply(args, strsplit, "=")
  print(args)
  for(arg in args){

    ## Check if the thing is integer
    all_numbers = str_detect(arg[2], "^[:digit:]+$")

    ## Assign the variable
    if(all_numbers){
      assign(arg[1], as.numeric(arg[2]), inherits = TRUE)
    } else {
      assign(arg[1], arg[2], inherits = TRUE)
    }

    if(verbose){
      cat(arg[1], "takes the value of", arg[2],
          "from command line input", fill=TRUE)
    }
    print("===============================")
  }
}


