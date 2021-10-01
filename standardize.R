# Function for standardizing regression predictors by dividing by 2 sd's
#
# 17 Aug 2006:  rewritten to allow more options for binary inputs.  The new
#   default is to center binary inputs at zero (to allow for interactions to
#   be easier to interpret) but _not_ to rescale by dividing by 2 sd's.

standardize <- function (object, unchanged=NULL,
                         standardize.y=FALSE, binary.inputs="center"){
  #
  # Function for automatically standardizing by dividing by 2 sd's
  # Default is to standardize the input variables as follows:
  #   - Numeric variables that take on more than two values are each rescaled
  #     to have a mean of 0 and a sd of 0.5 (so that the scaled regression coef
  #     corresponds to a change from mean - 1*sd to mean + 1*sd)
  #   - Binary variables are rescaled to have a mean of 0 and a difference of 1
  #     between their two categories
  #   - Non-numeric variables that take on more than two values are unchanged
  #   - Variables that take on only one value are unchanged
  # In the new model, centered variables (by default, binary inputs) are
  #   prefixed with "c." and rescaled variables (by default, all other inputs)
  #   are prefixed with "z."
  #
  # Options:
  #   unchanged:  vector of names of parameters to leave unstandardized
  #   standardize.y:  if TRUE, the outcome variable is standardized also
  #   binary.inputs:  options for standardizing binary input variables:
  #     "0/1" (rescale so that the lower value is 0 and the upper is 1)
  #     "-0.5/0.5" (rescale so that the lower value is -0.5 and upper is 0.5)
  #     "center" (rescale so that the mean of the data is 0 and the difference
  #               between the two categories is 1)
  #     "full" (rescale by subtracting the mean and dividing by 2 sd's)
  #     "leave.alone" (do nothing)
  #
  call <- object$call
  if (is.null(call)) call <- object@call
  form <- call$formula
  varnames <- all.vars (form)
  n.vars <- length (varnames)
  #
  # Decide which variables will be unchanged
  #
  transform <- rep ("leave.alone", n.vars)
  if (standardize.y) {
    transform[1] <- "full"
  }
  for (i in 2:n.vars){
    v <- varnames[i]
    if (is.null(call$data)){   # if regression is using the regular workspace
      thedata <- get(v)
    }
    else {                     # if the regression is using a data frame
      thedata <- get(as.character(call$data))[[v]]
    }
    if (is.na(match(v,unchanged))){
      num.categories <- length (unique(thedata[!is.na(thedata)]))
      if (num.categories==2){
        transform[i] <- binary.inputs
      }
      else if (num.categories>2 & is.numeric(thedata)){
        transform[i] <- "full"
      }
    }
  }
  #
  # New variable names:
  #   prefix with "c." if centered or "z." if centered and scaled
  #
  varnames.new <- ifelse (transform=="leave.alone", varnames,
                          ifelse (transform=="full", paste ("z", varnames, sep="."),
                                  paste ("c", varnames, sep=".")))
  transformed.variables <- (1:n.vars)[transform!="leave.alone"]
  
  if (is.null(call$data)){
    #
    # If the regression is using the regular workspace, define the new variables
    #
    for (i in transformed.variables){
      assign (varnames.new[i], rescale(get(varnames[i]), binary.inputs))
    }
  }
  else {
    #
    # If the regression uses a data frame, define the new variables there
    #
    newvars <- NULL
    for (i in transformed.variables){
      assign (varnames.new[i],
              rescale (get (as.character(call$data)) [[varnames[i]]], binary.inputs))
      newvars <- cbind (newvars, get(varnames.new[i]))
    }
    assign (as.character(call$data),
            cbind (get(as.character(call$data)), newvars))
  }
  #
  # Now call the regression with the new variables
  #
  call.new <- call
  L <- sapply (as.list (varnames.new), as.name)
  names(L) <- varnames
  call.new$formula <- do.call (substitute, list (form, L))
  return (eval (call.new))
}

rescale <- function (x, binary.inputs){
  # function to rescale by subtracting the mean and dividing by 2 sd's
  x.obs <- x[!is.na(x)]
  if (!is.numeric(x)) x <- as.numeric(factor(x))
  if (length(unique(x.obs))==2){
    x <- (x-min(x.obs))/(max(x.obs)-min(x.obs))
    if (binary.inputs=="0/1") return (x)
    else if (binary.inputs=="-0.5,0.5") return (x-0.5)
    else if (binary.inputs=="center") return (x-mean(x.obs))
    else if (binary.inputs=="full") return ((x-mean(x.obs))/(2*sd(x.obs)))
  }      
  else {
    return ((x-mean(x.obs))/(2*sd(x.obs)))
  }
}

