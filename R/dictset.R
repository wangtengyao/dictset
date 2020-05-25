#' dictset: An implementation for Dictionary and Set data types.
#'
#' @docType package
#' @name dictset
#'
#' @import stats
#' @import digest
#' @import utils
NULL


#' Compute MD5 of R object
#' @param obj R object
#' @return MD5 of serialised object
md5 <- function(obj) return(digest::digest(obj))

#' Check if Dictionary/Set is empty
#' @param x Dictionary or Set
#' @return boolean
#' @export
isEmpty <- function(x, ...) UseMethod('isEmpty')

#' Check if Dictionary/Set contains an element
#' @param x Dictionary or Set
#' @return boolean
#' @export
contains <- function(x, ...) UseMethod('contains')

#' Get an element from a Dictionary/Set
#' @param x Dictionary or Set
#' @return element value
#' @export
get <- function(x, ...) UseMethod('get')

#' put element to Dictionary/Set
#' @param x Dictionary or Set
#' @return invisible Dictionary/Set after change
#' @export
put <- function(x, ...) UseMethod('put')

#' Copy a Dictionary/Set
#' @param x Dictionary or Set
#' @return a new copy
#' @export
copy <- function(x, ...) UseMethod('copy')

#' Output all keys in a dictionary
#' @param x Dictionary
#' @return all keys
#' @export
keys <- function(x, ...) UseMethod('keys')

#' Output all values in Dictionary/Set
#' @param x Dictionary or Set
#' @return all values
#' @export
vals <- function(x, ...) UseMethod('vals')

#' Remove an element from Dictionary/Set and get its value
#' @param x Dictionary or Set
#' @return value of popped element
#' @export
pop <- function(x, ...) UseMethod('pop')

#' Short string description of an R object
#' @param x R object
#' @return a short string description
#' @export
toStr <- function(x){
  if (is.vector(x)){
    if (length(x)==1){
      if (is.character(x)) return(paste0('"', x, '"')) else return(toString(x))
    } else {
      return(paste0(class(x), '(', length(x), ')'))
    }
  } else if (is.matrix(x)){
    return(paste0(class(x[1,1]), '(', dim(x)[1], ', ', dim(x)[2], ')'))
  } else if (is.atomic(x)){
    return(class(x))
  } else {
    address <- substring(capture.output(.Internal(inspect(x)))[1],2,17)
    return(paste0('<',class(x),': 0x', sub(' .*', '', address),'>'))
  }
}

###########   Dictionary   ##############
#' Constructor for the Dictionary class
#' @param keys a list/vector of keys, can be of any class, and do not need to be
#' the same class.
#' @param vals a list/vector of values, can be of any class and do not need to
#' be the same class.
#' @return a pointer to a Dictionary object initialised with keys and vals
#' @export
Dictionary <- function(keys=NULL, vals=NULL){
  if (length(keys) != length(vals))
    stop('The length of keys must be equal to the length of vals.')

  dict <- structure(new.env(hash=TRUE), class='Dictionary')

  for (i in seq_along(keys)){
    put(dict, keys[[i]], vals[[i]])
  }

  return(invisible(dict))
}

#' Check if dictionary is empty
#' @param x A Dictionary object
#' @return boolean
#' @export
isEmpty.Dictionary <- function(dict){
  return(length(dict)==0)
}

#' Check if dictionary contains a specific key
#' @param x A Dictionary object
#' @param key query key
#' @return boolean
#' @export
contains.Dictionary <- function(dict, key){
  key_hash <- md5(key)
  return(key_hash %in% names(dict))
}

#' Returns value associated with a specific key
#' @param dict A Dictionary object
#' @param key query key
#' @return value associated with the query key
#' @export
get.Dictionary <- function(dict, key){
  key_hash <- md5(key)
  if (!(key_hash %in% names(dict))) {
    stop('No such key contained in dictionary')
  }
  return(dict[[key_hash]]$val)
}

#' put a key/value pair to the dictionary
#' @param dict A Dictionary object
#' @param key key to be added
#' @param val associated value
#' @param overwrite whether to overwrite if key already exists in dict
#' @return a pointer to the updated Dictionary object
#' @details the dictionary is updated in place
#' @export
put.Dictionary <- function(dict, key, val, overwrite=TRUE){
  key_hash <- md5(key)
  if (overwrite | !(key_hash %in% names(dict))) {
    dict[[key_hash]] <- list(key=key, val=val)
  }
  return(invisible(dict))
}

#' copy a dictionary to another
#' @param dict Dictionary object to be copied
#' @return a clone of dict
#' @export
copy.Dictionary <- function(dict){
  ret <- structure(new.env(hash=TRUE), class='Dictionary')
  for (name in ls(dict)){
    ret[[name]] <- dict[[name]]
  }
  return(ret)
}

#' Remove all entries in a Dictionary object
#' @param dict A Dictionary object
#' @return the resulting dictionary with all entries removed
#' @export
clear.Dictionary <- function(dict){
  rm(list=ls(envir=dict), envir=dict)
  return(invisible(dict))
}

#' Return a list of keys in dictionary
#' @param dict A Dictionary object
#' @return a list of keys in dict
#' @export
keys.Dictionary <- function(dict){
  sapply(ls(dict), function(x) dict[[x]]$key, USE.NAMES=F)
}

#' Return a list of values in dictionary
#' @param dict A Dictionary object
#' @return a list of values in dict
#' @export
vals.Dictionary <- function(dict){
  sapply(ls(dict), function(x) dict[[x]]$val, USE.NAMES=F)
}

#' Removes the element with the specified key
#' @param dict A Dictionary object
#' @param key query key
#' @return value associated with query key
#' @details entry removal in dict is achieved as a side effect
#' @export
pop.Dictionary <- function(dict, key=NULL){
  if (is.null(key)){
    key_hashes <- names(dict)
    if (length(key_hashes) == 0) stop('Dictionary is empty.')
    key_hash <- key_hashes[1]
  } else {
    key_hash <- md5(key)
  }
  if (!(key_hash %in% names(dict))) stop('No such key contained in dictionary')

  val <- dict[[key_hash]]$val
  rm(list = key_hash, envir=dict)
  return(val)
}


#' Printing methods for the 'Dictionary' class
#' @param x object of the 'Dictionary' class
#' @param ... other arguments used in \code{print}
#' @export
print.Dictionary <- function(x, ...){
  hashed_keys <- names(x)
  omission <- FALSE
  if (length(hashed_keys) > 10) {
    hashed_keys <- head(hashed_keys, 10)
    omission <- TRUE
  }
  cat('[')
  for (h in hashed_keys){
    key = x[[h]]$key
    val = x[[h]]$val
    cat('\n  key = ', toStr(key), '; val = ', toStr(val), sep='')
  }
  if (omission) cat('\n  ...\n]\n') else cat('\n]\n')
}


###########   Set   ##############
#' Constructor for the Set class
#' @param elements a list/vector of elements, can be of any class, and do not
#' need to be the same class.
#' @return a pointer to a Set object initialised with elements
#' @export
Set <- function(elements=NULL){
  set <- structure(new.env(hash=TRUE), class='Set')

  for (i in seq_along(elements)){
    put(set, elements[[i]])
  }

  return(invisible(set))
}

#' Check if dictionary is empty
#' @param x A Dictionary object
#' @return boolean
#' @export
isEmpty.Set <- function(set){
  return(length(set)==0)
}

#' Check if set contains a specific elements
#' @param set A Set object
#' @param element query element
#' @return boolean
#' @export
contains.Set <- function(set, element){
  key_hash <- md5(element)
  return(key_hash %in% names(set))
}

#' put an element to the set
#' @param set A Set object
#' @param element element to be added
#' @return a pointer to the updated Set object
#' @export
put.Set <- function(set, element){
  key_hash <- md5(element)
  if (!(key_hash %in% names(set))) set[[key_hash]] <- element
  return(invisible(set))
}

#' copy a set to another
#' @param set Set object to be copied
#' @return a clone of set
#' @export
copy.Set <- function(set){
  ret <- structure(new.env(hash=TRUE), class='Set')
  for (name in ls(set)){
    ret[[name]] <- set[[name]]
  }
  return(ret)
}

#' Remove all entries in a Set object
#' @param set A Set object
#' @return the resulting set with all entries removed
#' @export
clear.Set <- function(set){
  rm(list=ls(envir=set), envir=set)
  return(invisible(set))
}

#' Return a list of elements in set
#' @param set A Set object
#' @return a list of elements in set
#' @export
vals.Set <- function(set){
  sapply(ls(set), function(x) set[[x]], USE.NAMES=F)
}


#' Removes an element from the set
#' @param set A Set object
#' @return an element
#' @export
pop.Set <- function(set){
  key_hashes <- names(set)
  if (length(key_hashes) == 0) stop('Set is empty.')
  key_hash <- key_hashes[1]

  element <- set[[key_hash]]
  rm(list = key_hash, envir=set)
  return(element)
}


#' Printing methods for the 'Set' class
#' @param x object of the 'Set' class
#' @param ... other arguments used in \code{print}
#' @export
print.Set <- function(x, ...){
  hashed_keys <- names(x)
  omission <- FALSE
  if (length(hashed_keys) > 10) {
    hashed_keys <- head(hashed_keys, 10)
    omission <- TRUE
  }
  cat('{')
  for (h in hashed_keys){
    key = x[[h]]
    cat('\n  ', toStr(key), sep='')
  }
  if (omission) cat('\n  ...\n}\n') else cat('\n}\n')
}
