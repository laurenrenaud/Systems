# key words to regex function ----
keyWordsToRegex <- function(key.words){
  #' Takes a list of keywords and returns a regex 
  #' string with each keyword seperated by a pipe 
  #' indicating the terms are or'd together.
  #' @param key.words   List of keywords
  paste(key.words, collapse = "|")
}
# housing ------
cartridge_string <- keyWordsToRegex(
  c("cart", "refill", "vc", "vap", "pen", "juju", "joint")
)


## need to think about formatting grepl with | 
## or
## do we turn everything to lower
## and then use %in% with " "
 

categorizeProdName <- function(productName){
  #' Takes product name and categorizes it into a 
  #' product category type
  #' @param productName  A string of inhalant product names
  #' @return A categorized usage of the productname as a string.
  #' Takes a resource subtype and categorizes it into a 
  #' Particular type of use of the resource
  #' @param subtype  A string resource.hub subtype
  #' @return A categorized usage of the resource as a string.
  
  # cartidge
  if(grepl("cartridge_string", subtype, ignore.case=TRUE)){
    return("Cartridge")
  }
  # Health
  else (grepl("oil_string", subtype, ignore.case=TRUE)){
    return("Oil")
  }
  else return("Unclassified")
}

# use categeories
inhalantnames$Type <- as.character(sapply(as.character(inhalantnames$productname), categorizeProdName))

