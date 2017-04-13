cartridge_string <- "cart|vc|vap|refill|juju|joint"
oil_string <- "oil|RSO|ESO"

categorizeProductName <- function(productName){
  #' Takes product name and categorizes it into a 
  #' product category type
  #' @param subtype  A string of inhalant product names
  #' @return A categorized usage of the productname as a string.
  #' Takes a resource subtype and categorizes it into a 
  #' Particular type of use of the resource
  #' @param subtype  A string resource.hub subtype
  #' @return A categorized usage of the resource as a string.
  
  # cartridge
  if(grepl(cartridge_string, productName, ignore.case=TRUE)){
    return("Cartridges")
  }
  # oil
  else if(grepl(oil_string, productName, ignore.case=TRUE)){
    return("Oil")
  }
  else{
    return("Uncategorized")
  }
}
