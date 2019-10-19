# 
# # ----- Validating countryCode and stateCode
# 
# if ( FALSE ) {
#   
#   # countryCode
#   countryCode <- toupper(countryCode)
#   if ( !countryCode %in% countrycode::codelist[,"iso2c"] ) {
#     stop(sprintf(
#       "countryCode \"%s\" is not recognized. You must ISO 3166-1 alpha-2.",
#       countryCode
#     ))
#   }
#   
#   # TODO:  We need to create an internal list of acceptable stateCodes
#   
#   # stateCode
#   stateCode <- toupper(stateCode)
#   if ( stringr::str_length(stateCode) > 2 ) {
#     stop(sprintf(
#       "stateCode \"%s\" is not recognized. You must ISO 3166-2 alpha-2.",
#       stateCode
#     ))
#   }
#   
#   
# }