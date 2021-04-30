# Two functions set up the dictionary and returns matching results of "key"
# They assume the variables "dictionary" and "language" are
# in the scope but defined outside of these functions.

setup_dictionary <- function(dictionary_csv) {
  dictionary <- dictionary_csv %>%
    read_csv(col_types = cols(.default = "c")) %>%
    split(.$key)
  return(dictionary)
}

tr <- function(key) {
  dictionary[[key]][[language]]
}
  
# Turn the above functions into a Reference Class
# This keeps the relavant variables inside of the class
# But it's slower than the above approach.
SimpleTranslator <- setRefClass(
  "SimpleTranslator",
  fields = list(
    dictionary_csv = "character",
    dictionary = "ANY",
    language = "character"
  ),
  methods = list(
    initialize = function(dictionary_csv, language = "en")
    { 
      dictionary_csv <<- dictionary_csv
      dictionary <<- dictionary_csv %>%
        read.csv() %>%
        split(.$key)
      language <<- language
    },
    tr = function(key)
    {
      dictionary[[key]][[language]]
    }
  )
)
