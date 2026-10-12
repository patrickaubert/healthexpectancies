# Internal helper used by ApproxPrevalenceTable()

# Lower bound of age brackets, from a numeric variable (minimum age or single age) or from labels of age brackets
# such as "[60,65)", "[85,Inf]", "60-64" or "de 60 a 64 ans" (the first number found in the label is used).
age_lower_bound <- function(x) {
  if (is.numeric(x)) { return(as.numeric(x)) }
  as.numeric(stringr::str_extract(as.character(x), "[0-9]+"))
}
