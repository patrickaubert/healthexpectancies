#' Transforms a table with prevalences by age brackets into a table with (smoothed) prevalences by age
#'
#' This function transforms a table with prevalences by age brackets and by (optional) sex, year and categories
#' into a table with smoothed prevalences at all ages (for all sex, year and categories).
#'
#' Parameters must include the vector of age cuts, which defines the age brackets. For instance, c(60,70,80)
#' defines age brackets `[60,70)`, `[70,80)` and `[80,Inf]`.
#'
#' By default, the calculation minimises the sum of squares of second-differences of the logit of prevalences
#' by age, under the constraint that average prevalences by age brackets
#' (weighted according to the 'weight' vector, usually the vector of population size at each age)
#' are equal to the 'prevalence' input vector (see \code{\link{prevalenceApprox}}); prevalences must then be
#' proportions between 0 and 1. If 'option' is set to 'legacy', the former method (second-differences of prevalences
#' rather than of their logit, which underestimates prevalences at the first ages) is used.
#' Alternatively, if 'option' is set to 'polynomial', polynomial approximation is calculated.
#' The degree of the polynomial function can be constrained
#' by setting option to 'polynomialD' (with D = degree, between 1 and 4) ; default value is 4.
#'
#' In the output table, 'prevalence.raw' corresponds to input values of prevalences (constant by age bracket)
#' while 'prevalence.approx' corresponds to smoothed values (different value at every age).
#'
#' @param tab a dataframe containing categorisation variables (year, sex, categ) and prevalences by age bracket
#' @param agecuts a vector with age defining the age brackets (minimum age in each age bracket)
#' @param agemin minimum age in the output table
#' @param agemax maximum age in the output table
#' @param weights.tab a dataframe with weights (ie population) by age and some of the categorisation variables of the 'tab' dataframe
#' @param categories a vector of names of variables of the 'tab' dataframe, representing categories
#' @param option smoothing method : "" (default, logit scale), "legacy", "polynomial" or "polynomialD" (see Details)
#'
#' @return a table containing the same categorisation variables than the input table, plus prevalences by age
#'
#' @export ApproxPrevalenceTable
ApproxPrevalenceTable <- function(tab,
                                  agecuts,agemin,agemax,
                                  weights.tab = NULL,
                                  categories = c("categ"),
                                  option = ""){

  # == if input tables are tibbles, transform into dataframe

  tab <- as.data.frame(tab)
  if (!is.null(weights.tab)) { weights.tab <- as.data.frame(weights.tab) }

  # == checks on arguments

  if (is.unsorted(agecuts, strictly = TRUE)) { stop("'agecuts' must be strictly increasing.", call. = FALSE) }
  if (agemin < agecuts[1]) { stop("'agemin' must not be lower than the first value of 'agecuts'.", call. = FALSE) }

  # == extract names of variables in input tables

  # -- in 'tab'

  notfound <- setdiff(categories, c(names(tab), "categ"))
  if (NROW(notfound) > 0) {
    warning(paste0("Variable(s) listed in 'categories' but not found in 'tab' (ignored): ",
                   paste0("'",notfound,"'",collapse=", "),"."), call. = FALSE)
  }

  categories <- intersect( names(tab), unique( c( categories, "year","sex","categ") ) )

  name.agetab <- names(tab)[grepl("^(A|a|â)ge",names(tab))]
  name.prev <- names(tab)[!(names(tab) %in% categories) & !(names(tab) %in% name.agetab)]
  if (NROW(name.prev)==0) { stop("No prevalence variable found in 'tab'.", call. = FALSE) }
  if (NROW(name.prev)>1) { stop("More than one prevalence variable in 'tab'")
  } else { message(paste0("'",name.prev,"' is used as prevalence variable."))}
  # age variable used to identify age brackets : 'age' if present, otherwise the first variable whose name starts with 'age'
  name.agetab <- name.agetab[order(name.agetab != "age")][1]

  # -- in 'weights.tab'

  categories.w <- categories[categories %in% names(weights.tab) ]

  name.agew <- names(weights.tab)[grepl("^(A|a|â)ge",names(weights.tab))]

  name.w <- names(weights.tab)[!(names(weights.tab) %in% c(categories.w,name.agew))]
  if (NROW(name.w)>1) { stop("More than one weight variable in 'weights.tab'")}

  # == readable label of each category (used in error messages)

  if (NROW(categories)==0) { labelcateg <- rep("all data", nrow(tab))
  } else { labelcateg <- do.call(paste, c(lapply(categories, function(v) tab[[v]]), sep = " / ")) }

  # == if relevent, aggregates categories (including sex and year) into a unique categ variable

  # -- in 'tab'

  if (NROW(categories)==0) { tab$categloc <- rep("#",nrow(tab))
  } else {

    for (k in 1:NROW(categories)) {
      tab[,categories[k]] <- paste0("#",k,tab[,categories[k]],"#")
      if (categories[k] %in% names(weights.tab)) {weights.tab[,categories[k]] <- paste0("#",k,weights.tab[,categories[k]],"#")}
    }

    tab <- tab %>% unite(categloc,all_of(categories),sep="", remove = FALSE)
  }

  if (NROW(categories.w)==0) { tab$categ.w <- rep("#",nrow(tab))
  } else {
    tab <- tab %>% unite(categ.w,all_of(categories.w),sep="", remove = FALSE)
  }

  # -- in 'weights.tab'

  if (NROW(categories.w)==0) { weights.tab$categ.w <- rep("#",nrow(weights.tab))
  } else {
    weights.tab <- weights.tab %>% unite(categ.w,all_of(categories.w),sep="", remove = FALSE)
  }

  # == list of all categories

  cases <- tab[ , c("categloc","categ.w")] %>% distinct()
  cases$label <- labelcateg[match(cases$categloc, tab$categloc)]

  # == call approximation function for every category, sex and year

  option <- tolower(option)
  degree <- 4
  if (grepl("polynomial[[:digit:]]",option)) {
    degree <- gsub("[^[:digit:]]","",option) %>% as.numeric()
    degree <- min(4,max(1,degree))
    option <- "polynomial"
  }

  approxloc <- function(i) {

    lab <- cases$label[i]

    # -- weights : sorted by age, and restricted to ages between 'agemin' and 'agemax'
    if (is.null(weights.tab)) { weights.loc <- rep(1,(agemax-agemin+1))
    } else {
      wloc <- weights.tab[weights.tab$categ.w == cases$categ.w[i], , drop = FALSE]
      if (nrow(wloc) == 0) {
        stop(paste0("No weights found in 'weights.tab' for the category: ",lab,"."), call. = FALSE)
      }
      if (NROW(name.agew) >= 1) {
        name.agew1 <- name.agew[order(name.agew != "age")][1]
        wage <- age_lower_bound(wloc[[name.agew1]])
        keep <- !is.na(wage) & wage >= agemin & wage <= agemax
        wloc <- wloc[keep, , drop = FALSE]; wage <- wage[keep]
        wloc <- wloc[order(wage), , drop = FALSE]; wage <- sort(wage)
        if (!identical(as.numeric(wage), as.numeric(agemin:agemax))) {
          stop(paste0("Weights for the category '",lab,"' must be provided exactly once for each age from ",
                      agemin," to ",agemax," (check 'weights.tab')."), call. = FALSE)
        }
      }
      weights.loc <- wloc[[name.w]]
      if (length(weights.loc) != (agemax-agemin+1)) {
        stop(paste0("Weights for the category '",lab,"': ",length(weights.loc)," values found, ",
                    (agemax-agemin+1)," expected (ages ",agemin," to ",agemax,")."), call. = FALSE)
      }
    }

    approxloc <- function(...) {
      if (option %in% c("polynomial")) { prevalence_to_polynomial(degree = degree, ...)
      } else { prevalenceApprox(..., method = ifelse(option == "legacy", "legacy", "logit")) }
    }

    agebracket <- cut(c(agemin:agemax), breaks = c(agecuts,Inf), include.lowest = TRUE, right = FALSE)
    # -- prevalences : sorted by age bracket, and checked against 'agecuts'
    tabloc <- tab[tab$categloc == cases$categloc[i], , drop = FALSE]
    if (!is.na(name.agetab)) {
      lower <- age_lower_bound(tabloc[[name.agetab]])
      if (anyNA(lower) | anyDuplicated(lower) > 0 | !setequal(lower, agecuts)) {
        stop(paste0("For the category '",lab,"', the age brackets in 'tab' (starting at ",
                    paste(sort(unique(lower)), collapse = ", "),") do not match 'agecuts' (",
                    paste(agecuts, collapse = ", "),"). Is a bracket missing or duplicated, ",
                    "or does 'tab' contain several versions of the same series ",
                    "(another variable that should be part of 'categories', or a filter to apply) ?"), call. = FALSE)
      }
      tabloc <- tabloc[order(lower), , drop = FALSE]
    } else if (nrow(tabloc) != NROW(agecuts)) {
      stop(paste0("For the category '",lab,"', 'tab' has ",nrow(tabloc)," rows but 'agecuts' defines ",
                  NROW(agecuts)," age brackets."), call. = FALSE)
    }
    prevalence.raw <- tabloc[[name.prev]]
    if (anyNA(prevalence.raw)) {
      stop(paste0("Missing values in the prevalences of the category '",lab,"'."), call. = FALSE)
    }

    data.frame(
      categloc = rep(cases$categloc[i],(agemax-agemin+1)),
      age = c(agemin:agemax),
      agebracket = agebracket,
      prevalence.approx = approxloc(
        prevalence = prevalence.raw,
        agecuts = agecuts, agemin = agemin, agemax = agemax,
        weight = weights.loc ),
      stringsAsFactors = FALSE
    ) %>% left_join(
      data.frame(prevalence.raw = prevalence.raw,
                 agebracket = unique(agebracket),
                 stringsAsFactors = FALSE),
      by = "agebracket"
    )
  }

  tabout <- do.call("bind_rows",lapply(c(1:nrow(cases)),approxloc))

  # == recreates seperate variables for categories

  for (j in c(1:NROW(categories))) {
    tabout[,categories[j]] <- str_extract(tabout$categloc,paste0("(?<=#",j,")[^#]*(?=#)"))
  }

  return(tabout[,c(categories,"age","agebracket","prevalence.raw","prevalence.approx")])

}
