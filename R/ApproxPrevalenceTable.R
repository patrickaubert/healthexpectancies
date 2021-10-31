#' Transforms a table with prevalences by age brackets into a table with (smoothed) prevalences by age
#'
#' This function transforms a table with prevalences by age brackets and by (optional) sex, year and categories
#' into a table with smoothed prevalences at all ages (for all sex, year and categories).
#' Parameters must include the vector of age cuts, which defines the age brackets. For instance, c(60,70,80)
#' defines age brackets [60,70), [70,80) and [80,Inf].
#' The calculation minimises the sum of squares of second-differences of prevalences
#' by age, under the constraint that average prevalences by age brackets
#' (weighted according to the 'weight' vector, usually the vector of population size at each age)
#' are equal to the 'prevalence' input vector. Alternatively, if 'option' is set to 'polynomial',
#' polynomial approximation is calculated.
#' In the output table, 'prevalence.raw' corresponds to input values of prevalences (constant by age bracket)
#' while 'prevalence.approx' corresponds to smoothed values (different value at every age).
#'
#' @param tab a dataframe containing categorisation variables (year, sex, categ) and prevalences by age bracket
#' @param agecuts a vector with age defining the age brackets (minimum age in each age bracket)
#' @param agemin minimum age in the output table
#' @param agemax maximum age in the output table
#' @param weights.tab a dataframe with weights (ie population) by age and some of the categorisation variables of the 'tab' dataframe
#' @param categories a vector of names of variables of the 'tab' dataframe, representing categories
#' @param option maximum age in the output table
#'
#' @return a table containing the same categorisation variables than the input table, plus prevalences by age
#'
#' @export ApproxPrevalenceTable
#'
#' @examples prevalenceApprox(prevalence = (FRDreesAPA2017 %>% filter(sex=="female",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,95,5)), agemin=60, agemax=100, weight=(FRInseePopulation %>% filter(sex=="female",year==2018,age0101>=60) %>% arrange(age0101))$popx)
#' @examples prevalenceApprox(prevalence = (FRDreesAPA %>% filter(year==2018,sex=="male",typepresta=="APA à domicile"))$prevalence, agecuts=c(seq(60,90,5)), agemin=60, agemax=100, weight=(FRInseePopulation %>% filter(sex=="male",year==2019,age0101>=60) %>% arrange(age0101))$popx)
ApproxPrevalenceTable <- function(tab,
                                  agecuts,agemin,agemax,
                                  weights.tab = NULL,
                                  categories = c("categ"),
                                  option = ""){

  # == if input tables are tibbles, transform into dataframe

  tab <- as.data.frame(tab)
  if (!is.null(weights.tab)) { weights.tab <- as.data.frame(weights.tab) }

  # == extract names of variables in input tables

    # -- in 'tab'

  categories <- intersect( names(tab), unique( c( categories, "year","sex","categ") ) )

  name.prev <- names(tab)[!(names(tab) %in% categories) & !(grepl("^(A|a|â)ge",names(tab)))]
  if (NROW(name.prev)>1) { stop("More than one prevalence variable in 'tab'")
  } else { message(paste0("'",name.prev,"' is used as prevalence variable."))}

    # -- in 'weights.tab'

  categories.w <- categories[categories %in% names(weights.tab) ]

  name.agew <- names(weights.tab)[grepl("^(A|a|â)ge",names(weights.tab))]

  name.w <- names(weights.tab)[!(names(weights.tab) %in% c(categories.w,name.agew))]
  if (NROW(name.w)>1) { stop("More than one weight variable in 'weights.tab'")}

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

  # == call approximation function for every category, sex and year

  approxloc <- function(i) {

    if (is.null(weights.tab)) { weights.loc <- rep(1,(agemax-agemin+1))
    } else { weights.loc <- weights.tab[weights.tab$categ.w == cases$categ.w[i],c(name.w)]   }

    approxloc <- function(...) {
      if (option %in% c("polynomial")) { prevalence_to_polynomial(...)
      } else { prevalenceApprox(...)    }
    }

    agebracket <- cut(c(agemin:agemax), breaks = c(agecuts,Inf), include.lowest = TRUE, right = FALSE)
    prevalence.raw <- tab[tab$categloc == cases$categloc[i],c(name.prev)]

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
                 stringsAsFactors = False),
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

