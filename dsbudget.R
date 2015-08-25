# Copyright (c) 2015 Ben Zimmer. All rights reserved.

# Functions for analyzing and visualizing data from dsBudget.

# 2015-08-19: Created.
# 2015-08-23: Correct results.
# 2015-08-24: Cleaned up and commented.


library(XML)
library(magrittr)
library(dplyr)



#' Load xml for all pages in a dsBudget XML.
#'
#' @param inputFile    name of dsBudget XML file
#' @param inputDir     containing directory of dsBudget XML file
#' @return list of page XML elements

loadPages <- function(inputFile, inputDir) {

  doc <- xmlTreeParse(paste0(inputDir, "/", inputFile))
  xmlRoot <- xmlRoot(doc)

  xmlChildren(xmlRoot) %>% Filter(function(x) {
    pageName <- xmlAttrs(x)["name"]
    !any(sapply(c("Sample", "New"), function(x) grepl(x, pageName, fixed = TRUE)))
  }, .)

}



#' Get a dataframe of transactions from a page XML element.
#'
#' @param page    page XML element
#' @return dataframe of transactions

pageToDataFrame <- function(page) {

  pageCtime <- as.integer(xmlAttrs(page)["ctime"])
  pageItems <- xmlChildren(page)

  # take just the category pages
  categories <- pageItems %>% Filter(function(x) xmlName(x) == "Category", .)

  # for each category, get the date and sum up the items
  result <- do.call(rbind, lapply(categories, function(x) {

    # print(x)

    attributes <- xmlAttrs(x)
    categoryName <- attributes["name"]
    categoryBudget <- as.integer(attributes["budget"]) / 100

    spends <- xmlChildren(x)

    if (length(spends) > 0) {

      spendDates <- sapply(spends, function(x) as.integer(xmlAttrs(x)["time"]))
      amounts <- sapply(spends, function(x) {
        0 - as.integer(xmlAttrs(x)["amount"]) / 100
      })
      wheres <- sapply(spends, function(x) {
        xmlAttrs(x)["where"]
      })

    } else {
      spendDates <- c()
      amounts <- c()
      wheres <- c()
    }

    data.frame(categoryName = categoryName,
               spendDate = c(pageCtime, spendDates),
               amount = c(categoryBudget, amounts),
               where = c("budget", wheres),
               pageName = xmlAttrs(page)["name"],
               stringsAsFactors = FALSE)

  }))

  result$spendDate <- as.Date(as.POSIXct(result$spendDate, origin = "1970-01-01"))
  result <- result[, c(5, 1, 2, 3, 4)]
  result <- result %>% filter(!grepl("(Balance from+)", where))

  rownames(result) <- NULL

  result

}



#' Plot savings or spending goals
#'
#' @param spends           dataframe of transactions
#' @param whichCategory    vector of categories to examine
#' @param goalAmount       goal amount to save or spend
#' @param goalEndDate      goal date
#' @param goalType         goal type (save or spend)
#' @return nothing

plotGoals <- function(spends, whichCategory, goalAmount, goalEndDate, goalType = "save") {

  curSpends <- spends %>%
    filter(categoryName %in% whichCategory) %>%
    arrange(spendDate)

  if (goalType == "save") {
    curSpends$cumAmount <- cumsum(curSpends$amount)
  } else {
    spends <- ifelse(curSpends$amount < 0, curSpends$amount, 0)
    curSpends$cumAmount <- cumsum(spends)
  }

  startDate <- as.Date(paste0(substr(head(curSpends$spendDate, 1), 1, 5), "01-01"))
  endDate <- as.Date(paste0(substr(goalEndDate + 365, 1, 5), "01-01"))

  plot(0, type = "n",
       xlim = c(startDate, endDate),
       ylim = range(c(curSpends$cumAmount, 0, goalAmount)),
       xaxt = "n",
       xlab = "date", ylab = "amount")

  axis.Date(side = 1, at = seq.Date(startDate, endDate, "quarter"), format = "%Y-%m-%d")

  abline(v = seq.Date(startDate, endDate, "month"), col = "lightgray")
  abline(v = seq.Date(startDate, endDate, "year"), col = "black")

  amountBy <- ifelse(goalType == "save", 1000, -1000)

  abline(h = seq(from = 0, to = goalAmount, by = amountBy), col = "lightgray")
  abline(h = c(0, goalAmount), col = "black")


  lines(curSpends$cumAmount ~ curSpends$spendDate,
        col = "blue", type = "o", lwd = 3)

  lines(c(tail(curSpends$spendDate, 1), goalEndDate),
        c(tail(curSpends$cumAmount, 1), goalAmount),
        col = "green", type = "o", lwd = "3")

  title(main = whichCategory[1])

}

