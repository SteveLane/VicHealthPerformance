################################################################################
################################################################################
## Title: Vic Health Hospital Performance
## Author: Steve Lane
## Date: Tuesday, 11 August 2015
## Synopsis: rvest and analyse Vic Health hospital performance data.
################################################################################
################################################################################
rm(list = ls())
require(rvest)
require(dplyr)
require(tidyr)
require(ggplot2)
################################################################################
################################################################################

################################################################################
################################################################################
## Begin Section: Get some data
################################################################################
################################################################################
## Category 2 Elective surgery patients
perf <-
    html_session("http://performance.health.vic.gov.au/Home/Report.aspx?ReportKey=59")
perfButton <- perf %>% html_form()
## Get the table
cat2Overdue <- submit_form(perf, perfButton[[2]]) %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table()
## Tidy the table up
cat2Overdue <- cat2Overdue %>%
    rename(Hospital = ) %>%
    filter(Hospital != "Total") %>%
    gather("Quarter", "Overdue", 2:6)
perf <-
    html_session("http://performance.health.vic.gov.au/Home/Report.aspx?ReportKey=57")
perfButton <- perf %>% html_form()
cat2OnTime <- submit_form(perf, perfButton[[2]]) %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table()
## Tidy the table up
cat2OnTime <- cat2OnTime %>%
    rename(Hospital = ) %>%
    filter(Hospital != "Total") %>%
    gather("Quarter", "OnTime", 2:6)
## Join tables together
cat2 <- left_join(cat2OnTime, cat2Overdue, by = c("Hospital", "Quarter"))
## Proportion treated within 90 days
cat2 <- cat2 %>% mutate(
    Overdue = as.numeric(gsub(",", "", Overdue)),
    OnTime = as.numeric(gsub(",", "", OnTime)),
    Total = OnTime + Overdue,
    prop = OnTime/Total
    )
################################################################################
################################################################################

################################################################################
################################################################################
## Begin Section: Funnel plot
################################################################################
################################################################################
## To plot the limits (per facet)
limFun <- function(pAll, x, q){
    pAll + qnorm(q)*sqrt(pAll*(1 - pAll)/x)
}
xLim <- with(cat2, seq(min(Total), max(Total), by = 1))
overall <- cat2 %>% group_by(Quarter) %>%
    summarise(pAll = mean(prop))
curves <- data_frame(
    Total = rep(xLim, nrow(overall)),
    prop1Upper = c(sapply(overall$pAll, limFun, x = xLim, q = 0.975)),
    prop1Lower = c(sapply(overall$pAll, limFun, x = xLim, q = 0.025)),
    prop2Upper = c(sapply(overall$pAll, limFun, x = xLim, q = 0.999)),
    prop2Lower = c(sapply(overall$pAll, limFun, x = xLim, q = 0.001)),
    Quarter = rep(levels(cat2$Quarter), each = length(xLim)))
pl1 <- ggplot(cat2, aes(x = Total, y = prop)) +
    geom_point() +
    geom_line(data = curves, aes(y = prop1Upper), colour = "blue", linetype = 2) +
    geom_line(data = curves, aes(y = prop1Lower), colour = "blue", linetype = 2) +
    geom_line(data = curves, aes(y = prop2Upper), colour = "blue", linetype = 3) +
    geom_line(data = curves, aes(y = prop2Lower), colour = "blue", linetype = 3) +
    geom_hline(data = overall, aes(yintercept = pAll), colour = "blue") +
    facet_wrap(~ Quarter, ncol = 2)
## Need to try and get ggobi going for brushing.
################################################################################
################################################################################
