\name{function.Time_Series}
\alias{function.Time_Series}
\title{
Analysis Time Series Data
}
\description{
Performs analysis of time series data and predict the variables over time. It takes into account the seasonality and trend of data and uses \pkg{forecast}, \pkg{vars}, \pkg{urca} to make the predictions. It uses \pkg{ggplot2} to create plots.
}
\usage{
function.Time_Series(Predefined_lists, rv)
}
\arguments{
\item{Predefined_lists}{A list supplied by 'EQUAL-STATS' application}
\item{rv}{A list supplied by 'EQUAL-STATS' application based on user input}
}
\value{
\item{analysis_outcome}{Whether the analysis was performed successfullly}
\item{plan}{Plan used for analysis}
\item{code}{Part of code generated for performing the analysis in a standalone version of R}
\item{results}{Analysis results}
\item{results_display}{In order to present a single table, multiple results are combined. This results in some numbers stored as text and can cause very wide tables in the shiny output. This issue is fixed wth some modifications to the results table for display purposes.}
\item{plots_list}{A list of plots generated. Returns "" if no plots are generated.}
\item{plots_list_display}{In the shiny application, only one figure is displayed. Therefore, a composite image is created from the plots for display purposes. Some analysis functions may return \code{NULL}.}
\item{selections}{Selections made by the user for display.}
\item{display_table}{Whether the results table should be displayed in the shiny app.}
\item{display_plot}{Whether the plot should be displayed in the shiny app.}
}
\references{
\url{https://sites.google.com/view/equal-group/home}
}
\author{
Kurinchi Gurusamy
}
\note{
This is part of a suite of functions required to allow 'EQUAL-STATS' program to run. This is unlikely to be used as a stand alone function.
}
\seealso{
function.submit_choices
\code{\link[stats:ts]{stats::ts()}}
\code{\link[forecast:auto.arima]{forecast::auto.arima()}}
\code{\link[forecast:forecast.fracdiff]{forecast::forecast.fracdiff()}}
\code{\link[forecast:findfrequency]{forecast::findfrequency()}}
\code{\link[forecast:ndiffs]{forecast::ndiffs()}}
\code{\link[tseries:adf.test]{tseries::adf.test()}}
\code{\link[urca:ca.jo]{urca::ca.jo()}}
\code{\link[vars:vec2var]{vars::vec2var()}}
\code{\link[vars:VAR]{vars::VAR()}}
\code{\link[viridisLite:viridis]{viridisLite::viridis()}}
\code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}
\code{\link[cowplot:plot_grid]{cowplot::plot_grid()}}
}
\concept{ EQUAL-STATS }
\examples{
# Create simulated data ####
data <- cbind.data.frame(
  Date = as.Date("2023-04-01") + 0:99,
  `Currency exchange rate`= rnorm(100, mean = 95, sd = 5)
)
# Simulate lists provided by EQUAL-STATS
Predefined_lists <- list(
  main_menu = c(
    'Calculate summary measures',
    'Create plots',
    'Check distribution',
    'Compare sample mean versus population mean',
    'Compare groups/variables (independent samples)',
    'Compare groups/variables (paired samples or repeated measures)',
    'Find the correlation (quantitative variables)',
    'Calculate measurement error',
    'Find the diagnostic accuracy (primary data)',
    'Perform sample size and power calculations (primary data)',
    'Perform survival analysis',
    'Perform regression analysis',
    'Analyse time series',
    'Perform mixed-effects regression',
    'Perform multivariate regression',
    'Generate hypothesis',
    'Perform sample size and power calculations (effect size approach)',
    'Make correct conclusions (effect size approach)',
    'Find the diagnostic accuracy (tabulated data)'
  ),
  menu_short = c(
    'Summary_Measures',
    'Create_Plots',
    'Check_Distribution',
    'Compare_Sample_Pop_Means',
    'Compare_Groups',
    'Repeated_Measures',
    'Correlation',
    'Measurement_Error',
    'Diagnostic_Accuracy_Primary',
    'Sample_Size_Calculations_Primary',
    'Survival_Analysis',
    'Regression_Analysis',
    'Time_Series',
    'Mixed_Effects_Regression',
    'Multivariate_Regression',
    'Generate_Hypothesis',
    'Sample_Size_Calculations_Effect_size',
    'Make_Conclusions_Effect_size',
    'Diagnostic_Accuracy_Tables'
  )
)
entry <- list()
entry <- lapply(1:15, function(x) entry[[x]] <- '')
rv <- list(
  StorageFolder = tempdir(),
  first_menu_choice = NA,
  second_menu_choice = NA,
  entry = entry,
  import_data = NULL,
  same_row_different_row = NA,
  submit_button_to_appear = FALSE,
  summary_measures_choices = c("EQUAL-STATS choice", "Total observations",
  "Missing observations", "Available observations"),
  analysis_outcome = list(),
  code = list(),
  plan = list(),
  results = list(),
  plots_list = list(),
  reports = list()
)
# Store the data in a folder
data_file_path = paste0(tempdir(), "/data.csv")
write.csv(data, file = data_file_path, row.names = FALSE, na = "")
# Load the necessary packages and functions ####
library(stringr)
library(forecast)
library(viridisLite)
library(cowplot)
library(ggplot2)
rv$import_data <- function.read_data(data_file_path)
rv$first_menu_choice <- "Time_Series"
rv$second_menu_choice <- NA
rv$entry[[1]] <- "Currency exchange rate"
rv$entry[[2]] <- "Date"
rv$entry[[3]] <- ""
rv$entry[[4]] <- ""
rv$entry[[5]] <- "Univariate"
rv$entry[[6]] <- 10
# Final function ####
Results <- function.Time_Series(Predefined_lists, rv)
}
