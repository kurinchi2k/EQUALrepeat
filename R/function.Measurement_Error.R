globalVariables(c("descriptive_summary"), "EQUALrepeat", add = TRUE)
function.Measurement_Error <- function(Predefined_lists, rv){
  # Lists
  plan <- {cbind.data.frame(
    analysis_number = paste0("AN", formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
    first_menu_choice = rv$first_menu_choice,
    second_menu_choice = rv$second_menu_choice,
    entry_1 = paste0(rv$entry[[1]], collapse = "%_%"),
    entry_2 = paste0(rv$entry[[2]], collapse = "%_%"),
    entry_3 = paste0(rv$entry[[3]], collapse = "%_%"),
    entry_4 = paste0(rv$entry[[4]], collapse = "%_%"),
    entry_5 = paste0(rv$entry[[5]], collapse = "%_%"),
    entry_6 = paste0(rv$entry[[6]], collapse = "%_%"),
    entry_7 = paste0(rv$entry[[7]], collapse = "%_%"),
    entry_8 = paste0(rv$entry[[8]], collapse = "%_%"),
    entry_9 = paste0(rv$entry[[9]], collapse = "%_%"),
    entry_10 = paste0(rv$entry[[10]], collapse = "%_%"),
    entry_11 = paste0(rv$entry[[11]], collapse = "%_%"),
    entry_12 = paste0(rv$entry[[12]], collapse = "%_%"),
    entry_13 = paste0(rv$entry[[13]], collapse = "%_%"),
    entry_14 = paste0(rv$entry[[14]], collapse = "%_%"),
    entry_15 = paste0(rv$entry[[15]], collapse = "%_%"),
    same_row_different_row = ""
  )}
  selections <- {paste0(
    '<b>entry_1: </b>', paste0(rv$entry[[1]], collapse = "; "), '<br>',
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
    'rv$entry[[2]] <- ', ifelse(length(rv$entry[[2]]) > 1,
                                paste0('c("', paste0(rv$entry[[2]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[2]],'"')), '\n',
    'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_results <- function.',rv$first_menu_choice,'(Predefined_lists, rv)', '\n',
    if(length(rv$plan) == 0){
      'if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(AN0001_results$plots_list, paste0(AN0001_results$plots_list,"_copy")))}
'
    } else {
      paste0(
        'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results[2,1] <- "AN',formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'"', '\n',
        'if (TRUE %in% (AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list != "")) {invisible(file.rename(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, str_replace_all(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, "/AN0001_", "/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_")))}', '\n')
    },
    'write.table(x = AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results, append = TRUE, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")', '\n'
  )}
  # Get data
  data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
  data <- na.omit(data)
  # Do the analysis only if at least two rows are present
  if (nrow(data) >= 2) {
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      if (rv$entry[[1]] %in% rv$import_data$ordinal) {
        # If ordinal, Kendall correlation coefficient
        concordance_correlation_coefficient <- kendall(data, correct=TRUE)
        correlation_coefficient <- data.frame(
          `Correlation coefficient` = concordance_correlation_coefficient$value,
          `P value` = concordance_correlation_coefficient$p.value,
          check.names = FALSE
        )
        correlation_type <- "Concordance coefficient (Kendall)"
      } else if (nlevels(data[,1]) == 2 && nlevels(data[,2]) == 2) {
        # if both rv$variables are binary, Cohen's kappa
        concordance_correlation_coefficient <- kappa2(data, weight="unweighted",sort.levels=TRUE)
        correlation_coefficient <- data.frame(
          `Correlation coefficient` = concordance_correlation_coefficient$value,
          `P value` = concordance_correlation_coefficient$p.value,
          check.names = FALSE
        )
        correlation_type <- "Concordance coefficient (Cohen's kappa)"
      } else {
        # more than two levels - it is Fleiss kappa
        concordance_correlation_coefficient <- kappam.fleiss(data, exact = FALSE, detail = FALSE)
        correlation_coefficient <- data.frame(
          `Correlation coefficient` = concordance_correlation_coefficient$value,
          `P value` = concordance_correlation_coefficient$p.value,
          check.names = FALSE
        )
        correlation_type <- "Concordance coefficient (Fleiss kappa)"
      }
      plots_list <- ""
      display_plot <- FALSE
    } else {
      concordance_correlation_coefficient <- CCC(data[,1], data[,2], ci = "z-transform", conf.level = 0.95, na.rm= TRUE)
      correlation_coefficient <- concordance_correlation_coefficient$rho.c
      colnames(correlation_coefficient) <- c("Point estimate","Lower CI", "Upper CI")
      bland_altman <- concordance_correlation_coefficient$blalt
      mean_difference <- mean(bland_altman$delta)
      se_difference <- (var(bland_altman$delta))^0.5
      correlation_type <- "concordance_correlation_coefficient"
      plot_title <- paste0(rv$entry[[1]], " versus ", rv$entry[[2]], ": Bland-Altman plot")
      plot <- ggplot(data = bland_altman,aes(x = bland_altman[,1], y = bland_altman[,2]))+ geom_point() + xlab("Means") + ylab("Differences") + xlim(round_near(min(min(data[,1]),min(data[,2]))*0.5),round_near(max(max(data[,1]),max(data[,2]))*2)) + ylim(round_near(min(bland_altman[,2])*0.5),round_near(max(bland_altman[,2])*2)) + geom_hline(yintercept=mean_difference, lty = 1, col = "gray") + geom_hline(yintercept=mean_difference - (2 * se_difference), lty = 2, col = "gray") + geom_hline(yintercept=mean_difference + (2 * se_difference), lty = 2, col = "gray") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title)
      suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                                 substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_bland_altman__plot.png'),
                                               plot = plot)))
      plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                           substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'_bland_altman__plot.png')
      display_plot <- TRUE
    }
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Successful",
      `First variable` = rv$entry[[1]],
      `Second variable` = rv$entry[[2]],
      `Number of observations` = nrow(data),
      `Type of correlation coefficient` = correlation_type,
      correlation_coefficient,
      check.names = FALSE
    )
    plots_list_display <- plots_list
    analysis_outcome <- "Successful"
  } else {
    results_display <- data.frame(
      `Analysis number` = c(paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)), rep(NA, nrow(descriptive_summary)-1)),
      `Analysis type` = c(paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),rep(NA, nrow(descriptive_summary)-1)),
      `Analysis outcome` = c("Unsuccessful",  rep(NA, nrow(descriptive_summary)-1)),
      `First variable` = c(rv$entry[[1]], rep(NA, nrow(descriptive_summary)-1)),
      `Second variable` = c(rv$entry[[2]], rep(NA, nrow(descriptive_summary)-1)),
      `Reason for unsuccesful analysis` = "There were very few observations to perform an analysis. There must be at least two valid observations of each variable to perform a successful analysis.",
      check.names = FALSE
    )
    plots_list <- ""
    plots_list_display <- plots_list
    analysis_outcome <- "Unsuccessful"
    display_plot <- FALSE
  }
  results <- rbind.data.frame(
    colnames(results_display),
    results_display
  )
  display_table <- TRUE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
