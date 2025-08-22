globalVariables(c("Time", "Average", "outcome", "subject_id", "time_of_measurement", "group"), "EQUALrepeat", add = TRUE)
function.Repeated_Measures <- function(Predefined_lists, rv){
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
    same_row_different_row = rv$same_row_different_row
  )}
  selections <- {paste0(
    '<b>entry_1: </b>', paste0(rv$entry[[1]], collapse = "; "), '<br>',
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>',
    '<b>entry_3: </b>', paste0(rv$entry[[3]], collapse = "; "), '<br>',
    '<b>entry_4: </b>', paste0(rv$entry[[4]], collapse = "; "), '<br>',
    '<b>entry_5: </b>', paste0(rv$entry[[5]], collapse = "; "), '<br>'
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
    'rv$entry[[3]] <- ', ifelse(length(rv$entry[[3]]) > 1,
                                paste0('c("', paste0(rv$entry[[3]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[3]],'"')), '\n',
    'rv$entry[[4]] <- ', ifelse(length(rv$entry[[4]]) > 1,
                                paste0('c("', paste0(rv$entry[[4]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[4]],'"')), '\n',
    'rv$entry[[5]] <- ', ifelse(length(rv$entry[[5]]) > 1,
                                paste0('c("', paste0(rv$entry[[5]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[5]],'"')), '\n',
    'rv$same_row_different_row <- "', rv$same_row_different_row, '"\n',
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
  # Check normality
  func.check.normality <- function(variable) {
    # Test normality through skewness, kurtosis, Shapiro-Wilk test, and Kolmogrov-Smirnov test
    # Skewness - the confidence intervals do not overlap (-0.5) to 0.5
    skewness <- suppressWarnings(try(Skew(variable, conf.level = 0.95), silent = TRUE))
    if (str_detect(skewness[[1]][1], "Error")) {
      skewness <- "Not possible to determine"
    } else if ((skewness[3] < (-0.5)) | (skewness[2] > 0.5)) {
      skewness <- "Non-normal"
    } else {
      skewness <- "No evidence that it is non-normal"
    }
    kurtosis <- suppressWarnings(try(Kurt(variable, conf.level = 0.95), silent = TRUE))
    if (str_detect(kurtosis[[1]][1], "Error")) {
      kurtosis <- "Not possible to determine"
    } else if ((kurtosis[2] + 3) > 3) { # kurtosis provided is excess kurtosis. To get the actual kurtosis add 3
      kurtosis <- "Non-normal"
    } else {
      kurtosis <- "No evidence that it is non-normal"
    }
    # Shapiro-Wilk test: This may not work for more than 5000 observations
    shapiro.wilk <- suppressWarnings(try(shapiro.test(variable[!is.na(variable)]), silent = TRUE))
    if (str_detect(shapiro.wilk[[1]][1], "Error")) {
      if (length(variable) > 5000) {
        shapiro.wilk <- "No evidence that it is non-normal"
      } else {
        shapiro.wilk <- "Not possible to determine"
      }
    } else if (shapiro.wilk$p.value <= 0.10) {
      shapiro.wilk <- "Non-normal"
    } else {
      shapiro.wilk <- "No evidence that it is non-normal"
    }
    ks <- suppressWarnings(ks.test(variable,"pnorm", mean=mean(variable), sd=sd(variable)))
    if (is.na(ks$p.value)) {
      ks <- "Not possible to determine"
    } else if (ks$p.value <= 0.10) {
      ks <- "Non-normal"
    } else {
      ks <- "No evidence that it is non-normal"
    }
    normality_results <- c(skewness, kurtosis, shapiro.wilk, ks)
    table(normality_results)
    # Now at least one is "Non-normal" or at least three have "Not possible to determine"
    if (
      ("Non-normal" %in% normality_results) |
      (length(normality_results[normality_results == "Not possible to determine"]) > 2)
    ) {
      distribution <- "Non-normal"
    } else {
      distribution <- "No evidence that it is non-normal"
    }
    return(distribution)
  }
  # Some generic processing of data to remove unrepresented factors
  func.keep_present_categories_only <- function(variable_values, is.ordinal) {
    factors_in_variable <- levels(variable_values)
    factors_in_variable <- factors_in_variable[! is.na(match(factors_in_variable, variable_values))]
    factor(as.character(variable_values), levels = factors_in_variable, ordered = is.ordinal)
  }
  # Summary
  func.summary.categorical <- function(variable, prefix) {
    if(nlevels(variable) == 2) {
      summary <- data.frame(sapply(
        1:nlevels(variable), function(x) {
          as.numeric(BinomCI(length(variable[(!is.na(variable)) & variable == levels(variable)[x]]), length(variable[! is.na(variable)]), conf.level = 0.95))
        }
      ), check.names = FALSE)
      colnames(summary) <- paste0(prefix, ": ", levels(variable))
      row.names(summary) <- c("Proportion", "Proportion - LCI", "Proportion - UCI")
    } else {
      summary <- data.frame(t(MultinomCI(table(variable), conf.level = 0.95)),row.names = c("Proportion", "Proportion - LCI", "Proportion - UCI"), check.names = FALSE)
      colnames(summary) <- paste0(prefix, ": ", colnames(summary))
    }
    return(summary)
  }
  func.summary.quantitative <- function(variable, prefix, normality) {
    if (length(variable) == 1) {
      summary <- c(prefix, variable, NA, NA)
    } else {
      if (normality == TRUE) {
        mean_ci <- suppressWarnings(try(MeanCI(variable, conf.level = 0.95), silent = TRUE))
        if (length(mean_ci) < 3) {mean_ci <- c(NA, NA, NA)}
        summary <- c(prefix, as.numeric(mean_ci))
      } else {
        median_ci <- suppressWarnings(try(MedianCI(variable, conf.level = 0.95), silent = TRUE))
        if (length(median_ci) < 3) {median_ci <- c(NA, NA, NA)}
        summary <- c(prefix, as.numeric(median_ci))
      }
    }
    return(summary)
  }
  # As there are multiple situations for unsuccessful outcome, create a function for unsuccessful outcome
  func.unsuccessful.outcome <- function(reason) {
    results_display <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      `Reason for unsuccesful analysis` = reason,
      check.names = FALSE
    )
    results <- rbind.data.frame(
      colnames(results_display),
      results_display
    )
    plots_list <- ""
    plots_list_display <- plots_list
    analysis_outcome <- "Unsuccessful"
    display_plot <- FALSE
    display_table <- TRUE
    function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  }
  func.successful.outcome <- function(data, test_results) {
    results <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Successful",
      check.names = FALSE
    )
    descriptive_summary_display <- descriptive_summary
    differences_display <- differences
    test_results_display <- test_results
    results_display <- results
    descriptive_summary_display <- data.frame(lapply(descriptive_summary_display[,1:ncol(descriptive_summary_display)], function(x){round(as.numeric(x),4)}))
    colnames(descriptive_summary_display) <- colnames(descriptive_summary)
    differences_display[,2:4] <- sapply(differences_display[,2:4], function(x){round(as.numeric(x),4)})
    if (rv$entry[[4]] != "") {
      test_results_display[,3:4] <- sapply(test_results_display[,3:4], function(x){round(as.numeric(x),4)})
    } else {
      test_results_display[,2:3] <- sapply(test_results_display[,2:3], function(x){round(as.numeric(x),4)})
    }
    results <- function.rbind_different_column_numbers(list(results, descriptive_summary, test_results, differences))
    results_display <- function.rbind_different_column_numbers(list(results_display, descriptive_summary_display, test_results_display, differences_display))
    if (rv$same_row_different_row == "Same row") {plot_title <- paste0(rv$entry[[2]], " versus ", rv$entry[[3]])} else {plot_title <- paste0(rv$entry[[2]], " over time")}
    if (rv$entry[[2]] %in% rv$import_data$categorical) {
      data_2 <- data.frame(table(data$outcome, data$time_of_measurement))
      if (rv$same_row_different_row == "Same row") {colnames(data_2)[1:2] <- c("Variable value", "Time")} else {colnames(data_2)[1:2] <- c(rv$entry[[2]],rv$entry[[3]])}
      plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    } else {
      if (rv$entry[[4]] != "") {
        data_2 <- data.frame(Average = as.numeric(t(descriptive_summary[,sequence(nlevels(data$time_of_measurement)*nlevels(data$group), 1, 3)])),
                             Time = unlist(lapply(1:nlevels(data$time_of_measurement), function(x) {rep(levels(data$time_of_measurement)[x],nlevels(data$group))})),
                             Group = rep(levels(data$group), nlevels(data$time_of_measurement))
        )
      } else {
        data_2 <- data.frame(Average = as.numeric(t(descriptive_summary[,sequence(nlevels(data$time_of_measurement), 1, 3)])), Time = levels(data$time_of_measurement))
      }
      lower_limit <- ifelse(min(data_2$Average) < 0, pretty(min(data_2$Average)), 0)
      upper_limit <- pretty(data_2$Average)[length(pretty(data_2$Average))]
      y_ticks <- pretty(lower_limit:upper_limit, n = 4)
      lower_limit <- min(y_ticks)
      upper_limit <- max(y_ticks)
      if (rv$entry[[4]] != "") {
        plot_title <- paste0(plot_title, "\n(by ", rv$entry[[4]], ")")
        each_plot <- list()
        data_3 <- list()
        for (i in 1:nlevels(data$group)) {
          data_3[[i]] <- data_2[data_2$Group == levels(data$group)[i],]
          each_plot[[i]] <- ggplot(data_3[[i]], aes(x = Time, y = Average, fill= Time)) + geom_bar(position = "dodge", stat="identity", width = 0.7, show.legend = FALSE) +
            theme(plot.title = element_text(color="navyblue", size=8, face="bold", hjust = 0.5)) +
            ylab("Mean") + labs(fill=levels(data_3[[i]]$Time)) + ggtitle(levels(data$group)[i]) + theme(axis.title.x = element_blank()) + scale_y_continuous(limits = c(lower_limit, upper_limit), breaks = y_ticks)
        }
        plot_title_combined <- ggdraw() + draw_label(plot_title, color="navyblue", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
        plot <- plot_grid(plot_title_combined, plot_grid(plotlist = each_plot, ncol = 2) + theme(plot.background = element_rect(fill = "white", colour = NA)), ncol = 1, rel_heights = c(0.1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))
      } else {
        plot <- ggplot(data_2, aes(x = Time, y = Average, fill = Time)) + geom_bar(position = "dodge", stat="identity") +
          theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) +
          ylab(ifelse(((repeated_measures_test == "Paired-sample T-test") | (repeated_measures_test == "Repeated Measures ANOVA")), "Mean", "Median")) +
          labs(fill=levels(data_2$Categories)) + ggtitle(plot_title) + theme(axis.title.x = element_blank()) + scale_y_continuous(limits = c(lower_limit, upper_limit), breaks = y_ticks)
      }
    }
    suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                                                               substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png'),
                                             plot = plot)))
    plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_',
                         substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 80) ,'.png')
    plots_list_display <- plots_list
    analysis_outcome <- "Successful"
    display_plot <- TRUE
    display_table <- TRUE
    function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  }
  func.ci_differences_paired_data_categorical <- function(data, alpha) {
    z = qnorm(1-alpha/2)
    # Convert data to wide format
    subject_id <- unique(data$subject_id)
    data_wide_list <- lapply(1:nlevels(data$time_of_measurement), function(y) {
      outcome <- data[data$time_of_measurement == levels(data$time_of_measurement)[y],1:2]
      colnames(outcome) <- c("subject_id", levels(data$time_of_measurement)[y])
      return(outcome)
    })
    data_wide <- data_wide_list[[1]]
    for (i in 2:nlevels(data$time_of_measurement)) {data_wide <- merge(data_wide, data_wide_list[[i]])}
    data_wide <- data_wide[,2:ncol(data_wide)]
    contingency_tables_list_1 <- lapply(2:nlevels(data$time_of_measurement), function(y) {
      data_contingency_table <- cbind.data.frame(data_wide[,1], data_wide[,y])
      contingency_table <- table(data_contingency_table)
    })
    names(contingency_tables_list_1) <- paste0(levels(data$time_of_measurement)[2:nlevels(data$time_of_measurement)], " vs ", levels(data$time_of_measurement)[1])
    # If the data is ordinal, cumulative proportions versus reference else each category versus reference
    if (rv$entry[[3]] %in% rv$import_data$ordinal) {
      contingency_tables_list_final <- lapply(1:length(contingency_tables_list_1), function(z) {
        contingency_table_nxn <- contingency_tables_list_1[[z]]
        contingency_table_2x2 <- lapply(2:nrow(contingency_table_nxn), function(x) {
          c(paste0(names(contingency_tables_list_1)[z], ": ",
                   if (x == 2) {
                     colnames(contingency_table_nxn)[2]
                   } else if (x == 3) {
                     paste0(colnames(contingency_table_nxn)[2:x], collapse = " or ")
                   } else {
                     paste0(paste0(colnames(contingency_table_nxn)[2:(x-1)], collapse = ", "), ", or ",  colnames(contingency_table_nxn)[x])
                   },
                   " vs ", colnames(contingency_table_nxn)[1]),
            sum(contingency_table_nxn[2:x,2:x]),
            sum(contingency_table_nxn[1,2:x]),
            sum(contingency_table_nxn[2:x,1]),
            contingency_table_nxn[1,1]
          )
        })
        if (nrow(contingency_table_nxn) > 2) {
          contingency_table_2x2 <- do.call(rbind.data.frame,contingency_table_2x2)
        } else {
          contingency_table_2x2 <- data.frame(matrix(data = contingency_table_2x2[[1]], nrow = 1, ncol = 5))
        }
        colnames(contingency_table_2x2) <- c("Comparison", "r", "s", "t", "u")
        return(contingency_table_2x2)
      })
    } else {
      contingency_tables_list_final <- lapply(1:length(contingency_tables_list_1), function(z) {
        contingency_table_nxn <- contingency_tables_list_1[[z]]
        contingency_table_2x2 <- lapply(2:nrow(contingency_table_nxn), function(x) {
          c(paste0(names(contingency_tables_list_1)[z], ": ", colnames(contingency_table_nxn)[x], " vs ", colnames(contingency_table_nxn)[1]),
            contingency_table_nxn[x,x],
            contingency_table_nxn[1,x],
            sum(contingency_table_nxn[x,1]),
            contingency_table_nxn[1,1]
          )
        })
        if (nrow(contingency_table_nxn) > 2) {
          contingency_table_2x2 <- do.call(rbind.data.frame,contingency_table_2x2)
        } else {
          contingency_table_2x2 <- data.frame(matrix(data = contingency_table_2x2[[1]], nrow = 1, ncol = 5))
        }
        colnames(contingency_table_2x2) <- c("Comparison", "r", "s", "t", "u")
        return(contingency_table_2x2)
      })
    }
    if (length(contingency_tables_list_final) == 1) {contingency_tables <- contingency_tables_list_final[[1]]} else {contingency_tables <- do.call(rbind.data.frame,contingency_tables_list_final)}
    contingency_tables[,c("r", "s", "t", "u")] <- lapply(contingency_tables[,c("r", "s", "t", "u")], as.numeric)
    contingency_tables$n <- rowSums(contingency_tables[,c("r", "s", "t", "u")])
    contingency_tables$pt <- (contingency_tables$r + contingency_tables$s)/contingency_tables$n
    contingency_tables$qt <- 1-contingency_tables$pt
    contingency_tables$At <- 2 * (contingency_tables$r + contingency_tables$s) + z^2
    contingency_tables$Bt <- z*(z^2 + 4 * (contingency_tables$r + contingency_tables$s) * contingency_tables$qt)^0.5
    contingency_tables$Ct <- 2 *(contingency_tables$n + z^2)
    contingency_tables$lt <- (contingency_tables$At - contingency_tables$Bt)/contingency_tables$Ct
    contingency_tables$ut <- (contingency_tables$At + contingency_tables$Bt)/contingency_tables$Ct
    contingency_tables$pc <- (contingency_tables$r + contingency_tables$t)/contingency_tables$n
    contingency_tables$qc <- 1-contingency_tables$pc
    contingency_tables$Ac <- 2 * (contingency_tables$r + contingency_tables$t) + z^2
    contingency_tables$Bc <- z*(z^2 + 4 * (contingency_tables$r + contingency_tables$t) * contingency_tables$qc)^0.5
    contingency_tables$Cc <- 2 *(contingency_tables$n + z^2)
    contingency_tables$lc <- (contingency_tables$Ac - contingency_tables$Bc)/contingency_tables$Cc
    contingency_tables$uc <- (contingency_tables$Ac + contingency_tables$Bc)/contingency_tables$Cc
    contingency_tables$D <- (contingency_tables$s - contingency_tables$t)/contingency_tables$n
    contingency_tables$phi <- sapply(1:nrow(contingency_tables), function(x){
      if (((contingency_tables$r[x] + contingency_tables$s[x]) == 0) | ((contingency_tables$t[x] + contingency_tables$u[x]) == 0) | ((contingency_tables$r[x] + contingency_tables$t[x]) == 0) | ((contingency_tables$s[x] + contingency_tables$u[x]) == 0)){
        0
      } else {
        A <- (contingency_tables$r[x] + contingency_tables$s[x]) * (contingency_tables$t[x] + contingency_tables$u[x]) * (contingency_tables$r[x] + contingency_tables$t[x]) * (contingency_tables$s[x] + contingency_tables$u[x])
        B <- (contingency_tables$r[x] * contingency_tables$u[x]) - (contingency_tables$s[x] * contingency_tables$t[x])
        if (B > contingency_tables$n[x]/2) {
          C <- B - contingency_tables$n[x]/2
        } else if (B < 0) {
          C <- B
        } else {
          C <- 0
        }
        C/A^0.5
      }
    })
    contingency_tables$LCI <- contingency_tables$D - ((contingency_tables$pt - contingency_tables$lt)^2 - 2 * contingency_tables$phi * (contingency_tables$pt - contingency_tables$lt) * (contingency_tables$uc - contingency_tables$pc) + (contingency_tables$uc - contingency_tables$pc)^2)^0.5
    contingency_tables$UCI <- contingency_tables$D + ((contingency_tables$pc - contingency_tables$lc)^2 - 2 * contingency_tables$phi * (contingency_tables$pc - contingency_tables$lc) * (contingency_tables$ut - contingency_tables$pt) + (contingency_tables$ut - contingency_tables$pt)^2)^0.5
    differences <- cbind.data.frame(
      Comparison = contingency_tables$Comparison,
      `Differences in proportion` = contingency_tables$D,
      `Differences in proportion - LCI` = contingency_tables$LCI,
      `Differences in proportion - UCI` = contingency_tables$UCI
    )
    return(differences)
  }
  func.ci_differences_paired_data_quantitative <- function(data, alpha, repeated_measures_test) {
    if ((repeated_measures_test == "Paired-sample T-test") | (repeated_measures_test == "Repeated Measures ANOVA")) {normality <- TRUE} else {normality <- FALSE}
    if (normality == TRUE) {
      differences <- lapply(2:nlevels(data$time_of_measurement), function(x) {
        test_results <- t.test(data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[1]], data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[x]], paired = TRUE, alpha = alpha)
        c(paste0(if(rv$same_row_different_row == "Same row") {paste0(rv$entry[[3]], " vs ", rv$entry[[2]])} else {rv$entry[[2]]}, ": ",
                 levels(data$time_of_measurement)[x], " vs ", levels(data$time_of_measurement)[1]
        ),
        -test_results$estimate,
        -test_results$conf.int[2],
        -test_results$conf.int[1]
        )
      })
      if (length(differences) > 1) {differences <- do.call(rbind.data.frame, differences)} else {differences <- data.frame(matrix(data = differences[[1]], nrow = 1, ncol = 4))   }
      colnames(differences) <- c("Difference in means: Comparison", "Mean difference", "Mean difference - LCI", "Mean difference - UCI")
    } else {
      # Convert data to wide format
      subject_id <- unique(data$subject_id)
      data_wide_list <- lapply(1:nlevels(data$time_of_measurement), function(y) {
        outcome <- data[data$time_of_measurement == levels(data$time_of_measurement)[y],1:2]
        colnames(outcome) <- c("subject_id", levels(data$time_of_measurement)[y])
        return(outcome)
      })
      data_wide <- data_wide_list[[1]]
      for (i in 2:nlevels(data$time_of_measurement)) {data_wide <- merge(data_wide, data_wide_list[[i]])}
      data_wide <- data_wide[,2:ncol(data_wide)]
      differences <- lapply(2:nlevels(data$time_of_measurement), function(y) {
        differences_individual <- data_wide[,y] - data_wide[,1]
        median_ci <- suppressWarnings(try(MedianCI(differences_individual, conf.level = (1- alpha), method = "exact", na.rm = TRUE), silent = TRUE))
        if (str_detect(median_ci[[1]][1], "Error")) {
          c(paste0(if(rv$same_row_different_row == "Same row") {paste0(rv$entry[[3]], " vs ", rv$entry[[2]])} else {rv$entry[[2]]}, ": ",
                   levels(data$time_of_measurement)[y], " vs ", levels(data$time_of_measurement)[1]
          ),
          rep("Not estimable", 3)
          )
        } else {
          c(paste0(if(rv$same_row_different_row == "Same row") {paste0(rv$entry[[3]], " vs ", rv$entry[[2]])} else {rv$entry[[2]]}, ": ",
                   levels(data$time_of_measurement)[y], " vs ", levels(data$time_of_measurement)[1]
          ),
          as.numeric(median_ci)
          )
        }
      })
      if (length(differences) > 1) {differences <- do.call(rbind.data.frame, differences)} else {differences <- data.frame(matrix(data = differences[[1]], nrow = 1, ncol = 4))   }
      colnames(differences) <- c("Difference in medians: Comparison", "Median difference", "Median difference - LCI", "Median difference - UCI")
    }
    return(differences)
  }
  if (rv$entry[[5]] == "") {alpha = 0.05} else {alpha = as.numeric(rv$entry[[5]])}
  # Now with the data
  data_initial <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]], rv$entry[[3]], if(rv$entry[[4]]!=""){rv$entry[[4]]})]
  data_initial <- na.omit(data_initial)
  # Convert all data to long format
  if (rv$same_row_different_row == "Same row") {
    data = cbind.data.frame(
      subject_id = as.factor(c(data_initial[,1], data_initial[,1])),
      outcome = c(data_initial[,2], data_initial[,3]),
      time_of_measurement = as.factor(c(rep(rv$entry[[2]], nrow(data_initial)), rep(rv$entry[[3]], nrow(data_initial))))
    )
    if(rv$entry[[4]]!="") {data$group <- c(data_initial[,4], data_initial[,4])}
  } else {
    data <- data.frame(data_initial)
    if (! is.factor(data[,1])) {factor(data[,1])}
    colnames(data) <- c("subject_id", "outcome", "time_of_measurement", if(rv$entry[[4]]!="") {"group"})
  }
  # Do the analysis only if at least 2 subjects
  if (nlevels(data$subject_id) >= 2) {
    # Keep only the present categories
    if (rv$entry[[2]] %in% rv$import_data$categorical) {data$outcome <- func.keep_present_categories_only(variable_values = data$outcome, is.ordinal = (rv$entry[[2]] %in% rv$import_data$ordinal))}
    data$time_of_measurement <- func.keep_present_categories_only(variable_values = data$time_of_measurement, is.ordinal = TRUE)
    if(rv$entry[[4]]!="") {data$group <- func.keep_present_categories_only(variable_values = data$group, is.ordinal = (rv$entry[[2]] %in% rv$import_data$ordinal))}
    # If EQUAL-STATS choice, find which test to perform
    if (rv$second_menu_choice == "EQUAL-STATS choice") {
      if (rv$entry[[2]] %in% rv$import_data$categorical) {
        if ((nlevels(data$outcome) == 2) & (nlevels(data$time_of_measurement) == 2)) {
          repeated_measures_test <- "McNemar test"
        } else {
          repeated_measures_test <- "Cochran Q-test"
        }
      } else {
        distribution <- func.check.normality(data$outcome)
        if (nlevels(data$time_of_measurement) == 2) {
          # If normal, paired T-test else Wilcoxon signed-rank test
          if (distribution == "No evidence that it is non-normal"){
            repeated_measures_test <- "Paired-sample T-test"
          } else {
            repeated_measures_test <- "Wilcoxon signed-rank test"
          }
        } else {
          # If normal, ANOVA else Friedman test
          if (distribution == "No evidence that it is non-normal"){
            repeated_measures_test <- "Repeated Measures ANOVA"
          } else {
            repeated_measures_test <- "Friedman test"
          }
        }
      }
    } else {
      repeated_measures_test <- rv$second_menu_choice
    }
    if (repeated_measures_test != "Repeated Measures ANOVA") {
      if (rv$entry[[2]] %in% rv$import_data$categorical) {
        descriptive_summary <- lapply(1:nlevels(data$time_of_measurement), function(x) {
          summary <- func.summary.categorical(variable = data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[x]], prefix = levels(data$time_of_measurement)[x])
          summary_all_rows <- data.frame(t(summary), check.names = FALSE)
          summary_each_row <- lapply(1:nrow(summary_all_rows), function(z) {
            each_row <- cbind.data.frame(summary_all_rows[z,])
            colnames(each_row) <- paste0(row.names(each_row), " (", colnames(each_row), ")")
            return(each_row)
          })
          summary <- do.call(cbind.data.frame, summary_each_row)
        })
        descriptive_summary <- do.call(cbind.data.frame, descriptive_summary)
        differences <- func.ci_differences_paired_data_categorical(data, alpha)
      } else {
        descriptive_summary <- lapply(1:nlevels(data$time_of_measurement), function(x) {
          summary <- func.summary.quantitative(variable = data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[x]],
                                               prefix = levels(data$time_of_measurement)[x],
                                               normality = (repeated_measures_test == "Paired-sample T-test")
          )
          summary_data_frame <- cbind.data.frame(summary[2], summary[3], summary[4])
          if (repeated_measures_test == "Paired-sample T-test") {
            colnames(summary_data_frame) <- paste0(summary[1], ": ", c("Mean", "Mean - LCI", "Mean - UCI"))
          } else {
            colnames(summary_data_frame) <- paste0(summary[1], ": ", c("Median", "Median - LCI", "Median - UCI"))
          }
          return(summary_data_frame)
        })
        descriptive_summary <- do.call(cbind.data.frame, descriptive_summary)
        differences <- func.ci_differences_paired_data_quantitative(data, alpha, repeated_measures_test)
      }
    } else {
      if (rv$entry[[4]] != "") {
        descriptive_summary <- lapply(1:nlevels(data$time_of_measurement), function(x) {
          summary_each_group <- lapply(1:nlevels(data$group), function(z) {
            summary <- func.summary.quantitative(variable = data$outcome[(data$time_of_measurement == levels(data$time_of_measurement)[x]) & (data$group == levels(data$group)[z])],
                                                 prefix = paste0("Group - ", levels(data$group)[z], " & ", "Time - ", levels(data$time_of_measurement)[x]),
                                                 normality = TRUE
            )
            summary_data_frame <- cbind.data.frame(summary[2], summary[3], summary[4])
            colnames(summary_data_frame) <- paste0(summary[1], ": ", c("Mean", "Mean - LCI", "Mean - UCI"))
            return(summary_data_frame)
          })
          summary_each_group <- do.call(cbind.data.frame, summary_each_group)
          return(summary_each_group)
        })
        descriptive_summary <- do.call(cbind.data.frame, descriptive_summary)
        differences <- lapply(1:nlevels(data$group), function(z) {
          each_group <- func.ci_differences_paired_data_quantitative(data[data$group == levels(data$group)[z],], alpha, repeated_measures_test)
          each_group[,1] <- paste0(levels(data$group)[z], ": ", paste0(levels(data$time_of_measurement)[2:nlevels(data$time_of_measurement)]), " vs ", levels(data$time_of_measurement)[1])
          return(each_group)
        })
        differences <- do.call(rbind.data.frame, differences)
      } else {
        descriptive_summary <- lapply(1:nlevels(data$time_of_measurement), function(x) {
          summary <- func.summary.quantitative(variable = data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[x]],
                                               prefix = levels(data$time_of_measurement)[x],
                                               normality = TRUE
          )
          summary_data_frame <- cbind.data.frame(summary[2], summary[3], summary[4])
          colnames(summary_data_frame) <- paste0(summary[1], ": ", c("Mean", "Mean - LCI", "Mean - UCI"))
          return(summary_data_frame)
        })
        descriptive_summary <- do.call(cbind.data.frame, descriptive_summary)
        differences <- func.ci_differences_paired_data_quantitative(data, alpha, repeated_measures_test)
      }
    }
    if (repeated_measures_test == "McNemar test") {
      if (nlevels(data$time_of_measurement) == 2) {
        test_results <- mcnemar.test(data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[1]], data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[2]], correct = TRUE)
        test_results <- cbind.data.frame(
          Test = repeated_measures_test,
          `Test statistic` = test_results$statistic,
          `P value` = test_results$p.value
        )
        function_output <- func.successful.outcome(data, test_results)
      } else {
        function_output <- func.unsuccessful.outcome("The number of timepoints should be exactly two. Please try Cochran Q-test if the number of time points is more than two. There must be at least two valid observations at each timepoint.")
      }
    } else if (repeated_measures_test == "Cochran Q-test") {
      test_results <- suppressMessages(suppressWarnings(try(CochranQTest(outcome ~ time_of_measurement | subject_id, data=data), silent = TRUE)))
      if (suppressWarnings(str_detect(test_results[[1]][1],"Error"))) {
        function_output <- func.unsuccessful.outcome("There was an error in running the test. This may be due to the values of the data.")
      } else {
        test_results <- cbind.data.frame(
          Test = repeated_measures_test,
          `Test statistic` = test_results$statistic,
          `P value` = test_results$p.value
        )
        function_output <- func.successful.outcome(data, test_results)
      }
    } else if (repeated_measures_test == "Paired-sample T-test") {
      if (nlevels(data$time_of_measurement) == 2) {
        test_results <- t.test(data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[1]], data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[2]], paired = TRUE)
        test_results <- cbind.data.frame(
          Test = repeated_measures_test,
          `Test statistic` = test_results$statistic,
          `P value` = test_results$p.value
        )
        function_output <- func.successful.outcome(data, test_results)
      } else {
        function_output <- func.unsuccessful.outcome("The number of timepoints should be exactly two. Please try Repeated Measures ANOVA if the number of time points is more than two. There must be at least two valid observations at each timepoint.")
      }
    } else if (repeated_measures_test == "Repeated Measures ANOVA") {
      if (rv$entry[[4]] != "") {
        test_results <- suppressMessages(suppressWarnings(try(rstatix::get_anova_table(rstatix::anova_test(data = data, dv = outcome, wid = subject_id, within = time_of_measurement, between = group)), silent = TRUE)))
        if (suppressWarnings(str_detect(test_results[[1]][1],"Error"))) {
          function_output <- func.unsuccessful.outcome("There was an error in running the test. This may be due to the values of the data.")
        } else {
          test_results <- cbind.data.frame(
            Test = c(repeated_measures_test, rep(NA,2)),
            Variable = c(rv$entry[[4]], paste0("Change over time: ", rv$entry[[3]], " vs ", rv$entry[[2]]), paste0("Difference in change over time: ", rv$entry[[3]], " vs ", rv$entry[[2]], " between different categories of ", rv$entry[[4]])),
            `Test statistic` = test_results$F,
            `P value` = test_results$p
          )
          function_output <- func.successful.outcome(data, test_results)
        }
      } else {
        test_results <- suppressMessages(suppressWarnings(try(rstatix::get_anova_table(rstatix::anova_test(data = data, dv = outcome, wid = subject_id, within = time_of_measurement)), silent = TRUE)))
        if (suppressWarnings(str_detect(test_results[[1]][1],"Error"))) {
          function_output <- func.unsuccessful.outcome("There was an error in running the test. This may be due to the values of the data.")
        } else {
          test_results <- cbind.data.frame(
            Test = repeated_measures_test,
            Variable = paste0("Change over time: ", rv$entry[[3]], " vs ", rv$entry[[2]]),
            `Test statistic` = test_results$F,
            `P value` = test_results$p
          )
          function_output <- func.successful.outcome(data, test_results)
        }
      }
    } else if (repeated_measures_test == "Wilcoxon signed-rank test") {
      if (nlevels(data$time_of_measurement) == 2) {
        test_results <- suppressWarnings(wilcox.test(data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[1]], data$outcome[data$time_of_measurement == levels(data$time_of_measurement)[2]], paired = TRUE))
        test_results <- cbind.data.frame(
          Test = repeated_measures_test,
          `Test statistic` = test_results$statistic,
          `P value` = test_results$p.value
        )
        function_output <- func.successful.outcome(data, test_results)
      } else {
        function_output <- func.unsuccessful.outcome("The number of timepoints should be exactly two. Please try Friedman test if the number of time points is more than two. There must be at least two valid observations at each timepoint.")
      }
    } else if (repeated_measures_test == "Friedman test") {
      test_results <- suppressMessages(suppressWarnings(try(friedman.test(outcome ~ time_of_measurement | subject_id, data=data), silent = TRUE)))
      if (suppressWarnings(str_detect(test_results[[1]][1],"Error"))) {
        function_output <- func.unsuccessful.outcome("There was an error in running the test. This may be due to the values of the data.")
      } else {
        test_results <- cbind.data.frame(
          Test = repeated_measures_test,
          `Test statistic` = test_results$statistic,
          `P value` = test_results$p.value
        )
        function_output <- func.successful.outcome(data, test_results)
      }
    }
  } else {
    function_output <- func.unsuccessful.outcome("Insufficient number of observations")
  }
  return(function_output)
}
