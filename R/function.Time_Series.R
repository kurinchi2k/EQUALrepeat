globalVariables(c("ACF", "Lag"), "EQUALrepeat", add = TRUE)
function.Time_Series <- function(Predefined_lists, rv){
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
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>',
    '<b>entry_3: </b>', paste0(rv$entry[[3]], collapse = "; "), '<br>',
    '<b>entry_4: </b>', paste0(rv$entry[[4]], collapse = "; "), '<br>',
    '<b>entry_5: </b>', paste0(rv$entry[[5]], collapse = "; "), '<br>',
    '<b>entry_6: </b>', paste0(rv$entry[[6]], collapse = "; "), '<br>'
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
    'rv$entry[[6]] <- ', paste0(rv$entry[[6]], '\n'),
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
  func.recognise_date <- function(data) {
    lapply(1:ncol(data), function(y) {
      if (is.numeric(data[,y])) {
        field_type <- "numeric"
        converted <- NA
      } else {
        character <- as.character(data[,y])
        character <- character[! is.na(character)]
        if (length(character) > 0) {
          # Number of characters must be 10 in a date field
          characters_count <- nchar(character)
          if (length(unique(characters_count)) > 1) {
            field_type <- "non-date character field"
            converted <- NA
          } else if (unique(characters_count) != 10) {
            field_type <- "non-date character field"
            converted <- NA
          } else {
            # If forward slash is present, it must be exactly two
            forward_slash_count <- str_count(character, "/")
            if (length(unique(forward_slash_count)) > 1) {
              field_type <- "non-date character field"
              converted <- NA
            } else if (unique(forward_slash_count) != 2) {
              # If hyphen is present, it must be exactly two
              hyphen_count <- str_count(character, "-")
              if (length(unique(hyphen_count)) > 1) {
                field_type <- "non-date character field"
                converted <- NA
              } else if (unique(hyphen_count) != 2) {
                # If dot is present, it must be exactly two
                dot_count <- str_count(character, "\\.")
                if (length(unique(dot_count)) > 1) {
                  field_type <- "non-date character field"
                  converted <- NA
                } else if (unique(dot_count) != 2) {
                  field_type <- "non-date character field"
                  converted <- NA
                } else {
                  field_split <- do.call(rbind.data.frame, str_split(character,"\\."))
                  colnames(field_split) <- paste0("X",1:3)
                  # Number of characters
                  nchar1 <- nchar(field_split$X1)
                  nchar2 <- nchar(field_split$X2)
                  nchar3 <- nchar(field_split$X3)
                  unique_nchar1 <- unique(nchar1)
                  unique_nchar2 <- unique(nchar2)
                  unique_nchar3 <- unique(nchar3)
                  if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                    field_type <- "non-date character field"
                    converted <- NA
                  } else {
                    combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                    if (combination == "224") {
                      # Check whether the first field or second field is month
                      converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                      if (str_detect(converted[[1]][1], "Error")) {
                        converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                      }
                      if (str_detect(converted[[1]][1], "Error")) {
                        field_type <- "non-date character field"
                        converted <- NA
                      } else {
                        field_type <- "date"
                      }
                    } else if (combination == "422") {
                      converted <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                      if (str_detect(converted[[1]][1], "Error")) {
                        field_type <- "non-date character field"
                        converted <- NA
                      } else {
                        field_type <- "date"
                      }
                    } else {
                      field_type <- "non-date character field"
                      converted <- NA
                    }
                  }
                }
              } else {
                field_split <- do.call(rbind.data.frame, str_split(character,"-"))
                colnames(field_split) <- paste0("X",1:3)
                # Number of characters
                nchar1 <- nchar(field_split$X1)
                nchar2 <- nchar(field_split$X2)
                nchar3 <- nchar(field_split$X3)
                unique_nchar1 <- unique(nchar1)
                unique_nchar2 <- unique(nchar2)
                unique_nchar3 <- unique(nchar3)
                if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                  field_type <- "non-date character field"
                  converted <- NA
                } else {
                  combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                  if (combination == "224") {
                    # Check whether the first field or second field is month
                    converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                    if (str_detect(converted[[1]][1], "Error")) {
                      converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                    }
                    if (str_detect(converted[[1]][1], "Error")) {
                      field_type <- "non-date character field"
                      converted <- NA
                    } else {
                      field_type <- "date"
                    }
                  } else if (combination == "422") {
                    converted <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                    if (str_detect(converted[[1]][1], "Error")) {
                      field_type <- "non-date character field"
                      converted <- NA
                    } else {
                      field_type <- "date"
                    }
                  } else {
                    field_type <- "non-date character field"
                    converted <- NA
                  }
                }
              }
            } else {
              field_split <- do.call(rbind.data.frame, str_split(character,"/"))
              colnames(field_split) <- paste0("X",1:3)
              # Number of characters
              nchar1 <- nchar(field_split$X1)
              nchar2 <- nchar(field_split$X2)
              nchar3 <- nchar(field_split$X3)
              unique_nchar1 <- unique(nchar1)
              unique_nchar2 <- unique(nchar2)
              unique_nchar3 <- unique(nchar3)
              if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                field_type <- "non-date character field"
                converted <- NA
              } else {
                combination <- paste0(unique_nchar1, unique_nchar2, unique_nchar3)
                if (combination == "224") {
                  # Check whether the first field or second field is month
                  converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X2, "-", field_split$X1), tz = "GMT"), silent = TRUE)
                  if (str_detect(converted[[1]][1], "Error")) {
                    converted <- try(as.POSIXct(paste0(field_split$X3, "-", field_split$X1, "-", field_split$X2), tz = "GMT"), silent = TRUE)
                  }
                  if (str_detect(converted[[1]][1], "Error")) {
                    field_type <- "non-date character field"
                    converted <- NA
                  } else {
                    field_type <- "date"
                  }
                } else if (combination == "422") {
                  converted <- try(as.POSIXct(paste0(field_split$X1, "-", field_split$X2, "-", field_split$X3), tz = "GMT"), silent = TRUE)
                  if (str_detect(converted[[1]][1], "Error")) {
                    field_type <- "non-date character field"
                    converted <- NA
                  } else {
                    field_type <- "date"
                  }
                } else {
                  field_type <- "non-date character field"
                  converted <- NA
                }
              }
            }
          }
        } else {
          field_type <- "Unknown"
          converted <- NA
        }
      }
      output <- list(field_type = field_type, converted = converted)
      return(output)
    })
  }
  func.recognise_time <- function(data) {
    sapply(1:ncol(data), function(y) {
      if (is.numeric(data[,y])) {
        time <- FALSE
      } else {
        character <- as.character(data[,y])
        character <- character[! is.na(character)]
        if (length(character) > 0) {
          characters_count <- nchar(character)
          if (length(unique(characters_count)) > 1) {
            time <- FALSE
          } else if (unique(characters_count) != 8) {
            time <- FALSE
          } else {
            colon_count <- str_count(character, ":")
            if (length(unique(colon_count)) > 1) {
              time <- FALSE
            } else if (unique(colon_count) != 2) {
              time <- FALSE
            } else {
              field_split <- do.call(rbind.data.frame, str_split(character,":"))
              colnames(field_split) <- paste0("X",1:3)
              # Number of characters
              nchar1 <- nchar(field_split$X1)
              nchar2 <- nchar(field_split$X2)
              nchar3 <- nchar(field_split$X3)
              unique_nchar1 <- unique(nchar1)
              unique_nchar2 <- unique(nchar2)
              unique_nchar3 <- unique(nchar3)
              if ((length(unique_nchar1) > 1) | (length(unique_nchar2) > 1) | (length(unique_nchar3) > 1)) {
                time <- FALSE
              } else if ((unique_nchar1 != 2) | (unique_nchar2 != 2) | (unique_nchar1 != 2)) {
                time <- FALSE
              } else if ((TRUE %in% (field_split$X1 > 23)) | (TRUE %in% (field_split$X2 > 59)) | (TRUE %in% (field_split$X1 > 59))) {
                time <- FALSE
              } else {
                time <- TRUE
              }
            }
          }
        }
      }
    })
  }
  func.convert_to_date <- function(number) {
    x <- as.POSIXlt.numeric(number, tz = "GMT")
    attr(x, "tzone") <- "GMT"
    as.POSIXct(x)
  }
  func.time_series_data <- function(data) {
    output <- list(outcome = NA, start = NA, interval = NA)
    if (TRUE %in% (is.na(data[,rv$entry[[2]]]))) {
      output <- list(outcome = "Unsuccessful", start = NA, interval = NA, date = "There cannot be missing values in the date. Please try again with corrected data or another date field.")
    } else if (rv$entry[[3]] != "") {
      if (TRUE %in% (is.na(data[,rv$entry[[3]]]))) {
        output <- list(outcome = "Unsuccessful", start = NA, interval = NA, date = "There cannot be missing values in the time. Please try again with corrected data or another time field.")
      }
    }
    if (is.na(output$outcome)) {
      if (rv$entry[[3]] != "") {
        date <- as.numeric(data[,rv$entry[[2]]]) + as.numeric(data[,rv$entry[[3]]])
      } else {
        date <- as.numeric(data[,rv$entry[[2]]])
      }
      difference <- sapply(2:length(date), function(x) {
        as.numeric(date[x]) - as.numeric(date[x-1])
      })
      average_difference <- mean(difference)
      output <- list(outcome = "Successful", start = date[1], interval = average_difference, date = date)
    }
    return(output)
  }
  func.determine_x_ticks <- function(lower_limit, upper_limit, interval){
    if (interval < 86400) {
      difference_in_minutes <- (upper_limit - lower_limit)/60
      # If difference is 12 hours or more, every 6 hours
      if (difference_in_minutes >= (12*60)) {
        lower_limit <- as.numeric(as.POSIXct(format(func.convert_to_date(lower_limit), "%Y-%m-%d"), tz="GMT"))
        x_ticks <- lower_limit + (3600 * c(0, 6, 12, 18, 24))
        x_labels <- c(paste0(format(func.convert_to_date(lower_limit), "%e"), "-", month.abb[as.numeric(format(func.convert_to_date(lower_limit), "%m"))], " ", c("00:00", "06:00", "12:00", "18:00")),
                      paste0(format(func.convert_to_date(lower_limit + 86400), "%e"), "-", month.abb[as.numeric(format(func.convert_to_date(lower_limit + 86400), "%m"))], " ", "00:00"))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If difference is 4 hours or more, every 2 hours
      } else if (difference_in_minutes >= (4*60)) {
        closest_even_hour <- as.numeric(format(func.convert_to_date(lower_limit), "%H"))
        closest_even_hour <- ifelse((closest_even_hour %% 2) == 1, closest_even_hour - 1, closest_even_hour - 2)
        if (closest_even_hour < 0) {
          date <- format(func.convert_to_date(lower_limit - 86400), "%Y-%m-%d")
          closest_even_hour <- (24 + closest_even_hour)
        } else {
          date <- format(func.convert_to_date(lower_limit), "%Y-%m-%d")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date, " ", formatC(closest_even_hour, width = 2, flag = "0"), ":00:00"), tz="GMT"))
        x_ticks <- lower_limit + (3600 * c(0, 4, 8, 12, 16, 20))
        x_labels <- sapply(x_ticks, function(x) {
          paste0(format(func.convert_to_date(x), "%e"), "-", month.abb[as.numeric(format(func.convert_to_date(x), "%m"))], " ", format(func.convert_to_date(x), "%H"), ":00")
        })
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If difference is 1 hour or more, every hour
      } else if (difference_in_minutes >= 60) {
        closest_hour <- as.numeric(format(func.convert_to_date(lower_limit), "%H")) - 1
        if (closest_hour < 0) {
          date <- format(func.convert_to_date(lower_limit - 86400), "%Y-%m-%d")
          closest_hour <- (24 + closest_hour)
        } else {
          date <- format(func.convert_to_date(lower_limit), "%Y-%m-%d")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date, " ", formatC(closest_hour, width = 2, flag = "0"), ":00:00"), tz="GMT"))
        x_ticks <- lower_limit + (3600 * c(0, 1, 2, 3, 4, 5))
        x_labels <- sapply(x_ticks, function(x) {
          paste0(format(func.convert_to_date(x), "%e"), "-", month.abb[as.numeric(format(func.convert_to_date(x), "%m"))], " ", format(func.convert_to_date(x), "%H"), ":00")
        })
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If difference is 10 minutes or more, every 10 minutes
      } else if (difference_in_minutes >= 10) {
        closest_10_minutes <- (round(as.numeric(format(func.convert_to_date(lower_limit), "%M")),-1) - 10)
        if (closest_10_minutes < 0) {
          date_hour <- format(func.convert_to_date(lower_limit - 3600), "%Y-%m-%d %H")
          closest_10_minutes <- (60 + closest_10_minutes)
        } else {
          date_hour <- format(func.convert_to_date(lower_limit), "%Y-%m-%d %H")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date_hour, ":", formatC(closest_10_minutes, width = 2, flag = "0"), ":00"), tz="GMT"))
        x_ticks <- lower_limit + (60 * c(0, 10, 20, 30, 40, 50, 60, 70))
        x_labels <- sapply(x_ticks, function(x) {format(func.convert_to_date(x), "%H:%M")})
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If difference is 1 minute or more, every 2 minutes
      } else if (difference_in_minutes >= 1) {
        closest_even_minutes <- round(as.numeric(format(func.convert_to_date(lower_limit), "%M")))
        closest_even_minutes <- ifelse((closest_even_minutes %% 2) == 1, closest_even_minutes - 1, closest_even_minutes - 2)
        if (closest_even_minutes < 0) {
          date_hour <- format(func.convert_to_date(lower_limit - 3600), "%Y-%m-%d %H")
          closest_even_minutes <- (60 + closest_even_minutes)
        } else {
          date_hour <- format(func.convert_to_date(lower_limit), "%Y-%m-%d %H")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date_hour, ":", formatC(closest_even_minutes, width = 2, flag = "0"), ":00"), tz="GMT"))
        x_ticks <- lower_limit + (60 * c(0, 2, 4, 6, 8, 10, 12))
        x_labels <- sapply(x_ticks, function(x) {format(func.convert_to_date(x), "%H:%M")})
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If difference is 10 seconds or more, every 10 seconds
      } else if (interval >= 10) {
        closest_10_seconds <- (round(as.numeric(format(func.convert_to_date(lower_limit), "%S")),-1) - 10)
        if (closest_10_seconds < 0) {
          date_hour_min <- format(func.convert_to_date(lower_limit - 60), "%Y-%m-%d %H:%M")
          closest_10_seconds <- (60 + closest_10_seconds)
        } else {
          date_hour_min <- format(func.convert_to_date(lower_limit), "%Y-%m-%d %H:%M")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date_hour_min, ":", formatC(closest_10_seconds, width = 2, flag = "0")), tz="GMT"))
        x_ticks <- lower_limit + c(0, 10, 20, 30, 40, 50, 60, 70)
        x_labels <- sapply(x_ticks, function(x) {paste0(format(func.convert_to_date(x), "%M:%S"), " min:sec")})
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
      } else {
        closest_second <- as.numeric(format(func.convert_to_date(lower_limit), "%S")) - 1
        if (closest_second < 0) {
          date_hour_min <- format(func.convert_to_date(lower_limit - 60), "%Y-%m-%d %H:%M")
          closest_second <- (60 + closest_second)
        } else {
          date_hour_min <- format(func.convert_to_date(lower_limit), "%Y-%m-%d %H:%M")
        }
        lower_limit <- as.numeric(as.POSIXct(paste0(date_hour_min, ":", formatC(closest_second, width = 2, flag = "0")), tz="GMT"))
        x_ticks <- lower_limit + c(0, 2, 4, 6, 8, 10, 12)
        x_labels <- sapply(x_ticks, function(x) {paste0(format(func.convert_to_date(x), "%M:%S"), " min:sec")})
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
      }
    } else {
      difference_in_days <- (upper_limit - lower_limit)/86400
      # If number of days > 4 years, it should be years
      if (difference_in_days > (4*365.25)) {
        lower_limit <- as.numeric(as.character(format(func.convert_to_date(lower_limit), "%Y")))
        upper_limit <- as.numeric(as.character(format(func.convert_to_date(upper_limit), "%Y"))) + 1
        x_ticks <- pretty(lower_limit:upper_limit, 4)
        x_labels <- as.character(x_ticks)
        x_ticks <- as.numeric(as.POSIXct(paste0(x_ticks, "-01-01 00:00:00 GMT")))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If it is between 2 years and 4 years, annual
      } else if (difference_in_days > (2 * 365.25)) {
        lower_limit <- as.numeric(as.character(format(func.convert_to_date(lower_limit), "%Y")))
        upper_limit <- lower_limit + 5
        x_ticks <- lower_limit:upper_limit
        x_labels <- as.character(x_ticks)
        x_ticks <- as.numeric(as.POSIXct(paste0(x_ticks, "-01-01 00:00:00 GMT")))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If it is between 1 year and 2 years, 6 monthly
      } else if (difference_in_days > 365.25) {
        lower_limit <- as.numeric(as.character(format(func.convert_to_date(lower_limit), "%Y")))
        upper_limit <- as.numeric(as.character(format(func.convert_to_date(upper_limit), "%Y"))) + 1
        x_ticks <- as.numeric(as.vector(sapply(1:length(lower_limit:upper_limit), function(x) {as.POSIXct(paste0((lower_limit:upper_limit)[x], c("-01-01 00:00:00 GMT", "-07-01 00:00:00 GMT")))})))
        x_labels <- as.vector(sapply(1:length(lower_limit:upper_limit), function(x) {paste0((lower_limit:upper_limit)[x], c(" 1st half", " 2nd half"))}))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If it is between 3 months and 1 year, quarterly
      } else if (difference_in_days > 90) {
        which_quarter <- quarters(func.convert_to_date(lower_limit))
        lower_limit <- as.numeric(as.character(format(func.convert_to_date(lower_limit), "%Y")))
        x_ticks_list <- c(paste0(lower_limit, c("-01-01 GMT", "-04-01 GMT", "-07-01 GMT", "-10-01 GMT")),
                          paste0((lower_limit+1), c("-01-01 GMT", "-04-01 GMT", "-07-01 GMT", "-10-01 GMT")),
                          paste0((lower_limit+2), c("-01-01 GMT", "-04-01 GMT", "-07-01 GMT", "-10-01 GMT"))
        )
        x_labels_list <- c(paste0(lower_limit, c(" Q1", " Q2", " Q3", " Q4")),
                           paste0((lower_limit+1), c(" Q1", " Q2", " Q3", " Q4")),
                           paste0((lower_limit+2), c(" Q1", " Q2", " Q3", " Q4"))
        )
        x_ticks <- as.numeric(as.vector(sapply(x_ticks_list[as.numeric(substr(which_quarter,2,2)):(as.numeric(substr(which_quarter,2,2))+ 6)],as.POSIXct)))
        x_labels <- x_labels_list[as.numeric(substr(which_quarter,2,2)):(as.numeric(substr(which_quarter,2,2))+ 6)]
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If it is between 1 month and 3 months, months
      } else if (difference_in_days > 30) {
        which_month <- match(months(func.convert_to_date(lower_limit)), month.name)
        lower_limit <- as.numeric(as.character(format(func.convert_to_date(lower_limit), "%Y")))
        x_ticks_list <- c(paste0(lower_limit, "-", formatC(1:12, width = 2, flag = "0"), "-01 GMT"), paste0((lower_limit + 1), "-", formatC(1:12, digits = 1, flag = "0"),"-01 GMT"))
        x_labels_list <- c(paste0(month.abb, " ", lower_limit), paste0(month.abb, " ", (lower_limit + 1)))
        x_ticks <- as.numeric(as.vector(sapply(x_ticks_list[which_month:(which_month + 3)],as.POSIXct)))
        x_labels <- x_labels_list[which_month:(which_month + 3)]
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
        # If it is between 1 week and 30 days, specific days starting from 1
      } else if (difference_in_days > 7) {
        x_ticks <- seq(from = lower_limit, to = upper_limit, by = (86400 *7))
        x_ticks <- c(x_ticks, max(x_ticks) + (86400 *7))
        x_ticks <- sapply(x_ticks, func.convert_to_date)
        x_labels <- as.character(as.POSIXct(x_ticks, tz = "GMT"))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
      } else {
        x_ticks <- seq(from = lower_limit, to= upper_limit,  by = 86400)
        x_labels <- as.character(as.POSIXct(x_ticks, tz = "GMT"))
        lower_limit <- min(x_ticks)
        upper_limit <- max(x_ticks)
      }
    }
    output <- list(x_ticks = x_ticks, x_labels = x_labels, lower_limit = lower_limit, upper_limit = upper_limit)
    return(output)
  }
  if (TRUE %in% (rv$entry[[4]] != "")) {
    xreg <- data.matrix(rv$import_data$data[,rv$entry[[4]]])
    colnames(xreg) <- rv$entry[[4]]
    xreg_future <- cbind.data.frame(lapply(1:length(rv$entry[[4]]), function(y) {
      if (is.numeric(rv$import_data$data[,rv$entry[[4]][y]])) {
        output <- mean(xreg[,y], na.rm = TRUE)
      } else {
        frequencies <- as.data.frame(table(as.factor(rv$import_data$data[,rv$entry[[4]][y]])))
        output <- frequencies$Var1[which.max(frequencies$Freq)]
      }
    }))
    colnames(xreg_future) <- rv$entry[[4]]
    xreg_future <- do.call(rbind.data.frame, lapply(1:as.numeric(rv$entry[[6]]), function(x) {xreg_future[1,]}))
    xreg_future <- data.matrix(xreg_future)
    colnames(xreg_future) <- rv$entry[[4]]
  }
  processed_data <- func.recognise_date(data = rv$import_data$data)
  data_converted <- lapply(1:ncol(rv$import_data$data), function(y) {
    if (colnames(rv$import_data$data)[y] == rv$entry[[2]]) {
      column_data <- as.character(rv$import_data$data[,y])
      column_data[! is.na(column_data)] <- processed_data[[y]][[2]]
    } else {
      column_data <- rv$import_data$data[,y]
    }
    return(column_data)
  })
  names(data_converted) <- colnames(rv$import_data$data)
  data_converted <- do.call(cbind.data.frame, data_converted)
  if (rv$entry[[3]] != "") {
    data_converted <- lapply(1:ncol(rv$import_data$data), function(y) {
      if (colnames(data_converted)[y] == rv$entry[[3]]) {
        column_data <- as.character(data_converted[,y])
        column_data[! is.na(column_data)] <- as.POSIXct(paste0("1970-01-01 ",column_data[! is.na(column_data)], tz = "GMT"))
      } else {
        column_data <- data_converted[,y]
      }
      return(column_data)
    })
    names(data_converted) <- colnames(rv$import_data$data)
    data_converted <- do.call(cbind.data.frame, data_converted)
    data <- data_converted[,c(as.numeric(rv$entry[[1]]), rv$entry[[2]], rv$entry[[3]])]
    sort_order <- order(as.numeric(data[,rv$entry[[2]]]) + as.numeric(data[,rv$entry[[3]]]))
  } else {
    data <- data_converted[,c(rv$entry[[1]], rv$entry[[2]])]
    sort_order <- order(as.numeric(data[,rv$entry[[2]]]))
  }
  data <- data[order(sort_order), ]
  time_series <- func.time_series_data(data)
  if (time_series$outcome == "Successful") {
    if (length(rv$entry[[1]]) == 1) {
      data_for_analysis <-  data.frame(ts(data = data[,rv$entry[[1]]], start = func.convert_to_date(time_series$start), deltat = time_series$interval), check.names = FALSE)
    } else {
      data_for_analysis <-  ts(data = data[,rv$entry[[1]]], start = func.convert_to_date(time_series$start), deltat = time_series$interval)
    }
    if ("date_time" %in% colnames(data_for_analysis)) {
      colnames(data_for_analysis)[colnames(data_for_analysis) == "date_time"] <- "date_time_input"
    }
    data_for_plot <- cbind.data.frame(data_for_analysis, date_time = time_series$date)
    model_fit <- list()
    residuals <- list()
    model_forecast <- list()
    data_forecast <- list()
    geom_line_text <- list()
    geom_line_text_residuals <- list()
    geom_line_text_with_forecast <- list()
    number_of_predictions <- as.numeric(rv$entry[[6]])
    new_colours <- viridis((5*length(rv$entry[[1]])))
    if ((rv$entry[[5]] == "Univariate") | (length(rv$entry[[1]]) == 1)) {
      for (i in 1:length(rv$entry[[1]])) {
        if (TRUE %in% (rv$entry[[4]] != "")) {
          model_fit[[i]] <- auto.arima(data_for_analysis[,i], xreg = xreg)
          residuals[[i]] <- model_fit[[i]]$residuals
          model_forecast[[i]] <- forecast(object = model_fit[[i]], h = number_of_predictions, xreg = xreg_future)
        } else {
          model_fit[[i]] <- auto.arima(data_for_analysis[,i])
          residuals[[i]] <- model_fit[[i]]$residuals
          model_forecast[[i]] <- forecast(model_fit[[i]], number_of_predictions)
        }
        data_forecast[[i]] <- cbind.data.frame(date_time = seq(from = attr(model_forecast[[i]]$mean, "tsp")[1], to = attr(model_forecast[[i]]$mean, "tsp")[2], length.out = number_of_predictions),
                                               forecast_average = model_forecast[[i]]$mean,
                                               lower = model_forecast[[i]]$lower,
                                               upper = model_forecast[[i]]$upper
        )
        x_values <- sapply(data_for_plot$date_time, func.convert_to_date)
        graph_x_tick_details <- func.determine_x_ticks(lower_limit = min(time_series$date), upper_limit = max(time_series$date), interval = time_series$interval)
        x_values_2 <- sapply(data_forecast[[1]]$date_time, func.convert_to_date)
        graph_x_tick_details_2 <- func.determine_x_ticks(lower_limit = min(data_forecast[[1]]$date_time), upper_limit = max(data_forecast[[1]]$date_time), interval = time_series$interval)
        geom_line_text[[i]] <- paste0('      geom_line(aes(y=data[,rv$entry[[1]][', i, ']], colour = rv$entry[[1]][', i, '])) + \n')
        geom_line_text_with_forecast[[i]] <- paste0('
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,2]), colour = new_colours[', (i-1)*3+1, '], linetype = "1"), linewidth = 1.1) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,3]), colour = new_colours[', (i-1)*3+2, '], linetype = "3"), linewidth = 0.6) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,4]), colour = new_colours[', (i-1)*3+3, '], linetype = "4"), linewidth = 0.7) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,5]), colour = new_colours[', (i-1)*3+2, '], linetype = "3"), linewidth = 0.6) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,6]), colour = new_colours[', (i-1)*3+3, '], linetype = "4"), linewidth = 0.7) +
      geom_ribbon(aes(ymin = as.numeric(data_forecast[[', i, ']][,4]), ymax = as.numeric(data_forecast[[', i, ']][,6])), fill = new_colours[', (i-1)*3+1, '], alpha = 0.1) +
                                                ')
        geom_line_text_residuals[[i]] <- paste0('      geom_line(aes(y=as.numeric(residuals[,', i, ']), colour = rv$entry[[1]][', i, '])) + \n')
      }
      geom_line_text <- do.call(paste0, geom_line_text)
      geom_line_text_residuals <- do.call(paste0, geom_line_text_residuals)
      geom_line_text_with_forecast <- do.call(paste0, geom_line_text_with_forecast)
      residuals <- do.call(cbind.data.frame, residuals)
      colnames(residuals) <- rv$entry[[1]]
      plot_text_residuals <- paste0('ggplot(residuals, aes(x=x_values)) + \n', geom_line_text_residuals,
                                    '      xlab("Time") + ylab("Value") + theme_classic() +
      scale_colour_viridis_d(name = "Group") +
      scale_x_continuous(limits = c(graph_x_tick_details$lower_limit, graph_x_tick_details$upper_limit),
                        breaks = graph_x_tick_details$x_ticks,
                        labels = graph_x_tick_details$x_labels
                        )')
    } else {
      season <- max(sapply(1:ncol(data_for_analysis), function(y) {findfrequency(data_for_analysis[,y])}))
      lag_order <- suppressWarnings(max(sapply(1:ncol(data_for_analysis), function(y) {tseries::adf.test(data_for_analysis[,y])$parameter})))
      differences <- max(sapply(1:ncol(data_for_analysis), function(y) {ndiffs(data_for_analysis[,y])}))
      if (season > 1) {
        differences_seasonal <- suppressWarnings(max(sapply(1:ncol(data_for_analysis), function(y) {nsdiffs(data_for_analysis[,y])})))
        if (TRUE %in% (rv$entry[[4]] != "")) {
          if (differences > 0) {
            if (differences_seasonal >0) {
              cointegration <- summary(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(diff(xreg, differences = differences_seasonal),  lag = lag_order,  differences = differences), season = season))
            } else {
              cointegration <- summary(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences), season = season))
            }
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              if (differences_seasonal >0) {
                model_fit <- try(vec2var(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(diff(xreg, differences = differences_seasonal),  lag = lag_order,  differences = differences), season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(diff(xreg, differences = differences_seasonal),  lag = lag_order,  differences = differences), season = season), r = r), silent = TRUE)
                }
              } else {
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences), season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences), season = season), r = r), silent = TRUE)
                }
              }
            } else {
              if (differences_seasonal > 0) {
                model_fit <- VAR(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), p = lag_order, exogen = diff(diff(xreg, differences = differences_seasonal),  lag = lag_order,  differences = differences), season = season)
              } else {
                model_fit <- VAR(diff(data_for_analysis, lag = lag_order, differences = differences), p = lag_order, exogen = diff(xreg, lag = lag_order, differences = differences), season = season)
              }
            }
          } else {
            if (differences_seasonal > 0) {
              cointegration <- summary(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, differences = differences_seasonal), season = season))
            } else {
              cointegration <- summary(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg, season = season))
            }
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              if (differences_seasonal >0) {
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, differences = differences_seasonal), season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, differences = differences_seasonal), season = season), r = r), silent = TRUE)
                }
              } else {
                model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg, season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg, season = season), r = r), silent = TRUE)
                }
              }
            } else {
              if (differences_seasonal > 0) {
                model_fit <- VAR(diff(data_for_analysis, differences = differences_seasonal), p = lag_order, exogen = diff(xreg, differences = differences_seasonal), season = season)
              } else {
                model_fit <- VAR(data_for_analysis, p = lag_order, exogen = xreg, season = season)
              }
            }
          }
        } else {
          if (differences > 0) {
            if (differences_seasonal > 0) {
              cointegration <- summary(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season))
            } else {
              cointegration <- summary(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season))
            }
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              if (differences_seasonal >0) {
                model_fit <- try(vec2var(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                }
              } else {
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                }
              }
            } else {
              if (differences_seasonal > 0) {
                model_fit <- VAR(diff(diff(data_for_analysis, differences = differences_seasonal),  lag = lag_order,  differences = differences), p = lag_order, season = season)
              } else {
                model_fit <- VAR(diff(data_for_analysis, lag = lag_order, differences = differences), p = lag_order, season = season)
              }
            }
          } else {
            if (differences_seasonal > 0) {
              cointegration <- summary(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season))
            } else {
              cointegration <- summary(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season))
            }
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              if (differences_seasonal >0) {
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, differences = differences_seasonal), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                }
              } else {
                model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                if (str_detect(model_fit[[1]][1], "Error")) {
                  limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                  r <- max(as.numeric(unlist(str_split(limits, ":"))))
                  model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", season = season), r = r), silent = TRUE)
                }
              }
            } else {
              if (differences_seasonal > 0) {
                model_fit <- VAR(diff(data_for_analysis, differences = differences_seasonal), p = lag_order, season = season)
              } else {
                model_fit <- VAR(data_for_analysis, p = lag_order, season = season)
              }
            }
          }
        }
      } else {
        if (TRUE %in% (rv$entry[[4]] != "")) {
          if (differences > 0) {
            cointegration <- summary(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences)))
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences)), r = r), silent = TRUE)
              if (str_detect(model_fit[[1]][1], "Error")) {
                limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                r <- max(as.numeric(unlist(str_split(limits, ":"))))
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = diff(xreg, lag = lag_order, differences = differences)), r = r), silent = TRUE)
              }
            } else {
              model_fit <- VAR(diff(data_for_analysis, lag = lag_order, differences = differences), p = lag_order, exogen = diff(xreg, lag = lag_order, differences = differences))
            }
          } else {
            cointegration <- summary(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg))
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg), r = r), silent = TRUE)
              if (str_detect(model_fit[[1]][1], "Error")) {
                limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                r <- max(as.numeric(unlist(str_split(limits, ":"))))
                model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory", dumvar = xreg), r = r), silent = TRUE)
              }
            } else {
              model_fit <- VAR(data_for_analysis, p = lag_order, exogen = xreg)
            }
          }
        } else {
          if (differences > 0) {
            cointegration <- summary(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"))
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"), r = r), silent = TRUE)
              if (str_detect(model_fit[[1]][1], "Error")) {
                limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                r <- max(as.numeric(unlist(str_split(limits, ":"))))
                model_fit <- try(vec2var(ca.jo(diff(data_for_analysis, lag = lag_order, differences = differences), type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"), r = r), silent = TRUE)
              }
            } else {
              model_fit <- VAR(diff(data_for_analysis, lag = lag_order, differences = differences), p = lag_order)
            }
          } else {
            cointegration <- summary(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"))
            reject_r0 <- (cointegration@teststat[length(cointegration@teststat)] > cointegration@cval[length(cointegration@teststat),2])
            if (reject_r0 == TRUE) {
              r = cbind.data.frame(test_statistic = cointegration@teststat, critical_value_0.05 = cointegration@cval[,2])
              r$r <- (nrow(r)-1):0
              r <- r[order(r$r),]
              r$reject_hypothesis <- (r$test_statistic > r$critical_value_0.05)
              if (is.na(match(FALSE, r$reject_hypothesis))) {
                r <- max(r$r + 1)
              } else {
                r <- r$r[match(FALSE, r$reject_hypothesis)]
              }
              model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"), r = r), silent = TRUE)
              if (str_detect(model_fit[[1]][1], "Error")) {
                limits <-  substr(model_fit,  (str_locate(model_fit[[1]][1],"\\[")[1,1] + 1), (str_locate(model_fit[[1]][1],"\\]")[1,1] - 1))
                r <- max(as.numeric(unlist(str_split(limits, ":"))))
                model_fit <- try(vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"), r = r), silent = TRUE)
              }
              model_fit <- vec2var(ca.jo(data_for_analysis, type = "eigen", ecdet = "const", K = lag_order, spec = "transitory"), r = r)
            } else {
              model_fit <- VAR(data_for_analysis, p = lag_order)
            }
          }
        }
      }
      residuals <- resid(model_fit)
      time_series_details <- attr(model_fit$vecm@x, "tsp")
      geom_line_text_residuals <- do.call(paste0, lapply(1:length(rv$entry[[1]]), function(x) {paste0('geom_line(aes(y=residuals[,',x,'], colour = rv$entry[[1]][',x,'])) + \n')}))
      plot_text_residuals <- paste0('ggplot(residuals, aes(x=1:nrow(residuals))) + \n', geom_line_text_residuals,
                                    '      xlab("Time") + ylab("Value") + theme_classic() + theme(axis.text = element_blank()) +
      scale_colour_viridis_d(name = "Group")')
      geom_line_text <- do.call(paste0, lapply(1:length(rv$entry[[1]]), function(x) {paste0('geom_line(aes(y=data[,rv$entry[[1]][',x,']], colour = rv$entry[[1]][',x,'])) + \n')}))
      x_values <- sapply(data_for_plot$date_time, func.convert_to_date)
      graph_x_tick_details <- func.determine_x_ticks(lower_limit = min(time_series$date), upper_limit = max(time_series$date), interval = time_series$interval)
      x_values_2 <- sapply(seq(from = (time_series_details[2] + 1/time_series_details[3]), by = (1/time_series_details[3]), length.out = number_of_predictions), func.convert_to_date)
      graph_x_tick_details_2 <- func.determine_x_ticks(lower_limit = (time_series_details[2] + 1/time_series_details[3]), upper_limit = (time_series_details[2] + 1/time_series_details[3] + number_of_predictions/time_series_details[3]), interval = 1/time_series_details[3])
      if (TRUE %in% (rv$entry[[4]] != "")) {
        model_forecast_80 <- predict(model_fit, n.ahead = number_of_predictions, ci = 0.80, dumvar = xreg_future)
        model_forecast_95 <- predict(model_fit, n.ahead = number_of_predictions, ci = 0.95, dumvar = xreg_future)
      } else {
        model_forecast_80 <- predict(model_fit, n.ahead = number_of_predictions, ci = 0.80)
        model_forecast_95 <- predict(model_fit, n.ahead = number_of_predictions, ci = 0.95)
      }
      for (i in 1:length(rv$entry[[1]])) {
        data_forecast[[i]] <- cbind.data.frame(date_time = seq(from = (time_series_details[2] + 1/time_series_details[3]), by = (1/time_series_details[3]), length.out = number_of_predictions),
                                               forecast_average = model_forecast_80$fcst[[i]][,"fcst"],
                                               `lower.80%` = model_forecast_80$fcst[[i]][,"lower"],
                                               `lower.95%` = model_forecast_95$fcst[[i]][,"lower"],
                                               `upper.80%` = model_forecast_80$fcst[[i]][,"upper"],
                                               `upper.95%` = model_forecast_80$fcst[[i]][,"upper"]
        )
        geom_line_text_with_forecast[[i]] <- paste0('
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,2]), colour = new_colours[', (i-1)*3+1, '], linetype = "1"), linewidth = 1.1) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,3]), colour = new_colours[', (i-1)*3+2, '], linetype = "3"), linewidth = 0.6) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,4]), colour = new_colours[', (i-1)*3+3, '], linetype = "4"), linewidth = 0.7) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,5]), colour = new_colours[', (i-1)*3+2, '], linetype = "3"), linewidth = 0.6) +
      geom_line(aes(y=as.numeric(data_forecast[[', i, ']][,6]), colour = new_colours[', (i-1)*3+3, '], linetype = "4"), linewidth = 0.7) +
      geom_ribbon(aes(ymin = as.numeric(data_forecast[[', i, ']][,4]), ymax = as.numeric(data_forecast[[', i, ']][,6])), fill = new_colours[', (i-1)*3+1, '], alpha = 0.1) +
                                                ')
      }
      geom_line_text_with_forecast <- do.call(paste0, geom_line_text_with_forecast)
    }
    plot_text <- paste0('ggplot(data, aes(x=x_values)) + \n', geom_line_text,
                        '      xlab("Time") + ylab("Value") + theme_classic() +
      scale_colour_viridis_d(name = "Group") +
      scale_x_continuous(limits = c(graph_x_tick_details$lower_limit, graph_x_tick_details$upper_limit),
                        breaks = graph_x_tick_details$x_ticks,
                        labels = graph_x_tick_details$x_labels
                        )')
    plot_text_with_forecast <- paste0('ggplot(data_forecast[[1]], aes(x=x_values_2)) + \n', geom_line_text_with_forecast,
                                      '      xlab("Time") + ylab("Value") + theme_classic() +
      scale_x_continuous(limits = c(graph_x_tick_details_2$lower_limit, graph_x_tick_details_2$upper_limit),
                        breaks = graph_x_tick_details_2$x_ticks,
                        labels = graph_x_tick_details_2$x_labels
                        ) +
      scale_linetype_manual(name = "Line type",
                          values = c(1, 4, 3),
                          labels = c("Average", "80% limit", "95% limit")
  ) +
      scale_colour_manual(name = "Colour",
                          values = new_colours[1:(3*length(rv$entry[[1]]))],
                          labels = as.vector(sapply(1:length(rv$entry[[1]]), function(x) {paste0(rv$entry[[1]][x], ": ", c("Average", "80% limit", "95% limit"))}))
  )')
    time_series_plot <- eval(parse(text = plot_text))
    time_series_plot_with_forecast <- eval(parse(text = plot_text_with_forecast))
    residuals_plot <- eval(parse(text = plot_text_residuals))
    acf_plots <- lapply(1:length(rv$entry[[1]]), function(x) {
      temp <- acf(residuals[!is.na(residuals[,x]),x], plot = FALSE)
      acf <- cbind.data.frame(Lag = temp$lag/time_series$interval, ACF = temp$acf)
      acf <- acf[2:nrow(acf),]
      ggplot(data = acf, aes(x = Lag, y = ACF)) + geom_segment(yend = 0) + scale_y_continuous(limits = c(-ceiling(max(abs(acf$ACF))*10)/10,ceiling(max(abs(acf$ACF))*10)/10)) + scale_x_continuous(expand = c(0,0)) + theme(axis.line.y = element_line(colour = "black"), panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0) + ggtitle(rv$entry[[1]][x])
    })
    acf_plots <- eval(parse(text = paste0('plot_grid(', paste0('acf_plots[[',1:length(rv$entry[[1]]),']]', collapse = ", "),', ncol = 1) + theme(plot.background = element_rect(fill = "white", colour = NA))')))
    plot_title <- "Time series analysis"
    plot_title_combined <- ggdraw() + draw_label("Time series analysis", color="darkgreen", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
    plots_list <- c("time_series_plot", "time_series_plot_with_forecast", "residuals_plot", "acf_plots")
    plots_list <- sapply(1:length(plots_list), function(x){
      eval(parse(text = paste0(
        "suppressWarnings(suppressMessages(try(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                                                   substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','",plots_list[x],"','.png'),
                                                 plot = ",plots_list[x],"), silent = TRUE)))"
      )))
    })
    plots_list_display <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = 'd', flag = 0), '_',
                                 substr(str_replace_all(plot_title, '[^[:alnum:]]', '_'), 1, 80) ,'_','composite_plot','.png')
    composite_plot <- suppressWarnings(suppressMessages(plot_grid(plot_title_combined, plot_grid(time_series_plot, time_series_plot_with_forecast, residuals_plot, acf_plots, ncol = 2),
                                                                  ncol=1, rel_heights = c(0.1, 3)) + theme(plot.background = element_rect(fill = "white", colour = NA))))
    prediction_date_time <- sapply(data_forecast[[1]]$date_time, function(x) {as.character(func.convert_to_date(x))})
    data_forecast <- do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
      data_forecast_each_outcome <- data_forecast[[x]][2:6]
      colnames(data_forecast_each_outcome) <- paste0(rv$entry[[1]][x], "_", c("forecast_average", "lower_80%", "lower_95%", "upper_80%", "upper_95%"))
      return(data_forecast_each_outcome)
    }))
    data_forecast_display <- cbind.data.frame(prediction_date_time, lapply(1:ncol(data_forecast), function(y) {round(data_forecast[,y], 4)}))
    colnames(data_forecast_display) <- c("prediction_date_time",colnames(data_forecast))
    data_forecast <- cbind.data.frame(prediction_date_time, data_forecast)
    results <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = rv$first_menu_choice,
      `Analysis outcome` = "Successful",
      check.names = FALSE
    )
    results_display <- results
    if ((rv$entry[[5]] == "Univariate") | (length(rv$entry[[1]]) == 1)) {
      regression_coefficients <- cbind.data.frame(
        coefficients = names(model_fit[[1]]$coef),
        do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
          regression_coefficients_each_outcome <- cbind.data.frame(
            point_estimate = model_fit[[x]]$coef,
            se = sapply(1:length(model_fit[[x]]$coef), function(z) {model_fit[[x]]$var.coef[z,z]})^0.5
          )
          colnames(regression_coefficients_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(regression_coefficients_each_outcome))
          return(regression_coefficients_each_outcome)
        }))
      )
      regression_coefficients_display <- cbind.data.frame(
        coefficients = names(model_fit[[1]]$coef),
        do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
          regression_coefficients_each_outcome <- cbind.data.frame(
            point_estimate = round(model_fit[[x]]$coef,4),
            se = round(sapply(1:length(model_fit[[x]]$coef), function(z) {model_fit[[x]]$var.coef[z,z]})^0.5,4)
          )
          colnames(regression_coefficients_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(regression_coefficients_each_outcome))
          return(regression_coefficients_each_outcome)
        }))
      )
      model_fit_statistics <- cbind.data.frame(
        parameter = c("AIC", "BIC", "-2 log likelihood"),
        do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
          model_fit_statistics_each_outcome <- cbind.data.frame(
            statistic = c(model_fit[[x]]$aicc, model_fit[[x]]$bic, model_fit[[x]]$loglik)
          )
          colnames(model_fit_statistics_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(model_fit_statistics_each_outcome))
          return(model_fit_statistics_each_outcome)
        }))
      )
      model_fit_statistics_display <- cbind.data.frame(
        parameter = c("AIC", "BIC", "-2 log likelihood"),
        do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
          model_fit_statistics_each_outcome <- cbind.data.frame(
            statistic = round(c(model_fit[[x]]$aicc, model_fit[[x]]$bic, model_fit[[x]]$loglik),4)
          )
          colnames(model_fit_statistics_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(model_fit_statistics_each_outcome))
          return(model_fit_statistics_each_outcome)
        }))
      )
    } else {
      if (model_fit$call[[1]] == "VAR") {
        regression_coefficients <- cbind.data.frame(
          coefficients = names(model_fit$varresult[[1]]$coefficients),
          do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
            regression_coefficients_each_outcome <- cbind.data.frame(
              point_estimate = model_fit$varresult[[x]]$coefficients,
              se = sapply(1:length(model_fit$varresult[[1]]$coefficients), function(z) {vcov(model_fit$varresult[[x]])[z,z]})^0.5
            )
            colnames(regression_coefficients_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(regression_coefficients_each_outcome))
            return(regression_coefficients_each_outcome)
          }))
        )
        regression_coefficients_display <- cbind.data.frame(
          coefficients = names(model_fit$varresult[[1]]$coefficients),
          do.call(cbind.data.frame, lapply(1:length(rv$entry[[1]]), function(x) {
            regression_coefficients_each_outcome <- cbind.data.frame(
              point_estimate = round(model_fit$varresult[[x]]$coefficients,4),
              se = round(sapply(1:length(model_fit$varresult[[1]]$coefficients), function(z) {vcov(model_fit$varresult[[x]])[z,z]})^0.5,4)
            )
            colnames(regression_coefficients_each_outcome) <- paste0(rv$entry[[1]][x], "_", colnames(regression_coefficients_each_outcome))
            return(regression_coefficients_each_outcome)
          }))
        )
      } else {
        regression_coefficients <- cbind.data.frame(
          coefficients = colnames(model_fit$deterministic),
          t(model_fit$deterministic)
        )
        regression_coefficients_display <- cbind.data.frame(
          coefficients = colnames(model_fit$deterministic),
          sapply(2:ncol(regression_coefficients), function(y) {round(regression_coefficients[,y],4)})
        )
      }
      model_fit_statistics <- cbind.data.frame(
        parameter = c("AIC", "BIC", "-2 log likelihood"),
        statistic = c(AIC(model_fit), BIC(model_fit), logLik(model_fit))
      )
      model_fit_statistics_display <- cbind.data.frame(
        parameter = c("AIC", "BIC", "-2 log likelihood"),
        statistic = round(c(AIC(model_fit), BIC(model_fit), logLik(model_fit)),4)
      )
    }
    results <- suppressWarnings(function.rbind_different_column_numbers(list = list(results, regression_coefficients, model_fit_statistics, data_forecast)))
    results_display <- suppressWarnings(function.rbind_different_column_numbers(list = list(results_display, regression_coefficients_display, model_fit_statistics_display, data_forecast_display)))
    display_table <- TRUE
    display_plot <- TRUE
    analysis_outcome <- "Successful"
  } else {
    results <- data.frame(
      `Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
      `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
      `Analysis outcome` = "Unsuccessful",
      `Reason` = time_series$date,
      check.names = FALSE
    )
    results_display <- results
    plots_list <- ""
    plots_list_display <- ""
    display_table <- TRUE
    display_plot <- FALSE
    analysis_outcome <- "Unsuccessful"
  }
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
