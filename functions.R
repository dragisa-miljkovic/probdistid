
############ FITING #########################################

fit_distributions <- function(x, y, distributions) {
  fit_results <- list()
  print("FIT DISTRIBUTIONS")
  print(distributions)
  for (dist_name in distributions) {
    dist_object <- distribution_map[[dist_name]]
    fit_obj <- dist_object$fit(x, y)
    # metrics <- dist_object$get_metrics()
    #fit_results <- append(fit_results, list(list(s)))
    # names(fit_results)[length(fit_results)] <- pas\te(dist_name)
    print(dist_name)
    print(fit_obj)
    if (!is.null(fit_obj)) {
      print("dist_object$check_fit()")
      print(dist_object$check_fit())
      if (dist_object$check_fit()) {
        fit_results[[dist_name]] <- fit_obj
      }
    }
  }
  #list(
  #"metrics" = metrics,
  #"model" = dist_object$model
  #)
  
  return(fit_results)
}



# Extract the name, AIC, BIC, adjusted R-squared, and RMSE for each nls object
# Use lapply to extra ct the name, AIC, BIC, adjusted R-squared, and RMSE for each nls object
calculate_fit_metrics <- function(fit_results, data, x, y) {
  result <- lapply(names(fit_results), function(name) {
    nls_obj <- fit_results[[name]]
    aic <- AIC(nls_obj)
    bic <- BIC(nls_obj)
    
    y_pred <- predict(nls_obj, newdata = list(x = x))
    abs_diffs <- abs(y - y_pred)
    mae <- mean(abs_diffs)
    
    residuals <- summary(nls_obj)$residuals
    rmse <- sqrt(mean(residuals ^ 2))
    
    RSS <- sum(residuals ^ 2)
    y_mean <- mean(y)
    TSS <- sum((y - y_mean) ^ 2)
    R_squared <- 1 - (RSS / TSS)
    
    n = length(y)
    coefficients <- coef(nls_obj)
    p <- length(coefficients)
    adjusted_R_squared <-
      1 - (1 - R_squared) * (n - 1) / (n - p - 1)
    
    return(c(name, aic, bic, mae, R_squared, rmse, adjusted_R_squared))
  })
  
  return(result)
}


# Extract the model name, parameter names, and parameter values for each nls object
extract_model_params <- function(fit_results) {
  param_result <- lapply(names(fit_results), function(name) {
    nls_obj <- fit_results[[name]]
    
    coefficients <- coef(nls_obj)
    
    param_str <- sapply(names(coefficients), function(param_name) {
      paste0(param_name, " = ", coefficients[param_name])
    })
    
    max_params <-
      max(sapply(fit_results, function(model)
        length(coef(model))))
    param_str <-
      c(param_str, rep(NA, max_params - length(param_str)))
    
    params <- c(name, param_str)
    names(params) <- c("name", paste0("parameter", 1:max_params))
    
    return(params)
  })
  return(param_result)
}



############ GET AND SET DISTRIBUTION PARAMETERS #########################################

# Function that returns a list of the selected distribution parameters
get_distribution_parameters <- function(distribution_name) {
  # Get the distribution object based on the name
  dist_obj <- distribution_map[[distribution_name]]
  if (is.null(dist_obj)) {
    stop("Invalid distribution name provided.")
  }
  # Extract the parameters from the distribution object and format them as required
  params <-
    lapply(names(dist_obj$parameters), function(param_name) {
      min_value <- dist_obj$parameters[[param_name]][1]
      max_value <- dist_obj$parameters[[param_name]][2]
      list(name = param_name,
           default_value = c(min_value, max_value))
    })
  return(params)
}


############ FILE SIGNALS PROCESSING #####################################################

# Process signals from input file
process_signals <- function(signals) {
  # Process signals and create a table to display
  signal_data <- lapply(seq_along(signals), function(i) {
    signal <- signals[[i]]
    distribution <- signal$distribution
    parameters <-
      paste(names(signal$parameters),
            "=",
            signal$parameters,
            collapse = ", ")
    samples <- length(signal$samples)
    
    data.frame(
      Signal = paste0("Signal ", i),
      Distribution = distribution,
      Parameters = parameters,
      Number_of_samples = samples,
      stringsAsFactors = FALSE
    )
  })
  
  print(signal_data)
  print(summary(signal_data))
  
  # Return the processed signal data
  return(signal_data)
}


process_text_file <- function(signals) {
  # Prepare signal data as a list
  signal_data_list <- list(
    distribution = "Unknown distribution",
    parameters = "Unknown parameters",
    samples = signals
  )
  
  # Prepare signal data as a data frame for display
  signal_data_frame <- data.frame(
    Signal = paste0("Signal ", 1),
    Distribution = signal_data_list$distribution,
    Parameters = signal_data_list$parameters, # Convert parameters to string for data frame
    Number_of_samples = length(signal_data_list$samples),
    stringsAsFactors = FALSE
  )
  
  # Instead of returning a list containing the data frame and raw data,
  # we'll return just the data frame, similar to process_signals function.
  return(signal_data_frame)
}

############ INPUT FILE PROCESSING #######################################################

# A function that calls signal processing functions based on the chosen file format
process_file <- function(file_path, file_format) {
  # Initialize signals variable
  signals <- NULL
  
  # Read signals based on the chosen file format
  if (file_format == "RData") {
    signals <- process_rdata(file_path)
  } else if (file_format == "CSV") {
    signals <- process_csv(file_path)
  } else if (file_format == "XML") {
    signals <- process_xml(file_path)
  } else if (file_format == "JSON") {
    signals <- process_json(file_path)
  } else if (file_format == "TXT") {
    signals <- process_plain_text(file_path)
  }
  
  return(signals)
}

# Function to process RData files
process_rdata <- function(file_path) {
  loaded_data <- new.env()
  load(file_path, envir = loaded_data)
  loaded_list <- as.list(loaded_data)
  distribution_list <- loaded_list$distributions
  
  signals <- lapply(distribution_list, function(distribution) {
    list(
      distribution = distribution$distribution_name,
      parameters = distribution$parameters,
      samples = as.numeric(distribution$samples)
    )
  })
  
  return(signals)
}


# Function to process JSON files
process_json <- function(file_path) {
  json_data <- fromJSON(file_path)
  signal_names <- names(json_data)
  
  signals <- lapply(signal_names, function(signal_name) {
    list(
      distribution = json_data[[signal_name]]$distribution,
      parameters = json_data[[signal_name]]$parameters,
      samples = as.numeric(json_data[[signal_name]]$samples)
    )
    # cat("Distribution:", distribution, "\n")
    # cat("Parameters:", paste0(names(parameters), "=", unlist(parameters), collapse = ", "), "\n")
    # cat("Samples:", paste(samples, collapse = ", "), "\n\n")
  })
  
  # Loop through signals
  # for (signal_name in names(json_data)) {
  #}
  
  return(signals)
}


# Function to process XML files
process_xml <- function(file_path) {
  xml_data <- read_xml(file_path)
  signal_nodes <- xml_find_all(xml_data, "//signal")
  
  signals <- lapply(signal_nodes, function(signal_node) {
    distribution_value <- xml_text(xml_find_first(signal_node, "./distribution"))
    parameter_nodes <- xml_find_all(signal_node, "./parameters/parameter")
    parameters <- setNames(
      as.numeric(xml_text(parameter_nodes)),
      xml_attr(parameter_nodes, "name")
    )
    samples <- as.numeric(xml_text(xml_find_all(signal_node, "./samples/sample")))
    
    list(
      distribution = distribution_value,
      parameters = parameters,
      samples = samples
    )
  })
  
  # for (i in 1:length(signals)) {
  #   signal_name <- xml_attr(signals[[i]], "id")
  #   signal_values <-
  #     as.numeric(xml_text(xml_children(signals[[i]])))
  #   signal_list[[signal_name]] <- signal_values
  # }
  return(signals)
}


# Function to process CSV files
process_csv <- function(file_path) {
  csv_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  signals <- lapply(seq_len(nrow(csv_data)), function(i) {
    num_parameters <- csv_data[i, "num_parameters"]
    parameter_names <- character(0)
    parameter_values <- numeric(0)
    
    for (j in 1:num_parameters) {
      parameter_names <-
        c(parameter_names, csv_data[i, paste0("parameter", j, "_name")])
      parameter_values <-
        c(parameter_values, csv_data[i, paste0("parameter", j, "_value")])
    }
    
    samples <-
      as.numeric(unlist(strsplit(csv_data[i, "samples"], ",")))
    
    list(
      distribution = csv_data[i, "distribution"],
      parameters = setNames(parameter_values, parameter_names),
      samples = samples
    )
  })
  
  return(signals)
}


# Plain text file processing
process_plain_text <- function(filename) {
    # Read the file as a text
    lines <- readLines(filename, warn = FALSE)
    # Check if the last line is incomplete and add a new line if it is
    if(length(lines) > 0 && nchar(lines[length(lines)]) > 0) {
      lines <- c(lines, "")
    }
    # Concatenate all the lines into a single character string
    lines_concat <- paste(lines, collapse = " ")
    # Remove brackets
    clean_lines <- gsub("\\[|\\]", "", lines_concat)
    # Remove characters that are not numeric, dot (.), comma (,), or space
    clean_lines <- gsub("[^0-9\\.\\,\\s]", " ", clean_lines)
    # Split the character string into a vector using multiple separators
    signals_str <- strsplit(clean_lines, "[, ]+")[[1]]
    # Convert the vector of character strings into a vector of numeric values
    signals_num <- as.numeric(signals_str)
    # Remove NA values that might occur due to multiple separators
    signals_num <- signals_num[!is.na(signals_num)]
    signals <- list(
      list(
        distribution = "Unknown distribution",
        parameters = "Unknown parameters",
        samples = as.numeric(signals_num)
      )
    )
    
    return(signals)
}
  




###### DCDF  ######

dcdf_signal_processing <- function(samples, num_samples, num_bins) {
  # slice the samples vector to take the first num_samples elements
  samples <- samples[1:num_samples]
  
  min_sample = min(samples)
  max_sample = max(samples)
  start = 0
  if (min_sample < 0) {
    start = floor(min_sample * 100) / 100
    # bin_size = round((max_sample - start) / (num_bins - 2), digits = 2)
    bin_size = ((round(max_sample - start, digits = 1) + 0.1) / (num_bins - 1))
  } else {
    # bin_size = round(max_sample / (num_bins - (num_bins // 10)), 2)
    # bin_size =  (round(max_sample, digits = 2))/(num_bins-1)
    bin_size = ((round(max_sample, digits = 1) + 0.1) / (num_bins - 1))
  }
  
  
  while ((num_bins * bin_size) < max_sample) {
    bin_size = bin_size * 1.01
  }
  
  # bin_size = ceiling(bin_size*100000.0) / 100000.0
  bins <- c()
  for (i in (0:(num_bins - 1))) {
    bins <- append(bins, round((start + i * bin_size), digits = 4))
  }
  
  cum_dist = integer(num_bins)
  for (i in (1:num_bins)) {
    for (sample in samples) {
      if (sample <= bins[i]) {
        cum_dist[i] = cum_dist[i] + 1
      }
    }
  }
  
  norm_dist <- c()
  for (e in cum_dist) {
    norm_dist = append(norm_dist, e / num_samples)
  }
  
  newList <- list("bins" = bins, "norm_dist" = norm_dist)
  x <<- bins
  y <<- norm_dist
  return (newList)
  # newList$bins
  # newList$norm_dist
}


############ OUTPUT FILE PROCESSING ######################################################

# A function that calls file save functions based on the chosen file format
save_to_file <- function(signals, file, file_format) {
  # Write signals based on the chosen file format
  if (file_format == "RData") {
    file <- save_rdata(signals, file)
  } else if (file_format == "CSV") {
    file <- save_csv(signals, file)
  } else if (file_format == "XML") {
    file <- save_xml(signals, file)
  } else if (file_format == "JSON") {
    file <- save_json(signals, file)
  }
  
  return(file)
}


# Function to save RData files
save_rdata <- function(signals, file_path) {
  distributions <- lapply(signals, function(signal) {
    list(
      distribution_name = signal$distribution,
      parameters = signal$parameters,
      samples = as.numeric(signal$samples)
    )
  })
  
  save(distributions, file = file_path)
}


# Function to save JSON files
save_json <- function(signals, file_path) {
  json_data <- lapply(seq_along(signals), function(i) {
    list(
      distribution = signals[[i]]$distribution,
      parameters = signals[[i]]$parameters,
      samples = signals[[i]]$samples
      # distribution_name = signal$distribution,
      # parameters = signal$parameters,
      # samples = as.numeric(signal$samples)
      
    )
  })
  
  names(json_data) <- paste0("signal_", seq_along(signals))

  json_file <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json_file, file_path)
  
  #toJSON(json_data, auto_unbox = TRUE, pretty = TRUE) %>% write(file = file_path)
}


# Function to save XML files
# 
# library(xml2)
# 
# save_xml <- function(signals, file_path) {
#   doc <- xml_new_root("signals")
#   
#   for (signal in signals) {
#     signal_node <- xml_add_child(doc, "signal")
#     
#     distribution_node <- xml_add_child(signal_node, "distribution")
#     xml_add_child(distribution_node, "value") <- signal$distribution
#     
#     parameters_node <- xml_add_child(signal_node, "parameters")
#     for (param_name in names(signal$parameters)) {
#       parameter_node <- xml_add_child(parameters_node, "parameter")
#       xml_add_child(parameter_node, "name") <- param_name
#       xml_add_child(parameter_node, "value") <- signal$parameters[[param_name]]
#     }
#     
#     samples_node <- xml_add_child(signal_node, "samples")
#     for (sample in signal$samples) {
#       sample_node <- xml_add_child(samples_node, "sample")
#       xml_add_child(sample_node, "value") <- as.character(sample)
#     }
#   }
#   
#   write_xml(doc, file_path)
# }

# Function to save XML files
save_xml <- function(signals, file_path) {
  doc <- xml_new_root("data")  # Create the XML document
  
  distributions <- xml_add_child(doc, "distributions")  # Create the distributions element
  
  # Iterate over the signals
  for (signal in signals) {
    distribution <- xml_add_child(distributions, "distribution")  # Create the distribution element
    
    distribution_name <- xml_new_child(distribution, "distribution_name")  # Add distribution_name element
    xml_text(distribution_name) <- signal$distribution
    
    parameters <- xml_add_child(distribution, "parameters")  # Add parameters element
    for (param in signal$parameters) {
      parameter <- xml_add_child(parameters, "parameter")  # Add parameter element
      xml_set_attr(parameter, "name", param$name)
      xml_text(parameter) <- param$value
    }
    
    samples <- xml_add_child(distribution, "samples")  # Add samples element
    xml_text(samples) <- paste(signal$samples, collapse = " ")
  }
  
  xml_save(doc, file_path)  # Save the XML document to file
}




# Function to save CSV files
save_csv <- function(signals, file_path) {
  csv_data <- do.call(rbind, lapply(signals, function(signal) {
    num_parameters <- length(signal$parameters)
    data.frame(
      distribution = signal$distribution,
      num_parameters = num_parameters,
      setNames(as.list(signal$parameters), 
               paste0("parameter", seq_len(num_parameters), "_value")),
      samples = paste(signal$samples, collapse = ",")
    )
  }))
  
  write.csv(csv_data, file_path, row.names = FALSE)
}


############ MODAL WARNINGS ##############################################################

# Distributions and their parameters not selected
showParameterWarning <- function(session) {
  updateTabsetPanel(session, "mainTabset", selected = "tab1")
  showModal(
    modalDialog(
      title = "Distribution parameters are not set",
      "You need to finish selecting  distributoin parameter values. Click the 'SELECT' button after you make your selections.",
      easyClose = TRUE
    )
  )
}

# Signal or DCDF missing
showSignalMissingWarning <- function() {
  showModal(
    modalDialog(
      title = "Signal or DCDF missing",
      "Please generate a signal and then calculate the DCDF.",
      easyClose = TRUE
    )
  )
}

# Distributions not selected
showSelectDistributionWarning <- function() {
  showModal(
    modalDialog(
      title = "Please select the distribution",
      "Use the drop-down list to select the distribution before signal generation.",
      easyClose = TRUE
    )
  )
}

# File not loaded 
showUnsuccessfulLoadingWarning <- function(){
  showModal(modalDialog(
    title = "Unsuccessful loading",
    paste("An error occurred while loading the file: ", e$message),
    easyClose = TRUE
  ))
}

# Invalid parameter values
showInvalidParametersWarning <- function(){
  showModal(
    modalDialog(
      title = "Invalid parameter values",
      "Warning: Some parameter values are invalid. Please enter valid numeric values.",
      easyClose = TRUE
    )
  )
}

# Parameter values outside of defined range
showParameterValuesWarning <- function(session){
  showModal(
    modalDialog(
      title = "Parameters values outside of defined range",
      "The values must be within the range you selected.",
      easyClose = TRUE
    )
  )
  updateTabsetPanel(session, "mainTabset", selected = "tab1") # Show tab1
}

# Selected signal samples > signal length
showSampleNumberWarning <- function(){
  showModal(
    modalDialog(
      title = "Signal samples warning",
      "Warning: Number of samples cannot be greater than the available data points. Number of samples has been set to the maximum allowed value.",
      easyClose = TRUE
    )
  )        
}

# Selected bin number > selected number of samples
showBinNumberWarning <- function(){
  showModal(
    modalDialog(
      title = "Number of bins warning",
      "Warning: Number of bins should not be greater than the number of samples. Number of bins has been set to the maximum allowed value.",
      easyClose = TRUE
    )
  )
}

# The signal could not be fitted
showNoFitWarning <- function(){
  showModal(modalDialog(
    title = "No fit",
    "The signal could not be fitted.",
    easyClose = TRUE
  )
  )
}