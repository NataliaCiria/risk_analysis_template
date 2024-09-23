#' Summarize Stochastic Node with Key Columns
#'
#' @param mcnode A Monte Carlo node object
#' @param data A dataframe containing key columns
#' @param keys Vector of column names to use as keys (default: NULL)
#'
#' @return A dataframe with stochastic node summary and key columns
#'
#' @importFrom stats summary
#' @importFrom utils unlist
mc_keys_summary <- function(mcnode, data, keys = NULL, agg_by = NULL) {
  # If keys is NULL, use factor columns from data as keys
  if (is.null(keys)) {
    keys <- names(data[sapply(data, is.factor)])
  }
  
  #if aggregated by
  if(!is.null(agg_by)){
    mcnode<-agg_totals_mc(mcnode,data,by=agg_by)
    data<-summarise(data,.by=all_of(agg_by))
    keys<-agg_by
  }
  
  # Access summary data frame (provided in a list)
  summary_l <- summary(mcnode)[[1]]
  
  # If it's a single line mcnode, save single df summary in list for compatibility
  if (!is.list(summary_l)) {
    summary_l <- list(summary_l)
  }
  
  # Extract column names
  summary_names <- colnames(summary_l[[1]])
  
  # Summarize all mcnode variates in a dataframe
  summary_df <- data.frame(matrix(unlist(summary_l), nrow = length(summary_l), byrow = TRUE))
  
  # Include column names
  names(summary_df) <- paste(summary_names)
  
  # Merge keys and summary
  summary_df <- cbind(data[keys], summary_df)
  
  return(summary_df)
}

#' Convert Monte Carlo Node to Long Format
#'
#' @param mcnode A Monte Carlo node object
#' @param data A dataframe containing key columns
#' @param keys Vector of column names to use as keys (default: NULL)
#'
#' @return A long format dataframe of the Monte Carlo node
#'
#' @importFrom tidyr pivot_longer unite
#' @importFrom dplyr %>%
long_mc <- function(mcnode, data, keys = NULL) {
  # If keys is NULL, use factor columns from data as keys
  if (is.null(keys)) {
    keys <- names(data[sapply(data, is.factor)])
  }
  
  # Use mean as central measure (for ordering etc.)
  node_summary <- mc_keys_summary(mcnode, data)
  central_col <- node_summary$mean
  
  # Convert mcnode to long data frame
  mcnode_long <- as.data.frame(t(unmc(mcnode)))
  
  # Merge keys and summary
  results <- cbind(data[keys], central_col, mcnode_long) %>%
    pivot_longer(cols = starts_with("V", ignore.case = FALSE)) %>%
    unite(key, all_of(keys), remove = FALSE)
  
  return(results)
}

#' Create Boxplot for Long Format Monte Carlo Data
#'
#' @param data A long format dataframe of Monte Carlo data
#' @param value Column name for values (default: "value")
#' @param key_label Column name for key labels (default: "key")
#' @param central_col Column name for central measure (default: "central_col")
#'
#' @return A ggplot object representing the boxplot
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot scale_color_gradient scale_fill_gradient scale_x_discrete theme_minimal coord_flip theme element_text
#' @importFrom dplyr %>% group_by_at summarise
long_mc_boxplot <- function(data, value = "value", key_label = "key", central_col = "central_col") {
  my_labels <- data %>%
    group_by_at(c("key", key_label)) %>%
    summarise(central_col = mean(!!sym(value)), .groups = "keep")
  
  my_labels <- my_labels[order(my_labels$central_col), ][[key_label]]
  
  data %>%
    ggplot(aes(x = reorder(key, !!sym(value)), y = !!sym(value), fill = !!sym(value), colour = !!sym(value))) +
    geom_point(aes(x = reorder(key, !!sym(value)), y = !!sym(value)),
               position = position_jitter(width = 0.15, height = 0),
               size = 0.5, alpha = 0.2) +
    geom_boxplot(alpha = 0.4, aes(colour = !!sym(central_col)), fill = "white", outlier.alpha = 0) +
    scale_color_gradient(low = "#119da4", high = "#fe6847", guide = NULL) +
    scale_fill_gradient(low = "#119da4", high = "#fe6847", guide = NULL) +
    scale_x_discrete(labels = my_labels) +
    theme_minimal() +
    coord_flip() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      legend.position = "left",
      plot.title = element_text(size = 12)
    )
}

#' Replace NA and Infinite Values in mcnode
#'
#' This function replaces NA and infinite values in an mcnode object with a specified value.
#'
#' @param mcnode An mcnode object potentially containing NA or infinite values.
#' @param na_value The value to replace NA and infinite values with. Default is 0.
#'
#' @return An mcnode object with NA and infinite values replaced by na_value.
#'
#' @export
#'
#' @examples
#' # Create an mcnode with some NA values
#' x <- mcdata(c(1, 2, NA, 4, Inf, -Inf), type="U")
#' 
#' # Replace NA and infinite values with 0
#' result <- mcnode_na_rm(x)
#' print(result)
#'
#' # Replace NA and infinite values with -1
#' result_custom <- mcnode_na_rm(x, na_value = -1)
#' print(result_custom)

mcnode_na_rm <- function(mcnode, na_value = 0) {
  replace(mcnode, is.na(mcnode) | is.infinite(mcnode), na_value)
}

#' Aggregate Totals for Monte Carlo Nodes
#'
#' @param mcnode A multivariate Monte Carlo node
#' @param data A data frame containing the data
#' @param by A character vector of column names to group by
#' @param keep_dim Logical, whether to return aggregated variates keeping original dimensions
#'
#' @return An aggregated Monte Carlo node
#'
#' @examples
#' # Add examples here
#'
#' @export
agg_totals_mc <- function(mcnode, data, by, keep_dim = FALSE) {
  # Extract each variate (row) of the multivariate mcnode in a different mcnode
  variates_list <- list()
  inv_variates_list <- list()
  for (i in 1:dim(mcnode)[3]) {
    variates_list[[i]] <- extractvar(mcnode, i)
    inv_variates_list[[i]] <- 1 - extractvar(mcnode, i)
  }
  
  # Create index to filter variates by keys
  key_col <- data %>%
    select(all_of(by)) %>%
    unite(everything(), col = "key", sep = ", ", remove = FALSE)
  
  key_levels <- unique(key_col$key)
  
  for (i in 1:length(key_levels)) {
    index <- key_col$key %in% key_levels[i]
    
    # Aggregate filtered variates "Probability at least one of the events happening"
    total_lev <- 1 - Reduce("*", inv_variates_list[index])
    
    # Aggregate level to total_agg (multivariate node)
    if (keep_dim) {
      # One row per original variate
      agg_index <- mcdata(index, type = "0", nvariates = length(index))
      
      if (exists("total_agg")) {
        total_agg <- total_agg + agg_index * total_lev
      } else {
        total_agg <- agg_index * total_lev
      }
      
      key_data <- data
    } else {
      # One row per result
      if (exists("total_agg")) {
        total_agg <- addvar(total_agg, total_lev)
      } else {
        total_agg <- total_lev
      }
      new_by <- by
      key_data <- unique(key_col)
    }
  }
  
  return(total_agg)
}
