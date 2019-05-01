### RAM Legacy Stock Assessment Database - Data Transformation Functions ##########
## Created by Kelly Mistry, kelly.r.mistry@gmail.com
## Last revised: 3/21/2019

#--------------------------------------------------------------------------------
# Function creating dataframe with the number of stocks and either taxonomy 
# groups in each region or regions in taxonomy groups, along with the first 
# and last year for data in each region or taxGroup. This dataframe is used 
# to generate data to put in the table at the top of region or taxGroup pages 
# on the website.
#
# Parameters:
#--- input_data is timeseries_values_views in RAM v4.44
#--- region_or_taxGroup is the vector with either region or taxonomy names
#     that match the corresponding input_data row values (regions or 
#     TB_taxGroup_list)
#--- row_names will be either region_plot_titles or TB_taxGroup_plot_titles

summary_fun <- function(type_of_plot = c("Region", "taxGroup"),
                        input_data,
                        region_or_taxGroup,
                        number_taxGroup_or_region, 
                        row_names) {
  dest_dataframe <- data.frame(matrix(NA, 
                                      nrow = number_taxGroup_or_region, 
                                      ncol = 4))
  if (type_of_plot == "Region") {
    colnames(dest_dataframe) <- c("Num_Stocks", "Num_taxGroup", "First_year", "Last_year")
  } else {
    colnames(dest_dataframe) <- c("Num_Stocks", "Num_Region", "First_year", "Last_year")
  }
  rownames(dest_dataframe) <- row_names
  
  if (type_of_plot == "Region") {
    for (i in 1:number_taxGroup_or_region) {
      data <- input_data[input_data$region == region_or_taxGroup[i], ]
      dest_dataframe[i, 1] <- length(unique(data$stockid))
      dest_dataframe[i, 2] <- length(unique(data$taxGroup))
      dest_dataframe[i, 3] <- min(data$year)
      dest_dataframe[i, 4] <- max(data$year)
    }
  } else {
    for (i in 1:number_taxGroup_or_region) {
      data <- input_data[input_data$taxGroup == region_or_taxGroup[i], ]
      dest_dataframe[i, 1] <- length(unique(data$stockid))
      dest_dataframe[i, 2] <- length(unique(data$region))
      dest_dataframe[i, 3] <- min(data$year)
      dest_dataframe[i, 4] <- max(data$year)
    }
  }
  return(dest_dataframe)
}

#--------------------------------------------------------------------------------

# Funciton to calculate average biomass over the time series for each stock in 
# each region or taxGroup and extract the first and last years that each stock 
# appears in the assessment 
#
# Parameters:
#--- input_data is All_TBbest.df in RAM v4.44
#--- region_or_taxGroup_labels is either region_labels or TB_taxGroup_labels
#--- number_region_or_taxGroup is either number_regions or number_TB_taxGroups
#--- regions_or_taxGroup is the vector with either region or taxonomy names 
#     that match the corresponding input_data row values
#--- regions_or_taxGroup is either regions or TB_taxGroup_list

mean_biomass_fun <- function(type_of_plot = c("Region", "taxGroup"),
                             region_or_taxGroup_labels,
                             number_region_or_taxGroup,
                             input_data,
                             regions_or_taxGroup,
                             year_min) {
  mean_prefix <- "mean_biomass"
  mean_region_names <- c(paste(mean_prefix, region_or_taxGroup_labels, sep = "_"))
  mean_biomass <- vector("list", length = number_region_or_taxGroup)
  names(mean_biomass) <- mean_region_names

  if (type_of_plot == "Region") {
    for (i in 1:number_region_or_taxGroup) {
      num_stocks <- length(unique(input_data$stockid[input_data$region == regions_or_taxGroup[i]]))
      stock_names <- unique(input_data$stocklong[input_data$region == regions_or_taxGroup[i]])
      mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 5))
      colnames(mean_biomass[[i]]) <- c("stock_names", "mean_bio", "first_year", "last_year", "taxGroup")
      x <- subset(input_data, input_data$region == regions_or_taxGroup[i] & input_data$year >= year_min)
      for (j in 1:num_stocks) {
        mean_biomass[[i]][j, 1] <- as.character(stock_names[j])
        mean_biomass[[i]][j, 2] <- round(mean(x$TBbest[x$stocklong == stock_names[j]]))
        mean_biomass[[i]][j, 3] <- min(x$year[x$stocklong == stock_names[j]])
        mean_biomass[[i]][j, 4] <- max(x$year[x$stocklong == stock_names[j]])
        mean_biomass[[i]][j, 5] <- as.character(x$taxGroup[x$stocklong == stock_names[j]][1])
        mean_biomass[[i]] <- arrange(mean_biomass[[i]], desc(mean_bio))
      }
    }
  } else {
    for (i in 1:number_region_or_taxGroup) {
      num_stocks <- length(unique(input_data$stockid[input_data$taxGroup == regions_or_taxGroup[i]]))
      stock_names <- unique(input_data$stocklong[input_data$taxGroup == regions_or_taxGroup[i]])
      mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 5))
      colnames(mean_biomass[[i]]) <- c("stock_names", "mean_bio", "first_year", "last_year", "region")
      x <- subset(input_data, input_data$taxGroup == regions_or_taxGroup[i] & input_data$year >= year_min)
      for (j in 1:num_stocks) {
        mean_biomass[[i]][j, 1] <- as.character(stock_names[j])
        mean_biomass[[i]][j, 2] <- round(mean(x$TBbest[x$stocklong == stock_names[j]]))
        mean_biomass[[i]][j, 3] <- min(x$year[x$stocklong == stock_names[j]])
        mean_biomass[[i]][j, 4] <- max(x$year[x$stocklong == stock_names[j]])
        mean_biomass[[i]][j, 5] <- as.character(x$region[x$stocklong == stock_names[j]][1])
        mean_biomass[[i]] <- arrange(mean_biomass[[i]], desc(mean_bio))
      }
    }
  }
  # Adding jittering to duplicated values so that they will plot on separate
  # lines for biomass coverage plots
  for (i in 1:number_region_or_taxGroup) {
    for (j in 1:nrow(mean_biomass[[i]]))  {
      mean_biomass[[i]]$jittered_mean[j] <- mean_biomass[[i]]$mean_bio[j]
      temp <- mean_biomass[[i]]
      dup_index <- which(duplicated(temp$mean_bio) == TRUE)
      mean_biomass[[i]]$jittered_mean[dup_index] <- jitter(mean_biomass[[i]]$mean_bio[dup_index])
    }
  }
  return(mean_biomass)
}

#--------------------------------------------------------------------------------
# Function to create custom y axis vectors for each of the region or taxGroup
# biomass coverage for all stocks plots

custom_y_axis_fun <- function(number_region_or_taxGroup, 
                              region_or_taxGroup_labels,
                              mean_biomass_data) {
  y_axis_labels <- c(1, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 
                     1000000, 5000000, 10000000, 50000000)
  custom_y_axis <- vector("list", length = number_region_or_taxGroup)
  y_axis_prefix <- "custom_y-axis"
  y_axis_region_names <- c(paste(y_axis_prefix, region_or_taxGroup_labels, sep = "_"))
  names(custom_y_axis) <- y_axis_region_names
  
  for (i in 1:number_region_or_taxGroup) {
    custom_y_axis[[i]]$max <- max(mean_biomass_data[[i]]$mean_bio)
    custom_y_axis[[i]]$min <- min(mean_biomass_data[[i]]$mean_bio)
    custom_y_axis[[i]]$max[2] <- y_axis_labels[(y_axis_labels >= custom_y_axis[[i]]$max) == TRUE][1]
    custom_y_axis[[i]]$min[2] <- rev(y_axis_labels[(y_axis_labels <= custom_y_axis[[i]]$min) == TRUE])[1]
    custom_y_axis[[i]]$y_axis_labels <- subset(y_axis_labels, 
                                               y_axis_labels >= custom_y_axis[[i]]$min[2] & 
                                                 y_axis_labels <= custom_y_axis[[i]]$max[2])
  }
  return(custom_y_axis)
}

#--------------------------------------------------------------------------------
# Function creating dataframe with absolute mean surplus production for each 
# stock in each region or taxGroup, ordered by mean SP so that the top 4 stocks 
# can be plotted individually

mean_SP_fun <- function(surplus_file, 
                        type_of_plot = c("Region", "Taxonomy Group"),
                        region_or_taxGroup,
                        region_or_taxGroup_labels,
                        number_region_or_taxGroup) {
  mean_sp_prefix <- "mean_surplus"
  mean_sp_region_names <- c(paste(mean_sp_prefix, region_or_taxGroup_labels, sep = "_"))
  surplus_mean_biomass <- vector("list", length = number_region_or_taxGroup)
  names(surplus_mean_biomass) <- mean_sp_region_names
  surplus_stock_ids <- unique(surplus_file$stockid)
  
  if (type_of_plot == "Region") {
    for (i in 1:number_region_or_taxGroup) {
      x <- subset(surplus_file, 
                  surplus_file$region == region_or_taxGroup[i] & 
                    is.na(surplus_file$SP) == FALSE)
      sp_stock_names <- unique(x$stocklong)
      num_stocks <- length(sp_stock_names)
      surplus_mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 3))
      colnames(surplus_mean_biomass[[i]]) <- c("stock_names", "mean_bio","mean_surplus")
      for (j in 1:num_stocks) {
        surplus_mean_biomass[[i]][j, 1] <- as.character(sp_stock_names[j])
        surplus_mean_biomass[[i]][j, 2] <- round(mean(x$B[x$stocklong == sp_stock_names[j]]))
        surplus_mean_biomass[[i]][j, 3] <- round(mean(abs(x$SP[x$stocklong == sp_stock_names[j]])))
        surplus_mean_biomass[[i]] <- arrange(surplus_mean_biomass[[i]], desc(mean_bio))
      }
      surplus_mean_biomass[[i]]$percent_of_total <- surplus_mean_biomass[[i]][, 3]/sum(surplus_mean_biomass[[i]][, 3])
    }
  } else {
    for (i in 1:number_region_or_taxGroup) {
      x <- subset(surplus_file, 
                  surplus_file$taxGroup == region_or_taxGroup[i] & 
                    is.na(surplus_file$SP) == FALSE)
      sp_stock_names <- unique(x$stocklong)
      num_stocks <- length(sp_stock_names)
      surplus_mean_biomass[[i]] <- data.frame(matrix(NA, nrow = num_stocks, ncol = 3))
      colnames(surplus_mean_biomass[[i]]) <- c("stock_names", "mean_bio","mean_surplus")
      for (j in 1:num_stocks) {
        surplus_mean_biomass[[i]][j, 1] <- as.character(sp_stock_names[j])
        surplus_mean_biomass[[i]][j, 2] <- round(mean(x$B[x$stocklong == sp_stock_names[j]]))
        surplus_mean_biomass[[i]][j, 3] <- round(mean(abs(x$SP[x$stocklong == sp_stock_names[j]])))
        surplus_mean_biomass[[i]] <- arrange(surplus_mean_biomass[[i]], desc(mean_bio))
      }
      surplus_mean_biomass[[i]]$percent_of_total <- surplus_mean_biomass[[i]][, 3]/sum(surplus_mean_biomass[[i]][, 3])
    }
  }
  return(surplus_mean_biomass)
} 









