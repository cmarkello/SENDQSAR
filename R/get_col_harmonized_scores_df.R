#' @title get_col_harmonized_scores_df
#' @description
#' This function harmonizes liver score data by cleaning column names,
#' replacing missing values with zeros, and optionally rounding specific columns.
#' The function also identifies and harmonizes synonyms, removes unnecessary columns,
#' and reorders the data based on column sums.
#'
#' @param liver_score_data_frame A data frame containing liver score data.
#'   This data frame should have column names that may require harmonization.
#' @param Round A logical value indicating whether the data should be rounded.
#'   If TRUE, certain liver-related columns are floored and capped, and histology-related columns are ceiled. Default is FALSE.
#'
#' @details
#' The function performs the following operations:
#' - Harmonizes column names by replacing spaces, commas, and slashes with dots.
#' - Replaces missing values (NA) with zero.
#' - Identifies and harmonizes synonym columns, replacing their values with the higher value between the synonyms.
#' - Removes specific unwanted columns such as 'INFILTRATE', 'UNREMARKABLE', 'THIKENING', and 'POSITIVE'.
#' - Optionally rounds liver score columns by flooring and capping them at 5, and histology-related columns by ceiling.
#' - Reorders columns based on the sum of their values.
#'
#' @return A data frame with harmonized liver scores, optional rounding, and columns reordered based on their sums.
#' First column is "STUDYID",  followed by columns with score values.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- get_col_harmonized_scores_df(liver_score_data_frame = liver_scores, Round = TRUE)
#' }
#'
#' @export
get_col_harmonized_scores_df <- function(liver_score_data_frame,
                                         Round = FALSE){


  ###-----------column harmonization of "liver_scores"-------------
  liver_scores <- liver_score_data_frame

  # Replace spaces and commas in column names with dots
  colnames(liver_scores) <- gsub(' ', '.', colnames(liver_scores))
  colnames(liver_scores) <- gsub(',', '.', colnames(liver_scores))
  colnames(liver_scores) <- gsub('/', '.', colnames(liver_scores))

  # Standardize column names in 'liver_scores' by replacing invalid characters (e.g., spaces, +, -, /) with dots (.)
  # and ensuring all names are syntactically valid and unique for safe access in R.
  colnames(liver_scores) <- make.names(colnames(liver_scores))

  # Replace all NA (missing) values in the 'liver_scores' data frame with '0'
  liver_scores[is.na(liver_scores)] <- 0

  #Identify Columns with Periods
  findings2replaceIndex <- grep('.', colnames(liver_scores), fixed = T)

  #Identify Unique Column Names without Periods
  fn2replace <- unique(toupper(colnames(liver_scores)[-findings2replaceIndex]))

  # Store Column Names with Periods
  f2replace <- colnames(liver_scores)[findings2replaceIndex]

  #Remove Specific Columns from Processing
  removeIndex <- which(fn2replace %in% c('STUDYID',
                                         'UNREMARKABLE',
                                         'THIKENING',
                                         'POSITIVE'))

  fn2replace <- fn2replace[-removeIndex]

  #--------------------------------------------------------------------------
  #Harmonize Synonym Columns
  # for (finding in fn2replace) {
  #   # Find synonyms matching the current finding
  #   synonyms <- grep(finding, f2replace, ignore.case = T, value = T)
  #
  #   # Process each synonym
  #   for (synonym in synonyms) {
  #     index <- which(liver_scores[[synonym]] > 0)
  #
  #     # Harmonize values for matching indices
  #     for (i in index) {
  #       if (liver_scores[[synonym]][i] > liver_scores[[finding]][i]) {
  #         liver_scores[[finding]][i] <- liver_scores[[synonym]][i]
  #       }
  #     }
  #   }
  # }
 #---------------------------------------------------------------------------

  # Harmonize Synonym Columns
  for (finding in fn2replace) {

    # Find synonyms matching the current finding
    synonyms <- grep(finding, f2replace, ignore.case = TRUE, value = TRUE)

    # If no synonyms are found, move to the next iteration
    if (length(synonyms) == 0) {
      next
    }

    # Process each synonym
    for (synonym in synonyms) {
      # Check if columns exist in liver_scores to avoid errors
      if (!(synonym %in% names(liver_scores)) || !(finding %in% names(liver_scores))) {
        warning(paste("Missing columns for:", synonym, "or", finding))
        next
      }

      # Find indices where the synonym has positive values
      index <- which(liver_scores[[synonym]] > 0)

      # Harmonize values for matching indices
      for (i in index) {
        # Check index bounds to avoid errors
        if (i > length(liver_scores[[finding]]) || i > length(liver_scores[[synonym]])) {
          warning("Index out of bounds for", finding, "or", synonym)
          next
        }

        # Update finding values if synonym values are greater
        if (liver_scores[[synonym]][i] > liver_scores[[finding]][i]) {
          liver_scores[[finding]][i] <- liver_scores[[synonym]][i]
        }
      }
    }
  }

  # Return the modified or original liver_scores data frame
  #return(liver_scores)



#------------------------------------------------------------------------


  #Remove Synonym Columns
  liver_scores <- liver_scores[,-findings2replaceIndex]

 # rename the "liver_scores"
  Data <- liver_scores

  removeEndpoints <- c('INFILTRATE', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')


  #Data <- column_harmonized_liver_scores

  removeIndex <- which(colnames(Data) %in% removeEndpoints)

  # remove the removeIndex
  if (length(removeIndex) > 0) {
    removedCols <- colnames(Data)[removeIndex]
    cat("Removing columns:", paste(removedCols, collapse = ", "), "\n")
    Data <- Data[, -removeIndex]
  } else {
    cat("...No matching columns found to remove.....\n")
  }

  print(dim(Data))

  # Check if the "Round" condition is TRUE
  # zscoreIndex is basically rounding the livertobw and LB score columns
  # histoindex is basically rounding the MI score columns----------------
  if (Round == T) {


    # Identify columns where the name contains "avg_" or "liver"
    zscoreIndex <- unique(c(grep('avg_', colnames(Data)), grep('liver', colnames(Data))))
    # "----one-column can contain both 'avg' and 'liver'..need to figure it out"
    #change_the_avg_llivertoBW_zscore to "avg_livtoBW_zscore"---------------

    # Loop through each column identified in zscoreIndex
    for (i in zscoreIndex) {

      # Floor the values in the column (round down to the nearest integer)
      Data[, i] <- floor(Data[, i])

      # Find indices of values greater than 5 in the column
      maxIndex <- which(Data[, i] > 5)

      # Cap values at 5 for these indices
      Data[maxIndex, i] <- 5
    }
    # Identify columns where the first character of the name is an uppercase letter
    histoIndex <- which(substr(colnames(Data), 1, 1) %in% toupper(letters))

    # Exclude the first column from the selection
    histoIndex <- histoIndex[-1]

    # Loop through each column identified in histoIndex
    for (i in histoIndex) {
      # Ceiling the values in the column (round up to the nearest integer)
      Data[, i] <- ceiling(Data[, i])
    }
  }

  # Calculate the sum of values for each column in Data, starting from the 2nd column onwards
  # Exclude missing values (na.rm = TRUE) in the calculation
  columnSums <- sort(colSums(Data[,2:ncol(Data)], na.rm = T), decreasing = T) # This produced named attribute

  # Reorder the columns in Data (from the 2nd column onwards) based on the sorted column sums
  # This aligns the columns with higher sums (more "important" columns) to the leftmost positions
  Data[,2:ncol(Data)] <- Data[, names(columnSums)]

  # Update the column names in Data (from the 2nd column onwards) to match the reordered columns
  # This ensures that the column names are consistent with the new ordering
  colnames(Data)[2:ncol(Data)] <- names(columnSums)




  return(as.data.frame(Data))
}


