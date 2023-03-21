
#' Extending the statsBy Funtionality of the Psych Package.
#'
#' @param data Dataframe only including Variables to be included in correlation table.
#' @param group Clustering variable, such as a personID
#' @param alpha significance Level that is passed to the psych::statsBy function.
#'    This significance level is used for all outputs provided by the original
#'    psych::statsBy function, but is not used for the functionality provided
#'    by this specific package.
#' @param var_names a list of names that are used for pretty printing
#' @param pretty_alphas you may provide a list of 3 alpha levels that are used
#'    to calculate the confidence interval for the prettystatsBy output.?p
#' @param ... Refer to the psych::statsBy documentation for additional parameters.
#'
#' @return The function returns a psych::statsBy object that retains all
#'    functionality of the original object, while extending the functionality.
#'    The function automatically calculates confidence intervals with the
#'    alpha leves specified in pretty_alphas and provides formatted correlation
#'    tables with indications of significance. All original calls and exact
#'    confidence intervals for the single alpha level provided to the function can
#'    still be obtained via the regular functionality of the psych::statsBy function.
#' @export
#'
#' @examples # create the object (example 1)
#' statsByObject <- statsBy(df[,vars], df$userID, var_names=vars)
#' # create the object (example 2)
#' statsByObject <- statsBy(df, 'userID', var_names=colnames(df))
#'
#' # extract information
#' print(statsByObject) # To explore the original functionality
#' print(statsByObject$pretty.within) # Full within- cluster correlation matrix
#' print(statsByObject$pretty.between) # Full between- cluster correlation matrix
#' print(statsByObject$pretty.combined) # top half are within-correlations, bottom half are between correlations.
#'
#'
statsBy <- function(data = NULL, group = NULL, alpha = 0.05, var_names = NULL, pretty_alphas = c(0.05, 0.01, 0.001), ...) {

  data <- as.data.frame(data)

  # if group is provided via the $ operator, we check if it is also in the data and remove it if yes.
  if (is.character(group)){
    var_names <- var_names[var_names!=group]
  } else {
    for (i in 1:length(data)) {
      if (identical(data[[i]], group)) {
        group_name <- colnames(data[i])
        data <- data[,-i]
        var_names <- var_names[-i]
        break
      }
    }
  }

  pretty_alphas <- prepare_alphas(pretty_alphas)

  # Reference Object that is modified
  statsByObject <- psych::statsBy(data = data, group = group, alpha = alpha, ...)

  cors05 <- get_confidence_intervals(data = data, group = group, alpha = pretty_alphas[1], ...)
  cors01 <- get_confidence_intervals(data = data, group = group, alpha = pretty_alphas[2], ...)
  cors001 <- get_confidence_intervals(data = data, group = group, alpha = pretty_alphas[3], ...)

  within <- convert_to_matrix(cors05$within_cors, cors01$within_cors, cors001$within_cors)
  between <- convert_to_matrix(cors05$between_cors, cors01$between_cors, cors001$between_cors)

  combined <- combine_matrices(within, between)

  # rename the variables and annotate tables
  annotation_combined <- "Note. Within-group correlations are in the upper triangle, between-group correlations in the lower triangle."
  annotation <- sprintf("*CI(%s%%) significant, **CI(%s%%) significant, ***CI(%s%%) significant.", ((1-pretty_alphas[1]) * 100), ((1-pretty_alphas[2]) * 100), ((1-pretty_alphas[3]) * 100))
  statsByObject$pretty.within <- list(matrix_to_tibble(within, var_names), annotation)
  statsByObject$pretty.between <- list(matrix_to_tibble(between, var_names), annotation)
  statsByObject$pretty.combined <- list(matrix_to_tibble(combined, var_names), annotation_combined, annotation)
  statsByObject$pretty <- list(within = statsByObject$pretty.within, between = statsByObject$pretty.between, combined = statsByObject$pretty.combined)

  return(statsByObject)
}


prepare_alphas <- function(pretty_alphas) {
  if (length(pretty_alphas) == 0){
    return(pretty_alphas)
  } else if (length(pretty_alphas) != 3) {
    stop("Provide a vector of three alpha levels for 'pretty_alphas'")
  } else if (!all(sapply(pretty_alphas, function(x) is.numeric(x) && x > 0 && x < 1))) {
    stop("pretty_alphas must be between 0 and 1")
  } else {

    pretty_alphas <- sort(pretty_alphas, decreasing=TRUE)
    return(pretty_alphas)
  }


}


get_confidence_intervals <- function(data = NULL, group = NULL, alpha = NULL, ...) {
  statsByObject <- suppressWarnings(psych::statsBy(data = data, group = group, alpha = alpha, ...))

  within_cors <- statsByObject$ci.wg$r.ci
  between_cors <- statsByObject$ci.bg$r.ci

  return(list(within_cors = within_cors, between_cors = between_cors))
}

convert_to_matrix <- function(input_cors05, input_cors01, input_cors001) {
  df_cors <- data.frame(var1 = character(), var2 = character(), r_value = character())

  for (i in 1:nrow(input_cors05)) {
    # extracting the variable names
    var1 <- strsplit(row.names(input_cors05)[i], "-")[[1]][1]
    var2 <- strsplit(row.names(input_cors05)[i], "-")[[1]][2]

    # extracting the CI values
    lower05 <- input_cors05[i, 1]
    upper05 <- input_cors05[i, 3]

    lower01 <- input_cors01[i, 1]
    upper01 <- input_cors01[i, 3]

    lower001 <- input_cors001[i, 1]
    upper001 <- input_cors001[i, 3]

    # Adding stars according to significance levels

    r_value <- format(round(input_cors05[i, 2], 2), nsmall = 2)
    if (is.na(lower05) | is.na(lower01) | is.na(lower001)) {
      r_with_star <- "-"
    } else if (lower05 < 0 && upper05 > 0) {
      r_with_star <- as.character(r_value)
    } else if (lower01 < 0 && upper01 > 0) {
      r_with_star <- paste0(as.character(r_value), "*")
    } else if (lower001 < 0 && upper001 > 0) {
      r_with_star <- paste0(as.character(r_value), "**")
    } else {
      r_with_star <- paste0(as.character(r_value), "***")
    }
    df_cors <- rbind(df_cors, data.frame(var1 = var1, var2 = var2, r_value = r_with_star))
  }

  # Get unique variable names
  variables <- unique(c(df_cors$var1, df_cors$var2))

  # Create an empty correlation matrix
  cor_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
  rownames(cor_matrix) <- variables
  colnames(cor_matrix) <- variables

  # Fill the correlation matrix with r_values
  for (i in 1:nrow(df_cors)) {
    row_var <- df_cors$var1[i]
    col_var <- df_cors$var2[i]
    r_value <- df_cors$r_value[i]

    # Since correlation matrices are symmetric, we fill both positions [row, col] and [col, row]
    cor_matrix[row_var, col_var] <- r_value
    cor_matrix[col_var, row_var] <- r_value
  }

  # Set the diagonal to 1 (correlation of a variable with itself is always 1)
  diag(cor_matrix) <- 1

  return(cor_matrix)
}


combine_matrices <- function(within_matrix, between_matrix) {
  combined_matrix <- within_matrix

  for (i in 1:nrow(combined_matrix)) {
    for (j in 1:ncol(combined_matrix)) {
      if (i > j) {
        combined_matrix[i, j] <- between_matrix[i, j]
      }
    }
  }
  return(combined_matrix)
}

# to rename the variables in the matrix
matrix_to_tibble <- function(matrix, var_names) {
  df <- as.data.frame(matrix)
  tibble <- tibble::rownames_to_column(df, "Variable")
  # rename the columns
  if (!is.null(var_names)) {
    colnames(tibble) <- c("Variable", var_names)
    tibble$Variable <- var_names
  }
  return(tibble)
}

calm <- function() {
  print('This is fine :)')
}
