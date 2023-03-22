# Pretty statsBy

Extending the statsBy Funtionality of the Psych Package. \## Installation You can install this package by running the following in your R-installation:

``` r
install.packages("devtools")
devtools::install_github("Pascal-Kueng/prettystatsBy")
```

## Usage

``` r
statsBy(
  data = NULL,
  group = NULL,
  alpha = 0.05,
  var_names = NULL,
  pretty_alphas = c(0.05, 0.01, 0.001),
  ...
)
```

## Arguments

-   `data` Dataframe only including Variables to be included in correlation table.

-   `group` Clustering variable, such as a personID

-   `alpha` significance Level that is passed to the psych::statsBy function. This significance level is used for all outputs provided by the original psych::statsBy function, but is not used for the functionality provided by this specific package.

-   `var_names` a list of names that are used for pretty printing

-   `pretty_alphas` you may provide a list of 3 alpha levels that are used to calculate the confidence interval for the prettystatsBy output.

-   `...` Refer to the psych::statsBy documentation for additional parameters.

## Value

The function returns a psych::statsBy object that retains all functionality of the original object, while extending the functionality. The function automatically calculates confidence intervals with the alpha levels of 0.05, 0.01, and 0.001 and provides formatted correlation tables with indications of significance. All original calls and exact confidence intervals for the alpha level provided to the function can still be obtained via the regular functionality of the psych::statsBy function.

## Examples

``` r
# create the object (example 1)
statsByObject <- statsBy(df[,vars], df$userID, var_names=vars)
# create the object (example 2)
statsByObject <- statsBy(df, 'userID', var_names=colnames(df))

# extract information
print(statsByObject) # To explore the original functionality
print(statsByObject$pretty$within) # Full within- cluster correlation matrix
print(statsByObject$pretty$between) # Full between- cluster correlation matrix
print(statsByObject$pretty$combined) # top half are within-correlations, bottom half are between correlations.
```

## Citation

Don't forget to also cite the psych package. To cite prettystatsBy in publications use:

*Küng P (2023). PrettystatsBy: Extending the 'statsBy' Functionality of the Pych Package. University of Zurich. R package version 0.1.0, <https://github.com/Pascal-Kueng/prettystatsBy>.*

BibTeX-Entry:

``` bibtex
  @Manual{,
    title = {PrettystatsBy: Extending the 'statsBy' Functionality of the Pych Package},
    author = {Pascal Küng},
    organization = {University of Zurich},
    year = {2023},
    note = {R package version 0.1.0},
    url = {https://github.com/Pascal-Kueng/prettystatsBy},
  }
```

