#' Apply NEJM-inspired ggplot2 Theme  
#'  
#' A minimal, clean theme for plots, mimicking the *New England Journal of Medicine* style.  
#' Removes grid lines, adjusts fonts, and formats axes/legends for readability.  
#'  
#' @return A ggplot2 theme object.  
#' @examples  
#' ggplot(mtcars, aes(x = mpg, y = wt)) +  
#'   geom_point() +  
#'   theme_nejm()  
theme_nejm <- function() {
  theme_bw() +
    theme(
      # remove all grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # remove background of plot titles
      strip.background = element_blank(),
      # remove borders...
      panel.border = element_blank(),
      # ...and add them again for axes
      axis.line = element_line(colour = "black"),
      text = element_text(size = 14),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "top",
    )
}

#' Apply custom, publication-ready ggplot2 Theme  
#'  
#' Ajdust grid lines, fonts, and formats axes/legends for readability.  
#'  
#' @return A ggplot2 theme object.  
#' @examples  
#' ggplot(mtcars, aes(x = mpg, y = wt)) +  
#'   geom_point() +  
#'   theme_pub()  
theme_pub <- function() {
  theme_bw() +
    theme(
      # remove all grid lines
      panel.grid.major.y = element_line(color = 'darkgrey', linetype = 'dashed'),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      # remove background of plot titles
      strip.background = element_blank(),
      # remove borders...
      # panel.border = element_blank(),
      # ...and add them again for axes
      # axis.line = element_line(colour = "black"),
      # text = element_text(size = 18),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#' Adjust Flextable Width to Fit A4 Word Document  
#'  
#' Automatically resizes a `flextable` object to span the width of an A4 page in Word  
#' while preserving relative column proportions.  
#'  
#' @param ft A `flextable` object.  
#' @param pgwidth Numeric scaling factor (default: 6). Increase/decrease to fine-tune.  
#' @return A `flextable` with adjusted column widths.  
#' @examples  
#' ft <- flextable(head(mtcars))  
#' ft %>% fit_pagewidth(pgwidth = 6)  
fit_pagewidth <- function(ft, pgwidth = 6){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth / (flextable_dim(ft_out)$widths))
  return(ft_out)
}

#' Test Categorical Variable Independence  
#'  
#' Automatically chooses between Fisher's exact test (for small samples)  
#' and chi-squared test to assess independence between two categorical variables.  
#'  
#' @param df Data frame containing the variables.  
#' @param x,y Names of categorical variables (as strings).  
#' @param only_p If `TRUE` (default), returns only the p-value; otherwise, returns full test results.  
#' @return Either a p-value (if `only_p = TRUE`) or an `htest` object.  
#' @examples  
#' test_independence(iris, "Species", "Sepal.Length > 5")  
test_independence <- function(df, x, y, only_p = TRUE) {
  if(any(table(df[[x]], df[[y]]) < 6)) {
    test <- fisher.test(x = df[[x]], y = df[[y]])
    fisher <- TRUE
  } else {
    test <- chisq.test(x = df[[x]], y = df[[y]])
    fisher <- FALSE
  }
  if (only_p) {
    return(test$p.value)
  } else {
    return(test) 
  }
}

#' Check for Overdispersion in GLMs  
#'  
#' Computes Pearson chi-squared statistic to test for overdispersion  
#' in Poisson or negative binomial models.  
#'  
#' @param model A fitted GLM (e.g., from `glm()` or `glm.nb()`).  
#' @return A named vector with:  
#' - `chisq`: Pearson chi-squared statistic  
#' - `ratio`: Chi-squared / residual degrees of freedom  
#' - `rdf`: Residual degrees of freedom  
#' - `p`: P-value for overdispersion test  
#' @examples  
#' model <- glm(count ~ spray, data = InsectSprays, family = poisson)  
#' overdisp_fun(model)  
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#' Automatically Select and Fit Model to Count Data  
#'  
#' Fits either Poisson or negative binomial regression based on  
#' overdispersion diagnostics. Chooses NB if overdispersion is detected  
#' (p < 0.05 and variance > mean).  
#'  
#' @param data Input data frame.  
#' @param outcome Name of count-dependent variable (string).  
#' @param pred Predictor variable(s) (string, e.g., "x1 + x2").  
#' @param p_only If `TRUE` (default), returns only the p-value for the predictor;  
#'               otherwise, returns the full model.  
#' @return Either a p-value or a `glm`/`negbin` object.  
#' @examples  
#' test_count(InsectSprays, "count", "spray", p_only = TRUE)  
test_count <- function(data, outcome, pred, p_only = TRUE) {
  current_formula = as.formula(paste(outcome, "~", pred))
  
  if ((overdisp_fun(glm(current_formula, 
                        data = data, 
                        family = "poisson"))["p"] < .05)
      & (var(data[[outcome]], na.rm=TRUE) > mean(data[[outcome]], na.rm=TRUE))) { 
    current_model <- glm.nb(current_formula, data)
    # print(paste0("For the count variable ", outcome, ", the selected model is negative binomial regression"))
  } else {
    
    current_model <- glm(current_formula, data, family = "poisson")
    #print(paste0("For the count variable ", outcome, ", the selected model is Poisson regression"))
  }
  
  if (p_only) {
    return(anova(current_model)[2, "Pr(>Chi)"])
  } else {
    return(current_model)
  }
}