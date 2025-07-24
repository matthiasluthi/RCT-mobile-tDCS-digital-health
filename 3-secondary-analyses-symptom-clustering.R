
# Load libraries & data ------------------------

library(tidyverse)
library(ggpubr)
library(sjmisc)
library(ggdendro)
library(rlang)
library(lme4)
library(lmerTest)
library(lmtest)
library(broom.mixed)
library(glue)
library(flextable)
library(emmeans)
library(effectsize)
library(ggpubr)
library(dynamicTreeCut)

load(data/psylect.RData)


# function-definitions ---------------------------------------------------------
transform_item_names <- function(x) {
  y <- gsub(pattern = "_", replacement = " ", x)
  z <- gsub(pattern = "^H0|^H", replacement = "", y)
  z <- str_split_fixed(z, " ", n = 2)
  paste0(paste(z[[2]], z[[1]], sep = " ("), ")")
}

create_hc <- function(data, dist, clust, h, return = TRUE, show_graph = TRUE, 
                      draw_threshold = TRUE) {
  
  # Create title
  if (endsWith(deparse(substitute(data)), ".n1")) {
    graph_title <- "Items normalized (N1), "
  } else if (endsWith(deparse(substitute(data)), ".n2")) {
    graph_title <- "Items normalized (N2), "
  } else if (endsWith(deparse(substitute(data)), ".s")) {
    graph_title <- "Items standardized, "
  } else {
    graph_title <- "Raw scores, "
  }
  graph_title <- paste0(graph_title, dist, " distance, ", clust, " clustering")
  
  # Prepare data
  data_temp <- data
  rownames(data_temp) <- sapply(rownames(data_temp), FUN = transform_item_names)
  
  # Fit hc model
  hc_model <- hclust(dist(data_temp, method = dist), method = clust)
  
  # Create dendrogram
  if (draw_threshold) {
    graph <- ggdendrogram(hc_model, rotate = TRUE) +
      geom_hline(yintercept = h, linetype = "dashed", color = "grey") +
      ggtitle({{graph_title}})
  } else {
    graph <- ggdendrogram(hc_model, rotate = TRUE) +
      ggtitle({{graph_title}})
  }
  
  if (show_graph) {show(graph)}
  
  if (return) {return(list(model = hc_model, data = data, title = graph_title, graph = graph, h = h))}
}

test_clusters <- function(hc, df, conditions = 3, time_log = FALSE, 
                          threelevels = FALSE, print_table = TRUE, 
                          print_models = FALSE, clustering = NULL, 
                          return = TRUE, fdr_correction = FALSE) {
  
  if (time_log) {time = "time_log"} else { time = "time"}
  
  # Create dataframe with columns item, cluster_num, cluster_desc  
  if (is.null(clustering)) {
    cluster_items <- hc$data %>% 
      transmute(cluster_num = cutree(hc$model, h = hc$h)) %>%
      rownames_to_column(var = "item") %>%
      mutate(cluster_desc = NA_character_)
  } else {
    cluster_items <- data.frame(
      item = row.names(hc$data),
      cluster_num = clustering,
      cluster_desc = NA_character_
    )
  }
  
  # Copy df
  clstrs <- df
  
  # Create sum scores 
  for (i in 1:max(cluster_items$cluster_num)) {
    current_cluster <- paste0("cluster_", i)
    
    # Get cluster items
    current_items <- cluster_items %>% 
      filter(cluster_num == i) %>%
      select(item) %>%
      pull()
    
    # Create cluster sum: arbitrarily set to mean and SD of 10
    clstrs <- clstrs %>%
      rowwise() %>%
      mutate({{current_cluster}} := sum(c_across(all_of(current_items)))) %>%
      ungroup() # %>%
    # 2-level model
    # Create lmer formula and correct row selection in lmer for later extraction
    if (conditions == 2) {
      formula <- as.formula(paste(current_cluster, "~ condition_tdcs *", time, 
                                  "+ (1|subject)"))
      rows <- 4
    } else if (conditions == 3){
      formula <- as.formula(paste(current_cluster, "~ condition *", time, 
                                  "+ (1|subject)"))
      rows <- 5:6
    }
    
    # Run lmer
    lmm <- lmer(formula, clstrs)
    if (print_models) {print(summary(lmm))}
    
    # Add a descriptive cluster column to cluster_items
    current_cluster_desc <- sapply(current_items, FUN = transform_item_names)[1:3]
    current_cluster_desc <- paste(word(current_cluster_desc, -2), collapse = ", ")
    current_cluster_desc <- paste0("Cluster ", i, " (", current_cluster_desc, ")")
    
    cluster_items <- cluster_items %>%
      mutate(cluster_desc = if_else(cluster_num == i, 
                                    {{current_cluster_desc}}, 
                                    cluster_desc))
    # Create tidy version of LMM
    lmm_tidy <- tidy(lmm)[rows, 3:8] 

    # Calculate effect size: Cohen's d (The new way)
    # Create emmeans trends for current object
    if (conditions == 2) {
      emt <- emtrends(lmm, ~condition_tdcs, var = time)
    } else {
      emt <- emtrends(lmm, ~condition, var = time)
    }
    if (time_log) {
        cohensd <- summary(eff_size(emt,
                                    sigma = sigma(lmm),
                                    edf = df.residual(lmm))) %>%
          mutate(d_complete = paste0(
            round(log(7)*effect.size, 2),
            " [",
            round(log(7)*lower.CL, 2),
            ", ",
            round(log(7)*upper.CL, 2),
            "]"
          ))
    } else {
      cohensd <- summary(eff_size(emt,
                                  sigma = sigma(lmm),
                                  edf = df.residual(lmm))) %>%
        mutate(d_complete = paste0(
          round(6*effect.size, 2),
          " [",
          round(6*lower.CL, 2),
          ", ",
          round(6*upper.CL, 2),
          "]"
        ))    
      
    }

    # Put results in a table
    if (conditions == 3) {
      if(current_cluster == "cluster_1"){
        table_results <- cbind(cluster_desc = current_cluster_desc, 
                               lmm_tidy,
          "Cohen's d" = cohensd$d_complete[1:2]) %>%
          add_row(term = {{current_cluster_desc}}, .before = 1) 
      } else {
        table_results <- table_results %>%
          add_row(term = {{current_cluster_desc}}) %>%
          add_row(cluster_desc = current_cluster_desc, 
                  lmm_tidy,
                  "Cohen's d" = cohensd$d_complete[1:2])
      }
    # Else: There are only 2 conditions, everything goes on 1 line
    } else {
      if(current_cluster == "cluster_1"){
        table_results <- cbind(cluster_desc = current_cluster_desc, 
                               term = current_cluster_desc,
                               select(lmm_tidy, -term),
                               "Cohen's d" = cohensd$d_complete)
      } else {
        table_results <- table_results %>%
          add_row(cluster_desc = current_cluster_desc, 
                  term = current_cluster_desc,
                  select(lmm_tidy, -term),
                  "Cohen's d" = cohensd$d_complete)
      }      
    }
    
  } 

  if (threelevels) {
    # Create long format of clusters
    clstrs_lg <- clstrs %>%
      select(subject, condition, condition_tdcs, time, time_log, starts_with("cluster_")) %>%
      pivot_longer(cols = starts_with("cluster_"),
                   names_to = "cluster_type",
                   values_to = "cluster_score",
                   names_transform = list(cluster_type = as.factor)
      )
    
    # 3-level model
    # Create lmer formula 
    if (conditions == 2) {
      formula.3lvl <- as.formula(
        paste("cluster_score ~ condition_tdcs * cluster_type *",
              time, 
              "+ (1|subject/cluster_type)"))
    } else if (conditions == 3){
      formula.3lvl <- as.formula(
        paste("cluster_score ~ condition * cluster_type *",
              time, 
              "+ (1|subject/cluster_type)"))
    }
    
    # Run model
    lmm.3lvl <- lmer(formula.3lvl, clstrs_lg, control = lmerControl(optimizer = "bobyqa"))
    if (print_models) {print(anova(lmm.3lvl))}
    
    # Create tidy version (used at the end of the function)
    lmm.3level.tidy <- tidy(anova(lmm.3lvl))[7, ]
    
    # emtrends
    if (conditions == 3) {
      emt.3lvl <- emtrends(lmm.3lvl, ~condition * cluster_type, var = "time")
      emt.tidy <- tidy(contrast(emt.3lvl, 
                                method = list("tDCS & DI" =c(-1, 1, 0), 
                                              "tDCS only" = c(-1, 0, 1)), 
                                by = "cluster_type", 
                                adjust = "none"))
      
    } else if (conditions == 2) {
      emt.3lvl <- emtrends(lmm.3lvl, ~condition_tdcs * cluster_type, var = "time")
      emt.tidy <- tidy(contrast(emt.3lvl, 
                                method = list("tDCS (combined)" =c(-1, 1)), 
                                by = "cluster_type", 
                                adjust = "none"))
    }
    # Add descriptive cluster name to emt.tidy
    emt.tidy$cluster_desc <- NA_character_
    
    for (i in 1:max(cluster_items$cluster_num)) {
      current_cluster_desc <- cluster_items %>%
        filter(cluster_num == i) %>%
        select(cluster_desc) %>%
        unique() %>%
        pull()
      emt.tidy <- emt.tidy %>%
        mutate(cluster_desc = if_else(str_detect(cluster_type, as.character(i)), 
                                      {{current_cluster_desc}}, 
                                      cluster_desc))
      
    }
    
    # Add the p-values from emt.tidy to table_results
    table_results <- table_results %>%
      mutate(
        # Give proper comparison names
        term = str_replace(term, 
                           "conditiontDCS & DI:time",
                           "tDCS & DI"),
        term = str_replace(term, 
                           "conditiontDCS only:time",
                           "tDCS only"),
        term = str_replace(term, 
                           "condition_tdcstDCS:time",
                           "tDCS (combined)"),      
        term = str_replace(term, 
                           "_log",
                           "")
      ) %>%
      left_join(select(emt.tidy, cluster_desc, term = contrast, p.3lvl = p.value),
                by = c("cluster_desc", "term")) %>%
      select(-cluster_desc) %>%
      # add three-way interaction to top
      add_row(term = "Three-way interaction",
              statistic = lmm.3level.tidy$statistic,
              df = lmm.3level.tidy$DenDF,
              p.3lvl = lmm.3level.tidy$p.value, 
              .before = 1)
    
    # Without three-levels
  } else {
    table_results <- table_results %>%
      mutate(
        # Give proper comparison names
        term = str_replace(term, 
                           "conditiontDCS & DI:time",
                           "tDCS & DI"),
        term = str_replace(term, 
                           "conditiontDCS only:time",
                           "tDCS only"),
        term = str_replace(term, 
                           "condition_tdcstDCS:time",
                           "tDCS (combined)"),      
        term = str_replace(term, 
                           "_log",
                           "")
      ) %>%
      select(-cluster_desc)  
  }

  # FDR correction 
  if (fdr_correction) {
    table_results <- table_results %>%
      mutate(p.fdr = p.adjust(p.value, method = "fdr"))
  }
  
  # Create formatted results table
  ft_results <- format_table(table_results, threelevels, conditions, fdr_correction)
  if (print_table) {show(ft_results)}
  
  # Return
  if (return) {return(list(model = hc$model,
                           data = hc$data,
                           title = hc$title,
                           h = hc$h,
                           table_results = table_results,
                           ft_results = ft_results, 
                           cluster_scores = clstrs %>% select(
                             subject, condition, condition_tdcs, time, 
                             starts_with("cluster_")), 
                           cluster_items = cluster_items))}
}

test_clusters_with_covs <- function(hc, df, covs, conditions = 3, time_log = FALSE, 
                          threelevels = FALSE, print_table = TRUE, 
                          print_models = FALSE, clustering = NULL, 
                          return = TRUE, fdr_correction = FALSE) {
  fts_results <- list()
  if (time_log) {time = "time_log"} else { time = "time"}
  for (cov in covs) {
    # Create dataframe with columns item, cluster_num, cluster_desc  
    if (is.null(clustering)) {
      cluster_items <- hc$data %>% 
        transmute(cluster_num = cutree(hc$model, h = hc$h)) %>%
        rownames_to_column(var = "item") %>%
        mutate(cluster_desc = NA_character_)
    } else {
      cluster_items <- data.frame(
        item = row.names(hc$data),
        cluster_num = clustering,
        cluster_desc = NA_character_
      )
    }
    
    # Copy df
    clstrs <- df
    
    # Create sum scores 
    for (i in 1:max(cluster_items$cluster_num)) {
      current_cluster <- paste0("cluster_", i)
      
      # Get cluster items
      current_items <- cluster_items %>% 
        filter(cluster_num == i) %>%
        select(item) %>%
        pull()
      
      # Create cluster sum: arbitrarily set to mean and SD of 10
      clstrs <- clstrs %>%
        rowwise() %>%
        mutate({{current_cluster}} := sum(c_across(all_of(current_items)))) %>%
        ungroup() # %>%
      # 2-level model
      # Create lmer formula and correct row selection in lmer for later extraction
      if (conditions == 2) {
        formula <- as.formula(paste(current_cluster, "~ condition_tdcs *", time, 
                                    "*", cov, 
                                    "+ (1|subject)"))
        rows <- 8
      } else if (conditions == 3){
        formula <- as.formula(paste(current_cluster, "~ condition *", time, 
                                    "*", cov, 
                                    "+ (1|subject)"))
        rows <- 5:6
      }
      
      # Run lmer
      lmm <- lmer(formula, clstrs)
      if (print_models) {print(summary(lmm))}
      
      # Add a descriptive cluster column to cluster_items
      current_cluster_desc <- sapply(current_items, FUN = transform_item_names)[1:3]
      current_cluster_desc <- paste(word(current_cluster_desc, -2), collapse = ", ")
      current_cluster_desc <- paste0("Cluster ", i, " (", current_cluster_desc, ")")
      
      cluster_items <- cluster_items %>%
        mutate(cluster_desc = if_else(cluster_num == i, 
                                      {{current_cluster_desc}}, 
                                      cluster_desc))

      # Create tidy version of LMM
      lmm_tidy <- tidy(lmm)[rows, 3:8] 
      
      # Calculate effect size: Cohen's d (The new way)
      # Create emmeans trends for current object
      if (conditions == 2) {
        emt <- emtrends(lmm, ~condition_tdcs, var = time)
      } else {
        emt <- emtrends(lmm, ~condition, var = time)
      }
      if (time_log) {
        cohensd <- summary(eff_size(emt,
                                    sigma = sigma(lmm),
                                    edf = df.residual(lmm))) %>%
          mutate(d_complete = paste0(
            round(log(7)*effect.size, 2),
            " [",
            round(log(7)*lower.CL, 2),
            ", ",
            round(log(7)*upper.CL, 2),
            "]"
          ))
      } else {
        cohensd <- summary(eff_size(emt,
                                    sigma = sigma(lmm),
                                    edf = df.residual(lmm))) %>%
          mutate(d_complete = paste0(
            round(6*effect.size, 2),
            " [",
            round(6*lower.CL, 2),
            ", ",
            round(6*upper.CL, 2),
            "]"
          ))    
        
      }
      
      # Put results in a table
      if (conditions == 3) {
        if(current_cluster == "cluster_1"){
          table_results <- cbind(cluster_desc = current_cluster_desc, 
                                 lmm_tidy,
                                 "Cohen's d" = cohensd$d_complete[1:2]) %>%
            add_row(term = {{current_cluster_desc}}, .before = 1) 
        } else {
          table_results <- table_results %>%
            add_row(term = {{current_cluster_desc}}) %>%
            add_row(cluster_desc = current_cluster_desc, 
                    lmm_tidy,
                    "Cohen's d" = cohensd$d_complete[1:2])
        }
        # Else: There are only 2 conditions, everything goes on 1 line
      } else {
        if(current_cluster == "cluster_1"){
          table_results <- cbind(cluster_desc = current_cluster_desc, 
                                 term = current_cluster_desc,
                                 select(lmm_tidy, -term),
                                 "Cohen's d" = cohensd$d_complete)
        } else {
          table_results <- table_results %>%
            add_row(cluster_desc = current_cluster_desc, 
                    term = current_cluster_desc,
                    select(lmm_tidy, -term),
                    "Cohen's d" = cohensd$d_complete)
        }      
      }
      
    } 
    
    if (threelevels) {
      # Create long format of clusters
      clstrs_lg <- clstrs %>%
        select(subject, condition, condition_tdcs, time, time_log, starts_with("cluster_")) %>%
        pivot_longer(cols = starts_with("cluster_"),
                     names_to = "cluster_type",
                     values_to = "cluster_score",
                     names_transform = list(cluster_type = as.factor)
        )
      
      # 3-level model
      # Create lmer formula 
      if (conditions == 2) {
        formula.3lvl <- as.formula(
          paste("cluster_score ~ condition_tdcs * cluster_type *",
                time, 
                "+ (1|subject/cluster_type)"))
      } else if (conditions == 3){
        formula.3lvl <- as.formula(
          paste("cluster_score ~ condition * cluster_type *",
                time, 
                "+ (1|subject/cluster_type)"))
      }
      
      # Run model
      lmm.3lvl <- lmer(formula.3lvl, clstrs_lg, control = lmerControl(optimizer = "bobyqa"))
      if (print_models) {print(anova(lmm.3lvl))}
      
      # Create tidy version (used at the end of the function)
      lmm.3level.tidy <- tidy(anova(lmm.3lvl))[7, ]
      
      # emtrends
      if (conditions == 3) {
        emt.3lvl <- emtrends(lmm.3lvl, ~condition * cluster_type, var = "time")
        emt.tidy <- tidy(contrast(emt.3lvl, 
                                  method = list("tDCS & DI" =c(-1, 1, 0), 
                                                "tDCS only" = c(-1, 0, 1)), 
                                  by = "cluster_type", 
                                  adjust = "none"))
        
      } else if (conditions == 2) {
        emt.3lvl <- emtrends(lmm.3lvl, ~condition_tdcs * cluster_type, var = "time")
        emt.tidy <- tidy(contrast(emt.3lvl, 
                                  method = list("tDCS (combined)" =c(-1, 1)), 
                                  by = "cluster_type", 
                                  adjust = "none"))
      }
      # Add descriptive cluster name to emt.tidy
      emt.tidy$cluster_desc <- NA_character_
      
      for (i in 1:max(cluster_items$cluster_num)) {
        current_cluster_desc <- cluster_items %>%
          filter(cluster_num == i) %>%
          select(cluster_desc) %>%
          unique() %>%
          pull()
        emt.tidy <- emt.tidy %>%
          mutate(cluster_desc = if_else(str_detect(cluster_type, as.character(i)), 
                                        {{current_cluster_desc}}, 
                                        cluster_desc))
        
      }
      
      # Add the p-values from emt.tidy to table_results
      table_results <- table_results %>%
        mutate(
          # Give proper comparison names
          term = str_replace(term, 
                             "conditiontDCS & DI:time",
                             "tDCS & DI"),
          term = str_replace(term, 
                             "conditiontDCS only:time",
                             "tDCS only"),
          term = str_replace(term, 
                             "condition_tdcstDCS:time",
                             "tDCS (combined)"),      
          term = str_replace(term, 
                             "_log",
                             "")
        ) %>%
        left_join(select(emt.tidy, cluster_desc, term = contrast, p.3lvl = p.value),
                  by = c("cluster_desc", "term")) %>%
        select(-cluster_desc) %>%
        # add three-way interaction to top
        add_row(term = "Three-way interaction",
                statistic = lmm.3level.tidy$statistic,
                df = lmm.3level.tidy$DenDF,
                p.3lvl = lmm.3level.tidy$p.value, 
                .before = 1)
      
      # Without three-levels
    } else {
      table_results <- table_results %>%
        mutate(
          # Give proper comparison names
          term = str_replace(term, 
                             "conditiontDCS & DI:time",
                             "tDCS & DI"),
          term = str_replace(term, 
                             "conditiontDCS only:time",
                             "tDCS only"),
          term = str_replace(term, 
                             "condition_tdcstDCS:time",
                             "tDCS (combined)"),      
          term = str_replace(term, 
                             "_log",
                             "")
        ) %>%
        select(-cluster_desc)  
    }
    
    # FDR correction 
    if (fdr_correction) {
      table_results <- table_results %>%
        mutate(p.fdr = p.adjust(p.value, method = "fdr"))
    }
    
    # Create formatted results table
    ft_results <- format_table(table_results, threelevels, conditions, fdr_correction, cov)
    if (print_table) {show(ft_results)}
    fts_results[[cov]] <- ft_results
  }
  # Return
  if (return) {return(list(model = hc$model,
                           data = hc$data,
                           title = hc$title,
                           h = hc$h,
                           fts_results = fts_results,
                           cluster_scores = clstrs %>% select(
                             subject, condition, condition_tdcs, time,
                             starts_with("cluster_")),
                           cluster_items = cluster_items))}
}


format_table <- function(table, threelevels, conditions, fdr_correction, cov) {
  
  if (!missing(cov)) {
    header <- cov
  } else if (conditions == 3) {
    header <- "Comparison to double sham"
  } else {
    header <- "Active vs. sham tDCS"
  }
  
  if (threelevels) {
    table %>%
      # Round and format p
      mutate(
        across(estimate:df, ~ round(., 2)),
        across(p.value:p.3lvl, ~ round(., 3))
      ) %>%
      # Take out NAs by replacing them with ""
      replace_na(value = "") %>%
      rename(
        "{header}" := term,
        Estimate = estimate,
        SE = std.error,
        t = statistic,
        DF = df,
        "p (2 lvls)" = p.value,
        "p (3 lvls)" = p.3lvl
      ) %>%
      flextable() %>%
      italic(i = 1) %>%
      bold(i = ~`p (2 lvls)`  < 0.05, j = 6) %>%
      bold(i = ~`p (3 lvls)`  < 0.05, j = 7) %>%
      autofit()
  } else {
    if (fdr_correction) {
      table %>%
        # Round and format p
        mutate(
          across(estimate:df, ~ round(., 2)),
          p.value = round(p.value, 3),
          p.fdr = round(p.fdr, 3)
        ) %>%
        # Take out NAs by replacing them with ""
        replace_na(value = "") %>%
        rename(
          "{header}" := term,
          Estimate = estimate,
          SE = std.error,
          t = statistic,
          DF = df,
          P = p.value,
          `P(FDR)` = p.fdr
        ) %>%
        flextable() %>%
        bold(i = ~P<0.05, j = 6) %>%
        bold(i = ~`P(FDR)`<0.05, j = 7) %>%
        autofit()
    } else {
      table %>%
        # Round and format p
        mutate(
          across(estimate:df, ~ round(., 2)),
          p.value = round(p.value, 3)
        ) %>%
        # Take out NAs by replacing them with ""
        replace_na(value = "") %>%
        rename(
          "{header}" := term,
          Estimate = estimate,
          SE = std.error,
          t = statistic,
          DF = df,
          P = p.value
        ) %>%
        flextable() %>%
        bold(i = ~P<0.05, j = 6) %>%
        align(j = 7, align = "right", part = "all") %>%
        autofit()
    }
  }
}

theme_pub <- function() {
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
      axis.line = element_line(colour = "black")
    )
}

# Theme New England Journal of Medicine
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

plot_clusters <- function(hc, ci = 0.95){
  
  # Get z-value for confidence interval
  z <- qnorm((1-ci)/2, lower.tail = FALSE)
  
  # Create df with needed summary stats
  summaries <- hc$cluster_scores %>%
    group_by(time, condition_tdcs) %>%
    summarise(across(starts_with("cluster_"),
                     list(mean = mean, 
                          SEM = ~sd(.x, na.rm = TRUE)/sqrt(sum(!is.na(.x)))
                     ),
                     .names = "{col}_{fn}")
    )
  
  plots <- list()
  
  # Loop over clusters and create plots
  for (i in 1:max(hc$cluster_items$cluster_num)) {
    
    current_y <- hc$cluster_items %>%
      filter(cluster_num == i) %>%
      select(cluster_desc) %>%
      distinct() %>%
      pull()
    current_mean <- paste0("cluster_", i, "_mean")
    current_sem <- paste0("cluster_", i, "_SEM")
    
    plots[[i]] <- ggplot(summaries, aes(x = time,
                                        y = .data[[current_mean]],
                                        color = condition_tdcs,
                                        shape = condition_tdcs)) +
      
      geom_errorbar(aes(ymin = .data[[current_mean]] - {{z}}*.data[[current_sem]],
                        ymax = .data[[current_mean]] + {{z}}*.data[[current_sem]]),
                    width = 0.8,
                    linewidth = 0.75,
                    alpha = 0.8,
                    position = position_dodge(width = 0.15)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 3.2, position = position_dodge(width = 0.15)) +
      scale_x_continuous(breaks = c(0, 2, 3, 4, 6),
                         labels=c("Baseline", "2", "3", "4", "6")) +
      labs(y = current_y, x = "Week") +
      theme_nejm()
  }
  # Group the graphs
  do.call(ggarrange, c(plots, common.legend = TRUE))
  
  return(plots)
}

report_treecut <- function(data, hc, ...) {
  
  cutree_result <- cutreeDynamic(dendro = hc, ...) 
  print(paste(max(cutree_result), "clusters"))
  data$cluster_no <- cutree_result
  
  for (i in 1:max(cutree_result)) {
    
    print(paste0("Cluster ", i, ":"))
    current_items <- data %>%
      mutate(item = rownames(data)) %>%
      filter(cluster_no == {{i}}) %>%
      pull(item)
    
    print(current_items)
  }
}

fit_pagewidth <- function(ft, pgwidth = 6){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth / (flextable_dim(ft_out)$widths))
  return(ft_out)
}

# Additional data cleaning --------------

trials <- psylect %>% 
  rename(
    # HAM-D/HDRS
    "H01_Mood" = "hamd_1",
    "H02_Guilt" = "hamd_2",
    "H03_Suicide" = "hamd_3",
    "H04_Early_insomnia" = "hamd_4",
    "H05_Middle_insomnia" = "hamd_5",
    "H06_Late_insomnia" = "hamd_6",
    "H07_Work_Activities" = "hamd_7",
    "H08_Retardation" = "hamd_8",
    "H09_Agitation" = "hamd_9",
    "H10_Psychic_anxiety" = "hamd_10",
    "H11_Somatic_anxiety" = "hamd_11",
    "H12_Gastrointestinal" = "hamd_12",
    "H13_Somatic_symptoms" = "hamd_13",
    "H14_Gential_symptoms" = "hamd_14",
    "H15_Hypchondriasis" = "hamd_15",
    "H16_Weight_loss" = "hamd_16",
    "H17_Insight" = "hamd_17",
    
    # MADRS
    'M01_Apparent_sadness' = 'madrs_1',
    'M02_Reported_sadness' = 'madrs_2',
    'M03_Inner_tension' = 'madrs_3',
    'M04_Reduced_sleep' = 'madrs_4',
    'M05_Reduced_appetite' = 'madrs_5',
    'M06_Concentration_difficulties' = 'madrs_6',
    'M07_Motor_inhibition' = 'madrs_7',
    'M08_Inability_to_feel' = 'madrs_8',
    'M09_Pessimistic_thoughts' = 'madrs_9',
    'M10_Suicidal_thoughts' = 'madrs_10',
    
    # BDI
    "B01_Sadness" = "bdi_1",
    "B02_Pessimism" = "bdi_2",
    "B03_Past_failure" = "bdi_3",
    "B04_Loss_of_pleasure" = "bdi_4",
    "B05_Guilt" = "bdi_5",
    "B06_Punishment" = "bdi_6",
    "B07_Self_dislike" = "bdi_7",
    "B08_self_critical" = "bdi_8",
    "B09_Suicide" = "bdi_9",
    "B10_Crying" = "bdi_10",
    "B11_Agitation" = "bdi_11",
    "B12_Interest_loss" = "bdi_12",
    "B13_Indecisiveness" = "bdi_13",
    "B14_Worthlessness" = "bdi_14",
    "B15_Loss_of_energy" = "bdi_15",
    "B16_Sleep" = "bdi_16_calc",
    "B17_Irritability" = "bdi_17",
    "B18_Appetite" = "bdi_18_calc",
    "B19_Concentration" = "bdi_19",
    "B20_Tired_fatigue" = "bdi_20",
    "B21_Libido" = "bdi_21",
    
    # YMRS
    'Y01_Elevated_mood' = 'ymrs_1',
    'Y02_Increased_energy_&_motor_activity' = 'ymrs_2',
    'Y03_Sexual_interest' = 'ymrs_3',
    'Y04_Reduced_sleep' = 'ymrs_4',
    # double weighted:
    'Y05_Irritability_8' = 'ymrs_5',
    # double weighted:
    'Y06_Speech_rate_&_amount_8' = 'ymrs_6',
    'Y07_Language-thought_disorder' = 'ymrs_7',
    # double weighted:
    'Y08_Content_of_thoughts_8' = 'ymrs_8',
    # double weighted:
    'Y09_Disruptive-aggressive_behavior_8' = 'ymrs_9',
    'Y10_Appearance' = 'ymrs_10',
    'Y11_Insight' = 'ymrs_11'
  )


likerts <- names(select(trials, 
                        edin_1:edin_4, 
                        H01_Mood:H17_Insight, 
                        hama_1:hama_14,
                        M01_Apparent_sadness:M10_Suicidal_thoughts,
                        Y01_Elevated_mood:Y11_Insight,
                        panas_aflito:panas_vigoroso,
                        idatet_1:idatet_20, idatee_1:idatee_20,
                        B01_Sadness:B18_Appetite,
                        side_1:side_r_16))



trials_wide <- trials %>%
  filter(week == 0 | week == 2 | week == 3 | week == 4 | week == 6) %>%
  mutate(across(all_of(likerts), as.ordered)) %>%
  group_by(subject) %>%
  fill(reason_dropout, .direction = "downup") %>%
  ungroup() %>%
  select(-record_id_sc, -arm_belong, -redcap_event_name, -record_id2, -dob, 
         -starts_with("blind_"), -starts_with("followu"), -data_end_dropout,
         -final_study, -ends_with("_timestamp"), -panas_data,
         -starts_with("side_effect_discont_"),  
         -starts_with("vas_psylect"), -stai_s_positive, -stai_s_negative,
         -stai_t_positive, -stai_t_negative, -Grupos, -bdi_16, -bdi_18,
         -where(is.character), -contains("_bl"), -bdi_total_final
  ) %>%
  pivot_wider(names_from = week,
              values_from = c(H01_Mood:ymrs_total, panas_aflito:hamd_remission),
              names_sep = "_w"
  ) 

# Exclude variables that were introduced by the wide format but never 
# measured, e.g. HAMA was not measured at week 2 and 4. 
drops <- names(which(colSums(is.na(trials_wide)) == nrow(trials_wide)))
trials_wide <- select(trials_wide, !all_of(drops)) %>%
  # exclude all side effects: they correctly contain a lot NAs which would be 
  # imputed otherwise. Similarly
  select(-starts_with("side_g_"), -starts_with("side_r_"))


# Imputation with miss Forest --------------------------------------------------

# Select baseline data and transform to dataframe (missForest does not work with tibbles)
cols_bl <- names(select(trials_wide, !matches("_w[0-6]$")))
trials_temp <- as.data.frame(select(trials_wide, c(all_of(cols_bl), -subject, -reason_dropout)))

# Do the first imputation
mf <- missForest(trials_temp)

# Need to correct some imputations that will be outside of the range.
mins <- sapply(select(trials_wide, where(is.numeric)), min, na.rm = TRUE)
maxs <- sapply(select(trials_wide, where(is.numeric)), max, na.rm = TRUE)

for (week in c("_w0", "_w2", "_w3", "_w4", "_w6")) {
  trials_temp <- as.data.frame(cbind(mf$ximp, select(trials_wide, ends_with(week))))
  mf <- missForest(trials_temp)
  
  for (n in names(select(mf$ximp, where(is.numeric)))) {
    if (min(mf$ximp[n]) != mins[n]) {
      mf$ximp[n][mf$ximp[n] < mins[n]] <- mins[n]
    }
    if (max(mf$ximp[n]) != maxs[n]) {
      mf$ximp[n][mf$ximp[n] > maxs[n]] <- maxs[n]
    }
  }
  
  print(paste("Done:", ncol(mf$ximp), "columns"))
}

trials_wide_imp <- cbind(trials_wide$subject, trials_wide$reason_dropout, mf$ximp) %>%
  rename(subject = "trials_wide$subject",
         reason_dropout = "trials_wide$reason_dropout")

save(trials_wide_imp, file = "trials_wide_imp5.RData")

trials_imp <- trials_wide_imp %>%
  pivot_longer(cols = matches(".*_w[0-6]$"),
               names_pattern = "^(.*)_w(.*)$",
               names_to = c(".value", "week"),
               names_transform = list(week = as.integer)) %>%
  relocate(week, .after = "condition2")

# Further data cleaning after imputation -----------------------------------------------
trials_imp <- trials_imp %>%
  rename(H01_Sad_mood = H01_Mood,
         H02_Guilt_Delusions = H02_Guilt,
         H07_Loss_of_interest = H07_Work_Activities,
         H12_Loss_of_appetite = H12_Gastrointestinal,
         H13_Energy_Somatic = H13_Somatic_symptoms,
         H14_Reduced_libido = H14_Gential_symptoms,
         time = week, 
         condition_tdcs = condition2) %>%
  mutate(across(c(M01_Apparent_sadness:M10_Suicidal_thoughts, 
                  H01_Sad_mood:H17_Insight), 
                ~as.numeric(as.character(.x))),
         # Create normalized items according to theoretical max
         across(c(H01_Sad_mood:H03_Suicide,
                  H07_Loss_of_interest:H11_Somatic_anxiety,
                  H15_Hypchondriasis), 
                ~./4,
                .names = "{.col}.n1"),
         H16_Weight_loss.n1 = H16_Weight_loss/3,
         across(c(H04_Early_insomnia:H06_Late_insomnia,
                  H12_Loss_of_appetite:H14_Reduced_libido,
                  H17_Insight), 
                ~./2,
                .names = "{.col}.n1"),
         across(M01_Apparent_sadness:M10_Suicidal_thoughts,
                ~./6,
                .names = "{.col}.n1"),
         # Create normalized items according to measured max
         across(c(M01_Apparent_sadness:M10_Suicidal_thoughts, 
                  H01_Sad_mood:H17_Insight), 
                ~./max(.),
                .names = "{.col}.n2"),
         # Create standardized items
         across(c(M01_Apparent_sadness:M10_Suicidal_thoughts, 
                  H01_Sad_mood:H17_Insight), 
                ~scale(.)[, 1],
                .names = "{.col}.s"),
         time_log = log(time + 1),
         condition = fct_relevel(condition, "Double-placebo"),
         condition_tdcs = fct_relevel(condition_tdcs, "Double-placebo"))


# Zero counts ---------
zero_counts <- pslct_imp %>%
  summarize(across(c(
    H01_Sad_mood:H17_Insight),
    list(N = ~sum(.x == 0), 
         Prcnt = ~sum(.x == 0)/sum(!is.na(.x))), .names = "{.col}-{.fn}"),
  ) %>%
  pivot_longer(cols = everything(),
               names_sep = "-",
               names_to = c("Item", ".value")
  ) %>%
  arrange(desc(N)) 
# Most frequent zeroes:
# 1. H17_Insight
# 2. H16_Weight_loss
# 3. H12_Loss_of_appetite
# 4. H15_Hypochondriasis

# Hierarchical clustering of symptoms ------------

hdrs <- pslct_imp %>%
  select(
    H01_Sad_mood:H11_Somatic_anxiety,
    H13_Pains_and_fatigability:H15_Hypochondriasis) %>%
  rotate_df() 


hc.hdrs <- create_hc(hdrs, "euclidean", "ward.D2", 43, draw_threshold = FALSE)


## Dynamic treecut ------

report_treecut(
  hc.hdrs$data,
  hc.hdrs$model,
  distM = as.matrix(dist(hdrs, method = "euclidean")),
  method = "hybrid",
  minClusterSize = 1
)
# Group comparisons ----------------

# 2 group comparison 
hc.hdrs.2 <- test_clusters(hc = hc.hdrs, df = pslct_imp, conditions = 2,
                           time_log = T, threelevels = FALSE)

ft_results <- hc.hdrs.2$ft_results %>%
  compose(i = 1, j = 1, value = as_paragraph("Emotional cluster")) %>%
  compose(i = 2, j = 1, value = as_paragraph("Psychosomatic cluster")) %>%
  compose(i = 3, j = 1, value = as_paragraph("Sleep cluster")) %>%
  compose(i = 4, j = 1, value = as_paragraph("Psychomotor cluster")) %>%
  autofit()

ft_results

save_as_docx(
  fit_pagewidth(ft_results),
  path = "output/secondary analysis/Table_HDRSonly_TreatmentEffects.docx")

# Cluster score table ----------

tb_clusterscores <- hc.hdrs.2$cluster_scores %>%
  group_by(time, condition_tdcs) %>%
  summarize(
    across(cluster_1:cluster_4,
           ~paste(round(mean(.), 2), "±", round(sd(.), 2))
           )
  ) %>%
  rename(Week = time, Group = condition_tdcs, 
         "Emotional cluster" = cluster_1, "Psychosomatic cluster" = cluster_2,
         "Sleep cluster" = cluster_3, "Psychomotor cluster" = cluster_4)

ft_clusterscores <- tb_clusterscores %>%
  flextable() %>%
  fit_pagewidth()

ft_clusterscores

save_as_docx(
  ft_clusterscores,
  path = "output/secondary analysis/Table_Clusterscores_Nov2024.docx")

# Line plots -------------

cluster_plots <- plot_clusters(hc.hdrs.2, ci = 0.95)

# Clusters with significant effects
ggarrange(
  cluster_plots[[1]] + labs(y = "Emotional cluster", x = NULL) + coord_cartesian(ylim = c(0, NA)), 
  cluster_plots[[2]] + labs(y = "Psychosomatic cluster", x = NULL) + coord_cartesian(ylim = c(0, NA)), 
  cluster_plots[[3]] + labs(y = "Sleep cluster") + coord_cartesian(ylim = c(0, NA)), 
  cluster_plots[[4]] + labs(y = "Psychomotor cluster") + coord_cartesian(ylim = c(0, NA)),
  common.legend = T,
  nrow = 2,
  ncol = 2
)

ggsave("output/secondary analysis/LinePlots.pdf")



# k-means ----------------------
# confirmatory analysis

kmeans(hdrs, 4)

ft_km <- kmeans(hdrs, 4)$cluster %>%
  as.data.frame() %>%
  rename(Cluster = ".") %>%
  rownames_to_column(var = "Item") %>%
  arrange(Cluster) %>%
  mutate(Item = sapply(Item, transform_item_names)) %>%
  flextable() %>% 
  autofit()

ft_km
 save_as_docx(ft_km, path = "output/secondary analysis/Table_Kmeans.docx")



