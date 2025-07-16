library(igraph)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(purrr)
library(ggplot2)
library(tidyr)
library(dplyr)



# plot a reduced representation of the binary tree
depth <- 5

n_nodes <- 2^depth - 1
node_names <- paste0("n", 0:(n_nodes - 1))

edges <- c()
for (i in 0:((n_nodes - 1) %/% 2)) {
  left_child  <- 2 * i + 1
  right_child <- 2 * i + 2
  if (left_child < n_nodes) {
    edges <- c(edges, paste0("n", i), paste0("n", left_child))
  }
  if (right_child < n_nodes) {
    edges <- c(edges, paste0("n", i), paste0("n", right_child))
  }
}

g <- graph(edges, directed = TRUE)

plot(g,
     layout = layout_as_tree(g, root = "n0"),
     vertex.size = 13,
     vertex.label.cex = 0.6,
     vertex.color = "lightblue",
     edge.arrow.size = 0,
     main = "")

# Load the returns simulated on python
simulated_returns <- read_excel("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/Project/simulated_returns.xlsx", sheet = "df")
summary(simulated_returns)

simulated_returns_long <- melt(simulated_returns, id.vars = c("stage", "node"), variable.name = "asset", value.name = "return")

# boxplot simulated returns
ggplot(simulated_returns_long, aes(x = as.factor(stage), y = return, fill = asset)) +
  geom_boxplot() +
  labs(title = "Boxplot simulated returns for asset and stage", x = "Stage", y = "Return")+
  theme_minimal()

# Scatterplot simulated returns vs stage
ggplot(simulated_returns_long, aes(x = as.factor(stage), y = return, color=asset)) +
  geom_point() +
  labs(title = "", x = "Stage", y = "Return") +
  facet_wrap(~asset)+
  theme_minimal()

# returns distribution
ggplot(simulated_returns_long, aes(x = return, fill = asset)) +
  geom_histogram(aes(y = ..density..), alpha =1, bins = 15, position = "identity",color="black") +
  geom_density(alpha = 0, color = "black") +
  facet_wrap(~asset, scales = "free") +
  labs(title = "Returns distribution for each asset", y = "Density") +
  scale_fill_manual(values = c(
    "money"  = "#f1eef6",
    "bond"   = "#bdc9e1",
    "stock2" = "#74a9cf",
    "stock1" = "#0570b0"
  ), name = "Asset class") +
  theme_minimal()

ggplot(simulated_returns_long, aes(return, fill=asset))+
  geom_density(alpha=0.6)+
  theme_minimal()

# Plot the simulated returns in the semplified tree
assets <- c("stock1", "stock2", "bond", "money")

tree_layout <- layout_as_tree(g, root = "n0")

all_returns <- unlist(simulated_returns[assets])
global_min <- min(all_returns, na.rm = TRUE)
global_max <- max(all_returns, na.rm = TRUE)
global_abs_max <- max(abs(global_min), abs(global_max))  # per centrare su 0

color_palette <- colorRampPalette(c("#D73027", "#FEE08B", "#1A9850"))

for (asset in assets) {
  node_returns <- simulated_returns[[asset]][match(V(g)$name, simulated_returns$node)]
  V(g)$label <- round(node_returns, 3)

  norm_returns <- (node_returns + global_abs_max) / (2 * global_abs_max)

  color_index <- as.numeric(cut(norm_returns, breaks = 100, include.lowest = TRUE))
  vertex_colors <- color_palette(100)[color_index]

  plot(g,
       layout = tree_layout,
       vertex.size = 13,
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       vertex.color = vertex_colors,
       edge.arrow.size = 0)
}


dati_single_stage=read_csv("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/AMPL_files/project/Single_stage_Markovitz/results_single_stage.csv")

# order the assets
ordine_asset <- c("mm", "bonds", "stock2", "stock1")

dati_single_stage_long <- dati_single_stage %>%
  pivot_longer(cols = c(stock1, stock2, bonds, mm),
               names_to = "Asset",
               values_to = "Proporzione") %>%
  mutate(Asset = factor(Asset, levels = ordine_asset))

ggplot(dati_single_stage_long, aes(x = targetR, y = Proporzione, fill = Asset)) +
  geom_area(alpha = 0.9, size = 0.5, colour = "white") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0.03, 0.09))+
  scale_fill_manual(values = c(
    "mm" = "#f1eef6",
    "bonds" = "#bdc9e1",
    "stock2" = "#74a9cf",
    "stock1" = "#0570b0"
  )) +
  labs(
    #title = "Portfolio Composition as Target Return Increases",
    x = "Target Return",
    y = "Portfolio Proportion"
  ) +
  theme_minimal() +
  geom_area(position = "fill")
theme(legend.position = "bottom")


ggplot(dati_single_stage_long, aes(x = variance, y = Proporzione, fill = Asset)) +
  geom_area(alpha = 0.9, size = 0.5, colour = "white") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "mm" = "#f1eef6",
    "bonds" = "#bdc9e1",
    "stock2" = "#74a9cf",
    "stock1" = "#0570b0"
  )) +
  labs(
    #title = "Portfolio Composition as Portfolio Variance Increases",
    x = "Portfolio Variance",
    y = "Portfolio Proportion"
  ) +
  theme_minimal() +
  geom_area(position = "fill")
theme(legend.position = "bottom")

ggplot(dati_single_stage_long, aes(x = variance, y = targetR)) +
  geom_point(shape = 16, color = "dodgerblue3", size = 2) +
  geom_line(color = "coral3", size = 1) +
  labs(
    #title = "Efficient Frontier: Target Return vs Portfolio Variance",
    x = "Portfolio Variance",
    y = "Target Return")+
  theme_minimal(
  ) 


# ortfolio analysis function 
analyze_portfolios <- function(data, final_stage = 9, alpha = 0.95) {
  
  # Filter data up to final_stage
  data <- data %>% filter(Stage <= final_stage)
  
  # Data at final stage for final wealth distribution
  df_stage <- data %>% filter(Stage == final_stage)
  
  # 1. Wealth distribution over stages
  p0 <- ggplot(data, aes(x = as.factor(Stage), y = Wealth)) +
    geom_point(color = "steelblue") +
    labs(title = "Wealth distribution by stage", x = "Stage", y = "Wealth") +
    theme_minimal(base_size = 13)
  
  # 2. Final wealth distribution density
  p1 <- ggplot(df_stage, aes(x = Wealth)) +
    geom_density(fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = paste("Final wealth distribution (Stage", final_stage, ")"), x = "Wealth", y = "Density") +
    theme_minimal(base_size = 14)
  
  # 3. Portfolio composition at final stage
  df_long <- data %>%
    pivot_longer(cols = stock1:money, names_to = "Asset", values_to = "Allocation") %>%
    mutate(Proportion = Allocation / Wealth)
  
  df_long_stage <- df_long %>%
    filter(Stage == final_stage) %>%
    mutate(Asset = factor(Asset, levels = c("money", "bond", "stock2", "stock1")))
  
  p2 <- ggplot(df_long_stage, aes(x = Wealth, y = Proportion, fill = Asset)) +
    geom_area(position = "stack", alpha = 0.85) +
    scale_fill_manual(values = c(
      "money"  = "#f1eef6",
      "bond"   = "#bdc9e1",
      "stock2" = "#74a9cf",
      "stock1" = "#0570b0"
    ), name = "Asset class") +
    labs(
      title = paste("Portfolio Composition at Stage", final_stage),
      subtitle = "Proportional allocation by wealth level",
      x = "Wealth",
      y = "Portfolio Proportion"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right", plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 10))
  
  # 4. Average asset allocation evolution over time
  df_media <- df_long %>%
    group_by(Stage, Asset) %>%
    summarise(MeanProportion = weighted.mean(Proportion, Probability), .groups = "drop") %>%
    mutate(Asset = factor(Asset, levels = c("money", "bond", "stock2", "stock1")))
  
  p3 <- ggplot(df_media, aes(x = Stage, y = MeanProportion, fill = Asset)) +
    geom_col(alpha = 0.8, color = "black", size = 0.3, position = "stack") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = unique(df_media$Stage)) +
    scale_fill_manual(values = c(
      "money"  = "#f1eef6",
      "bond"   = "#bdc9e1",
      "stock2" = "#74a9cf",
      "stock1" = "#0570b0"
    )) +
    labs(title = "Average portfolio composition over time", x = "Stage (Time)", y = "Average proportion") +
    theme_minimal()
  
  # Return all plots as a list
  return(list(
    wealth_by_stage = p0,
    final_density = p1,
    composition = p2,
    allocation_evolution = p3
  ))
}


dati_cara_gamma <- read.csv("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/AMPL_files/project/CARA/cara_gamma_all.csv")

# stage = 10
dati_finale_cara <- dati_cara_gamma %>% filter(Stage == 10)
dati_finale_cara$Gamma <- as.factor(dati_finale$Gamma)

dati_finale_cara$Gamma <- as.factor(dati_finale_cara$Gamma)

# Plot 
ggplot(dati_finale_cara, aes(x = Gamma, y = Wealth, fill=Gamma)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Final Wealth by Gamma",
       x = "Gamma",
       y = "Wealth at Stage 10") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(dati_finale_cara, aes(x = Wealth, color = Gamma, fill = Gamma)) +
  geom_density(alpha = 0.3) +   
  labs(
    title = "Density Plot of Final Wealth by Gamma (Stage 10)",
    x = "Wealth",
    y = "Density",
    color = "Gamma",
    fill = "Gamma"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# summary stats for each gamma
summary_stats_cara <- dati_finale_cara %>%
  group_by(Gamma) %>%
  summarise(
    Min = min(Wealth, na.rm = TRUE),
    Q1 = quantile(Wealth, 0.25, na.rm = TRUE),
    Median = median(Wealth, na.rm = TRUE),
    Mean = mean(Wealth, na.rm = TRUE),
    Q3 = quantile(Wealth, 0.75, na.rm = TRUE),
    Max = max(Wealth, na.rm = TRUE),
    Variance = var(Wealth, na.rm = TRUE),
    SD = sd(Wealth, na.rm = TRUE)
  )

print(summary_stats_cara)


# We choose gamma=2
dati_cara=dati_cara_gamma%>%filter(Gamma==2)
analyze_portfolios(dati_cara)



dati_crra_gamma <- read.csv("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/AMPL_files/project/CRRA/crra_gamma_all.csv")

# filter for stage = 10
dati_crra_finale <- dati_crra_gamma %>% filter(Stage == 10)
dati_crra_finale$Gamma <- as.factor(dati_crra_finale$Gamma)

dati_crra_finale$Gamma <- as.factor(dati_crra_finale$Gamma)

# Plot 
ggplot(dati_crra_finale, aes(x = Gamma, y = Wealth, fill=Gamma)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Final Wealth by Gamma",
       x = "Gamma",
       y = "Wealth at Stage 10") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(dati_crra_finale, aes(x = Wealth, color = Gamma, fill = Gamma)) +
  geom_density(alpha = 0.3) +   
  labs(
    title = "Density Plot of Final Wealth by Gamma (Stage 10)",
    x = "Wealth",
    y = "Density",
    color = "Gamma",
    fill = "Gamma"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Summary statistics for each gamma
summary_stats_crra <- dati_crra_finale %>%
  group_by(Gamma) %>%
  summarise(
    Min = min(Wealth, na.rm = TRUE),
    Q1 = quantile(Wealth, 0.25, na.rm = TRUE),
    Median = median(Wealth, na.rm = TRUE),
    Mean = mean(Wealth, na.rm = TRUE),
    Q3 = quantile(Wealth, 0.75, na.rm = TRUE),
    Max = max(Wealth, na.rm = TRUE),
    Variance = var(Wealth, na.rm = TRUE),
    SD = sd(Wealth, na.rm = TRUE)
  )

print(summary_stats_crra)


# We choose gamma=2
dati_crra=dati_crra_gamma%>%filter(Gamma==2)
analyze_portfolios(dati_crra)







# --- Data loading ---
dati_incr_gamma <- read.csv("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/AMPL_files/project/incr_gamma/incr_gamma.csv")
dati_decr_gamma <- read.csv("/Users/lorenzograssi/Desktop/Financial & Insurance Risk Modeling/AMPL_files/project/decr_gamma/decr_gamma.csv")

analyze_portfolios(dati_cara)

# Helper function to add Type column
add_type <- function(df, type) {
  df %>% mutate(Type = type)
}

# Add Type column to each dataset
dati_cara <- add_type(dati_cara, "CARA")
dati_crra <- add_type(dati_crra, "CRRA")
dati_incr_gamma <- add_type(dati_incr_gamma, "Increasing gamma")
dati_decr_gamma <- add_type(dati_decr_gamma, "Decreasing gamma")

# Combine data for stage 10
dati_all_10 <- bind_rows(
  dati_cara %>% filter(Stage == 10),
  dati_crra %>% filter(Stage == 10),
  dati_incr_gamma %>% filter(Stage == 10),
  dati_decr_gamma %>% filter(Stage == 10)
)


# Density plot Stage 10 
ggplot(dati_all_10, aes(x = Wealth, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1.1) +
  labs(title = "Final Wealth Density (Stage 10)", x = "Wealth", y = "Density") +
  scale_color_manual(values = c("CARA" = "#0570b0", "CRRA" = "orange", "Increasing gamma" = "#31a354", "Decreasing gamma" = "tomato")) +
  scale_fill_manual(values = c("CARA" = "#0570b0", "CRRA" = "orange", "Increasing gamma" = "#31a354", "Decreasing gamma" = "tomato")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Boxplot Stage 10 
ggplot(dati_all_10, aes(x = Type, y = Wealth, color = Type, fill = Type)) +
  geom_boxplot(alpha = 0.3, size = 1.1) +
  labs(title = "Final Wealth Boxplot (Stage 10)", x = "Utility Function", y = "Wealth") +
  scale_color_manual(values = c("CARA" = "#0570b0", "CRRA" = "orange", "Increasing gamma" = "#31a354", "Decreasing gamma" = "tomato")) +
  scale_fill_manual(values = c("CARA" = "#0570b0", "CRRA" = "orange", "Increasing gamma" = "#31a354", "Decreasing gamma" = "tomato")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Summary and variance 
summary_list <- list(
  CARA = summary(dati_cara %>% filter(Stage == 10) %>% pull(Wealth)),
  CRRA = summary(dati_crra %>% filter(Stage == 10) %>% pull(Wealth)),
  Increasing_gamma = summary(dati_incr_gamma %>% filter(Stage == 10) %>% pull(Wealth)),
  Decreasing_gamma = summary(dati_decr_gamma %>% filter(Stage == 10) %>% pull(Wealth))
)

sd_list <- list(
  CARA = sd(dati_cara %>% filter(Stage == 10) %>% pull(Wealth)),
  CRRA = sd(dati_crra %>% filter(Stage == 10) %>% pull(Wealth)),
  Increasing_gamma = sd(dati_incr_gamma %>% filter(Stage == 10) %>% pull(Wealth)),
  Decreasing_gamma = sd(dati_decr_gamma %>% filter(Stage == 10) %>% pull(Wealth))
)

print(summary_list)
print(sd_list)

mean_vec <- sapply(summary_list, function(x) as.numeric(x["Mean"]))
sd_vec <- unlist(sd_list)

df_mean_sd <- data.frame(
  Utility = names(mean_vec),
  Mean = mean_vec,
  sd = sd_vec)

print(df_mean_sd)

# Plot scatter mean vs SD
ggplot(df_mean_sd, aes(x = sd, y = Mean, label = Utility, color = Utility)) +
  geom_point(size = 4) +
  scale_color_manual(values = c(
    "CARA" = "#0570b0",
    "CRRA" = "orange",
    "Increasing_gamma" = "#31a354",
    "Decreasing_gamma" = "tomato"
  )) +
  labs(
    title = "Mean vs SD of Final Wealth by Utility Function",
    x = "Standard Deviation",
    y = "Mean Wealth",
    color = "Utility Function"
  ) +
  theme_minimal(base_size = 14)

#  Overall plot 
dati_all <- bind_rows(dati_cara, dati_crra, dati_incr_gamma, dati_decr_gamma)

ggplot(dati_all, aes(x = as.factor(Stage), y = Wealth, color = Type)) +
  scale_color_manual(values = c("CARA" = "#0570b0", "CRRA" = "orange", "Increasing gamma" = "#31a354", "Decreasing gamma" = "tomato")) +
  geom_point(alpha = 0.7) +
  facet_wrap(~Type) +
  theme_bw() +
  theme(legend.position = "none")


# combine the datasets
gamma_data <- bind_rows(
  dati_crra %>% select(Stage, Gamma) %>% mutate(Type = "CRRA"),
  dati_incr_gamma %>% select(Stage, Gamma) %>% mutate(Type = "Increasing gamma"),
  dati_decr_gamma %>% select(Stage, Gamma) %>% mutate(Type = "Decreasing gamma")
)

# Compute gamma for each stage 
gamma_mean_by_stage <- gamma_data %>%
  group_by(Stage, Type) %>%
  summarise(MeanGamma = mean(Gamma), .groups = "drop")

# plot 
ggplot(gamma_mean_by_stage, aes(x = Stage, y = MeanGamma, color = Type)) +
  geom_line(alpha = 1, size = 1.2) +
  labs(
    title = "Evolution of Gamma over Time",
    x = "Stage",
    y = "Gamma",
    color = ""  # titolo della legenda
  ) +
  scale_color_manual(
    values = c(
      "CRRA" = "orange",
      "Increasing gamma" = "#31a354",
      "Decreasing gamma" = "tomato"
    ),
    labels = c(
      "CRRA" = "Constant gamma",
      "Increasing gamma" = "Increasing gamma",
      "Decreasing gamma" = "Decreasing gamma"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

