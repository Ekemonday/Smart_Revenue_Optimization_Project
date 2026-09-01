# zone 2 

# =============================================================================
#  SROP DATA ANALYSIS — Zone-Wide Aggregated Analysis
#  Includes: Compliance Rates, Digital Readiness Scores,
#            Gap Summary, Inter-State Comparisons
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
library(flextable)
library(officer)
library(purrr)

# fp_border() used for flextable borders — comes from the officer package
fp_border <- officer::fp_border

# ── Working Directory & Data ──────────────────────────────────────────────────
setwd("C:/Users/user/Desktop/DATAFRONTEIRA/SROP Project/SROP DATA ANALYSIS")

srop <- read_excel("SROP_DATA_CLEANED.xlsx") %>%
  mutate(state = trimws(state))

# ── Nigeria Geopolitical Zone Mapping ────────────────────────────────────────
# Hardcoded — no zone column needed in the dataset
nigeria_zones <- list(
  "North Central" = c("Benue", "Kogi", "Kwara", "Nasarawa",
                      "Niger", "Plateau", "FCT", "Abuja",
                      "Federal Capital Territory"),
  "North East"    = c("Adamawa", "Bauchi", "Borno",
                      "Gombe", "Taraba", "Yobe"),
  "North West"    = c("Jigawa", "Kaduna", "Kano",
                      "Katsina", "Kebbi", "Sokoto", "Zamfara"),
  "South East"    = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
  "South South"   = c("Akwa Ibom", "Bayelsa", "Cross River",
                      "Delta", "Edo", "Rivers"),
  "South West"    = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")
)

# Build a lookup table: state -> zone
zone_lookup <- stack(nigeria_zones) %>%
  rename(state = values, zone = ind) %>%
  mutate(state = as.character(state),
         zone  = as.character(zone))

# Join zone onto the dataset based on state name
srop <- srop %>%
  left_join(zone_lookup, by = "state")

# Report any states in the data that did not match the zone mapping
unmatched <- srop %>%
  filter(is.na(zone)) %>%
  distinct(state) %>%
  pull(state)

if (length(unmatched) > 0) {
  cat("\nWARNING - these states were NOT matched to any zone:\n")
  print(unmatched)
  cat("Check spelling in your dataset against the zone mapping above.\n")
} else {
  cat("\nAll states matched to zones successfully.\n")
}

# ── Get all unique zones present in the data ─────────────────────────────────
all_zones <- srop %>%
  filter(!is.na(zone)) %>%
  pull(zone) %>%
  unique() %>%
  sort()

cat("Zones found:\n"); print(all_zones)


# =============================================================================
#  SHARED HELPERS
# =============================================================================

# Colour palette used consistently across all zone charts
ZONE_COLOURS <- c(
  "North Central" = "#1f77b4",
  "North East"    = "#ff7f0e",
  "North West"    = "#2ca02c",
  "South East"    = "#d62728",
  "South South"   = "#9467bd",
  "South West"    = "#8c564b"
)

# Safe percentage helper — avoids NaN when denominator is 0
safe_pct <- function(x) ifelse(is.na(x) | is.nan(x), 0, round(x * 100, 1))

# Standard bold theme applied to every chart
bold_theme <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 1),
      plot.subtitle    = element_text(size = base - 1, color = "gray40"),
      axis.text        = element_text(color = "black", face = "bold"),
      axis.title       = element_text(face = "bold"),
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Save a ggplot into a folder, return the full path
save_plot <- function(plot, folder, filename,
                      width = 8, height = 5) {
  path <- file.path(folder, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  cat("  Saved:", path, "\n")
  path
}


# =============================================================================
#  METRIC COMPUTATION  (zone-level & state-level)
# =============================================================================

# ── 1. Aggregate compliance & awareness rates per zone ────────────────────────
compute_zone_metrics <- function(data) {
  data %>%
    group_by(zone) %>%
    summarise(
      n                  = n(),
      
      # Tax
      tax_aware_pct      = safe_pct(mean(awaretax  == "Yes", na.rm = TRUE)),
      tax_comply_pct     = safe_pct(mean(pay_tax   == "Yes", na.rm = TRUE)),
      tin_pct            = safe_pct(mean(tin        == "Yes", na.rm = TRUE)),
      
      # Fee
      fee_aware_pct      = safe_pct(mean(awarefee  == "Yes", na.rm = TRUE)),
      fee_comply_pct     = safe_pct(mean(pay_fee   == "Yes", na.rm = TRUE)),
      
      # Digital readiness — proportion who used Digital across all payment cols
      digital_pct        = safe_pct(mean(c(
        edu_fees, utility_fees, land_fees, health_fees,
        business_fees, transport_fees, env_fees, jud_fees,
        agric_fees, lic_fee, other_fee,
        fine_pen, licence, earn_sale, rent, int_rep
      ) == "Digital", na.rm = TRUE)),
      
      # Satisfaction (Very Satisfied + Satisfied = positive)
      tax_sati_pct       = safe_pct(mean(sati      %in% c("Very Satisfied", "Satisfied"), na.rm = TRUE)),
      fee_sati_pct       = safe_pct(mean(fee_sati  %in% c("Very Satisfied", "Satisfied"), na.rm = TRUE)),
      nont_sati_pct      = safe_pct(mean(nont_sat  %in% c("Very Satisfied", "Satisfied"), na.rm = TRUE)),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Overall compliance rate = average of tax & fee compliance
      compliance_rate    = round((tax_comply_pct + fee_comply_pct) / 2, 1),
      
      # Readiness score = average of digital usage & TIN possession
      readiness_score    = round((digital_pct + tin_pct) / 2, 1),
      
      # Gap = awareness minus compliance (how many aware but still not complying)
      tax_gap            = round(tax_aware_pct  - tax_comply_pct,  1),
      fee_gap            = round(fee_aware_pct  - fee_comply_pct,  1),
      overall_gap        = round((tax_gap + fee_gap) / 2, 1)
    )
}

# ── 2. State-level metrics for inter-state comparison within a zone ───────────
compute_state_metrics <- function(data, zone_name) {
  data %>%
    filter(zone == zone_name) %>%
    group_by(zone, state) %>%                          # group by BOTH zone & state
    summarise(
      n               = n(),
      tax_aware_pct   = safe_pct(mean(awaretax == "Yes", na.rm = TRUE)),
      tax_comply_pct  = safe_pct(mean(pay_tax  == "Yes", na.rm = TRUE)),
      tin_pct         = safe_pct(mean(tin       == "Yes", na.rm = TRUE)),
      fee_aware_pct   = safe_pct(mean(awarefee == "Yes", na.rm = TRUE)),
      fee_comply_pct  = safe_pct(mean(pay_fee  == "Yes", na.rm = TRUE)),
      digital_pct     = safe_pct(mean(c(
        edu_fees, utility_fees, land_fees, health_fees,
        business_fees, transport_fees, env_fees, jud_fees,
        agric_fees, lic_fee, other_fee,
        fine_pen, licence, earn_sale, rent, int_rep
      ) == "Digital", na.rm = TRUE)),
      tax_sati_pct    = safe_pct(mean(sati     %in% c("Very Satisfied","Satisfied"), na.rm = TRUE)),
      fee_sati_pct    = safe_pct(mean(fee_sati %in% c("Very Satisfied","Satisfied"), na.rm = TRUE)),
      nont_sati_pct   = safe_pct(mean(nont_sat %in% c("Very Satisfied","Satisfied"), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      compliance_rate = round((tax_comply_pct + fee_comply_pct) / 2, 1),
      readiness_score = round((digital_pct + tin_pct) / 2, 1),
      tax_gap         = round(tax_aware_pct - tax_comply_pct, 1),
      fee_gap         = round(fee_aware_pct - fee_comply_pct, 1),
      overall_gap     = round((tax_gap + fee_gap) / 2, 1)
    )
}


# =============================================================================
#  CHART FUNCTIONS
# =============================================================================

# ── Chart 1: Zone-Wide Compliance Rate (all zones side by side) ───────────────
chart_zone_compliance <- function(zone_metrics) {
  plot_data <- zone_metrics %>%
    select(zone, tax_comply_pct, fee_comply_pct, compliance_rate) %>%
    pivot_longer(cols = c(tax_comply_pct, fee_comply_pct, compliance_rate),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           tax_comply_pct  = "Tax Compliance",
                           fee_comply_pct  = "Fee Compliance",
                           compliance_rate = "Overall Compliance"
    ))
  
  ggplot(plot_data, aes(x = reorder(zone, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Compliance"     = "#1f77b4",
      "Fee Compliance"     = "#ff7f0e",
      "Overall Compliance" = "#2ca02c"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = "Zone-Wide Compliance Rates",
         subtitle = "Tax compliance, fee compliance and overall compliance by zone",
         x = NULL, y = "Rate (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 2: Zone-Wide Digital Readiness Score ────────────────────────────────
chart_zone_readiness <- function(zone_metrics) {
  plot_data <- zone_metrics %>%
    select(zone, digital_pct, tin_pct, readiness_score) %>%
    pivot_longer(cols = c(digital_pct, tin_pct, readiness_score),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           digital_pct     = "Digital Payment Usage",
                           tin_pct         = "TIN Possession",
                           readiness_score = "Readiness Score (Avg)"
    ))
  
  ggplot(plot_data, aes(x = reorder(zone, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Digital Payment Usage"  = "#9467bd",
      "TIN Possession"         = "#8c564b",
      "Readiness Score (Avg)"  = "#17becf"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = "Zone-Wide Digital Readiness Scores",
         subtitle = "Digital payment usage, TIN possession, and composite readiness score",
         x = NULL, y = "Score (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 3: Awareness vs Compliance Gap Summary ─────────────────────────────
chart_zone_gap <- function(zone_metrics) {
  plot_data <- zone_metrics %>%
    select(zone, tax_gap, fee_gap, overall_gap) %>%
    pivot_longer(cols = c(tax_gap, fee_gap, overall_gap),
                 names_to = "Gap_Type", values_to = "Gap") %>%
    mutate(Gap_Type = recode(Gap_Type,
                             tax_gap     = "Tax Gap",
                             fee_gap     = "Fee Gap",
                             overall_gap = "Overall Gap"
    ))
  
  ggplot(plot_data, aes(x = reorder(zone, -Gap), y = Gap, fill = Gap_Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Gap, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Gap"     = "#d62728",
      "Fee Gap"     = "#ff7f0e",
      "Overall Gap" = "#7f7f7f"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = "Awareness–Compliance Gap by Zone",
         subtitle = "Gap = % aware minus % compliant; higher = more non-compliant despite awareness",
         x = NULL, y = "Gap (percentage points)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 4: Satisfaction Rates Across Zones ─────────────────────────────────
chart_zone_satisfaction <- function(zone_metrics) {
  plot_data <- zone_metrics %>%
    select(zone, tax_sati_pct, fee_sati_pct, nont_sati_pct) %>%
    pivot_longer(cols = c(tax_sati_pct, fee_sati_pct, nont_sati_pct),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           tax_sati_pct  = "Tax Satisfaction",
                           fee_sati_pct  = "Fee Satisfaction",
                           nont_sati_pct = "Non-Tax Satisfaction"
    ))
  
  ggplot(plot_data, aes(x = reorder(zone, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Satisfaction"      = "#1f77b4",
      "Fee Satisfaction"      = "#2ca02c",
      "Non-Tax Satisfaction"  = "#9467bd"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = "Satisfaction Rates by Zone",
         subtitle = "Proportion of respondents who are Satisfied or Very Satisfied",
         x = NULL, y = "Satisfaction (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 5: Inter-State Compliance Comparison (within one zone) ──────────────
chart_state_compliance <- function(state_metrics, zone_name) {
  plot_data <- state_metrics %>%
    select(state, tax_comply_pct, fee_comply_pct, compliance_rate) %>%
    pivot_longer(cols = c(tax_comply_pct, fee_comply_pct, compliance_rate),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           tax_comply_pct  = "Tax Compliance",
                           fee_comply_pct  = "Fee Compliance",
                           compliance_rate = "Overall Compliance"
    ))
  
  ggplot(plot_data, aes(x = reorder(state, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Compliance"     = "#1f77b4",
      "Fee Compliance"     = "#ff7f0e",
      "Overall Compliance" = "#2ca02c"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = paste0(zone_name, " — Inter-State Compliance Comparison"),
         subtitle = "Tax, fee and overall compliance rates by state",
         x = NULL, y = "Rate (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 6: Inter-State Readiness Comparison (within one zone) ───────────────
chart_state_readiness <- function(state_metrics, zone_name) {
  plot_data <- state_metrics %>%
    select(state, digital_pct, tin_pct, readiness_score) %>%
    pivot_longer(cols = c(digital_pct, tin_pct, readiness_score),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           digital_pct     = "Digital Payment Usage",
                           tin_pct         = "TIN Possession",
                           readiness_score = "Readiness Score (Avg)"
    ))
  
  ggplot(plot_data, aes(x = reorder(state, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Digital Payment Usage"  = "#9467bd",
      "TIN Possession"         = "#8c564b",
      "Readiness Score (Avg)"  = "#17becf"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = paste0(zone_name, " — Inter-State Digital Readiness"),
         subtitle = "Digital payment usage, TIN possession and composite readiness score by state",
         x = NULL, y = "Score (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 7: Inter-State Gap Comparison (within one zone) ─────────────────────
chart_state_gap <- function(state_metrics, zone_name) {
  plot_data <- state_metrics %>%
    select(state, tax_gap, fee_gap, overall_gap) %>%
    pivot_longer(cols = c(tax_gap, fee_gap, overall_gap),
                 names_to = "Gap_Type", values_to = "Gap") %>%
    mutate(Gap_Type = recode(Gap_Type,
                             tax_gap     = "Tax Gap",
                             fee_gap     = "Fee Gap",
                             overall_gap = "Overall Gap"
    ))
  
  ggplot(plot_data, aes(x = reorder(state, -Gap), y = Gap, fill = Gap_Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Gap, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Gap"     = "#d62728",
      "Fee Gap"     = "#ff7f0e",
      "Overall Gap" = "#7f7f7f"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = paste0(zone_name, " — Inter-State Awareness–Compliance Gap"),
         subtitle = "Gap = % aware minus % compliant by state",
         x = NULL, y = "Gap (percentage points)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}

# ── Chart 8: Inter-State Satisfaction Comparison (within one zone) ────────────
chart_state_satisfaction <- function(state_metrics, zone_name) {
  plot_data <- state_metrics %>%
    select(state, tax_sati_pct, fee_sati_pct, nont_sati_pct) %>%
    pivot_longer(cols = c(tax_sati_pct, fee_sati_pct, nont_sati_pct),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric,
                           tax_sati_pct  = "Tax Satisfaction",
                           fee_sati_pct  = "Fee Satisfaction",
                           nont_sati_pct = "Non-Tax Satisfaction"
    ))
  
  ggplot(plot_data, aes(x = reorder(state, -Value), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(label = paste0(Value, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.8, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(
      "Tax Satisfaction"     = "#1f77b4",
      "Fee Satisfaction"     = "#2ca02c",
      "Non-Tax Satisfaction" = "#9467bd"
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title    = paste0(zone_name, " — Inter-State Satisfaction Comparison"),
         subtitle = "Proportion Satisfied or Very Satisfied by state",
         x = NULL, y = "Satisfaction (%)", fill = NULL) +
    bold_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "top")
}


# =============================================================================
#  SUMMARY TABLE HELPER
# =============================================================================

make_zone_summary_table <- function(zone_metrics) {
  tbl <- zone_metrics %>%
    arrange(zone) %>%
    select(
      Zone              = zone,
      `Sample (n)`      = n,
      `Tax Aware (%)`   = tax_aware_pct,
      `Tax Comply (%)`  = tax_comply_pct,
      `Fee Aware (%)`   = fee_aware_pct,
      `Fee Comply (%)`  = fee_comply_pct,
      `TIN (%)`         = tin_pct,
      `Digital (%)`     = digital_pct,
      `Readiness Score` = readiness_score,
      `Compliance Rate` = compliance_rate,
      `Tax Gap`         = tax_gap,
      `Fee Gap`         = fee_gap,
      `Overall Gap`     = overall_gap
    )
  
  n_rows <- nrow(tbl)
  n_cols <- ncol(tbl)
  
  flextable(tbl) %>%
    # ── Header ──────────────────────────────────────────────────────────────
    bold(part = "header") %>%
    bg(part = "header", bg = "#1f77b4") %>%
    color(part = "header", color = "white") %>%
    align(part = "header", align = "center") %>%
    # ── Zone column — prominent, left-aligned, highlighted ──────────────────
    bold(j = "Zone", part = "body") %>%
    color(j = "Zone", color = "#1f77b4", part = "body") %>%
    align(j = "Zone", align = "left", part = "body") %>%
    bg(j = "Zone", bg = "#EEF4FB", part = "body") %>%
    # ── Zebra striping on numeric columns ───────────────────────────────────
    bg(i = seq(1, n_rows, by = 2), j = 2:n_cols,
       bg = "#F7F7F7", part = "body") %>%
    bg(i = seq(2, n_rows, by = 2), j = 2:n_cols,
       bg = "#FFFFFF", part = "body") %>%
    # ── Centre-align numeric columns ────────────────────────────────────────
    align(j = 2:n_cols, align = "center", part = "body") %>%
    # ── Borders ─────────────────────────────────────────────────────────────
    border_outer(part = "all",
                 border = fp_border(color = "#1f77b4", width = 1.5)) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "#CCCCCC", width = 0.5)) %>%
    border_inner_v(part = "body",
                   border = fp_border(color = "#CCCCCC", width = 0.5)) %>%
    set_caption("Zone-Wide Summary: Compliance, Readiness, and Gap Metrics") %>%
    autofit()
}

make_state_summary_table <- function(state_metrics, zone_name) {
  tbl <- state_metrics %>%
    arrange(state) %>%
    select(
      State             = state,         # State column — first
      `Sample (n)`      = n,
      `Tax Aware (%)`   = tax_aware_pct,
      `Tax Comply (%)`  = tax_comply_pct,
      `Fee Aware (%)`   = fee_aware_pct,
      `Fee Comply (%)`  = fee_comply_pct,
      `TIN (%)`         = tin_pct,
      `Digital (%)`     = digital_pct,
      `Readiness Score` = readiness_score,
      `Compliance Rate` = compliance_rate,
      `Tax Gap`         = tax_gap,
      `Fee Gap`         = fee_gap,
      `Overall Gap`     = overall_gap
    )
  
  n_rows <- nrow(tbl)
  n_cols <- ncol(tbl)
  
  flextable(tbl) %>%
    # ── Header ──────────────────────────────────────────────────────────────
    bold(part = "header") %>%
    bg(part = "header", bg = "#2ca02c") %>%
    color(part = "header", color = "white") %>%
    align(part = "header", align = "center") %>%
    # ── State column — green, left-aligned, highlighted ──────────────────────
    bold(j = "State", part = "body") %>%
    color(j = "State", color = "#2ca02c", part = "body") %>%
    align(j = "State", align = "left", part = "body") %>%
    bg(j = "State", bg = "#EEF8EE", part = "body") %>%
    # ── Zebra striping on numeric columns ───────────────────────────────────
    bg(i = seq(1, n_rows, by = 2), j = 2:n_cols,
       bg = "#F7F7F7", part = "body") %>%
    bg(i = seq(2, n_rows, by = 2), j = 2:n_cols,
       bg = "#FFFFFF", part = "body") %>%
    # ── Centre-align numeric columns ────────────────────────────────────────
    align(j = 2:n_cols, align = "center", part = "body") %>%
    # ── Borders ─────────────────────────────────────────────────────────────
    border_outer(part = "all",
                 border = fp_border(color = "#2ca02c", width = 1.5)) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "#CCCCCC", width = 0.5)) %>%
    border_inner_v(part = "body",
                   border = fp_border(color = "#CCCCCC", width = 0.5)) %>%
    set_caption(paste0("Inter-State Summary — ", zone_name)) %>%
    autofit()
}


# =============================================================================
#  STEP 1 — Compute zone-level metrics (used across all zones)
# =============================================================================
zone_metrics <- compute_zone_metrics(srop)
cat("\nZone metrics computed.\n")
print(zone_metrics)


# =============================================================================
#  STEP 2 — Build & save zone-wide charts (all-zones overview)
# =============================================================================

zone_overview_folder <- file.path(getwd(), "Zone_Overview")
dir.create(zone_overview_folder, showWarnings = FALSE)
cat("\nCreated zone overview folder:", zone_overview_folder, "\n")

overview_plots <- list(
  "01_zone_compliance_rates.png"  = chart_zone_compliance(zone_metrics),
  "02_zone_readiness_scores.png"  = chart_zone_readiness(zone_metrics),
  "03_zone_gap_summary.png"       = chart_zone_gap(zone_metrics),
  "04_zone_satisfaction_rates.png"= chart_zone_satisfaction(zone_metrics)
)

overview_paths <- list()
for (fname in names(overview_plots)) {
  overview_paths[[fname]] <- save_plot(overview_plots[[fname]],
                                       zone_overview_folder, fname)
}

# Export all-zones summary Word doc
zone_summary_table <- make_zone_summary_table(zone_metrics)

doc_overview <- read_docx() %>%
  body_add_par("Zone-Wide Aggregated Findings", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par("1. Summary Table", style = "heading 2") %>%
  body_add_flextable(zone_summary_table) %>%
  body_add_par("") %>%
  body_add_par("2. Compliance Rates by Zone", style = "heading 2") %>%
  body_add_img(src = overview_paths[["01_zone_compliance_rates.png"]],
               width = 7, height = 4.5) %>%
  body_add_par("") %>%
  body_add_par("3. Digital Readiness Scores by Zone", style = "heading 2") %>%
  body_add_img(src = overview_paths[["02_zone_readiness_scores.png"]],
               width = 7, height = 4.5) %>%
  body_add_par("") %>%
  body_add_par("4. Awareness–Compliance Gap by Zone", style = "heading 2") %>%
  body_add_img(src = overview_paths[["03_zone_gap_summary.png"]],
               width = 7, height = 4.5) %>%
  body_add_par("") %>%
  body_add_par("5. Satisfaction Rates by Zone", style = "heading 2") %>%
  body_add_img(src = overview_paths[["04_zone_satisfaction_rates.png"]],
               width = 7, height = 4.5) %>%
  body_add_par("")

overview_doc_path <- file.path(zone_overview_folder, "srop_zone_overview.docx")
print(doc_overview, target = overview_doc_path)
cat("  Exported:", overview_doc_path, "\n")


# =============================================================================
#  STEP 3 — Loop over each zone: inter-state comparison charts + Word doc
# =============================================================================

for (zone_name in all_zones) {
  
  cat("\n========================================\n")
  cat(" Processing zone:", zone_name, "\n")
  cat("========================================\n")
  
  zone_slug   <- gsub(" ", "_", zone_name)
  zone_folder <- file.path(getwd(), paste0("Zone_", zone_slug))
  dir.create(zone_folder, showWarnings = FALSE)
  cat("  Folder:", zone_folder, "\n")
  
  # Compute state-level metrics for this zone
  state_metrics <- compute_state_metrics(srop, zone_name)
  
  # Generate all 4 inter-state charts
  zone_plots <- list(
    "01_state_compliance.png"   = chart_state_compliance(state_metrics, zone_name),
    "02_state_readiness.png"    = chart_state_readiness(state_metrics, zone_name),
    "03_state_gap.png"          = chart_state_gap(state_metrics, zone_name),
    "04_state_satisfaction.png" = chart_state_satisfaction(state_metrics, zone_name)
  )
  
  zone_paths <- list()
  for (fname in names(zone_plots)) {
    zone_paths[[fname]] <- save_plot(zone_plots[[fname]], zone_folder, fname)
  }
  
  # Summary table for this zone's states
  state_table <- make_state_summary_table(state_metrics, zone_name)
  
  # Build Word document for this zone
  doc_zone <- read_docx() %>%
    body_add_par(paste0(zone_name, " — Zone Analysis"), style = "heading 1") %>%
    body_add_par("") %>%
    body_add_par("1. Inter-State Summary Table", style = "heading 2") %>%
    body_add_flextable(state_table) %>%
    body_add_par("") %>%
    body_add_par("2. Inter-State Compliance Comparison", style = "heading 2") %>%
    body_add_img(src = zone_paths[["01_state_compliance.png"]],
                 width = 7, height = 4.5) %>%
    body_add_par("") %>%
    body_add_par("3. Inter-State Digital Readiness", style = "heading 2") %>%
    body_add_img(src = zone_paths[["02_state_readiness.png"]],
                 width = 7, height = 4.5) %>%
    body_add_par("") %>%
    body_add_par("4. Inter-State Awareness–Compliance Gap", style = "heading 2") %>%
    body_add_img(src = zone_paths[["03_state_gap.png"]],
                 width = 7, height = 4.5) %>%
    body_add_par("") %>%
    body_add_par("5. Inter-State Satisfaction Comparison", style = "heading 2") %>%
    body_add_img(src = zone_paths[["04_state_satisfaction.png"]],
                 width = 7, height = 4.5) %>%
    body_add_par("")
  
  zone_doc_path <- file.path(zone_folder, paste0("srop_", zone_slug, ".docx"))
  print(doc_zone, target = zone_doc_path)
  cat("  Exported:", zone_doc_path, "\n")
}

cat("\n All zone analyses completed!\n")