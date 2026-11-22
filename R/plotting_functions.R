library(ggplot2)
library(plotly)
library(gt)
library(highcharter)
library(data.table)
library(dplyr)
library(tidyr)

plot_lifeexp <- function(cca, comparison = "Chicago", title = "Life Expectancy by Year (2010 to 2024)") 
{le_plot <- lifeexp %>% filter(CCA %in% c(cca, comparison)) 
  highchart() %>% hc_add_series( le_plot, type = "line", hcaes( x = Year, y = le, group = CCA ) ) %>% 
    hc_tooltip( shared = TRUE, useHTML = TRUE, sort = TRUE ) %>%
    hc_title(text = title, style = list(color="black", fontSize = "20", fontFamily = "Arial")) %>% 
    hc_xAxis(title = list(text = "Year", style = list(color="black", fontSize = "16", fontFamily = "Arial")),
             labels = list(style = list(color="black", fontSize="14", fontFamily = "Arial")))%>%
    hc_yAxis(title = list(text = "Life Expectancy (Years)",  style = list(color="black", fontSize = "16", fontFamily = "Arial")),
             labels = list(style = list(color="black", fontSize="14", fontFamily = "Arial")))%>%
    hc_legend(itemStyle = list(color = "black", fontSize = "14", fontFamily = "Arial", fontWeight = "normal"))%>% 
    hc_plotOptions( series = list( marker = list(enabled = TRUE) ) )}

plot_lcod_overtime <- function(cca,
                               title = "Leading Causes of Death Mortality Rates Over Time") {
  
  library(dplyr)
  library(highcharter)
  
  LCOD_time_plot <- LCOD_ot %>%
    filter(CCA == cca)
  
  LCOD_time_plot <- LCOD_time_plot %>%
    rename("Heart Disease" = "Heart.Disease", 
           "Chronic Lower Respiratory Diseases" = "Chronic.Lower.Respiratory.Diseases", 
           "Alzheimer's Disease" = "Alzheimer.s.Disease", 
           "Kidney Disease" = "Kidney.Disease")
  
  causes <- setdiff(
    names(LCOD_time_plot),
    c("CCA", "Year")
  )
  
  max_rate <- ceiling(
    max(
      as.matrix(LCOD_time_plot[, causes]),
      na.rm = TRUE
    ) / 25
  ) * 25
  
  
  hc <- highchart()
  
  for (cause in causes) {
    
    if (cause %in% names(LCOD_time_plot)) {
      
      hc <- hc %>%
        hc_add_series(
          LCOD_time_plot,
          name = cause,
          type = "line",
          hcaes(
            x = Year,
            y = .data[[cause]]
          )
        )
    }
  }
  
  hc %>%
    hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      sort = TRUE
    ) %>%
    hc_title(text = title, style = list(color="black", fontSize = "20", fontFamily = "Arial")) %>%
    hc_xAxis(categories = LCOD_time_plot$Year,
      title = list(text = "Rolling Five-Year Period", 
                   style = list(color="black", fontSize = "16", fontFamily = "Arial"), margin = 15),
      labels = list(rotation = -30, style = list(color="black", fontSize="14", fontFamily = "Arial")))%>%
    hc_yAxis(
      title = list(text = "Mortality Rate per 100,000", style = list(color="black", fontSize = "16", fontFamily = "Arial"), margin = 15,
      min = 0,
      max = max_rate),
      labels = list(style = list(color="black", fontSize="14", fontFamily = "Arial")))%>%
    hc_legend(itemStyle = list(color = "black", fontSize = "14", fontFamily = "Arial", fontWeight = "normal"))
}

make_lcod_table <- function(cca){
  
  library(dplyr)
  library(knitr)
  library(kableExtra)
  
  urls <- c(
    "Heart Disease" = "https://chicagohealthatlas.org/indicators/VRHDR?topic=heart-disease-mortality-rate",
    "Cancer" = "https://chicagohealthatlas.org/indicators/VRCAR?topic=cancer-mortality-rate",
    "Accidents" = "https://chicagohealthatlas.org/indicators/VRACR?topic=accidents-mortality-rate",
    "COVID-19" = "https://chicagohealthatlas.org/indicators/CDCOVDR?topic=covid-19-death-rate",
    "Diabetes" = "https://chicagohealthatlas.org/indicators/VRDIAR?topic=diabetes-mortality-rate",
    "Homicide" = "https://chicagohealthatlas.org/indicators/VRHOR?topic=homicide-mortality-rate",
    "Chronic Lower Respiratory Diseases" =
      "https://chicagohealthatlas.org/indicators/VRLRR?topic=chronic-lower-respiratory-disease-mortality-rate",
    "Alzheimer's Disease" =
      "https://chicagohealthatlas.org/indicators/VRADR?topic=alzheimers-disease-mortality-rate",
    "Kidney Disease" =
      "https://chicagohealthatlas.org/indicators/VRNER?topic=nephritis-nephrotic-syndrome-and-nephrosis-mortality-rate"
  )
  
  
  tbl <- lcod %>%
    filter(CCA == cca | CCA == 'Chicago') %>%
    rename('Heart Disease' = 'Heart', 'Accidents (Unintential Injuries)' = 'Accidents', 'COVID-19' = 'COVID', 'Assault (Homicide)' = 'Assault', 'Chronic Lower Respiratory Diseases' = 'Resp', 
           "Alzheimer's Disease" = "Alz", "Kidney Disease" = "Kidney")
  
  tbl <- data.table::transpose(tbl, keep.names = "Leading Causes of Death (per 100,000)", make.names = 1)
  
  tbl <- tbl %>%
    arrange(desc(.[[cca]]))
  
  cause_col <- names(tbl)[1]
  
  tbl[[cause_col]] <- paste0(
    "<a href='",
    ifelse(is.na(urls[tbl[[cause_col]]]), "#", urls[tbl[[cause_col]]]),
    "' target='_blank'>",
    tbl[[cause_col]],
    "</a>"
  )
  
  gt(tbl) %>%
    fmt_markdown(columns = 1)
  
  tbl[[cause_col]] <- paste0(
    "[",
    tbl[[cause_col]],
    "](",
    urls[tbl[[cause_col]]],
    ")"
  )
  
  library(gt)
  
  tbl %>%
    gt() %>%
    fmt_markdown(columns = all_of(cause_col)) %>%
    fmt_number(
      columns = 2:3,
      decimals = 1
    ) %>%
    cols_align(
      align = "left",
      columns = 1
    ) %>%
    cols_align(
      align = "center",
      columns = 2:3
    ) %>%
    opt_table_font(
      font = "Arial"
    ) %>%
    tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      
      table.border.top.width = px(2),
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.width = px(1),
      
      heading.align = "left",
      table.font.size = px(16),
      data_row.padding = px(8),
      column_labels.font.weight = "bold"
    )
}

make_resource_table <- function() {
  
  library(dplyr)
  library(gt)
  
  resources <- c(
    "Heart Disease Prevention (CDC)" =
      "https://www.cdc.gov/heart-disease/prevention/index.html",
    "Heart Disease Resources" =
      "https://www.thecommunityguide.org/topics/heart-disease-stroke-prevention.html",
    "Cancer Prevention (CDC)" =
      "https://www.cdc.gov/cancer/prevention/index.html",
    "Cancer Resources" =
      "https://www.thecommunityguide.org/topics/cancer.html",
    "Overdose Resources" =
      "https://www.chicago.gov/city/en/depts/cdph/supp_info/behavioral-health/overcome-opioids.html",
    "COVID-19 Guidance" =
      "https://www.cdc.gov/covid/prevention/index.html",
    "Diabetes Prevention" =
      "https://www.cdc.gov/diabetes/prevention-type-2/index.html",
    "Diabetes Resources" =
      "https://www.thecommunityguide.org/topics/diabetes.html",
    "Homicide Resources" =
      "https://www.chicago.gov/city/en/depts/cdph/supp_info/behavioral-health/victims-and-survivors.html",
    "Chronic Lower Respiratory Diseases" =
      "https://my.clevelandclinic.org/health/diseases/8709-chronic-obstructive-pulmonary-disease-copd#prevention",
    "Alzheimer's Disease" =
      "https://www.nia.nih.gov/health/alzheimers-and-dementia/preventing-alzheimers-disease-what-do-we-know",
    "Kidney Disease" =
      "https://www.cdc.gov/kidney-disease/prevention/index.html"
  )
  
  descriptions <- c(
    "Information on lifestyle habits and medical conditions to help prevent heart disease and heart attack.",
    "Information on interventions for heart disease and best practices. This resource includes links to resources, summary pages, and presentations/webinars.",
    "Overview of how to lower the risk of many common kinds of cancers with a focus on screening, vaccination, and lifestyle.",
    "Information for increasing cancer screening through community interventions with implementation resources.",
    "This resource helps connect individuals with the right services and information around opioid misuse and overdose. This includes general information, prevention, response, and recovery.",
    "Core prevention strategies and risk factors for COVID-19.",
    "This resource aims to help individuals at risk for or with prediabetes to manage the condition and prevent development of type 2 diabetes. It outlines risk factors and the CDC-recognized National Diabetes Prevention Program.",
    "Community preventive services for type 2 Diabetes including lifestyle interventions and comprehensive information.",
    "Victim Services for those affected by community violence in Chicago with information for accessing these services.",
    "Description of Chronic Obstructive Pulmonary Disease (COPD), its causes and risk factors, diagnostic tests, and how to manage and receive treatment for the disease.",
    "Strategies for preventing or delaying Alzheimer's Disease with additional resources linked for information and services.",
    "Description of Chronic Kidney Disease with information on managing risk factors and getting tested."
  )
  
  tbl <- data.frame(
    Resource = names(resources),
    Description = descriptions,
    URL = unname(resources),
    stringsAsFactors = FALSE
  )
  
  tbl$Resource <- paste0(
    "<a href='",
    tbl$URL,
    "' target='_blank'>",
    tbl$Resource,
    "</a>"
  )
  
  tbl |>
    select(Resource, Description) |>
    gt() |>
    fmt_markdown(columns = Resource) |>
    cols_align(
      align = "left",
      columns = everything()
    ) |>
    cols_width(
      Resource ~ pct(35),
      Description ~ pct(65)
    ) |>
    opt_table_font(
      font = "Arial"
    ) |>
    tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      
      table.border.top.width = px(2),
      table.border.bottom.width = px(2),
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.width = px(1),
      
      table.font.size = px(16),
      data_row.padding = px(8),
      column_labels.font.weight = "bold"
    )
}

plot_race <- function(cca, comparison = "Chicago", title = "Race/Ethnicity (2020 to 2024)")
{race_plot <- demographics %>%
  filter(CCA %in% c(cca, comparison)) %>%
  select(CCA, White, Black, Asian, Hispanic)
  
race_plot_long <- pivot_longer(
  race_plot, cols = c(White, Black, Asian, Hispanic),
  names_to = "Race/Ethnicity",
  values_to = "Percentage"
)
  ggplot(data = race_plot_long, aes(fill=`Race/Ethnicity`, y=Percentage, x=CCA)) +
    geom_col(position = position_dodge(0.9), stat="identity", width=0.75) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=12)),
          axis.text.x = element_text(size = 12, color = "black", family = "sans", margin = margin(t=10)),
          axis.text.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=8)),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)) +
    geom_text(aes(label = ifelse(Percentage < 1, "<1%", paste0(round(Percentage), "%"))), vjust = -0.5, position = position_dodge(0.9), color = "black", family = "sans") +
    scale_fill_manual(values=c("#001E62", "#00966c", "#3D83A4", "#D50032"))} +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = function(x) ifelse(x == 0, "", x))

plot_sex <- function(cca, comparison = "Chicago", title = "Sex (2020 to 2024)")
{sex_plot <- demographics %>%
  filter(CCA %in% c(cca, comparison)) %>%
  select(CCA, Female, Male)

sex_plot_long <- pivot_longer(
  sex_plot, cols = c(Female, Male),
  names_to = "Sex",
  values_to = "Percentage"
)
ggplot(data = sex_plot_long, aes(fill=Sex, y=Percentage, x=CCA)) +
  geom_col(position = position_dodge(0.9), stat="identity", width=0.65) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=12)),
        axis.text.x = element_text(size = 12, color = "black", family = "sans", margin = margin(t=10)),
        axis.text.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=8)),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  geom_text(aes(label = ifelse(Percentage < 1, "<1%", paste0(round(Percentage), "%"))), vjust = -0.5, position = position_dodge(0.9), color = "black", family = "sans") +
  scale_fill_manual(values=c("#00966c", "#D50032"))} +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = function(x) ifelse(x == 0, "", x))

plot_age <- function(cca, comparison = "Chicago", title = "Age (2020 to 2024)")
{age_plot <- demographics %>%
  filter(CCA %in% c(cca, comparison)) %>%
  rename("<18" = X.18, "18 to 39" = X18.to.39, "40 to 64" = X40.to.64, "65+" = X65.) %>%
  select(CCA, "<18", "18 to 39", "40 to 64", "65+")

age_plot_long <- pivot_longer(
  age_plot, cols = c("<18", "18 to 39", "40 to 64", "65+"),
  names_to = "Age",
  values_to = "Percentage"
)
ggplot(data = age_plot_long, aes(fill=Age, y=Percentage, x=CCA)) +
  geom_col(position = position_dodge(0.9), stat="identity", width=0.75) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=12)),
        axis.text.x = element_text(size = 12, color = "black", family = "sans", margin = margin(t=10)),
        axis.text.y = element_text(size = 12, color = "black", family = "sans", margin = margin(r=8)),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  geom_text(aes(label = ifelse(Percentage < 1, "<1%", paste0(round(Percentage), "%"))), vjust = -0.5, position = position_dodge(0.9), color = "black", family = "sans") +
  scale_fill_manual(values=c("#001E62", "#00966c", "#3D83A4", "#D50032"))} +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = function(x) ifelse(x == 0, "", x))
