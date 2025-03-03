# Load required libraries
library(tidyverse)
library(lubridate)

# Create a tibble with the correct lab results
lab_data <- tibble(
  Date              = as.Date(c("2020-08-07", "2021-11-17", "2022-08-04", "2023-08-08", "2024-09-09")),
  BPsys             = c(125  , 120  , 111  , 119  , 108  ),
  BPdia             = c(66   , 76   , 77   , 64   , 72   ),
  Pulse             = c(73   , 55   , 88   , 57   , 49   ),
  O2                = c(96   , 98   , 98   , 98   , 98   ),
  Temp              = c(97.3 , 97.9 , 97.1 , 97.5 , 98   ),
  WBC               = c(5.1  , 6.4  , 5.2  , 4.5  , 5.5  ),
  RBC               = c(5.04 , 4.94 , 5.13 , 4.89 , 4.93 ),
  HGB               = c(15.5 , 15.3 , 15.5 , 15.0 , 15.2 ),
  HCT               = c(45.1 , 45.3 , 46.2 , 44.1 , 45.0 ),
  MCV               = c(90   , 92   , 90   , 90   , 91.3 ),
  MCH               = c(30.8 , 31.0 , 30.2 , 30.7 , 30.8 ),
  MCHC              = c(34.4 , 33.8 , 33.5 , 34.0 , 33.8 ),
  RDW               = c(12.9 , 13.1 , 12.7 , 12.5 , 12.5 ),
  PLT               = c(224  , 244  , 244  , 213  , 216  ),
  NeutrophilsPct    = c(53   , 55   , 55   , 57   , NA   ),
  LymphocytesPct    = c(36   , 34   , 37   , 33   , NA   ),
  MonocytesPct      = c(7    , 7    , 6    , 8    , NA   ),
  EosinophilsPct    = c(3    , 2    , 2    , 2    , NA   ),
  BasophilsPct      = c(1    , 1    , 0    , 0    , NA   ),
  NeutrophilsAbs    = c(2.7  , 3.6  , 2.8  , 2.5  , NA   ),
  LymphocytesAbs    = c(1.8  , 2.2  , 1.9  , 1.5  , NA   ),
  MonocytesAbs      = c(0.4  , 0.5  , 0.3  , 0.4  , NA   ),
  EosinophilsAbs    = c(0.1  , 0.1  , 0.1  , 0.1  , NA   ),
  BasophilsAbs      = c(0.0  , 0.0  , 0.0  , 0.0  , NA   ),
  GranulocytesPct   = c(0    , 1    , 0    , 0    , NA   ),
  GranulocytesAbs   = c(0.0  , 0.0  , 0.0  , 0.0  , NA   ),
  Glucose           = c(88   , 100  , 97   , 94   , 100  ),
  BUN               = c(16   , 13   , 15   , 17   , 12   ),
  Creatinine        = c(0.87 , 0.88 , 0.84 , 0.80 , 0.88 ),
  eGFR              = c(113  , 111  , 116  , 117  , 113  ),
  Sodium            = c(141  , 142  , 140  , 142  , 140  ),
  Potassium         = c(4.2  , 4.7  , 4.2  , 4.4  , 4.7  ),
  Chloride          = c(107  , 105  , 105  , 105  , 106  ),
  CO2               = c(21   , 23   , 22   , 21   , 27   ),
  Calcium           = c(9.5  , 9.8  , 9.6  , 9.6  , 9.5  ),
  Protein           = c(7.0  , 7.5  , 7.3  , 7.1  , 6.6  ),
  Albumin           = c(4.8  , 5.0  , 5.0  , 4.8  , 4.5  ),
  Globulin          = c(2.2  , 2.5  , 2.3  , 2.3  , 2.1  ),
  AGratio           = c(2.2  , 2.0  , 2.2  , 2.1  , 2.1  ),
  Bilirubin         = c(0.4  , 0.3  , 0.5  , 0.5  , 0.8  ),
  ALP               = c(59   , 76   , 73   , 69   , 53   ),
  AST               = c(19   , 19   , 15   , 17   , 13   ),
  ALT               = c(19   , 28   , 10   , 15   , 17   ),
  Total_Cholesterol = c(120  , 138  , 135  , 123  , 141  ),
  HDL_Cholesterol   = c(41   , 45   , 42   , 47   , 50   ),
  VLDL_Cholesterol  = c(7    , 11   , 12   , 11   , NA   ),
  LDL_Cholesterol   = c(70   , 82   , 81   , 65   , 77   ),
  Triglycerides     = c(47   , 52   , 55   , 47   , 61   ),
  A1C               = c(4.8  , 5.1  , 5.3  , 4.8  , 5.0  ),
  eAG               = c(91   , 100  , 105  , 91   , 97   ),
  TSH               = c(1.520, 1.630, 1.070, 1.320, 1.36 ),
  Uric              = c(5.6  , NA   , NA   , NA   , NA   ),
  VitD              = c(NA   , NA   , 57.6 , NA   , NA   ),
  VitB12            = c(NA   , NA   , NA   , NA   , 515  ),
  Folate            = c(NA   , NA   , NA   , NA   , 21.4 ),
  T4Free            = c(NA   , 1.32 , NA   , NA   , 1.2  ),
  T3Free            = c(NA   , 3.8  , NA   , NA   , 3.3  ),
  PSA               = c(NA   , NA   , NA   , 0.4  , 0.71 ),
  PMV               = c(NA   , NA   , NA   , NA   , 9.1  ),
  TESTOST           = c(NA   , NA   , NA   , NA   , 263  ),
  TESTOSTFree       = c(NA   , NA   , NA   , NA   , 34.9 ),
)


# Historical lab data from PDF (2012-2017)
historical_lab_data <- tibble(
  # Dates from the PDF (converted to standard format)
  Date              = as.Date(c("2012-10-15", "2013-01-30", "2015-11-05", "2016-06-17", "2016-10-03", "2017-04-12")),
  # Liver function tests
  ALT               = c(NA      , 20       , 68       , 34       , 18       , 23       ),
  AST               = c(NA      , 20       , 39       , 35       , 22       , 20       ),
  ALP               = c(NA      , 62       , 70       , 54       , 64       , 53       ),
  Bilirubin         = c(NA      , 0.6      , 0.7      , 0.6      , 0.7      , 0.8      ),
  # Blood chemistry
  Protein           = c(NA      , 7.2      , 7.3      , 6.6      , 7.2      , 6.9      ),
  Albumin           = c(NA      , 4.3      , 4.4      , 4.0      , 4.5      , 4.1      ),
  Globulin          = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  AGratio           = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  # Electrolytes & Kidney function
  Sodium            = c(NA      , NA       , NA       , NA       , 140      , NA       ),
  Potassium         = c(NA      , NA       , NA       , NA       , 3.6      , NA       ),
  Chloride          = c(NA      , NA       , NA       , NA       , 107      , NA       ),
  CO2               = c(NA      , NA       , NA       , NA       , 26       , NA       ),
  BUN               = c(NA      , NA       , NA       , NA       , 15       , NA       ),
  Creatinine        = c(NA      , NA       , NA       , NA       , 0.8      , NA       ),
  eGFR              = c(NA      , NA       , NA       , NA       , 60       , NA       ),
  Calcium           = c(NA      , NA       , NA       , NA       , 9.4      , NA       ),
  Glucose           = c(NA      , NA       , NA       , NA       , 98       , NA       ),
  # Lipid panel
  Total_Cholesterol = c(84      , 104      , 104      , NA       , 117      , 101      ),
  HDL_Cholesterol   = c(30      , 34       , NA       , NA       , NA       , NA       ),
  LDL_Cholesterol   = c(30      , 38       , 48       , NA       , NA       , 46       ),
  Triglycerides     = c(120     , 158      , 128      , NA       , 153      , 106      ),
  VLDL_Cholesterol  = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  # Fill all other columns with NA
  BPsys             = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  BPdia             = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  Pulse             = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  O2                = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  Temp              = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  WBC               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  RBC               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  HGB               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  HCT               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  MCV               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  MCH               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  MCHC              = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  RDW               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  PLT               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  NeutrophilsPct    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  LymphocytesPct    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  MonocytesPct      = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  EosinophilsPct    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  BasophilsPct      = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  NeutrophilsAbs    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  LymphocytesAbs    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  MonocytesAbs      = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  EosinophilsAbs    = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  BasophilsAbs      = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  GranulocytesPct   = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  GranulocytesAbs   = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  A1C               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  eAG               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  TSH               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  Uric              = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  VitD              = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  VitB12            = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  Folate            = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  T4Free            = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  T3Free            = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  PSA               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  PMV               = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  TESTOST           = c(NA      , NA       , NA       , NA       , NA       , NA       ),
  TESTOSTFree       = c(NA      , NA       , NA       , NA       , NA       , NA       )
)

# Historical lab data from PDF (2012-2017)
in_body_scans <- tibble(
  # Dates from the PDF (converted to standard format)
  Date              = as.Date(c("2023-07-12", "2013-01-30")),
  Glucose           = c(87      , 86       ),
  # Lipid panel
  Total_Cholesterol = c(101     , 116      ),
  HDL_Cholesterol   = c(49      , 52       ),
  LDL_Cholesterol   = c(40      , 52       ),
  Triglycerides     = c(62      , 57      ),
  BPsys             = c(131      , 117       ),
  BPdia             = c(76      , 68       ),
  VLDL_Cholesterol  = c(NA      , NA       ),
  # Fill all other columns with NA
  # Liver function tests
  ALT               = c(NA      , NA       ),
  AST               = c(NA      , NA       ),
  ALP               = c(NA      , NA       ),
  Bilirubin         = c(NA      , NA       ),
  # Blood chemistry
  Protein           = c(NA      , NA       ),
  Albumin           = c(NA      , NA       ),
  Globulin          = c(NA      , NA       ),
  AGratio           = c(NA      , NA       ),
  # Electrolytes & Kidney function
  Sodium            = c(NA      , NA       ),
  Potassium         = c(NA      , NA       ),
  Chloride          = c(NA      , NA       ),
  CO2               = c(NA      , NA       ),
  BUN               = c(NA      , NA       ),
  Creatinine        = c(NA      , NA       ),
  eGFR              = c(NA      , NA       ),
  Calcium           = c(NA      , NA       ),
  Pulse             = c(NA      , NA       ),
  O2                = c(NA      , NA       ),
  Temp              = c(NA      , NA       ),
  WBC               = c(NA      , NA       ),
  RBC               = c(NA      , NA       ),
  HGB               = c(NA      , NA       ),
  HCT               = c(NA      , NA       ),
  MCV               = c(NA      , NA       ),
  MCH               = c(NA      , NA       ),
  MCHC              = c(NA      , NA       ),
  RDW               = c(NA      , NA       ),
  PLT               = c(NA      , NA       ),
  NeutrophilsPct    = c(NA      , NA       ),
  LymphocytesPct    = c(NA      , NA       ),
  MonocytesPct      = c(NA      , NA       ),
  EosinophilsPct    = c(NA      , NA       ),
  BasophilsPct      = c(NA      , NA       ),
  NeutrophilsAbs    = c(NA      , NA       ),
  LymphocytesAbs    = c(NA      , NA       ),
  MonocytesAbs      = c(NA      , NA       ),
  EosinophilsAbs    = c(NA      , NA       ),
  BasophilsAbs      = c(NA      , NA       ),
  GranulocytesPct   = c(NA      , NA       ),
  GranulocytesAbs   = c(NA      , NA       ),
  A1C               = c(NA      , NA       ),
  eAG               = c(NA      , NA       ),
  TSH               = c(NA      , NA       ),
  Uric              = c(NA      , NA       ),
  VitD              = c(NA      , NA       ),
  VitB12            = c(NA      , NA       ),
  Folate            = c(NA      , NA       ),
  T4Free            = c(NA      , NA       ),
  T3Free            = c(NA      , NA       ),
  PSA               = c(NA      , NA       ),
  PMV               = c(NA      , NA       ),
  TESTOST           = c(NA      , NA       ),
  TESTOSTFree       = c(NA      , NA       )
)


lab_data <- bind_rows(historical_lab_data, lab_data) %>%
            bind_rows(in_body_scans) %>%
  mutate(yearmonth = floor_date(Date, "month"))

# Create the plot function
plot_lab_measure <- function(data, measure, measname) {
  plot <- data %>%
    ggplot(aes(x = Date, y = !!sym(measure), group = 1)) + # Add group = 1
    geom_line() +
    geom_point() +
    geom_vline(xintercept = as.numeric(as.Date("2022-07-01")), 
               color = "red", linetype = "dashed") +
    labs(x = "Date",
         y = measname) +
    theme_minimal() +
    # Use annual breaks with vertical labels
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%b\n%Y") +  # Add line break between month and year
    theme(
      axis.text.x = element_text(angle = 45,  # Completely vertical text
                                 hjust = 0.5, # Center horizontally
                                 vjust = 0.5) # Center vertically
    )
  return(plot)
}

plot_lab_measure_with_segments <- function(data, measure, measname, 
                                           max_gap_days = 365,
                                           angle = 45) {
  
  # Filter out NA values for the specified measure
  filtered_data <- data %>%
    filter(!is.na(!!sym(measure))) %>%
    arrange(Date)
  
  # Calculate date differences
  date_diffs <- filtered_data %>%
    mutate(
      next_date = lead(Date),
      next_value = lead(!!sym(measure)),
      gap_days = as.numeric(next_date - Date)
    ) %>%
    filter(!is.na(next_date))  # Remove the last row with NA for next values
  
  # Create plot base
  plot <- ggplot() +
    # Add points for all measurements
    geom_point(data = filtered_data, 
               aes(x = Date, y = !!sym(measure)), 
               size = 2) +
    
    # Add connecting segments with conditional styling
    geom_segment(data = date_diffs,
                 aes(x = Date, y = !!sym(measure),
                     xend = next_date, yend = next_value,
                     linetype = if_else(gap_days > max_gap_days, "dashed", "solid")),
                 show.legend = FALSE) +
    
    # Define line types
    scale_linetype_identity() +
    
    # Add the vertical reference line
    geom_vline(xintercept = as.numeric(as.Date("2022-07-01")), 
               color = "red", linetype = "dashed") +
    
    # Labels and theme
    labs(x = "Date", 
         y = measname) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
    theme(
      axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1),
      plot.margin = margin(b = 20, unit = "pt")
    )
  
  return(plot)
}

# Example usage:
plot_lab_measure_with_segments(lab_data, "Total_Cholesterol", "Total Cholesterol", max_gap_days = 20000)
ggsave('../exhibits/figures/TotChol.png', width = 7, height = 5)
plot_lab_measure_with_segments(lab_data, "LDL_Cholesterol", "LDL Cholesterol", max_gap_days = 20000)
ggsave('../exhibits/figures/LDLChol.png', width = 7, height = 5)
plot_lab_measure_with_segments(lab_data, "HDL_Cholesterol", "HDL Cholesterol", max_gap_days = 20000)
ggsave('../exhibits/figures/HDLChol.png', width = 7, height = 5)
plot_lab_measure_with_segments(lab_data, "Triglycerides", "Triglycerides", max_gap_days = 20000)
ggsave('../exhibits/figures/Triglycerides.png', width = 7, height = 5)
plot_lab_measure_with_segments(lab_data, "Glucose", "Glucose", max_gap_days = 20000)
ggsave('../exhibits/figures/Glucose.png', width = 7, height = 5)
plot_lab_measure_with_segments(lab_data, "A1C", "HbA1c (%)", max_gap_days = 20000)
ggsave('../exhibits/figures/HbA1c.png', width = 7, height = 5)

asf

# Function to create a line plot for a single test
create_plot <- function(data, test_name) {
  data %>%
    ggplot(aes(x = Date, y = .data[[test_name]])) +
    geom_line() +
    geom_point() +
    labs(title = paste(test_name, "Trend"), y = test_name, x = "Date") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create plots for each test
plots <- lab_data %>%
  select(-Date) %>%
  names() %>%
  set_names() %>%
  map(~ create_plot(lab_data, .x))

# Save plots to PDF
pdf("bloodwork_trends_corrected.pdf", width = 10, height = 6)
walk(plots, print)
dev.off()

# Print a message to confirm the PDF has been created
cat("Corrected bloodwork trend plots have been saved to 'bloodwork_trends_corrected.pdf'\n")