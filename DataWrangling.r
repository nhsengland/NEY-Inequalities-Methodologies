latest_date <- as.Date("2023-12-01") # "To December 2023"
first_date <- as.Date("2023-03-01") # "To March 2023"
ICB_Name <- "NHS Humber and North Yorkshire ICB"
National <- "England"
NEYAreas <- c("NHS Humber and North Yorkshire ICB", "NHS North East and North Cumbria ICB",
              "NHS South Yorkshire ICB", "NHS West Yorkshire ICB")

inputfile <- "CVDP003CHOL.xlsx"
DATA <- read_excel(file.path(this.dir(), "DataFiles", inputfile))

DATA$TimePeriodName <- gsub("To March ", "01-03-", DATA$TimePeriodName)
DATA$TimePeriodName <- gsub("To June ", "01-06-", DATA$TimePeriodName)
DATA$TimePeriodName <- gsub("To September ", "01-09-", DATA$TimePeriodName)
DATA$TimePeriodName <- gsub("To December ", "01-12-", DATA$TimePeriodName)
DATA$TimePeriodName <- as.Date(DATA$TimePeriodName, format = "%d-%m-%Y")


DATA$NewLow <- DATA$Value - DATA$LowerConfidenceLimit

DATA$MetricCategoryName <- (ifelse(grepl("1 - most deprived", DATA$MetricCategoryName),
                                                 "1", DATA$MetricCategoryName))
DATA$MetricCategoryName <- (ifelse(grepl("5 - least deprived", DATA$MetricCategoryName),
                                                 "5", DATA$MetricCategoryName))

DATA <- NameWrangle(DATA, AreaName)

#  Code for Sii Bar Chart
      DATA_BAR <- DATA %>% filter(TimePeriodName == latest_date)
      areas <- unique(DATA_BAR$AreaName)
      metrics <- unique(DATA_BAR$IndicatorCode)
      
      AllICB_Sii <- data.frame(IndicatorCode = character(0), AreaName = character(0), TimePeriodName = character(0), Sii = numeric(0), 
                               lower = numeric(0), upper = numeric(0))
      
      for (i in 1:(length(areas) ) ) {    # was -1, for not England, but now need it
        for (j in 1:length(metrics)) {
          #SII_Loop_Output <- Quintiles_new(areas[i], DATA_BAR, FALSE) 
          SII_Loop_Output <- Quintiles_new_Sii(DATA, metrics[j], latest_date, areas[i], National, FALSE, reps = 1000,
                                   AreaName_Col = AreaName, MetricName_Col = IndicatorCode, IMD_Col= MetricCategoryName, Population_Col = Denominator, 
                                   Value_Col = Value, Lower_CI_Col = LowerConfidenceLimit, Upper_CI_Col = UpperConfidenceLimit, TimePeriodName_Col = TimePeriodName)
          # Note that the SII_Loop_Output returns 10 values, including 
          # 1 - 5 the quintile midpoints, 6,7 intercept & slope (Sii) of fitted line, 8 & 9 lower & upper Conf limits
          # so xx[7] - xx[8] is the length of the conf interval (to 4 dp)
          AllICB_Sii[nrow(AllICB_Sii) + 1, ] <- c(
            metrics[j], areas[i], latest_date,
            round(SII_Loop_Output[7], digits = 4), 
            round((SII_Loop_Output[7] - SII_Loop_Output[8]), digits = 2), 
            round((SII_Loop_Output[9] - SII_Loop_Output[7]), digits = 2)
          )
        }
      }
      
      AllICB_Sii$Sii <- round(as.numeric(AllICB_Sii$Sii), digits = 2)
      AllICB_Sii$TimePeriodName <- as.Date(as.numeric(AllICB_Sii$TimePeriodName))
      AllICB_Sii$lower <- as.numeric(AllICB_Sii$lower)
      AllICB_Sii$upper <- as.numeric(AllICB_Sii$upper)
      
      AllICB_Sii$Region_Flag <- (ifelse(grepl("Humber and North Yorkshire|North East and North Cumbria|South Yorkshire|West Yorkshire", AllICB_Sii$`AreaName`), AllICB_Sii$`AreaName`, "Other"
      ))
      
  
#  Code for Sii Time Series Chart 
      DATA_ALL <- DATA %>% 
        # filter(MetricCategoryName != "Persons") %>%
        filter(AreaName %in% c("NHS Humber and North Yorkshire ICB", "NHS North East and North Cumbria ICB", 
                               "NHS South Yorkshire ICB", "NHS West Yorkshire ICB", "England") )
      
      areas <- unique(DATA_ALL$AreaName) 
      dates <- unique(DATA_ALL$TimePeriodName)
      metrics <- unique(DATA_ALL$IndicatorCode)
      
      ICB_Sii <- data.frame(IndicatorCode = character(0), AreaName = character(0), TimePeriodName = character(0), Sii = numeric(0), 
                            lower = numeric(0), upper = numeric(0))
      
      for (i in 1:length(areas)) {    # was -1, for not England
        for (j in 1:length(dates)){
          for (k in 1:length(metrics)){
            # data_sii <- DATA_ALL %>%
            #   filter(TimePeriodName == dates[j])
  
            # SII_Loop_Output <- Quintiles_new(areas[i], data_sii, FALSE) 
            SII_Loop_Output <- Quintiles_new_Sii(DATA_ALL, metrics[k], dates[j], areas[i], National, FALSE, reps = 1000,
                                     AreaName_Col = AreaName, MetricName_Col = IndicatorCode, IMD_Col= MetricCategoryName, Population_Col = Denominator, 
                                     Value_Col = Value, Lower_CI_Col = LowerConfidenceLimit, Upper_CI_Col = UpperConfidenceLimit, TimePeriodName_Col = TimePeriodName)
            # Note that the SII_Loop_Output returns 10 values, including 
            # 1 - 5 the quintile midpoints, 6,7 intercept & slope (Sii) of fitted line, 8 & 9 lower & upper Conf limits
            # so xx[7] - xx[8] is the length of the conf interval (to 4 dp)
            ICB_Sii[nrow(ICB_Sii) + 1, ] <- c(
              metrics[k], areas[i], dates[j], round(SII_Loop_Output[7], digits = 4), 
              round((SII_Loop_Output[7] - SII_Loop_Output[8]), digits = 2), 
              round((SII_Loop_Output[9] - SII_Loop_Output[7]), digits = 2)
          )
          }  
        }
      }
      
      ICB_Sii$Sii <- as.numeric(ICB_Sii$Sii)
      ICB_Sii$TimePeriodName <- as.Date(as.numeric(ICB_Sii$TimePeriodName))
      ICB_Sii$Region_Flag <- (ifelse(grepl("Humber and North Yorkshire|North East and North Cumbria|South Yorkshire|West Yorkshire", ICB_Sii$`AreaName`), ICB_Sii$`AreaName`, "Other"
      ))
      
      
# Code for Maps      
      inputfile <- "ForMaps.xlsx"
      # Mapping <- read_excel(here("DataFiles", inputfile), sheet = "LSOA to ICB Mapping")
      Mapping <- read_excel(file.path(this.dir(), "DataFiles", inputfile), sheet = "LSOA to ICB Mapping")
      
      # LSOA_AGI <- read_excel(here("DataFiles", inputfile), sheet = "ENGLAND LSOA AGI", range = "A4:D32848")
      LSOA_AGI <- read_excel(file.path(this.dir(), "DataFiles", inputfile), sheet = "ENGLAND LSOA AGI", range = "A4:D32848")
      
      # LSOA_shapefile <- here("ShapeFiles", "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp")   %>%
      #   st_read(quiet = TRUE)
      LSOA_shapefile <- file.path(this.dir(), "ShapeFiles", "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp")   %>%
        st_read(quiet = TRUE)
      LSOA_shapefile_wgs84 <- st_transform(LSOA_shapefile, "+proj=longlat +datum=WGS84")
      
      LSOA_AGI <- dplyr::rename(LSOA_AGI, `IMD rank` = `IMD proportional rank (where 1 Is most deprived)`)
      LSOA_AGI$`ISR per 100,000` <- round(LSOA_AGI$`ISR per 100,000`, digits = 2)
      LSOA_AGI$`IMD rank` <- round(LSOA_AGI$`IMD rank`, digits = 4)

      LSOAtoICB <- Mapping %>%
        select(LSOA11CD, LSOA11NM, ICB22NM, Region22NM)
      LSOA_Scatter <- merge(LSOA_AGI, LSOAtoICB, by.x = 'Area codes', by.y = 'LSOA11CD') %>%
        mutate("Region2" = case_when(Region22NM == "North East and Yorkshire" ~ `ICB22NM`, TRUE ~ "Other"))
      
#      All cause mortality among patients with GP recorded cardiovascular disease (wide definition) in patients aged 18 and over (Age standardised rate per 100,000 person years)
      DATAMORT <- data.frame(
        AreaName = c("North East and Yorkshire", "North East and Yorkshire", "North East and Yorkshire", "North East and Yorkshire", "North East and Yorkshire", "North East and Yorkshire",  
                     "England", "England", "England", "England", "England", "England"),
        IndicatorCode = c("Mort", "Mort", "Mort", "Mort", "Mort", "Mort", 
                          "Mort", "Mort", "Mort", "Mort", "Mort", "Mort"),
        MetricCategoryName = c("Persons", 1L,2L,3L,4L,5L, "Persons", 1L,2L,3L,4L,5L),
        Denominator = c(521125, 158490, 103930, 90955, 92570, 75575,
                       3044598, 612333, 601683, 624249, 623088, 587074),
        Value  = c(4965.524, 6120.439, 5328.008, 4606.034, 4257.793, 3857.097, 
                   4637.208, 5904.904, 5027.487, 4544.012, 4159.649, 3789.195), 
        LowerConfidenceLimit = c(4910.438, 6006.536, 5199.995, 4481.318, 4137.267, 3730.578, 
                     4615.471, 5847.704, 4976.304, 4496.809, 4114.345, 3745.319),
        UpperConfidenceLimit = c(5021.042, 6235.928, 5458.231, 4733.102, 4380.603, 3986.296,
                     4659.016, 5962.519, 5079.047, 4591.553, 4205.281, 3833.4),
        TimePeriodName =c("2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01",
                          "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01")
        # TimePeriodName =c("To December 2023", "To December 2023", "To December 2023", "To December 2023", "To December 2023", "To December 2023",
        #                   "To December 2023", "To December 2023", "To December 2023", "To December 2023", "To December 2023", "To December 2023")
        # TimePeriodName = c("Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", 
        #                    "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023", "Oct 2022 - Sep 2023")
      )
      
      DATAMORT$NewLow <- DATAMORT$Value - DATAMORT$LowerConfidenceLimit
      
  # AGI chart for GP Practice 
      inputfile <- "CVDP003CHOL_Dec23(GP).xlsx"
      GP_Data <- read_excel(file.path(this.dir(), "DataFiles", inputfile), sheet = "Practice")
      GP_Data <- GP_Data %>% 
        filter(!is.na(Value)) %>%
        filter(`IMD Score` != '#N/A')
      # min_imd <- min(GP_Data$`IMD Score`)
      # max_imd <- max(GP_Data$`IMD Score`)
      # GP_Data$rank <- round( (GP_Data$`IMD Score` - min_imd) / (max_imd - min_imd), digits = 3)
      GPtot <- nrow(GP_Data)
      GP_Data$rank <- rank(GP_Data$`IMD Score`)
      GP_Data$rank <- round(GP_Data$rank/ GPtot,  digits = 4)
      
  # HNY Smoking data, example for lines above 100%    
      SMOK_HNY <- data.frame(
          AreaName = c("Humber and North Yorkshire", "Humber and North Yorkshire", "Humber and North Yorkshire", "Humber and North Yorkshire", "Humber and North Yorkshire",  
                       "England", "England", "England", "England", "England"),
          IndicatorCode = c("Smoking", "Smoking", "Smoking", "Smoking", "Smoking",
                            "Smoking", "Smoking", "Smoking", "Smoking", "Smoking"),
          MetricCategoryName = c(1L,2L,3L,4L,5L, 1L,2L,3L,4L,5L),
          Denominator = c(90, 50, 25, 25, 10,
                         15790, 8485, 5435, 4035, 2755),
          Value  = c(94.44, 100.00, 100.00, 100.00, 100.00,
                     70.61, 66.41, 64.77, 66.54, 63.52), 
          TimePeriodName =c("2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01",
                            "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01", "2023-12-01")
          # TimePeriodName = c("To December 2023", "To December 2023", "To December 2023", "To December 2023", "To December 2023",
          #                    "To December 2023", "To December 2023", "To December 2023", "To December 2023",  "To December 2023")
          # TimePeriodName = c("2023 Q4", "2023 Q4", "2023 Q4", "2023 Q4", "2023 Q4", 
          #                    "2023 Q4", "2023 Q4", "2023 Q4", "2023 Q4", "2023 Q4")
        )
      SMOK_HNY$NewLow <- 0
      