## Function Name: NameWrangle

## Overview: Function for reformatting geography names within a data frame

## Arguments required:

# DF = Name of the data frame you're running through the function
# NamesColumn = Name of the column with the geography names in

NameWrangle <- function(DF, NamesColumn){
  DF %>% 
    mutate( {{NamesColumn}} := str_replace( {{NamesColumn}}, " Nhs Trust", "") ) %>%
    mutate( {{NamesColumn}} := str_replace( {{NamesColumn}}, " Integrated Care Board", " ICB") ) %>%
    mutate( {{NamesColumn}} := str_replace( {{NamesColumn}}, " Icb", " ICB") ) %>%
    mutate( {{NamesColumn}} := str_replace( {{NamesColumn}}, " Nhs", "") ) %>%
    mutate( {{NamesColumn}} := str_replace( {{NamesColumn}}, "And", "&") )
}

## Function Name: Quintiles_old

## Overview: Function for calculating SII values using the 'old' methodology (deciles/quintiles locally defined)

## Arguments required:

# ICBname = Name of the geography you want to get an output for
# DF = Name of the data frame you are running through the function

## Notes

# Various column names are hardcoded into the function. For it to work for a different data frame to the one in this analysis, you'll therefore probably need to rename various column names to align with those in the function

Quintiles_old <- function(ICBname, DF) {
  Quint <- DF %>%
    filter(AreaName == ICBname, MetricCategoryName != "Persons", TimePeriodName == latest_date)
  
  # Sort the data by the imd_decile
  sorting_columns <- list(Quint$MetricCategoryName)
  Quint <- Quint[do.call(order, sorting_columns), ]
  
  # Parameters for Sii calculation
  TotPop <- sum(Quint$Denominator)
  a_local <- Quint$Denominator / TotPop    # if populations are approx equal, each value of a is about 0.1
  sqr_a_local <- sqrt(a_local)
  
  # b is the decile/quintile midpoints, weighted by population
  b <- vector()  
  b[1] <- a_local[1] / 2
  for (i in 2:(nrow(Quint))) {
    b[i] <- a_local[i] /2 + a_local[i-1] / 2 + b[i-1]
  }
  b_sqr_a_local <- b * sqr_a_local
  
  Val_sqr_a_local <- Quint$Value * sqr_a_local  
  
  # This does the regression, least squares
  weighted_lm <- lm(Val_sqr_a_local ~ sqr_a_local + b_sqr_a_local - 1) 
  
  # Extract the SII coefficient, calculate the rii coefficient
  sii_coefficient <- coef(weighted_lm)[2]
  rii_coefficient <- (coef(weighted_lm)[1] + coef(weighted_lm)[2]) / coef(weighted_lm)[1]
  
  res <- c("b" = b, "Intercept" = unname(coef(weighted_lm)[1]), "Slope/Sii" = unname(coef(weighted_lm)[2]), 
           "s_low" = 0., "s_high" = 0., "flip" = FALSE)
  return(res)
}

## Function Name: Quintiles_new_Sii

## Overview: Function for calculating SII values using the 'new' methodology (deciles/quintiles nationally defined) with bootstrapping confidence intervals

## Arguments required:

# DF = Data frame you are analysing

# metric = Name of the metric you are analysing, which will appear in the MetricName_Col
# date = a single time period within DF, which will appear in TimePeriodName_Col
# Geog_Name = Name of the geography (e.g. ICB) you want to produce an output for
# BiggerGeog = Name of the larger geography to base the spacing of the quintiles on (e.g. England) based on the cumulative population in each

# FLIP = Question of if we want to invert the quintiles - default is false. Purpose is to ensure that a positive SII/RII value always equates to inequalities disadvantaging the most deprived
# reps = number of Confidence Interval simulations. Minimum to get CI output is 40.

# AreaName_Col = Name of the column with the geographies in
# MetricName_Col = Name of the column with the metric names in
# IMD_Col = Name of the column with IMD information. 
# Population_Col = Name of the population/denominator column
# Value_Col = Name of the column with the value in
# Lower_CI_Col - Name of the LowerCI column
# Upper_CI_Col - Name of the LowerCI column 
# TimePeriodName_Col - Name of the column containing the time periods (the filter for 'date' above)

## Notes

# The function filters out 'Persons' & 'Total' values in the IMD column (i.e. so just has IMD quintiles listed, and not an overall value). If the overall IMD value is named differently you'll need to use a different name
# If you want to get outputs for multiple metrics/geographies/time periods you'll need to embed the function within a loop. Examples of how to do this can be found in the DataWrangling.R file
# If confidence intervals are not included in your underlying data, the phe_rate and phe_proportion functions as part of the PHEindicatormethods package can be used for this purpose.
# To run the outputs from this function through the quadrant chart visualisation you'll need to add a region flag column. Examples of how to do this can be found in the DataWrangling.R file.

Quintiles_new_Sii <- function(DF, metric, date, Geog_Name, BiggerGeog, FLIP, reps,
                              AreaName_Col, MetricName_Col, IMD_Col, Population_Col, Value_Col, Lower_CI_Col, Upper_CI_Col, TimePeriodName_Col) {
  
  AreaName_Col = enquo(AreaName_Col)
  MetricName_Col = enquo(MetricName_Col)
  IMD_Col = enquo(IMD_Col)
  Population_Col = enquo(Population_Col)
  Value_Col = enquo(Value_Col)
  TimePeriodName_Col = enquo(TimePeriodName_Col)

  Quint <- DF %>%
    filter(!!AreaName_Col == Geog_Name, !!MetricName_Col == metric, !!TimePeriodName_Col == date,  !!IMD_Col != c("Persons", "Total")) 
  QuintEng <- DF %>%
    filter(!!AreaName_Col == BiggerGeog, !!MetricName_Col == metric, !!TimePeriodName_Col == date, !!IMD_Col != c("Persons", "Total"))
  
  # Parameters for Sii calculation
  TotPop <- Quint %>%  select( !!Population_Col )  %>% sum()
  a_local <- Quint %>%  select( !!Population_Col ) / TotPop 
  a_local <- a_local %>% pull()   # if Population_Cols are approx equal, each Value_Col of a is about 0.1

  TotEngPop <- QuintEng %>%  select( !!Population_Col ) %>% sum()
  a_eng <- QuintEng %>%  select( !!Population_Col ) / TotEngPop %>% as.numeric()
  
  sqr_a_local <- sqrt(a_local)
  
  # b is the decile/quintile midpoints, weighted by Population_Col
  b <- vector()
  b[1] <- a_eng[1,] / 2
  for (i in 2:(nrow(QuintEng))) {
    b[i] <- a_eng[i,] /2 + a_eng[i-1,] / 2 + b[i-1]
  }

  if (FLIP == TRUE) {
    b <- 1 - b
  }
  
  b_sqr_a_local <- b * sqr_a_local
  
  QValue_Col <- Quint %>% select( !!Value_Col )
  QValue_Col <- QValue_Col %>% pull()
  Val_sqr_a_local <- QValue_Col * sqr_a_local
  
  # This does the regression, least squares
  weighted_lm <- lm(Val_sqr_a_local ~ sqr_a_local + b_sqr_a_local - 1)
  
  # Extract the SII coefficient, calculate the rii coefficient
  sii_coefficient <- coef(weighted_lm)[2]
  rii_coefficient <- (coef(weighted_lm)[1] + coef(weighted_lm)[2]) / coef(weighted_lm)[1]
  
  #  The Conf Intervals simulations
  if( missing(Lower_CI_Col) ){
    # print("missing :) ")
    S_Low_CI <- 0.
    S_High_CI <- 0.
  } else {
      QLower <- Quint %>% select( {{Lower_CI_Col}} )
      QLower <- QLower %>% pull()
      QUpper <- Quint %>% select( {{Upper_CI_Col}} )
      QUpper <- QUpper %>% pull()
      Value_ColSE = (QUpper - QLower) / 1.96 / 2
      
      S_CI <- vector()
      RandVals <- vector()
      RandVals_Sqr_a <- vector()
      
      # runif is uniform random number generator, Value_Cols in (0, 1)
      # Qnorm is inverse Normal - from probability back to a Value_Col
      conf <- 0.95
      
      for (i in 1:reps){
        for (j in 1:nrow(Quint)){
          RandVals[j] <- qnorm(runif(1), QValue_Col[j] , Value_ColSE[j])
          RandVals_Sqr_a[j] <- RandVals[j] * sqr_a_local[j]
        }
      
        # least squares line fitting, again
        weighted_lm_CI <- lm(RandVals_Sqr_a ~ sqr_a_local + b_sqr_a_local - 1) #,  data = data)
        S_CI[i] <- coef(weighted_lm_CI)[2]
      }
      # For all simulations, sort them, and note Value_Cols towards either end
      # e.g for 10000 reps, 95% limits - this gives the 250th and 9750th Value_Cols
      S_Low_CI <- sort(S_CI)[as.integer(reps * (1 - conf) / 2)]
      S_High_CI <- sort(S_CI)[as.integer(reps * (1 + conf) / 2)]
  }
  
  res <- c("b" = b, "Intercept" = unname(coef(weighted_lm)[1]), "Slope/Sii" = unname(coef(weighted_lm)[2]),
           "s_low" = S_Low_CI, "s_high" = S_High_CI, "flip" = FLIP)
  return(res)
}

## Function Name: ICB_Dumbbells_new

## Overview: Function for producing dumbbell charts where 83.4% CIs are already calculated in the underlying data.

## Arguments required:

# DF = Data frame you are analysing

# date = a single time period within DF, which will appear in TimePeriodName_Col

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title
  
# AreaName_Col = Name of the column with the geographies in
# IMD_Col = Name of the column with IMD information. 
# Population_Col = Name of the population/denominator column
# Value_Col = Name of the column with the value in
# Lower_CI_Col = Name of the LowerCI column
# Upper_CI_Col = Name of the LowerCI column 
# TimePeriodName_Col - Name of the column containing the time periods (the filter for 'date' above)

## Notes

# For the function to work, the name of the most deprived, least deprived and overall values will need to be 1, 5 and "Persons" respectively.
# The function will produce an output for however many geographies included in the inputted dataframe. You'll therefore need to do any filtering before feeding data into the function.
# Confidence intervals should be calculated at the 83.4% not the 95% level to align with the methodologies put forward by Goldstein and Healy (1995) - https://www.jstor.org/stable/2983411

ICB_Dumbbells_new <- function(DF, date, 
                              Chart_title, Xaxis_title, Yaxis_title,
                              AreaName_Col, IMD_Col, Population_Col, Value_Col, Lower_CI_Col, Upper_CI_Col, TimePeriodName_Col) {

  Dec_filter <- c(1, 5, "Persons")
  DumbbellChart <- DF %>%
    filter(  {{IMD_Col}} %in% Dec_filter )    #c(1, 5, "Persons"))

  DumbbellChart <- DumbbellChart %>%
    filter( {{AreaName_Col}} %in% NEYAreas) %>%
    filter( {{TimePeriodName_Col}} == date) %>%
    select( {{AreaName_Col}}, {{IMD_Col}}, {{Value_Col}}, {{Lower_CI_Col}}, {{Upper_CI_Col}}, {{TimePeriodName_Col}})

  maxdbc <- DumbbellChart %>% select({{Value_Col}}) %>% max() + 1
  mindbc <- DumbbellChart %>% select({{Value_Col}}) %>% min() - 1
  
  DbC_wide <- pivot_wider(DumbbellChart, names_from = {{IMD_Col}}, values_from = c({{Value_Col}}, {{Lower_CI_Col}}, {{Upper_CI_Col}} ))

  LCIwide <- ""
  UCIwide <- ""
  Valwide <- ""
  for (i in 1:length(Dec_filter)){
    LCIwide[i] <- paste0(as_label(enquo(Lower_CI_Col)), "_", Dec_filter[i])
    UCIwide[i] <- paste0(as_label(enquo(Upper_CI_Col)), "_", Dec_filter[i])
    Valwide[i] <- paste0(as_label(enquo(Value_Col)), "_", Dec_filter[i])
  }
  AreaName_Col <- as_label(enquo(AreaName_Col))
  DbC_wide$flag <- ifelse(DbC_wide[[LCIwide[1]]] > DbC_wide[[UCIwide[2]]], "circle", ifelse(DbC_wide[[UCIwide[1]]] < DbC_wide[[LCIwide[2]]], "circle", "circle-open"))
  
  plot_ly(DbC_wide, color = I("gray80")) %>%
    add_segments(x = DbC_wide[[Valwide[1]]], xend = DbC_wide[[Valwide[2]]], y = DbC_wide[[AreaName_Col]], yend = DbC_wide[[1]], showlegend = FALSE) %>%
    add_markers(x = DbC_wide[[Valwide[1]]], y = DbC_wide[[AreaName_Col]], name = "Most", color = I("blue"), marker = list(symbol = ~flag), size = 8) %>%
    add_markers(x = DbC_wide[[Valwide[2]]], y = DbC_wide[[AreaName_Col]], name = "Least", color = I("orange"), marker = list(symbol = ~flag), size = 8) %>%
    add_markers(x = DbC_wide[[Valwide[3]]], y = DbC_wide[[AreaName_Col]], name = "Total", color = I("grey"), size = 8) %>%
    layout(
      title = "Chart_title", ,
      xaxis = list(title = "Xaxis_title", range = c(mindbc, maxdbc), ticksuffix = "%"),
      yaxis = list(titleYaxis_titlecategoryorder = "category descending"),
      margin = list(l = 65)
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
      "pan2d", "autoScale2d", "zoom2d"
    )) # ,"resetScale2d", "hoverClosestCartesian"))
}

## Function Name: SiiTimeSeries_new

## Overview: Function for producing a time series chart showing changes in ICB values over time

## Arguments required:

# DF = Data frame you are analysing

# metric = Name of the metric you are producing an output for (will appear in the MetricName_Col)

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# MetricName_Col = Name of the column with metric descriptions in
# Name_Col = Name of the column with the geographies in
# TimePeriodName_Col = Name of the column containing the time periods
# Sii_Col = Name of the column with the value in

## Notes

# The function is set up for England values to be labelled as "England". If a different name is used (e.g. "ENG") this will need renaming to "England"
# It is also just set up for percentage values (e.g. % is prefixed on the ends of values)
# The function is intended for producing an output with the 4 NEY ICBs in with the ICB colours agreed by the NE&Y analytics team applied. It will produce an output for additional geographies, but won't specify the colours for these.

SiiTimeSeries_new <- function(DF, metric, 
                              Chart_title, Xaxis_title, Yaxis_title,
                              MetricName_Col, Name_Col, TimePeriodName_Col, Sii_Col){

  quo_Metric <- enquo(MetricName_Col)
  quo_Name <- enquo(Name_Col)
  quo_TPN <- enquo(TimePeriodName_Col)
  quo_sii <- enquo(Sii_Col)
  
  DF <- DF %>%
    filter(!!quo_Metric == metric) 
  
  plot_ly(data = DF) %>%
    add_trace(data = DF %>% filter(!!quo_Name != "England"),
              x = quo_TPN, y = quo_sii, type = "scatter", mode = "lines",
              color = quo_Name, colors = c("#000000", "#AE2573", "#41B6E6", "#003087", "#ED8B00"),
              showlegend = TRUE, hoverinfo = "text",  line = list(dash = "solid"),
              customdata = quo_Name,
              hovertemplate = paste("%{customdata} <br>", "Time Period (to): %{x}<br>", "Value: %{y:,.1f}%<br><extra></extra>")
              
                  )%>%
    add_trace(data = DF %>% filter(!!quo_Name == "England"),
              x = quo_TPN, y = quo_sii, type = "scatter", mode = "lines",
              color = quo_Name, colors = c("#000000", "#AE2573", "#41B6E6", "#003087", "#ED8B00"),
              showlegend = TRUE, hoverinfo = "text",  line = list(dash = "dash"),
              customdata = quo_Name,
              hovertemplate = paste("%{customdata} <br>", "Time Period (to): %{x}<br>", "Value: %{y:,.1f}%<br><extra></extra>")
    )%>%
    layout(
      title = Chart_title,
      xaxis = list(
        title = list(text = Xaxis_title)
      ),
      yaxis = list(title = list(text = Yaxis_title), ticksuffix = "%"),
      legend = list(
        orientation = "h",
        x = 0.5, y = -0.4, xanchor = "center", hovermode = "x unified"
      )
    )
}

## Function Name: QuintileChart_new

## Overview: Function for producing a time series chart showing IMD quintile and overall values for a geography

## Arguments required:

# DF = Data frame you are analysing

# Geog_Name = Name of the geography you are producing an output for (will appear in the AreaName_Col)

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# AreaName_Col = Name of the column with the geography in
# IMD_Col = Name of the column with IMD information in
# Value_Col = Name of the column with values in
# TimePeriodName_Col = Name of the column with time periods in

## Notes

# The function is set up for overall values to be described as "Persons". If another name is used (e.g. "Total") this will need to be renamed.

QuintileChart_new <- function(DF, Geog_Name,
                              Chart_title, Xaxis_title, Yaxis_title,
                              AreaName_Col, IMD_Col, Value_Col, TimePeriodName_Col){

  AreaName_Col = enquo(AreaName_Col)
  TimePeriodName_Col <- enquo(TimePeriodName_Col)
  Value_Col <- enquo(Value_Col)
  IMD_Col <- enquo(IMD_Col)
  
  plot_ly(
      data = DF %>% filter( !!AreaName_Col == Geog_Name, !!IMD_Col != "Persons"), 
      x = TimePeriodName_Col, y = Value_Col, color = IMD_Col, type = "scatter", mode = "lines",
      legendgroup = IMD_Col,
      colors = c("#0000FF", "#4020BF", "#804080", "#BF6040", "#FF8000"),
      hoverinfo = "text", customdata = IMD_Col,
      hovertemplate = paste("Quintile: %{customdata}<br>", "%{x}<br>", "%{y:,.1f}%<br><extra></extra>")
      # text = ~ paste( IMD_Col, "</br></br>", TimePeriodName_Col, "</br>", Value_Col, "%")
    )  %>%
      add_trace(
        data = DF %>% filter( !!AreaName_Col == Geog_Name, !!IMD_Col == "Persons"),
        x = TimePeriodName_Col, y = Value_Col, name = "Total", type = "scatter", mode = "lines", 
        line = list(color = "grey", dash = "dash"), legendgroup = IMD_Col, inherit = FALSE
      ) %>%
      style(visible = "legendonly", traces = c(2, 3, 4)) %>%
      layout(
        title = Chart_title,
        xaxis = list(
          title = list(text = Xaxis_title)#,
        ),
        yaxis = list(
          title = list(text = Yaxis_title, font = list(size = 12), y = 1.3), # range = c(50, 80),
          ticksuffix = "%"
        ),
        margin = list(l = 65)
      ) %>%
      config(modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
        "pan2d", "autoScale2d", "zoom2d"
      ))
}

## Function Name: Regression_plot_new

## Overview: Function for producing the regression chart visualising how SII values have been calculated

## Arguments required:

# DF = Data frame you are analysing

# metric = Name of the metric you are analysing (will appear in the MetricName_Col)
# date = Name of the date you're analysing the metric for (will appear in the TimePeriodName_Col)
# Geog_Name = Name of the geography you are producing an output for (will appear in the AreaName_Col)
# midpoints = Will be calculated as part of the Quintiles_new_Sii function. For examples of how to declare the outputs from the Quintiles_new_Sii as the 'midpoint' variable see the QMD output file.

# Chart_title = Name to be given for the chart title
# Yaxis_title = Name to be given for the y axis title

# AreaName_Col = Name of the column with the geography in
# MetricName_Col = Name of the column with the metric name in
# IMD_Col = Name of the column with IMD information in
# Population_Col = Name of the column with the population values in
# Value_Col = Name of the column with values in
# NewLow_Col = Name of the column with the lowerCI value in
# TimePeriodName_Col = Name of the column with time periods in

## Notes

# This function needs to be used in combination with the Quintiles_new_Sii function to get the midpoints information. See the QMD file for an example of how to do this.
# LowerCI column should be the difference between the value and lower CI (i.e. Value - LowerCI) not the overall lowerCI value

Regression_plot_new <- function(DF, metric, date, Geog_Name, midpoints,
                                Chart_title, Yaxis_title,
                                AreaName_Col, MetricName_Col, IMD_Col, Population_Col, Value_Col, NewLow_Col, TimePeriodName_Col) {
  
  Quint <- DF %>%
    filter( {{AreaName_Col}} == Geog_Name, {{MetricName_Col}} == metric, {{IMD_Col}} != "Persons") %>%
    filter( {{TimePeriodName_Col}} == date )
  MaxQpop <- Quint %>% select( {{Population_Col}} ) %>% max()
  Quint$NewNum <- Quint %>% select( {{Population_Col}} ) %>% pull()
  Quint$NewNum <- Quint$NewNum * 40 / MaxQpop

  Quint$b <- midpoints[1:5]
  
  MinQval <- Quint %>% select( {{Value_Col}} ) %>% min()
  MaxQval <- Quint %>% select( {{Value_Col}} ) %>% max()
  if (midpoints[7] > 0){
    miny <- min(MinQval, midpoints[6])
    maxy <- max(MaxQval, midpoints[6] + midpoints[7])
    miny <- miny - 0.1 * midpoints[7]
    maxy <- maxy + 0.1 * midpoints[7]
    eqntext <- paste("y = ", round(midpoints[6], digits = 3), " + ", round(midpoints[7], digits = 3), " x")
  } else {
    miny <- min(MinQval, midpoints[6] + midpoints[7])
    maxy <- max(MaxQval, midpoints[6])
    miny <- miny + 0.1 * midpoints[7]
    maxy <- maxy - 0.1 * midpoints[7]
    eqntext <- paste("y = ", round(midpoints[6], digits = 3), " - ", abs(round(midpoints[7], digits = 3)), " x")
  }
  
  xaxisname <- ifelse(midpoints[10] == FALSE, "Deprivation quintile, most deprived to the left", 
                      "Deprivation quintile, most deprived to the right")

  TimePeriodName_Col <- enquo(TimePeriodName_Col)  
  MetricName_Col <- enquo(MetricName_Col)
  AreaName_Col <- enquo(AreaName_Col)
  IMD_Col <- enquo(IMD_Col)
  Population_Col <- enquo(Population_Col)
  Value_Col <- enquo(Value_Col)
  NewLow_Col = enquo(NewLow_Col)

  plot_ly(Quint , x = ~b) %>%
    add_trace(
      y = Value_Col, type = "scatter", mode = "markers",
      marker = list(size = ~NewNum, sizemode = "diameter"),
      hoverinfo = "text", 
      customdata = Population_Col,
      hovertemplate = paste(Geog_Name, "<br>", "Value: %{y:,.1f}<br>", "Total Patients: %{customdata}<br><extra></extra>") ,
      error_y = list(array = NewLow_Col, color = "#bcbcbc", width = 10)
    )  %>%
    add_trace(
      x = c(0,1), y =  c(midpoints[6], midpoints[6] + midpoints[7]), name = "Regression Fit",
      type = "scatter", mode = "lines", alpha = 1
    ) %>%
    layout(
      title = Chart_title,
      xaxis = list(title = list(text = xaxisname)),
      yaxis = list(
        title = list(
          text = Yaxis_title,
          font = list(size = 12), y = 1.2
        ),
        range = c(miny, maxy)  #,              ticksuffix = "%"
      ),
      annotations = list(text = eqntext, x = 0.53, y = maxy - 1, showarrow = FALSE),
      legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center", hovermode = "x unified")
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", 
      "pan2d", "autoScale2d", "zoom2d"
    )) 
  
  }

## Function Name: QuadrantChart_new_labels

## Overview: Function for producing the quadrant chart combining overall and SII values

## Arguments required:

# DF = Data frame with the overall values in. Should include all ICBs and England, with NE&Y ICBs containing both the most recent and historic values for comparison on the final output
# Sii4allICB - Data frame containing all the latest SII values (Eng and ICB)
# Sii4NEYICB - Data frame containing historic SII values (just for NEY ICBs)

# date_early = Name of the date to show historic values for NEY ICBs
# date_late = Latest date to show values for on the chart
# IMD_Total_Name = Name given to overall values (will appear in the IMD_Col)
# ENG Name = Name given to England in the dataset (will appear in the AreaName_Col)

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# Top_Left = Name to be given to the top-left quadrant
# Top_Right = Name to be given to the top-right quadrant
# Bottom_Right = Name to be given to the bottom-right quadrant
# Bottom_Left = Name to be given to the bottom-left quadrant

# Sii_Col = Name of the columns with the SII values in
# Region_Flag_Col = Name of the column flagging NEY ICBs in
# AreaName_Col = Name of the column with geography names in
# MetricName_Col = Name of the column with the metric name in
# IMD_Col = Name of the column with IMD information in
# Value_Col = Name of the column with values in
# TimePeriodName_Col = Name of the column with time periods in
# NewLow_Col = Name of the column with the lowerCI value in

## Notes

# This function needs to be used in combination with the Quintiles_new_Sii function to get the SII outputs. See the DataWrangling and QMD files for worked examples
# The LowerCI column will need to be the difference between the Value and Lower CI (i.e. Value - LowerCI) not the LowerCI value by itself.
# You'll need to make sure all 3 data frames have the exact same column names (e.g. the AreaName_Col in all 3 has the same name as declared in the function) otherwise it won't work.
# The function assumes that the first data frame has a column which breaks the values down by IMD Quintile, and then filters this on the overall value. If you're underlying data doesn't have an IMD column in, you'll need to add this in.

QuadrantChart_new_labels <- function(DF, Sii4allICB, Sii4NEYICB, metric, date_early, date_late, IMD_Total_Name, ENG_Name,
                                     Chart_title, Xaxis_title, Yaxis_title,
                                     Top_Left, Top_Right, Bottom_Left, Bottom_Right, 
                                     Sii_Col, Region_Flag_Col,
                                     AreaName_Col, MetricName_Col, IMD_Col, Value_Col, TimePeriodName_Col, NewLow_Col){
  
  Sii_Col <- enquo(Sii_Col)
  Region_Flag_Col <- enquo(Region_Flag_Col)
  TimePeriodName_Col <- enquo(TimePeriodName_Col)
  MetricName_Col <- enquo(MetricName_Col)
  AreaName_Col <- enquo(AreaName_Col)
  IMD_Col <- enquo(IMD_Col)
  Value_Col <- enquo(Value_Col)
  NewLow_Col = enquo(NewLow_Col)
  
  DF_BAR_Per <- DF %>% filter( !!MetricName_Col == metric, {{IMD_Col}} == IMD_Total_Name) %>%
    filter({{TimePeriodName_Col}} == date_late) %>%
    select(Name = {{AreaName_Col}}, {{Value_Col}}, Lower_CI_Col = {{NewLow_Col}})
  
  Sii4allICB <- Sii4allICB %>% rename( Name := {{ AreaName_Col}})
  Sii4NEYICB <- Sii4NEYICB %>% rename( Name := {{ AreaName_Col}})
  
  Sii4allICB_per <- merge(x = Sii4allICB, y = DF_BAR_Per, by = "Name") #, by.y = "AreaName_Col" )
  
  Sii4allICB_per <- Sii4allICB_per %>% 
    filter(!!MetricName_Col == metric)
  
  DF_ALL_per <- DF %>%
    filter(!!MetricName_Col == metric, {{IMD_Col}} == IMD_Total_Name, {{TimePeriodName_Col}} == date_early) %>%
    select(Name = {{AreaName_Col}}, {{Value_Col}}, Lower_CI_Col = {{NewLow_Col}})
  
  Sii4NEYICB_first <- Sii4NEYICB %>%
    filter({{TimePeriodName_Col}} == date_early,!!MetricName_Col == metric)  # NOTE that ICB_Sii is created by DataWrangling - so name is fixed!!
  
  Sii4NEYICB_first <- merge(x = Sii4NEYICB_first, y = DF_ALL_per, by = "Name") #, by.y = AreaName_Col )
  
  QuadDF <- rbind(Sii4NEYICB_first, Sii4allICB_per)
  
  QuadDF <- QuadDF %>%
    mutate(R_Flag := paste( {{Region_Flag_Col}}, {{TimePeriodName_Col}}, sep = " ") )
  England_Performance <- QuadDF %>%
    filter(Name == ENG_Name, {{TimePeriodName_Col}} == date_late)
  
  QuadDF <- QuadDF %>% filter(Name != ENG_Name)
  
  eng_per <- England_Performance %>% select({{Value_Col}}) %>% pull()
  
  mycolors <- c("#AE2573", "#41B6E6", "#003087", "#ED8B00", "#AE2573", "#41B6E6", "#003087", "#ED8B00", "#cccccc")

  col_names <- QuadDF %>% filter({{TimePeriodName_Col}} == date_late, Region_Flag != "Other") %>% select( Name )
  names(mycolors) <- c(paste(col_names$Name[1], date_late, sep = " "), paste(col_names$Name[2], date_late, sep = " "), paste(col_names$Name[3], date_late, sep = " "), 
                       paste(col_names$Name[4], date_late, sep = " "), paste(col_names$Name[1], date_early, sep = " "), paste(col_names$Name[2], date_early, sep = " "), 
                       paste(col_names$Name[3], date_early, sep = " "), paste(col_names$Name[4], date_early, sep = " "), paste("Other", date_late, sep = " ") )
  
  # Calculate dimensions of 'box' for the chart
  MinX1 <- QuadDF %>%
    filter({{TimePeriodName_Col}} == date_late | !{{Region_Flag_Col}} == "Other") %>%
    pull(Sii) %>%
    min()
  MaxX1 <- QuadDF %>%
    filter({{TimePeriodName_Col}} == date_late | !{{Region_Flag_Col}} == "Other") %>%
    pull(Sii) %>%
    max()
  MinY1 <- QuadDF %>%
    filter({{TimePeriodName_Col}} == date_late | !{{Region_Flag_Col}} == "Other") %>%
    pull({{Value_Col}}) %>%
    min()
  MaxY1 <- QuadDF %>%
    filter({{TimePeriodName_Col}} == date_late | !{{Region_Flag_Col}} == "Other") %>%
    pull({{Value_Col}}) %>%
    max()
  MinX <- MinX1 - 0.1 * (MaxX1 - MinX1)
  MaxX <- MaxX1 + 0.1 * (MaxX1 - MinX1)
  MinY <- MinY1 - 0.1 * (MaxY1 - MinY1)
  MaxY <- MaxY1 + 0.1 * (MaxY1 - MinY1)
  
  plot_ly(colors = mycolors) %>%
    add_trace(
      data = QuadDF %>% filter({{TimePeriodName_Col}} == date_late, {{Region_Flag_Col}} != "Other"),
      x = Sii_Col, y = Value_Col, color = ~R_Flag, type = "scatter", mode = "markers", size = 10,
      
      hoverinfo = "text", customdata = ~Name,
      hovertemplate = paste("%{customdata} <br>", "Date: ", date_late, "<br>",
                            "Metric: %{y}<br>", "Sii Value: %{x}<br><extra></extra>"),     #{x:,.1f}%
      
      error_x = list(array = ~lower, color = "#dcdcdc", width = 10),   #bcbcbc
      error_y = list(array = ~Lower_CI_Col, color = "#dcdcdc", width = 10)
    )  %>%
    add_trace(
      data = QuadDF %>% filter({{TimePeriodName_Col}} == date_late, {{Region_Flag_Col}} == "Other"),
      x = Sii_Col, y = Value_Col, color = ~R_Flag, type = "scatter", mode = "markers", size = 10,
      hoverinfo = "text", customdata = ~Name,
      hovertemplate = paste("%{customdata} <br>", "Date: ", date_late, "<br>",
                            "Metric: %{y}<br>", "Sii Value: %{x:,.1f}%<br><extra></extra>")

    ) %>%
    add_trace(
      data = QuadDF %>% filter({{TimePeriodName_Col}} == date_early & !{{Region_Flag_Col}} == "Other"),
      type = "scatter", mode = "markers", size = 10, alpha = 0.5,
      x = Sii_Col, y = Value_Col, color = ~R_Flag, type = "scatter", mode = "markers",
      hoverinfo = "text", customdata = ~Name,
      hovertemplate = paste("%{customdata} <br>", "Date: ", date_early, "<br>",
                            "Metric: %{y}<br>", "Sii Value: %{x:,.1f}%<br><extra></extra>")

    ) %>%
    add_annotations(
      x = MinX, y = MinY, xanchor = "left", yanchor = "bottom",
      text = ~ paste("<b>", Bottom_Left), showarrow = F
    ) %>%
    add_annotations(
      x = MinX, y = MaxY, xanchor = "left", yanchor = "top",
      text = ~ paste("<b>", Top_Left), showarrow = F
    ) %>%
    add_annotations(
      x = MaxX, y = MinY, xanchor = "right", yanchor = "bottom",
      text = ~ paste("<b>", Bottom_Right), showarrow = F
    ) %>%
    add_annotations(
      x = MaxX, y = MaxY, xanchor = "right", yanchor = "top",
      text = ~ paste("<b>",  Top_Right), showarrow = F
    ) %>%
    add_lines(
      x = c(MinX, MaxX),
      y = c( eng_per, eng_per ),  #c(England_Performance$Value_Col, England_Performance$Value_Col),
      line = list(color = "black"), showlegend = F, inherit = FALSE,
      hoverinfo = "text", text = ~ paste(
        "England Performance: ", eng_per,  #England_Performance$`Value_Col`,
        "</br></br>", "Date: ", date_late
      )
    ) %>%
    add_lines(
      x = c(England_Performance$Sii, England_Performance$Sii),
      y = c(MinY, MaxY),
      line = list(color = "black"), showlegend = F, inherit = FALSE,
      hoverinfo = "text", text = ~ paste(
        "England SII Value_Col: ", England_Performance$`Sii`,
        "</br></br>", "Date: ", date_late
      )
    ) %>%
    layout(
      title = Chart_title, # title = Title,
      xaxis = list(title = Xaxis_title, zeroline = FALSE, ticksuffix = "%"),
      yaxis = list(title = Yaxis_title, ticksuffix = "%"),
      legend = list(orientation = "h", x = 0.5, y = -0.4, xanchor = "center")
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
      "pan2d", "autoScale2d", "zoom2d"
    )) # ,"resetScale2d", "hoverClosestCartesian"))
}

## Function Name: Wgt_Scatter_plot_AGI_new

## Overview: Function for producing the AGI scatter plot chart

## Arguments required:

# Scatter = Name of dataframe

# geog = Name of the ICB you are producing an output for (as appears in the ICB_Col). You can produce an output for all geographies (e.g. all 4 NEY ICBs) by declaring this to be 'All'

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# AreaName_Col = Name of the column with geography names in (e.g. GP Practices)
# Denominator_Col = Name of the column with denominators in
# Value_Col = Name of the column with values in
# ICB_Col = Name of the column which shows ICB that AreaName_Col geographies map to
# SubICB_Col = Name of the column which shows the sub-ICB locations that AreaName_Col geographies map to
# irank_Col = Name of the column ranking geographies in the AreaName_Col from low to high. For an example see the DataWrangling.R file.

## Notes

# See the DataWrangling.R file for an example of creating the irank_Col, and the QMD file for then using in practice

Wgt_Scatter_plot_AGI_new <- function(Scatter, geog,
                                     Chart_title, Xaxis_title, Yaxis_title,
                                     AreaName_Col, Denominator_Col, Value_Col, ICB_Col, SubICB_Col, irank_Col) {
  
  miny <- Scatter %>% select( {{Value_Col}} ) %>% min()
  maxy <- Scatter %>% select( {{Value_Col}} ) %>% max()
  
  if (geog == "All"){
    Scat_Regr <- Scatter
  } else {
    Scat_Regr <- Scatter %>%
      filter({{ICB_Col}}  == geog)
  }
  Denom <- as_label(enquo(Denominator_Col))
  
  Scat_Regr$case_wts <- importance_weights(Scat_Regr[[Denom]]) #%>% select({{Denominator_Col}}) %>% pull() %>% importance_weights()  #importance_weights(Scat_Regr$Denominator_Col)
  
  # Scat_Regr$NewNum <- 1 + round(40 * Scat_Regr$Denominator_Col / max(Scat_Regr$Denominator_Col), digits = 0)
  MaxQpop <- Scat_Regr %>% select( {{Denom}} ) %>% max()
  Scat_Regr$NewNum <- Scat_Regr %>% select( {{Denom}} ) %>% pull()
  Scat_Regr$NewNum <- Scat_Regr$NewNum * 40 / MaxQpop
  
  #  This bit is a fudge:
  #  I cannot work out how to code irank_Col in the lines  "add_formula(Value_Col ~ irank_Col ) %>%",  "colnames(xdf) <- c("rank") "
  #  so am going to try 
  Scat_Regr$Irank <- Scat_Regr %>% select( {{irank_Col}} ) %>% pull()
  Scat_Regr$IValue_Col <- Scat_Regr %>% select( {{Value_Col}} ) %>% pull()
  # fudge ends
  
  # linear modelling bit
  lm_spec <- linear_reg() %>%
    set_engine("lm")
  
  lm_wflow <- workflow() %>%
    add_case_weights(case_wts) %>%
    #  add_recipe(base_recipe) %>%
    add_formula(IValue_Col ~ Irank ) %>%  #add_formula(Value_Col ~ irank_Col ) %>%
    add_model(lm_spec)
  
  lm_model <- fit(lm_wflow, data = Scat_Regr) # , case_weights = Scat_Regr$case_wts)
  
  x_range <- seq(0.00, 1.00, length.out = 11)
  x_range <- matrix(x_range, nrow = 11, ncol = 1)
  # something about plot and trace having same dimension to make hoverinfo work!!
  xdf <- data.frame(x_range)
  colnames(xdf) <- c("Irank")
  
  ydf <- round( lm_model %>% predict(xdf), digits = 2)
  
  colnames(ydf) <- c("Val")
  xy <- data.frame(xdf, ydf)
  
  Coeffs <- tidy(lm_model)
  
  eqntext <- paste("y = ", round(Coeffs$estimate[1], digits = 3), " + ", round(Coeffs$estimate[2], digits = 3), " x")
  # eqntext <- paste("y = ", round(lm_model$fit$Coefficients[[1]], digits=3), " + ", round(lm_model$fit$Coefficients[[2]], digits=3), " x")
  
  Scat_Regr <- Scat_Regr %>% arrange( {{SubICB_Col}} )
  
  quo_Sub <- enquo(SubICB_Col)
  quo_Val <- enquo(Value_Col)
  quo_rank <- enquo(irank_Col)
  quo_Name <- enquo(AreaName_Col)
  quo_Den <- enquo(Denominator_Col)

  fig <- plot_ly(Scat_Regr, x = quo_rank) %>%
    add_trace(
      y = quo_Val, type = "scatter", mode = "markers", color = quo_Sub,
      marker = list(size = ~ Scat_Regr$NewNum, sizemode = "diameter"),
      hoverinfo = "text", text = ~ paste(
        Scat_Regr[[as_label(quo_Name)]], "</br></br>",
        "Value_Col:", Scat_Regr[[as_label(quo_Val)]], "</br>", "Total Patients:", Scat_Regr[[as_label(quo_Den)]]
      )
    )  %>%
    add_trace(
      data = xy, x = ~`Irank`, y = ~Val, name = "Regression Fit",
      type = "scatter", mode = "lines", alpha = 1, line = list(color = "orange")
    ) %>%
    layout(
           title = Chart_title, # title = "% CVD patients treated to threshold for cholesterol vs IMD Quintile",
           xaxis = list(title = list(text = Xaxis_title )),
           yaxis = list(
             title = list(
               text = Yaxis_title, #' CVDP007CHOL',
               font = list(size = 12), y = 1.2
             ), # range = c(miny, maxy),
             ticksuffix = "%"
           ),
           annotations = list(text = eqntext, x = 0.53, y = maxy - 0.10, showarrow = FALSE),
           legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center", hovermode = "x unified")
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
      "pan2d", "autoScale2d", "zoom2d"
    )) # ,"resetScale2d", "hoverClosestCartesian"))
  
  return(fig)
}

## Function Name: SiiBar_new

## Overview: Function for producing a bar chart with SII values on

## Arguments required:

# DF = Name of dataframe

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# Name_Col = Name of the column with geography names in
# Sii_Col = Name of the column with values in
# lower_col = Name of the column with lower CI values in
# Region_Flag_Col = Column flagging NEY ICBs in.

## Notes

# LowerCI value needs to be the difference between the Value and LowerCI (i.e. Value - LowerCI) not the LowerCI by itself

SiiBar_new <- function(DF,
                       Chart_title, Xaxis_title, Yaxis_title,
                       Name_Col, Sii_Col, lower_Col, Region_Flag_Col){
  
  quo_Name <- enquo(Name_Col)
  quo_Sii <- enquo(Sii_Col)
  quo_lower <- enquo(lower_Col)
  quo_Region_Flag = enquo(Region_Flag_Col)

  plot_ly(DF, x = quo_Name, y = quo_Sii, type = "bar", color = quo_Region_Flag,
          colors = c("#AE2573", "#41B6E6", "#003087", "#ED8B00", "#cccccc"),
          hoverinfo = "text", 
          hovertemplate = paste("%{x}<br> %{y:, .1f}%<br><extra></extra>"),
          # text = ~ paste(DF[[as_label(Name)]], "</br></br>", DF[[as_label(Sii)]], "%"),     WHY DIDN'T THIS WORK?
          error_y = list(array = quo_lower, color = "#bcbcbc")
  ) %>%
    layout(title = Chart_title, 
           xaxis = list(
             title = list(text = Xaxis_title),
             categoryorder = "total ascending",
             showticklabels = FALSE    #This has removed the labels under the bars :)
           ),
           yaxis = list(title = list(text = Yaxis_title, font = list(size = 12), y = 1.2) , ticksuffix = "%"),
           legend = list(
             orientation = "h",
             x = 0.5, y = -0.4, xanchor = "center"
           )
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
      "pan2d", "autoScale2d", "zoom2d"
    )) # ,"resetScale2d", "hoverClosestCartesian")) ## Removing the icon
}

## Function Name: ICB_Dumbbells_new_834

## Overview: Function for producing dumbbell charts, which calculates 83.4% CIs as part of the function

## Arguments required:

# DF = Data frame you are analysing

# date = a single time period within DF, which will appear in TimePeriodName_Col

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# AreaName_Col = Name of the column with the geographies in
# IMD_Col = Name of the column with IMD information. 
# Population_Col = Name of the population/denominator column
# Value_Col = Name of the column with the value in
# Numerator_Col = Name of the numerator column
# Denominator_Col = Name of the denominator column (to check if this is needed - could we just use 'Population_Col' instead?)
# Lower_CI_Col - Name of the LowerCI column
# Upper_CI_Col - Name of the LowerCI column 
# TimePeriodName_Col - Name of the column containing the time periods (the filter for 'date' above)

## Notes

# For the function to work, the name of the most deprived, least deprived and overall values will need to be 1, 5 and "Persons" respectively
# The function will produce an output for however many geographies included in the inputted dataframe. You'll therefore need to do any filtering on the required output before feeding data into the function.
# The function is just set up for percentage indicators, with Wilson CIs calculated. It is not set up for rates indicators, where the Byars method should be used to calculate CI values.

ICB_Dumbbells_new_834 <- function(DF, date, 
                                  Chart_title, Xaxis_title, Yaxis_title,
                                  AreaName_Col, IMD_Col, Population_Col, Value_Col, Numerator_Col, Denominator_Col, TimePeriodName_Col) {
  
  Dec_filter <- c(1, 5, "Persons")
  DumbbellChart <- DF %>%
    filter(  {{IMD_Col}} %in% Dec_filter ) 
  
  DumbbellChart <- DumbbellChart %>%
    filter( {{AreaName_Col}} %in% NEYAreas) %>%
    filter( {{TimePeriodName_Col}} == date) %>%
    select( {{AreaName_Col}}, {{IMD_Col}}, {{Value_Col}}, {{Numerator_Col}}, {{Denominator_Col}}, {{TimePeriodName_Col}})

  Numer <- as_label(enquo(Numerator_Col))
  Denom <- as_label(enquo(Denominator_Col))
  
  # confidence limits
  wcL <- c(1:dim(DumbbellChart)[1])
  wcU <- c(1:dim(DumbbellChart)[1])
  for (i in 1:dim(DumbbellChart)[1]) {
    tmp <- wilson.ci(x = DumbbellChart[[Numer]][i], n = DumbbellChart[[Denom]][i], 0.834)
    # wc[i] <- 100 * (tmp[2] - tmp[1]) / 4
    wcL[i] <- tmp[1]
    wcU[i] <- tmp[2]
    next
  }
  DumbbellChart <- cbind(DumbbellChart, Lower_CI = wcL, Upper_CI = wcU) %>%
    select(-{{Numerator_Col}}, -{{Denominator_Col}})

  maxdbc <- DumbbellChart %>% select({{Value_Col}}) %>% max() + 1
  mindbc <- DumbbellChart %>% select({{Value_Col}}) %>% min() - 1
  
  DbC_wide <- pivot_wider(DumbbellChart, names_from = {{IMD_Col}}, values_from = c({{Value_Col}}, Lower_CI, Upper_CI ))

  LCIwide <- ""
  UCIwide <- ""
  Valwide <- ""
  for (i in 1:length(Dec_filter)){
    LCIwide[i] <- paste0("Lower_CI", "_", Dec_filter[i])
    UCIwide[i] <- paste0("Upper_CI", "_", Dec_filter[i])
    Valwide[i] <- paste0(as_label(enquo(Value_Col)), "_", Dec_filter[i])
  }
  AreaName_Col <- as_label(enquo(AreaName_Col))
  DbC_wide$flag <- ifelse(DbC_wide[[LCIwide[1]]] > DbC_wide[[UCIwide[2]]], "circle", ifelse(DbC_wide[[UCIwide[1]]] < DbC_wide[[LCIwide[2]]], "circle", "circle-open"))

  plot_ly(DbC_wide, color = I("gray80")) %>%
    add_segments(x = DbC_wide[[Valwide[1]]], xend = DbC_wide[[Valwide[2]]], y = DbC_wide[[AreaName_Col]], yend = DbC_wide[[1]], showlegend = FALSE) %>%
    add_markers(x = DbC_wide[[Valwide[1]]], y = DbC_wide[[AreaName_Col]], name = "Most", color = I("blue"), marker = list(symbol = ~flag), size = 8) %>%
    add_markers(x = DbC_wide[[Valwide[2]]], y = DbC_wide[[AreaName_Col]], name = "Least", color = I("orange"), marker = list(symbol = ~flag), size = 8) %>%
    add_markers(x = DbC_wide[[Valwide[3]]], y = DbC_wide[[AreaName_Col]], name = "Total", color = I("grey"), size = 8) %>%
    layout(
      title = Chart_title, 
      xaxis = list(title = Xaxis_title, range = c(mindbc, maxdbc), ticksuffix = "%"),
      yaxis = list(title = Yaxis_title, categoryorder = "category descending"),
      margin = list(l = 65)
    ) %>%
    config(modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
      "pan2d", "autoScale2d", "zoom2d"
    )) # ,"resetScale2d", "hoverClosestCartesian"))
}

## Function Name: QuintileChart_4new

## Overview: Function for producing a time series chart showing IMD quintile and overall values for 4 geographies on individual charts

## Arguments required:

# DF = Data frame you are analysing

# metric = Name of the metric you are producing an output for (will appear in the MetricName_Col)
# Areas = Name of the geographies you are producing an output for (will appear in the AreaName_Col)
# Total_IMD_Name = Name given to overall IMD values (will appear in the IMD_Col)

# Chart_title = Name to be given for the chart title
# Xaxis_title = Name to be given for the x axis title
# Yaxis_title = Name to be given for the y axis title

# AreaName_Col = Name of the column with the geography in
# MetricName_Col = Name of the column with the metric name in
# IMD_Col = Name of the column with IMD information in
# Value_Col = Name of the column with values in
# TimePeriodName_Col = Name of the column with time periods in

## Notes

# Areas should be declared as a string in the following format for NEY. England needs to appear as the first name in it: c("England","Humber and North Yorkshire","North East and North Cumbria","South Yorkshire","West Yorkshire")

QuintileChart_4new <- function(DF, metric, Areas, Total_IMD_Name,
                               Chart_title, Xaxis_title, Yaxis_title,
                               AreaName_Col, MetricName_Col, IMD_Col, Value_Col, TimePeriodName_Col){
  
  AreaName_Col = enquo(AreaName_Col)
  MetricName_Col = enquo(MetricName_Col)
  TimePeriodName_Col <- enquo(TimePeriodName_Col)
  Value_Col <- enquo(Value_Col)
  IMD_Col <- enquo(IMD_Col)
  
  DF <- DF %>% filter( !!MetricName_Col == metric )
  
  tmp <- DF %>% filter( !!AreaName_Col %in% Areas, !!IMD_Col != Total_IMD_Name) %>% 
    select(!!Value_Col) %>% pull()
  miny <- min(tmp)
  maxy <- max(tmp)
  
  fig <- vector("list", length = 4)
  
  for (i in 1:4){  
    fig[[i]] <- plot_ly(
      data = DF %>% filter( !!AreaName_Col == Areas[i+1], !!IMD_Col != Total_IMD_Name), 
      x = TimePeriodName_Col, y = Value_Col, color = IMD_Col, type = "scatter", mode = "lines",
      legendgroup = IMD_Col,
      colors = c("#0000FF", "#4020BF", "#804080", "#BF6040", "#FF8000"),
      hoverinfo = "text", customdata = IMD_Col,
      hovertemplate = paste("Quintile: %{customdata}<br>", "%{x}<br>", "%{y:,.1f}%<br><extra></extra>")
      # text = ~ paste( IMD_Col, "</br></br>", TimePeriodName_Col, "</br>", Value_Col, "%")
    )  %>%
      add_trace(
        data = DF %>% filter( !!AreaName_Col == Areas[i+1], !!IMD_Col == Total_IMD_Name),
        x = TimePeriodName_Col, y = Value_Col, name = "Total", type = "scatter", mode = "lines", 
        line = list(color = "black", dash = "dash"), legendgroup = IMD_Col, inherit = FALSE
      ) %>%
      style(visible = "legendonly", traces = c(2, 3, 4)) %>%
      layout(
        # title = Chart_title,
        xaxis = list(
          title = list(text = "") #Xaxis_title)
        ),
        yaxis = list(
          title = list(text = Yaxis_title, font = list(size = 12), y = 1.3), range = c(miny, maxy),
          ticksuffix = "%"
        ),
        margin = list(l = 65)
      ) %>%
      config(modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "select2d", "lasso2d", # "toImage",
        "pan2d", "autoScale2d", "zoom2d"
      ))
  }
  
  fig_all4 <- subplot(style(fig[[1]], showlegend = F), style(fig[[2]], showlegend = F), 
                      style(fig[[3]], showlegend = F), fig[[4]], nrows = 2, margin=0.08, shareY = TRUE)
  
  annotations = list( 
    list(x = 0.20, y = 1.0, text = "HNY", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
    list(x = 0.80, y = 1.0, text = "NENC", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
    list(x = 0.20, y = 0.4, text = "SY", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
    list(x = 0.80, y = 0.4, text = "WY", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE) )
  
  fig_all4 <- fig_all4 %>% layout(annotations = annotations,
                                  title = Chart_title,
                                  legend = list(orientation = "h", x=0.5, y = -0.2, xanchor = "center")) 
  
  fig_all4
}

# Function which uses the FunnelPlotR function to create a funnel plot

Funnel <- function(){
  
  NEYFunnelTheme <- funnel_clean() + theme(axis.title.y = element_text(size = 8), axis.title.x = element_text(size = 8), legend.position = 'right', legend.text = element_text(size = 6))
  
  FunnelData <- DATA_BAR %>%
    filter(MetricCategoryName == 1, AreaName != "England")
  FunnelData$Numerator <- FunnelData$Value * FunnelData$Denominator / 100
  
  Highlight = c("NHS Humber and North Yorkshire ICB", "NHS North East and North Cumbria ICB", "NHS South Yorkshire ICB", "NHS West Yorkshire ICB")
  
  funnel_details <- funnel_plot(.data = FunnelData, numerator = Numerator, denominator = Denominator, 
                                group = AreaName, title = "Funnel", draw_unadjusted = FALSE,
                                draw_adjusted = TRUE, label = "highlight", data_type="PR", highlight=Highlight, limit=95,
                                multiplier = 100, y_label = "Value", x_label = "Pop", theme = NEYFunnelTheme) 
}

# Function for creating chloropleth map (for the maps showing variation in LSOA IMD Scores)
Chloropleth_Map <- function(ICB_Name, lng, lat, zoom){
  LSOA_ScatterICB <- LSOA_Scatter %>%
    filter(Region2 == ICB_Name)
  LSOA_shapefile_HNY <- merge(LSOA_shapefile_wgs84, LSOA_ScatterICB, by.x = 'LSOA11NM', by.y = 'LSOA11NM')
  
  bins_agi <- quantile(LSOA_Scatter$`IMD rank`, probs=c(0., 0.2, 0.4, 0.6, 0.8, 1.) )[1:6]  #was ScatterICB
  pal_agi <- colorBin("YlGnBu", domain = LSOA_shapefile_HNY$`IMD rank`, bins = bins_agi)  
  
  agi_chloropleth_map <- leaflet() %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = LSOA_shapefile_HNY,
                label = LSOA_shapefile_HNY$LSOA11NM, #when you hover over something what pop ups
                popup = paste0("<h3>", LSOA_shapefile_HNY$LSOA11NM, "</h3>", #what you see when you hover
                               "Rank: ", sprintf(LSOA_shapefile_HNY$`IMD rank`, fmt = '%#.4f')   ),
                fillColor = ~pal_agi(`IMD rank`),   #  `AGI (SLOPE OF LINE)`),
                color = "lightgrey", weight = 2,  smoothFactor = 0.5,  opacity = 1.0,  fillOpacity = 0.70,
                highlightOptions = highlightOptions(color = "white",  weight = 2,  bringToFront = FALSE))%>%
    addLegend("topright", pal = pal_agi, values = bins_agi,
              title = paste0("IMD rank Quintiles:" ),
              #              "ISR ..." , "<br>","Source: HP: ", as.character(Sys.Date(),format = "%d/%m/%Y")),
              opacity = 0.70
    )
  
  agi_chloropleth_map
}
