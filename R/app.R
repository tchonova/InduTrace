

library(shiny); library(shinycssloaders); library(shinyFeedback)
library(dplyr); library(readr); library(tidyr); library(lubridate); library(ggplot2)
library(forcats); library(tibble); library(patchwork)


##################
# InduTrace_0.0.1
#################


InduTrace <- function(...) {

options(shiny.maxRequestSize=500*1024^2)

prioritise_profiles <- function(x, profile_id = profile_id, dates = dates, intensity = intensity,
                                max_int = 7, freq_det = 70, cum_int = 5) {

  x <- x %>% select(dates = {{dates}}, profile_id = {{profile_id}}, intensity = {{intensity}})

  sampled_days <- x %>% distinct(dates) %>% nrow()

  priority_profiles <- list()

  priority_profiles$overview <- x %>%
    filter(!is.na(intensity)) %>%
    group_by(profile_id) %>%
    summarise(int_max = max(round(log10(intensity), 2)),
              det_freq = n(),
              int_cum = sum(intensity)) %>%
    ungroup() %>%
    mutate(det_freq_perc = round(det_freq*100/sampled_days));

  priority_profiles$overview <- priority_profiles$overview %>%
    arrange(desc(int_cum)) %>%
    mutate(int_cum_perc = (row_number()/nrow(priority_profiles$overview))*100) %>%
    select(-int_cum, -det_freq)

  priority_profiles$min_one_criteria <- suppressMessages({
    priority_profiles$overview %>%
      filter(int_max >= max_int | det_freq_perc >= freq_det | int_cum_perc <= cum_int) %>%
      distinct(profile_id) %>%
      left_join(x) %>% select(profile_id, dates, intensity)
  })

  suppressMessages({
    priority_profiles$overview <- priority_profiles$overview %>%
      filter(int_max >= max_int | det_freq_perc >= freq_det | int_cum_perc <= cum_int) %>%
      mutate(maximum_intensity = ifelse(int_max >= max_int, TRUE, FALSE),
             detection_frequency = ifelse(det_freq_perc >= freq_det, TRUE, FALSE),
             cumulative_intensity = ifelse(int_cum_perc <= cum_int, TRUE, FALSE)) %>%
      select(profile_id, maximum_intensity, detection_frequency,
             cumulative_intensity) %>%
      arrange(cumulative_intensity)
  })

  return(priority_profiles)
}


compute_int_spr <- function(x, profile_id = profile_id, intensity = intensity) {

  x <- x %>% select(profile_id = {{profile_id}}, intensity = {{intensity}})

  int_spread <- x %>%
    group_by(profile_id) %>%
    summarise(min = min(intensity),
              max_intensity = max(intensity),
              q05 = quantile(intensity, probs = 0.05, na.rm = TRUE),
              q95 = quantile(intensity, probs = 0.95, na.rm = TRUE),
              intensity_spread = q95/q05) %>%
    ungroup() %>%
    select(profile_id, intensity_spread)
  return(int_spread)
}


detect_breaks <- function(x, profile_id = profile_id, dates = dates, intensity = intensity) {
  x <- x %>% select(dates = {{dates}}, profile_id = {{profile_id}}, intensity = {{intensity}})

  count_length <- function(x) {
    n <- length(x)
    res <- numeric(n)
    current_run_length <- 0

    for (i in seq_along(x)) {
      if (!is.na(x[i]) && x[i] == 1) {
        current_run_length <- current_run_length + 1
      } else {
        current_run_length <- 0
      }
      res[i] <- current_run_length
    }

    for (i in seq_along(x)) {
      if (res[i] > 0) {
        run_length <- res[i]
        for (j in seq(i - run_length + 1, i)) {
          res[j] <- run_length
        }
      }
    }

    return(res)
  }

  detect_br <- x %>%
    filter(!is.na(intensity)) %>%
    arrange(profile_id, as.Date(dates)) %>%
    group_by(profile_id) %>%
    mutate(
      limit_break = min(intensity) + diff(quantile(intensity, probs = c(0.001, 0.999), na.rm = TRUE)) / 30,
      below_limit_break = ifelse(intensity < limit_break, 1, NA),
      break_length = count_length(below_limit_break)
    ) %>%
    distinct(profile_id, break_length) %>%
    slice_max(break_length) %>%
    ungroup()

  return(detect_br)
}

calculate_dist <- function(x, profile_id = profile_id, dates = dates,
                           intensity = intensity, method = "euclidean") {

  library(dplyr); library(tidyr);

  x <- x %>% select(dates = {{dates}}, profile_id = {{profile_id}}, intensity = {{intensity}})

  x1 <- x %>%
    arrange(as.Date(dates)) %>%
    pivot_wider(id_cols = dates, names_from = profile_id,
                values_from = intensity, values_fill = 0)
  x1 <- as.data.frame(x1)
  rownames(x1) <- x1[,1 , drop = TRUE] ; x1 <- x1[,-1]
  x1 <- x1 %>% as.matrix()
  dt_sc <- scale(x1, center = FALSE, scale = apply(x1, 2, sd, na.rm = TRUE))
  dist_mat <- dist(t(dt_sc), method = method)

  return(dist_mat)

}

show_similar <- function(x, dist_mat, profile_id = profile_id, dates = dates,
                         intensity = intensity, sel_prof, nr = 3) {

  x <- x %>% select(dates = {{dates}}, profile_id = {{profile_id}}, intensity = {{intensity}})

  prof <- as.matrix(dist_mat)
  prof <- enframe(prof[sel_prof,]) %>% slice_min(value, n = nr+1) %>% select(prof_id = name, distance = value);

  p1 <- x %>%
    filter(profile_id %in% prof$prof_id) %>%

    left_join(x) %>%
    ggplot(aes(x = as.Date(dates), y = intensity)) +
    geom_line(size = 0.5) + geom_point(size = 0.5) +
    facet_wrap(vars(factor(profile_id, levels = prof$prof_id)), scales = "free", ncol = 1) +
    labs(x = "Date", y = "") +
    theme_bw()

  p2 <- x %>%
    filter(profile_id %in% prof$prof_id) %>%
    left_join(x) %>%
    ggplot(aes(x = as.Date(dates), y = intensity, color = profile_id), si) +
    geom_line(size = 0.5) + geom_point(size =0.5) +
    labs(title = sel_prof, x = "Date", y = "intensity") +
    theme_bw()

  p3 <- x %>%
    filter(profile_id %in% prof$prof_id) %>%
    left_join(x) %>%
    ggplot(aes(x = as.Date(dates), y = log10(intensity), color = profile_id), si) +
    geom_line(size = 0.5) + geom_point(size =0.5) +
    labs(title = sel_prof, x = "Date", y = "log10(intensity)") +
    theme_bw()

  plt <- p1 + (p2 / p3) + plot_layout(ncol = 2, widths = c(1, 1)) +
    plot_annotation(tag_levels = 'A')

  plt <- plt + plot_layout(heights = c(3, 1))
  res <- list(plt = plt, prof = prof)

  return(res)

}

############ UI ############

ui <- fluidPage(
  h1("InduTrace - Industrial Tracer"),
  shinyFeedback::useShinyFeedback(),

  tabsetPanel(
    ####################################################################################################################
    # Read and Prioritise data
    ###################################################################################################################
    tabPanel("Read and prioritise data",
             h2("Upload"),
             actionLink("helpLink0", "read me"),
             fileInput(inputId = "upload", label = "", buttonLabel = "Upload data...", accept = ".csv", multiple = FALSE),
             conditionalPanel("output.panelStatus1",
                              h2("Prioritise"),
                              actionLink("helpLink1", "read me"),
                              br(),
                              h3("Criteria"),
                              numericInput("MI", "Maximum intensity (log10)", value = 7, min = 0, max = 100, step = 0.1),
                              numericInput("CI", "Cumulative intensity (%)", value = 5, min = 0, max = 100, step = 1),
                              numericInput("DF", "Detection frequency (%)", value = 70, min = 0, max = 100, step = 5),
                              actionButton("runFilter", "Prioritise"),
                              br("")),
             conditionalPanel("output.panelStatus2",
                              h2("Output"),
                              strong("Number of profiles"),
                              verbatimTextOutput("filtProfNr"),
                              strong("Percent from raw data"),
                              verbatimTextOutput("filtProfPerc"),
                              br(""),
                              dataTableOutput("filtTable"))
    ),
    ####################################################################################################################
    # Classify source
    ###################################################################################################################
    tabPanel("Classify source",
             h2("Classify"),
             actionLink("helpLink2", "read me"),
             br(),
             numericInput("IntThreshold", "Threshold of intensity spread", value = 10, min = 0, max = 1000),
             numericInput("BreakLength", "Length of break (days)", value = 7, min = 1, max = 100),
             actionButton("runSourceIndic", "Classify"),
             br(""),
             conditionalPanel("output.panelStatus3",
                              h2("Output"),
                              plotOutput("ProfsPerSource", width = "400px", height = "400px") %>% withSpinner(color="#0dc5c1"),
                              br(""),
                              dataTableOutput("emissionTable"))
    ),
    ####################################################################################################################
    # Detect similar profiles
    ###################################################################################################################
    tabPanel("Detect similar profiles",
             h2("Settings"),
             actionLink("helpLink3", "read me"),
             br(),
             selectInput("SelDist", "Method",
                         choices = list("method 1" = "euclidean",
                                        "method 2" = "minkowski",
                                        "method 3" = "manhattan",
                                        "method 4" = "canberra",
                                        "method 5" = "maximum")),
             actionButton("CalcDist", "Calculate"),
             textOutput("DistText") %>% withSpinner(color="#0dc5c1"),
             conditionalPanel("output.panelStatus4",
                              h2("Select and plot"),
                              actionLink("helpLink4", "read me"),
                              br(),
                              selectInput("OrderBy", "Order",
                                          choices = list("cumulative intensity", "maximum intensity", "detection frequency")),
                              selectInput("ProfList", "Choose", choices = NULL),
                              numericInput("NrFriends", "Number of other profiles", min = 1, max = 20, value = 3, step = 1),
                              plotOutput("ShowFriends", width = "1000px", height = "800px") %>% withSpinner(color="#0dc5c1"),
                              br("")),
    ),
    ####################################################################################################################
    # About
    ###################################################################################################################
    tabPanel("About",
             h3("About"),
             p("InduTrace (Industrial Tracer) is a Shiny application designed to help users prioritize
               and classify contaminants based on their temporal patterns and identify contaminants with
               similar patterns in time-profiles from long-term, high-frequency river monitoring data. The
               application offers a user-friendly interface with a familiar dashboard layout."),
             p("The tool helps users to:"),
             tags$ul(
               tags$li("select and rank priority compounds based on intensity and detection frequency;"),
               tags$li("classify compounds based on their temporal behaviour;"),
               tags$li("detect and visualize compounds with similar temporal patterns.")),
             p("For more details, please refer to Chonova et al. (2024)."),
             tags$hr(),
             h3("Disclaimer"),
             p("This application was developed based on the characteristics of the RUES Data 2.0 project dataset.
               The accuracy and reliability of the results depend on the quality and characteristics of the datasets used.
               We strongly recommend a testing phase to optimize algorithms and settings before broader application."),
             p("We accept no responsibility for misuse by third parties. Users are fully responsible for any
               conclusions or decisions based on the outputs."),
             tags$hr(),
             h3("License and Terms of Use"),
             p("This application is open-source and licensed under the CC BY 4.0 License. You are free to use, modify, and distribute the app
               under the terms of this license."),
             tags$hr(),
             h3("Citation"),
             tags$ul(
               # tags$li(tags$span("Zenodo citation", style = "color: red;")),
               tags$li("Chonova T., Honti M., Loos M., Ruppe S., Langlois I., Griesshaber D., Fenner K., Singer H., 2024. Unveiling
                        industrial emissions in a large European river: Insights from data mining of high-frequency measurements. ",
                       tags$i("Water Research."),
                       ""),
             ),
             tags$hr(),
             h3("Acknowledgment"),
             p("This work builds on the developments of the RUES Data 2.0 project, funded by the Swiss Federal Office for the Environment,
               the Swiss Agency for Environment and Energy in Basel and the German Baden Wuerttemberg State Institute for the Environment,
               Measurements and Nature Conservation. We thank everyone who provided technical support and inspiration for this work."),
             tags$hr(),
             h3("Developer information"),
             tags$ul(
               tags$li("Developed by: Dr. Teofana Chonova"),
               tags$li("Role: Scientist at Eawag Research Institute"),
               tags$li("Contact: teofana.chonova@gmail.com")),
             tags$hr()
    ),
  ),
)


############ Server ############

server <- function(input, output, session) {

  options(shiny.maxRequestSize=400*1024^2)

  ###################################################################################################################
  # Read and prioritise data
  ###################################################################################################################

  observeEvent(req(input$upload), {

    dat <<- read_csv(input$upload$datapath) %>%
      mutate(year = as.numeric(year), month = as.numeric(month),
             day = as.numeric(day), intensity = as.numeric(intensity))%>%
      unite(dates, sep = "-", year, month, day) %>%
      mutate(dates = ymd(dates))

    output$panelStatus1 <- reactive({
      TRUE
    })
    outputOptions(output, "panelStatus1", suspendWhenHidden = FALSE)

    print(dat)
  })

  observeEvent(input$helpLink0, {
    showModal(modalDialog(
      title = "Help",
      HTML("
      <p>The input data must be provided as a long-format data frame with the following columns:</p>
      <ul>
        <li><strong>profile_id</strong>: A unique identifier for each profile.</li>
        <li><strong>intensity</strong>: A numeric column representing the intensity for each profile on a specific date.</li>
        <li><strong>year</strong>: A numeric column identifying the sampling year.</li>
        <li><strong>month</strong>: A numeric column identifying the sampling month.</li>
        <li><strong>day</strong>: A numeric column identifying the sampling day.</li>
      </ul>
      <p>Ensure that all column names are in lowercase, as specified, and numeric columns contain only numbers. Any missing values
      or zero detections should be removed from the dataset. Prior to upload, perform data cleaning to remove low-quality
      profiles and profiles with rare detections.</p>
    "),
      easyClose = TRUE
    ))
  })

  observeEvent(input$helpLink1, {
    showModal(modalDialog(
      title = "Help",
      "Profiles are prioritised based on three criteria: maximum intensity (the lowest acceptable maximum intensity
      that a profile should have), cumulative intensity (the percentage of profiles with the highest cumulative
      intensity) and detection frequency (the minimum detection frequency of a profile over time, expressed
      as a percentage). Profiles are selected if they meet at least one of these criteria.",
      easyClose = TRUE
    ))
  })

  dat_filt <- eventReactive(input$runFilter, {

    dat_update <- dat %>%
      filter(!is.na(intensity), intensity > 0, !is.na(dates)) %>%
      distinct(profile_id, dates, .keep_all = TRUE) %>%
      mutate(profile_id = as.character(profile_id)) %>%
      group_by(profile_id) %>%
      mutate(det_freq = n()) %>%
      select(profile_id, intensity, dates) %>%
      ungroup()
    dat_filtered <- prioritise_profiles(dat_update, max_int = input$MI, freq_det = input$DF,
                                        cum_int = input$CI)$min_one_criteria


    summary <- dat_filtered %>%
      group_by(profile_id) %>%
      summarise("cumulative intensity" = sum(intensity),
                "maximum intensity" = max(round(log10(intensity), 2)),
                "detection frequency" = n()) %>%
      ungroup() %>%
      left_join(prioritise_profiles(dat_update,
                                    max_int = input$MI,
                                    freq_det = input$DF,
                                    cum_int = input$CI)$overview %>%
                  select(profile_id))

    res <- list(dat_filtered = dat_filtered, summary = summary)

    return(res)
  })

  observeEvent(input$runFilter, {
    output$panelStatus2 <- reactive({
      TRUE
    })
    outputOptions(output, "panelStatus2", suspendWhenHidden = FALSE)
  })

  output$filtProfNr <- renderText({
    nrow(dat_filt()$summary)
  })


  observeEvent(input$runFilter, {
    output$filtProfPerc <- renderText({
      profile_number <- dat %>% distinct(profile_id) %>% nrow()
      x <- nrow(dat_filt()$summary)/profile_number*100; round(x,1)
    })
  })


  output$filtTable <- renderDataTable({
    dat_filt()$summary
  })


  ####################################################################################################################
  # Classify source
  ###################################################################################################################

  # help
  observeEvent(input$helpLink2, {
    showModal(modalDialog(
      title = "Help",
      "Source classification is based on two indicators: intensity spread and activity break (Chonova
      et al., 2024, Water Research). Profiles are classified as originating from irregular emission sources
      if they meet either of the following conditions: the intensity spread exceeds the defined
      threshold or there is at least one activity break period, with a minimum duration equal to
      the specified number of consecutive measurements.",
      easyClose = TRUE
    ))
  })

  summary_classes <- eventReactive(input$runSourceIndic, {

    int_spread <- compute_int_spr(dat_filt()$dat_filtered)
    breaks_detect <- detect_breaks(dat_filt()$dat_filtered)

    classes <- breaks_detect %>%
      left_join(int_spread) %>%
      mutate(emission = ifelse((intensity_spread > input$IntThreshold |
                                  break_length >= input$BreakLength),
                               "irregular", "constant")) %>%
      left_join(dat_filt()$summary)
    return(classes);

  })

  observeEvent(input$runSourceIndic, {
    output$panelStatus3 <- reactive({
      TRUE
    })
    outputOptions(output, "panelStatus3", suspendWhenHidden = FALSE)
  })

  output$ProfsPerSource <- renderPlot({
    summary_classes() %>%
      ggplot() +
      geom_bar(aes(emission)) +
      labs(x = "source", y = "number of profiles") +
      theme_bw()}, res = 96
  )

  output$emissionTable <- renderDataTable({
    summary_classes() %>%
      select(profile_id, emission, break_length, intensity_spread) %>%
      arrange(desc(emission))
  })


  ####################################################################################################################
  # Detect similar profiles
  ###################################################################################################################

  observeEvent(input$helpLink3, {
    showModal(modalDialog(
      title = "Help",
      "A distance/dissimilarity matrix is computed to assess similarity between profiles.
      Several methods are available: (1) Euclidean, (2) Minkowski, (3) Manhattan, (4) Canberra,
          (5) Maximum." ,
      easyClose = TRUE
    ))
  })

  calc_dist <- eventReactive(input$CalcDist, {


    dat_sel <- dat_filt()$dat_filtered %>%
      left_join(summary_classes()) %>%
      select(profile_id, dates, intensity)

    distances <- calculate_dist(dat_sel, method = input$SelDist)
    return(distances)
  })


  output$DistText <- renderText({
    nrow(calc_dist())
  })

  observeEvent(input$helpLink4, {
    showModal(modalDialog(
      title = "Help",
      "Profiles can be ordered based on a selected priority criterion. The default
      profile displayed in the input box is the highest-ranked profile according to the selected
      criterion. A different profile can be selected either from the dropdown menu, where profiles
      are listed in order of importance, or by entering a specific profile ID directly into the
      input box. Additionally, the number of profiles with high similarity to display in the figures
      below can be specified.",
      easyClose = TRUE
    ))
  })

  observeEvent(input$CalcDist, {
    output$panelStatus4 <- reactive({
      TRUE
    })
    outputOptions(output, "panelStatus4", suspendWhenHidden = FALSE)
  })

  ChoicesProf <- reactive({

    sel <- summary_classes()
    sel <- sel[order(sel[[input$OrderBy]], decreasing = TRUE), ] %>%
      pull(profile_id)
    return(sel)
  })


  UpdatedChoices <- reactive({
    ChoicesProf()
  })


  observe({
    updateSelectInput(session, "ProfList", choices = UpdatedChoices())
  })


  output$ShowFriends <- renderPlot({
    frnds <- show_similar(x = dat_filt()$dat_filtered, dist_mat = calc_dist(),
                          sel_prof = input$ProfList, nr = input$NrFriends)
    frnds$plt
  }, res = 96
  )

}

####################################################################################################################
# Run App
###################################################################################################################

shinyApp(ui = ui, server = server,
         onStart = function() {
           cat("Doing application setup\n")

           onStop(function() {
             rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
           })
         })

}