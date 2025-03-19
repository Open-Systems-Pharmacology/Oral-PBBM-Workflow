# Required packages
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(gdata) # Make sure the correct perl.exe (Strawberry) is called in read.xls()
library(openxlsx)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  useShinyjs(), 
  
  # Add OSP logo to navigation bar & define greyed out button for surface pH calculation
  tags$head(
    tags$script(type="text/javascript", src = "code.js"),
    tags$style(HTML("
      .btn-disabled {
        opacity: 0.65;
        cursor: not-allowed;
        background-color: #cccccc !important;
        border-color: #cccccc !important;
      }
      .input-disabled {
      opacity: 0.65;
      background-color: #f5f5f5 !important;
      cursor: not-allowed;
    }
  "))
  ),
  
  #Navbar structure for UI
  navbarPage(title = strong("OSP Solubility Toolbox"), inverse=T, theme = shinytheme("cerulean"),
             
             # API Properties tab
             tabPanel("API properties", fluid = TRUE, icon = icon("pills"),
                      
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          h3(p("API properties")),
                          textInput("API", label = "Active Pharmaceutical Ingredient (API):", value = "Name"),
                          fluidRow(
                            column(3,autonumericInput("LogP", "LogP:", min=-10, max=10, value=0, width = "100px",
                                                      decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")),
                            column(3, offset = 1,autonumericInput("MW.API", "Mol. Wt.:", min=0, max=1e4, value=250, width = "100px",
                                                                  decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")),
                            column(3, offset = 1,selectInput("MW.unit", "Unit:", choices = c("g/mol"), selected = "g/mol", width = "1000px"))
                          ),br(),
                          
                          # Input: Select a file ----
                          h3(p("Observed Solubilities")),
                          fileInput("obs.file", "Choose observed solubility data file (.xlsx)",
                                    multiple = FALSE,
                                    accept = c(".xlsx")),
                          radioButtons("disp", "Display",
                                       choices = c(Aqueous ="obs.aq.raw",Biorelevant ="obs.br.raw"),
                                       selected = "obs.aq.raw",
                                       inline = TRUE)
                        ),  
                        mainPanel(tableOutput("observed"))
                      )
             ),
             
             # SG-estimation tool for aqueous solubility
             tabPanel("Aqueous Solubility", fluid = TRUE, icon = icon("flask"),
                      sidebarPanel(
                        # Select API ionizable groups and pKa values
                        h3(p("API ionizable groups")),
                        fluidRow(
                          column(3,
                                 selectInput(inputId = "CT0",label = "pKa1 type :",choices = c("None" = 0, "Acid" = -1, "Base" = 1),
                                             selected = "None", width = "100px"),
                                 autonumericInput("pKa0", "Value (1):", min=0, max=14, value=0, width = "100px",
                                                  decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,
                                 selectInput(inputId = "CT1",label = "pKa2 type :",choices = c("None" = 0, "Acid" = -1, "Base" = 1),
                                             selected = "None", width = "100px"),
                                 autonumericInput("pKa1", "Value (2):", min=0, max=14, value=0, width = "100px",
                                                  decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,
                                 selectInput(inputId = "CT2",label = "pKa3 type :",choices = c("None" = 0, "Acid" = -1, "Base" = 1),
                                             selected = "None", width = "100px"),
                                 autonumericInput("pKa2", "Value (3):", min=0, max=14, value=0, width = "100px",
                                                  decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          )
                        ),br(),
                        
                        # Select reference solubility
                        h3(p("Reference solubility for fitting")),
                        fluidRow(
                          column(3,autonumericInput("ref_pH", "Ref. pH:", min=0, max=14, value=7, width = "100px",
                                                    decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,autonumericInput("ref_sol", "Solubility:", min=0, max=1e6, value=0, width = "130px",
                                                                decimalPlaces = 7, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,selectInput(inputId = "ref_unit",label = "Unit:", choices = c("mg/ml","ug/ml","ng/ml","pg/ml",
                                                                                                             "g/l","mg/l","ug/l","ng/l","pg/l",
                                                                                                             "M","mM","uM","nM",
                                                                                                             "mg/dl","ug/dl","ng/dl","pg/dl"), 
                                                           selected = "mg/ml", width = "100px")
                          )
                        ),br(),
                        
                        fluidRow(  
                          column(7,autonumericInput("SG_I", "Solubility Gain per charge:", min=1, max=1e6, value=1000, width = "130px",
                                                    decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          )
                        ),
                        
                        # Optimization Scale option
                        fluidRow(
                          column(6,
                                 radioButtons("sg_fit_scale", "Optimization Scale:",
                                              choices = list("Linear" = "linear", "Logarithmic" = "log"),
                                              selected = "linear")
                          )
                        ),
                        
                        fluidRow(  
                          column(3, actionButton("SGfit", "Estimate SG")),
                          column(3, offset = 0.5, downloadButton("downloadAQpred", "Export tables")),
                          column(3, offset = 0.5, downloadButton("downloadAQreport", "Export plots"))
                        ), br()
                        
                      ),
                      mainPanel(h4(p("Upload observed solubility data and input the API properties, then press 'Estimate SG' in order to fit SG to your data")),
                                plotOutput(outputId = "InitialAqSol"),
                                tableOutput("SG.tab"),
                                h5(p(textOutput("CopyPasteSG"))),
                                uiOutput("PlotRes"),
                                plotOutput(outputId = "SGpHRes")
                      )
             ),
             
             # Bile salt partitioning estimation tool for biorelevant solubility
             tabPanel("Biorelevant Solubility", fluid = TRUE, icon = icon("vial-virus"),
                      sidebarPanel(
                        h3(p("Initial micelle partitioning coefficients")),
                        numericInput("IE.Kn", "Log Km:w (neutral)", value=0, width = "200px"),
                        em(textOutput("Kn.IE.txt")),br(),
                        numericInput("IE.Ki", "Log Km:w (ionized)", value=0, width = "200px"),
                        em(textOutput("Ki.IE.txt")),br(),
                        fluidRow(
                          column(3,autonumericInput("int_pH", "pH (intrinsic):", min=0, max=14, value=0, width = "100px",
                                                    decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,autonumericInput("int_sol", "Intrinsic Sol.:", min=0, max=1e4, value=0, width = "130px",
                                                                decimalPlaces = 8, digitGroupSeparator = ",", decimalCharacter = ".")
                          ),
                          column(3, offset = 1,selectInput(inputId = "int_unit",label = "Unit:", choices = c("mg/ml","ug/ml","ng/ml","pg/ml",
                                                                                                             "g/l","mg/l","ug/l","ng/l","pg/l",
                                                                                                             "M","mM","uM","nM",
                                                                                                             "mg/dl","ug/dl","ng/dl","pg/dl"), 
                                                           selected = "mg/ml", width = "100px")
                          )
                        ),
                        
                        em(textOutput("IntS.txt")),br(),
                        
                        # Optimization Scale option:
                        fluidRow(
                          column(6,
                                 radioButtons("fit_scale", "Optimization Scale:",
                                              choices = list("Linear" = "linear", "Logarithmic" = "log"),
                                              selected = "linear")
                          )
                        ),
                        
                        fluidRow(  
                          column(3, actionButton("Kfit", "Estimate Km:w")),
                          column(3, offset = 0.5, downloadButton("downloadBRpred", "Export tables")),
                          column(3, offset = 0.5, downloadButton("downloadBRreport", "Export plots"))
                        ), br()
                      ),
                      
                      mainPanel(
                        # Header
                        h4(p("Upload observed solubility data and input the API properties, then press 'Estimate Km:w' to fit the micelle partitioning coefficient(s)")),
                        
                        # Predicted vs Observed plot
                        tags$h5(style = "font-size: 16px; font-weight: bold; color: black; font-family: Arial, sans-serif; margin-top: 15px; 
                                margin-bottom: 10px; text-align: center", "Predicted vs Observed Biorelevant Solubility"),
                        plotOutput(outputId = "biorelevant_plot", height = "500px"),
                        
                        # Vertical space to prevent overlap
                        tags$div(style = "margin-top: 20px;"),
                        
                        # Km:w estimation results and residual plots
                        h4(""),
                        tableOutput("K.tab"),
                        h5(p(textOutput("CopyPasteK"))),
                        fluidRow(
                          column(3, uiOutput("updateK"))
                        ),
                        plotOutput(outputId = "KpHRes")
                      )
             ),
             
             # Surface/microenvironmental pH estimation tab
             tabPanel("Surface pH", fluid = TRUE, icon = icon("flask-vial"),
                      sidebarPanel(
                        div(
                          style = "background-color: #fff3cd; padding: 10px; margin-bottom: 15px; border: 1px solid #ffeeba; border-radius: 4px;",
                          h4(style = "color: #856404; margin: 0;", "Preliminary Version"),
                          p(style = "color: #856404; margin: 10px 0 0 0;", 
                            "This functionality is currently limited to monoprotic acids and bases only.")
                        ),
                        h3("Hydrogen and hydroxide"),
                        fluidRow(
                          column(6, numericInput("DH", "Diffusion coefficient of hydrogen [cm²/s]:", value = 1E-4, 
                                                 min = 0, step = 1e-5)),
                          column(6, numericInput("DOH", "Diffusion coefficient of hydroxide [cm²/s]:", value = 6.3E-5, 
                                                 min = 0, step = 1e-6))
                        ),
                        numericInput("Kw", "Water dissociation constant [M²]:", value = 2.6E-14, 
                                     min = 0, step = 1e-15),
                        
                        h3("Buffer"),
                        fluidRow(
                          column(6, numericInput("C_buffer", "Buffer concentration [M]:", value = 5E-2,
                                                 min = 0, step = 1e-3)),
                          column(6, selectInput("BT", "Buffer type:", 
                                                choices = list("Base" = 1, "Acid" = -1, "Unbuffered" = 0),
                                                selected = 1))
                        ),
                        fluidRow(
                          column(6, selectInput("BcarB", "Bicarbonate buffer:", 
                                                choices = list("No" = 0),
                                                selected = 0)),
                          column(6, numericInput("pKaYH", "Buffer pKa:", value = 14,
                                                 min = 0, max = 14, step = 0.1))
                        ),
                        fluidRow(
                          column(6, numericInput("DYH", "DYH [cm²/s]:", value = 1.8E-5,
                                                 min = 0, step = 1e-6)),
                          column(6, numericInput("DX", "DX [cm²/s]:", value = 1.2E-5,
                                                 min = 0, step = 1e-6))
                        ),
                        
                        h3("API properties"),
                        numericInput("DAPI", "Diffusion coefficient of API [cm²/s]:", value = 0,
                                     min = 0, step = 1e-7),
                        em(textOutput("DAPI_suggested")), br(),
                        
                        numericInput("S0", "Intrinsic solubility [M]:", value = 0,
                                     min = 0, step = 1e-7),
                        em(textOutput("S0_suggested")), br(),
                        
                        fluidRow(
                          column(6, selectInput("Salt", "Salt:", 
                                                choices = list("No" = 0),
                                                selected = 0)),
                          column(6, numericInput("Ksp", "Salt solubility product [M²]:", value = 0,
                                                 min = 0, step = 0.01))
                        ),
                        
                        actionButton("calc_pH", "Calculate Surface pH", class = "btn-primary")
                      ),
                      mainPanel(
                        conditionalPanel(
                          condition = "input.calc_pH > 0 && $('html').hasClass('shiny-busy')",
                          div(
                            style = "color: #00008B; text-align: center; padding: 20px;",
                            h3(icon("spinner", class = "fa-spin"), "Surface pH is being calculated...")
                          )
                        ),
                        plotOutput("surface_pH_plot")
                      )
             )
  )
)

#### Server for solubility fittings ----
server <- function(input, output, session) {
  
  calc_status <- reactiveVal(TRUE)
  vals <- reactiveValues(p1=NULL,p2=NULL,p3=NULL,p4=NULL,p5=NULL,output_df = NULL)
  
  # First observe() wrapper for data import, aqueous solubility, and biorelevant solubility calculations:
  observe({
    API      <- input$API
    MW.API   <- input$MW.API
    MW.unit  <- input$MW.unit
    LogP     <- input$LogP
    
    CT0      <- as.numeric(input$CT0)
    pKa0     <- input$pKa0
    CT1      <- as.numeric(input$CT1)
    pKa1     <- input$pKa1
    CT2      <- as.numeric(input$CT2)
    pKa2     <- input$pKa2
    
    ref_pH   <- input$ref_pH
    ref_sol  <- input$ref_sol
    ref_unit <- input$ref_unit
    base     <- input$SG_I
    
    int_pH   <- input$int_pH
    int_sol  <- input$int_sol
    int_unit <- input$int_unit
    IE.Kn    <- input$IE.Kn
    IE.Ki    <- input$IE.Ki
    C.H2O    <- 55.56 # Temperature dependent
    
    # Load observed aqueous & biorelevant solubility data:
    req(input$obs.file)
    obs.aq  <- read.xlsx(input$obs.file$datapath,sheet="Observed.Aqueous")
    obs.aq  <- data.frame(apply(obs.aq,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
    
    obs.br  <- read.xlsx(input$obs.file$datapath,sheet="Observed.Biorelevant")
    obs.br  <- data.frame(apply(obs.br,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
    
    # Make API properties non-editable after data import
    observeEvent(input$obs.file, {
      # Disable the input fields
      shinyjs::disable("API")
      shinyjs::disable("LogP")
      shinyjs::disable("MW.API")
      shinyjs::disable("MW.unit")
      
      # Add CSS to make them appear greyed out
      shinyjs::addClass("API", "input-disabled")
      shinyjs::addClass("LogP", "input-disabled")
      shinyjs::addClass("MW.API", "input-disabled")
      shinyjs::addClass("MW.unit", "input-disabled")
    })
    
    # Source helper functions
    source("SolubilityFunctionsExport.R", local = T)

    # Solubility unit conversion calculations
    S_ref <- getUnitFactor.ref.f(ref_unit)*ref_sol
    S_int <- getUnitFactor.int.f(int_unit)*int_sol
    
    aq.num <- c("Obs.aq.sol","pH.final")
    obs.aq[aq.num] <- sapply(obs.aq[aq.num],as.numeric)
    obs.aq <- getUnitFactor.aq.f(obs.aq)
    obs.aq$aq.S_mg.ml <- obs.aq$Obs.aq.sol*obs.aq$factor
    
    br.num <- c("Obs.BR.sol","pH.BR","BS_mM")
    obs.br[br.num] <- sapply(obs.br[br.num],as.numeric)
    obs.br <- getUnitFactor.br.f(obs.br)
    obs.br$BR.S_mg.ml <- obs.br$Obs.BR.sol*obs.br$factor
    
    
    #### Generate output ####
    # Observed solubility table, based on input$obs.file and selected display sheet 
    output$observed <- renderTable({
      req(input$obs.file)
      
      obs.aq.raw <- obs.aq[,c("ID.aq","Source","Medium","pH.final","aq.S_mg.ml","Obs.aq.sol","SD.aq.Sol","Obs.aq_unit")]
      obs.aq.raw$pH.final <- formatC(obs.aq.raw$pH.final, digits = 2)
      colnames(obs.aq.raw) <- c("ID.aq","Source","Medium","Reported pH","Solubility (mg/mL)","Raw sol.","SD","Unit")
      
      
      obs.br.raw <- obs.br[,c("ID.BR","Source","Name","pH.BR","BS_mM","BR.S_mg.ml","Obs.BR.sol","SD.BR.Sol","Obs.BR_unit")]
      obs.br.raw$pH.BR <- formatC(obs.br.raw$pH.BR, digits = 2)
      obs.br.raw$BS_mM <- formatC(obs.br.raw$BS_mM, digits = 2)
      colnames(obs.br.raw) <- c("ID.br","Source","Medium","Reported pH","Bile salt (mM)","Solubility (mg/mL)","Raw sol.",
                                "SD","Unit")
      
      if(input$disp == "obs.aq.raw") {
        return(obs.aq.raw)
      } else {
        return(obs.br.raw)
      }
    },digits = -2)
    
    # Initial aqueous solubility plot
    output$InitialAqSol <- renderPlot({
      pH.range <- 1:14
      df <- data.frame(pH.range)
      aq.plot <- aq.polt.f(df,pH.range)
      vals$p1 <- aq.plot
      return(aq.plot)
    })
    
    # Estimate SG and intrinsic solubility using nls() function and observed data and generate output
    output$SG.tab <- renderTable({
      SG.Est.f()$t
    })
    
    output$downloadAQpred <- downloadHandler(
      filename = "AqSol.xlsx",
      content = function(filename) {
        df_list <- list("Input parameters"=Input.tab.f(), "Output SG fit"=SG.Est.f()$t, "Pred. vs. obs. Solubilities"=SG.Est.f()$obs.aq)
        write.xlsx(x = df_list, file = filename, rowNames = FALSE)
      }
    )
    
    observeEvent(input$SGfit, {
      output$CopyPasteSG <- renderText({
        "Copy Estimated SG and paste in SG input field, then press 'Plot Residuals'"
      })
    })
    
    observeEvent(input$SGfit, {
      output$PlotRes <- renderUI({
        actionButton("PlotRes", label = "Plot Residuals")
      })
    })
    
    observeEvent(input$PlotRes, {
      output$SGpHRes <- renderPlot({
        obs.aq <- SG.Est.f()$obs.aq
        RES.pH.aq <- plot.RES.pH.aq.f(df,obs.aq)
        vals$p2 <- RES.pH.aq
        return(RES.pH.aq)
      })
    })
    
    # Biorelevant solubility
    output$IntS.txt <- renderText(paste0("Suggested back-calculated intrinsic solubility: ",S.int.f(CT0,CT1,CT2,pKa0,pKa1,pKa2,S_ref,ref_pH)," mg/mL, extrapolated to solubility @pH: ", 
                                         pH.int.f(CT0,CT1,CT2,pKa0,pKa1,pKa2)))
    output$Kn.IE.txt <- renderText(paste0("Suggested initial estimate: ",Kmwn.f(LogP)," (LogP based)"))
    output$Ki.IE.txt <- renderText(paste0("Suggested initial estimate: ",Kmwi.f(CT0,CT1,CT2,LogP)," (LogP and API type based)"))
    
    output$K.tab <- renderTable({
      Kmw.est.f()$t
    })
    
    output$downloadBRpred <- downloadHandler(
      filename = "BrSol.xlsx",
      content = function(filename) {
        obs.br <- S.ion.f(obs.br)
        obs.br <- Pred.br.f(obs.br)
        obs.br$Ln.RES <- log(obs.br$Pred.br)-log(obs.br$BR.S_mg.ml)
        df_list <- list("Input parameters"=Input.tab.f(),"Output LogK fit"=Kmw.est.f()$t, "Pred. vs. obs. Solubilities"=obs.br)
        write.xlsx(x = df_list , file = filename, rowNames = FALSE)
      }
    )
    
    observeEvent(input$Kfit, {
      output$CopyPasteK <- renderText({
        "Copy Estimated Km:w values and paste in respective input fields, then press 'Plot Residuals'"
      })
    })
    
    observeEvent(input$Kfit, {
      output$updateK <- renderUI({
        actionButton("updateK", label = "Plot Residuals")
      })
    })
    
    observeEvent(input$updateK, {
      output$KpHRes <- renderPlot({
        obs.br <- S.ion.f(obs.br)
        obs.br <- Pred.br.f(obs.br)
        obs.br$Ln.RES <- log(obs.br$Pred.br)-log(obs.br$BR.S_mg.ml)
        vals$p3 <- plot.RES.pH.br.f(obs.br)
        vals$p4 <- plot.RES.bs.br.f(obs.br)
        grid.arrange(grobs=list(vals$p3, vals$p4), ncol=2)
      })
    })
    
    output$biorelevant_plot <- renderPlot({
      
      # Process the data stored in the environment
      obs.br_processed <- S.ion.f(obs.br)
      obs.br_processed <- Pred.br.f(obs.br_processed)
      
      max_value <- max(max(obs.br_processed$BR.S_mg.ml, na.rm = TRUE), 
                       max(obs.br_processed$Pred.br, na.rm = TRUE))
      min_value <- min(min(obs.br_processed$BR.S_mg.ml[obs.br_processed$BR.S_mg.ml > 0], na.rm = TRUE), 
                       min(obs.br_processed$Pred.br[obs.br_processed$Pred.br > 0], na.rm = TRUE))
      
      # linear scale plot:
      p1 <- ggplot(obs.br_processed, aes(x = BR.S_mg.ml, y = Pred.br)) +
        create_osp_theme() +
        
        geom_point(size=3, stroke = 0.5, color = "#2e6f8e") +
        geom_abline(slope = 1, intercept = 0, linewidth = 0.75, linetype = "solid", color = "gray50") +
        
        scale_x_continuous(limits = c(0, max_value * 1.1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, max_value * 1.1), expand = c(0, 0)) +
        
        labs(subtitle = "Linear Scale",
             x = "Observed Biorelevant Solubility (mg/mL)",
             y = "Predicted Biorelevant Solubility (mg/mL)") +
        
        theme(aspect.ratio = 1)
      
      # log scale plot:
      p2 <- ggplot(obs.br_processed, aes(x = BR.S_mg.ml, y = Pred.br)) +
        create_osp_theme() +
        
        geom_point(size=3, stroke = 0.5, color = "#2e6f8e") +
        geom_abline(slope = 1, intercept = 0, linewidth = 0.75, linetype = "solid", color = "gray50") +
        
        scale_x_log10(limits = c(min_value * 0.9, max_value * 1.1)) +
        scale_y_log10(limits = c(min_value * 0.9, max_value * 1.1)) +
        
        labs(subtitle = "Log Scale",
             x = "Observed Biorelevant Solubility (mg/mL)",
             y = "Predicted Biorelevant Solubility (mg/mL)") +
        
        theme(aspect.ratio = 1)
      
      vals$p5 <- p1
      vals$p6 <- p2
      
      # Arrange plots side by side
      grid.arrange(p1, p2, ncol=2)
      
    }, height = 500, width = 1000)  # Adjusted width to accommodate two plots
    
    output$downloadAQreport = downloadHandler(
      filename = function() {"AQreport.pdf"},
      content = function(file) {
        pdf(file, onefile = TRUE)
        grid.arrange(vals$p1,vals$p2)
        dev.off()
      }
    )
    
    output$downloadBRreport = downloadHandler(
      filename = function() {"BRreport.pdf"},
      content = function(file) {
        pdf(file, onefile = TRUE)
        grid.arrange(vals$p3,vals$p4,vals$p5,vals$p6, ncol=2) 
        dev.off()
      }
    )
  })
  
  
  # Second observe() wrapper for surface pH calculations:
  
  observe({
    API      <- input$API
    MW.API   <- input$MW.API
    MW.unit  <- input$MW.unit
    LogP     <- input$LogP
    
    CT0      <- as.numeric(input$CT0)
    pKa0     <- input$pKa0
    CT1      <- as.numeric(input$CT1)
    pKa1     <- input$pKa1
    CT2      <- as.numeric(input$CT2)
    pKa2     <- input$pKa2
    
    ref_pH   <- input$ref_pH
    ref_sol  <- input$ref_sol
    ref_unit <- input$ref_unit
    base     <- input$SG_I
    
    # Source helper functions
    source("SolubilityFunctionsExport.R", local = T)
    
    # Calculate suggested diffusion coefficient based on LogP
    suggested_diffusion <- calculate_diffusion_coefficient(MW.API)
    
    # Add output for suggested diffusion coefficient
    output$DAPI_suggested <- renderText({
      paste0("Suggested diffusion coefficient: ", formatC(suggested_diffusion, format = "e", digits = 2), 
             " cm²/s (PK-Sim equation)")
    })
    
    # Add output for suggested intrinsic solubility (using same function as in biorelevant solubility tab)
    output$S0_suggested <- renderText({
      paste0("Suggested back-calculated intrinsic solubility: ", 
             S.int.f(CT0, CT1, CT2, pKa0, pKa1, pKa2, getUnitFactor.ref.f(ref_unit) * ref_sol, ref_pH)/MW.API, 
             " M, (", S.int.f(CT0, CT1, CT2, pKa0, pKa1, pKa2, getUnitFactor.ref.f(ref_unit) * ref_sol, ref_pH),
             " mg/mL), extrapolated to solubility @pH: ", pH.int.f(CT0, CT1, CT2, pKa0, pKa1, pKa2))
    })
    
    # Surface pH calculations
    surface_ph_data <- eventReactive(input$calc_pH, {
      
      # Disable button immediately when clicked
      shinyjs::disable("calc_pH")
      shinyjs::addClass(id = "calc_pH", class = "btn-disabled")
      shinyjs::removeClass(id = "calc_pH", class = "btn-primary")
      
      # Create a progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating surface pH", value = 0)
      on.exit({
        progress$close()
        # Re-enable button when calculation is complete
        shinyjs::enable("calc_pH")
        shinyjs::removeClass(id = "calc_pH", class = "btn-disabled")
        shinyjs::addClass(id = "calc_pH", class = "btn-primary")
      })
      
      # Define progress callback function
      update_progress <- function(value, detail) {
        progress$set(value = value, detail = detail)
        # Add small delay to allow UI to update
        Sys.sleep(0.01)
      }
      
      # Setup parameters for surface pH calculation
      CT0_num <- as.numeric(input$CT0)
      CT1_num <- 0  # For monoprotic compounds
      CT2_num <- 0  # For monoprotic compounds
      BT_num <- as.numeric(input$BT)
      BcarB_num <- as.numeric(input$BcarB)
      Salt_num <- as.numeric(input$Salt)
      
      # Calculate surface pH using outsourced functions
      pH_values <- seq(0.1, 14, by = 0.1)
      result_df <- calculate_surface_ph(
        pH_values = pH_values,
        DH = input$DH,
        DOH = input$DOH,
        DAPI = input$DAPI,
        DYH = input$DYH,
        DX = input$DX,
        CT0_num = CT0_num,
        CT1_num = CT1_num,
        CT2_num = CT2_num,
        pKa0 = input$pKa0,
        Salt_num = Salt_num,
        S0 = input$S0,
        BT_num = BT_num,
        C_buffer = input$C_buffer,
        BcarB_num = BcarB_num,
        Kw = input$Kw,
        pKaYH = input$pKaYH,
        progress_callback = update_progress
      )
      
      vals$output_df <- result_df
      return(vals$output_df)
    })
    
    observe({
      if(!calc_status()) {
        shinyjs::disable("calc_pH")
      } else {
        shinyjs::enable("calc_pH")
      }
    })
    
    # Create surface pH plot
    output$surface_pH_plot <- renderPlot({
      req(surface_ph_data())
      
      # Use outsourced plot creation function
      create_surface_ph_plot(vals$output_df, API)
      
    }, height = 750)
  })
  
}

#### Shiny app ----
shinyApp(ui = ui, server = server)
