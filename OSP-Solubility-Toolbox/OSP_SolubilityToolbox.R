# Required packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(gdata) # Make sure the correct perl.exe (Strawberry) is called in read.xls()
library(openxlsx)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  # Add OSP logo to navigation bar
  tags$head(tags$script(type="text/javascript", src = "code.js")),                   
  
  #Navbar structure for UI
  navbarPage(title = strong("OSP Solubility Toolbox"), inverse=T, theme = shinytheme("cerulean"),
             
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
                          column(7,autonumericInput("SG_I", "Initial Solubility Gain per charge:", min=1, max=1e6, value=1000, width = "235px",
                                                    decimalPlaces = 2, digitGroupSeparator = ",", decimalCharacter = ".")
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
                        fluidRow(  
                          column(3, actionButton("Kfit", "Estimate Km:w")),
                          column(3, offset = 0.5, downloadButton("downloadBRpred", "Export tables")),
                          column(3, offset = 0.5, downloadButton("downloadBRreport", "Export plots"))
                        ), br()
                      ),
                      mainPanel(h4(p("Upload observed solubility data and input the API properties, then press 'Estimate Km:w' 
                                     in order to fit the micelle partitioning coefficient(s) to your data")),
                                tableOutput("K.tab"),
                                h5(p(textOutput("CopyPasteK"))),
                                fluidRow(
                                  column(3, uiOutput("updateK")),
                                  column(3, offset = 1, uiOutput("plotPredObs"))
                                ),                                plotOutput(outputId = "KpHRes"),
                                plotOutput(outputId = "KbsRes"),
                                plotOutput(outputId = "predicted_vs_observed_bs")
                      )
             )
  )
)

#### Server for solubility fittings ----
server <- function(input, output, session) {
  
  vals <- reactiveValues(p1=NULL,p2=NULL,p3=NULL,p4=NULL,p5=NULL)
  
  # Specify input data once, using observe() wrapper
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
    
    # Load observed aqueous & biorelevant solubilities:
    req(input$obs.file)
    obs.aq  <- read.xlsx(input$obs.file$datapath,sheet="Observed.Aqueous")
    obs.aq  <- data.frame(apply(obs.aq,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
    
    obs.br  <- read.xlsx(input$obs.file$datapath,sheet="Observed.Biorelevant")
    obs.br  <- data.frame(apply(obs.br,MARGIN = 2,trimws,"both"),stringsAsFactors = FALSE)
    
    # Solubility unit conversion calculations
    source("SolubilityFunctionsExport.R", local = T)
    
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
        df_list <- list("Input parameters"=Input.tab.f(),"Output SG fit"=SG.Est.f()$t, "Pred. vs. obs. Solubilities"=SG.Est.f()$obs.aq)
        write.xlsx(x = df_list , file = filename, rowNames = FALSE)
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
      output$plotPredObs <- renderUI({
        actionButton("plotPredObs", label = "Plot Predicted vs. Observed")
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
    
    observeEvent(input$plotPredObs, {
      output$predicted_vs_observed_bs <- renderPlot({
        obs.br <- S.ion.f(obs.br)
        obs.br <- Pred.br.f(obs.br)
        
        max_value <- max(max(obs.br$BR.S_mg.ml, na.rm = TRUE), 
                         max(obs.br$Pred.br, na.rm = TRUE))
        min_value <- min(min(obs.br$BR.S_mg.ml[obs.br$BR.S_mg.ml > 0], na.rm = TRUE), 
                         min(obs.br$Pred.br[obs.br$Pred.br > 0], na.rm = TRUE))
        
        # linear scale plot:
        p1 <- ggplot(obs.br, aes(x = BR.S_mg.ml, y = Pred.br)) +
          theme(
            axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
            rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
            
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"),
            
            panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
            panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
            panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
            
            legend.key = element_rect(fill = "white"),
            
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 16,hjust = 0.5),
            plot.margin=unit(c(.4,.4,.2,.5), 'cm'),
            
            # Ensure the plot panel is square
            aspect.ratio = 1
          ) +
          
          geom_point(size=3, stroke = 0.5, color = "blue") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          
          # Set identical limits and breaks for both axes
          scale_x_continuous(limits = c(0, max_value * 1.1), 
                             expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, max_value * 1.1),
                             expand = c(0, 0)) +
          
          labs(title = "Predicted vs Observed Biorelevant Solubility",
               subtitle = paste0(API, " (Linear Scale)"),
               x = "Observed Biorelevant Solubility (mg/mL)",
               y = "Predicted Biorelevant Solubility (mg/mL)")
        
        # log scale plot:
        p2 <- ggplot(obs.br, aes(x = BR.S_mg.ml, y = Pred.br)) +
          theme(
            axis.line = element_line(colour = "black", linewidth = 1, linetype = "solid"),
            rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
            
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"),
            
            panel.background = element_rect(fill = "white", colour = "white", linewidth = 0.5, linetype = "solid"),
            panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "lightgray"),
            panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "lightgray"),
            
            legend.key = element_rect(fill = "white"),
            
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 16,hjust = 0.5),
            plot.margin=unit(c(.4,.4,.2,.5), 'cm'),
            
            # Ensure the plot panel is square
            aspect.ratio = 1
          ) +
          
          geom_point(size=3, stroke = 0.5, color = "blue") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          
          # Set log scales with identical limits
          scale_x_log10(limits = c(min_value * 0.9, max_value * 1.1)) +
          scale_y_log10(limits = c(min_value * 0.9, max_value * 1.1)) +
          
          labs(title = "Predicted vs Observed Biorelevant Solubility",
               subtitle = paste0(API, " (Log Scale)"),
               x = "Observed Biorelevant Solubility (mg/mL)",
               y = "Predicted Biorelevant Solubility (mg/mL)")
        
        # Store plots in reactive values
        vals$p5 <- p1
        vals$p6 <- p2
        
        # Arrange plots side by side
        grid.arrange(p1, p2, ncol=2)
        
      }, height = 500, width = 1000)  # Adjusted width to accommodate two plots
    })

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
}

#### Shiny app ----
shinyApp(ui = ui, server = server)
