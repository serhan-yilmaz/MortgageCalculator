library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinytoastr)
library(shinyWidgets)
library(cicerone)
#library(jsonlite)
library(rjson)
library(clipr)
#library(shinyCopy2clipboard)

collapseInput <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}

optionBox <- function(..., title = "", status = "primary", id = ""){
  colid = paste0(id, "_collapse")
  tags$div(
    id = paste0(id, "_wrapper"), 
    box(id = id, status = status, width = NULL, collapsible = T, title = title, solidHeader = T, collapsed = T, 
        ...
    ),
    collapseInput(colid, boxId = id)
  )
}

option_set = list(
  "house_price" = list(type = "numericInput"),
  "mortgage_duration" = list(type = "selectInput"),
  "mortgage_downpayment" = list(type = "numericInput"),
  "monthly_fee" = list(type = "numericInput"),
  "house_yearly_income" = list(type = "numericInput"),
  "house_income_invest_earnings" = list(type = "checkboxInput"),
  "house_income_investment_strategy" = list(type = "selectInput"),
  "investment_gain" = list(type = "sliderInput"),
  "income_tax" = list(type = "sliderInput"),
  "inflation" = list(type = "sliderInput"),
  "housing_inflation" = list(type = "sliderInput"),
  "adjust_by_inflation" = list(type = "checkboxInput"),
  "checksum"="320932023409243"
)

guide <- Cicerone$
  new()$ 
  step("about_main_div", 
       "Welcome",
       "This is a quick tutorial to help you get started. Click next to continue."
  )$
  step("sidebar_option_div", 
       "Input & Options",
       "This is main area to specify the options for the analysis. "
  )$
  step("sidebar_option_div2", 
       "Input & Options",
       "We will quickly walk through each of them."
  )$
  step("house_price", 
       "House price",
       "First, enter the house price."
  )$
  step("mortgage_optionbox_wrapper", 
       "Mortgage",
       "Next, specify the mortgage options (duration, downpayment, monthly fees)."
  )$
  step("mortgage_downpayment", 
       "Downpayment",
       "This also includes other upfront fees like closing fees or discount points."
  )$
  step("house_income_optionbox_wrapper", 
       "House Income",
       "Here, enter the expected income or savings made from the purchase of the house (substracting all costs, property tax, maintenance fees, or income tax if applicable)"
  )$
  step("investment_optionbox_wrapper", 
       "Investment",
       "Here, enter the expected yearly gain % from the investments (the default value is the 20 year average of S&P500 index, November 2002-2022)."
  )$
  step("tax_optionbox_wrapper", 
       "Tax",
       "Here, expected tax % to pay on the investment gains (e.g., on the dividends or on the capital gains)"
  )$
  step("inflation_optionbox_wrapper", 
       "Inflation",
       "Here, you can specify the expected yearly inflation rates (the default values are the 20-year CPI averages of USA, November 2002-2022)"
  )$
  step("config_optionbox_wrapper", 
       "Configuration",
       "Here, you can save the selected options for future use or generate a link to share them with others."
  )$
  step("verbatimText_main", 
       "Analysis Results",
       "Here, you can view the analysis results. For the money necessary to pay for the mortgage, several competing scenarios are considered to assess the opportunity cost: Retirement (tax-free) investment, tax-liable investment, inflation protection and so on."
  )$
  step("about_main_div", 
       "End of Tutorial",
       "This is the end of the tutorial. Hope you enjoyed it!"
  )


jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


# Define UI for application that draws a histogram
ui <- fluidPage(
    useToastr(),
    useShinyjs(),
    shinyjs:::extendShinyjs(text = jscode, functions = c("collapse")),
    # Application title
    tags$div(style = "margin-top:50px;"), 
    titlePanel("Mortgage Calculator"),
    tags$h4(style = "font-size:16px; color: #666666; margin-bottom:12px;", "Opportunity cost of getting a mortgage compared to other investment options"), 
    tags$head(
      tags$meta(name = "description", content = "A simple calculator to assess the opportunity cost of getting a mortgage taking into account the expected inflation and other investment options.")
    ),
    #tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) {document.title=x});')),
    # tags$script(HTML(
    # 'Shiny.addCustomMessageHandler("changetitle", function(x) {
    # document
    #   .getElementsByTagName("meta")
    #   .namedItem("description")
    #   .setAttribute("content", x)});')),
    useShinydashboard(),
    use_cicerone(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tags$div(id = "sidebar_option_div",tags$div(id = "sidebar_option_div2", 
          numericInput("house_price", "House Price ($):", 379000, min = 1000, max = 10000000, step = 100),
          optionBox(id = "mortgage_optionbox", title = "Mortgage Options", 
            tags$div(id = "mortgage_opts_div",
            selectInput("mortgage_duration", "Mortgage Duration:", c("10 Years", "15 Years", "20 Years", "30 Years"), selected = "15 Years"),
            numericInput("mortgage_downpayment", "Down Payment + Upfront Fees ($):", 75800, min = 0, max = 10000000, step = 100),
            numericInput("monthly_fee", "Monthly Payment ($):", 2432, min = 0, max = 100000, step = 1),
            ),
          ),
          optionBox(id = "house_income_optionbox", title = "House Income", 
            numericInput("house_yearly_income", "House Yearly Net Income ($):", 4700, min = 0, max = 100000, step = 10),
            checkboxInput("house_income_invest_earnings", "Invest the earnings", value = T, width = NULL),
            selectInput("house_income_investment_strategy", "Investment Strategy:", c("Retirement Investment", "Tax-liable Investment", "Protect against inflation"), selected = "Tax-liable Investment")
          ),
          optionBox(id = "investment_optionbox", title = "Investment", 
          sliderInput("investment_gain", "Expected Investment Gain Annual:", 
                      min = 0, max = 20, step  = 0.02, 
                      value = 7.72, post = "%"),
          ),
          optionBox(id = "tax_optionbox", title = "Tax",
          sliderInput("income_tax", "Income Tax (on investments):", 
                      min = 0, max = 50, step  = 0.1, 
                      value = 15, post = "%"),
          ),
          optionBox(id = "inflation_optionbox", title = "Expected Inflation",
            sliderInput("inflation", "Inflation:", 
                        min = 0, max = 10, step  = 0.01, 
                        value = 2.52, post = "%"),
            sliderInput("housing_inflation", "Housing Inflation:", 
                        min = 0, max = 10, step  = 0.01, 
                        value = 2.77, post = "%"),
          ),
          checkboxInput("adjust_by_inflation", "Adjust numbers by inflation", value = T, width = NULL),
          
          optionBox(id = "config_optionbox", title = "Import/Export Config", status = "success", 
            textInput("config_name", "Configuration name (optional):", value = ""),
            tags$div(
              style = "margin-bottom: 6px;", 
              downloadButton('download_config', 'Download Config'),
              #tags$div(style = "margin-left: auto; margin-right: auto; width:0px;"),
              tags$div(style = "display: inline-block; float: right;",
                actionButton("generate_token_button", "Generate token")
              )
            ),
            tags$div(style = "font-size:16px; float:right; margin-right: 4px;",
              tags$a(id = "config_link_element", ""),
            ),
            textInput("config_token", "Enter token:", value = ""),
            tags$div(
              tags$div(style = "display: inline-block; float:right;",
                actionButton("restore_token_button", "Restore config")
              )
            )
          )
          )),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tags$div(
            id = "about_main_div", 
            actionLink(style = "font-size: large;", "interactiveDemo", "Run Interactive Tutorial"),
          ),
          verbatimTextOutput("verbatimText_main")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    mortgage_duration <- reactive({
      switch(input$mortgage_duration, 
             "10 Years" = a <- 10, 
             "15 Years" = a <- 15, 
             "20 Years" = a <- 20, 
             "30 Years" = a <- 30)
      #message(a)
      return(a)
    })
  
    mortgage_total <- reactive({
      mortgage_duration() * 12 * input$monthly_fee + input$mortgage_downpayment
    })
    
    mortgage_total_adjusted <- reactive({
      monthly_inflation = 1
      inflation_factor = input$inflation/100
      nMonth = mortgage_duration() * 12
      initial = input$mortgage_downpayment
      total = initial
      for(iMonth in (1:nMonth)){
        monthly_inflation = monthly_inflation * ((1+inflation_factor)^(1/12));
        total = total + input$monthly_fee / monthly_inflation;
      }
      return(total)
    })
    
    inflation_adjustment_ <- reactive({
      (1+input$inflation/100)^mortgage_duration()
    })
    
    foSimulator <- function(yearly_interest, 
                            initial = input$mortgage_downpayment,
                            nMonth = mortgage_duration() * 12, 
                            monthly_mortgage = input$monthly_fee, 
                            income_tax = (1 - input$income_tax/100),
                            tax = T, 
                            inflation_scaling = F,
                            inflation_factor = input$inflation/100, 
                            adjust_by_inflation = input$adjust_by_inflation,
                            inflation_adjustment = inflation_adjustment_()){

      monthly_inflation = 1
      monthly_interest = (1 + yearly_interest)^(1/12) - 1;
      #yearly_interest = input$investment_gain/100
      #income_tax = (1 - input$income_tax/100)
      #monthly_mortgage = input$monthly_fee 
      #initial = input$mortgage_downpayment
      if(tax == FALSE){
        income_tax = 1;
      }
      if(inflation_scaling == FALSE){
        inflation_factor = 0
      }
      total = initial
      for(iMonth in (1:nMonth)){
        total = total * (1+monthly_interest*income_tax) + monthly_mortgage * monthly_inflation;
        monthly_inflation = monthly_inflation * ((1+inflation_factor)^(1/12));
      }
      if(adjust_by_inflation){
        total = total / inflation_adjustment
      }
      return(total)
    }
    
    scenarioA_investment <- reactive({
      foSimulator(input$investment_gain/100)
    })
    
    scenarioB_inflation <- reactive({
      foSimulator(input$inflation/100, tax = F)
    })
    
    scenarioC_checking <- reactive({
      foSimulator(0, tax = F)
    })
    
    scenarioD_housenogain <- reactive({
      foSimulator(input$housing_inflation/100, tax = F, initial = input$house_price, monthly_mortgage = 0)
    })
    
    scenarioE_houserent <- reactive({
      if(input$house_income_invest_earnings){
        switch(input$house_income_investment_strategy,
          "Retirement Investment" = {
            yearly_interest <- input$investment_gain/100
            tax = F
          }, 
          "Tax-liable Investment" = {
            yearly_interest <- input$investment_gain/100
            tax = T
          },
          "Protect against inflation" = {
            yearly_interest <- input$inflation/100
            tax = F
          }
        )
      } else {
        yearly_interest = 0
        tax = T
      }
      scenarioD_housenogain() + foSimulator(
        yearly_interest, initial = 0, tax = tax, 
        monthly_mortgage = input$house_yearly_income/12, 
        inflation_scaling = T, inflation_factor = input$housing_inflation/100)
    })
    
    scenarioF_retirement_investment <- reactive({
      foSimulator(input$investment_gain/100, tax = F)
    })
    
    downpaymentWarning = reactiveVal(FALSE);
    started = reactiveVal(FALSE)
    
    output$verbatimText_main <- renderText({
      if(!started()){
        #session$sendCustomMessage("changetitle", "Configuration: xkjfkjgj")
        #html(id = "metadata_description_text", html=paste0("Configuration:", 'qwer'))
        query <- parseQueryString(session$clientData$url_search)
        query_s = paste(names(query), query, sep = "=", collapse=", ")
        token = query[["token"]]
        if(!is.null(token)){
          foRestoreConfiguration(token)
        }
        started(TRUE)
      }
      
      R = session$request
     # C = session$clientData
      linesep = "=============================================="
      if(input$mortgage_downpayment > input$house_price && !downpaymentWarning()){
        downpaymentWarning(TRUE)
        toastr_warning("Who taught you to give a mortgage downpayment larger than the house price?", closeButton = F, timeOut = 7500, extendedTimeOut = 3000)
        delay(2500, toastr_warning("Seriously, I want to meet them.", closeButton = F, extendedTimeOut = 2000))
      }
      
      if(input$adjust_by_inflation){
        star_ = "*"
        note_1 = "* Prices are adjusted by inflation and reflect today's prices"
      } else {
        star_ = ""
        note_1 = ""
      }
      query <- parseQueryString(session$clientData$url_search)
      query_s = paste(names(query), query, sep = "=", collapse=", ")
      token = query[["token"]]
      
      paste("Hello world!", "This is your mortgage calculator speaking.", 
            "If you look outside, you can see the mortgages", 
            "smiling with around 5.21% yearly interest.", linesep, 
            "Enough chitchat. Let's talk business.", 
            #paste("Mortgage Duration:", mortgage_duration()), 
            paste0("Mortgage Total Payment: ", sprintf("%.1fK", mortgage_total()*1e-3), " (", sprintf("%.1fK* adjusted)", mortgage_total_adjusted()*1e-3)), 
           # paste0("Mortgage Total Payment (Adjusted): ", ),
            "", "Now, let's see some opportunity cost, shall we?", linesep,
            paste0("After ", mortgage_duration(), " years:"), 
            paste0("Scenario A - Retirement Investment: ", sprintf("%.1fK", scenarioF_retirement_investment()*1e-3), star_),
            paste0("Scenario B - Investment (e.g., stocks): ", sprintf("%.1fK", scenarioA_investment()*1e-3), star_),
            paste0("Scenario C - Buy a house and rent: ", sprintf("%.1fK", scenarioE_houserent()*1e-3), star_), 
            paste0("Scenario D - House value only: ", sprintf("%.1fK", scenarioD_housenogain()*1e-3), star_), 
            paste0("Scenario E - Protect against Inflation (e.g., TIPS): ", sprintf("%.1fK", scenarioB_inflation()*1e-3), star_), 
            paste0("Scenario F - Put the money to a checking account: ", sprintf("%.1fK", scenarioC_checking()*1e-3), star_),
            note_1, 
            #linesep, 
            #paste0("HTTP Origin: ", R$HTTP_ORIGIN), 
            #paste0("PATH INFO: ", R$PATH_INFO), 
            #paste0("Query String: ", R$QUERY_STRING),
            #paste0("Script Name: ", R$SCRIPT_NAME), 
            #paste0("Server Name: ", R$SERVER_NAME), 
            #paste0("Server Port: ", R$SERVER_PORT), 
            #paste0("HTTP_X_FORWARDED_FOR:", R$HTTP_X_FORWARDED_FOR), 
            #paste0("ClientData: ", query_s),
            #paste0("Token:", token), 
           # paste0("Request: ", toJSON(as.list(R))), 
             sep = "\n")
      })
  
    convertRaw2Str = function(x) paste(x,collapse = '')
    
    convertStr2Raw = function(s){
      sst <- strsplit(s, "")[[1]]
      out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
      as.raw(as.hexmode((out)))
    }
    
    foInputList <- function(){
      out = list()
      for(iX in 1:(length(option_set) - 1)){
        name = names(option_set[iX])
        out[[name]] = input[[name]]
      }
      if(nchar(input$config_name) > 0){
        out$config_name = input$config_name
      }
      out
    }
    
    foRestoreConfig <- function(inputlist){
      out = list()
      for(iX in 1:(length(option_set) - 1)){
        name = names(option_set[iX])
        type = option_set[iX][[name]]$type
        value = inputlist[[name]]
        if(is.na(value)){
          stop(paste0("Input option value not found: ", name))
        }
        switch(type,
          "numericInput" = {updateNumericInput(session, name, value = value)},
          "selectInput" = {updateSelectInput(session, name, selected = value)},
          "sliderInput" = {updateSliderInput(session, name, value = value)},
          "checkboxInput" = {updateCheckboxInput(session, name, value = value)},
          stop(paste0("Invalid option type: ", type))
        )
      }
    }
    
    reactive_inputset <- reactive({
      set = foInputList()
    })
    
    foGenerateToken <- function(){
      set = reactive_inputset()
      xs = toJSON(set, indent=0, method="C" )
      token = convertRaw2Str(memCompress(xs, "g")) 
    }
    
    observeEvent(input$config_token,
      if(nchar(input$config_token) > 1){
          html("config_link_element", html=paste0("<a href='?token=", input$config_token, "'", ">Link</a>"))
      } else {
          html("config_link_element", html="")
      }
    )
    
    observeEvent(input$generate_token_button, {
      token = foGenerateToken()
      updateTextInput(session, "config_token", value = token)
      #writeClipboard(as.character(token))
      if(clipr_available()){
        write_clip(as.character(token))
        alert_txt = "Config token generated and Copied to clipboard"
      } else {
        alert_txt = "Config token generated"
      }
      show_alert(title = alert_txt, showCloseButton = F, type = "success", btn_labels = NA, timer = 1000, showConfirmButton = F)
    })
    
    foRestoreConfiguration <- function(token){
      tryCatch({
        xd = memDecompress(convertStr2Raw(token), "g", asChar = T)
        xx = fromJSON(xd)
        foRestoreConfig(xx)
        name = xx[["config_name"]] 
        if(!is.null(name)){
          #html("metadata_description_text", html=paste0("Configuration:", name))
          updateTextInput(session, "config_name", value = name)
          name = paste0("'", name, "'")
        } else {
          name = ""
        }
        show_alert(title = paste0("Configuration ", name, " successfully restored."), showCloseButton = F, type = "success", btn_labels = NA, timer = 1000, showConfirmButton = F)
      }, error = function(e){
        show_alert(title = "An error occurred while restoring the config.", showCloseButton = F, type = "error", btn_labels = NA, timer = 1000, showConfirmButton = F)
        message(as.character(e))
      }
      )
    }
    
    foUncollapseBoxIfNeeeded <- function(id, collapse = F){
      colid = paste0(id, "_collapse");
      val = input[[colid]]
      if(is.null(val)){
        val = T
      }
      if(val == !collapse){
        js$collapse(id)
      }
    }
    
    observeEvent(guide$get_next(), {
      a <- guide$get_next()
      if(!is.null(a)){
        b <- a$highlighted
        if(b == "sidebar_option_div2"){
          foUncollapseBoxIfNeeeded("mortgage_optionbox")
          foUncollapseBoxIfNeeeded("house_income_optionbox")
          foUncollapseBoxIfNeeeded("investment_optionbox")
          foUncollapseBoxIfNeeeded("tax_optionbox")
          foUncollapseBoxIfNeeeded("inflation_optionbox")
          foUncollapseBoxIfNeeeded("config_optionbox")
          # val = input[["mortgage_optionbox_collapse"]]
          # if(!is.null(val)){
          #   js$collapse("mortgage_optionbox")
          # }
        }
        if(b == "config_optionbox_wrapper"){
          foUncollapseBoxIfNeeeded("mortgage_optionbox", collapse = T)
          foUncollapseBoxIfNeeeded("house_income_optionbox", collapse = T)
          foUncollapseBoxIfNeeeded("investment_optionbox", collapse = T)
          foUncollapseBoxIfNeeeded("tax_optionbox", collapse = T)
          foUncollapseBoxIfNeeeded("inflation_optionbox", collapse = T)
          foUncollapseBoxIfNeeeded("config_optionbox", collapse = T)
        }
      }
    })
    
    observeEvent(input$interactiveDemo, {
      guide$init()$start()
    })
    
    observeEvent(input$restore_token_button, {
      token = input$config_token
      foRestoreConfiguration(token)
    })
    
    output$download_config <- downloadHandler(
      filename = function() { paste('config.json', sep='') },
      content = function(file) {
        jsonlite::write_json(reactive_inputset(), path = file, pretty = TRUE)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
