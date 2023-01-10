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

# For tooltips
library(shinyhelper)
library(shinyBS) 

on_ready <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "Shiny.setInputValue('initialized', 1);",
  "});",
  "",
  "});",
  sep = "\n"
)

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

optionBox <- function(..., title = "", status = "primary", id = "", collapsed = T, class = ""){
  colid = paste0(id, "_collapse")
  tags$div(
    id = paste0(id, "_wrapper"), 
    box(id = id, class = class, status = status, width = NULL, collapsible = T, title = title, solidHeader = T, collapsed = collapsed, 
        ...
    ),
    collapseInput(colid, boxId = id),
  )
}

dropdown_options <- function(content, titletxt = "Options", tooltip = "Click to see options."){
  # position:absolute;
  tags$div(style = "margin-top: auto; margin-bottom: auto; margin-left:4px; max-width: 50px; float: right;vertical-align: middle; right: 0px; top:50%;", 
           dropdownButton(
             size = "sm", 
             icon = icon("cog"), #status = "info", 
             right = T, 
             up = F, 
             tooltip = tooltipOptions(title = tooltip, placement = "right", html = T), 
             tags$h4(style = "font-weight:bold; margin-bottom: 10px; white-space: nowrap;", titletxt), 
             content, 
             tags$p(style = "margin-bottom: 10px;", "")
           ))
}

option_set = list(
  "house_price" = list(type = "anumericInput"),
  "mortgage_duration" = list(type = "selectInput"),
  "mortgage_downpayment" = list(type = "anumericInput"),
  "monthly_fee" = list(type = "anumericInput"),
  # "house_yearly_income" = list(type = "numericInput"),
  "house_rent" = list(type = "anumericInput"),
  "house_costs" = list(type = "anumericInput"),
  "house_income_invest_earnings" = list(type = "checkboxInput"),
  "house_income_investment_strategy" = list(type = "selectInput"),
  "investment_gain" = list(type = "sliderInput"),
  "income_tax" = list(type = "sliderInput"),
  "inflation" = list(type = "sliderInput"),
  "housing_inflation" = list(type = "sliderInput"),
  "adjust_by_inflation" = list(type = "checkboxInput"),
  "retirement_ira_max_contribution" = list(type = "anumericInput"),
  "retirement_roth_ira_max_contribution" = list(type = "anumericInput"),
  "house_costs_property_tax" = list(type = "anumericInput"),
  "house_costs_insurance" = list(type = "anumericInput"),
  "house_costs_hoa_fee" = list(type = "anumericInput"),
  "house_costs_maintenance" = list(type = "anumericInput"),
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
  step("account_for_inflation_div", 
       "Accounting for inflation in the results",
       "If this tickmark is checked, the numbers in the results will be adjusted for expected inflation and reflect today's prices."
  )$
  step("config_optionbox_wrapper", 
       "Configuration",
       "Here, you can save the selected options for future use or generate a link to share them with others."
  )$
  step("main_output_div", 
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

value_div <- function(value_id){
  uiOutput(value_id, style = "font-size:22px; text-align: center; width:100%; margin-top:0px;min-height:31px;")
}

scenario_box <- function(title="", img_src, label = "", value_id="", margin=0){
  wxx = 125;
  st_mrg = paste0("margin:", sprintf("%.1f", wxx*margin/200), "px;");
  wxx_t = wxx * (1 - margin/100);
  p_left=6;
  
  
  tags$div(style = "display:inline-flex;", 
      # helper(
      tags$div(
      style = "display:inline-block; border: groove; padding:8px; border-width:1.5px; margin:4px; max-width:380px; min-height:160px;", 
      tags$div(
      tags$div(
        style = "display:inline-block; vertical-align:middle; margin-right:auto; width:63%;", 
        tags$div(title, style = "font-size: 18px; text-align: center; width:100%;"),
        value_div(value_id)
      ),
      tags$img(height = wxx_t, width = wxx_t+p_left, src = img_src, style = st_mrg, style = "float:right;", style = sprintf("padding-left:%dpx;", p_left)),
      ),
      tags$div(style = "font-size: 15px; text-align:justify;", 
        label
      )
    )
    # , type = "markdown", content = "input_data_format")
  )
}

foBoxLabel <- function(pre, outputid, post){
  tags$span(
    pre,
    uiOutput(outputid, inline = T),
    post
  )
}

boxLabels <- list(
  "rent" = foBoxLabel("This scenario assumes the house is purchased to be rented out, resulting in a ", "uioutput_buyhouseandrent_label", "saving."),
  "liveinit" = foBoxLabel("This scenario assumes the house is purchased to be used as a primary residence, making a ", "uioutput_buyhouseandliveinit_label", "saving by not paying rent."),
  "retirement" = foBoxLabel("This scenario assumes that up to", "uioutput_retirement_account_label", "of the money is put into a retirement account, providing tax-free gains on the investments.")
)

boxes <- list(
  scenario_box("Buy house and live in it", "icon_buy_house_and_live.png", value_id = "uioutput_buyhouseandliveinit", margin = 7, label = boxLabels$liveinit),
  scenario_box("Buy house and rent", "icon_buy_house_and_rent.png", value_id = "uioutput_buyhouseandrent", label = boxLabels$rent),
  scenario_box("Use the money for investment", "icon_investment_account.png", value_id = "uioutput_investment", margin = -15, label = "This scenario assumes the money to be used for mortgage are put into a tax-liable investment account instead of buying a house."),
  scenario_box("Retirement Account ", "icon_retirement_investment.png", value_id = "uioutput_retirement", margin = -14, label = boxLabels$retirement),
  scenario_box("Protect against inflation", "icon_protect_against_inflation.png", value_id = "uioutput_protectagainstinflation", margin = 14, label = "This scenario assumes that the value of money is retained against inflation, for example, by using TIPS or I-bonds."),
  scenario_box("Keep in checking account", "icon_fail_grade4.png", value_id = "uioutput_keepincheckingaccount", margin = 10, label = "This scenario assumes all the money is deposited into a checking account without investment.")
  )

addchildren <- function(el, boxes){
  el
  nchild = length(el$children);
  for(i in 1:length(boxes)){
    box = boxes[[i]]
    
    el$children[[nchild+i]] = box
  }
  return(el);
}


generate_scenario_area <- function(boxes, nRow = 3){
  outer = tags$div();
  
  iRow = 1;
  iCol = 0;
  cur_row = fluidRow();
  for(i in 1:length(boxes)){
    iCol = iCol + 1;
    box = boxes[[i]]
    
    cur_row$children[[iCol]] = column(12/nRow, box);
    if(iCol == nRow){
      iCol = 0;
      outer$children[[iRow]] = cur_row
      cur_row = fluidRow();
      iRow = iRow + 1;
    }
  }
  if(iCol != 0){
    outer$children[[iRow]] = cur_row
  }
  return(outer);
}

scenario_area <- generate_scenario_area(boxes);

foInlineDiv <- function(...){
  tags$div(
    style = "display:inline-block;",
    ...
  )
}

anumericInput <- function(id, title, value, min, max, step=1){
  shinyWidgets::autonumericInput(
    inputId = id, 
    label = title, 
    currencySymbol = "$", 
    align = "left", 
    value = value, 
    currencySymbolPlacement = "p",
    decimalPlaces = 0,
    digitGroupSeparator = ",",
    decimalCharacter = ".",
    minimumValue = min, 
    maximumValue = max
  )
}


mortgage_inputs <- list(
  anumericInput("house_price", "House Price:", 379000, min = 0, max = 100000000, step = 100),
  anumericInput("mortgage_downpayment", "Down Payment + Upfront Fees:", 75800, min = 0, max = 10000000, step = 100),
  selectInput("mortgage_duration", "Mortgage Duration:", c("5 Years", "10 Years", "15 Years", "20 Years", "30 Years"), selected = "15 Years"),
  anumericInput("monthly_fee", "Monthly Payment:", 2432, min = 0, max = 100000, step = 1)
)

dropdown_content = tags$div(style = "min-width:200px;",
  tags$p("Enter monthly costs:"),
  generate_scenario_area(list(
  anumericInput("house_costs_property_tax", "Property Tax:", 650, min = 0, max = 10000000),
  anumericInput("house_costs_insurance", "Insurance:", 150, min = 0, max = 10000000),
  anumericInput("house_costs_hoa_fee", "HOA Fee:", 300, min = 0, max = 10000000),
  anumericInput("house_costs_maintenance", "Maintenance:", 100, min = 0, max = 10000000)
  ), nRow = 2),
  tags$div(style = "margin-left:auto; margin-right:auto; width:80px;", actionButton("house_costs_compute_button", "Compute")),
  # tags$p("All costs are monthly.")
)

house_income_inputs <- list(
  anumericInput("house_rent", "Monthly Rent:", 2300, min = 0, max = 100000, step = 10),
  tags$div(style = "display:flex;",
           anumericInput("house_costs", "Monthly Costs:", 1200, min = 0, max = 100000, step = 10),
           dropdown_options(dropdown_content, title = "Compute House Costs", tooltip = "Click to compute monthly house costs")),
  tags$div(
    style = "vertical-align: top; font-size: 14px;",
    checkboxInput("house_income_invest_earnings", "Invest the earnings", value = T, width = NULL)),
  selectInput("house_income_investment_strategy", "Investment Strategy:", c("Retirement Investment", "Tax-liable Investment", "Protect against inflation"), selected = "Retirement Investment")
)

retirement_limits <- list(
  anumericInput("retirement_ira_max_contribution", "Contribution limit 401(k)", 23500, min = 0, max = 1000000),
  anumericInput("retirement_roth_ira_max_contribution", "Contribution limit Roth IRA", 6500, min = 0, max = 1000000)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
    useToastr(),
    useShinyjs(),
    shinyjs:::extendShinyjs(text = jscode, functions = c("collapse")),
    
    tags$head(
      tags$script(on_ready),
      tags$link(rel="shortcut icon", href="favicon.png")
    ),
    
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
    
    tags$head(tags$style(HTML(gsub("##PLACEHOLDER##", "#227777", "
                .box.box-solid:has(.requiredinputbox) >.box-header {
                background:##PLACEHOLDER##;
                color:#fff;
                }
                .box.box-solid:has(.requiredinputbox){
                border-bottom-color:##PLACEHOLDER##;
                border-left-color:##PLACEHOLDER##;
                border-right-color:##PLACEHOLDER##;
                border-top-color:##PLACEHOLDER##;
                }
                
                .tooltip-inner {
                min-width:130px;
                }
                ")))),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tags$div(id = "sidebar_option_div",tags$div(id = "sidebar_option_div2", 
          optionBox(id = "mortgage_optionbox", title = "Mortgage Options", status = "danger", collapsed = F, class = "requiredinputbox",
            tags$div(id = "mortgage_opts_div",
            generate_scenario_area(mortgage_inputs, nRow=2),
            ),
          ),
          optionBox(id = "house_income_optionbox", title = "House Income", status = "danger", collapsed = F, class = "requiredinputbox",
            generate_scenario_area(house_income_inputs, nRow=2),
            # checkboxInput("house_income_invest_earnings", "Invest the earnings", value = T, width = NULL),
            # selectInput("house_income_investment_strategy", "Investment Strategy:", c("Retirement Investment", "Tax-liable Investment", "Protect against inflation"), selected = "Tax-liable Investment")
            # numericInput("house_yearly_income", "House Yearly Net Income ($):", 4700, min = 0, max = 100000, step = 10),
          ),
          optionBox(id = "investment_optionbox", title = "Investment", 
          sliderInput("investment_gain", "Expected Investment Gain Annual:", 
                      min = 0, max = 20, step  = 0.02, 
                      value = 7.72, post = "%"),
            generate_scenario_area(retirement_limits, nRow = 2)
          ),
          optionBox(id = "tax_optionbox", title = "Tax and Fees",
          sliderInput("income_tax_main", "Income Tax (for house rent):", 
                      min = 0, max = 50, step  = 0.1, 
                      value = 30, post = "%"),
          sliderInput("income_tax", "Tax on Gains/Dividends (for investments):", 
                      min = 0, max = 50, step  = 0.1, 
                      value = 15, post = "%"),
          sliderInput("capital_gains_tax", "Capital Gains Tax (for house resell):", 
                      min = 0, max = 50, step  = 0.1, 
                      value = 15, post = "%"),
          anumericInput("capital_gains_exemption", "Capital Gains Tax Exemption (if primary residence)", 250000, min = 0, max = 10000000, step = 10),
          sliderInput("realtor_commission_fee", "Realtor Commission Fee (on house resell):", 
                      min = 0, max = 10, step  = 0.1, 
                      value = 5, post = "%"),
          ),
          optionBox(id = "inflation_optionbox", title = "Expected Inflation",
            sliderInput("inflation", "Inflation:", 
                        min = 0, max = 10, step  = 0.01, 
                        value = 2.52, post = "%"),
            sliderInput("housing_inflation", "Housing Inflation:", 
                        min = 0, max = 10, step  = 0.01, 
                        value = 2.77, post = "%"),
          ),
          tags$div(id = "account_for_inflation_div", 
          checkboxInput("adjust_by_inflation", "Adjust numbers by inflation", value = T, width = NULL),
          ),
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
          # verbatimTextOutput("verbatimText_main"),
          tags$div(
            style = "max-width:800px;", 
            tags$h3("Hello World!", style = "margin-top:4px;"),
            tags$p(style = "font-size: 16px; text-align:justify; margin-bottom:0px;", 
                   "This is your Mortgage Calculator speaking! If you look outside, you can see the mortgages smiling with only around 5.21% yearly interest...",
                   # tags$br(), 
                   "Enough chitchat. Let's talk business.",
                   # ,tags$p(style = "font-size: 16px; margin-bottom:0px;", "Enough chitchat. Let's talk business."),
                   # "In this tool, you can assess the opportunity cost of getting a mortgage, taking the expected inflation and other investment options into account."
                   # "This is your Mortgage Calculator speaking! Here, you can assess the opportunity cost of getting a mortgage, taking the expected inflation and other investment options into account."
                   # "Welcome to the Opportunity Cost Calculator for Home Buying and Investment! This tool allows you to compare the potential financial outcomes of purchasing a home and living in it, purchasing a home and renting it out, investing in a retirement account, or investing in other financial instruments. By inputting your financial information and desired investment strategy, you can make informed decisions about how to best use your money to achieve your long-term financial goals."
                   ), 
            tags$p(style = "font-size: 16px; text-align:justify; margin-top:0px;",
                   "In this tool, you can assess the opportunity cost of getting a mortgage, taking the expected inflation and other investment options into account. Each box below shows the outcome of an alternative scenario using the money to pay off the mortgage for a different investment."
            ),
            tags$div(
              id = "about_main_div", style = "font-size:17px; margin-bottom:10px;",
              "Need help getting started? ", actionLink("interactiveDemo", " Run Interactive Tutorial"),
            ),
            ), 
          
          tags$div(id = "main_output_div",
            tags$div(
              uiOutput("uioutput_afterxyears_label", style = "font-size: 18px; min-height:23px;"),
            ),
            addchildren(tags$div(style = "max-width:800px;"), boxes)
          ), 
          tags$div(
            style = "font-size: 16px;", 
            uiOutput("uioutput_inflation_adjusted_label", style = "font-size: 15px; color: #333333"),
          )
          # scenario_area,
          # scenario_area,
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe_helpers(withMathJax = TRUE, help_dir = "helpfiles")
  
  observeEvent(input$initialized, {
    # main_logging("Session Initialized")
    # if(!dir.exists("logs/")){
    #   dir.create("logs/")
    # }
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
  })
  
  
    mortgage_duration <- reactive({
      switch(input$mortgage_duration, 
             "5 Years" = a <- 5, 
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
    
    foMonthlyPayment <- function(loan_amount, yearly_interest, numyears){
      monthly_interest = (1 + yearly_interest)^(1/12) - 1;
      nMonth = numyears * 12;
      Y = loan_amount;
      r = monthly_interest;
      t = nMonth;
      X = Y * r / (1 - (1+r)^(-t));
      return(X);
    }
    
    scenarioA_investment <- reactive({
      foSimulator(input$investment_gain/100)
    })
    
    foComputeRetirementInitial <- function(initial, retirement_yearly_max, monthly_mortgage, monthly_inflation, inflation_factor){
      total = 0
      monthly_inflation = 1
      for(iMonth in (1:12)){
        total = total + monthly_mortgage * monthly_inflation
        monthly_inflation = monthly_inflation * ((1+inflation_factor)^(1/12));
      }
      yearly_remain = max(0, retirement_yearly_max - total)
      retirement_initial = max(min(initial, yearly_remain), 0)
    }
    
    foSimulator <- function(yearly_interest, 
                            initial = input$mortgage_downpayment,
                            nMonth = mortgage_duration() * 12, 
                            monthly_mortgage = input$monthly_fee, 
                            investment_tax = (1 - input$income_tax/100),
                            tax = T, 
                            inflation_scaling = F,
                            inflation_factor = input$inflation/100, 
                            adjust_by_inflation = input$adjust_by_inflation,
                            inflation_adjustment = inflation_adjustment_(),
                            retirement = F,
                            retirement_yearly_max = retirement_effective_contribution(),
                            retirement_yearly_inflation = input$inflation/100,
                            retirement_list_output = F
                            ){

      monthly_inflation = 1
      yearly_inflation = 1
      monthly_interest = (1 + yearly_interest)^(1/12) - 1;
      #yearly_interest = input$investment_gain/100
      #income_tax = (1 - input$income_tax/100)
      #monthly_mortgage = input$monthly_fee 
      #initial = input$mortgage_downpayment
      if(tax == FALSE && retirement == FALSE){
        investment_tax = 1;
      }
      if(inflation_scaling == FALSE){
        inflation_factor = 0
      }
      
      if(retirement == TRUE){
        retirement_account = foComputeRetirementInitial(
          initial = initial,
          retirement_yearly_max = retirement_yearly_max, 
          monthly_mortgage = monthly_mortgage, 
          monthly_inflation = monthly_inflation, 
          inflation_factor = inflation_factor
        );
        total = initial - retirement_account
      } else {
        total = initial
        retirement_account = 0
      }
      
      for(iMonth in (1:nMonth)){
        monthly_input = monthly_mortgage * monthly_inflation
        if(retirement == TRUE){
          yearly_max = retirement_yearly_max * yearly_inflation
          retirement_input = max(min(yearly_max/12, monthly_input), 0)
          investment_input = monthly_input - retirement_input
        } else {
          investment_input = monthly_input
          retirement_input = 0
        }
        total = total * (1+monthly_interest*investment_tax) + investment_input;
        retirement_account = retirement_account * (1+monthly_interest) + retirement_input;
        monthly_inflation = monthly_inflation * ((1+inflation_factor)^(1/12));
        if((iMonth %% 12) == 0){
          yearly_inflation = yearly_inflation * (1+retirement_yearly_inflation)
        }
      }
      if(adjust_by_inflation){
        total = total / inflation_adjustment
        retirement_account = retirement_account / inflation_adjustment
      }
      if(retirement == TRUE){
        investment = total
        total = total + retirement_account
        if(retirement_list_output == TRUE){
          return(list(investment = investment, retirement = retirement_account, total = total))
        }
      }
      return(total)
    }
    
    retirement_effective_contribution <- reactive({
      input$retirement_roth_ira_max_contribution + input$retirement_ira_max_contribution * (1 - input$income_tax_main/100)
    })
    
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
    
    scenarioD_housevalue_after_commission <- reactive({
      scenarioD_housenogain() * (1 - input$realtor_commission_fee/100)
    })
    
    
    foCapitalGainsCalc <- function(capital_gains_exemption = F){
      value <- scenarioD_housevalue_after_commission()
      if(input$adjust_by_inflation){
        # Revert the inflation adjustment if applicable
        value = value * inflation_adjustment_() 
      }
      gain = value - input$house_price
      if(capital_gains_exemption){
        taxable_gain = max(gain - input$capital_gains_exemption, 0)
      } else {
        taxable_gain = gain
      }
      total = value - taxable_gain * input$capital_gains_tax/100
      if(input$adjust_by_inflation){
        total = total / inflation_adjustment_() 
      }
      return(total)
    }
        
    
    scenarioD_housevalue_liveinit <- reactive({
      foCapitalGainsCalc(capital_gains_exemption = T)
    })
    
    scenarioD_housevalue_rent <- reactive({
      foCapitalGainsCalc(capital_gains_exemption = F)
    })
    
    investment_strategy_text <- reactive({
      if(input$house_income_invest_earnings){
        switch(input$house_income_investment_strategy,
               "Retirement Investment" = {
                 return("retirement")
               }, 
               "Tax-liable Investment" = {
                 return("investment")
               },
               "Protect against inflation" = {
                 return("savings")
               }
        )
      } else {
        return("savings")
      }
    })
    
    fo_house_income <- function(tax_rent = input$income_tax_main, isprimaryresidence = F){
      if(input$house_income_invest_earnings){
        switch(input$house_income_investment_strategy,
               "Retirement Investment" = {
                 yearly_interest <- input$investment_gain/100
                 tax = T
                 retirement = T
               }, 
               "Tax-liable Investment" = {
                 yearly_interest <- input$investment_gain/100
                 tax = T
                 retirement = F
               },
               "Protect against inflation" = {
                 yearly_interest <- input$inflation/100
                 tax = F
                 retirement = F
               }
        )
      } else {
        yearly_interest = 0
        tax = T
        retirement = F
      }
      # monthly_income = input$house_yearly_income/12
      monthly_income = input$house_rent * (1 - tax_rent/100) - input$house_costs
      
      if(isprimaryresidence){
        house_val <- scenarioD_housevalue_liveinit()
      } else {
        house_val <- scenarioD_housevalue_rent()
      }
      
      x = foSimulator(
        yearly_interest, initial = 0, tax = tax, 
        monthly_mortgage = monthly_income, 
        inflation_scaling = T, inflation_factor = input$housing_inflation/100,
        retirement = retirement, retirement_list_output = T)
      if(retirement == TRUE){
        out = x
      } else {
        out = list(total = x, retirement = 0, investment = x)
      }
      out$house_val = house_val
      out$total = out$total + house_val
      return(out)
    }
    
    scenarioE_houserent_list <- reactive({
      fo_house_income(isprimaryresidence = F)
    })
    
    scenarioE_houseliveinit_list <- reactive({
      fo_house_income(tax_rent = 0, isprimaryresidence = T)
    })
    
    scenarioE_houserent <- reactive({
      scenarioE_houserent_list()$total
    })
    
    scenarioE_houseliveinit <- reactive({
      scenarioE_houseliveinit_list()$total
    })
    
    # retirement_with_max_limit <- reactive({
    #   initial = input$mortgage_downpayment;
    #   monthly_mortgage = input$monthly_fee;
    #   
    #   yearly_max = retirement_effective_contribution()
    #   yearly_remain = max(0, yearly_max - monthly_mortgage * 12)
    #   initial1 = max(min(initial, yearly_remain), 0)
    #   initial2 = initial - initial1
    #   monthly_mortgage1 = max(min(yearly_max/12, monthly_mortgage), 0)
    #   monthly_mortgage2 = monthly_mortgage - monthly_mortgage1
    #   
    #   # foSimulator(input$investment_gain/100, tax = F)
    #   v1 <- foSimulator(initial = initial1, monthly_mortgage = monthly_mortgage1, input$investment_gain/100, tax = F)
    #   v2 <- foSimulator(initial = initial2, monthly_mortgage = monthly_mortgage2, input$investment_gain/100, tax = T)
    #   return(list(retirement = v1, investment = v2))
    # })
    
    retirement_with_max_limit <- reactive({
      foSimulator(yearly_interest = input$investment_gain/100, tax = T, 
                         retirement = TRUE, retirement_list_output = T)
    })
    
    scenarioF_retirement_investment <- reactive({
      x <- retirement_with_max_limit()
      return(x$retirement + x$investment)
    })
    
    scenarioF_retirement_only <- reactive({
      x <- retirement_with_max_limit()
      return(x$retirement)
    })
    
    downpaymentWarning = reactiveVal(FALSE);
    started = reactiveVal(FALSE)
    
    adjusted_star <- reactive({
      if(input$adjust_by_inflation){
        star_ = "*"
      } else {
        star_ = ""
      }
    })
    
    output$verbatimText_main <- renderText({
      
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
          "anumericInput" = {updateAutonumericInput(session, name, value = value)},
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
    
    foUncollapseBoxIfNeeeded <- function(id, collapse = F, nullval = T){
      colid = paste0(id, "_collapse");
      val = input[[colid]]
      if(is.null(val)){
        val = nullval
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
          foUncollapseBoxIfNeeeded("mortgage_optionbox", nullval = F)
          foUncollapseBoxIfNeeeded("house_income_optionbox", nullval = F)
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
    
    output$uioutput_buyhouseandrent <- renderUI({
      value = scenarioE_houserent() - scenarioD_housenogain()
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioE_houserent()*1e-3), adjusted_star())),
        tooltip_buyhouseandrent()
        # paste0(sprintf("Equivalent to: <br> The house + %.1fK", value*1e-3), adjusted_star())
      )
    })
    
    foHouseInvestmentTooltip <- function(val, scenario_output_list){
      if(investment_strategy_text() != "retirement"){
        v4 <- sprintf("+ %.1fK%s in %s", val*1e-3, adjusted_star(), investment_strategy_text())
      } else {
        val = scenario_output_list$retirement
        val2 = scenario_output_list$investment
        vx <- sprintf("+ %.1fK%s in %s", val*1e-3, adjusted_star(), investment_strategy_text())
        if(val2 == 0){
          v4 = vx
        } else {
          v4 = sprintf("%s<br>+ %.1fK%s in %s", vx, val2*1e-3, adjusted_star(), "investment")
        }
      }
      return(v4)
    }
    
    tooltip_buyhouseandliveinit <- reactive({
      val = scenarioE_houseliveinit() - scenarioD_housevalue_liveinit()
      v1 <- paste0(sprintf("House value: %.1fK", scenarioD_housenogain()*1e-3), adjusted_star())
      v2 <- paste0(sprintf("House resell value: %.1fK", scenarioD_housevalue_liveinit()*1e-3), adjusted_star())
      v3 <- sprintf("(after %s%% realtor commission)", input$realtor_commission_fee)
      v4 <- foHouseInvestmentTooltip(val, scenarioE_houseliveinit_list())
      return(paste(v1, v2, v3, v4, sep ="<br>"))
    })
    
    tooltip_buyhouseandrent <- reactive({
      val = scenarioE_houserent() - scenarioD_housevalue_rent()
      v1 <- paste0(sprintf("House value: %.1fK", scenarioD_housenogain()*1e-3), adjusted_star())
      v2 <- paste0(sprintf("House resell value: %.1fK", scenarioD_housevalue_rent()*1e-3), adjusted_star())
      v3 <- sprintf("(after %s%% realtor commission", input$realtor_commission_fee)
      v3_ <- sprintf("and %s%% capital gains tax)", input$capital_gains_tax)
      v4 <- foHouseInvestmentTooltip(val, scenarioE_houserent_list())
      # v4 <- sprintf("+ %.1fK%s in %s", val*1e-3, adjusted_star(), investment_strategy_text())
      return(paste(v1, v2, v3, v3_, v4, sep ="<br>"))
    })
    
    output$uioutput_buyhouseandrent_label <- renderUI({
      rent_after_tax = input$house_rent*(1-input$income_tax_main/100);
      # tags$span(
        # paste0("This scenario assumes the house is purchased to be rented out, resulting in a "),
        tipify(tags$span(paste0("$", rent_after_tax - input$house_costs, "*/mo")),
               paste0(paste0("$", rent_after_tax, " rent (after ", input$income_tax_main, "% tax)", "<br> - $", input$house_costs, " costs"), 
                      "<br>", "*Scales with housing inflation"),
               )
        # "saving.  "
      # )
    })
    
    output$uioutput_buyhouseandliveinit_label <- renderUI({
      # tags$span(
        # paste0("This scenario assumes the house is purchased to be used as a primary residence, making a "),
        tipify(tags$span(paste0("$", input$house_rent - input$house_costs, "*/mo")),
               paste0(paste0("$", input$house_rent, " rent - $", input$house_costs, " costs"),
                      "<br>", "*Scales with housing inflation")
        )
      #   "saving by not paying rent.  "
      # )
    })
    
    output$uioutput_retirement_account_label <- renderUI({
      roth_text = paste0("$", input$retirement_roth_ira_max_contribution, " Roth IRA contribution +")
      
      # "This scenario assumes the money to be used for mortgage is put into a retirement account, providing tax-free gains on the investments."
      # tags$span(
        # paste0("This scenario assumes that up to"),
        tipify(tags$span(paste0("$", retirement_effective_contribution(), "*/year")),
               paste0(paste0(roth_text, "<br>", "$", input$retirement_ira_max_contribution*(1-input$income_tax_main/100), " 401(k) contribution <br> (after ", input$income_tax_main, "% tax)"),
               "<br>", "*Scales with inflation")
        )
        # "of the money is put into a retirement account, providing tax-free gains on the investments."
      # )
    })
    
    output$uioutput_buyhouseandliveinit <- renderUI({
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioE_houseliveinit()*1e-3), adjusted_star())),
        tooltip_buyhouseandliveinit()
        # paste0(tooltip_buyhouseandliveinit(), "<br>", "This scenario assumes $", input$house_rent, " saving is made every month by not paying rent.")
      )
    })
    
    output$uioutput_investment <- renderUI({
      ratio = scenarioA_investment() / scenarioB_inflation()
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioA_investment()*1e-3), adjusted_star())),
        sprintf("%.2fx gain over inflation <br> after %d years", ratio, mortgage_duration())
      )
    })
    
    output$uioutput_retirement <- renderUI({
      retirement_val = scenarioF_retirement_only()
      investment_val = scenarioF_retirement_investment() - retirement_val
      retirement_txt = paste0(sprintf("%.1fK", retirement_val*1e-3), adjusted_star())
      investment_txt = paste0(sprintf("%.1fK", investment_val*1e-3), adjusted_star())
      # ratio = scenarioF_retirement_investment() / scenarioB_inflation()
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioF_retirement_investment()*1e-3), adjusted_star())),
        sprintf("In Retirement Account: %s <br>In Tax-liable Investment: %s", retirement_txt, investment_txt)
        # sprintf("%.2fx gain over inflation <br> after %d years", ratio, mortgage_duration())
      )
      # paste0(sprintf("%.1fK", scenarioF_retirement_investment()*1e-3), adjusted_star())
    })
    
    output$uioutput_keepincheckingaccount <- renderUI({
      ratio = 1 - scenarioC_checking() / scenarioB_inflation()
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioC_checking()*1e-3), adjusted_star())),
        sprintf("%.0f%% value loss against inflation <br> over %d years", 100*ratio, mortgage_duration())
      )
      # paste0(sprintf("%.1fK", scenarioC_checking()*1e-3), adjusted_star())
    })
    
    output$uioutput_protectagainstinflation <- renderUI({
      ratio = scenarioB_inflation() / scenarioC_checking() - 1
      tipify(
        tags$span(paste0(sprintf("%.1fK", scenarioB_inflation()*1e-3), adjusted_star())),
        sprintf("The money has grown by %.0f%% <br> after %d years", 100*ratio, mortgage_duration())
      )
      # paste0(sprintf("%.1fK", scenarioC_checking()*1e-3), adjusted_star())
    })
    
    # output$uioutput_protectagainstinflation <- renderUI({
    #   paste0(sprintf("%.1fK", scenarioB_inflation()*1e-3), adjusted_star())
    # })
    
    output$uioutput_afterxyears_label <- renderUI({
      tags$b(
        id = "afterxyears_label", 
        sprintf("With %.1fK initial capital and $%.0f monthly payments, after %d years:", input$mortgage_downpayment*1e-3, input$monthly_fee, mortgage_duration()),
      )
    })
    
    output$uioutput_inflation_adjusted_label <- renderUI({
      if(input$adjust_by_inflation)
        tags$span(("*"), " Prices are adjusted by inflation and reflect today's prices")
      else
        return("")
    })
    
    observe({
      if(input$house_income_invest_earnings){
        shinyjs::enable("house_income_investment_strategy")
      } else {
        shinyjs::disable("house_income_investment_strategy")
      }
    })
    
    observeEvent(input$house_costs_compute_button, {
      value = input$house_costs_property_tax + input$house_costs_insurance + 
              input$house_costs_hoa_fee + input$house_costs_maintenance;
      updateAutonumericInput(session, "house_costs", value = value)
    })
    
    # uioutput_keepincheckingaccount
    
}

# Run the application 
shinyApp(ui = ui, server = server)
