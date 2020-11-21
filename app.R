
# https://mastering-shiny.org/scaling-modules.html
library(sortable)
library(tidyverse)
library(shiny)


### helpers 
selectVarServer <- function(id, data, filter = is.numeric) {
    stopifnot(is.reactive(data))
    stopifnot(!is.reactive(filter))
    
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(session, "var", choices = find_vars(data(), filter))
        })
        
        list(
            name = reactive(input$var),
            value = reactive(data()[[input$var]])
        )
    })
}

###
filterUi <- function(id) {
    uiOutput(NS(id, "controls"))
}

# make_ui <- function(x, id, var) {
#     if (is.numeric(x)) {
#         rng <- range(x, na.rm = TRUE)
#         sliderInput(id, var, min = rng[1], max = rng[2], value = rng)
#     } else if (is.factor(x)) {
#         levs <- levels(x)
#         selectInput(id, var, choices = levs, selected = levs, multiple = TRUE)
#     } else {
#         # Not supported
#         NULL
#     }
# }

make_ui <- function(x, id, var) {
    if (is.numeric(x)) {
        # Not supported
        NULL
    } else if (is.factor(x)) {
        levs <- levels(x)
        rank_list(
            input_id = id,
            text = var,
            labels = levs,
            options = sortable_options(
                multiDrag = TRUE
            )
        )

    } else if (is.character(x)) {
        rank_list(
            input_id = id,
            text = var,
            labels = x %>% unique(),
            options = sortable_options(
                multiDrag = TRUE
            )
        )
        
    } else {
        # Not supported
        NULL
    }
}

# filter_var <- function(x, val) {
#     if (is.numeric(x)) {
#         !is.na(x) & x >= val[1] & x <= val[2]
#     } else if (is.factor(x)) {
#         x %in% val
#     } else {
#         # No control, so don't filter
#         TRUE
#     }
# }

reorder_var <- function(x, val) {
    if (is.numeric(x)) {
        # No control, so don't order
        x 
    } else if (is.factor(x) | is.character(x)) {
       factor(x, levels = val)
    } else {
        # No control, so don't order
       x
    }
}

reorderServer <- function(id, df, hide_vars =  c("well", "column", "row", "condition")) {
    stopifnot(is.reactive(df))
    
    moduleServer(id, function(input, output, session) {
        vars <- reactive(names(df() %>% select(-all_of(hide_vars))))
        
        output$controls <- renderUI({ 
            map(vars(), function(var) make_ui(df()[[var]], NS(id, var), var))
        })
        
        reactive({
            each_var <- map(vars(), function(var) reorder_var(df()[[var]], input[[var]]))
            each_var
            #reduce(each_var, `&`)
        })
    })
}

##### the app 
filterApp <- function() {
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(
                textOutput("n"),
                filterUi("filter")
            ),
            mainPanel(
                tableOutput("table")    
            )
        )
    )
    
    server <- function(input, output, session) {
        layout <- reactive(readRDS("input_data/sample_layout_file.rds") %>% head())
        order <- reorderServer("filter", layout)
        ordered_layout <- reactive(layout() %>% 
                                       mutate(across(all_of(names(order())), ~ factor(.x, levels = order()[[cur_column()]])))
                                   )
        
        output$table <- renderTable(ordered_layout() )
        output$n <- renderPrint(order() )
    }
    shinyApp(ui, server)
}

filterApp()
