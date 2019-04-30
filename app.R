#
# Where do people end up working after their qualification

library(shiny)
library(tidyverse)
# theme_set(theme_bw())
library(grattantheme)
library(plotly)

# Get data --------------------------------------------------------------------

data <- read_rds("data/occ_education.rds")

occs <- data %>% 
    filter(!is.na(occ_group)) %>% 
    pull(occ_group) %>% 
    unique()



# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme = "journal.css",
                
                # Application title
                titlePanel("Educational make-up of jobs in Australia"),
                
                p(paste("The chart below uses ABS Census data to show the education levels of 25-64 year-olds",
                        "who work in specific jobs. Occupations are sorted from most to least workers.")),
                
                # Select
                fluidRow(
                    column(3,
                           selectInput("occ_group",
                                       "Occupation group",
                                       choices = occs, 
                                       selected = "Managers")),
                    
                    column(3,
                           selectInput("age",
                                       "Age",
                                       choices = c("25-34",
                                                   "35-44",
                                                   "45-54",
                                                   "55-64",
                                                   "Compare"),
                                       selected = "Compare")),
                    
                    column(6,  
                           sliderInput("occ_number", 
                                       "Minimum number of workers per occupation",
                                       min = 100, max = 50000, value = 10000))
                    
                ),
                
                h2(textOutput("occ_title")),
                plotOutput("plotted_occs", height = "3600px"),
                
                
                # A bit of JS to record the window size
                tags$head(tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '))
                
)


# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    output$occ_title <- renderText({
            paste0(
                "Education breakdown of '", input$occ_group, "' occupations (per cent of group)"
            )
    })
    
    
    output$plotted_occs <- renderPlot({ 
        
        
        # if(length(input$width) == 0) w <- NULL
        # if(length(input$width) != 0) w <- input$width
        # 
        # # Rotate axis titles if screen is narrow
        rotate <- 90
        just   <- 0
        # suppressWarnings(
        #     if (w <  1200) {
        #         rotate <- 90
        #         just   <- 0
        #     }
        #     )
        # 
        
        max <- data %>% 
            filter(occ_group == input$occ_group) %>% 
            pull(occ_total) %>% 
            max()
        
        if (input$occ_number > max) stop(paste0("There are no ", input$occ_group, " occupations with ", input$occ_number, " workers"))
        
        if (input$age == "Compare") {
            agecol <- c(
                grattan_yellow,
                grattan_lightorange,
                grattan_darkorange,
                grattan_red,
                grattan_darkred
            )
        } 
        
        if (input$age != "Compare") {
            data <- data %>% filter(age == input$age)
            agecol <- case_when(
                input$age == "25-34"   ~ grattan_yellow,
                input$age == "35-44"   ~ grattan_lightorange,
                input$age == "45-54"   ~ grattan_darkorange,
                input$age == "55-64"   ~ grattan_red,
                input$age == "Over 65" ~ grattan_darkred
            )
        }
        
        
        data %>% 
            filter(occ_group == input$occ_group,
                   occ_total > input$occ_number) %>% 
            ggplot(aes(
                x = qual,
                y = pc,
                fill = age,
                alpha = observation_weight)) +
            geom_col(position = "dodge") +
            facet_grid(reorder(occ, -occ_total) ~ sex, switch = "y") +
            grattan_y_continuous() + 
            scale_x_discrete(position = "top") +
            scale_alpha_continuous(range = c(.3, 1)) +
            scale_fill_manual(values = agecol) + 
            theme(strip.placement = "outside",
                  strip.text.y = element_text(size = 14, angle = 180, hjust = 1),
                  strip.text.x = element_text(size = 14),
                  panel.spacing.x = unit(10, "mm"),
                  panel.spacing.y = unit(3, "mm"),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(angle = rotate, hjust = just),
                  legend.position = "top",
                  legend.text = element_text(size = 14),
                  legend.key.size = unit(10, "mm"),
                  legend.title = element_text(size = 14)) +
            labs(fill = "Age",
                 x = "",
                 y = "") +
            guides(alpha = FALSE)
    })
    
    
    
}

# Run --------------------------------------------------
shinyApp(ui = ui, server = server)

