#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import purrr
#' @noRd
our_data <- readxl::read_excel("data/Master_2.xlsx",
                               sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))
oem_data <- read.csv("data/oemDataSearch1.csv") %>%
  filter(Mfg == "Cynosure")
app_server <- function(input, output, session) {
selected_data_oem <- eventReactive(input$mod,{
  req(input$mod)
  oem_data %>%
    filter(`Mod` == input$mod)
})
selected_data <- eventReactive(input$mod,{
  req(input$mod)
  map(unique(selected_data_oem()$Lens), ~tibble(filter(our_data, Lens == .x)))
})
output$laser_specs <- renderUI({
  f7Table(card = T,
          select(selected_data_oem(), `Eyewear.Requirement`) %>%
            unique() %>%
            mutate("Selected Laser Specifications (nm)" = Eyewear.Requirement, .keep = "none") %>%
            tibble())
})
output$linksOEM <- renderUI({
  req(selected_data())
  map(1:length(selected_data()), ~f7Shadow(intensity = 5,
                                           f7Card(
                                             f7Row(
                                               f7Col(
                                                 h1(f7Link(
                                                   href = selected_data()[[.x]]$Website,
                                                   label = selected_data()[[.x]]$Lens
                                                 ))),
                                               f7Col(img(src =selected_data()[[.x]]$Image, width = "144px"))),
                                             f7Card(
                                               f7Align(side = 'center',
                                                       img(src = selected_data()[[.x]]$Graph, width = "100%"))),
                                             f7Table(card =T,
                                                     tibble("Qualities/Specifications" = c("Optical Density", "% VLT", "Price (from)"),
                                                            "Values" = c(selected_data()[[.x]]$OD,
                                                                         selected_data()[[.x]]$VLT,
                                                                         selected_data()[[.x]]$`Price(from)`))
                                             )
                                           )
  )
  )

})
}
