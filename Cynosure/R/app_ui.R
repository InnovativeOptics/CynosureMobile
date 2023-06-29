#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @noRd
oem_data <- read.csv("data/oemDataSearch1.csv") %>%
  filter(Mfg == "Cynosure")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(allowPWA = T,
           options = list(
             dark = F,
             color = "#14ccff"
           ),
           title = "Cynosure Laser Safety Glasses",
           f7SingleLayout(navbar =
                            f7Navbar(
                              title = "Cynosure Laser Eye Protection",
                              hairline = T,
                              shadow = T
                            ),
             img(src = "flair/physician_patient.jpg",
                 width = "100%"),
             f7Block(
               h1(style = "
                           font-size: 30px;
              text-shadow: 3px 3px 3px #ababab;",
                  "Search for protective lenses that are compatible with Cynosure lasers"),
             ),
             f7Block(h2(
                   style = "color: #ff4714;
                           font-size: 28px;
              text-shadow: 3px 3px 3px #ababab;",
                   strong("Select A Laser Model")
                 )),
                 f7Shadow(intensity = 3,
                          f7Card(
                            f7SmartSelect(
                              inputId = "mod",
                              label = h3(strong("Model")),
                              choices = sort(unique(oem_data$`Mod`)),
                              openIn = "popup",
                              virtualList = TRUE
                            )
                          )),
                 uiOutput("laser_specs"),
                 f7Block(f7Margin(h2(
                   strong("Compatible Lenses")
                 )),
                 f7Margin(h3(
                   style = "color: #ff4714",
                   em("(Click the cyan link to view frame styles)")
                 )),
                 uiOutput("linksOEM"),
             f7Align(side = 'center',
             f7Block(
               f7Row(
                 f7Col(
               img(src = "https://www.cynosure.com/wp-content/themes/cynosure/assets/images/layout/logo-cynosure-black.svg",
                   width = "144px")

             ),
             f7Col(
               img(src = "icons/INVO_978.png",
                   width = "72px")
             ))))
           )
    )
  )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    system.file("app/www", package = "Cynosure")
  )

  tags$head(
    favicon(ext = 'png')
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
