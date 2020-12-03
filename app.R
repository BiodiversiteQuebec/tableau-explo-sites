library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(shinyWidgets)


if(!exists("all_obs")){source(here::here("make_local_data", "Manip_climate_data_coleo.R"))}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(

            box(width = 3,
                height = 900,
                selectInput("an",
                            "Année d'échantillonnage des sites",
                            unique(sort(all_obs$obs_year))),
                uiOutput("echan"),
                div(style = "font-size:25px; text-align:center",
                    "Conditions météorologiques du site",
                    tags$sup(circleButton(inputId = "info",
                         label = "",
                         icon = icon("info"),
                         size = "xs",
                         status = "primary"))),
                plotlyOutput("TempPrec")),

            box(width = 6,
                height = 900,
                leafletOutput("map", height = 880)),

            box(width = 3,
                height = 900,
                div(style = "font-size:25px; text-align:center",
                    "Scénarios changements climatiques pour la région",
                    tags$sup(circleButton(inputId = "info2",
                                          label = "",
                                          icon = icon("info"),
                                          size = "xs",
                                          status = "primary"))),
                plotlyOutput("scenarios_temp"),
                plotlyOutput("scenarios_prec"))
        ),

        fluidRow(

            box(width = 6,
                height = 600,
                div(style = "font-size:25px; text-align:center",
                    "Diversité des indicateurs",
                    tags$sup(circleButton(inputId = "info3",
                                          label = "",
                                          icon = icon("info"),
                                          size = "xs",
                                          status = "primary"))),
                selectInput("illu_indic",
                            "Définir l'axe des x",
                            c("occurences", "diversité alpha"),
                            selected = "diversité alpha"),
                checkboxInput("log",
                              "Échelle logarithmique ?",
                              value = TRUE,
                              width = NULL),
                plotlyOutput("waff")),

            box(width = 6,
                height = "auto",
                dataTableOutput("donnees"),
                downloadButton("DL_data", "Télécharger"))
        )
    )
)

# -------------------------------------------------------- #
# ------------ When the magic happens -------------------- #
# -------------------------------------------------------- #

server <- function(input, output, session) {
    # -------- #
    # infos texte
    observeEvent(input$info, {
        showModal(modalDialog(
            title = "Conditions météorologiques du site",
            HTML("Ce graphique décrit la variation des températures mensuelles moyennes et des précipitations cumulées mensuelles au cours de l'année pour le site sélectionné. Les données sont issues de <a href=\"https://earthmap.org\">https://earthmap.org/</a>."),
            easyClose = TRUE,
            footer = NULL))
    })

    observeEvent(input$info2, {
        showModal(modalDialog(
            title = "Scénarios changements climatiques pour la région",
            HTML("Ces graphiques présentent l'évolution des températures et précipitations annuelles entre 1950 et 2007 pour une région données, mais également l'évolution des températures et précipitations prévue dans le cadre des scénarios des changements climatiques. Les données sont extraites de <a href=\"https://www.ouranos.ca/portraits-climatiques/\">https://www.ouranos.ca/portraits-climatiques/</a>."),
            easyClose = TRUE,
            footer = NULL))
    })

    observeEvent(input$info3, {
        showModal(modalDialog(
            title = "Diversité des indicateurs",
            HTML("définition d'un indicateur dont la proportion pour chaque site est représenté en terme de diversité alpha (définition) et de nombre d'occurence."),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    # -------- #
    # Listes déroulantes avec années, puis les types d'échantillonnage correspondants - Listes reliées

    obs_an = reactive({
        all_obs[all_obs$obs_year == input$an,]
    })
    output$echan <- renderUI({
        selectInput("hab_type", "Type d'habitat", c("Tous", sort(unique(obs_an()$hab_type))))
    })

    # -------- #
    # Map output
    output$map <- renderLeaflet({

        if(input$hab_type == "Tous"){

        leaflet() %>%
            addTiles() %>% # Affichage du fond de carte
            addCircleMarkers(lng = obs_an()$long_site, # Positionnement des sites avec les coordonnées long/lat
                             lat = obs_an()$lat_site,
                             radius = 8, # taille du cercle
                             popup = obs_an()$popup_info, # Ajout de fenêtres pop-up
                             color = obs_an()$col,
                             layerId = obs_an()$site_code)



    } else {

        leaflet() %>%
            addTiles() %>% # Affichage du fond de carte
            addCircleMarkers(lng = obs_an()$long_site[obs_an()$hab_type == input$hab_type], # Positionnement des sites avec les coordonnées long/lat
                             lat = obs_an()$lat_site[obs_an()$hab_type == input$hab_type],
                             radius = 8, # taille du cercle
                             popup = obs_an()$popup_info[obs_an()$hab_type == input$hab_type], # Ajout de fenêtres pop-up
                             color = unique(obs_an()$col[obs_an()$hab_type == input$hab_type]),
                             layerId = obs_an()$site_code[obs_an()$hab_type == input$hab_type])

    }

    })
    # ---------------------- #
    # Click on a map marker #
    # --------------------- #

    #
    observe({

        event <- input$map_marker_click

        # Obtention de la description du site et des conditions météorologiques

        output$TempPrec <- renderPlotly({

            if (is.null(event))
                return(NULL)

            cell <- unique(obs_an()$cell_id[obs_an()$site_code == event$id])
            temp <- meteoCELLSdf[meteoCELLSdf$cell_id == cell & meteoCELLSdf$indic_meteo == "Temp",]
            temp$Month <- factor(temp$Month, temp$Month)
            prec <- meteoCELLSdf[meteoCELLSdf$cell_id == cell & meteoCELLSdf$indic_meteo == "Prec",]
            prec$Month <- factor(prec$Month, prec$Month)

            # ---- #

            ay <- list(
                tickfont = list(color = "green"),
                overlaying = "y",
                side = "right",
                title = "Précipitations cumulées (mm)",
                showgrid = F)

            over <- plot_ly()
            over <- over %>% add_lines(x = temp$Month,
                                       y = temp$Value,
                                       type = "scatter",
                                       mode = "lines+markers",
                                       marker = list(symbol = "circle",
                                                     color = 'darkorange'),
                                       line = list(color = "darkorange"),
                                       fill = "tozeroy",
                                       fillcolor = "rgba(255,126,0,0.4)",
                                       name = "Températures")
            over <- over %>% add_trace(x = prec$Month,
                                       y = prec$Value,
                                       name = "Précipitations",
                                       type = "bar",
                                       opacity = 0.5,
                                       orientation = "v",
                                       marker = list(color = "green"),
                                       yaxis = "y2")
            over <- over %>%  layout(title = event$id,
                                     yaxis = list(title = "Températures moyennes (C)",
                                                  tickfont = list(color = "darkorange"),
                                                  showgrid = F),
                                     yaxis2 = ay,
                                     xaxis = list(title = "Mois",
                                                  showgrid = F),
                                     showlegend = TRUE,
                                     legend = list(orientation = 'v', # show entries horizontally
                                                   xanchor = "center", # use center of legend as anchor
                                                   x = 0.5,
                                                   y = -1), #put legend in center of x-axis
                                     margin = list(I = 80,
                                                   r = 80,
                                                   t = 100,
                                                   b = 100,
                                                   autoexpand = TRUE)) %>%
                config(displayModeBar = FALSE) %>%
                layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
                layout(paper_bgcolor = "rgba(254, 247, 234, 0)")
            over
        })
        # Obtention des paramètres météo historiques & les prévisions - Ouranos

        output$scenarios_temp <- renderPlotly({


            if (is.null(event))
                return(NULL)

            #---#
            reg <- unique(obs_an()$Region[obs_an()$site_code == event$id])
            scenar_temp <- scenario_meteo[scenario_meteo$Region == reg & scenario_meteo$param_met == "temp",]

            #---#
            figTemp <- plot_ly(x = scenar_temp$Annee[scenar_temp$Annee <= 2007],
                               y = scenar_temp$Obs[scenar_temp$Annee <= 2007],
                               type = "scatter",
                               mode = "lines",
                               line = list(color = 'darkorange'),
                               name = "Valeurs historiques",
                               showlegend = TRUE)
            figTemp <- figTemp %>% add_trace(x = scenar_temp$Annee[scenar_temp$Annee <= 2007],
                          y = scenar_temp$Hist.Max[scenar_temp$Annee <= 2007],
                          type = "scatter",
                          mode = "lines",
                          line = list(color = 'transparent'),
                          name = "hist_max",
                          showlegend = FALSE)
            figTemp <- figTemp %>% add_trace(x = scenar_temp$Annee[scenar_temp$Annee <= 2007],
                          y = scenar_temp$Hist.Min[scenar_temp$Annee <= 2007],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "hist_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>% add_trace(x = scenar_temp$Annee,
                                             y = scenar_temp$rcp45.Avg,
                                             type = "scatter",
                                             mode = "lines",
                                             line = list(color = 'rgba(153,102,0,1)'),
                                             name = "Scénario avec émissions modérées",
                                             showlegend = TRUE) %>%
                add_trace(x = scenar_temp$Annee,
                          y = scenar_temp$rcp45.Max,
                          type = "scatter",
                          mode = "lines",
                          line = list(color = 'transparent'),
                          name = "emission_moderee_max",
                          showlegend = FALSE) %>%
                add_trace(x = scenar_temp$Annee,
                          y = scenar_temp$rcp45.Min,
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "emission_moderee_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>% add_trace(x = scenar_temp$Annee,
                                             y = scenar_temp$rcp85.Avg,
                                             type = "scatter",
                                             mode = "lines",
                                             line = list(color = "rgba(255,51,51,1)"),
                                             name = "Scénario avec émissions fortes",
                                             showlegend = TRUE) %>%
                add_trace(x = scenar_temp$Annee,
                          y = scenar_temp$rcp85.Max,
                          type = "scatter",
                          mode = "lines",
                          line = list(color = 'transparent'),
                          name = "emission_forte_max",
                          showlegend = FALSE) %>%
                add_trace(x = scenar_temp$Annee,
                          y = scenar_temp$rcp85.Min,
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor= "rgba(255,51,51,0.2)",
                          line = list(color = 'transparent'),
                          name = "emission_forte_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>%
                layout(title = reg,
                       yaxis = list(title = "Températures moyennes (C)",
                                    #tickfont = list(color = "darkorange"),
                                    showgrid = F),
                       xaxis = list(title = "Année",
                                    showgrid = F),
                       showlegend = TRUE,
                       legend = list(orientation = 'h', # show entries horizontally
                                     xanchor = "center", # use center of legend as anchor
                                     x = 0.5,
                                     y = -0.5), #put legend in center of x-axis
                       margin = list(I = 80,
                                     r = 80,
                                     t = 100,
                                     b = 100,
                                     autoexpand = TRUE)) %>%
                layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
                layout(paper_bgcolor = "rgba(254, 247, 234, 0)") %>%
                config(displayModeBar = FALSE)
        })

        #---#
        output$scenarios_prec <- renderPlotly({


            if (is.null(event))
                return(NULL)

                #---#

                reg <- unique(obs_an()$Region[obs_an()$site_code == event$id])
                scenar_prec <- scenario_meteo[scenario_meteo$Region == reg & scenario_meteo$param_met == "prec",]

                #---#
                figPrec <- plot_ly(x = scenar_prec$Annee[scenar_prec$Annee <= 2007],
                                   y = scenar_prec$Obs[scenar_prec$Annee <= 2007],
                                   type = "scatter",
                                   mode = "lines",
                                   line = list(color = 'rgba(0,100,80,1)'),
                                   name = "hist_prec",
                                   showlegend = TRUE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee[scenar_prec$Annee <= 2007],
                                                 y = scenar_prec$Hist.Min[scenar_prec$Annee <= 2007],
                                                 type = "scatter",
                                                 mode = "lines",
                                                 line = list(color = 'transparent'),
                                                 name = "hist_prec_min",
                                                 showlegend = FALSE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee[scenar_prec$Annee <= 2007],
                                                 y = scenar_prec$Hist.Max[scenar_prec$Annee <= 2007],
                                                 type = "scatter",
                                                 mode = "lines",
                                                 fill = "tonexty",
                                                 fillcolor='rgba(0,100,80,0.2)',
                                                 line = list(color = 'transparent'),
                                                 name = "hist_prec_max",
                                                 showlegend = FALSE)
                #---#
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp45.Avg,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 line = list(color = 'rgba(153,102,0,1)'),
                                                 name = "emission_moderees",
                                                 showlegend = TRUE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp45.Min,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 line = list(color = 'transparent'),
                                                 name = "em_mod_min",
                                                 showlegend = FALSE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp45.Max,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 fill = "tonexty",
                                                 fillcolor='rgba(153,102,0,0.2)',
                                                 line = list(color = 'transparent'),
                                                 name = "em_mod_max",
                                                 showlegend = FALSE)
                #---#
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp85.Avg,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 line = list(color = "rgba(255,51,51,1)"),
                                                 name = "emission_fortes",
                                                 showlegend = TRUE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp85.Min,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 line = list(color = 'transparent'),
                                                 name = "em_forte_min",
                                                 showlegend = FALSE)
                figPrec <- figPrec %>% add_trace(x = scenar_prec$Annee,
                                                 y = scenar_prec$rcp85.Max,
                                                 type = "scatter",
                                                 mode = "lines",
                                                 fill = "tonexty",
                                                 fillcolor='rgba(255,51,51,0.2)',
                                                 line = list(color = 'transparent'),
                                                 name = "em_forte_max",
                                                 showlegend = FALSE)
                #---#
                figPrec <- figPrec %>%
                    layout(yaxis = list(title = "Précipitations cumulées (mm)",
                                        showgrid = F),
                           xaxis = list(title = "Année",
                                        showgrid = F),
                           showlegend = TRUE,
                           legend = list(orientation = 'h', # show entries horizontally
                                         xanchor = "center", # use center of legend as anchor
                                         x = 0.5,
                                         y = -0.5), #put legend in center of x-axis
                           margin = list(I = 80,
                                         r = 80,
                                         t = 20,
                                         b = 100,
                                         autoexpand = TRUE)) %>%
                    layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
                    layout(paper_bgcolor = "rgba(254, 247, 234, 0)") %>%
                    config(displayModeBar = FALSE)


        })


        # Obtention de la liste des espèces observées lors de l'échantillonnage TOUTES CAMPAGNES CONFONDUES
        #----------------------------------------------------------------------



        if(is.null(event$id)){
            mess <- unique(all_obs[, c("name", "category")])
            message <- mess[order(mess$name),]
            names(message) <- c("espèce", "catégorie")

            output$donnees <- renderDataTable(message,
                                              options = list(pageLength = 10))
        } else {

            mess <- obs_an()[obs_an()$site_code == event$id, c("name", "category")]
            mess <- unique(mess[c("name", "category")])
            message <- mess[order(mess$name),]
            names(message) <- c("espèce", "catégorie")

            output$donnees <- renderDataTable(message,
                                              options = list(pageLength = 10))
        }



        # Obtention du bar plot pour la répartition du type d'espèces observées TOUTES CAMPAGNES CONFONDUES
        # ------------------------------------------------------------------------


        output$waff <- renderPlotly({
            if(is.null(event$id)){
                if(input$illu_indic == "occurences"){
                    x_tot <- indic_div_coleo$freq_coleo
                    x_tot_2 <- x_tot
                    x_axis_lab <- "Nombre d'occurences"
                    title = "Pour tous les sites"
                    if(input$log == TRUE){
                        type <- "log"
                    } else {
                        type <- "-"
                    }
                } else {
                    x_tot <- indic_div_coleo$alpha_coleo
                    x_tot_2 <- x_tot
                    x_axis_lab <- "Diversité alpha"
                    title = "Pour tous les sites"
                    if(input$log == TRUE){
                        type <- "log"
                    } else {
                        type <- "-"
                    }
                }
            } else {
                if(input$illu_indic == "occurences"){
                    x_tot <- indic_div_sites$freq_site[indic_div_sites$site_code == event$id]
                    x_tot_2 <- indic_div_coleo$freq_coleo
                    x_axis_lab <- "Nombre d'occurences"
                    title = paste("Pour le site", event$id)
                    if(input$log == TRUE){
                        type <- "log"
                    } else {
                        type <- "-"
                    }
                } else {
                    x_tot <- indic_div_sites$alpha_site[indic_div_sites$site_code == event$id]
                    x_tot_2 <- indic_div_coleo$alpha_coleo
                    x_axis_lab <- "Diversité alpha"
                    title = paste("Pour le site", event$id)
                    if(input$log == TRUE){
                        type <- "log"
                    } else {
                        type <- "-"
                    }
                }

            }
                figure <- plot_ly()
                figure <- figure %>% add_trace(x = x_tot,
                                               y = indic_div_coleo$category,
                                               type = "bar",
                                               orientation = "h",
                                               color = indic_div_coleo$category,
                                               colors = "Dark2",
                                               opacity = 1,
                                               name = "Total") %>%
                        add_trace(x = x_tot_2,
                                  y = indic_div_coleo$category,
                                  type = "bar",
                                  orientation = "h",
                                  color = indic_div_coleo$category,
                                  colors = "Dark2",
                                  opacity = 0.1,
                                  name = "Total") %>%
                    layout(title = title,
                           barmode = "overlay",
                           xaxis = list(title = x_axis_lab,
                                        type = type),
                           yaxis = list(title = ""),
                           showlegend = F) %>%
                    config(displayModeBar = FALSE)
                figure



        })
    })



                # } else {
                #
                #     figure <- plot_ly()
                #     #data = site_count,
                #     figure <- figure %>% add_trace(x = as.numeric(site_count$freq_tot),
                #                                    y = site_count$category,
                #                                    type = "bar",
                #                                    orientation = "h",
                #                                    color = site_count$category,
                #                                    colors = "Dark2",
                #                                    opacity = 0.1,
                #                                    name = "Total") %>%
                #         add_trace(x = as.numeric(site_count$freq_site),
                #                   y = site_count$category,
                #                   type = "bar",
                #                   orientation = "h",
                #                   color = site_count$category,
                #                   colors = "Dark2",
                #                   opacity = 1,
                #                   name = "Site") %>%
                #         layout(title = paste("Pour le site", event$id),
                #                barmode = "overlay",
                #                xaxis = list(title = "Occurence"),
                #                yaxis = list(title = ""),
                #                showlegend = F) %>%
                #         config(displayModeBar = FALSE)
                #     figure
                # }

        # Obtention des données à télécharger
        # -----------------------------------

    #     output$DL_data <- downloadHandler(
    #         filename = function() {
    #             paste(event$id, paste("_", unique(obs_an()$obs_year[obs_an()$site_code == event$id]), sep = ""), '.csv', sep="")
    #         },
    #         content = function(file) {
    #             write.csv(message, file)
    #         }
    #     )
    # })

 }

# Run the application
shinyApp(ui = ui, server = server)
