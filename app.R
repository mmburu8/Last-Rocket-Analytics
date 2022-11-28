# install package
# import libraries
library(tidyverse)
library(shiny)
library(readxl)
library(shinythemes)
library(scales)
library(stringi)
#library(hrbrthemes)
library(gridExtra)
library(DBI)
library(RPostgres)
library(reactable)
library(rsconnect)
library(ggh4x)

# get name of title
artistique <- c("Burna Boy", "WSTRN", "Fireboy DML", "Victony", 
                "Bnxn TYE", "DrakeVEVO", "Gunna", "ASAKE", "Quavo Huncho",
                "Kendrick Lamar", "StarBoy TV", "Buruklynboyz", "RugerVEVO",
                "Beyonce", "Megan Thee Stallion", "ProtojeVEVO", "MATATA OFFICIAL", "KoffeeVEVO",
                "NikitaKeringVEVO")
album_head <- c("Love Damini", "WSTRN Season 3", "Playboy", "Outlaw",
                "Bad Since '97", "Her Loss", "DS4EVER",
                "Mr Money With The Vibe", "Only Built For Infinity Links",
                "Mr Morale And The Big Steppers", "More Love Less Ego", 
                "East Mpaka London", "The Second Wave", "Renaissance", "Traumazine",
                "Third Time's The Charm", "Super Morio", "Gifted", "The Other Side")
long <- c("Quavo Huncho", "Kendrick Lamar", "Burna Boy")
med_long <- c("Megan Thee Stallion", "MATATA OFFICIAL")
medium <- c("DrakeVEVO", "ASAKE", "Buruklynboyz", "Beyonce", "Gunna",  "WSTRN",  "ProtojeVEVO", "NikitaKeringVEVO")
short <- c("Fireboy DML", "Victony", "Bnxn TYE", "StarBoy TV", "RugerVEVO", "KoffeeVEVO")

# function
moneylove <- function(dt, rwSiz, colSiz, calzone){
    ggplot(dt, aes(x = viewers, y = Songs))+
    geom_bar(fill="red2", color="mintcream", stat="identity")+
    scale_x_continuous(labels = comma)+
    #theme_ipsum()+
    theme(
        axis.text.x = element_text(size=11, color="gray10"),
        axis.text.y = element_text(size=11, color="gray10"),
        axis.title.x = element_text(size=15, hjust=0.5),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill="gray100", color="gray100"),
        panel.grid.major.x = element_line(colour="snow4"),
        panel.grid.minor.x = element_line(colour="snow4"),
        plot.title = element_text(size=16, face="bold", hjust=0.4),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()
    )+
    force_panelsizes(rows = rwSiz, cols = colSiz, TRUE)+
    labs(x = "views", y="", title = album_head[[match(calzone, artistique)]])
}
# user interface
ui <- fluidPage(theme=shinytheme("cyborg"),
                # use bootstrap to my advantage
                tags$div(class="jumbotron text-center", 
                         style="margin-bottom:0px;margin-top:0px;height:130px",
                         tags$h2(class="jumbotron-heading",
                                 style="margin-bottom:0px;margin-top:0px",
                                 "Album Analytics"),
                         p("What song stole the show?")
                ),
                fluidRow(
                    column(5,
                           selectInput("artist", "Artist",
                                       list(
                                           "Burna Boy" = "Burna Boy",
                                           "WSTRN" = "WSTRN",
                                           "Fireboy DML" = "Fireboy DML",
                                           "Victony" = "Victony",
                                           "Bnxn TYE" = "Bnxn TYE",
                                           "Drake & 21 Savage" = "DrakeVEVO",
                                           "Wizkid" = "StarBoy TV",
                                           "Gunna" =  "Gunna",
                                           "ASAKE" = "ASAKE",
                                           "Quavo & Takeoff" = "Quavo Huncho",
                                           "Kendrick Lamar" = "Kendrick Lamar",
                                           "Buruklynboyz" = "Buruklynboyz",
                                           "Ruger" = "RugerVEVO",
                                           "Megan Thee Stallion" = "Megan Thee Stallion",
                                           "Beyonce" = "Beyonce",
                                           "Koffee" = "KoffeeVEVO",
                                           "MATATA" = "MATATA OFFICIAL",
                                           "Protoje" = "ProtojeVEVO",
                                           "Nikita Kering" = "NikitaKeringVEVO"
                                       )),
                           imageOutput("albums")),
                    column(7,
                           # output is a plot
                           plotOutput("plot", click="plot_click"),
                           tableOutput("dracarys"))
                )
)
# server function
server <- function(input, output, session){

    # summarise data
    data <- reactive({
        # database connect
        youtube <- read_excel("takeoff.xlsx")
        df <- youtube %>%
            filter(channelTitle == input$artist) %>%
            group_by(Songs = stringi::stri_trans_totitle(Song)) %>%
            summarise(viewers = max(viewCount),
                      likes = max(likeCount),
                      comments = max(commentCount)) %>%
            mutate(Songs = fct_reorder(Songs, viewers))
    })

    output$plot <- renderPlot({
        if (input$artist %in% long){
        moneylove(data(), 3, 3.9, input$artist)} else if (input$artist %in% medium){
            moneylove(data(), 2.7, 4.1, input$artist)   
        } else if (input$artist %in% short){
            moneylove(data(), 2, 4, input$artist) 
        } else if (input$artist %in% med_long){
            moneylove(data(), 3, 4.2, input$artist) 
        }
    })
    output$dracarys <- renderTable({
        req(input$plot_click)
        tristan <- nearPoints(data(), input$plot_click, threshold = 10)
        tristan %>%
            mutate(`Freq per 100` = paste(round((as.integer(likes) / as.integer(viewers)) * 100, 3), sep=""),
                   views = comma(as.integer(viewers)),
                   likes = comma(as.integer(likes)),
                   comments = comma(as.integer(comments)))%>%
            select(Songs, views, likes, comments, `Freq per 100`)
    })
    output$albums <-  renderImage({
        picFilePath <- paste(input$artist, ".jpg", sep="")
        return(list(
            src = picFilePath,
            contentType = "image/jpg",
            alt = "Album Cover",
            deleteFile=FALSE
        ))
    })
}

# create Shiny App
shinyApp(ui = ui, server = server)

