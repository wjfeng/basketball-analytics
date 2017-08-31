library(shiny)

server <- function(input, output) {
  current_player <- reactive({
    req(input$player_name)
  })
  chart_type <- reactive({
    req(input$map_type)
  })
  output$court <- renderPlot({
    if(input$player_name == "Willie Cauley-Stein") {
      if(input$map_type == "Pass") {
        wcs.passer
      }
      else if (input$map_type == "Shot") {
        wcs.shooter
      }
    }
    else if (input$player_name == "Kosta Koufos") {
      if(input$map_type == "Pass") {
        koufos.passer
      }
      else if (input$map_type == "Shot") {
        koufos.shooter
      }
    }
    else if (input$player_name == "Garrett Temple") {
      if(input$map_type == "Pass") {
        temple.passer
      }
      else if (input$map_type == "Shot") {
        temple.shooter
      }
    }
    else if (input$player_name == "George Hill") {
      if(input$map_type == "Pass") {
        hill.passer
      }
      else if (input$map_type == "Shot") {
        hill.shooter
      }
    }
    else if (input$player_name == "Zach Randolph") {
      if(input$map_type == "Pass") {
        zbo.passer
      }
      else if (input$map_type == "Shot") {
        zbo.shooter
      }
    }
    else if (input$player_name == "Vince Carter") {
      if(input$map_type == "Pass") {
        vince.passer
      }
      else if (input$map_type == "Shot") {
        vince.shooter
      }
    }
  }
  , width = 800, height = 600
  )
  output$summary_stats_header <- renderText({
    paste(current_player(), chart_type(), "Summary")
  })
  output$summary_stats <- renderTable({
    if(input$player_name == "Willie Cauley-Stein") {
      if(input$map_type == "Pass") {
        wcs.pass.table
      }
      else if (input$map_type == "Shot") {
        wcs.poss.table
      }
    }
    else if (input$player_name == "Kosta Koufos") {
      if(input$map_type == "Pass") {
        koufos.pass.table
      }
      else if (input$map_type == "Shot") {
        koufos.poss.table
      }
    }
    else if (input$player_name == "Garrett Temple") {
      if(input$map_type == "Pass") {
        temple.pass.table
      }
      else if (input$map_type == "Shot") {
        temple.poss.table
      }
    }
    else if (input$player_name == "George Hill") {
      if(input$map_type == "Pass") {
        hill.pass.table
      }
      else if (input$map_type == "Shot") {
        hill.poss.table
      }
    }
    else if (input$player_name == "Zach Randolph") {
      if(input$map_type == "Pass") {
        zbo.pass.table
      }
      else if (input$map_type == "Shot") {
        zbo.poss.table
      }
    }
    else if (input$player_name == "Vince Carter") {
      if(input$map_type == "Pass") {
        vince.pass.table
      }
      else if (input$map_type == "Shot") {
        vince.poss.table
      }
    }
  }
  )
}