library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)
library(httr)
shinyUI(fluidPage(
  #useShinyjs(),  # Include shinyjs
  
  # Application title
  titlePanel("Book a Bike!"),
  
  # Link to main.css stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-datetimepicker.min.css"),
    tags$head(tags$script(src="bootstrap-datetimepicker.min.js"))
  ),
  
  HTML("<script type='text/javascript'>
       $( document ).ready(function() {
       $('#datePicker').datetimepicker({format: 'yyyy-mm-dd hh:ii', endDate: '+4d',
       startDate:'+0d', autoclose: true});
       $('#datePicker').attr('readonly','');
       });
       </script>"),
  
  # Navigation Bar
  HTML("<nav class='navbar navbar-inverse navbar-fixed-top' role='navigation'>
       <div class=''>
       <div class='navbar-header'>
       <button type='button' class='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar' aria-expanded='false' aria-controls='navbar'>
       <span class='sr-only'>Toggle navigation</span>
       <span class='icon-bar'></span>
       <span class='icon-bar'></span>
       <span class='icon-bar'></span>
       </button>
       <a class='navbar-brand' href='#'>Book a Bike</a>
       </div>
       <div id='navbar' class='navbar-collapse collapse'>
       
       </div><!--/.navbar-collapse -->
       </div>
       </nav>"),
  
  HTML("<legend style='padding-top: 5px;'>Reiseauskunft</legend>"),
  
  # 
  # Here does the actual application begin
  #
  
  tags$div(id="removeDiv",
           fluidRow(
             column(6, 
                    textInput("strasse", "Standort: Straße & Straßennr.", value = "", placeholder = "z.B. Königstraße 1")
             ),
             column(6, 
                    selectInput("stadt", "Stadt", c("Stuttgart" = "Stuttgart"), selected = "Stuttgart")
                    #textInput("stadt", "Stadt", value = "", placeholder = " Stuttgart")
             ),
             column(12,
                    textInput("datePicker", "Datum & Uhrzeit", value = as.character(Sys.time())),
                    actionButton("showApp", "Suchen", class="pull-right"),
                    leafletOutput("mymap")
             )
           )
  ),
  
  tags$div(id="parameters",
           HTML("<div class='col-xs-12 parameters-top'>"), 
           HTML("<div class='col-xs-12 col-sm-2'>"), 
           checkboxInput("showCurrent", "Aktuell", value = FALSE),
           HTML("</div>"),
           HTML("<div class='col-xs-12 col-sm-5'>"), 
           sliderInput("radius", "Vorhersageradius in Meter", min=200, max=8200, value=800),
           HTML("</div>"),
           HTML("<div class='col-xs-12 col-sm-5'>"), 
           sliderInput("costRatio", "Kostenverhältnis/Risikoaversion", min=0.1, max=5, value=1.8),
           HTML("</div>"),
           HTML("</div>")
  ),
  tags$footer(id="footer", tags$p("© FZI Seminar - Gruppe 3"))
  
  ))
