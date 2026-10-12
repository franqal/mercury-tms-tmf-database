# Mercury Trophic Magnification World Base
# Built-in records are synthetic demonstration data, NOT scientific observations.

pkgs <- c("shiny", "bslib", "DT", "leaflet", "dplyr", "readr", "tibble", "htmltools")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages first: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))")

library(shiny); library(bslib); library(DT); library(leaflet)
library(dplyr); library(readr); library(tibble)

# The validated dataset is loaded automatically from Datos.csv.
load_database <- function(path = "Datos.csv") {
  if (!file.exists(path)) {
    stop("No se encontró Datos.csv. Coloca el archivo junto a app.R.")
  }
  x <- readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  names(x) <- trimws(names(x))
  x <- as.data.frame(x)

  required <- c("Ecosystem", "Continent", "Latitude", "Longitude", "TMS_THg")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Datos.csv no contiene las columnas requeridas: ",
         paste(missing, collapse = ", "))
  }

  numeric_cols <- c("Latitude", "Longitude", "TMS_THg", "TMF_THg",
                    "TMS_MeHg", "TMF_MeHg")
  for (nm in numeric_cols) {
    if (!nm %in% names(x)) x[[nm]] <- NA_real_
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  text_cols <- c("Record_ID", "Study_ID", "Reference", "DOI", "Country",
                 "Continent", "Ecosystem", "FW", "Region", "Bioregion",
                 "Data_status")
  for (nm in text_cols) {
    if (!nm %in% names(x)) x[[nm]] <- if (nm == "Data_status") {
      "Validated dataset"
    } else {
      if (nm == "Record_ID") paste0("REC-", seq_len(nrow(x))) else NA_character_
    }
  }
  x$Data_status[is.na(x$Data_status) | x$Data_status == ""] <- "Validated dataset"
  tibble::as_tibble(x)
}

standardize <- function(x) {
  
  names(x) <- trimws(names(x))
  original <- names(x)
  norm <- tolower(gsub("[^a-z0-9]", "", original))
  n <- nrow(x)
  
  find <- function(opts) {
    i <- match(opts, norm)
    i <- i[!is.na(i)]
    if (length(i)) original[i[1]] else NULL
  }
  
  get <- function(opts, default = NA_character_) {
    nm <- find(opts)
    
    if (is.null(nm)) {
      return(rep(default, n))
    }
    
    x[[nm]]
  }
  
  num <- function(opts) {
    suppressWarnings(as.numeric(get(opts)))
  }
  
  chr <- function(opts, default = "Not provided") {
    as.character(get(opts, default))
  }
  
  id <- find(c("recordid", "id"))
  
  tibble::tibble(
    Record_ID = if (is.null(id)) {
      paste0("REC-", seq_len(n))
    } else {
      as.character(x[[id]])
    },
    
    Study_ID = chr(c("studyid", "author", "authors", "study")),
    Reference = chr(c("reference", "citation", "publication", "title")),
    
    DOI = chr(c("doi", "url", "link"), NA_character_),
    Country = chr(c("country", "countryname", "pais")),
    Continent = chr(c("continent", "continente")),
    Ecosystem = chr(c("ecosystem", "ecosystemtype", "habitat", "fw", "foodweb")),
    
    Latitude = num(c("latitude", "lat")),
    Longitude = num(c("longitude", "lon", "long")),
    
    TMS_THg = num(c("tmsthg", "tms")),
    TMS_MeHg = num(c("tmsmehg")),
    TMF_THg = num(c("tmfthg", "tmf")),
    TMF_MeHg = num(c("tmfmehg")),
    
    Data_status = rep("Uploaded dataset", n)
  )
}

ui <- page_sidebar(
  title="Mercury Trophic Magnification World Base",
  theme=bs_theme(version=5, bootswatch="flatly", primary="#176B87"),
  tags$head(tags$style(HTML(".metric-card h2 { font-size: 1.8rem; margin: 0; } .metric-card h6 { margin-bottom: .5rem; }"))),
  sidebar=sidebar(width=310,
    h5("Explore the database"),
    fileInput("file","Upload another CSV (optional)",accept=".csv"),
    helpText("The included Datos.csv loads automatically. You can optionally upload another CSV for the current session."),
    tags$hr(),
    selectInput("metric","Metric",c("TMS","TMF"),"TMS"),
    selectInput("mercury","Mercury form",c("THg","MeHg"),"THg"),
    selectInput("region","Region",c("Southern Hemisphere","Northern Hemisphere","Global"),"Southern Hemisphere"),
    selectInput("ecosystem","Ecosystem","All ecosystems"),
    selectInput("fw","Food web (FW)","All food webs"),
    selectInput("continent","Continent","All continents"),
    textInput("search","Search study, reference, ecosystem or FW"),
    downloadButton("download","Download filtered CSV",class="btn-primary")
  ),
  
  div(
    class = "alert alert-info",
    
    tags$h4("About this resource"),
    
    tags$p(
      "The Mercury Trophic Magnification World Base compiles published ",
      "TMS and TMF estimates for total mercury (THg) and methylmercury ",
      "(MeHg) in aquatic food webs."
    ),
    
    tags$details(
      tags$summary("Authors & affiliations"),
      
      tags$p(
        tags$sup("1,2"),
        " Francisco Quesada-Alvarado; ",
        tags$sup("2,3"),
        " Gustavo Chiang Rojas"
      ),
      
      tags$p(
        tags$sup("1"),
        " Doctorado en Medicina de la Conservación, Universidad Andrés Bello"
      ),
      
      tags$p(
        tags$sup("2"),
        " Centro IRIS"
      ),
      
      tags$p(
        tags$sup("3"),
        " Universidad Mayor"
      ),
      
      tags$p(
        tags$a(
          href = "mailto:franQAL@gmail.com",
          "franQAL@gmail.com"
        ),
        " | ",
        tags$a(
          href = "mailto:gustavo.chiang@umayor.cl",
          "gustavo.chiang@umayor.cl"
        )
      )
    )
  ),
    layout_columns(
    card(class = "metric-card", card_body(h6("Records"), h2(textOutput("n", inline = TRUE)))),
    card(class = "metric-card", card_body(h6("Mapped records"), h2(textOutput("mapped", inline = TRUE)))),
    card(class = "metric-card", card_body(h6("Median estimate"), h2(textOutput("median", inline = TRUE))))
  ),
  navset_card_tab(
    nav_panel("World map",leafletOutput("map",height=560)),
    nav_panel("Records",
      p("TMS and TMF estimates are shown for both THg and MeHg. Blank cells indicate that a value is not available for that record."),
      DTOutput("records")),
    nav_panel("References",DTOutput("refs")),
    nav_panel("Data dictionary",tableOutput("dictionary"))
  ),
  footer=div(style="padding:12px 0;color:#667085;",
             "Mercury Trophic Magnification World Base. Verify original publications, coordinates, units, and calculation methods before citation.")
)

server <- function(input, output, session) {
  data <- reactiveVal(load_database())

  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      raw <- readr::read_csv(
        input$file$datapath,
        show_col_types = FALSE,
        name_repair = "minimal"
      )
      
      names(raw) <- trimws(names(raw))
      
      if (all(c("Ecosystem", "Continent", "Latitude",
                "Longitude", "TMS_THg") %in% names(raw))) {
        
        raw <- as.data.frame(raw)
        
        raw$Latitude  <- as.numeric(raw$Latitude)
        raw$Longitude <- as.numeric(raw$Longitude)
        raw$TMS_THg   <- as.numeric(raw$TMS_THg)
        
        if (!"Study_ID" %in% names(raw)) {
          raw$Study_ID <- "Not provided"
        }
        
        if (!"Reference" %in% names(raw)) {
          raw$Reference <- "Not provided"
        }
        
        if (!"Record_ID" %in% names(raw)) {
          raw$Record_ID <- paste0("REC-", seq_len(nrow(raw)))
        }
        
        if (!"DOI" %in% names(raw)) {
          raw$DOI <- NA_character_
        }
        
        if (!"Country" %in% names(raw)) {
          raw$Country <- "Not specified"
        }
        
        if (!"TMS_MeHg" %in% names(raw)) {
          raw$TMS_MeHg <- NA_real_
        }
        
        if (!"TMF_THg" %in% names(raw)) {
          raw$TMF_THg <- NA_real_
        }
        
        if (!"TMF_MeHg" %in% names(raw)) {
          raw$TMF_MeHg <- NA_real_
        }
        
        if (!"Data_status" %in% names(raw)) {
          raw$Data_status <- "Uploaded dataset"
        }
        
        data(tibble::as_tibble(raw))
        
      } else {
        stop("El CSV no contiene las columnas esperadas. Revisa names(raw).")
      }
      showNotification("CSV loaded. Check the mapped columns and records.")
    }, error=function(e) showNotification(paste("CSV error:",conditionMessage(e)),type="error",duration=NULL))
  })
  observe({
    d <- data()
    ec <- sort(unique(d$Ecosystem[!is.na(d$Ecosystem) & d$Ecosystem != ""]))
    fw <- sort(unique(d$FW[!is.na(d$FW) & d$FW != ""]))
    co <- sort(unique(d$Continent[!is.na(d$Continent) & d$Continent != ""]))
    updateSelectInput(session,"ecosystem",choices=c("All ecosystems",ec))
    updateSelectInput(session,"fw",choices=c("All food webs",fw))
    updateSelectInput(session,"continent",choices=c("All continents",co))
  })

  selected <- reactive({
    d <- data(); col <- paste0(input$metric,"_",input$mercury)
    if (!col %in% names(d)) d[[col]] <- NA_real_
    d <- d %>% filter(!is.na(.data[[col]]))
    if(input$region=="Southern Hemisphere") d <- d %>% filter(!is.na(Latitude),Latitude<0)
    if(input$region=="Northern Hemisphere") d <- d %>% filter(!is.na(Latitude),Latitude>=0)
    if(!is.null(input$ecosystem) && input$ecosystem!="All ecosystems") d <- d %>% filter(Ecosystem==input$ecosystem)
    if(!is.null(input$fw) && input$fw!="All food webs") d <- d %>% filter(FW==input$fw)
    if(!is.null(input$continent) && input$continent!="All continents") d <- d %>% filter(Continent==input$continent)
    q <- trimws(input$search %||% "")
    if(nzchar(q)) {
      s <- paste(d$Study_ID,d$Reference,d$Ecosystem,d$FW,d$Continent)
      d <- d[grepl(q,s,ignore.case=TRUE),,drop=FALSE]
    }
    d
  })

  output$n <- renderText(nrow(selected()))
  output$mapped <- renderText(sum(is.finite(selected()$Latitude)&is.finite(selected()$Longitude)))
  output$median <- renderText({
    d <- selected(); col <- paste0(input$metric,"_",input$mercury)
    if(!nrow(d)||!any(is.finite(d[[col]]))) "NA" else sprintf("%.3f",median(d[[col]],na.rm=TRUE))
  })

  output$map <- renderLeaflet({
    d <- selected(); col <- paste0(input$metric,"_",input$mercury)
    d <- d %>% filter(is.finite(Latitude),is.finite(Longitude),is.finite(.data[[col]]))
    m <- leaflet() %>% addTiles(
      urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
      attribution = '&copy; OpenStreetMap contributors'
    )
    if(nrow(d)) {
      d$popup_text <- paste0("<b>",htmltools::htmlEscape(d$Study_ID),"</b><br>Country: ",htmltools::htmlEscape(d$Country),
        "<br>Continent: ",htmltools::htmlEscape(d$Continent),"<br>Ecosystem: ",htmltools::htmlEscape(d$Ecosystem),
        ifelse(is.na(d$FW) | d$FW=="","","<br>FW: " %+% htmltools::htmlEscape(d$FW)),
        "<br>",input$metric," (",input$mercury,"): ",signif(d[[col]],4),
        "<br>Reference: ",htmltools::htmlEscape(d$Reference),
        ifelse(is.na(d$DOI)|d$DOI=="","","<br>DOI: " %+% htmltools::htmlEscape(d$DOI)))
      m <- m %>% addCircleMarkers(data=d,lng=~Longitude,lat=~Latitude,radius=6,
        color="#176B87",fillColor="#2AA6B8",fillOpacity=.8,weight=1,popup=~popup_text)
    }
    m %>% setView(lng=0,lat=-20,zoom=2)
  })

  output$records <- renderDT({
    d <- selected() %>%
      select(Record_ID,Study_ID,Reference,Continent,Region,Bioregion,Ecosystem,FW,
             Latitude,Longitude,TMS_THg,TMF_THg,TMS_MeHg,TMF_MeHg,
             Data_status,Country,DOI)
    datatable(d,rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })
  output$refs <- renderDT({
    d <- selected() %>% distinct(Study_ID,Reference,Continent,Region,Bioregion,Ecosystem,FW) %>% arrange(Reference)
    datatable(d,rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })
  output$dictionary <- renderTable({
    tibble(Field=c("Latitude","Longitude","TMS_THg / tms","TMS_MeHg / tmsMeHg","TMF_THg / tmf","TMF_MeHg / tmfMeHg","Ecosystem","FW","Region","Bioregion","Continent / Continente","Reference / Authors","DOI / URL"),
      Meaning=c("Decimal degrees; south negative","Decimal degrees; west negative","TMS for total mercury","TMS for methylmercury","TMF for total mercury","TMF for methylmercury","Ecosystem type","Food-web category provided in the source dataset","Geographic region","Biogeographic region","Continent","Citation or author/year","DOI or stable publication URL"))
  },striped=TRUE,bordered=TRUE,spacing="s")

  output$download <- downloadHandler(
    filename=function() paste0("mercury_database_",tolower(input$metric),"_",tolower(input$mercury),"_",Sys.Date(),".csv"),
    content=function(file) {
      d <- selected(); col <- paste0(input$metric,"_",input$mercury)
      d$Selected_metric <- d[[col]]
      write_csv(d,file,na="")
    }
  )
}
# Helper for joining popup strings.
`%+%` <- function(a,b) paste0(a,b)
shinyApp(ui,server)
