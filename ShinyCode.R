#server.r file

#loaded needed packages
library(shiny)
library(tidyr)
library(leaflet)
library(readr)
library(dplyr)
#then, set up data
pal3 <- colorFactor(topo.colors(10),
                    domain  = c("45-99", "100-199", "200-299", "300-399", "400-499", "500-599", "600+", "NoDate")
) # set colors
new_data <- read_csv("DataDownload.csv") #readin data from database 
new_df <- new_data %>% unite(Artifact_present,Artifact_Class, Present, sep="_" )  #get rid of things I don't need
new_df_selected <- new_df %>% select(Site_name:citation) 
df_2 <- new_df_selected
df_2$group <- factor(df_2$Group, levels = c( "45-99", "100-199", "200-299", "300-399", "400-499", "500-599", "600+", "NoDate"))
temp_date <- df_2 %>% spread(key = Artifact_present, value =citation) #put table into right format for dates
temp_date2 <- temp_date %>% select(Site_name:Hominin) # select only needed tables to display
full_table <- temp_date %>% select(Site_name:Group, ends_with("yes") ) #table for full search

#setup the server
#put map on main page
server <- function(input, output) {
  filtered_data <- reactive({df_2[df_2$Artifact_present == input$ArtifactTypes, ] })
  output$content <- renderDataTable({
    df_2[df_2$Artifact_present == input$ArtifactTypes, ] #table for artifact types
  }, options = list(orderClasses = TRUE, lengthMenu = c(5,10,50), pageLength = 10 ))
  output$date_table <- renderDataTable({ 
    temp_date2[temp_date2$Group == input$date_choose, ] #here, changing to temp_date
  }, options = list(orderClasses = TRUE, lengthMenu = c(5,10,50), pageLength = 10 )) #JUST ADDED.
  
  output$map <- renderLeaflet({
    leaflet(df_2) %>% addTiles() %>% addLegend("topright", pal=pal3, values =~group, title = "ages in ka")
  })
  observe({
    
    leafletProxy("map", data = filtered_data()) %>% clearMarkers() %>% addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Site_name), color = ~pal3(group), stroke = FALSE, fillOpacity = 0.5 )
    
    
  })
  #below for date information
  filtered_data_dates <- reactive({df_2[df_2$group == input$date_choose, ] }) #changed name of df_2 to temp_date
  output$date_map <- renderLeaflet({
    leaflet(df_2) %>% addTiles() %>% addLegend("topright", pal=pal3, values =~group, title = "ages in ka")
  })
  observe({
    leafletProxy("date_map", data = filtered_data_dates()) %>% clearMarkers() %>% addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Site_name), color = ~pal3(group), stroke = FALSE, fillOpacity = 0.5 )
  })
  output$genmap <-
    renderLeaflet({
      leaflet(full_table) %>% addTiles() %>%
        addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Site_name), color = ~pal3(Group), stroke = FALSE, fillOpacity = 0.5 ) %>%
        addLegend("topright", pal=pal3, values =~Group, title = "ages in ka")})
  
}




#ui.r file
#load needed packages
library(shiny)
library(shinydashboard)
library(leaflet) 

ui <- dashboardPage(
  dashboardHeader(title = "WISDOM database"), #title of site
  dashboardSidebar(  #setup the sidebar
    sidebarMenu(
      menuItem("Welcome page", tabName = "overview", icon = icon("info")),
      menuItem("Search by type", tabName = "by_type", icon = icon("wrench")),
      menuItem("Search by date", tabName = "by_date", icon = icon("calendar")),
      menuItem("Background and contact info", tabName = "background", icon = icon("university"))
      
    )
  ),
  dashboardBody(    #put info into each sidebar page
    tabItems(
      tabItem(tabName = "by_date",
              h2("Search by age"),
              fluidRow(
                box(
                  title = "Age group", status = "primary", solidHeader = TRUE,
                  "select an age range",
                  
                  selectInput("date_choose", "Choose Age Group",choices = c("45-99 ka" = "45-99", "100-199 ka" = "100-199", "200-299 ka" = "200-299", "300-399 ka" = "300-399",  "400-499 ka" = "400-499", "500-599 ka" = "500-599",  "600+ ka" = "600+"), selected = NULL)
                )
                ,
                
                box(
                  title = "Map of sites with chosen age group (in ka). Note that some locations are estimated.", 
                  status = "warning", solidHeader = TRUE,
                  leafletOutput("date_map")
                  
                ),
                box(
                  title = "data table", width = 12,
                  dataTableOutput("date_table")
                  
                  
                  
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "by_type",
              h2("Search by type"),
              fluidRow(
                box(
                  title = "Aritfact type", status = "primary", solidHeader = TRUE,
                  "Select an artifact type", br(),
                  "See 'overview' for details on artifact types",
                  selectInput("ArtifactTypes", "Artifact Type",choices = c("art" = "art_yes", "bone" = "bone_yes", "engraved ochre" = "engravedOchre_yes", "engraved bone" = "engravedBone_yes",  "engraved eggshell" = "engravedEggshell_yes", "engraved stone" = "engravedStone_yes",  "engraved ochre" = "engravedOchre_yes", "hafting" = "hafting_yes", "exotic" = "exotic_yes", "ornamentation" = "ornamentation_yes","ochre" = "ochre_yes", "engraved" = "engraved_yes", "wooden" = "wooden_yes"), selected = NULL)
                )
                ,
                
                box(
                  title = "Map of sites with chosen artifact type. Note that some locations are estimated.", 
                  status = "warning", solidHeader = TRUE,
                  leafletOutput("map")
                  
                ),
                box(
                  title = "data table", width = 12,
                  dataTableOutput("content")
                  
                  
                  
                )
              )
              
      ),
      #Overview page Updating with tabboxes
      tabItem(tabName = "overview",
              h2("Overview of project"),
              fluidRow(
                tabBox(title ="General information", width = 12,
                       tabPanel("Project overview",
                                strong("Welcome to the database of Worldwide Instances of Symbolic Data Outlining Modernity"),
                                br(),
                                p("This database is designed to aid in answering the connected questions of how many examples of symbolic expression exist in the Pleistocene archaeological record and if there are geographic or temporal patterns that hint at the origin of these behaviors. While we acknowledge significant debates about what constitutes 'symbolic' artifacts, this database includes the wide range of published and/or assessed materials broadly placed into the symbolic category in order to maximize familiarity across the available data for such debates."),
                                p("The map below shows all of the sites recorded in the database. Clicking on a site brings up the site name. To use this site, select either the 'search by type' or 'search by date' tab "),
                                strong("To gain access to the main database, with full details for each item and site, click the link below:"),
                                a("Link to Filemaker Database", href= "https://n25.fmphost.com/fmi/iwp/res/iwp_home.html", target="_blank"),
                                p("the username is 'Anthropology' and the password is 'Boas' "),
                                p(" Once you connect, you will see a GUI version of the database. The upper left box controls the ‘layout.’ Switching to “ArtifactsStandard” will bring you to an individual instance, where you can see the artifact itself and any relevant notes about the object. Clicking on ‘Find’ allows the user search by different parameters. For example, to bring up all the engraved ochre, click on the dropdown box above “Representation Type” and select ‘Engraved ochre’. Clicking on ‘perform find’ will run the search."),
                                p("we are currently working on a protocol to allow scholars to update the site. For now, please email us with any corrections or updates at mkissel@nd.edu"),
                                br(),
                                strong("Please cite as Kissel and Fuentes 2017."),
                                br(),
                                br(),
                                strong("Map of all sites in database. Some site locations estimated based on available data. Please contact us with location updates."),
                                leafletOutput("genmap")
                       )#close first tab panel
                       ,
                       tabPanel("Funding",
                                strong("Funding for the project:"),
                                p("This project/publication was made possible through the support of a grant from the John Templeton Foundation. The opinions expressed in this site are those of the authors and do not necessarily reflect the views of the John Templeton Foundation."),
                                p("Thanks also to the University of Notre Dame")
                                
                       )#close second TabPanel
                ) # close tabbox
              )#close fluidrow
              
              
              
      ),# close overview Tab
      
      tabItem(tabName = "background",
              h2("Background on project creation and implementation"),
              fluidRow(
                tabBox(title ="Background on the project", width = 12,
                       tabPanel("Database creation",
                                strong("Background on the database:"),
                                p("Before constructing the database, we explored preexisting datasets as a way to compare methods and as a source of primary and secondary data (d’Errico F. et al., 2003; Langley et al., 2008; Anderson, 2012; Lombard, 2012; Texier et al., 2013; e.g., Villa and Roebroeks, 2014; Kandel et al., 2015). Some sources, such as Bednarik and colleagues (Bednarik, 1995, 2005, 2013; Bednarik et al., 2005; Beaumont and Bednarik, 2013) and Marshack (Marshack, 1976, 1991) were useful as they discuss often under-studied examples. Other sources provided more nuanced studies of specific artifacts types.
                                  This database was created in FileMaker Pro 12. To construct the database, we define each instance based on the description of an archaeological type that fits into one the categories of symbolic objects (see Kissel and Fuentes, in review). When possible each artifact receives its own entry. Ochre is often counted by weight (in these cases, we note the sample size) and in some situations data are reported in aggregate (i.e., a site will report 25 engraved ostrich eggshells without details on individual artifacts. Once detailed information is available, we hope to expand these entries). 
                                  
                                  For dates, we use the age range given either in the original report or in subsequent publications on the site’s stratigraphy, along with the dating method used, when available. The relational nature of this database allows for updating when new dating methods are applied to an archaeological layer/site, thus keeping it up-to-date rather than static, as most published datasets are by definition. Future work may better contextualize these results. Artifacts that do not have reliable calendrical dates are placed into general age categories based on their stone tool typologies. Finally, those that are only dated to gross levels (e.g., “Middle Stone Age deposits”) are recorded but only used when contextual information allows. 
                                  Each entry in the main database is accompanied by contextualizing datasets, enabling researchers to ask different questions and to examine data by site, time period, geographic region, or artifact type. The database holds relevant information on specific sites, including when excavations took place, where the materials are stored (if known), details on the stratigraphic layers, the geographic coordinates, and a general description of the site itself, to aid in understanding the context of each artifact. For engraved objects, we note the design pattern. For sites where the exact GPS coordinates were unknown, site location was estimated based on the nearest known entity. When possible, each artifact includes information on its size, raw material, distance to the source from its find location, the primary and possible secondary uses for the object, if it is burnt, shows usewear, or has markings/stains and any relevant notes on the artifact itself."),
                                br(),
                                br(),
                                p("For the purposes of this site Exotic refers to artifacts that are not local to the region. For example, at Wonderwerk Cave in South Africa exotic manuports made from a variety of crystals were purposefully brought to the back of the cave (Chazan and Horwitz, 2009). Lithic refers to stone tool technology that has been argued to imply complex cognitive function, such as hafting which may require the use of pyrotechnology (Wadley et al., 2009). Art refers to artifacts that have been argued to be representations of natural or artificial object, such as cave paintings (Aubert et al., 2014) or figurines (d’Errico and Nowell, 2001; Bednarik, 2003). Ornamentation are artifacts that were most likely worn, such as shell beads (d’Errico et al., 2005). Engraved artifacts are those in which an individual purposefully marked an artifact by drawing a tool over its surface, be it stone (Hovers et al., 1997), bone (Vogelsang et al., 2010), eggshell (Texier et al., 2013), or ochre (Henshilwood et al., 2009). Bone refers to bone tools, such as those found at Schoningen (van Kolfschoten et al., 2015). The last category, ochre, refers to the presence of ochre nodules, which may have been used as a colorant (for this category, as some publications record the presence by weight and others by individual pieces, it is hard to judge sample size). 
                                  
                                  "),
                                strong("Data assessment:"),
                                p(" There is no consensus on what makes an artifact symbolic. Archaeological sites are palimpsests of activity (Dominguez-Rodrigo, 2002; Bolus, 2004), and interpreting individual artifacts is often difficult. A case in point is a purported ‘lion figurine’ from the Lower Paleolithic of India (Rajendran, 2012). The object, while undated, is suggested to be an early example of paleoart. However, the lack of detailed taphonomic description makes it difficult to assess its legitimacy. That, plus a lack of contextual information (Chauhan, 2013), puts the claim into question. Even well-known examples such as the Bilzingsleben markings (Mania and Mania, 2005) have been questioned by skeptics (Davidson, 1990) on taphonomic grounds. However, after extensive review of the literature, and the debates, we included artifacts in the database if they fit at least three of the following criteria:
                                  1.	Published in a peer-reviewed article or book chapter.
                                  2.	Had an illustration/picture of the artifact or sufficient data-based descriptions that supported the assertion of potentially symbolic representation.
                                  3.	Had contextual information to assess its age (though in context of surface finds this was relaxed).
                                  4.	Has undergone some taphonomic study.
                                  5.	Referenced/discussed as a valid symbolic artifact in at least two publications by different scholars.
                                  "),
                                strong("to contact us please email at mkissel@nd.edu")
                                
                                )#close first tab panel
                       ,
                       tabPanel("References",
                                strong("References for background"),
                                p("Anderson, H., 2012. Crossing the Line: The Early Expression of Pattern in Middle Stone Age Africa. Journal of World Prehistory. 25, 183–204."),
                                p("Aubert, M., Brumm,  a., Ramli, M., Sutikna, T., Saptomo, E.W., Hakim, B., Morwood, M.J., van den Bergh, G.D., Kinsley, L., Dosseto,  a., 2014. Pleistocene cave art from Sulawesi, Indonesia. Nature. 514, 223–227."),
                                p("Beaumont, P.B., Bednarik, R.G., 2013. Tracing the emergence of palaeoart in sub-Saharan Africa. Rock Art Research. 30, 33–54."),
                                p("Beaumont, P.B., Morris, D., 1990. Guide to archaeological sites in the Northern Cape. McGregor Museum, Kimberley."),
                                p("Bednarik, R.G., 1995. Concept-Mediated Marking in the Lower Palaeolithic. Current Anthropology. 36, 605–634."),
                                p("Bednarik, R.G., 2005. Middle Pleistocene beads and symbolism. Anthropos. 100, 537–552."),
                                p("Bednarik, R.G., 2013. Lower Palaeolithic Rock Art of India and Its Global Context. Indian journal of physical anthropology and human genetics. 32, 113–142."),
                                p("Bednarik, R.G., Kumar, G., Watchman, A., Roberts, R.G., 2005. Preliminary Results of the EIP Project. Rock Art Research. 22, 147–197."),
                                p("Bolus, M., 2004. Settlement analysis of sites of the Blattspitzen complex in central Europe. In: Conard, N.J. (Ed.), Settlement Dynamics of the Middle Paleolithic and Middle Stone Age II. Kerns Verlag, Tubingen, pp. 201–226."),
                                p("Chauhan, P., 2013. Lion figurine from Abhayagiri. Current Science. 105, 751–752."),
                                p("Chazan, M., Horwitz, L.K., 2009. Milestones in the development of symbolic behaviour: a case study from Wonderwerk Cave, South Africa. World Archaeology. 41, 521–539."),
                                p("Clark, J.D., 1963. Prehistoric cultures of northeast Angola and their significance in tropical Africa. Museu do Dundo Publicacões Culturais, Lisbon."),
                                p("d’Errico F., Henshilwood C., Lawson G., Vanhaeren M., Tillier A.M., Soressi M., Bresson F., Maureille B., Nowell A., Lakarra J., Backwell L., M., J., 2003. Archaeological Evidence for the Emergence of Language, Symbolism, and Music—An Alternative Multidisciplinary Perspective. Journal of World Prehistory. 17, 1–70."),
                                p("d’Errico, F., Nowell, A., 2001. A New Look at the Berekhat Ram Figurine: Implications for the Origins of Symbolism, Cambridge Archaeological Journal."),
                                p("d’Errico, F., Villa, P., 1997. Holes and Grooves: The Contribution of Microscopy andTaphonomy to the Problem of Art Origins. Journal of Human Evolution.  33, 1–31."),
                                p("Davidson, I., 1990. Bilzingsleben and early marking. Rock Art Research. 7, 52–56."),
                                p("de Lumley, H., 1966. Les fouilles de Terra Amata á Nice: premiers resultats. Bulletin du Musée d’Anthropologie Préhistorique de Monaco. 13, 29–51."),
                                p("Dominguez-Rodrigo, M., 2002. Hunting and scavenging by early humans: The state of the debate. Journal of World Prehistory. 16, 1–54."),
                                p("García-Díez, M., Ochoa, B., Barandiarán, I., 2013. Neanderthal graphic behaviour. The pecked pebble from Axlor rockshelther (Northern Spain). Journal of Anthropological Research. 69, 397–410."),
                                p("Henshilwood, C.S., D’Errico, F., Watts, I., 2009. Engraved ochers from the Middle Stone Age levels at Blombos Cave, South Africa. Journal of Human Evolution. 57, 27–47."),
                                p("Hovers, E., Vandermeersch, B., Bar-Yosef, O., 1997. A Middle Palaeolithic engraved artifact from Qafzeh Cave, Israel. 14, 79–87."),
                                p("Kandel, A.W., Bolus, M., Bretzke, K., Bruch, A.A., Haidle, M.N., Hertler, C., Märker, M., 2015. Increasing Behavioral Flexibility? An Integrative Macro-Scale Approach to Understanding the Middle Stone Age of Southern Africa. Journal of Archaeological Method and Theory. 23, 623–668."),
                                p("Khan, K.S., Kunz, R., Kleijnen, J., Antes, G., 2003. Five steps to conducting a systematic review. Journal of the Royal Society of Medicine. 96, 118–121."),
                                p("Langley, M.C., Clarkson, C., Ulm, S., 2008. Behavioural Complexity in Eurasian Neanderthal Populations: a Chronological Examination of the Archaeological Evidence. Cambridge Archaeological Journal. 18, 289–307."),
                                p("Lombard, M., 2012. Thinking through the Middle Stone Age of sub-Saharan Africa. Quaternary International. 270, 140–155."),
                                p("Mania, D., Mania, U., 2005. The natural and sociocultural environment of Homo erectus at Bilzingsleben, Germany. In: Gamble, C., Porr, M. (Eds.), The Hominid Individual in Context: Archaeological Investigations of Lower and Middle Palaeolithic Landscapes, Locales and Artifacts. Routledge, New York, pp. 98–114."),
                                p("Marshack, A., 1976. Some Implications of the Paleolithic Symbolic Evidence for the Origin of Language. Annals of the New York Academy of Sciences. 280, 289–311."),
                                p("Marshack, A., 1991. A reply to Davidson on Mania & Mania. Rock Art Research. 8, 47–58."),
                                p("Mason, R., Brain, C.K., 1988. Cave of Hearths, Makapansgat, Transvaal. Archaeological Research Unit, University of the Witwatersrand."),
                                p("Patou-Mathis, M., 1995. Etude préliminaire de certaines pièces osseuses de Stránska Skála (Moravie) présentant des stigmates d’intervention humaine. In: Musil, R. (Ed.), Stránska Skála Hill. Moravian Museum, Brno, pp. 169–"),
                                p("Rajendran, P., 2012. A lion figurine with non-Acheulian Lower Palaeolithic implements. Current Science. 102, 1260–1261."),
                                p("Texier, P., Porraz, G., Parkington, J., Rigaud, J., Poggenpoel, C., Tribolo, C., 2013. The context , form and significance of the MSA engraved ostrich eggshell collection from Diepkloof Rock Shelter , Western Cape , South Africa. Journal of Archaeological Science. 40, 3412–3431."),
                                p("Valoch, K., 1987. the Early Palaeolithic Site Stránská Skála I Near Brno ( Czechoslovakia ). Anthropologie. 25, 125–142."),
                                p("Cape , South Africa. Journal of Archaeological Science. 40, 3412–3431.
                                  van Kolfschoten, T., Parfitt, S.A., Serangeli, J., Bello, S.M., 2015. Lower Palaeolithic bone tools from the “Spear Horizon” at Schöningen (Germany). Journal of Human Evolution. 89, 226–263.
                                  "),
                                p("Villa, P., Roebroeks, W., 2014. Neandertal demise: An archaeological analysis of the modern human superiority complex. PLoS ONE. 9, e96424."),
                                p("Vogelsang, R., Richter, J., Jacobs, Z., Eichhorn, B., Linseele, V., Roberts, R.G., 2010. New Excavations of Middle Stone Age Deposits at Apollo 11 Rockshelter, Namibia: Stratigraphy, Archaeology, Chronology and Past Environments. Journal of African Archaeology. 8, 185–218."),
                                p("Wadley, L., Hodgskiss, T., Grant, M., 2009. Implications for complex cognition from the hafting of tools with compound adhesives in the Middle Stone Age, South Africa. Proceedings of the National Academy of Sciences of the United States of America. 106, 9590–4."),
                                p("Watts, I., Chazan, M., Wilkins, J., 2016. Early Evidence for Brilliant Ritualized Display: Specularite Use in the Northern Cape (South Africa) between ∼500 and ∼300 Ka. Current Anthropology. 57, 287–310"),
                                p("Wendt, W.., 1975. Notes on some unusual artefacts from South West Africa. Cimbebasia. 2, 179–186.")
                                
                                
                                
                                
                                
                                )#close second TabPanel
                       ) # close tabbox
                                )#close fluidrow
              
              
              
                       ) #end of backgroundd page
      
      
      
              )
    )
)



