#' knowledge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_knowledge_ui <- function(id){
  ns <- NS(id)
  b64 <- base64enc::dataURI(file="data/covid.gif", mime="image/gif")
  tagList(
    h1("Covid-19",  class = "center"),
    br(),
    h2("Flattening the Curve"),
    "COVID-19 / Coronavirus has led to a pandemic that is threatening every country today. The promising strategy to limit the damage caused by this pandemic is to flatten the curve.",
    br(),
    div(class = "image_padding",
    imageOutput(ns("gif"))
    ),
    br(), br(),
    span(h1("Most Frequently Asked Questions"), class = "center"),
    h2("What is an epidemic curve?"),
    span("During any pandemic, healthcare resources like hospitals, ICU beds, and healthcare staff can be overwhelmed by the more number of patients, beyond the baseline number of patients who are already being cared for by the healthcare system. An epidemic curve is drawn to visualize the progress of a disease outbreak over time. This curve gives us a brief regarding the number of new outbreak cases by date of onset of the disease and hence the overall shape of the curve can reveal the type of outbreak we're dealing with"),
    br(),
    h2("Healthcare capacity line:"),
    span("The horizontal line represents the capacity of the country’s healthcare system. This capacity can be defined as the number of beds, staffing, and other measures available for patient care. Today, due to COVID-19 most of the countries are already operating close to the capacity line and the curve posses a threat of crossing this line as the virus spreads very rapidly."),
    br(),
    h2("What happens when the line is crossed?"),
    span("When the epidemic curve crosses the healthcare capacity line, the healthcare system can no longer meet the needs of COVID-19 patients and the other types of patients. At this point, people may not get the best care and the mortality rate starts to rise rapidly."),
    br(),
    h2("What is flattening the curve?"),
    span("A large number of lives can be saved by just ensuring that people get sick at a slower rate. This is termed as flattening the epidemic curve. A flattened curve indicates that the same number of people ultimately get infected with coronavirus but spread over a longer period which leads to a less stressed health care system."),
    br(),
    h2("When does flattening the COVID-19 epidemic curve take place?"),
    span("The most important key factor to flatten the curve is social distancing. The objective of social distancing or self-isolation is to reduce the probability of close contact between persons carrying an infection, and others who are not infected, thus minimizing the virus transmission. This is the underlying reason why governments are closing schools, non-critical businesses, social/sports events and other places where a ton people gather."),
    br(),
    h2("Can social distancing alone flatten the curve?"),
    span("By limiting interactions between individuals, we can limit the spread of the disease but that doesn’t mean it is the only effective way to flatten the curve. There are several other factors that you can do to aid flatten the curve such as practicing good hygiene by washing your hands, using sanitizers, cleaning frequently touched surfaces and self-isolating in confirmed and suspected cases."),
    br(),
    div(class = "image_padding",
    imageOutput(ns("handwash"))
    ),
    br(),
    br(),
    br(),
    "Source:", f7Link("WHO",  href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public"),     f7Link("Wikipedia",  href = "https://commons.wikimedia.org/wiki/File:Covid-19-curves-graphic-social-v3.gif"),
    span(h1("Some Myths Busters About Covid-19"), class = "center"),
    h2("COVID-19 virus can be transmitted in areas with hot and humid climates"),
    span("From the evidence so far, the COVID-19 virus can be transmitted in ALL AREAS, including areas with hot and humid weather. Regardless of climate, adopt protective measures if you live in, or travel to an area reporting COVID-19. The best way to protect yourself against COVID-19 is by frequently cleaning your hands. By doing this you eliminate viruses that may be on your hands and avoid infection that could occur by then touching your eyes, mouth, and nose."),
    h2("Cold weather and snow CANNOT kill the new coronavirus."),
    span("There is no reason to believe that cold weather can kill the new coronavirus or other diseases. The normal human body temperature remains around 36.5°C to 37°C, regardless of the external temperature or weather. The most effective way to protect yourself against the new coronavirus is by frequently cleaning your hands with alcohol-based hand rub or washing them with soap and water."),
    h2("Taking a hot bath does not prevent the new coronavirus disease"),
    span("Taking a hot bath will not prevent you from catching COVID-19. Your normal body temperature remains around 36.5°C to 37°C, regardless of the temperature of your bath or shower. Actually, taking a hot bath with extremely hot water can be harmful, as it can burn you. The best way to protect yourself against COVID-19 is by frequently cleaning your hands. By doing this you eliminate viruses that may be on your hands and avoid infection that could occur by then touching your eyes, mouth, and nose."),
    h2("The new coronavirus CANNOT be transmitted through mosquito bites."),
    span("To date there has been no information nor evidence to suggest that the new coronavirus could be transmitted by mosquitoes. The new coronavirus is a respiratory virus which spreads primarily through droplets generated when an infected person coughs or sneezes, or through droplets of saliva or discharge from the nose. To protect yourself, clean your hands frequently with an alcohol-based hand rub or wash them with soap and water. Also, avoid close contact with anyone who is coughing and sneezing."),
    h2("Are hand dryers effective in killing the new coronavirus?"),
    span("No. Hand dryers are not effective in killing the 2019-nCoV. To protect yourself against the new coronavirus, you should frequently clean your hands with an alcohol-based hand rub or wash them with soap and water. Once your hands are cleaned, you should dry them thoroughly by using paper towels or a warm air dryer."),
    h2("Can an ultraviolet disinfection lamp kill the new coronavirus?"),
    span("UV lamps should not be used to sterilize hands or other areas of skin as UV radiation can cause skin irritation."),
    h2("How effective are thermal scanners in detecting people infected with the new coronavirus?"),
    span("Thermal scanners are effective in detecting people who have developed a fever (i.e. have a higher than normal body temperature) because of infection with the new coronavirus.", br(), "However, they cannot detect people who are infected but are not yet sick with fever. This is because it takes between 2 and 10 days before people who are infected become sick and develop a fever"),
    h2("Can spraying alcohol or chlorine all over your body kill the new coronavirus?"),
    span("No. Spraying alcohol or chlorine all over your body will not kill viruses that have already entered your body. Spraying such substances can be harmful to clothes or mucous membranes (i.e. eyes, mouth). Be aware that both alcohol and chlorine can be useful to disinfect surfaces, but they need to be used under appropriate recommendations."),
    h2("Can eating garlic help prevent infection with the new coronavirus?"),
    span("Garlic is a healthy food that may have some antimicrobial properties. However, there is no evidence from the current outbreak that eating garlic has protected people from the new coronavirus."),
    "Source:", f7Link("WHO",  href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public/myth-busters"),     f7Link("Wikipedia",  href = "https://commons.wikimedia.org/wiki/File:Covid-19-curves-graphic-social-v3.gif"),
    
  )
}
    
#' knowledge Server Function
#'
#' @noRd 
mod_knowledge_server <- function(input, output, session){
  ns <- session$ns
  output$gif <- renderImage({
    list(src = "data/covid.gif",
         alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  output$handwash <- renderImage({
    list(src = "data/handwash.jpg",
         alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
}
    
## To be copied in the UI
# mod_knowledge_ui("knowledge_ui_1")
    
## To be copied in the server
# callModule(mod_knowledge_server, "knowledge_ui_1")