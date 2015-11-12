library(shiny)

shinyServer(function(input, output){
  library(dplyr)
  library(tidyr)
  
  input_model <- eventReactive(input$calc, {
    data.frame(Age = input$age,
               Race = input$race,
               VKORC1 = input$vkorc1,
               CYP2C9 = input$cyp2c9,
               On_Amiodarone = input$amiodarone,
               On_Enzyme_Inducers = input$enzyme_inducers)
  })
  
  output$selectedvalues <- renderTable({input_model() %>% 
      gather(key = Variable, value = Selection)})
  
  output$warfarindose <- renderText({
    warfarin_model <- structure(list(term = c("intercept", "age_decades", "vkorc1_1639_ag", "vkorc1_1639_aa", "vkorc1_1639_unknown", "cyp2c9_1_2", "cyp2c9_1_3", "cyp2c9_2_2", "cyp2c9_2_3", "cyp2c9_3_3", "cyp2c9_unknown", "asian", "african_american", "missing_or_mixed_race", "amiodarone", "enzyme_inducers"), 
                                     estimate = c(8.29529623468211, -0.284625991543081, -0.804050018868126, -1.58281937447931, -0.585810870878652, -0.473767057887023, -0.901866452197908, -1.09125866824621, -1.87135980567376, -2.50972717609401, -0.389492614878111, -0.670801757840009, -0.0675483297437883, -0.348827044586359, -0.672182905952411, 0.54552327241645), 
                                     std.error = c(0.072079981014331, 0.0102560287706372, 0.0449131398786503, 0.0545551821972256, 0.0443661947089462, 0.0465310858226987, 0.0534852345292653, 0.150214643924848, 0.137395891813837, 0.244898760990986, 0.102165232807463, 0.0447428773165641, 0.0579547790963726, 0.0545341398105267, 0.0810398751212533, 0.224099351950369), 
                                     statistic = c(115.084606265821, -27.7520664097549, -17.9023337277369, -29.0131809798961, -13.2039917942416, -10.1817322658718, -16.8619706005858, -7.26466235071039, -13.6202020378406, -10.2480190832259, -3.81237926224998, -14.9923696925874, -1.16553510852768, -6.39648935141037, -8.29447114703323, 2.43429205693226), 
                                     p.value = c(0, 1.03038776788502e-158, 1.064652478523e-69, 2.79523401421071e-172, 3.29186868870607e-39, 3.92172887720116e-24, 3.11993851496051e-62, 4.26388815028701e-13, 1.43668477234196e-41, 2.00539244089927e-24, 0.00013914798310481, 7.94475752194028e-50, 0.24385325010913, 1.72192992300359e-10, 1.35992655573032e-16, 0.0149527114251593)), 
                                .Names = c("term", "estimate", "std.error", "statistic", "p.value"), 
                                row.names = c(NA, -16L), 
                                class = "data.frame")
    
    predicted_dose <- input_model() %>% 
      mutate(intercept = 1,
             age_decades = as.numeric(substr(Age, 1, 1)),
             vkorc1_1639_ag = ifelse(VKORC1 == "G/A", 1, 0),
             vkorc1_1639_aa = ifelse(VKORC1 == "A/A", 1, 0),
             vkorc1_1639_unknown = ifelse(VKORC1 == "Unknown", 1, 0),
             cyp2c9_1_2 = ifelse(CYP2C9 == "*1/*2", 1, 0),
             cyp2c9_1_3 = ifelse(CYP2C9 == "*1/*3", 1, 0),
             cyp2c9_2_2 = ifelse(CYP2C9 == "*2/*2", 1, 0), 
             cyp2c9_2_3 = ifelse(CYP2C9 == "*2/*3", 1, 0), 
             cyp2c9_3_3 = ifelse(CYP2C9 == "*3/*3", 1, 0), 
             cyp2c9_unknown = ifelse(CYP2C9 == "Unknown", 1, 0), 
             asian = ifelse(Race == "Asian", 1, 0),
             african_american = ifelse(Race == "African American", 1, 0),
             missing_or_mixed_race = ifelse(Race == "Other or Unknown", 1, 0), 
             amiodarone = ifelse(On_Amiodarone, 1, 0),
             enzyme_inducers = ifelse(On_Enzyme_Inducers, 1, 0)) %>% 
      select(-c(Age:On_Enzyme_Inducers)) %>% 
      gather(key = term, value = value) %>% 
      mutate(term = as.character(term)) %>% 
      inner_join(warfarin_model) %>% 
      mutate(weighted = value * estimate) %>% 
      summarise(round(sum(weighted)^2))
    
    
    paste0("Based on the values entered, the predicted warfarin dose is: ", predicted_dose, "mg per week, or ~", round(predicted_dose/7),"mg per day.")
  })
}
)