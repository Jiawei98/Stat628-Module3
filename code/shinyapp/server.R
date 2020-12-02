#Jiawei Huang: shinyapp developer
load("module3_app.rda")

server <- function(input, output) {
  
  name = eventReactive(input$go,input$name)
  address = eventReactive(input$go,input$address)
  state = eventReactive(input$go,input$state)
  
  exist = renderText({
    sum(burgers$name==name() & burgers$state==state() & burgers$address==address())
  })
  
  output$error = renderText({
    if (exist() == 0){
      if (name() %in% burgers$name[burgers$state==state()]){
        postal_refer = names(table(burgers$address[burgers$name==name() & burgers$state==state()]))
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Opps!</b></span></font><br>
                      <font size="3"><b>Seems you have inputed a wrong address for ',name(),' in ',state(),'. <br> You can try<br></b></font>',
                      '<font size="4"><b><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',paste(postal_refer,collapse="<br>"),'</span></b></font>'))
      }
      else {
        shop_refer = names(sort(table(burgers$name[burgers$state==state()]),decreasing =T))[1:20]
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Opps!</b></span></font><br>
                      <font size="3"><b>Seems the restaurant name you input is not in ',state(),'.<br>You can try<br></b></font>',
                      '<font size="4"><b><span style=\"color:',brewer.pal(8,"Spectral")[8],'\">',paste(shop_refer,collapse="<br>"),'</span></b></font>'))
      }
    }
  })
  
  output$ave_per = renderText({
    if (exist() > 0){
      ave = mean(shops$stars_y[shops$name==name() & shops$state==state() & shops$address==address()])
      col = blues9[floor(ave*2-0.0001)]
      per = round(mean(ave > unlist(shops$stars_y))*100,2)
      return(paste0('<font size="4"><b>The percentile of your rating in all burger restaurants is: <br></font>',
                    '<font size="10"><span style=\"color:',col,'\">',per,'%</span></font>'))
    }
  })
  
  output$ave_info = renderText({
    if (exist() > 0){
      ave = mean(shops$stars_y[shops$name==name() & shops$state==state() & shops$address==address()])
      col = blues9[floor(ave*2-0.0001)]
      return(paste0('<font size="4"><b>The average rating of your restaurant is: <br></font>',
                    '<font size="10"><span style=\"color:',col,'\">',round(ave,1),'</span></font>'))
    }
  })
  
  output$plot_text = eventReactive(input$go,
    if (exist() > 0){
      '<font size="4"><b>Rating distribution: </b></font>'
    }
  )
  
  output$SummaryPlot = renderPlot({
    if (exist() > 0){
    p1 = ggplot(burgers[burgers$name==name() & burgers$state==state() & burgers$address==address(),], aes(x=stars_x, fill=as.factor(stars_x))) +
      geom_bar(color="grey",size=1,alpha=0.8) +
      geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.1) +
      scale_fill_manual(values=blues9[c(1,3,5,7,9)]) +
      theme_minimal() + 
      labs(title="Barplot of rating",
           subtitle=paste("for restaurant",sep =" ",name(),"at",address(),state()),
           caption = "The data are from Yelp reviews.",
           x = "Rating",
           y = "count",
           fill = "Rating") +
      theme(
            plot.title = element_text(size = 20,hjust = 0.5),
            plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.8),
            plot.caption = element_text(size = 12, face = "italic"),
            axis.text = element_text(size=12),
            axis.title = element_text(size=14),
            legend.title = element_text(size=14,face="bold"))
    return(p1)} 
    else {return(NULL)}
    }, 
    height = 400, 
    width = 600
    )
  
  output$size = renderText({
    if (exist() > 0){
      review_count = nrow(X[burgers$name==name() & burgers$state==state() & burgers$address==address(),])
      if (review_count < 20){
        return(paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[2],'\"><i>This resturant only get less than 20 reviews,
               the recommendation system may not be able to provide valid recommendations due to the limited sample size, 
                      please consider add more comments.</i></span></font>'))
      }
      else {
        return(paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[2],'\"><i>This resturant get ',
               review_count,' reviews.</i></span></font>'))
      }
    }
  })
    
  output$service = renderText({
    if (exist() > 0){
      target_review = X[burgers$name==name() & burgers$state==state() & burgers$address==address(),]
      service = c("rude","unfriendly","unprofessional","wrong","waiting","slow","disrespectful")
      percent = mean(apply(target_review[,service],1,sum) > 0)*100
      words = names(which(apply(target_review[,service],2,mean) > 0.01))
      if (length(words)==0){return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>SERVICE</b></span></font>
                                   <font size="2"><br><b>You have done a good job in this part! No further advices.<br>
                                          ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))}
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>SERVICE</b></span></font>',
                    '<font size="5"><br><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Keywords: ',paste(words,collapse=", "),'<br></b></span></font>',
                    '<font size="2"><i>',round(percent,1),'% of comments described the service as above, please consider improve the service through </i><br>
                    <b>(1) Prepare a better service attitude for customers, show respect and caring for them.<br>
                    (2) Improve the professionalism of service, like to reduce the waiting time or mistakes made during serving.<br>   
                    ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$food = renderText({
    if (exist() > 0){
      target_review = X[burgers$name==name() & burgers$state==state() & burgers$address==address(),]
      food = c("cold","awful","mediocre","dry","disgusting","burnt","tasteless","bland","gross","hair","raw",
               "salty","frozen","smell","no_cheese","greasy","soggy","expensive")
      percent = mean(apply(target_review[,food],1,sum) > 0)*100
      words = names(which(apply(target_review[,food],2,mean) > 0.01))
      if (length(words)==0){return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>FOOD</b></span></font>
                                   <font size="2"><br><b>You have done a good job in this part! No further advices.<br>
                                          ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))}
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>FOOD</b></span></font>',
                      '<font size="5"><br><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Keywords: ',paste(words,collapse=", "),'<br></b></span></font>',
                      '<font size="2"><i>',round(percent,1),'% of comments described the food as above, please consider improve the food quality through </i><br>
                      <b>(1) Choosing better, fresher, and various kinds of materials for cooking. <br>
                      (2) Improve the professionalism of cooks, making less raw, bland, greasy or salty food.<br>
                      (3) Improve hygiene and speed of cooking, avoid serving cold food.<br>
                      (4) If the word "Expensive" appears, consider if you charges too much for the products.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$environment = renderText({
    if (exist() > 0){
      target_review = X[burgers$name==name() & burgers$state==state() & burgers$address==address(),]
      environment = c("dirty","stale","filthy","trash")
      percent = mean(apply(target_review[,environment],1,sum) > 0)*100
      words = names(which(apply(target_review[,environment],2,mean) > 0.01))
      if (length(words)==0){return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>ENVIRONMENT</b></span></font>
                                   <font size="2"><br><b>You have done a good job in this part! No further advices.<br>
                                          ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))}
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>ENVIRONMENT</b></span></font>',
                      '<font size="5"><br><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Keywords: ',paste(words,collapse=", "),'<br></b></span></font>',
                      '<font size="2"><i>',round(percent,1),'% of comments described the environment as above, please consider improve the food environment through </i><br>
                      <b>(1) Clean the resturant more frequently. <br>
                      (2) Renew the restaurant furniture or appliances. <br>                       
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$race = renderText({
    if (exist() > 0){
      target_review = X[burgers$name==name() & burgers$state==state() & burgers$address==address(),c("racist")]
      percent = mean(target_review > 0)*100
      words = ifelse(percent > 0,"racist",NA)
      if (is.na(words)){return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>ETHNICITY</b></span></font>
                                   <font size="2"><br><b>You have done a good job in this part! No racial discrimination occurs in your store.<br>
                                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))}
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>ETHNICITY</b></span></font>',
                      '<font size="5"><br><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Keywords: ',words,'<br></b></span></font>',
                      '<font size="2"><b>',round(percent,1),'% of comments proposed they faced racial discrimination in your restaurant, please treat serously of this problem!<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$cloudtext = renderText({
    if (exist() > 0){
      review_count = nrow(X[burgers$name==name() & burgers$state==state() & burgers$address==address(),])
      return(paste0('<font size="5"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>Here provides the cloudplot for the words in ',
                      review_count,' reviews.</b></span></font>'))
    }
  })
  
  output$cloud = renderWordcloud2({
    if (exist() > 0){
      target_review = X[burgers$name==name() & burgers$state==state() & burgers$address==address(),]
      freq = apply(target_review>0,2,sum)
      freq = data.frame(words = names(freq),frequency = freq)
      return(wordcloud2(freq,color='random-dark'))
    }
  })
  
  # analysis for attributes
  output$RestaurantsPriceRange2 = renderText({
    if (exist() > 0){
      target_att = att_mat$RestaurantsPriceRange2[att_mat$name==name() & att_mat$state==state() & att_mat$address==address()]
      if (target_att < 2){
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Price Range</b></span></font>',
                      '<font size="2"><b><br>Your restaurant offers cheap food, however, the cheap food is usually low in quality and may pull down the 
                        <br>overall rating, please consider serving more expensive luxury food.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>Price Range</b></span></font>',
                      '<font size="2"><b><br>You are selling food with relative high quality and high price range. That is good!<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$OutdoorSeating = renderText({
    if (exist() > 0){
      target_att = att_mat$OutdoorSeating[att_mat$name==name() & att_mat$state==state() & att_mat$address==address()]
      if (target_att %in% c("None","False")){
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Outdoor Seating</b></span></font>',
                      '<font size="2"><b><br>Your restaurant doesn\'t provide outdoor seating, please consider providing that to improve the overall<br> rating.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>Outdoor Seating</b></span></font>',
                      '<font size="2"><b><br>Your restaurant provides outdoor seating. That is good!<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$HasTV = renderText({
    if (exist() > 0){
      target_att = att_mat$HasTV[att_mat$name==name() & att_mat$state==state() & att_mat$address==address()]
      if (target_att == "False"){
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>TV</b></span></font>',
                      '<font size="2"><b><br>Your restaurant doesn\'t have TV, please consider install several TVs inside to provide <br>entertainment for customers.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>TV</b></span></font>',
                      '<font size="2"><b><br>Your restaurant has TV inside, people likes it!<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$Alcohol = renderText({
    if (exist() > 0){
      target_att = att_mat$Alcohol[att_mat$name==name() & att_mat$state==state() & att_mat$address==address()]
      if (target_att == "none"){
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Alcohol</b></span></font>',
                      '<font size="2"><b><br>Your restaurant doesn\'t provide alcohol, please consider providing that if possible, <br> since restaurants providing alcohol will usually get higher rating.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>Alcohol</b></span></font>',
                      '<font size="2"><b><br>Your restaurant provides alcohol, please keep on that since people likes alcohol in burger shop.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      }
    })
  
  output$BikeParking = renderText({
    if (exist() > 0){
      target_att = att_mat$BikeParking[att_mat$name==name() & att_mat$state==state() & att_mat$address==address()]
      if (target_att %in% c("None","False")){
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[1],'\"><b>Bike Parking</b></span></font>',
                      '<font size="2"><b><br>Your restaurant doesn\'t provide place for bike parking, you can set sepcific parking zone <br>to arrtact more bike users.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
      else {
        return(paste0('<font size="6"><span style=\"color:',brewer.pal(8,"Spectral")[8],'\"><b>Bike Parking</b></span></font>',
                      '<font size="2"><b><br>Your restaurant provides bike parking, which helps you attract customers with bike.<br>
                      ---------------------------------------------------------------------------------------------------------------------------------------------------------<br></b></font>'))
      }
    }
  })
  
  output$att_text = renderText({
    if (exist() > 0){
    return(paste0('<font size="3"><span style=\"color:',brewer.pal(8,"Spectral")[2],'\"><i>
                  Recommendations here are from the anova and regression tree analysis of your restaurant attritubes.</i></span></font>'))
    }
  })
}