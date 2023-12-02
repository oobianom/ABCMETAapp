require(shiny)
require(shinyjs)


# Define the UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("ABCMETAapp: Estimating Mean and Standard Deviation via ABC (Approximate Bayesian Computation)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel( style = "overflow-y:scroll; max-width: 400px;max-height: 800px",width = 6, 
                      tags$head(tags$script('
                                      var dimension = [0, 0];
                                            $(document).on("shiny:connected", function(e) {
                                            dimension[0] = window.innerWidth;
                                            dimension[1] = window.innerHeight;
                                            Shiny.onInputChange("dimension", dimension);
                                            });
                                            $(window).resize(function(e) {
                                            dimension[0] = window.innerWidth;
                                            dimension[1] = window.innerHeight;
                                            Shiny.onInputChange("dimension", dimension);
                                            });
                                            ')),
                      
          textInput("integer", label = "Sample Size(n):", value = "500" , width = '200px'),
          radioButtons("Scenarios", "Available summary statistics:",
                       c("Min, Med, Max" = "sc1",
                         "Q1, Med, Q3" = "sc2",
                         "Min, Q1, Med, Q3, Max" = "sc3")),
          #condition for radiobutton sc1
          conditionalPanel(
            condition = "input.Scenarios == 'sc1'",
            textInput("ss1min", label = "Minimum:", value = "" , width = '200px'),
            textInput("ss1med", label = "Median:", value = "" , width = '200px'),
            textInput("ss1max", label = "Maximum:", value = "" , width = '200px')),
          #condition for radiobutton sc2
          conditionalPanel(
            condition = "input.Scenarios == 'sc2'",
            textInput("ss2q1", label = "1st Quartile:", value = "" , width = '200px'),
            textInput("ss2med", label = "Median:", value = "" , width = '200px'),
            textInput("ss2q3", label = "3rd Quartile:", value = "" , width = '200px')), 
          #condition for radiobutton ss3
          conditionalPanel(
            condition = "input.Scenarios == 'sc3'",
            textInput("ss3min", label = "Minimum:", value = "" , width = '200px'),
            textInput("ss3q1", label = "1st Quartile:", value = "" , width = '200px'),
            textInput("ss3med", label = "Median:", value = "" , width = '200px'),
            textInput("ss3q3", label = "3rd Quartile:", value = "" , width = '200px'),
            textInput("ss3max", label = "Maximum:", value = "" , width = '200px')),
          selectInput("Typeofdistribution", label = "Underlying distribution:", c("Normal" = "normal","LogNormal" = "lognormal","Exponential" = "exponential","Beta" = "beta","Weibull" = "weibull", "Distribution Selection" = "DistributionSelection" ), width = "300px"),
          
          conditionalPanel(
            condition = "input.Typeofdistribution == 'normal'",
             textInput("nsigma", label = "Upper limit value for Sigma:", value = "50" , width = '200px')),
          conditionalPanel(
            condition = "input.Typeofdistribution == 'lognormal'",
            textInput("lsigma", label = "Upper limit value for Sigma:", value = "10" , width = '200px')),
          conditionalPanel(
            condition = "input.Typeofdistribution == 'exponential'",
            textInput("elambda", label = "Upper limit value for Lambda:", value = "40" , width = '200px')),
          conditionalPanel(
            condition = "input.Typeofdistribution == 'beta'",
            textInput("lbbeta", label = "Lower bound value:", value = "0" , width = '200px'),
            textInput("ubbeta", label = "Upper bound value:", value = "100" , width = '200px'),
            textInput("balpha", label = "Upper limit value for Alpha:", value = "40" , width = '200px'),
            textInput("bbeta", label = "Upper limit value for Beta:", value = "40" , width = '200px')),
          
          conditionalPanel(
            condition = "input.Typeofdistribution == 'weibull'",
            textInput("wlambda", label = "Upper limit value for Lambda:", value = "50" , width = '200px'),
            textInput("wkappa", label = "Upper limit value for Kappa:", value = "50" , width = '200px')),
           

          
          ## setup for ABC
          textInput("nb_simul", label = "Total No. of simulations:", value = "50000" , width = '200px'),
          textInput("accperc", label = "Acceptance percentage (%):", value = "0.1" , width = '200px'),

          
          actionButton("goButton", "Run ABCMETA!")),

          
        #mainPanel(verbatimTextOutput("nText", placeholder = TRUE))
      
       mainPanel(column(9, offset = 8,verbatimTextOutput("nText")))
        
        ,position = c("left", "right")
  )
  
)


# Define the server code
server <- function(input, output, session) {

  
  observeEvent(input$Senarios, {
    updateTextInput(session, "ss1min",value = "");
    updateTextInput(session, "ss1med",value = "");
    updateTextInput(session, "ss1max",value = "");
    updateTextInput(session, "ss1q1",value = "");
    updateTextInput(session, "ss1q3",value = "");
    updateTextInput(session, "ss2min",value = "");
    updateTextInput(session, "ss2med",value = "");
    updateTextInput(session, "ss2max",value = "");
    updateTextInput(session, "ss2q1",value = "");
    updateTextInput(session, "ss2q3",value = "");
    updateTextInput(session, "ss3min",value = "");
    updateTextInput(session, "ss3med",value = "");
    updateTextInput(session, "ss3max",value = "");
    updateTextInput(session, "ss3q1",value = "");
    updateTextInput(session, "ss3q3",value = "");

  })
  
  ntext <- eventReactive(input$goButton, 
                          { 
                            
                            nb_simul.val= as.numeric(input$nb_simul);
                            acc.perc= as.numeric(input$accperc)/100;
                            random.seed=1234;
                            distrib= input$Typeofdistribution
                            set.seed(random.seed);
                            n=input$integer;
                            
                            progress <- shiny::Progress$new()
                            on.exit(progress$close())
                            progress$set(message = "Simulate", value = 0)

                            
                            if(input$Typeofdistribution == 'normal')
                            {sigma = as.numeric(input$nsigma);}
                            
                            if(input$Typeofdistribution == 'lognormal')
                            {sigma = as.numeric(input$lsigma);}
                            
                            if(input$Typeofdistribution == 'exponential')
                            {lambda = as.numeric(input$elambda);}
                            
                            if(input$Typeofdistribution == 'beta')
                            {alpha = as.numeric(input$balpha);
                             beta = as.numeric(input$bbeta);
                             limit1 = as.numeric(input$lbbeta);
                             limit2 = as.numeric(input$ubbeta);
                             }
                            
                            if(input$Typeofdistribution == 'weibull')
                            {lambda = as.numeric(input$wlambda);
                              kappa = as.numeric(input$wkappa);
                              }
                            
                            if(input$Scenarios == 'sc1'){summ.val<-c(as.numeric(input$ss1min),NA,as.numeric(input$ss1med),NA,as.numeric(input$ss1max));}

                            if(input$Scenarios == 'sc2'){summ.val<-c(NA,as.numeric(input$ss2q1),as.numeric(input$ss2med),as.numeric(input$ss2q3),NA);}

                            if(input$Scenarios == 'sc3'){summ.val<-c(as.numeric(input$ss1min),as.numeric(input$ss2q1),as.numeric(input$ss2med),as.numeric(input$ss2q3),as.numeric(input$ss1max));}

                            
                            
                            up.ind= as.numeric(nb_simul.val)*as.numeric(acc.perc); ## 50 (top 0.1% among 50000);
                            
                            # Matrix of simulated mean and sd values 
                            est.mat=matrix(NA,ncol=10,nrow=nb_simul.val)
                            # distance matrix (1:normal, 2:lognormal, 3:exponential, 4:weibull, 5:beta)
                            dist.mat= matrix(NA,ncol=5,nrow=nb_simul.val)
                            
############################################################################
                            
                              
                              if (distrib=='normal'){
                                
                                for (i in 1:nb_simul.val){
                                
                                if (is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE){mustar1=runif(1,summ.val[2],summ.val[4]); }
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){mustar1=runif(1,summ.val[1],summ.val[5]); }      
                               
                                sigstar1=runif(1,0,sigma);
                                temp.sam=rnorm(n,mustar1,sigstar1);
                                est.mat[i,1]=mean(temp.sam);
                                est.mat[i,2]=sd(temp.sam);
                                
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat [i,1]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat [i,1]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat [i,1]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                
                                
                                
                                if(i%%500==0){print(paste('iter=======',i));
                                  progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val));
                                  }
                                  
                                  
                                
                              } # end of sim
                              
                                ind=sort(dist.mat[,1],index.return=T)$ix;mycol=1:2;
                                output=est.mat[ind[1:up.ind],mycol]
                                est= apply(output,2,'mean')
   
                                
                               
                              } # end of normal
                            
                            
##########################################################################                              
                              
                              
                              
                              
                              if (distrib=='lognormal'){
                                
                                for (i in 1:nb_simul.val){
                                
                                
                                if (is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE){mustar2=runif(1,log(summ.val[2]),log(summ.val[4])); }
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){mustar2=runif(1,log(summ.val[1]),log(summ.val[5])); }      
                                
                                sigstar2=runif(1,0,sigma);
                                temp.sam=rlnorm(n,mustar2,sigstar2);
                                est.mat[i,3]=mean(temp.sam);
                                est.mat[i,4]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat [i,2]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat [i,2]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat [i,2]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                              
                                
                                
                                if(i%%500==0){print(paste('iter=======',i));
                                  progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val));}
                                
                                
                                } # end of sim  
                                
                                ind=sort(dist.mat[,2],index.return=T)$ix;mycol=3:4;
                                
                                output=est.mat[ind[1:up.ind],mycol]
                                est= apply(output,2,'mean')
                                
                                
                                
                              } # end of lognormal
                            
                            
                            
                            
                              
                              if (distrib=='exponential'){
                                
                                for (i in 1:nb_simul.val){
                                
                                lamstar=runif(1,0,lambda);
                                temp.sam=rexp(n,1/lamstar);
                                est.mat[i,5]=mean(temp.sam);
                                est.mat[i,6]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat [i,3]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat [i,3]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat [i,3]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                
                                
                                
                                if(i%%500==0){print(paste('iter=======',i));
                                progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val))
                                }
                                
                                
                              } # end of sim
                                ind=sort(dist.mat[,3],index.return=T)$ix;mycol=5:6;
                                output=est.mat[ind[1:up.ind],mycol]
                                est= apply(output,2,'mean')
                                
                                
                                
                              } # end of expo
                            
                            
###########################################################################                            
                              
                              if (distrib=='weibull'){
                                
                                for (i in 1:nb_simul.val){
                                
                                lamstar=runif(1,0,lambda);
                                kappastar=runif(1,0,kappa);
                                temp.sam=rweibull(n,lamstar,kappastar);
                                est.mat[i,7]=mean(temp.sam);
                                est.mat[i,8]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat [i,4]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat [i,4]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat [i,4]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                
                                
                                if(i%%1000==0){print(paste('iter=======',i));
                                  progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val))
                                  }
                               
                                  
                                
                                
                                } # end of sim
                                ind=sort(dist.mat[,4],index.return=T)$ix;mycol=7:8;
                                output=est.mat[ind[1:up.ind],mycol]
                                est= apply(output,2,'mean')
                                
                                
                                
                              } # end of weibull

                            
##############################################################
                            
                              if (distrib=='beta'){
                                
                                
                                summ.val=(summ.val-limit1)/(limit2-limit1);
                                difflim=(limit2-limit1);
                                
                                for (i in 1:nb_simul.val){
                                  
                                  alphastar=runif(1,0,alpha);
                                  betastar=runif(1,0,beta);
                                  temp.sam=rbeta(n,alphastar,betastar);
                                  est.mat[i,9]=difflim*mean(temp.sam)+limit1;
                                  est.mat[i,10]=difflim*sd(temp.sam);
                                
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat [i,5]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat [i,5]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat [i,5]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                
                                
                                if(i%%500==0){print(paste('iter=======',i));
                                  progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val))
                                  }
                                
                                
                              } # end of sim
                                
                                ind=sort(dist.mat[,5],index.return=T)$ix;mycol=9:10;
                                
                                output=est.mat[ind[1:up.ind],mycol]
                                est= apply(output,2,'mean')
                                
                                  
                               } # end of beta
                            
    
                            if (distrib=='DistributionSelection'){
                            
                              set.seed(random.seed);
                              up.ind= nb_simul.val*acc.perc; ## 50 (top 0.1% among 50000);
                              
                              # Matrix of simulated mean and sd values 
                              est.mat=matrix(NA,ncol=10,nrow=nb_simul.val)
                              
                              # distance matrix (1:normal, 2:lognormal, 3:exponential, 4:weibull, 5:beta)
                              dist.mat= matrix(NA,ncol=5,nrow=nb_simul.val)
                              
                              for (i in 1:nb_simul.val){
                                
                                # normal distrib part
                                
                                if (is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE){mustar1=runif(1,summ.val[2],summ.val[4]); }
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){mustar1=runif(1,summ.val[1],summ.val[5]); }      
                                
                                sigstar1=runif(1,0,50);
                                temp.sam=rnorm(n,mustar1,sigstar1);
                                est.mat[i,1]=mean(temp.sam);
                                est.mat[i,2]=sd(temp.sam);
                                
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat[i,1]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat[i,1]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat[i,1]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                # lognormal distrib part
                                
                                if (is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE){mustar2=runif(1,log(summ.val[2]),log(summ.val[4])); }
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){mustar2=runif(1,log(summ.val[1]),log(summ.val[5])); }      
                                
                                sigstar2=runif(1,0,10);
                                temp.sam=rlnorm(n,mustar2,sigstar2);
                                est.mat[i,3]=mean(temp.sam);
                                est.mat[i,4]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat[i,2]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat[i,2]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat[i,2]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                # exponential 
                                
                                lamstar=runif(1,0,40);
                                temp.sam=rexp(n,1/lamstar);
                                est.mat[i,5]=mean(temp.sam);
                                est.mat[i,6]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat[i,3]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat[i,3]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat[i,3]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                #    weibull'
                                
                                lamstar=runif(1,0,50);
                                kappastar=runif(1,0,50);
                                temp.sam=rweibull(n,lamstar,kappastar);
                                est.mat[i,7]=mean(temp.sam);
                                est.mat[i,8]=sd(temp.sam);
                                ss1=min(temp.sam);ss2=quantile(temp.sam,.25)[[1]];
                                ss3=median(temp.sam);ss4= quantile(temp.sam,.75)[[1]];
                                ss5=max(temp.sam);
                                
                                if (is.na(summ.val[2])==TRUE & is.na(summ.val[4])==TRUE){dist.mat[i,4]=sqrt(sum((c(summ.val[1],summ.val[3],summ.val[5])-c(ss1,ss3,ss5))^2));}
                                if (is.na(summ.val[1])==TRUE & is.na(summ.val[5])==TRUE){dist.mat[i,4]=sqrt(sum((c(summ.val[2],summ.val[3],summ.val[4])-c(ss2,ss3,ss4))^2));}
                                if (is.na(summ.val[1])==FALSE & is.na(summ.val[2])==FALSE & is.na(summ.val[4])==FALSE & is.na(summ.val[5])==FALSE){dist.mat[i,4]=sqrt(sum((c(summ.val[1],summ.val[2],summ.val[3],summ.val[4],summ.val[5])-c(ss1,ss2,ss3,ss4,ss5))^2));}
                                
                                
                                if(i%%500==0){print(paste('iter=======',i));
                                  progress$inc(1/(nb_simul.val/500), detail = paste("Doing part", i, " of ",nb_simul.val))
                                  }
                                
                                
                              }
                              
                            
                              dist=c(dist.mat[,1], dist.mat[,2], dist.mat[,3], dist.mat[,4]);
                              model.ind=c(rep(1, nb_simul.val), rep(2, nb_simul.val), rep(3, nb_simul.val), rep(4, nb_simul.val));

                              ind.all=sort(dist,index.return=T)$ix
                              tb=rep(0,4);
                              for (k in 1:4){tb[k]=length(which(model.ind[ind.all[1:up.ind]]==k));}
                              
                              sel.model= which(tb==max(tb))[1]
                              if (sel.model==1){ind=sort(dist.mat[,1],index.return=T)$ix;mycol=1:2;sel.model.label='Normal';}
                              if (sel.model==2){ind=sort(dist.mat[,2],index.return=T)$ix;mycol=3:4;sel.model.label='Log-Normal';}
                              if (sel.model==3){ind=sort(dist.mat[,3],index.return=T)$ix;mycol=5:6;sel.model.label='Exponential';}
                              if (sel.model==4){ind=sort(dist.mat[,4],index.return=T)$ix;mycol=7:8;sel.model.label='Weibull';}
                              
                              output=est.mat[ind[1:up.ind],mycol];
                              est= apply(output,2,'mean');
         
                            }
                            
                            if (distrib=='DistributionSelection'){
                              paste("[","ABC Mean=",round(est[1],3),"]","[","ABC SD=",round(est[2],3),"]","[","Distribution=",sel.model.label,"]","[","model prob=",tb[sel.model]/up.ind,"]");   
                            }
                            else {paste("[","ABC Mean=",round(est[1],3),"]","[","ABC SD=",round(est[2],3),"]");  }
                            
                            })
  
  output$nText <- renderText({ntext()});

  
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server, options = list(launch.browser = T ))
