#' Fit the BTM Model to data from nmmMongo
#'
#' @param decisions Decisions from nmmMongo
#' @param anchors Named list of anchor names and values
#' @return Bradley Terry model
#' @examples
#' data("decisions")
#' mdl <- btmModel(decisions)
#' print(summary(mdl))
#' @export
#' @import sirt
btmModel <- function(decisions,anchors=NULL) {
  #Create a unique factor level for each candidate
  players <-
    unique(c(
      decisions$chosen,
      decisions$notChosen
    ))

  df <-
    data.frame(
      id1 = decisions$chosen,
      id2 = decisions$notChosen,
      result = 1
    )

  df$id1 <- factor(df$id1, levels = players)
  df$id2 <- factor(df$id2, levels = players)

  mod1 <-
    btm(
      df ,
      fix.eta = 0 ,
      ignore.ties = TRUE,
      eps = 0.3,
      fix.theta = anchors
    )

  return(mod1)
}

#' Simulate the reliability if judged to 100%
#'
#' @param decisions Decisions from nmmMongo
#' @return bootstrapped reliability when judged to 100%
#' @examples
#' @export
#' @import sirt
bootstrapReliability <- function(decisions){
  mdl <- btmModel(decisions)

  # bootstrap
  npersons <- nrow(mdl$effects)
  ndecisions <- npersons * 10

  rel <- NULL
  for(i in 1:100){
    sampleDecisions <- decisions %>% sample_n(size = ndecisions)
    sampleMdl <- btmModel(sampleDecisions)
    rel <- c(rel,sampleMdl$mle.rel)
  }

  ns <- seq(from = 5 * npersons, to = nrow(decisions))
  relG <- NULL
  for(n in ns){
    sampleDecisions <- decisions %>% sample_n(size = n)
    sampleMdl <- btmModel(sampleDecisions)
    relG <- c(relG,sampleMdl$mle.rel)
  }

  reliabilityG <- tibble(decisions = ns, reliability = relG)
  p <- ggplot(reliabilityG, aes(x = decisions, y=reliability))
  p <- p + geom_point()
  p <- p + geom_vline(xintercept = ndecisions, linetype = 'dotted')
  p <- p + theme_light()
  p

  return(rel)
}

#' Visualise the reliability
#'
#' @param decisions Decisions from nmmMongo
#' @return reliability plot
#' @examples
#' @export
#' @import sirt
visReliability <- function(decisions){
  mdl <- btmModel(decisions)

  # bootstrap
  npersons <- nrow(mdl$effects)
  ndecisions <- npersons * 10

  ns <- seq(from = 5 * npersons, to = nrow(decisions))
  relG <- NULL
  for(n in ns){
    sampleDecisions <- decisions %>% sample_n(size = n)
    sampleMdl <- btmModel(sampleDecisions)
    relG <- c(relG,sampleMdl$mle.rel)
  }

  reliabilityG <- tibble(decisions = ns, reliability = relG)
  p <- ggplot(reliabilityG, aes(x = decisions, y=reliability))
  p <- p + geom_point()
  p <- p + geom_vline(xintercept = ndecisions, linetype = 'dotted')
  p <- p + theme_light()
  p

  return(p)
}


#' Estimate judge infit
#'
#' @param probabilities Probabilities of decisions from btm model
#' @param decisions Decisions from nmmMongo
#' @return data frame of judges and infit values
#' @examples
#' data("decisions")
#' mdl <- btmModel(decisions)
#' probs <- mdl$probs
#' judge.infit <- btm_fit_2(probs,decisions)
#' @export
#' @import sirt
#' @import dplyr
#'
# judge infit statistic
btm_fit_2 <- function( probs , decisions){
  X_var1 <- probs[,1]
  X_exp1 <- probs[,1]
  X_var1 <- X_var1 - X_exp1^2
  Z_1 <- ( 1 - X_exp1 ) / sqrt( X_var1 )
  dat0 <- cbind(decisions,Z_1,X_var1)
  out <- dat0 %>% group_by(judgeName) %>%
    summarise(out1 = sum(Z_1^2),n1=n(),wvar1=sum(X_var1),win1=sum(X_var1*Z_1^2)) %>% data.frame
  out$infit <- out$win1 /out$wvar1
  return(out)
}

#' Scaled scores
#'
#' @param mdlEffects Effects from btm model
#' @param wRange Wanted range of scaled scores
#' @param wlow Wanted low of scaled scores
#' @return Data frame of scaled scores and standard errors
#' @examples
#' data("decisions")
#' mdl <- btmModel(decisions)
#' scaledScores <- scaleThetas(mdl$effects, 20, 0)
#' @export
#'
scaleThetas <- function(mdlEffects,wRange,wlow){
  mnTheta <- min(mdlEffects$theta)
  rnge <- max(mdlEffects$theta) - mnTheta
  uscale = wRange / rnge
  uimean = wlow - ( mnTheta * uscale )
  scaledScore = (mdlEffects$theta * uscale) + uimean
  scaledScoreSE = mdlEffects$se.theta * uscale
  scaledScores <- data.frame(scaledScore=scaledScore,scaledScoreSE=scaledScoreSE)
  return(scaledScores)
}

#' Bootstrap
#'
#' @param decisions decisions datafile
#' @param iterations number of boostrap iterations
#' @param size size of bootstrap sample as a fraction
#' @param replace boostrap with or without replacement
#' @return Data frame of thetas
#' @examples
#' data("decisions")
#  btmBootstrap(decisions, 10, size = 0.8, replace = FALSE)
#' @export
#' @import sirt
#' @import dplyr
#'
#'
btmBootstrap <- function(decisions,
         iterations = 10,
         size = 1,
         replace = TRUE) {
  mdl <- btmModel(decisions, anchors = NULL)
  bootstrapped <- NULL
  for (i in 1:iterations) {
    # bootstrap with replacement
    sampleDecisions <-
      decisions %>% sample_frac(size = size, replace = replace)
    smdl <- nmmBtm::btmModel(sampleDecisions, anchors = NULL)
    personsSample <- smdl$effects
    personsSample <-
      personsSample %>% mutate(iteration = i) %>% select(individual, theta, iteration)
    bootstrapped <- bind_rows(bootstrapped, personsSample)
  }
  return (bootstrapped)
}

#' Simulate
#'
#' @param decisions decisions from nmmMongo
#' @param anchor named list of anchor values
#' @param iterations number of simulations
#' @return Data frame of thetas
#' @examples
#' data("decisions")
#  btmSimulate(decisions, NULL, 10)
#' @export
#' @import sirt
#' @import dplyr
#'
#'
btmSimulate <-
  function(decisions,anchors,
           iterations = 10) {
    mdl <- btmModel(decisions, anchors)
    bootstrapped <- NULL
    probs <-
      tibble(
        leftScript = decisions$chosen,
        rightScript = decisions$notChosen,
        p = mdl$probs[, 1]
      )
    for (i in 1:iterations) {
      # sample probability distribution
      n <- nrow(decisions)
      sampleDecisions <- probs %>% mutate(runif  = runif(n))
      sampleDecisions <- sampleDecisions %>% mutate(
        chosen = case_when(p >= runif ~ leftScript,
                           TRUE ~ rightScript),
        notChosen = case_when(p < runif ~ leftScript,
                              TRUE ~ rightScript)
      ) %>% select(chosen, notChosen)

      smdl <- nmmBtm::btmModel(sampleDecisions, anchors)
      personsSample <- smdl$effects
      personsSample <-
        personsSample %>% mutate(iteration = i) %>% select(individual, theta, iteration)
      bootstrapped <- bind_rows(bootstrapped, personsSample)
    }
    return (bootstrapped)
  }

#' Estimate inter-rater reliability using split-halves
#'
#' @param iterations Number of iterations required (typically 100)
#' @param decisions Decisions from NMM
#' @return Median inter-rater reliability
#' @examples
#' r <- interrater(iterations,decisions)
#' @export
#' @import sirt
#'
# judge infit statistic
interrater <- function( iterations , decisions){
  #dataframe to store results from each iteration
  ir = data.frame(iteration = rep(0,iterations), pearson = rep(0,iterations), num_cand = rep(0,iterations))
  #get the scores for the whole data set
  mod1 <- btmModel(decisions)
  #now to split up the judges into two groups
  ip = as.data.frame(matrix(data=0,nrow=nrow(mod1$effects),ncol=2*iterations))
  colnames(ip) = as.character(c(1:(2*iterations))) #dataframe to store the new groups
  noj = length(unique(decisions$judgeName)) #number of judges
  judges = data.frame(row = 1:noj, judgeName = unique(decisions$judgeName), rand = rep(0,noj), group = rep(0,noj))
  #now to repeatedly calculate the correlation coefficents
  for(i in 1:iterations){
    ir[i,1] = i # fill iteration column
    judges$rand = runif(noj, -1, 1)  #randomly split judges
    judges <- judges[order(judges$rand),]
    judges$row = 1:noj
    judges$group = 2
    judges$group[1:(noj/2)] <-1
    #now split judgements
    groups = merge(decisions,judges) 
    g1 = groups[groups$group==1,]
    g2 = groups[groups$group==2,]
    df1 <- data.frame(chosen=g1$chosen, notChosen=g1$notChosen, result=1)
    modg1 <- btmModel(df1)
    #now to calculate the scores for group 2  
    df2 <- data.frame(chosen=g2$chosen, notChosen=g2$notChosen, result=1)
    modg2 <- btmModel(df2)
    #put the two sets of scores into one dataframe
    modg12 = merge(modg1$effects, modg2$effects, by="individual") #bodge in case different number of candidates in each rank
    ir[i,2] =  cor(modg12$theta.x,modg12$theta.y, method="pearson") # fill correlation column
    ir[i,4] = nrow(modg12)
    #now to store the correlations
    ip[,2*i-1] = c(modg12$theta.x, rep(0,nrow(mod1$effects)-nrow(modg12)))
    ip[,2*i] = c(modg12$theta.y, rep(0,nrow(mod1$effects)-nrow(modg12)))
  } # end of for i in 1:iterations loop
  return(median(ir$pearson))
}
