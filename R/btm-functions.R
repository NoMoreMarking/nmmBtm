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
#' @param judges Judges from nmmMongo
#' @return bootstrapped reliability when judged to 100%
#' @examples
#' @export
#' @import sirt
bootstrapReliability <- function(decisions, judges){
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
