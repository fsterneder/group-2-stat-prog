#####################################3
# Simplified code for project in Statistical Programming
# Task: explain *how* the code operates (I have described the statistics).
# 
# The algorithm cycles through a vector of p-values and tries to reject as
# many of the corresponding hypothesis tests as possible. The algorithm
# can work through the vector multiple times. This is referred to as a "pass"
# through the data and the number of times this happens is counted in the code.
# 
# Admittedly the algorithm may seem rather dumb. In order to simplify the 
# code enough for it to be a reasonable assignment, I had to simplify the algorithm
# too much. The real application performs autimated polynomial regression.
#####################################3

# Bidders -----------------------------------------------------------------

# The object "wealth" is "alpha-wealth", the error level or probability for the 
# entire procedure. This error probability is "spent" on various tests. 
# There is an initial amount of wealth, and it is modified during the procedure.
# Without worrying about the details, we have an algorithm which makes a "bid"
# for how much it would like to spend on an individual test. The amount spent on
# a test is the alpha-level for that test, ie, reject if pvalue < bid. When testing 
# a hypothesis, I *lose* wealth equal to the bid amount and if I reject the test I 
# *gain* wealth equal to my initial allocation (omega). The algorithm terminates 
# when the wealth is depleted.

# I included a couple of simple bidders in case you are curious about trying out
# other bidding strategies. For the project, you are only required to understand
# makeBidderGeom.

# this bidder bids some fraction of his remaining wealth on the next test
makeBidderGeom = function(wealth=.05, frac = .5) {
  list( 
    state = function() { 
      list(
        wealth = wealth,
        frac   = frac
      ) 
    },
    bidAccepted = function(a) { wealth <<- wealth + a },
    bidRejected = function(d) { wealth <<- wealth - d },
    bid = function()  { ifelse(wealth > .0001, wealth*frac, wealth) }
    # ifelse statement ensures termination by eventually bidding remaining wealth
  )
}

# this bidder always bids a constant amount given by const
makeBidderConst = function(wealth=.05, const=.01) {
  list( 
    state = function() { 
      list(
        wealth  = wealth,
        const   = const
      ) 
    },
    bidAccepted = function(a) { wealth <<- wealth + a },
    bidRejected = function(d) { wealth <<- wealth - d },
    bid = function()  { ifelse(wealth >= const, const, wealth) }  
    # can't bid more than current wealth
  )
}


# Constructors ------------------------------------------------------------

# The resulting object manages which hypothesis test should be tested next.
# As this algorithm is used to construct linear models, the vector of hypotheses
# is determined by the number of columns of the covariate matrix. That being said,
# we only care about an index, and ncolumns can just be the length of our pvalue
# vector. The constructor tests hypotheses in the vector back to front, skipping
# hypotheses that have been rejected in previous passes.

makeRawSource <- function(ncolumns) {
  activeColumns  = 0:ncolumns
  position       = ncolumns
  prevPosition   = NA
  nactive        = ncolumns
  
  list(	
    name = "Marginal",
    state = function() { 
      list(
        position     = position, 
        prevPosition = prevPosition,
        active       = activeColumns,
        nactive      = sum(!is.na(activeColumns[-1]))
      )
    },
    generateFeature = function() { 
      prevPosition  <<- position
      position      <<- max(activeColumns[activeColumns < position], na.rm=T)
      return(prevPosition) 
    },
    dropLastFeature = function() { activeColumns[prevPosition+1] <<- NA },
    # +1 accounts for the zero at the beginning of the activeColumns vector
    udPass = function() {
      position <<- max(activeColumns, na.rm=T)
      prevPosition <<- NA
    }
  )
}

# Experts -----------------------------------------------------------------

# Think of the expert as an individual that proposes which hypothesis to test
# and makes a bid at which to test the hypothesis. In reality, the full code 
# creates many experts which test hypotheses in various ways. All experts make 
# bids and the maximum bid is used. The resulting "auction" only calls expert 
# functions, so need to access other objects through the expert.

makeExpert <- function (bidder, constructor) {
  list(	
    name         = "Geom",
    bidder       = bidder,
    constructor  = constructor,
    state        = function() { 
      list(
        wealth   = bidder$state()$wealth,
        position = constructor$state()$position
      )
    },
    bid          = function() { bidder$bid() },
    feature      = function() { constructor$generateFeature() },
    finishedPass = function() { constructor$state()$position == 0 },
    finished     = function() { constructor$state()$nactive == 0 },
    passTest     = function(a) {
      bidder$bidAccepted(a)
      constructor$dropLastFeature() 
    },
    pay          = function(payment) {
      bidder$bidRejected(payment)
    },
    udPass       = function() { constructor$udPass() }
  )
}

makeExpertGeom <- function(ncolumns, wealth=.05, frac=.5) {
  makeExpert(
    makeBidderGeom(wealth, frac),
    makeRawSource(ncolumns)
  )
}

# Example of Hypothesis testing --------------------------------------------------

seed = 421232
set.seed(seed)  # ensures everyone gets the same "random" numbers
nNull = 20; nNonNull = 20
nulls = runif(nNull)
nonNulls = runif(nNonNull, 0, .05)
pvals = sample(c(nulls, nonNulls))  # vector of pvalues/hypotheses to test

omega = .1  # initial alpha-wealth and reward for rejection
expert = makeExpertGeom(length(pvals), wealth=omega, frac=.1)
rejectedHyp = list()
pass = 1

while (!expert$finished() && expert$state()$wealth > 0) {
  index = expert$feature()
  bid = expert$bidder$bid()
  expert$pay(bid)
  if (pvals[index] < bid) {
    expert$passTest(omega)
    rejectedHyp[[1+length(rejectedHyp)]] = index
  }
  if(expert$finishedPass()) {
    expert$udPass()
    pass = pass+1
  }
}
rejectedHyp; pass
pvals[unlist(rejectedHyp)]

# moral of this example: we have 20 hypotheses that are traditionally considered to
# be significant (pvalues < .05), but they are "hidden" in a set of 40 hypotheses.
# This is science! We don't know what is true/false a-priori! Observe the result:
# when we try to identify which hypotheses are true and which are false, we are
# only able to identify a small subset, and these are the hypotheses that are 
# *highly* significant (pvalues << .05). For more details, I teach a course on 
# large-scale inference (masters class).

