################################################################################
# This file contains the code used for the paper                               #
#                                                                              #
#   binGroup2: Statistical Tools for Infection Identification via              #
#              Group Testing                                                   #
#                                                                              #
# by Christopher R. Bilder, Brianna D. Hitt, Brad J. Biggerstaff,              #
#    Joshua M. Tebbs, and Christopher S. McMahan                               #
################################################################################

################################################################################
# Section: Operating characteristics

  # Example #1
  group.member <- matrix(data = c(rep(1, times=30), rep(1:3, each = 10), 1:30),
    nrow = 3, ncol = 30, byrow = TRUE)
  group.member[,11]

  library(package = "binGroup2")
  save.Lohse <- opChar1(algorithm = "D3", p = 0.0193, Se = 1, Sp = 1,
    hier.config = group.member, print.time = FALSE)
  summary(save.Lohse)

  ExpTests(save.Lohse)


  # Example #2
  library(package = "rBeta2009")

  set.seed(3789)
  p.unordered <- t(rdirichlet(n = 5, shape = c(10.99, 0.18, 2.04, 0.31)))
  p.ordered <- p.unordered[, order(1 - p.unordered[1,])]
  round(p.ordered,4)

  group.member <- matrix(data = c(rep(1, times = 5), 1, 1, 1, 2, 3, 1, 2, 3,
    NA, NA), nrow = 3, ncol = 5, byrow = TRUE)
  group.member


################################################################################
# Section: Optimal testing configuration

  # Example #1
  Se <- matrix(data = rep(c(0.979, 0.985), times = 3), nrow = 2, ncol = 3,
    dimnames = list(Infection = 1:2, Stage = 1:3))
  Sp <- matrix(data = rep(c(0.985, 0.996), times = 3), nrow = 2, ncol = 3,
    dimnames = list(Infection = 1:2, Stage = 1:3))
  save.SHL <- opChar2(algorithm = "ID3", probabilities = p.ordered, Se = Se,
    Sp = Sp, hier.config = group.member, print.time=FALSE)
  summary(save.SHL)

  OTC.Lohse <- OTC1(algorithm = "D3", p = 0.0193, Se = 1, Sp = 1,
    group.sz = 3:20, obj.fn = "ET", print.time = FALSE)
  summary(OTC.Lohse)

  Config(OTC.Lohse)

  # Example #2
  save.LabCorp <- opChar1(algorithm = "A2", p = 0.05, rowcol.sz = 5,
    Se = c(1-0.023,1), Sp = 1, print.time = FALSE)
  ExpTests(save.LabCorp)

  OTC.LabCorp.Array <- OTC1(algorithm = "A2", p = 0.05, Se = c(0.977,1),
    Sp = 1, group.sz = 3:10, obj.fn = "ET", trace = FALSE, print.time = FALSE)
  summary(OTC.LabCorp.Array)
  ExpTests(OTC.LabCorp.Array)


################################################################################
# Appendix A

  # Use these prevalences when testing the code
  p.seq <- seq(from = 0.05, to = 0.06, by = 0.01)
  # Prevalences used in the paper: c(0.001, 0.005, seq(from = 0.01, to = 0.20, by = 0.01))

  # Group membership matrix for Lohse et al. (2020)
  group.member <- matrix(data = c(rep(1, times=30), rep(1:3, each = 10), 1:30),
    nrow = 3, ncol = 30, byrow = TRUE)

  # Save results from for loop here
  save.res <- matrix(data = NA, nrow = length(p.seq), ncol = 7)
  save.res.SecondOf3Stages <- matrix(data = NA, nrow = length(p.seq), ncol = 2)

  counter <- 1

  # Use this group size when testing the code
  max.gp.size <- 30
  # Group size used in the paper: 64

  for (p in p.seq) {
    # Lohse et al. (2020) 30-10-1 testing configuration
    res.Lohse <- opChar1(algorithm = "D3", p = p, Se = 1, Sp = 1,
      hier.config = group.member, print.time = FALSE)
    # Find OTCs for two- and three-stage hierarchical testing
    res.D2 <- OTC1(algorithm = "D2", p = p, Se = 1, Sp = 1,
      group.sz = 3:max.gp.size, obj.fn = "ET", trace = FALSE, print.time = FALSE)
    res.D3 <- OTC1(algorithm = "D3", p = p, Se = 1, Sp = 1,
      group.sz = 3:max.gp.size, obj.fn = "ET", trace = FALSE, print.time = FALSE)
    # Save results
    save.res[counter,] <- c(p, res.Lohse$Config$Stage1, res.Lohse$value,
      res.D2$opt.ET$OTC$Stage1, res.D2$opt.ET$value,
      res.D3$opt.ET$OTC$Stage1, res.D3$opt.ET$value)
    save.res.SecondOf3Stages[counter,] <- c(p,
      paste(res.D3$opt.ET$OTC$Stage2, collapse = " "))
    counter <- counter + 1
  }

  # Expected number of tests per individual and corresponding group sizes
  colnames(save.res) <- c("p", "LohseSize", "LohseEff", "TwoStage.Stage1",
    "TwoStageEff", "ThreeStage.Stage1", "ThreeStageEff")
  save.res2 <- as.data.frame(save.res)
  save.res2$ThreeStage.Stage2 <- save.res.SecondOf3Stages[,2]

  # Compare the expected number of tests per individual
  plot(x = save.res2$p, y = save.res2$LohseEff, type = "l", xlab = "Prevalence",
    ylab = "Expected number of tests per individual", col = "black",
    panel.first = grid(), ylim = c(0,1.1), lwd = 2, yaxs = "i", xaxs = "i",
    xlim = c(0,0.2))
  lines(x = save.res2$p, y = save.res2$TwoStageEff, lwd = 2, lty = "longdash",
    col = "red")

  # Check if two-stage is more efficient than three-stage. If so, don't plot because
  #   a laboratory would not want to implement three stages if this occurred.
  check <- save.res2$TwoStageEff > save.res2$ThreeStageEf
  lines(x = save.res2$p[check], y = save.res2$ThreeStageEff[check], lwd = 2,
    lty = "dotdash", col = "blue")

  axis(side = 1, at =seq(from = 0.00, to = 0.2, by = 0.01), tck = -0.01,
    labels = FALSE)
  legend(x = 0.05, y = 0.3, legend = c("Lohse et al. (2020) with 30-10-1",
    "Three-stage hierarchical testing with optimal group sizes",
    "Two-stage hierarchical testing with optimal group sizes"),
    lty = c("solid", "dotdash", "longdash"), bty = "n", bg = "white",
    col = c("black", "blue", "red"), lwd = 2, seg.len = 4.0, cex = 0.9)


################################################################################
# Appendix B

  # Prevalences used in the paper
  p.seq <- c(0.001, 0.005, seq(from = 0.01, to = 0.20, by = 0.01))

  # Save results from for loop here
  save.res <- matrix(data = NA, nrow = length(p.seq), ncol = 7)

  counter <- 1

  for (p in p.seq) {
    # LabCorp (2021) 5x5 array
    res.LabCorp <- opChar1(algorithm = "A2", p = p, rowcol.sz = 5,
      Se = c(0.977,1), Sp = 1, print.time = FALSE)
    # Find OTCs for array testing and two-stage hierarchical testing
    res.A2 <- OTC1(algorithm = "A2", p = p, Se = c(0.977,1), Sp = 1,
      group.sz = 3:10, obj.fn = "ET", trace = FALSE, print.time = FALSE)
    res.D2 <- OTC1(algorithm = "D2", p = p, Se = c(0.977,1), Sp = 1,
      group.sz = 3:10, obj.fn = "ET", trace = FALSE, print.time = FALSE)
    # Save results
    save.res[counter,] <- c(p, res.LabCorp$Config$Array.dim, res.LabCorp$value,
      res.A2$opt.ET$OTC$Array.dim, res.A2$opt.ET$value,
        res.D2$opt.ET$OTC$Stage1, res.D2$opt.ET$value)
    counter <- counter + 1
  }

  # Expected number of tests per individual and the corresponding group sizes
  colnames(save.res) <- c("p", "LabCorpSize", "LabCorpEff", "ArrayOTC",
    "ArrayEff", "DorfOTC", "DorfEff")
  save.res2 <- as.data.frame(save.res)

  # Compare the expected number of tests per individual
  plot(x = save.res2$p, y = save.res2$LabCorpEff, type = "l",
    xlab = "Prevalence", ylab = "Expected number of tests per individual",
    col = "black", panel.first = grid(), ylim = c(0,1.1), lwd = 2, yaxs = "i",
    xaxs = "i", xlim = c(0,0.2))
  lines(x = save.res2$p, y = save.res2$DorfEff, lwd = 2, lty = "longdash",
    col = "red")
  lines(x = save.res2$p, y = save.res2$ArrayEff, lwd = 2, lty = "121252",
    col = "darkgreen")
  axis(side = 1, at =seq(from = 0.00, to = 0.2, by = 0.01), tck = -0.01,
    labels = FALSE)
  legend(x = 0.07, y = 0.3, lty = c("solid", "longdash", "121252"),
    legend = c("LabCorp with 5x5 array", "Array testing with optimal group sizes", "Two-stage hierarchical testing with optimal group sizes"), bty = "n", bg = "white", col = c("black", "red", "darkgreen"), lwd = 2, seg.len = 4.0, cex = 0.9)



#