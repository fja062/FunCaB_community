# data analysis plan
analysis_plan <- list(

    # calculate diversity metrics
    tar_target(
      name = diversity,
      command = calc_diversity(cover)
    )


  # make pca
  # tar_target(
  #   name = community_pca,
  #   command = make_sp_pca(cover)
  # )



#1. Effect of remaining/removed biomass on biomass of focal PFG

#Single FG presence:
#  g ~ b crb * f crb
#  b ~ g crb * f crb
#  f ~ b crb * g crb



#Multiple FG presence:
#  gb ~ b sb * f crb
#gb ~ g sb * f crb
#gf ~ f sb * b crb
#gf ~ g sb * b crb
#fb ~ b sb * g crb
#fb ~ f sb * g crb

#fgb(c) ~ b sb * f sb
#fgb(c) ~ g sb * f sb
#fgb(c) ~ b sb * g sb




#2. Effect of remaining/removed biomass on richness/evenness/diversity/traits of focal PFG
#Single FG presence (2019):
#  g ~ b crb * f crb
#f ~ b crb * g crb
#OR
#∂g ~ b crb * f crb
#∂f ~ b crb * g crb

#Multiple FG presence:
#  gb ~ b sb * f crb
#gf ~ f sb * b crb
#gf ~ g sb * b crb
#fb ~ b sb * g crb

#fgb(c) ~ b sb * f sb
#fgb(c) ~ b sb * g sb
)
