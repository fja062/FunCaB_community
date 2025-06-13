species_dictionary <- read_delim(delim = " ", file =
"species_old species_new
Aco.sep Aco.sp
Arc.alp Arc.uva
Car.dio
Cer.sp
Coe.vir
Jun.fil
Luz.tri
Phy.cae
Pic.abi
Pil.sp
Pop.tre
Sax.opp
Ste.als
Ste.sp
Ver.sp
Vic.sep Vic.cra
Aco.sp
Mai.sp
Cre.tec Cre.sp
Myo.sp
Ran.sp
Ran.pyg
Fes.viv
Dia.lan
NID.gram
Cre.pal Cre.sp
Frag.vir Fra.ves
Sch.gig Sch.pra
"
             )

species_corrections <- read_delim(delim = " ", file =
"
plotID year species_old species_new
Alr1C 2016 Poa.ann Poa.alp
Alr1G 2015 Hie.pil Hie.sp
Alr1G 2017 Hie.vul Hie.sp
Alr2B 2019 Pla.maj Pla.med
Alr2C 2015 Phl.pra Phl.alp
Alr2G 2019 Tri.med Tri.pra
Alr2G 2018 Rum.acl Rum.ace
Alr2G 2019 Mel.syl Mel.pra
Alr3C 2016 Tri.rep Tri.med
Alr3G 2015 Tri.rep Tri.pra
Alr3G 2017 Tri.rep Tri.med
Alr3GB 2017 Tri.rep Tri.med
Alr4B 2019 Hie.sp Hie.pil
Alr4GB 2015 Vio.sp Vio.tri

Arh1C 2018 Tri.pra Tri.rep
Arh1C 2019 Ran.acr Ran.aur
Arh1F 2018 Luz.mul Luz.pil
Arh1F 2015 Ave.pub Ave.fle
Arh1G 2018 Tri.pra Tri.rep
Arh1GB 2019 Ste.med Ste.gra
Arh2B 2018 Luz.mul Luz.pil
Arh2FB 2018 Luz.mul Luz.pil





"
)
