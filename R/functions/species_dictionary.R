# species_dictionary <- read_delim(delim = " ", file =
# "species_old species_new
# Aco.sep Aco.sp
# Arc.alp Arc.uva
# Car.dio
# Cer.sp
# Coe.vir
# Jun.fil
# Luz.tri
# Phy.cae
# Pic.abi
# Pil.sp
# Pop.tre
# Sax.opp
# Ste.als
# Ste.sp
# Ver.sp
# Vic.sep Vic.cra
# Aco.sp
# Mai.sp
# Cre.tec Cre.sp
# Myo.sp
# Ran.sp
# Ran.pyg
# Fes.viv
# Dia.lan
# NID.gram
# Cre.pal Cre.sp
# Frag.vir Fra.ves
# Sch.gig Sch.pra
# "
#              )

# species_corrections <- read_delim(delim = " ", file =
# "
# plotID year species_old species_new comment
# Alr1C 2016 Poa.ann Poa.alp
# Alr1G 2015 Hie.pil Hie.sp
# Alr1G 2017 Hie.vul Hie.sp
# Alr2B 2019 Pla.maj Pla.med
# Alr2C 2015 Phl.pra Phl.alp
# Alr2G 2019 Tri.med Tri.pra
# Alr2G 2018 Rum.acl Rum.ace
# Alr2G 2019 Mel.syl Mel.pra
# Alr3C 2016 Tri.rep Tri.med
# Alr3G 2015 Tri.rep Tri.pra
# Alr3G 2017 Tri.rep Tri.med
# Alr3GB 2017 Tri.rep Tri.med
# Alr4B 2019 Hie.sp Hie.pil
# Alr4GB 2015 Vio.sp Vio.tri

# Arh1C 2018 Tri.pra Tri.rep
# Arh1C 2019 Ran.acr Ran.aur
# Arh1F 2018 Luz.mul Luz.pil
# Arh1F 2015 Ave.pub Ave.fle
# Arh1G 2018 Tri.pra Tri.rep
# Arh1GB 2019 Ste.med Ste.gra
# Arh2B 2018 Luz.mul Luz.pil
# Arh2FB 2018 Luz.mul Luz.pil
# Arh3B 2019 Vio.bif Vio.pal ???
# Arh3C 2016 Hie.pil Hie.sp
# Arh3C 2019 Hie.vul Hie.sp
# Arh3G 2018 Tri.pra Tri.rep
# Arh3GB 2019 Vio.bif Vio.pal ???
# Arh3GB 2018 Tri.pra Tri.rep
# Arh4B 2018 Tri.pra Tri.rep
# Arh4G 2018 Tri.pra Tri.rep
# Fau1FB 2019 Ave.fle Ave.pub ?
# Fau1G 2018 Tri.rep Tri.pra
# Fau1G 2015 Pla.med Pla.lan
# Fau1G 2017 Hie.vul Hie.pil
# Fau2B 2018 Tri.pra Tri.rep
# Fau2C 2015 Vio.riv Vio.tri
# Fau2C 2017 Pla.lan Pla.med
# Fau3B 2017 Tri.pra Tri.rep
# Fau3B 2019 Hie.sp Hie.vul
# Fau4C 2018 Pla.lan Pla.med
# Fau4G 2019 Hie.sp Hie.vul
# Fau4GB 2019 Hie.sp Hie.vul
# Gud1B 2018 Fes.ovi Fes.rub ??
# Gud1C 2015 Ver.ser Ver.alp
# Gud1C 0000 festuca ovina and-festuca-rubra?-Carexes-a-mess
# Gud1FB 2017 Luz.spi Luz.mul
# Gud1G 2018 Pyr.rot Pyr.min
# Gud1GB 2019 Ver.ser Ver.alp
# Gud2B 2018 Vac.myr Vac.uli
# Gud2B 2017 Fes.ovi Fes.rub
# Gud2C 2018 Tof.cal Tof.pus
# Gud2C 2018 Car.pul Car.vag
# Gud2F 2018 Car.ech Car.vag
# Gud2FB 2018 Fes.ovi Fes.rub
# Gud2FB 2018 Car.ech Car.pul
# Gud2G 2018 Vac.myr Vac.uli
# Gud4B 2018 Pot.arg Pot.cra
# Gud4C 2015 Vio.pal Vio.bif
# Gud4C 2018 Pot.arg Pot.cra
# Gud4F 2019 Car.atro Car.atr
# Other Gudmedalen peculiarities to discuss
# Hog1B 2018 Tri.pra Tri.rep
# Hog2B 2018 Gal.uli Gal.sax
# Hog2B 2017 Car.pul Car.pil
# Hog2C 2016 Gal.sax Gal.uli
# Hog2FB 2017 Car.pul Car.pil
# Hog3C 2018 Car.pil Car.vag
# Hog4B 2019 Car.lep Car.vag ???
# Hog4C 2015 Gal.sax Gal.uli
# Hog4C 2019 Gal.sax Gal.uli
# Hog4G 2015 Gal.sax Gal.uli ???
# Hog4GB 2018 Gal.uli Gal.sax
# Lav1B CAREXES
# Lav1C 2018 Cer.cer Cer.alp
# Lav1C 2018 Car.nor Car.vag
# Lav1C 2015 Car.fla Car.big
# Lav1F 2015 Car.fla Car.big
# Lav1F 2018 Car.sp Car.big
# Lav1F 2018 Car.nor Car.vag
# Lav1FB 2017 Car.fla Car.big
# Lav1FB 2015 Car.fla Car.big
# Lav1FB 2017 Car.cap Car.nor
# Lav1G 2017 Rum.acl Rum.ace
# Lav2B CAREXES
# Lav2F CAREXES
# Lav2FB CAREXES
# Lav2FB 2017 Poa.pra Poa.alp
# Lav2FB 2018 Fes.rub Fes.ovi

# "
# )
