##### -------------------------------------------------- #####
##### ----- Manual Fixes for Diatom Harmonization ----- #####
##### -------------------------------------------------- #####

# date created: 14.08.20
# date used   : 14.08.20 + 21.09
# Jonathan Jupke 
# Get Real Working Package 2 - Diatoms 
# This used to be part of 005_harmonize_diatoms but since this is quite a long
# and homogeneous script I saved it in a separate one to declutter the former.

# Manual fixes  ----------------------------------------------------------- 

# A -----------------------------------------------------------------------
fixer(
        from = c(
                "Achnanthes lanceolata rostrata",
                "Achnanthes lanceolata ssp. frequentissima var. rostrata",
               " Achnanthes lanceolata ssp. frequentissima var. rostratiformis",
                "Achnanthes lanceolata ssp.rostrata",
                "Achnanthes lanceolata ssp. rostrata",
                "Achnanthes lanceolata var. rostrata"
        )
        ,
        to = "Achnanthes lanceolata ssp. rostrata (Oestrup) Lange-Bertalot"
)

fixer(
        from = c(
                "Achnanthes minutissima v.jackii",
                "Achnanthes minutissima var. jackii"
        ),
        to =   "Achnanthidium jackii"
)      

fixer(
        from = c(
                "Achnanthes minutissima",
                "Achnanthes minutissima var. affinis",
                "Achnanthes minutissima var. gracillima",
                "Achnanthes minutissima var. inconspicua",
                "Achnanthes minutissima var. macrocephala" ,
                "Achnanthes minutissima var. saprophila",
                "Achnanthes minutissima var. scotica"
        ),
        to = "Achnanthes minutissima (långsmal)"
)

fixer(from = "Achnanthes oblongella", to = "Achnanthes oblongella Oestrup")
fixer(from = "Achnanthidium minutissima var. affinis", to = "Achnanthidium affine")

fixer(
        from = c("Amphora veneta",
                 "Amphora veneta Kützing f. anormale"),
        to = "Amphora veneta Kützing"
)

# C -----------------------------------------------------------------------
fixer(from = "Cocconeis placentula Ehrenberg var.euglypta (Ehr.) Grunow", to = "Cocconeis placentula")

fixer(
        from = c(
                "Craticula halophilioides",
                "Craticula halophilioides (Hustedt) Lange-Bertalot"
        ),
        to   = "Craticula halophila"
)

data2[original_name %in% c("Cymbella amphicephala", 
                           "Cymbella amphicephala Naegeli", 
                           "Cymbella amphicephala var. citrus"), 
      c("species_clean", "name_source") := .("Cymbopleura amphicephala-naviculiformis-anglica",
                                                        "fwb_manual")]

fixer(from = "Cymbella delicatula", to = "Cymbella delicatula Kützing")

data2[species_clean == "Cymbella lange-bertalotii Krammer", c("species_clean", "name_source") := .(NA, "fwb_manual")]

# D -----------------------------------------------------------------------
data2[original_name %in% c(
        "Delicata delicatula",
        "Delicata delicatula (Kützing) Krammer var. alpestris Krammer",
        "Delicata delicatula (Kützing) Krammer var. delicatula",
        "Delicata delicatula var. alpestris",
        "Delicata delicatula var. angusta"
), c("species_clean", "name_source") := .(NA, "fwb_manual")]

data2[original_name == "Diploneis cf. elliptica (Kützing) Cleve", c("species_clean", "name_source") := .("Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", "fwb_manual")]
data2[original_name == "Diploneis cf. petersenii Hustedt", c("species_clean", "name_source") := .(NA, "omnida_manual")]
fixer(from = c(
        "Diploneis ovalis",
        "Diploneis ovalis (Hilse) Cleve"
        ),
      to = "Diploneis minuta"
)
 
fixer(
        from = c("Diploneis cf. petersenii Hustedt", "Diploneis petersenii"),
        to   = "Diploneis peterseni"
)

# E -----------------------------------------------------------------------

fixer(from = c("Encyonema cespitosum"),
      to = ("Encyonema caespitosum"))

fixer(
        from = c(
                "Encyonema lange-bertalotii Krammer",
                "Encyonema lange-bertalotii Krammer morphotype 1",
                "Encyonema lange-bertalotii Krammer var. obscuriformis Krammer",
                "Encyonema lange-bertalotii morfotipo 1",
                "Encyonema lange-bertalotii morphotype 1"
        ),
        to   = "Encyonema lange-bertalotii"
)

fixer (from = "Encyonema mesianum (Cholnoky) D.G. Mann in Round Crawford & Mann", to = "Encyonema mesianum")

fixer(
        from = c(
                "Encyonema silesiacum",
                "Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann",
                "Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann var.altensis Krammer",
                "Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann var.lata Krammer",
                "Encyonema silesiacum (Bleisch) D.G. Mann",
                "Encyonema silesiacum abnormal form",
                "Encyonema silesiacum var. distigmata",
                "Encyonema silesiacum var. latestriata",
                "Encyonema silesiacum var. ventriformis"
        ), 
        to = "Encyonema silesiacum var. lata"
)

data2[original_name == "Encyonopsis lange-bertalotii", c("species_clean", "name_source") := .("Encyonopsis lange-bertalotii", "omnida_manual")]

fixer(
        from = c(
                "Encyonopsis microcephala (Grunow) Krammer",
                "Encyonopsis microcephala (Grunow) Krammer f. anormale",
                "Encyonopsis microcephala var. robusta"
        ),
        to   = "Encyonopsis microcephala"
)

fixer(
        from = c(
                "Eunotia arcus",
                "Eunotia arcus Ehrenberg",
                "Eunotia arcus Ehrenberg var. arcus",
                "Eunotia arcus Ehrenberg var. arcus s.str.",
                "Eunotia arcus var. arcus",
                "Eunotia bilunaris (Ehrenberg) Schaarschmidt",
                "Eunotia bilunaris var. bilunaris",
                "Eunotia bilunaris var. bilunaris (Ehrenberg) Mills",
                "Eunotia bilunaris var. linearis",
                "Eunotia bilunaris var. mucophila",
                "Eunotia bilunaris var. mucophila Lange-Bertalot & Nörpel"
        ),
        to = "Eunotia bilunaris"
)

data2[original_name == "Eunotia cf. boreotenuis Nörpel-Sch. & Lange-Bertalot", c("species_new", "name_source") := .("Eunotia Complex", "fwb_manual")]
data2[original_name == "Eunotia cf. meisteri Hustedt"                        , c("species_new", "name_source") := .("Eunotia  exigua/elegans Complex", "fwb_manual")]

fixer(from = c(
        "Eunotia exigua",
        "Eunotia exigua (Brebisson ex Kützing) Rabenhorst",
        "Eunotia exigua (Brébisson) Rabenhorst",
        "Eunotia exigua [1]",
        "Eunotia exigua var. exigua",
        "Eunotia exigua var. gibba",
        "Eunotia exigua var. tenella"),
      to   = "Eunotia elegans")

fixer(
        from = c(
                "Eunotia incisa",
                "Eunotia incisa Gregory",
                "Eunotia incisa Gregory var.incisa"
        ),
        to   = "Eunotia incisa var. incisa"
)

fixer(from = c("Eunotia parallela"),
      to   = "Eunotia parallela var. angusta")

fixer(
        from = c(
                "Eunotia pectinalis",
                "Eunotia pectinalis (Kützing) Rabenhorst var.pectinalis",
                "Eunotia pectinalis [1]",
                "Eunotia pectinalis v.minor",
                "Eunotia pectinalis var undulata",
                "Eunotia pectinalis var. pectinalis",
                "Eunotia pectinalis var. recta",
                "Eunotia pectinalis var. undulata",
                "Eunotia pectinalis var. ventralis (Ehrenberg) Hustedt",
                "Eunotia pectinalis(Kütz.)Rabenhorst var.recta A.Mayer ex Patrick"
                
        ),
        to   = "Eunotia minor"
)

# F -----------------------------------------------------------------------

fixer(
        from = c(
                "Fallacia lenzi",
                "Fallacia lenzi(Hustedt) Lange-Bertalot"
        ),
        to = "Fallacia lenzii"
)


fixer (
        from = c("Fragilaria biceps",
                 "Fragilaria biceps f. teratogene"),
        to = "Fragilaria biceps (Kützing) Lange-Bertalot"
)

fixer(
        from = c(
                "Fragilaria construens",
                "Fragilaria construens f. binodis",
                "Fragilaria construens f. construens",
                "Fragilaria construens f. venter",
                "Fragilaria construens f. venter (Ehrenberg) Hustedt",
                "Fragilaria construens for. venter",
                "Fragilaria construens var. subsalina"
        ),
        to = "Fragilaria construens f. binodis (Ehrenberg.) Hustedt"
)

fixer(
        from = c(
                "Fragilaria leptostauron",
                "Fragilaria leptostauron (Ehr.) Hustedt var. leptostauron",
                "Fragilaria leptostauron (Ehrenberg) Hustedt",
                "Fragilaria leptostauron var. leptostauron"
        ),
        to = "Staurosira leptostauron"
)

fixer(
        from = c("Fragilaria leptostauron var. dubia (Grunow) Hustedt"),
        to = "Staurosira dubia"
)
fixer(
        from = c("Fragilaria leptostauron var. martyi"),
        to = "Staurosira leptostauron"
)

fixer (from = "Fragilaria parasitica var. parasitica"   , to = "Fragilaria parasitica")
fixer (from = "Fragilaria parasitica var. subconstricta", to = "Fragilaria parasitica var. subconstricta Grunow")

fixer(
        from = c(
                "Fragilaria pinnata Ehrenberg",
                "Fragilaria pinnata",
                "Fragilaria pinnata f. teratogene"
        ),
        to = "Fragilaria pinnata Ehrenberg var. pinnata"
)

fixer(
        from = c(
                "Fragilaria ulna (Nitzsch.) Lange-Bertalot var. ulna",
                "Fragilaria ulna (Nitzsch.)Lange-Bertalot var.acus (Kütz.) Lange-Bertalot",
                "Fragilaria ulna angustissima-Sippen",
                "Fragilaria ulna for. angustissima",
                "Fragilaria ulna for. angustissima forma anormal",
                "Fragilaria ulna species",
                "Fragilaria ulna var acus",
                "Fragilaria ulna var. acus",
                "Fragilaria ulna var. aequalis",
                "Fragilaria ulna var. amphirhynchus",
                "Fragilaria ulna var. oxyrhynchus",
                "Fragilaria ulna var. ulna",
                "Fragilaria ulna var. ulna (Nitzsch) Lange-Bertalot",
                "Fragilaria ulna"
        ),
        to = "Fragilaria ulna var. danica (Kützing) Lange-Bertalot"
)

fixer (
        from = c(
                "Fragilaria vaucheriae",
                "Fragilaria vaucheriae (K.) Pet. var.elliptica Manguin ex Kociolek & Revier",
                "Fragilaria vaucheriae (Kützing) Petersen",
                "Fragilaria vaucheriae var. capitellata"
        ),
        to = "Fragilaria rumpens"
)

# G -----------------------------------------------------------------------
data2[original_name == "Gomphonema cf. bozenae Lange-Bertalot & Reichardt", c("species_clean", "name_source") := .("Gomphonema bozenae"         , "omnida_manual")]
data2[original_name == "Gomphonema cf. clavatum Ehrenberg"                , c("species_clean", "name_source") := .("Gomphonema clavatulum"      , "fwb_manual"   )]
data2[original_name == "Gomphonema cf. gracile Ehrenberg"                 , c("species_clean", "name_source") := .("Gomphonema gracile complex" , "fwb_manual"   )]
data2[original_name == "Gomphonema cf. interpositum Reichardt"            , c("species_clean", "name_source") := .(NA                           , "fwb_manual"   )]
data2[original_name == "Gomphonema cf. pumilum (Grunow) Reichardt"        , c("species_clean", "name_source") := .("Gomphonema pumilum complex" , "fwb_manual"   )]
data2[original_name == "Gomphonema sp.tabilissimum"                       , c("species_clean", "name_source") := .("Gomphonema spectabilissimum", "omnida_manual")]

fixer (from = c("Gyrosigma sciotoense"), to = "Gyrosigma sciotense")


# M -----------------------------------------------------------------------
fixer(
        from = c(
                "Mayamaea atomus",
                "Mayamaea atomus (Kützing) Lange-Bertalot",
                "Mayamaea atomus (Kützing) Lange-Bertalot var.atomus",
                "Mayamaea atomus var. alcimonica (Reichardt) Reichardt",
                "Mayamaea atomus var. permitis abnormal form"
        ),
        to = "Mayamaea atomus var. alcimonica"
)
        
fixer(from = c("Mayamaea fossalis var. fossalis"),
      to = "Mayamaea fossalis")
fixer(from = c("Melosira varians Agardh"), to = "Melosira varians")
fixer(
        from = c(
                "Meridion circulare",
                "Meridion circulare (Greville) Agardh",
                "Meridion circulare (Greville) Agardh var.constrictum (Ralfs) Van Heurck",
                "Meridion circulare (Greville) C.A.Agardh var. circulare",
                "Meridion circulare abnormal form",
                "Meridion circulare f. teratogene",
                "Meridion circulare var. circulare",
                "Meridion circulare var. constrictum (Ralfs) Van Heurck"
        ),
        to = "Meridion circulare var. circulare"
)

# N -----------------------------------------------------------------------

fixer(from = c(
        "Navicula atomus var alcimonica",
        "Navicula atomus var. atomus",
        "Navicula atomus var. permitis"
        ),
      to = "Navicula atomus v.recondita"
)


fixer(from = "Navicula capitata", to = "Navicula capitata var. linearis")
fixer(from = "Navicula capitata var. hungarica", to = "Navicula capitata v.hungarica")

data2[original_name %in% c("Navicula cataracta-rheni",
                           "Navicula cataracta-rheni Lange-Bertalot"),
      c("species_clean", "name_source") := .("Navicula cataracta-rheni", "omnida_manual")]

fixer(from = c(
        "Navicula viridula",
        "Navicula viridula (Kützing) Ehrenberg",
        "Navicula viridula var. germainii",
        "Navicula viridula var. rostellata",
        "Navicula viridulacalcis ssp. neomundana"
        ), 
      to = "Navicula viridula var. viridula")

fixer(
        from = c(
                "Nitzschia angusta",
                "Nitzschia angustata",
                "Nitzschia angustata (W. Smith) Grunow",
                "Nitzschia angustata var. producta"
        ),
        to = "Nitzschia angustata Grunow"
)

data2[original_name == "Nitzschia cf. bavarica Hustedt"                     , c("species_clean", "name_source") := .("Nitzschia bavarica Complex"       , "fwb_manual")] 
data2[original_name == "Nitzschia cf. dissipata var. media (Kützing) Grunow", c("species_clean", "name_source") := .("Nitzschia dissipata-recta Complex", "fwb_manual")]
data2[original_name == "Nitzschia cf. palea (Kützing) W. Smith"             , c("species_clean", "name_source") := .("Nitzschia palea-paleacea"         , "fwb_manual")]

fixer(
        from = c(
                "Nitzschia fonticola",
                "Nitzschia fonticola Grunow in Cleve et Möller",
                "Nitzschia fonticola Grunow in Cleve et Möller f. anormale",
                "Nitzschia fonticola Grunow in Van Heurck",
                "Nitzschia fonticola Grunow var.pelagica Hustedt in Schmidt & al.",
                "Nitzschia fonticola abnormal form",
                "Nitzschia fonticola forma anormal",
                "Nitzschia fonticola var. pelagica"
        ),
        to = "Nitzschia fonticola var. fonticola"
)
fixer(
        from = c(
                "Nitzschia frustulum",
                "Nitzschia frustulum (Kützing) Grunow var.frustulum",
                "Nitzschia frustulum abnormal form",
                "Nitzschia frustulum var. inconspicua",
                "Nitzschia frustulum var. minutula",
                "Nitzschia frustulum(Kützing)Grunow var.frustulum"
        ),
        to = "Nitzschia frustulum var. frustulum"
)
fixer (from = "Nitzschia hungarica" , to = "Nitzschia hungarica Grunow")
fixer (from = "Nitzschia levidensis", to = "Nitzschia levidensis (W.Smith) Grunow in Van Heurck")
fixer (
        from = c(
                "Nitzschia liebetruthii",
                "Nitzschia liebetruthii Rabenhorst var.liebetruthii"
        ),
        to = "Nitzschia liebetruthii var. liebetruthii"
)
        
fixer(
        from = c(
                "Nitzschia linearis",
                "Nitzschia linearis (Agardh) W.M. Smith var.tenuis (W.Smith) Grunow in Cleve & Grunow",
                "Nitzschia linearis v.tenuis",
                "Nitzschia linearis var. tenuis",
                "Nitzschia linearis(Agardh) W.M.Smith var.linearis",
                "Nitzschia linearis(Agardh) W.M.Smith var.subtilis(Grunow) Hustedt"
        ),
        to = "Nitzschia linearis var. linearis"
)
fixer(
        from = c(
                "Nitzschia palea",
                "Nitzschia palea (Kützing) W. Smith",
                "Nitzschia palea (Kützing) W.Smith",
                "Nitzschia palea (Kützing) W.Smith f. anormale",
                "Nitzschia palea (Kützing) W.Smith var. palea",
                "Nitzschia palea (Kützing) W.Smith var.debilis(Kützing)Grunow in Cl. & Grun",
                "Nitzschia palea abnormal form",
                "Nitzschia palea f. major",
                "Nitzschia palea forma anormal",
                "Nitzschia palea group debilis",
                "Nitzschia palea group tenuirostris",
                "Nitzschia palea var. perminuta",
                "Nitzschia paleacea (Grunow) Grunow f. anormale",
                "Nitzschia paleacea (Grunow) Grunow in van Heurck",
                "Nitzschia paleacea abnormal form",
                "Nitzschia paleacea f. acicularioides",
                "Nitzschia paleacea f. teratogene",
                "Nitzschia paleaeformis Hustedt"
        ),
        to = "Nitzschia palea var. debilis"
)
fixer(
        from = c(
                "Nitzschia sinuata",
                "Nitzschia sinuata (Thwaites) Grunow var.tabellaria Grunow",
                "Nitzschia sinuata var. delognei"
        ),
        to = "Nitzschia sinuata var. tabellaria"
)
 

# P -----------------------------------------------------------------------
fixer(from = "Pinnularia acrospheria", to = "Pinnularia acrosphaeria")
fixer(
        from = c(
                "Pinnularia borealis",
                "Pinnularia borealis Ehrenberg var.scalaris (Ehr.) Rabenhorst",
                "Pinnularia borealis var. scalaris"
        ),
        to = "Pinnularia borealis var. borealis"
)
data2[original_name == "Pinnularia cf. pisciculus Ehrenberg", c("species_clean", "name_source") := .("Pinnularia pisciculus", "maria_manual")]

fixer(from = "Pinnularia major", to = "Pinnularia maior")


fixer(
        from = c(
                "Pinnularia subcapitata",
                "Pinnularia subcapitata Gregory",
                "Pinnularia subcapitata Gregory var. elongata Krammer",
                "Pinnularia subcapitata Gregory var. subcapitata",
                "Pinnularia subcapitata [1]",
                "Pinnularia subcapitata var hilseana",
                "Pinnularia subcapitata var. elongata Krammer",
                "Pinnularia subcapitata var. paucistriata"
        ),
        to = "Pinnularia subcapitata var. subcapitata"
)
fixer(
        from = c("Pinnularia subgibba",
                 "Pinnularia subgibba Krammer"),
        to = "Pinnularia subgibba var. undulata"
)

fixer(from = "Psammothidium oblongellum", to = "Psammothidium oblongellum(Oestrup) Van de Vijver")

fixer(
        from = c(
                "Pseudostaurosira parasitica",
                "Pseudostaurosira parasitica (W.Smith) Morales"
        ),
        to = "Pseudostaurosira parasitica var. subconstricta"
)




# S -----------------------------------------------------------------------
fixer(
        from = c(
                "Stauroneis smithii",
                "Stauroneis smithii Grunow",
                "Stauroneis smithii var. karelica"
        ),
        to = "Stauroneis smithii var. minima"
)
fixer(
        from = c(
                "Staurosira construens var. venter"
        ),
        to = "Staurosira construens var. construens"
)

fixer(from = "Staurosira pinnata", to = "Staurosira pinnata Ehrenberg")

fixer(
        from = c(
                "Surirella linearis",
                "Surirella linearis W. M. Smith",
                "Surirella linearis W.M.Smith in Schmidt & al."
        ),
        to = "Surirella linearis var. constricta"
)

# T -----------------------------------------------------------------------
fixer(from = c("Tryblionella constricta", "Tryblionella constricta(Kützing) Poulin in Poulin & al."), to = "Tryblionella constricta(Kützing) Poulin")



