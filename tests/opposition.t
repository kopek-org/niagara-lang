  $ OCAMLRUNPARAM=b niagara --test ../examples/opposition.nga <<EOF
  > 1: rbd += 100000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  1: ++ no events:
       - rbd { 300000., 300000. }:
         120000. -> distrib
         default 180000. -> rnc
       - rnc { 180000., 180000. }:
         180000. -> distrib[frais]
       - rnc @sofica { 240000., %not_computed% }:
         240000. -> distrib[frais] @sofica
       - distrib { 300000., 300000. }:
         - distrib[frais] { 180000., 180000. }:
         
       - distrib[frais] @sofica { 240000., 240000. }:
       
     ++ after event recup_frais @sofica :
       - rbd { 100000., 400000. }:
         40000. -> distrib
         default 60000. -> rnc
       - rnc { 60000., 240000. }:
         60000. -> distrib[frais]
       - rnc @sofica { 80000., %not_computed% }:
         default 80000. -> rnpp @sofica
       - rnpp @sofica { 80000., %not_computed% }:
         16000. -> sofica[recup] @sofica
       - distrib { 100000., 400000. }:
         - distrib[frais] { 60000., 240000. }:
         
       - sofica @sofica { 16000., %not_computed% }:
         - sofica[recup] @sofica { 16000., 16000. }:
         
       
     ++ after event recup_frais :
       - rbd { 7400000., 7800000. }:
         2960000. -> distrib
         default 4440000. -> rnc
       - rnc { 4440000., 4680000. }:
         default 4440000. -> rnpp
       - rnpp { 4440000., 4440000. }:
         888000. -> sofica[recup]
         default 3552000. -> prod[residuel]
       - rnc @sofica { 5920000., %not_computed% }:
         default 5920000. -> rnpp @sofica
       - rnpp @sofica { 5920000., %not_computed% }:
         1184000. -> sofica[recup] @sofica
       - distrib { 2960000., 3360000. }:
         
       - prod { 3552000., 3552000. }:
         - prod[residuel] { 3552000., 3552000. }:
         
       - sofica { 888000., 888000. }:
         - sofica[recup] { 888000., 888000. }:
         
       - sofica @sofica { 1184000., %not_computed% }:
         - sofica[recup] @sofica { 1184000., 1200000. }:
         
       
     ++ after event recup_sofica @sofica :
       - rbd { 2200000., 10000000. }:
         880000. -> distrib
         default 1320000. -> rnc
       - rnc { 1320000., 6000000. }:
         default 1320000. -> rnpp
       - rnpp { 1320000., 5760000. }:
         264000. -> sofica[recup]
         default 1056000. -> prod[residuel]
       - rnc @sofica { 1760000., %not_computed% }:
         default 1760000. -> rnpp @sofica
       - rnpp @sofica { 1760000., %not_computed% }:
         88000. -> sofica[residuel] @sofica
       - distrib { 880000., 4240000. }:
         
       - prod { 1056000., 4608000. }:
         - prod[residuel] { 1056000., 4608000. }:
         
       - sofica { 264000., 1152000. }:
         - sofica[recup] { 264000., 1152000. }:
         
       - sofica @sofica { 88000., %not_computed% }:
         - sofica[residuel] @sofica { 88000., %not_computed% }:
         
       
     
