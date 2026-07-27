  $ OCAMLRUNPARAM=b niagara --test ../examples/opposition.nga <<EOF
  > 1: rbd += 100000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 4000, 4000 }:
         1600 -> distrib
         default 2400 -> rnc
       - rnc { 2400, 2400 }:
         2400 -> distrib[frais]
       - sofica delta { 280, 280 }:
       - distrib { 4000, 4000 }:
         - distrib[frais] { 2400, 2400 }:
         
       
     ++ after event recup_frais :
       - rbd { 96000, 100000 }:
         38400 -> distrib
         default 57600 -> rnc
       - rnc { 57600, 60000 }:
         default 57600 -> rnpp
       - rnpp { 57600, 57600 }:
         11520 -> sofica[recup]
         default 46080 -> prod[residuel]
       - prod { 46080, 46080 }:
         - prod[residuel] { 46080, 46080 }:
         - prod[sofopp] { 1110, 1390 }:
         
       - sofica { 11520, 11520 }:
         - sofica[recup] { 11520, 11520 }:
         
       - sofica delta { 1110, 1390 }:
       - distrib { 38400, 42400 }:
         
       
     
  $ OCAMLRUNPARAM=b niagara --test ../examples/opposition.nga --for sofica <<EOF
  > 1: rbd += 78000$
  > 2: rbd += 22000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 2250, 2250 }:
         450 -> distrib
         default 1800 -> rnc @sofica
       - rnc @sofica { 1800, 1800 }:
         1800 -> distrib[frais] @sofica
       - distrib[frais] @sofica { 1800, 1800 }:
       
     ++ after event recup_frais @sofica :
       - rbd { 75000, 77250 }:
         15000 -> distrib
         default 60000 -> rnc @sofica
       - rnc @sofica { 60000, 61800 }:
         default 60000 -> rnpp @sofica
       - rnpp @sofica { 60000, 60000 }:
         12000 -> sofica[recup] @sofica
       - sofica @sofica { 12000, 12000 }:
         - sofica[recup] @sofica { 12000, 12000 }:
         
       - sofica[recup] { 8790, 8790 }:
       - sofica delta { 3210, 3210 }:
       
     ++ after event recup_sofica @sofica :
       - rbd { 750, 78000 }:
         150 -> distrib
         default 600 -> rnc @sofica
       - rnc @sofica { 600, 62400 }:
         default 600 -> rnpp @sofica
       - rnpp @sofica { 600, 60600 }:
         30 -> sofica[residuel] @sofica
       - sofica @sofica { 30, 12030 }:
         - sofica[residuel] @sofica { 30, 30 }:
         
       - sofica[recup] { 90, 8880 }:
       - sofica delta { -60, 3150 }:
       
     
  2: ++ no events:
       - rbd { 22000, 100000 }:
         4400 -> distrib
         default 17600 -> rnc @sofica
       - rnc @sofica { 17600, 80000 }:
         default 17600 -> rnpp @sofica
       - rnpp @sofica { 17600, 78200 }:
         880 -> sofica[residuel] @sofica
       - sofica @sofica { 880, 12910 }:
         - sofica[residuel] @sofica { 880, 910 }:
         
       - sofica[recup] { 2640, 11520 }:
       - sofica delta { -1760, 1390 }:
       
     
  $ OCAMLRUNPARAM=b niagara --test ../examples/opposition.nga --for prod <<EOF
  > 1: rbd += 100000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 4000, 4000 }:
         1600 -> distrib
         default 2400 -> rnc
       - rnc { 2400, 2400 }:
         2400 -> distrib[frais]
       - distrib[frais] { 2400, 2400 }:
       
     ++ after event recup_frais :
       - rbd { 96000, 100000 }:
         38400 -> distrib
         default 57600 -> rnc
       - rnc { 57600, 60000 }:
         default 57600 -> rnpp
       - rnpp { 57600, 57600 }:
         11520 -> sofica[recup]
         default 46080 -> prod[residuel]
       - prod { 46080, 46080 }:
         - prod[residuel] { 46080, 46080 }:
         - prod[sofopp] { 1110, 1390 }:
         
       - sofica[recup] { 11520, 11520 }:
       
     
  $ OCAMLRUNPARAM=b niagara --test ../examples/opposition.nga --forall <<EOF
  > 1: rbd += 50000$
  > 2: rbd += 50000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  2: ++ no events:
       - sofica @sofica { 12910, 12910 }:
         - sofica[residuel] @sofica { 910, 910 }:
         - sofica[recup] @sofica { 12000, 12000 }:
         
       - prod { 46080, 46080 }:
         - prod[residuel] { 46080, 46080 }:
         - prod[sofopp] { 1390, 1390 }:
         
       - distrib { 42400, 42400 }:
         - distrib[frais] { 2400, 2400 }:
         
       
     
