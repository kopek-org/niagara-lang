  $ OCAMLRUNPARAM=b niagara --test ../examples/opp_group.nga <<EOF
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
       - pool_sofica delta { 350, 350 }:
       - distrib { 4000, 4000 }:
         - distrib[frais] { 2400, 2400 }:
         
       
     ++ after event recup_frais :
       - rbd { 80000, 84000 }:
         32000 -> distrib
         default 48000 -> rnc
       - rnc { 48000, 50400 }:
         default 48000 -> rnpp
       - rnpp { 48000, 48000 }:
         7200 -> sofi1[recup]
         4800 -> sofi2[recup]
         default 36000 -> prod[residuel]
       - sofi1 { 7200, 7200 }:
         - sofi1[recup] { 7200, 7200 }:
         
       - prod { 36000, 36000 }:
         - prod[residuel] { 36000, 36000 }:
         
       - sofi2 { 4800, 4800 }:
         - sofi2[recup] { 4800, 4800 }:
         
       - pool_sofica delta { 1390, 1740 }:
       - distrib { 32000, 36000 }:
         
       
     ++ after event recup_sofica :
       - rbd { 16000, 100000 }:
         6400 -> distrib
         default 9600 -> rnc
       - rnc { 9600, 60000 }:
         default 9600 -> rnpp
       - rnpp { 9600, 57600 }:
         480 -> sofi1[residuel]
         480 -> sofi2[residuel]
         default 8640 -> prod[residuel]
       - sofi1 { 480, 7680 }:
         - sofi1[residuel] { 480, 480 }:
         
       - prod { 8640, 44640 }:
         - prod[residuel] { 8640, 44640 }:
         
       - sofi2 { 480, 5280 }:
         - sofi2[residuel] { 480, 480 }:
         
       - pool_sofica delta { 320, 2060 }:
       - distrib { 6400, 42400 }:
         
       
     
  $ OCAMLRUNPARAM=b niagara --test ../examples/opp_group.nga --for sofi1 <<EOF
  > 1: rbd += 78000$
  > 2: rbd += 22000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 2250, 2250 }:
         450 -> distrib
         default 1800 -> rnc @pool_sofica
       - rnc @pool_sofica { 1800, 1800 }:
         1800 -> distrib[frais] @pool_sofica
       - distrib[frais] @pool_sofica { 1800, 1800 }:
       
     ++ after event recup_frais @pool_sofica :
       - rbd { 60000, 62250 }:
         12000 -> distrib
         default 48000 -> rnc @pool_sofica
       - rnc @pool_sofica { 48000, 49800 }:
         default 48000 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 48000, 48000 }:
         7200 -> sofi1[recup] @pool_sofica
         4800 -> sofi2[recup] @pool_sofica
       - sofi1 @pool_sofica { 7200, 7200 }:
         - sofi1[recup] @pool_sofica { 7200, 7200 }:
         
       - sofi2[recup] @pool_sofica { 4800, 4800 }:
       - sofi1[recup] { 5242.5, 5242.5 }:
       
     ++ after event recup_sofica @pool_sofica :
       - rbd { 15750, 78000 }:
         3150 -> distrib
         default 12600 -> rnc @pool_sofica
       - rnc @pool_sofica { 12600, 62400 }:
         default 12600 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 12600, 60600 }:
         630 -> sofi1[residuel] @pool_sofica
       - sofi1 @pool_sofica { 630, 7830 }:
         - sofi1[residuel] @pool_sofica { 630, 630 }:
         
       - sofi1[recup] { 1417.5, 6660 }:
       
     
  2: ++ no events:
       - rbd { 22000, 100000 }:
         4400 -> distrib
         default 17600 -> rnc @pool_sofica
       - rnc @pool_sofica { 17600, 80000 }:
         default 17600 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 17600, 78200 }:
         880 -> sofi1[residuel] @pool_sofica
       - sofi1 @pool_sofica { 880, 8710 }:
         - sofi1[residuel] @pool_sofica { 880, 1510 }:
         
       - sofi1[residuel] { 480, 480 }:
       - sofi1[recup] { 540, 7200 }:
       
     
  $ OCAMLRUNPARAM=b niagara --test ../examples/opp_group.nga --for sofi2 <<EOF
  > 1: rbd += 78000$
  > 2: rbd += 22000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 2250, 2250 }:
         450 -> distrib
         default 1800 -> rnc @pool_sofica
       - rnc @pool_sofica { 1800, 1800 }:
         1800 -> distrib[frais] @pool_sofica
       - distrib[frais] @pool_sofica { 1800, 1800 }:
       
     ++ after event recup_frais @pool_sofica :
       - rbd { 60000, 62250 }:
         12000 -> distrib
         default 48000 -> rnc @pool_sofica
       - rnc @pool_sofica { 48000, 49800 }:
         default 48000 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 48000, 48000 }:
         7200 -> sofi1[recup] @pool_sofica
         4800 -> sofi2[recup] @pool_sofica
       - sofi1[recup] @pool_sofica { 7200, 7200 }:
       - sofi2 @pool_sofica { 4800, 4800 }:
         - sofi2[recup] @pool_sofica { 4800, 4800 }:
         
       - sofi2[recup] { 3495, 3495 }:
       
     ++ after event recup_sofica @pool_sofica :
       - rbd { 15750, 78000 }:
         3150 -> distrib
         default 12600 -> rnc @pool_sofica
       - rnc @pool_sofica { 12600, 62400 }:
         default 12600 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 12600, 60600 }:
         630 -> sofi2[residuel] @pool_sofica
       - sofi2 @pool_sofica { 630, 5430 }:
         - sofi2[residuel] @pool_sofica { 630, 630 }:
         
       - sofi2[recup] { 945, 4440 }:
       
     
  2: ++ no events:
       - rbd { 22000, 100000 }:
         4400 -> distrib
         default 17600 -> rnc @pool_sofica
       - rnc @pool_sofica { 17600, 80000 }:
         default 17600 -> rnpp @pool_sofica
       - rnpp @pool_sofica { 17600, 78200 }:
         880 -> sofi2[residuel] @pool_sofica
       - sofi2 @pool_sofica { 880, 6310 }:
         - sofi2[residuel] @pool_sofica { 880, 1510 }:
         
       - sofi2[residuel] { 480, 480 }:
       - sofi2[recup] { 360, 4800 }:
       
     
