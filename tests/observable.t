  $ OCAMLRUNPARAM=b niagara --test ../examples/observable.nga <<EOF
  > 1: entrees(France) += 10000
  > 2: entrees(Etranger) += 20000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - palier { -100, -100 }:
       
     
  1: ++ no events:
       - entrees { 200, 200 }:
         - entrees(France) { 200, 200 }:
         
       - rbd { 1000, 1000 }:
         - rbd(France) { 1000, 1000 }:
           default 1000 -> rnpp
         
       - rnpp { 1000, 1000 }:
         100 -> distrib
         default 900 -> prod
       - distrib { 100, 100 }:
       - prod { 900, 900 }:
       - palier { 800, 800 }:
       
     ++ after event seuil :
       - entrees { 9800, 10000 }:
         - entrees(France) { 9800, 10000 }:
         
       - rbd { 49000, 50000 }:
         - rbd(France) { 49000, 50000 }:
           default 49000 -> rnpp
         
       - rnpp { 49000, 50000 }:
         9800 -> distrib
         default 39200 -> prod
       - distrib { 9800, 9900 }:
       - prod { 39200, 40100 }:
       - palier { 40000, 40000 }:
       
     
  2: ++ no events:
       - entrees { 20000, 30000 }:
         - entrees(Etranger) { 20000, 20000 }:
         
       - rbd { 200000, 250000 }:
         - rbd(Etranger) { 200000, 200000 }:
           default 200000 -> rnpp
         
       - rnpp { 200000, 250000 }:
         40000 -> distrib
         default 160000 -> prod
       - distrib { 40000, 49900 }:
       - prod { 160000, 200100 }:
       - palier { 200000, 200000 }:
       
     
