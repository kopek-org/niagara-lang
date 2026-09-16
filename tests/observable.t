  $ OCAMLRUNPARAM=b niagara --test ../examples/observable.nga <<EOF
  > 1: entrees(France) += 10000
  > 2: entrees(Etranger) += 20000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - lierpa { -100, -100 }:
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
       - distrib delta { 100, 100 }:
       - prod { 900, 900 }:
         - prod[opp_distrib] { 100, 100 }:
         
       - lierpa { 800, 800 }:
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
       - distrib delta { 0, 100 }:
       - prod { 39200, 40100 }:
         - prod[opp_distrib] { 0, 100 }:
         
       - lierpa { 40000, 40000 }:
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
       - distrib delta { 0, 100 }:
       - prod { 160000, 200100 }:
         - prod[opp_distrib] { 0, 100 }:
         
       - lierpa { 200000, 200000 }:
       - palier { 200000, 200000 }:
       
     
  $ OCAMLRUNPARAM=b niagara --test --for distrib ../examples/observable.nga <<EOF
  > 1: entrees(France) += 10000
  > 2: entrees(Etranger) += 20000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - lierpa { -100, -100 }:
       - palier { -100, -100 }:
       - lierpa @distrib { -100, -100 }:
       - palier @distrib { -100, -100 }:
       
     
  1: ++ no events:
       - entrees(France) { 225, 225 }:
       - rbd(France) { 1125, 1125 }:
         default 1125 -> rnpp
       - rnpp { 1125, 1125 }:
         225 -> distrib @distrib
         default 900 -> prod @distrib
       - distrib @distrib { 225, 225 }:
       - prod @distrib { 900, 900 }:
       - distrib delta { 100, 100 }:
       - lierpa { 900, 900 }:
       - palier { 900, 900 }:
       - lierpa @distrib { 800, 800 }:
       - palier @distrib { 800, 800 }:
       
     ++ after event seuil @distrib :
       - entrees(France) { 9775, 10000 }:
       - rbd(France) { 48875, 50000 }:
         default 48875 -> rnpp
       - rnpp { 48875, 50000 }:
         9775 -> distrib @distrib
         default 39100 -> prod @distrib
       - distrib @distrib { 9775, 10000 }:
       - prod @distrib { 39100, 40000 }:
       - distrib delta { 0, 100 }:
       - lierpa { 40000, 40000 }:
       - palier { 40000, 40000 }:
       - lierpa @distrib { 39900, 39900 }:
       - palier @distrib { 39900, 39900 }:
       
     
  2: ++ no events:
       - entrees(Etranger) { 20000, 20000 }:
       - rbd(Etranger) { 200000, 200000 }:
         default 200000 -> rnpp
       - rnpp { 200000, 250000 }:
         40000 -> distrib @distrib
         default 160000 -> prod @distrib
       - distrib @distrib { 40000, 50000 }:
       - prod @distrib { 160000, 200000 }:
       - distrib delta { 0, 100 }:
       - lierpa { 200000, 200000 }:
       - palier { 200000, 200000 }:
       - lierpa @distrib { 199900, 199900 }:
       - palier @distrib { 199900, 199900 }:
       
     
