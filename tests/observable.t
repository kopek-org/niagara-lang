  $ OCAMLRUNPARAM=b niagara --test ../examples/observable.nga <<EOF
  > 1: entrees(France) += 10000
  > 2: entrees(Etranger) += 20000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  1: ++ no events:
       - entrees { 200, 200 }:
         - entrees(France) { 200, 200 }:
         
       - rbd { 100000, 100000 }:
         - rbd(France) { 100000, 100000 }:
           default 100000 -> rnpp
         
       - rnpp { 100000, 100000 }:
         10000 -> distrib
         default 90000 -> prod
       - distrib { 10000, 10000 }:
       - prod { 90000, 90000 }:
       - palier { 80000, 80000 }:
       
     ++ after event seuil :
       - entrees { 9800, 10000 }:
         - entrees(France) { 9800, 10000 }:
         
       - rbd { 4900000, 5000000 }:
         - rbd(France) { 4900000, 5000000 }:
           default 4900000 -> rnpp
         
       - rnpp { 4900000, 5000000 }:
         980000 -> distrib
         default 3920000 -> prod
       - distrib { 980000, 990000 }:
       - prod { 3920000, 4010000 }:
       - palier { 4000000, 4000000 }:
       
     
  2: ++ no events:
       - entrees { 20000, 30000 }:
         - entrees(Etranger) { 20000, 20000 }:
         
       - rbd { 20000000, 25000000 }:
         - rbd(Etranger) { 20000000, 20000000 }:
           default 20000000 -> rnpp
         
       - rnpp { 20000000, 25000000 }:
         4000000 -> distrib
         default 16000000 -> prod
       - distrib { 4000000, 4990000 }:
       - prod { 16000000, 20010000 }:
       - palier { 20000000, 20000000 }:
       
     
