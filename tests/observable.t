  $ OCAMLRUNPARAM=b niagara --test ../examples/observable.nga <<EOF
  > 1: entrees(France) += 10000
  > 2: entrees(Etranger) += 20000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  1: ++ no events:
       - entrees { 250, 250 }:
         - entrees(France) { 250, 250 }:
           125000 -> rbd(France)
         
       - rbd { 125000, 125000 }:
         - rbd(France) { 125000, 125000 }:
           25000 -> distrib
           default 100000 -> rnpp
         
       - rnpp { 100000, 100000 }:
         10000 -> distrib
         default 90000 -> prod
       - distrib { 35000, 35000 }:
       - prod { 90000, 90000 }:
       - palier { 80000, 80000 }:
       
     ++ after event seuil :
       - entrees { 9750, 10000 }:
         - entrees(France) { 9750, 10000 }:
           4875000 -> rbd(France)
         
       - rbd { 4875000, 5000000 }:
         - rbd(France) { 4875000, 5000000 }:
           975000 -> distrib
           default 3900000 -> rnpp
         
       - rnpp { 3900000, 4000000 }:
         780000 -> distrib
         default 3120000 -> prod
       - distrib { 1755000, 1790000 }:
       - prod { 3120000, 3210000 }:
       - palier { 3110000, 3190000 }:
       
     
  2: ++ no events:
       - entrees { 20000, 30000 }:
         - entrees(Etranger) { 20000, 20000 }:
           20000000 -> rbd(Etranger)
         
       - rbd { 20000000, 25000000 }:
         - rbd(Etranger) { 20000000, 20000000 }:
           default 20000000 -> rnpp
         
       - rnpp { 20000000, 24000000 }:
         4000000 -> distrib
         default 16000000 -> prod
       - distrib { 4000000, 5790000 }:
       - prod { 16000000, 19210000 }:
       - palier { 15990000, 19180000 }:
       
     
