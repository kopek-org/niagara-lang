  $ OCAMLRUNPARAM=b niagara --test ../examples/constraint_formula.nga <<EOF
  > 1: rbd += 5000$
  > 2: rbd += 10000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 2000, 2000 }:
         1000 -> distrib
         default 1000 -> prod
       - distrib { 1000, 1000 }:
       - prod { 1000, 1000 }:
       
     ++ after event recup_frais :
       - rbd { 3000, 5000 }:
         300 -> distrib
         default 2700 -> prod
       - distrib { 300, 1300 }:
       - b { 2700, 2700 }:
       - a { 300, 300 }:
       - prod { 2700, 3700 }:
       
     
  2: ++ no events:
       - rbd { 7000, 12000 }:
         700 -> distrib
         default 6300 -> prod
       - distrib { 700, 2000 }:
       - b { 6300, 9000 }:
       - a { 700, 1000 }:
       - prod { 6300, 10000 }:
       
     ++ after event apres_apres :
       - rbd { 3000, 15000 }:
         300 -> distrib
         default 2700 -> prod
       - distrib { 300, 2300 }:
       - b { 2700, 11700 }:
       - a { 300, 1300 }:
       - prod { 2700, 12700 }:
       
     
