  $ OCAMLRUNPARAM=b niagara --test ../examples/backing.nga <<EOF
  > 1: rbd += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 10000, 10000 }:
         300 -> auteur
         2000 -> distrib
         default 8000 -> rnpp
       - rnpp { 8000, 8000 }:
         400 -> auteur
         3200 -> sofica
         default 4800 -> prod
       - auteur { 700, 700 }:
       - prod { 4800, 4800 }:
         - prod[rem_auteur] { 700, 700 }:
         
       - sofica { 3200, 3200 }:
       - distrib { 2000, 2000 }:
       
     

  $ OCAMLRUNPARAM=b niagara --test ../examples/backing.nga --for sofica <<EOF
  > 1: rbd += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 10000, 10000 }:
         2000 -> distrib
         default 8000 -> rnpp
       - rnpp { 8000, 8000 }:
         3200 -> sofica
       - sofica { 3200, 3200 }:
       
     

  $ OCAMLRUNPARAM=b niagara --test ../examples/backing.nga --for auteur <<EOF
  > 1: rbd += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - rbd { 10000, 10000 }:
         300 -> auteur
         2000 -> distrib
         default 8000 -> rnpp
       - rnpp { 8000, 8000 }:
         400 -> auteur
       - auteur { 700, 700 }:
       
     
