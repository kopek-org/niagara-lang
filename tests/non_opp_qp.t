  $ OCAMLRUNPARAM=b niagara --test ../examples/non_opp_qp.nga <<EOF
  > 1: rbd += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  1: ++ no events:
       - rbd { 10000, 10000 }:
         2000 -> distrib
         default 10000 -> rnpp
       - distrib { 2000, 2000 }:
       - rnpp { 10000, 10000 }:
         4000 -> sofica
         default 6000 -> prod
       - prod { 6000, 6000 }:
         - prod[depassement_rbd] { 2000, 2000 }:
           2000 -> rbd
         
       - sofica { 4000, 4000 }:
       
     
