  $ OCAMLRUNPARAM=b niagara --test ../examples/base_rep.nga <<EOF
  > 1: x += 100000$
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       
     
  1: ++ no events:
       - x { 100000, 100000 }:
         20000 -> j[x]
         default 80000 -> i[x]
       - x3 { 300000, 300000 }:
         30000 -> j[x3]
       - x2 { 200000, 200000 }:
         40000 -> j[x2]
         default 160000 -> i[x2]
       - i { 240000, 240000 }:
         - i[j] { 70000, 70000 }:
         - i[x2] { 160000, 160000 }:
         - i[x] { 80000, 80000 }:
         
       - j { 90000, 90000 }:
         - j[x3] { 30000, 30000 }:
         - j[i] { 160000, 160000 }:
         - j[x2] { 40000, 40000 }:
         - j[x] { 20000, 20000 }:
         
       
     
