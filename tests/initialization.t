  $ OCAMLRUNPARAM=b niagara --test ../examples/initialization.nga <<EOF
  > init sofica = -400$
  > 1: entrees += 10000
  > EOF
  Awaiting inputs:
  Missing mandatory value for prod at init.
  [50]
  $ OCAMLRUNPARAM=b niagara --test ../examples/initialization.nga <<EOF
  > init sofica = -400$
  > init prod = 0$
  > 1: entrees += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - palier { -10000, -10000 }:
       
     
  1: ++ no events:
       - entrees { 321.42857..., 321.42857... }:
       - rbd { 160714.28571..., 160714.28571... }:
         32142.85714... -> distrib
         default 128571.42857... -> rnpp
       - rnpp { 128571.42857..., 128571.42857... }:
         12857.14285... -> distrib
         25714.28571... -> sofica
         default 90000 -> prod
       - distrib { 45000, 45000 }:
       - prod { 90000, 90000 }:
         - prod[opp] { 3214.28571..., 3214.28571... }:
         
       - sofica { 25714.28571..., -14285.71428... }:
       - sofica delta { 3214.28571..., 3214.28571... }:
       - palier { 80000, 80000 }:
       
     ++ after event seuil :
       - entrees { 9678.57142..., 10000 }:
       - rbd { 4839285.71428..., 5000000 }:
         967857.14285... -> distrib
         default 3871428.57142... -> rnpp
       - rnpp { 3871428.57142..., 4000000 }:
         1161428.57142... -> distrib
         774285.71428... -> sofica
         default 1935714.28571... -> prod
       - distrib { 2129285.71428..., 2174285.71428... }:
       - prod { 1935714.28571..., 2025714.28571... }:
         - prod[opp] { 96785.71428..., 100000 }:
         
       - sofica { 774285.71428..., 760000 }:
       - sofica delta { 96785.71428..., 100000 }:
       - palier { 2015714.28571..., 2015714.28571... }:
       
     

  $ OCAMLRUNPARAM=b niagara --test --for sofica ../examples/initialization.nga <<EOF
  > init sofica for sofica = 500$
  > init prod = 0$
  > 1: entrees += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - palier { -10000, -10000 }:
       
     
  1: ++ no events:
       - entrees { 10000, 10000 }:
       - rbd { 5000000, 5000000 }:
         500000 -> distrib
         default 4500000 -> rnpp @sofica
       - rnpp @sofica { 4500000, 4500000 }:
         900000 -> sofica @sofica
       - sofica @sofica { 900000, 950000 }:
       - sofica delta { 100000, 100000 }:
       - palier { 2015714.28571..., 2015714.28571... }:
       
     
