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
       - palier { -100, -100 }:
       
     
  1: ++ no events:
       - entrees { 321.42857..., 321.42857... }:
       - rbd { 1607.14285..., 1607.14285... }:
         321.42857... -> distrib
         default 1285.71428... -> rnpp
       - rnpp { 1285.71428..., 1285.71428... }:
         128.57142... -> distrib
         257.14285... -> sofica
         default 900 -> prod
       - distrib { 450, 450 }:
       - prod { 900, 900 }:
         - prod[opp] { 32.14285..., 32.14285... }:
         
       - sofica { 257.14285..., -142.85714... }:
       - sofica delta { 32.14285..., 32.14285... }:
       - palier { 800, 800 }:
       
     ++ after event seuil :
       - entrees { 9678.57142..., 10000 }:
       - rbd { 48392.85714..., 50000 }:
         9678.57142... -> distrib
         default 38714.28571... -> rnpp
       - rnpp { 38714.28571..., 40000 }:
         11614.28571... -> distrib
         7742.85714... -> sofica
         default 19357.14285... -> prod
       - distrib { 21292.85714..., 21742.85714... }:
       - prod { 19357.14285..., 20257.14285... }:
         - prod[opp] { 967.85714..., 1000 }:
         
       - sofica { 7742.85714..., 7600 }:
       - sofica delta { 967.85714..., 1000 }:
       - palier { 20157.14285..., 20157.14285... }:
       
     

  $ OCAMLRUNPARAM=b niagara --test --for sofica ../examples/initialization.nga <<EOF
  > init sofica for sofica = 500$
  > init prod = 0$
  > 1: entrees += 10000
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ no events:
       - palier { -100, -100 }:
       
     
  1: ++ no events:
       - entrees { 10000, 10000 }:
       - rbd { 50000, 50000 }:
         5000 -> distrib
         default 45000 -> rnpp @sofica
       - rnpp @sofica { 45000, 45000 }:
         9000 -> sofica @sofica
       - sofica @sofica { 9000, 9500 }:
       - sofica delta { 1000, 1000 }:
       - palier { 20157.14285..., 20157.14285... }:
       
     
