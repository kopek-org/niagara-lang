type activation = ACT
type value = VAL

type _ expr =
  | EVar of Variable.t
  | EPre
  | EAlways : activation expr
  | ENot : activation expr -> activation expr
  | EAnd : activation expr * activation expr -> activation expr
  | EGe : value expr * value expr -> activation expr
  | EConst : Literal.t -> value expr
  | EAdd : value expr * value expr -> value expr
  | EMult : value expr * value expr -> value expr
  | ENeg : value expr -> value expr
  | EInv : value expr -> value expr
  | EMerge of Variable.t list

type guarded_eq = {
  eq_activation : activation expr;
  eq_expr : value expr;
}

type program = {
  val_eqs : guarded_eq Variable.Map.t;
  act_eqs : activation expr Variable.Map.t;
}




(*

{

evenement a quand y = 100

evenement b quand y = 200

operation o1 sur x vers y
 avant a 50% # y_o1_a
 apres b 10% # y_o1_b

operation o2 sur x vers y
 apres a 100% # y_o2

}

y_o1_a : c(x) & !a & !b = 0.5x
y_o1_b : c(x) & b       = 0.1x

y_o2   : c(x) & a       = x

[
disjonction exclusive d'horloges :

exdisj(c(x) & !a & !b, c(x) & b, c(x) & a) = # c(x)
  (c(x) & !a & !b) || (c(x) & !a & b) || (c(x) & a & !b) || (c(x) & a & b)
        ^                     ^                 ^                  ^
     y_o1_a                 y_o1_b            y_o2          y_o1_b + y_o2
]

y_na   : c(x) & !a & b  = y_o1_b (when !a)
y_nb   : c(x) & a & !b  = y_o2 (when !b)
y_ab   : c(x) & a & b   = y_o1_b (when a) + y_o2 (when b)

y : c(x) = merge(y_o1_a, y_na, y_nb, y_ab)
y_c = pre(y_c) + or_zero(y) # assuming primitive for ever present value
# assumed all vars have their cumulated version '_c'

[
detection de pleine redistribution :

distrib(y) -> distrib(y_o1_a) x distrib(y_na) x distrib(y_nb) x distrib(y_ab)

distrib(y_o1_a) -> 0.5x # needs default

distrib(y_na) -> distrib(y_o1_b) -> 0.1x # needs default

distrib(y_nb) -> distrib(y_o2) -> x # leveled

distrib(y_ab) -> distrib(y_o1_b) + distrib(y_o2) -> 1.1x # deficit

]

# assuming default and deficit to 'i'

i_nanb : c(x) & !a & !b = 0.5x
i_nab  : c(x) & !a & b  = 0.9x

i      : c(x) & !a      = merge(i_nanb, i_nab)

i_prov : c(x) & a & b   = 0.1x #providing value

[
calcul de limites :

lim a -> y_c = 100
      -> y (when c(x)) = 100 - y_c
      -> "merge" (y_o1_a = 100 - y_ ; y_na = 100 - y_c ; y_nb = 100 - y_c ; y_ab = 100 - y_c)

         | lim (y_o1_a = 100 - y_c) : c(x) & !a & !b
           -> 0.5x = 100 - y_c

         | lim (y_na = 100 - y_c) : c(x) & !a & b
           -> y_o1_b = 100 - y_c
           -> 0.1x = 100 - y_c

         | ...

         | lim (y_ab = 100 - y_c) : c(x) & a & b
           -> y_o1_b + y_o2 = 100 - y_c
           -> 0.1x + x = 1.1x = 100 - y_c # a present && positive factor
           -> no limit

lim b -> # similar

]

*)

(*

{

evenement a quand w = 100

evenement b quand z = 200

operation o1 sur x vers z
 avant a 50% # z_o1_a
 apres b 10% # z_o1_b

operation o2 sur y
 avant a 100% vers w # w_o2
 apres a 100% vers z # z_o2

operation o3 sur z vers w
 apres b 100% vers w # w_o3

}

z_o1_a : c(x) & !a & !b = 0.5x
z_o1_b : c(x) & b       = 0.1x

w_o2   : c(y) & !a      = x
z_o2   : c(y) & a       = x

w_o3   : c(z) & b       = z # c(z) derived

[

pour z :
exdisj(c(x) & !a & !b, c(x) & b, c(y) & a) =
  (c(x) & !a & !b) || (c(x) & b) || (c(y) & a)
         ^                 ^             ^
      z_o1_a            z_o1_b          z_o2
]

z : c(x) & !a & !b) || (c(x) & b) || (c(y) & a = merge(z_o1_z, z_o1_b, z_o2)

[
pour w :
exdisj(c(y) & !a, c(x) & !a & !b), (c(x) & b), (c(y) & a) =
   (c(y) & !a) || (c(x) & !a & !b) || (c(x) & b) || (c(y) & a)
        ^                ^                  ^            ^
      w_o2             w_o3               w_o3         w_o3
]

w_nanb : c(x) & !a & !b = w_o3 (when !a & !b)
w_b    : c(x) & b       = w_o3 (when b)
w_a    : c(y) & a       = w_o3 (when a)

w : (c(y) & !a) || (c(x) & !a & !b) || (c(x) & b) || (c(y) & a) = merge (w_o2, w_nanb, w_b, w_a)
# assumed all vars have their cumulated version '_c'

[
calcul de limites :

lim a -> w_c = 100
      -> w = 100 - w_c
      -> "merge" (w_o2 = 100 - w_c, w_nanb = 100 - w_c, w_b = 100 - w_c, w_a = 100 - w_c)

         | lim (w_o2 = 100 - w_c) : c(y) & !a
           -> x = 100 - y_c

         | lim (w_nanb = 100 - w_c) : c(x) & !a & !b
           -> w_o3 = 100 - w_c
           -> z = 100 - w_c
           -> "merge" (z_o1_z = 100 - w_c, z_o1_b = 100 - w_c, z_o2 = 100 - w_c)
              | lim (z_o1_z = 100 - w_c) : c(x) & !a & !b
                -> 0.5x = 100 - w_c
              | ...
              | lim (z_o2 = 100 - w_c) : c(y) & a
                -> x = 100 - w_c
                -> no limit

         | ...


lim b -> # similar

]

*)
