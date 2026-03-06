  $ OCAMLRUNPARAM=b niagara --test ../examples/sur_un_nuage.nga <<EOF
  > 1: frais_edition_distributeur_du_desert += 10000$
  > 2: recette_brute_distributeur(Salle, France, Non_commercial) += 110000$
  > 3: recette_brute_distributeur(Salle, France, Commercial) += stabilize
  > 4: recette_brute_distributeur(Salle, France, Commercial) += 1000000$
  > 5: entree_salle_France += stabilize
  > 6: vente_tvsvod(TV, SVOD, France, Commercial) += stabilize
  > 7: frais_edition_vendeur_scorpion += stabilize
  > 8: recette_brute_vendeur(tout Support, Etranger, Commercial) += stabilize
  > 9: frais_edition_distributeur_du_desert += stabilize
  > EOF
  Awaiting inputs:
  ### OUTPUTS ###
  0: ++ after event recuperation_frais_edition_distributeur 
        after event recuperation_frais_edition_scorpion :
       
     
  1: ++ no events:
       - frais_edition_distributeur_du_desert { 1, 1 }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 999999, 1000000 }:
       
     
  2: ++ no events:
       - recette_brute_distributeur { 2000000, 2000000 }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 2000000, 2000000 }:
           1000000 -> distributeur_du_desert[commission_cinema_non_commerciale]
           1000000 -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 1000000, 1000000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 1000000, 1000000 }:
           1000000 -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 2000000, 2000000 }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 1000000, 1000000 }:
         - distributeur_du_desert[frais_edition] { 1000000, 1000000 }:
         
       
     ++ after event recuperation_frais_edition_distributeur :
       - recette_brute_distributeur { 9000000, 11000000 }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 9000000, 11000000 }:
           4500000 -> distributeur_du_desert[commission_cinema_non_commerciale]
           4500000 -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 4500000, 5500000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 4500000, 5500000 }:
           4500000 -> recette_nette_part_producteur(Salle, Video, France, Non_commercial)
         
       - distributeur_du_desert { 8100000, 10100000 }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 4500000, 5500000 }:
         - distributeur_du_desert[minimum_garanti] { 3600000, 3600000 }:
         
       - recette_nette_part_producteur { 4500000, 4500000 }:
         - recette_nette_part_producteur(Salle, Video, France, Non_commercial) { 4500000, 4500000 }:
           3600000 -> distributeur_du_desert[minimum_garanti]
           default 900000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - les_productions_du_chameau { 900000, 900000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 900000, 900000 }:
         
       
     
  3: ++ no events:
       - recette_brute_distributeur { 2500000, 13500000 }:
         - recette_brute_distributeur(Salle, France, Commercial) { 2500000, 2500000 }:
           750000 -> distributeur_du_desert[commission_cinema]
           1750000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 1750000, 7250000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 1750000, 1750000 }:
           1750000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 2150000, 12250000 }:
         - distributeur_du_desert[commission_cinema] { 750000, 750000 }:
         - distributeur_du_desert[minimum_garanti] { 1400000, 5000000 }:
         
       - recette_nette_part_producteur { 1750000, 6250000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 1750000, 1750000 }:
           1400000 -> distributeur_du_desert[minimum_garanti]
           350000 -> barbie[interessement]
         
       - barbie { 350000, 350000 }:
         - barbie[interessement] { 350000, 350000 }:
         
       
     ++ after event recuperation_minimum_garanti :
       - recette_brute_distributeur { 4285714.28571..., 17785714.28571... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 4285714.28571..., 6785714.28571... }:
           1285714.28571... -> distributeur_du_desert[commission_cinema]
           3000000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 3000000, 10250000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 3000000, 4750000 }:
           3000000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 1285714.28571..., 13535714.28571... }:
         - distributeur_du_desert[commission_cinema] { 1285714.28571..., 2035714.28571... }:
         
       - recette_nette_part_producteur { 3000000, 9250000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 3000000, 4750000 }:
           600000 -> barbie[interessement]
           300000 -> dromadaire_film[france]
           default 2100000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 300000, 300000 }:
         - dromadaire_film[france] { 300000, 300000 }:
         
       - les_productions_du_chameau { 2100000, 3000000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 2100000, 3000000 }:
         
       - barbie { 600000, 950000 }:
         - barbie[interessement] { 600000, 950000 }:
         
       
     ++ after event recup_risque_prod :
       - recette_brute_distributeur { 0, 17785714.28571... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 0, 6785714.28571... }:
           0 -> distributeur_du_desert[commission_cinema]
           0 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 0, 10250000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 0, 4750000 }:
           0 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 0, 13535714.28571... }:
         - distributeur_du_desert[commission_cinema] { 0, 2035714.28571... }:
         
       - recette_nette_part_producteur { 0, 9250000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 0, 4750000 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[france]
           default 0 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 0, 300000 }:
         - dromadaire_film[france] { 0, 300000 }:
         
       - les_productions_du_chameau { 0, 3000000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 0, 3000000 }:
         
       - barbie { 0, 950000 }:
         - barbie[interessement] { 0, 950000 }:
         
       
     
  4: ++ no events:
       - recette_brute_distributeur { 100000000, 117785714.28571... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 100000000, 106785714.28571... }:
           30000000 -> distributeur_du_desert[commission_cinema]
           70000000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 70000000, 80250000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 70000000, 74750000 }:
           70000000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 30000000, 43535714.28571... }:
         - distributeur_du_desert[commission_cinema] { 30000000, 32035714.28571... }:
         
       - recette_nette_part_producteur { 70000000, 79250000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 70000000, 74750000 }:
           14000000 -> barbie[interessement]
           7000000 -> dromadaire_film[france]
           default 49000000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 7000000, 7300000 }:
         - dromadaire_film[france] { 7000000, 7300000 }:
         
       - les_productions_du_chameau { 49000000, 52000000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 49000000, 52000000 }:
         
       - barbie { 14000000, 14950000 }:
         - barbie[interessement] { 14000000, 14950000 }:
         
       
     
  5: ++ no events:
       - entree_salle_France { 100000, 100000 }:
       
     ++ after event seuil_100000_entrees :
       - entree_salle_France { 0, 100000 }:
       - bonus sur le nombre d entrees : 1000000 -> barbie[bonus_nombre_entrees]
       - barbie { 1000000, 15950000 }:
         - barbie[bonus_nombre_entrees] { 1000000, 1000000 }:
         
       
     
  6: ++ no events:
       - vente_tvsvod { 0, 0 }:
         - vente_tvsvod(TV, SVOD, France, Commercial) { 0, 0 }:
           0 -> les_productions_du_chameau[commission]
           0 -> recette_nette_part_producteur(TV, SVOD, France, Commercial)
         
       - recette_nette_part_producteur { 0, 79250000 }:
         - recette_nette_part_producteur(TV, SVOD, France, Commercial) { 0, 0 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[tv_svod]
           default 0 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 0, 7300000 }:
         - dromadaire_film[tv_svod] { 0, 0 }:
         
       - les_productions_du_chameau { 0, 52000000 }:
         - les_productions_du_chameau[commission] { 0, 0 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 0, 52000000 }:
         
       - barbie { 0, 15950000 }:
         - barbie[interessement] { 0, 14950000 }:
         
       
     
  7: ++ no events:
       - frais_edition_vendeur_scorpion { 1, 1 }:
       
     ++ before event recuperation_frais_edition_scorpion :
       - frais_edition_vendeur_scorpion { 0, 1 }:
       
     
  8: ++ no events:
       - recette_brute_vendeur { 1.33333..., 1.33333... }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 1.33333..., 1.33333... }:
           0.33333... -> vendeur_scorpion[commission_vendeur]
           1 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 1, 1 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 1, 1 }:
           1 -> vendeur_scorpion[frais_edition_vendeur_scorpion]
         
       - vendeur_scorpion { 1.33333..., 1.33333... }:
         - vendeur_scorpion[commission_vendeur] { 0.33333..., 0.33333... }:
         - vendeur_scorpion[frais_edition_vendeur_scorpion] { 1, 1 }:
         
       
     ++ after event recuperation_frais_edition_scorpion :
       - recette_brute_vendeur { 0, 1.33333... }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 1.33333... }:
           0 -> vendeur_scorpion[commission_vendeur]
           0 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 0, 1 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 1 }:
           0 -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - vendeur_scorpion { 0, 1.33333... }:
         - vendeur_scorpion[commission_vendeur] { 0, 0.33333... }:
         
       - recette_nette_part_producteur { 0, 79250000 }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 0 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[etranger]
           default 0 -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - dromadaire_film { 0, 7300000 }:
         - dromadaire_film[etranger] { 0, 0 }:
         
       - les_productions_du_chameau { 0, 52000000 }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 0, 0 }:
         
       - barbie { 0, 15950000 }:
         - barbie[interessement] { 0, 14950000 }:
         
       
     
  9: ++ no events:
       - frais_edition_distributeur_du_desert { 1, 1000001 }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 0, 1000001 }:
       
     
