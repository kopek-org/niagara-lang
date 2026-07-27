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
       - frais_edition_distributeur_du_desert { 0.01, 0.01 }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 9999.99, 10000 }:
       
     
  2: ++ no events:
       - recette_brute_distributeur { 20000, 20000 }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 20000, 20000 }:
           10000 -> distributeur_du_desert[commission_cinema_non_commerciale]
           10000 -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 10000, 10000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 10000, 10000 }:
           10000 -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 20000, 20000 }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 10000, 10000 }:
         - distributeur_du_desert[frais_edition] { 10000, 10000 }:
         
       
     ++ after event recuperation_frais_edition_distributeur :
       - recette_brute_distributeur { 90000, 110000 }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 90000, 110000 }:
           45000 -> distributeur_du_desert[commission_cinema_non_commerciale]
           45000 -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 45000, 55000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 45000, 55000 }:
           45000 -> recette_nette_part_producteur(Salle, Video, France, Non_commercial)
         
       - distributeur_du_desert { 81000, 101000 }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 45000, 55000 }:
         - distributeur_du_desert[minimum_garanti] { 36000, 36000 }:
         
       - recette_nette_part_producteur { 45000, 45000 }:
         - recette_nette_part_producteur(Salle, Video, France, Non_commercial) { 45000, 45000 }:
           36000 -> distributeur_du_desert[minimum_garanti]
           default 9000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - les_productions_du_chameau { 9000, 9000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 9000, 9000 }:
         
       
     
  3: ++ no events:
       - recette_brute_distributeur { 25000, 135000 }:
         - recette_brute_distributeur(Salle, France, Commercial) { 25000, 25000 }:
           7500 -> distributeur_du_desert[commission_cinema]
           17500 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 17500, 72500 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 17500, 17500 }:
           17500 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 21500, 122500 }:
         - distributeur_du_desert[commission_cinema] { 7500, 7500 }:
         - distributeur_du_desert[minimum_garanti] { 14000, 50000 }:
         
       - recette_nette_part_producteur { 17500, 62500 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 17500, 17500 }:
           14000 -> distributeur_du_desert[minimum_garanti]
           3500 -> barbie[interessement]
         
       - barbie { 3500, 3500 }:
         - barbie[interessement] { 3500, 3500 }:
         
       
     ++ after event recuperation_minimum_garanti :
       - recette_brute_distributeur { 42857.14285..., 177857.14285... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 42857.14285..., 67857.14285... }:
           12857.14285... -> distributeur_du_desert[commission_cinema]
           30000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 30000, 102500 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 30000, 47500 }:
           30000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 12857.14285..., 135357.14285... }:
         - distributeur_du_desert[commission_cinema] { 12857.14285..., 20357.14285... }:
         
       - recette_nette_part_producteur { 30000, 92500 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 30000, 47500 }:
           6000 -> barbie[interessement]
           3000 -> dromadaire_film[france]
           default 21000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 3000, 3000 }:
         - dromadaire_film[france] { 3000, 3000 }:
         
       - les_productions_du_chameau { 21000, 30000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 21000, 30000 }:
         
       - barbie { 6000, 9500 }:
         - barbie[interessement] { 6000, 9500 }:
         
       
     ++ after event recup_risque_prod :
       - recette_brute_distributeur { 0, 177857.14285... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 0, 67857.14285... }:
           0 -> distributeur_du_desert[commission_cinema]
           0 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 0, 102500 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 0, 47500 }:
           0 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 0, 135357.14285... }:
         - distributeur_du_desert[commission_cinema] { 0, 20357.14285... }:
         
       - recette_nette_part_producteur { 0, 92500 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 0, 47500 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[france]
           default 0 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 0, 3000 }:
         - dromadaire_film[france] { 0, 3000 }:
         
       - les_productions_du_chameau { 0, 30000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 0, 30000 }:
         
       - barbie { 0, 9500 }:
         - barbie[interessement] { 0, 9500 }:
         
       
     
  4: ++ no events:
       - recette_brute_distributeur { 1000000, 1177857.14285... }:
         - recette_brute_distributeur(Salle, France, Commercial) { 1000000, 1067857.14285... }:
           300000 -> distributeur_du_desert[commission_cinema]
           700000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 700000, 802500 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 700000, 747500 }:
           700000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 300000, 435357.14285... }:
         - distributeur_du_desert[commission_cinema] { 300000, 320357.14285... }:
         
       - recette_nette_part_producteur { 700000, 792500 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 700000, 747500 }:
           140000 -> barbie[interessement]
           70000 -> dromadaire_film[france]
           default 490000 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 70000, 73000 }:
         - dromadaire_film[france] { 70000, 73000 }:
         
       - les_productions_du_chameau { 490000, 520000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 490000, 520000 }:
         
       - barbie { 140000, 149500 }:
         - barbie[interessement] { 140000, 149500 }:
         
       
     
  5: ++ no events:
       - entree_salle_France { 100000, 100000 }:
       
     ++ after event seuil_100000_entrees :
       - entree_salle_France { 0, 100000 }:
       - flat bonus : 10000 -> barbie[bonus_nombre_entrees]
       - barbie { 10000, 159500 }:
         - barbie[bonus_nombre_entrees] { 10000, 10000 }:
         
       
     
  6: ++ no events:
       - vente_tvsvod { 0, 0 }:
         - vente_tvsvod(TV, SVOD, France, Commercial) { 0, 0 }:
           0 -> les_productions_du_chameau[commission]
           0 -> recette_nette_part_producteur(TV, SVOD, France, Commercial)
         
       - recette_nette_part_producteur { 0, 792500 }:
         - recette_nette_part_producteur(TV, SVOD, France, Commercial) { 0, 0 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[tv_svod]
           default 0 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 0, 73000 }:
         - dromadaire_film[tv_svod] { 0, 0 }:
         
       - les_productions_du_chameau { 0, 520000 }:
         - les_productions_du_chameau[commission] { 0, 0 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 0, 520000 }:
         
       - barbie { 0, 159500 }:
         - barbie[interessement] { 0, 149500 }:
         
       
     
  7: ++ no events:
       - frais_edition_vendeur_scorpion { 0.01, 0.01 }:
       
     ++ before event recuperation_frais_edition_scorpion :
       - frais_edition_vendeur_scorpion { 0, 0.01 }:
       
     
  8: ++ no events:
       - recette_brute_vendeur { 0.01333..., 0.01333... }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0.01333..., 0.01333... }:
           0.00333... -> vendeur_scorpion[commission_vendeur]
           0.01 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 0.01, 0.01 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0.01, 0.01 }:
           0.01 -> vendeur_scorpion[frais_edition_vendeur_scorpion]
         
       - vendeur_scorpion { 0.01333..., 0.01333... }:
         - vendeur_scorpion[commission_vendeur] { 0.00333..., 0.00333... }:
         - vendeur_scorpion[frais_edition_vendeur_scorpion] { 0.01, 0.01 }:
         
       
     ++ after event recuperation_frais_edition_scorpion :
       - recette_brute_vendeur { 0, 0.01333... }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 0.01333... }:
           0 -> vendeur_scorpion[commission_vendeur]
           0 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 0, 0.01 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 0.01 }:
           0 -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - vendeur_scorpion { 0, 0.01333... }:
         - vendeur_scorpion[commission_vendeur] { 0, 0.00333... }:
         
       - recette_nette_part_producteur { 0, 792500 }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 0, 0 }:
           0 -> barbie[interessement]
           0 -> dromadaire_film[etranger]
           default 0 -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - dromadaire_film { 0, 73000 }:
         - dromadaire_film[etranger] { 0, 0 }:
         
       - les_productions_du_chameau { 0, 520000 }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 0, 0 }:
         
       - barbie { 0, 159500 }:
         - barbie[interessement] { 0, 149500 }:
         
       
     
  9: ++ no events:
       - frais_edition_distributeur_du_desert { 0.01, 10000.01 }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 0, 10000.01 }:
       
     
