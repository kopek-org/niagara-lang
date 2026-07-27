  $ OCAMLRUNPARAM=b niagara --test ../examples/sur_un_nuage.nga <<EOF
  > 1: frais_edition_distributeur_du_desert += 10000$
  > 2: recette_brute_distributeur(Salle, France, Non_commercial) += 6000$
  > 3: recette_brute_distributeur(Salle, France, Commercial) += 20000$
  > 4: recette_brute_distributeur(Video, France, Commercial) += 80000$
  > 5: entree_salle_France += 150000
  > 6: vente_tvsvod(TV, SVOD, France, Commercial) += 10000$
  > 7: frais_edition_vendeur_scorpion += 12000$
  > 8: recette_brute_vendeur(tout Support, Etranger, Commercial) += 57838.24$
  > 9: frais_edition_distributeur_du_desert += 10000$
  > 10: recette_brute_distributeur(Salle, France, Commercial) += 10000$
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
       - recette_brute_distributeur { 6000, 6000 }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 6000, 6000 }:
           3000 -> distributeur_du_desert[commission_cinema_non_commerciale]
           3000 -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 3000, 3000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 3000, 3000 }:
           3000 -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 6000, 6000 }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 3000, 3000 }:
         - distributeur_du_desert[frais_edition] { 3000, 3000 }:
         
       
     
  3: ++ no events:
       - recette_brute_distributeur { 10000, 16000 }:
         - recette_brute_distributeur(Salle, France, Commercial) { 10000, 10000 }:
           3000 -> distributeur_du_desert[commission_cinema]
           7000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 7000, 10000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 7000, 7000 }:
           7000 -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 10000, 16000 }:
         - distributeur_du_desert[commission_cinema] { 3000, 3000 }:
         - distributeur_du_desert[frais_edition] { 7000, 10000 }:
         
       
     ++ after event recuperation_frais_edition_distributeur :
       - recette_brute_distributeur { 10000, 26000 }:
         - recette_brute_distributeur(Salle, France, Commercial) { 10000, 20000 }:
           3000 -> distributeur_du_desert[commission_cinema]
           7000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 7000, 17000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 7000, 14000 }:
           7000 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 8600, 24600 }:
         - distributeur_du_desert[commission_cinema] { 3000, 6000 }:
         - distributeur_du_desert[minimum_garanti] { 5600, 5600 }:
         
       - recette_nette_part_producteur { 7000, 7000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 7000, 7000 }:
           5600 -> distributeur_du_desert[minimum_garanti]
           1400 -> barbie[interessement]
         
       - barbie { 1400, 1400 }:
         - barbie[interessement] { 1400, 1400 }:
         
       
     
  4: ++ no events:
       - recette_brute_distributeur { 74000, 100000 }:
         - recette_brute_distributeur(Video, France, Commercial) { 74000, 74000 }:
           18500 -> distributeur_du_desert[commission_video]
           55500 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 55500, 72500 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 55500, 69500 }:
           55500 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 62900, 87500 }:
         - distributeur_du_desert[commission_video] { 18500, 18500 }:
         - distributeur_du_desert[minimum_garanti] { 44400, 50000 }:
         
       - recette_nette_part_producteur { 55500, 62500 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 55500, 62500 }:
           44400 -> distributeur_du_desert[minimum_garanti]
           11100 -> barbie[interessement]
         
       - barbie { 11100, 12500 }:
         - barbie[interessement] { 11100, 12500 }:
         
       
     ++ after event recuperation_minimum_garanti :
       - recette_brute_distributeur { 6000, 106000 }:
         - recette_brute_distributeur(Video, France, Commercial) { 6000, 80000 }:
           1500 -> distributeur_du_desert[commission_video]
           4500 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 4500, 77000 }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 4500, 74000 }:
           4500 -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - distributeur_du_desert { 1500, 89000 }:
         - distributeur_du_desert[commission_video] { 1500, 20000 }:
         
       - recette_nette_part_producteur { 4500, 67000 }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 4500, 67000 }:
           900 -> barbie[interessement]
           450 -> dromadaire_film[france]
           default 3150 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 450, 450 }:
         - dromadaire_film[france] { 450, 450 }:
         
       - les_productions_du_chameau { 3150, 3150 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 3150, 3150 }:
         
       - barbie { 900, 13400 }:
         - barbie[interessement] { 900, 13400 }:
         
       
     
  5: ++ no events:
       - entree_salle_France { 100000, 100000 }:
       
     ++ after event seuil_100000_entrees :
       - entree_salle_France { 50000, 150000 }:
       - flat bonus : 10000 -> barbie[bonus_nombre_entrees]
       - barbie { 10000, 23400 }:
         - barbie[bonus_nombre_entrees] { 10000, 10000 }:
         
       
     
  6: ++ no events:
       - vente_tvsvod { 10000, 10000 }:
         - vente_tvsvod(TV, SVOD, France, Commercial) { 10000, 10000 }:
           2000 -> les_productions_du_chameau[commission]
           8000 -> recette_nette_part_producteur(TV, SVOD, France, Commercial)
         
       - recette_nette_part_producteur { 8000, 75000 }:
         - recette_nette_part_producteur(TV, SVOD, France, Commercial) { 8000, 8000 }:
           1600 -> barbie[interessement]
           3200 -> dromadaire_film[tv_svod]
           default 3200 -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - dromadaire_film { 3200, 3650 }:
         - dromadaire_film[tv_svod] { 3200, 3200 }:
         
       - les_productions_du_chameau { 5200, 8350 }:
         - les_productions_du_chameau[commission] { 2000, 2000 }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 3200, 6350 }:
         
       - barbie { 1600, 25000 }:
         - barbie[interessement] { 1600, 15000 }:
         
       
     
  7: ++ no events:
       - frais_edition_vendeur_scorpion { 0.01, 0.01 }:
       
     ++ before event recuperation_frais_edition_scorpion :
       - frais_edition_vendeur_scorpion { 11999.99, 12000 }:
       
     
  8: ++ no events:
       - recette_brute_vendeur { 16000, 16000 }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 16000, 16000 }:
           4000 -> vendeur_scorpion[commission_vendeur]
           12000 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 12000, 12000 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 12000, 12000 }:
           12000 -> vendeur_scorpion[frais_edition_vendeur_scorpion]
         
       - vendeur_scorpion { 16000, 16000 }:
         - vendeur_scorpion[commission_vendeur] { 4000, 4000 }:
         - vendeur_scorpion[frais_edition_vendeur_scorpion] { 12000, 12000 }:
         
       
     ++ after event recuperation_frais_edition_scorpion :
       - recette_brute_vendeur { 31838.23529..., 47838.23529... }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 31838.23529..., 47838.23529... }:
           4775.73529... -> vendeur_scorpion[commission_vendeur]
           27062.5 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 27062.5, 39062.5 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 27062.5, 39062.5 }:
           27062.5 -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - vendeur_scorpion { 4775.73529..., 20775.73529... }:
         - vendeur_scorpion[commission_vendeur] { 4775.73529..., 8775.73529... }:
         
       - recette_nette_part_producteur { 27062.5, 102062.5 }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 27062.5, 27062.5 }:
           5412.5 -> barbie[interessement]
           default 21650 -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - les_productions_du_chameau { 21650, 30000 }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 21650, 21650 }:
         
       - barbie { 5412.5, 30412.5 }:
         - barbie[interessement] { 5412.5, 20412.5 }:
         
       
     ++ after event recup_risque_prod :
       - recette_brute_vendeur { 10000.00470..., 57838.24 }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 10000.00470..., 57838.24 }:
           1500.00070... -> vendeur_scorpion[commission_vendeur]
           8500.004 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 8500.004, 47562.504 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 8500.004, 47562.504 }:
           8500.004 -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - vendeur_scorpion { 1500.00070..., 22275.736 }:
         - vendeur_scorpion[commission_vendeur] { 1500.00070..., 10275.736 }:
         
       - recette_nette_part_producteur { 8500.004, 110562.504 }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 8500.004, 35562.504 }:
           1700.0008 -> barbie[interessement]
           1700.0008 -> dromadaire_film[etranger]
           default 5100.0024 -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - dromadaire_film { 1700.0008, 5350.0008 }:
         - dromadaire_film[etranger] { 1700.0008, 1700.0008 }:
         
       - les_productions_du_chameau { 5100.0024, 35100.0024 }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 5100.0024, 26750.0024 }:
         
       - barbie { 1700.0008, 32112.5008 }:
         - barbie[interessement] { 1700.0008, 22112.5008 }:
         
       
     
  9: ++ no events:
       - frais_edition_distributeur_du_desert { 0.01, 10000.01 }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 9999.99, 20000 }:
       
     
  10: ++ no events:
        - recette_brute_distributeur { 10000, 116000 }:
          - recette_brute_distributeur(Salle, France, Commercial) { 10000, 30000 }:
            2000 -> distributeur_du_desert
            8000 -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
          
        - recette_nette_commission_distributeur { 8000, 85000 }:
          - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 8000, 82000 }:
            8000 -> distributeur_du_desert[frais_edition]
          
        - distributeur_du_desert { 10000, 99000 }:
          - distributeur_du_desert[frais_edition] { 8000, 18000 }:
          
        
      
