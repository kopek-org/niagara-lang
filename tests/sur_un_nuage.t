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
  1: ++ after event recuperation_frais_edition_distributeur 
        after event recuperation_frais_edition_scorpion :
       - frais_edition_distributeur_du_desert { 1., 1. }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 999999., 1000000. }:
       
     
  2: ++ no events:
       - recette_brute_distributeur { 600000., 600000. }:
         - recette_brute_distributeur(Salle, France, Non_commercial) { 600000., 600000. }:
           300000. -> distributeur_du_desert[commission_cinema_non_commerciale]
           300000. -> recette_nette_commission_distributeur(Salle, Video, France, Non_commercial)
         
       - recette_nette_commission_distributeur { 300000., 300000. }:
         - recette_nette_commission_distributeur(Salle, Video, France, Non_commercial) { 300000., 300000. }:
           300000. -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 600000., 600000. }:
         - distributeur_du_desert[commission_cinema_non_commerciale] { 300000., 300000. }:
         - distributeur_du_desert[frais_edition] { 300000., 300000. }:
         
       
     
  3: ++ no events:
       - recette_brute_distributeur { 1000000., 1600000. }:
         - recette_brute_distributeur(Salle, France, Commercial) { 1000000., 1000000. }:
           300000. -> distributeur_du_desert[commission_cinema]
           700000. -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 700000., 1000000. }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 700000., 700000. }:
           700000. -> distributeur_du_desert[frais_edition]
         
       - distributeur_du_desert { 1000000., 1600000. }:
         - distributeur_du_desert[commission_cinema] { 300000., 300000. }:
         - distributeur_du_desert[frais_edition] { 700000., 1000000. }:
         
       
     ++ after event recuperation_frais_edition_distributeur :
       - recette_brute_distributeur { 1000000., 2600000. }:
         - recette_brute_distributeur(Salle, France, Commercial) { 1000000., 2000000. }:
           300000. -> distributeur_du_desert[commission_cinema]
           700000. -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 700000., 1700000. }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 700000., 1400000. }:
           700000. -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - recette_nette_part_producteur { 700000., 700000. }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 700000., 700000. }:
           560000. -> distributeur_du_desert[minimum_garanti]
           140000. -> barbie[interessement]
         
       - distributeur_du_desert { 860000., 2460000. }:
         - distributeur_du_desert[commission_cinema] { 300000., 600000. }:
         - distributeur_du_desert[minimum_garanti] { 560000., 560000. }:
         
       - barbie { 140000., 140000. }:
         - barbie[interessement] { 140000., 140000. }:
         
       
     
  4: ++ no events:
       - recette_brute_distributeur { 7400000., 10000000. }:
         - recette_brute_distributeur(Video, France, Commercial) { 7400000., 7400000. }:
           1850000. -> distributeur_du_desert[commission_video]
           5550000. -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 5550000., 7250000. }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 5550000., 6950000. }:
           5550000. -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - recette_nette_part_producteur { 5550000., 6250000. }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 5550000., 6250000. }:
           4440000. -> distributeur_du_desert[minimum_garanti]
           1110000. -> barbie[interessement]
         
       - distributeur_du_desert { 6290000., 8750000. }:
         - distributeur_du_desert[commission_video] { 1850000., 1850000. }:
         - distributeur_du_desert[minimum_garanti] { 4440000., 5000000. }:
         
       - barbie { 1110000., 1250000. }:
         - barbie[interessement] { 1110000., 1250000. }:
         
       
     ++ after event recuperation_minimum_garanti :
       - recette_brute_distributeur { 600000., 10600000. }:
         - recette_brute_distributeur(Video, France, Commercial) { 600000., 8000000. }:
           150000. -> distributeur_du_desert[commission_video]
           450000. -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
         
       - recette_nette_commission_distributeur { 450000., 7700000. }:
         - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 450000., 7400000. }:
           450000. -> recette_nette_part_producteur(Salle, Video, France, Commercial)
         
       - recette_nette_part_producteur { 450000., 6700000. }:
         - recette_nette_part_producteur(Salle, Video, France, Commercial) { 450000., 6700000. }:
           90000. -> barbie[interessement]
           45000. -> dromadaire_film[france]
           default 315000. -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - les_productions_du_chameau { 315000., 315000. }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 315000., 315000. }:
         
       - distributeur_du_desert { 150000., 8900000. }:
         - distributeur_du_desert[commission_video] { 150000., 2000000. }:
         
       - dromadaire_film { 45000., 45000. }:
         - dromadaire_film[france] { 45000., 45000. }:
         
       - barbie { 90000., 1340000. }:
         - barbie[interessement] { 90000., 1340000. }:
         
       
     
  5: ++ no events:
       - entree_salle_France { 100000., 100000. }:
       
     ++ after event seuil_100000_entrees :
       - les_productions_du_chameau (as provider) { 1000000., 1000000. }:
         1000000. -> barbie[bonus_nombre_entrees]
       - barbie { 1000000., 2340000. }:
         - barbie[bonus_nombre_entrees] { 1000000., 1000000. }:
         
       - entree_salle_France { 50000., 150000. }:
       
     
  6: ++ no events:
       - vente_tvsvod { 1000000., 1000000. }:
         - vente_tvsvod(TV, SVOD, France, Commercial) { 1000000., 1000000. }:
           200000. -> les_productions_du_chameau[commission]
           800000. -> recette_nette_part_producteur(TV, SVOD, France, Commercial)
         
       - recette_nette_part_producteur { 800000., 7500000. }:
         - recette_nette_part_producteur(TV, SVOD, France, Commercial) { 800000., 800000. }:
           160000. -> barbie[interessement]
           320000. -> dromadaire_film[tv_svod]
           default 320000. -> les_productions_du_chameau[rnpp_france_residuelle]
         
       - les_productions_du_chameau { 520000., 835000. }:
         - les_productions_du_chameau[commission] { 200000., 200000. }:
         - les_productions_du_chameau[rnpp_france_residuelle] { 320000., 635000. }:
         
       - dromadaire_film { 320000., 365000. }:
         - dromadaire_film[tv_svod] { 320000., 320000. }:
         
       - barbie { 160000., 2500000. }:
         - barbie[interessement] { 160000., 1500000. }:
         
       
     
  7: ++ no events:
       - frais_edition_vendeur_scorpion { 1., 1. }:
       
     ++ before event recuperation_frais_edition_scorpion :
       - frais_edition_vendeur_scorpion { 1199999., 1200000. }:
       
     
  8: ++ no events:
       - recette_brute_vendeur { 1600000., 1600000. }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 1600000., 1600000. }:
           400000. -> vendeur_scorpion[commission_vendeur]
           1200000. -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 1200000., 1200000. }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 1200000., 1200000. }:
           1200000. -> vendeur_scorpion[frais_edition_vendeur_scorpion]
         
       - vendeur_scorpion { 1600000., 1600000. }:
         - vendeur_scorpion[commission_vendeur] { 400000., 400000. }:
         - vendeur_scorpion[frais_edition_vendeur_scorpion] { 1200000., 1200000. }:
         
       
     ++ after event recuperation_frais_edition_scorpion :
       - recette_brute_vendeur { 3183823.(5294117647058823), 4783823.(5294117647058823) }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 3183823.(5294117647058823), 4783823.(5294117647058823) }:
           477573.(5294117647058823) -> vendeur_scorpion[commission_vendeur]
           2706250. -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 2706250., 3906250. }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 2706250., 3906250. }:
           2706250. -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_part_producteur { 2706250., 10206250. }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 2706250., 2706250. }:
           541250. -> barbie[interessement]
           default 2165000. -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - les_productions_du_chameau { 2165000., 3000000. }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 2165000., 2165000. }:
         
       - vendeur_scorpion { 477573.(5294117647058823), 2077573.(5294117647058823) }:
         - vendeur_scorpion[commission_vendeur] { 477573.(5294117647058823), 877573.(5294117647058823) }:
         
       - barbie { 541250., 3041250. }:
         - barbie[interessement] { 541250., 2041250. }:
         
       
     ++ after event recup_risque_prod :
       - recette_brute_vendeur { 1000000.(4705882352941176), 5783824. }:
         - recette_brute_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 1000000.(4705882352941176), 5783824. }:
           150000.0(7058823529411764) -> vendeur_scorpion[commission_vendeur]
           850000.4 -> recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_commission_vendeur { 850000.4, 4756250.4 }:
         - recette_nette_commission_vendeur(Salle, TV, Video, SVOD, Etranger, Commercial) { 850000.4, 4756250.4 }:
           850000.4 -> recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial)
         
       - recette_nette_part_producteur { 850000.4, 11056250.4 }:
         - recette_nette_part_producteur(Salle, TV, Video, SVOD, Etranger, Commercial) { 850000.4, 3556250.4 }:
           170000.08 -> barbie[interessement]
           170000.08 -> dromadaire_film[etranger]
           default 510000.24 -> les_productions_du_chameau[rnpp_residuelle_etranger]
         
       - les_productions_du_chameau { 510000.24, 3510000.24 }:
         - les_productions_du_chameau[rnpp_residuelle_etranger] { 510000.24, 2675000.24 }:
         
       - vendeur_scorpion { 150000.0(7058823529411764), 2227573.6 }:
         - vendeur_scorpion[commission_vendeur] { 150000.0(7058823529411764), 1027573.6 }:
         
       - dromadaire_film { 170000.08, 535000.08 }:
         - dromadaire_film[etranger] { 170000.08, 170000.08 }:
         
       - barbie { 170000.08, 3211250.08 }:
         - barbie[interessement] { 170000.08, 2211250.08 }:
         
       
     
  9: ++ no events:
       - frais_edition_distributeur_du_desert { 1., 1000001. }:
       
     ++ before event recuperation_frais_edition_distributeur :
       - frais_edition_distributeur_du_desert { 999999., 2000000. }:
       
     
  10: ++ no events:
        - recette_brute_distributeur { 1000000., 11600000. }:
          - recette_brute_distributeur(Salle, France, Commercial) { 1000000., 3000000. }:
            200000. -> distributeur_du_desert
            800000. -> recette_nette_commission_distributeur(Salle, Video, France, Commercial)
          
        - recette_nette_commission_distributeur { 800000., 8500000. }:
          - recette_nette_commission_distributeur(Salle, Video, France, Commercial) { 800000., 8200000. }:
            800000. -> distributeur_du_desert[frais_edition]
          
        - distributeur_du_desert { 1000000., 9900000. }:
          - distributeur_du_desert[frais_edition] { 800000., 1800000. }:
          
        
      
