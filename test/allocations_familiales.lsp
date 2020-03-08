@@Code de la sécurité sociale@@

@Article L511-1@
Les prestations familiales comprennent :
1°) la prestation d'accueil du jeune enfant ;
2°) les allocations familiales ;
3°) le complément familial ;
4°) L'allocation de logement régie par les dispositions du livre VIII du code de la construction et de l'habitation ;
5°) l'allocation d'éducation de l'enfant handicapé ;
6°) l'allocation de soutien familial ;
7°) l'allocation de rentrée scolaire ;
8°) (Abrogé) ;
9°) l'allocation journalière de présence parentale.
/*
choix prestation:
  -- PrestationAccueilJeuneEnfant
  -- AllocationsFamiliales
  -- ComplementFamilial
  -- AllocationLogement
  -- AllocationEducationEnfantHandicape
  -- AllocationSoutienFamilial
  -- AllocationRentreeScolaire
  -- AllocationJournalierePresenceParentale.

situation ContextePrestationsFamiliales source loi :
  donnee prestation_courante de choix prestation.
*/

@Article L512-3@
Sous réserve des règles particulières à chaque prestation, ouvre droit aux prestations familiales :
/*
situation ContextePrestationsFamiliales source loi :
  donnee droits_ouverts.
*/
1°) tout enfant jusqu'à la fin de l'obligation scolaire ;
/*
situation EnfantPrestationsFamiliales source loi :
  donnee fin_obligation_scolaire de type entier.

situation ContextePrestationsFamiliales source loi :
  donnee enfants collection de situation EnfantPrestationsFamiliales ;
  regle condition
    existe enfant dans enfants tel que
      maintenant < enfant!fin_obligation_scolaire
  consequence droits_ouverts defini.
*/
2°) après la fin de l'obligation scolaire, et jusqu'à un âge limite, tout enfant dont la rémunération éventuelle n'excède pas un plafond.
/*
situation ContextePrestationsFamiliales source loi :
  donnee age_limite_L512_3_2 de type entier ;
  donnee plafond_remuneration_L512_3_2 de type montant.

situation EnfantPrestationsFamiliales source loi :
  donnee age de type entier ;
  donnee remuneration de type montant ;
  donnee qualifie_pour_prestation_sauf_age ;
  regle condition
    maintenant > fin_obligation_scolaire et
    remuneration < plafond_remuneration_L512_3_2
  consequence qualifie_pour_prestation_sauf_age defini ;
  donnee enfant_qualifie_pour_prestation ;
  regle condition
    qualifie_pour_prestation_sauf_age et
    age < age_limite_L512_3_2
  consequence qualifie_pour_prestation defini.

situation ContextePrestationsFamiliales source loi :
  regle condition
    existe enfant dans enfants tel que
      enfant!qualifie_pour_prestation
  consequence droits_ouverts defini.
*/
Toutefois, pour l'attribution du complément familial et de l'allocation de logement mentionnés aux 3° et 4° de l'article L. 511-1, l'âge limite peut être différent de celui mentionné au 2° du présent article.
/*
situation ContextePrestationsFamiliales source loi :
  donnee age_limite_L512_3_2_alternatif de type entier ;
  regle optionnel condition
    prestation_courante = ComplementFamilial ou
    prestation_courante = AllocationLogement
  consequence
    age_limite_L512_3_2 defini comme age_limite_L512_3_2_alternatif.
*/

@Article L521-1@ Les allocations familiales sont dues à partir du deuxième enfant à charge.
/*
situation AllocationsFamiliales source loi:
  donnee contexte de situation ContextePrestationsFamiliales ;
  regle contexte!prestation_courante
     defini comme AllocationsFamiliales ;
  donnee allocations_familiales_dues ;
  donnee nombre_enfants_a_charge de type entier ;
  regle nombre_enfants_a_charge defini comme cardinal(contexte!enfants) ;
  regle condition
    nombre_enfants_a_charge >= 2
  consequence allocations_familiales_dues defini.
*/
Une allocation forfaitaire par enfant d'un montant fixé par décret est versée pendant un an à la personne ou au ménage qui assume la charge d'un nombre minimum d'enfants également fixé par décret lorsque l'un ou plusieurs des enfants qui ouvraient droit aux allocations familiales atteignent l'âge limite mentionné au 2° de l'article L. 512-3. Cette allocation est versée à la condition que le ou les enfants répondent aux conditions autres que celles de l'âge pour l'ouverture du droit aux allocations familiales.
/*
situation AllocationFamiliales source loi :
  donnee allocation_forfaitaire_L521_1 de type montant ;
  assertion allocation_forfaitaire_L521_1 fixe par decret ;
  donnee nombre_minimum_enfants_L521_1 de type entier ;
  assertion nombre_minimum_enfants_L521_1 fixe par decret.

choix entite_en_charge :
  -- FamilleMonoparentale de situation Personne
  -- Couple de situation Menage.

situation AllocationFamiliales source loi :
  donnee entite_en_charge_des_enfants de choix entite_en_charge ;
  constante duree_allocation_familiale de type duree defini comme 1 an ;
  donnee allocation_forfaitaire_L521_1_versee ;
  regle condition
    nombre_enfants_a_charge > nombre_minimum_enfants_L521_1 et
    (existe enfant dans contexte!enfants tel que
      enfant!age = age_limite_L512_3_2 et
      enfant!qualifie_pour_prestation_sauf_age)
  consequence allocation_forfaitaire_L521_1_versee defini.
*/
Le montant des allocations mentionnées aux deux premiers alinéas du présent article, ainsi que celui des majorations mentionnées à l'article L. 521-3 varient en fonction des ressources du ménage ou de la personne qui a la charge des enfants, selon un barème défini par décret.
/*
situation Personne source implicite :
  donnee ressources de type montant.

situation Menage source implicite :
  donnee ressources de type montant ;
  donnee parent1 de situation Personne ;
  donnee parent2 de situation Personne ;
  regle ressources defini comme
    parent1!ressources + parent2!ressources.

situation AllocationFamiliales source loi :
  donnee ressources_entite_en_charge de type montant ;
  regle ressources_entite_en_charge defini comme
    selon entite_en_charge_enfants sous forme
    -- FamilleMonoparentale de parent : parent!ressources
    -- Couple de menage : menage!ressources ;
  donnee montant_allocations_familiales de type montant ;
  assertion montant_allocations_familiales fixe par decret ;
  assertion montant_allocations_familiales varie avec
    ressources_entite_en_charge ;
  assertion allocation_forfaitaire_L521_1 fixe par decret ;
  assertion allocation_forfaitaire_L521_1 varie avec
    ressources_entite_en_charge ;
  donnee majorations_512_3 de type montant ;
  assertion majorations_512_3 fixe par decret ;
  assertion majorations_512_3 varie avec
    ressources_entite_en_charge.
*/
Le montant des allocations familiales varie en fonction du nombre d'enfants a charge.
/*
situation AllocationFamiliales source loi :
  assertion
    montant_allocations_familiales varie avec nombre_enfants_a_charge.
*/
Les niveaux des plafonds de ressources, qui varient en fonction du nombre d'enfants à charge, sont révisés conformément à l'évolution annuelle de l'indice des prix à la consommation, hors tabac.
/*
situation AllocationFamiliales source loi :
 donnee plafonds_ressources_allocations_familiales
    collection de type montant ;
 assertion
  pour tout plafond dans plafonds_ressources_allocations_familiales on a
    plafond varie avec nombre_enfants_a_charge.
# TODO: comment parler de l'évolution?
*/
Un complément dégressif est versé lorsque les ressources du bénéficiaire dépassent l'un des plafonds, dans la limite de montants définis par décret. Les modalités de calcul de ces montants et celles du complément dégressif sont définies par décret.
/*
situation AllocationFamiliales source loi :
  donnee complement_degressif_allocations_familiales de type montant ;
  assertion complement_degressif_allocations_familiales varie avec
      nombre_enfants_a_charge decroissant ;
  assertion
     complement_degressif_allocations_familiales fixe par decret.
*/

@Article L521-2@ Les allocations sont versées à la personne qui assume, dans quelques conditions que ce soit, la charge effective et permanente de l'enfant.

En cas de résidence alternée de l'enfant au domicile de chacun des parents telle que prévue à l'article 373-2-9 du code civil, mise en oeuvre de manière effective, les parents désignent l'allocataire. Cependant, la charge de l'enfant pour le calcul des allocations familiales est partagée par moitié entre les deux parents soit sur demande conjointe des parents, soit si les parents sont en désaccord sur la désignation de l'allocataire. Un décret en Conseil d'Etat fixe les conditions d'application du présent alinéa.

Lorsque la personne qui assume la charge effective et permanente de l'enfant ne remplit pas les conditions prévues au titre I du présent livre pour l'ouverture du droit aux allocations familiales, ce droit s'ouvre du chef du père ou, à défaut, du chef de la mère.

Lorsqu'un enfant est confié au service d'aide sociale à l'enfance, les allocations familiales continuent d'être évaluées en tenant compte à la fois des enfants présents au foyer et du ou des enfants confiés au service de l'aide sociale à l'enfance. La part des allocations familiales dues à la famille pour cet enfant est versée à ce service. Toutefois, le juge peut décider, d'office ou sur saisine du président du conseil général, à la suite d'une mesure prise en application des articles 375-3 et 375-5 du code civil ou des articles 15,16,16 bis et 28 de l'ordonnance n° 45-174 du 2 février 1945 relative à l'enfance délinquante, de maintenir le versement des allocations à la famille, lorsque celle-ci participe à la prise en charge morale ou matérielle de l'enfant ou en vue de faciliter le retour de l'enfant dans son foyer.

Un décret en Conseil d'Etat fixe les conditions d'application du présent article, notamment dans les cas énumérés ci-dessous :
a) retrait total de l'autorité parentale des parents ou de l'un d'eux ;
b) indignité des parents ou de l'un d'eux ;
c) divorce, séparation de corps ou de fait des parents ;
d) enfants confiés à un service public, à une institution privée, à un particulier.

@Article L521-3@ Chacun des enfants à charge, à l'exception du plus âgé, ouvre droit à partir d'un âge minimum à une majoration des allocations familiales.


Toutefois, les personnes ayant un nombre déterminé d'enfants à charge bénéficient de ladite majoration pour chaque enfant à charge à partir de l'âge mentionné au premier alinéa.
