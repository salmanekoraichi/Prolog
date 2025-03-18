/* ==========================================================
 * SYSTEME EXPERT DE CLASSIFICATION DES POISSONS
 * ==========================================================
 *
 * Auteur : Système amélioré et corrigé
 * Date   : Mars 2025
 * 
 * Ce programme est un systeme expert permettant d'identifier
 * l'ordre, la famille et la race d'un poisson selon des 
 * caracteristiques fournies par l'utilisateur.
 *
 * Utilisation :
 *   1. Sauvegardez ce fichier sous "poissons.pl"
 *   2. Lancez SWI-Prolog : swipl -s poissons.pl
 *   3. Ou chargez-le dans la console : [poissons].
 *   4. Puis lancez : demarrer.
 */

:- dynamic connait/2.  % Prédicat dynamique pour mémoriser les réponses

% Déclaration des prédicats non contigus
:- discontiguous est_famille/2.
:- discontiguous est_race/3.
:- discontiguous est_ordre/1.

/* ==========================================================
 * INTERFACE UTILISATEUR ET GESTION DU DIALOGUE
 * ========================================================== */

% Point d'entrée du programme
demarrer :-
    afficher_en_tete,
    identifier_poisson.

% Affichage de l'en-tête
afficher_en_tete :-
    nl,
    writeln('*********************************************************'),
    writeln('*            SYSTEME EXPERT ICHTYOLOGIQUE               *'),
    writeln('*          CLASSIFICATION DES POISSONS D\'EAU DOUCE       *'),
    writeln('*********************************************************'),
    writeln('Ce systeme vous aide a identifier un poisson selon ses caracteristiques.'),
    writeln('Veuillez repondre aux questions par "oui" ou "non".'),
    nl.

% Processus d'identification principal
identifier_poisson :-
    effacer_base,
    determiner_classification(Ordre, Famille, Race),
    afficher_resultat(Ordre, Famille, Race),
    demander_continuer.

% Détermination de la classification complète
determiner_classification(Ordre, Famille, Race) :-
    trouve_ordre(Ordre),
    trouve_famille(Ordre, Famille),
    trouve_race(Ordre, Famille, Race).

% Recherche de l'ordre (avec coupure après la première solution)
trouve_ordre(Ordre) :-
    est_ordre(Ordre), !.
trouve_ordre(inconnu).

% Recherche de la famille (avec coupure après la première solution)
trouve_famille(inconnu, inconnu) :- !.
trouve_famille(Ordre, Famille) :-
    est_famille(Ordre, Famille), !.
trouve_famille(_, inconnu).

% Recherche de la race (avec coupure après la première solution)
trouve_race(inconnu, _, inconnu) :- !.
trouve_race(_, inconnu, inconnu) :- !.
trouve_race(Ordre, Famille, Race) :-
    est_race(Ordre, Famille, Race), !.
trouve_race(_, _, inconnu).

% Pose une question à l'utilisateur et vérifie que la réponse correspond à l'attendu.
% Si la question a déjà été posée, on compare la réponse mémorisée.
poser_question(Question, Attendu) :-
    (   connait(Question, Reponse)
    ->  ( Reponse = Attendu
        -> true
        ;  fail )
    ;   format('~w ? (oui/non): ', [Question]),
        flush_output(current_output),
        read_line_to_string(user_input, Input),
        string_lower(Input, InputLower),
        (   member(InputLower, ["oui", "o", "yes", "y"])
        ->  Reponse = "oui"
        ;   member(InputLower, ["non", "n", "no"])
        ->  Reponse = "non"
        ;   writeln('Veuillez repondre par "oui" ou "non".'),
            poser_question(Question, Attendu)
        ),
        assertz(connait(Question, Reponse)),
        Reponse = Attendu
    ).

% Affichage du résultat de l'identification
afficher_resultat(Ordre, Famille, Race) :-
    nl,
    writeln('*********************************************************'),
    writeln('RESULTAT DE L\'IDENTIFICATION :'),
    writeln('*********************************************************'),
    afficher_si_connu('Ordre  ', Ordre),
    afficher_si_connu('Famille', Famille),
    afficher_si_connu('Race   ', Race),
    nl.

% Affiche une propriété si elle est connue (différente de 'inconnu')
afficher_si_connu(Label, Valeur) :-
    (   Valeur \= inconnu
    ->  format('~w : ~w~n', [Label, Valeur])
    ;   format('~w : Impossible à déterminer avec les informations fournies~n', [Label])
    ).

% Nettoyage de la base de connaissances
effacer_base :-
    retractall(connait(_,_)).

% Demande à l'utilisateur s'il souhaite recommencer
demander_continuer :-
    nl,
    write('Voulez-vous identifier un autre poisson ? (oui/non): '),
    flush_output(current_output),
    read_line_to_string(user_input, Rep),
    string_lower(Rep, RepLower),
    (   member(RepLower, ["oui", "o", "yes", "y"])
    ->  nl, identifier_poisson
    ;   writeln('Merci d\'avoir utilise le systeme expert de classification des poissons !')
    ).

/* ==========================================================
 * REGLES DE CLASSIFICATION DES POISSONS
 * ========================================================== */

%-------------- Règles de détermination de l'ordre --------------

% R1 : Ordre des Cyprinodontiformes
est_ordre(cyprinodontiformes) :-
    poser_question('Le poisson a-t-il une petite bouche', "oui"),
    poser_question('Le poisson a-t-il des dents pointues', "oui"),
    poser_question('Le poisson vit-il dans les rivieres de plusieurs parties du monde', "oui").

% R6 : Ordre des Anatides (Labyrinthiformes)
est_ordre(anatides) :-
    poser_question('Le poisson est-il de taille moyenne', "oui"),
    poser_question('Le poisson a-t-il des dents pointues', "oui"),
    poser_question('Le poisson est-il de couleur bleue', "oui"),
    poser_question('Le poisson vit-il dans toutes les rivieres du monde', "oui"),
    poser_question('Le poisson possede-t-il des canaux labyrinthiques pour respirer hors de l\'eau', "oui").

% R9 : Ordre des Cichlides
est_ordre(cichlides) :-
    poser_question('Le poisson a-t-il une petite bouche', "oui"),
    poser_question('Le poisson vit-il dans toutes les rivieres du monde', "oui"),
    poser_question('Le poisson a-t-il la queue arrondie', "oui").

% Règle supplémentaire : Ordre des Cypriniformes (carpes, goujons)
est_ordre(cypriniformes) :-
    poser_question('Le poisson a-t-il une bouche en position inférieure', "oui"),
    poser_question('Le poisson possède-t-il des barbillons autour de la bouche', "oui"),
    poser_question('Le poisson a-t-il des écailles visibles', "oui").

% Règle supplémentaire : Ordre des Perciformes (perches, mérous)
est_ordre(perciformes) :-
    poser_question('Le poisson a-t-il deux nageoires dorsales distinctes', "oui"),
    poser_question('Le poisson a-t-il des nageoires épineuses', "oui").

% Règle supplémentaire : Ordre des Siluriformes (poissons-chats)
est_ordre(siluriformes) :-
    poser_question('Le poisson a-t-il une peau sans écailles', "oui"),
    poser_question('Le poisson possède-t-il des barbillons proéminents', "oui").

%-------------- Règles de détermination de la famille --------------

% R2 & R3 : Famille des Poeciliens (pour mâle et femelle)
est_famille(cyprinodontiformes, poeciliens) :-
    poser_question('Le poisson vit-il dans les rivieres d\'Amerique du Sud', "oui"),
    (   poser_question('Le poisson est-il male', "oui")
    ->  poser_question('Le poisson possede-t-il un gonopode', "oui"),
        writeln('Information: La femelle est 3 cm plus grande que le male.')
    ;   poser_question('Le poisson est-il femelle', "oui"),
        writeln('Information: Le male est 3 cm plus petit que la femelle.')
    ).

% Famille des Cyprinidés (pour Cypriniformes)
est_famille(cypriniformes, cyprinides) :-
    poser_question('Le poisson a-t-il un corps trapu', "oui"),
    poser_question('Le poisson a-t-il une nageoire dorsale unique', "oui").

% Famille des Percidés (pour Perciformes)
est_famille(perciformes, percides) :-
    poser_question('Le poisson a-t-il des rayures verticales sur le corps', "oui").

% Famille des Centrarchidés (pour Perciformes aussi)
est_famille(perciformes, centrarchides) :-
    poser_question('Le poisson a-t-il un corps aplati latéralement', "oui"),
    poser_question('Le poisson a-t-il une grande bouche', "oui").

% Famille des Siluridés (pour Siluriformes)
est_famille(siluriformes, silurids) :-
    poser_question('Le poisson a-t-il une large tête plate', "oui").

% Par défaut pour les autres ordres
est_famille(anatides, anatides_famille).
est_famille(cichlides, cichlides_famille).

%-------------- Règles de détermination de la race --------------

% R4 : Gambusia Affinis
est_race(cyprinodontiformes, poeciliens, gambusia_affinis) :-
    poser_question('Le poisson a-t-il des taches le long de son corps', "oui").

% R5 : Gambusia Pinctada
est_race(cyprinodontiformes, poeciliens, gambusia_pinctada) :-
    poser_question('Le poisson est-il de couleur grise', "oui"),
    poser_question('Le poisson a-t-il des rayures vertes le long de son corps', "oui").

% R7 : Combattant de Siam
est_race(anatides, _, combattant_de_siam) :-
    poser_question('Le poisson a-t-il des rayures', "oui"),
    poser_question('Le poisson vit-il dans les rivieres d\'Asie', "oui").

% R8 : Pecan Grimpant
est_race(anatides, _, pecan_grimpant) :-
    poser_question('Le poisson mesure-t-il 25 cm', "oui"),
    poser_question('Le poisson a-t-il des rayures', "non").

% R10 : Poisson Joyau
est_race(cichlides, _, poisson_joyau) :-
    poser_question('Le poisson vit-il dans les rivieres d\'Afrique', "oui"),
    poser_question('Le poisson est-il de couleur rouge avec des taches noires', "oui").

% Carpe commune
est_race(cypriniformes, cyprinides, carpe_commune) :-
    poser_question('Le poisson a-t-il des grandes écailles', "oui"),
    poser_question('Le poisson est-il de couleur bronze à doré', "oui").

% Carpe Koï
est_race(cypriniformes, cyprinides, carpe_koi) :-
    poser_question('Le poisson présente-t-il des motifs colorés variés', "oui"),
    poser_question('Le poisson est-il d\'origine japonaise', "oui").

% Perche commune
est_race(perciformes, percides, perche_commune) :-
    poser_question('Le poisson a-t-il une couleur verdâtre avec des bandes verticales sombres', "oui"),
    poser_question('Le poisson a-t-il des nageoires rougeâtres', "oui").

% Black-bass (achigan à grande bouche)
est_race(perciformes, centrarchides, black_bass) :-
    poser_question('Le poisson a-t-il une mâchoire supérieure qui dépasse l\'œil', "oui"),
    poser_question('Le poisson est-il de couleur vert olive', "oui").

% Silure glane
est_race(siluriformes, silurids, silure_glane) :-
    poser_question('Le poisson peut-il atteindre une grande taille (plus de 1 mètre)', "oui"),
    poser_question('Le poisson a-t-il six barbillons autour de la bouche', "oui").

% Initialisation automatique du programme
:- initialization(demarrer, main).
