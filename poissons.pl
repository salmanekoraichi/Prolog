/* ==========================================================
 * SYSTEME EXPERT DE CLASSIFICATION DES POISSONS - Système à Pondération
 * ==========================================================
 *
 * Auteur : Système amélioré et corrigé
 * Date   : Mars 2025
 * 
 * Description :
 *   Ce système expert utilise un mécanisme de pondération pour déterminer
 *   l'ordre, la famille et la race d'un poisson en fonction des réponses
 *   fournies par l'utilisateur. Chaque question pertinente possède un poids.
 *   Le score total de chaque candidat est calculé en sommant ces poids si la
 *   réponse est 'oui'. La classification retenue est celle avec le score maximum.
 *
 * Modes d'utilisation :
 *   - Mode interactif : Les questions sont posées au terminal via `demarrer/0`.
 *   - Mode non interactif : Utilisez le prédicat `classify_reponses/4`
 *     pour injecter directement une liste de couples Question-Réponse (par exemple,
 *     via une interface Python utilisant pyswip).
 *
 * Utilisation :
 *   1. Sauvegardez ce fichier sous "poissons.pl".
 *   2. Mode interactif :
 *         Lancez SWI-Prolog avec : swipl -s poissons.pl
 *         Puis lancez : demarrer.
 *   3. Mode non interactif (via Python par exemple) :
 *         Utilisez le prédicat classify_reponses/4.
 */

% -----------------------------------------------------------------
% Section 1 : Gestion de la Base de Connaissances et du Mode
% -----------------------------------------------------------------
:- dynamic connait/2.            % Stocke les réponses de l'utilisateur, sous forme connaitt(Question, Réponse)
:- dynamic non_interactive_mode/0. % Flag pour activer le mode non interactif

% Pour permettre la définition de règles dans des sections différentes
:- discontiguous score_order/2.
:- discontiguous score_family/3.
:- discontiguous score_race/4.

% -----------------------------------------------------------------
% Section 2 : Mode Interactif (Interface Terminal)
% -----------------------------------------------------------------

% demarrer/0 : Point d'entrée en mode interactif.
demarrer :-
    afficher_en_tete,
    identifier_poisson.

% afficher_en_tete/0 : Affiche un en-tête descriptif.
afficher_en_tete :-
    nl,
    writeln('*********************************************************'),
    writeln('*            SYSTEME EXPERT ICHTYOLOGIQUE               *'),
    writeln('*          CLASSIFICATION DES POISSONS D''EAU DOUCE       *'),
    writeln('*********************************************************'),
    writeln('Ce système expert identifie un poisson selon ses caractéristiques.'),
    writeln('Veuillez répondre aux questions par "oui" ou "non".'),
    nl.

% identifier_poisson/0 : Lance le processus interactif complet.
identifier_poisson :-
    effacer_base,                    % Réinitialise les réponses précédentes.
    classify_poisson(Order, Family, Race),
    afficher_resultat(Order, Family, Race),
    demander_continuer.

% afficher_resultat/3 : Affiche la classification obtenue.
afficher_resultat(Order, Family, Race) :-
    nl,
    writeln('*********************************************************'),
    writeln('RESULTAT DE L''IDENTIFICATION :'),
    writeln('*********************************************************'),
    afficher_si_connu('Ordre  ', Order),
    afficher_si_connu('Famille', Family),
    afficher_si_connu('Race   ', Race),
    nl.

% afficher_si_connu/2 : Affiche un label et sa valeur si connue, sinon affiche une erreur.
afficher_si_connu(Label, Valeur) :-
    (   Valeur \= inconnu
    ->  format('~w : ~w~n', [Label, Valeur])
    ;   format('~w : Impossible à déterminer avec les informations fournies~n', [Label])
    ).

% effacer_base/0 : Supprime toutes les réponses enregistrées.
effacer_base :-
    retractall(connait(_,_)).

% demander_continuer/0 : Demande à l'utilisateur s'il souhaite recommencer.
demander_continuer :-
    nl,
    write('Voulez-vous identifier un autre poisson ? (oui/non): '),
    flush_output(current_output),
    read_line_to_string(user_input, Rep),
    string_lower(Rep, RepLower),
    (   member(RepLower, ["oui", "o", "yes", "y"])
    ->  nl, identifier_poisson
    ;   writeln('Merci d''avoir utilisé le système expert de classification des poissons !')
    ).

% -----------------------------------------------------------------
% Section 3 : Prédicat poser_question/2 (Interactif / Non-interactif)
% -----------------------------------------------------------------
/*
  poser_question(Question, Attendu) :
  - Si la réponse à la Question est déjà enregistrée (via connait/2), on la compare à l'attendu.
  - En mode interactif, si elle n'est pas enregistrée, la question est posée au terminal.
  - En mode non interactif (flag activé), le prédicat échoue si la réponse n'est pas fournie.
*/
poser_question(Question, Attendu) :-
    (   connait(Question, R)
    ->  ( R = Attendu -> true ; fail )
    ;   ( non_interactive_mode ->
             format(user_error, 'Erreur: Réponse manquante pour "~w" en mode non interactif.~n', [Question]),
             fail
         ;   % Mode interactif : demande de la réponse
             format('~w ? (oui/non): ', [Question]),
             flush_output(current_output),
             read_line_to_string(user_input, Input),
             string_lower(Input, IL),
             (   member(IL, ["oui", "o", "yes", "y"]) -> R = 'oui'
             ;   member(IL, ["non", "n", "no"]) -> R = 'non'
             ;   writeln('Veuillez répondre par "oui" ou "non".'),
                 poser_question(Question, Attendu)
             ),
             assertz(connait(Question, R)),
             R = Attendu
         )
    ).

% -----------------------------------------------------------------
% Section 4 : Système de Pondération pour l'ORDRE
% -----------------------------------------------------------------
/*
  Chaque question importante pour déterminer un ordre possède un poids.
  Le prédicat score_question/3 retourne ce poids si la réponse enregistrée est 'oui', sinon 0.
*/
score_question(Question, Poids, Score) :-
    (   connait(Question, 'oui')
    ->  Score is Poids
    ;   Score is 0).

% Calcul des scores pour chaque ordre candidat :
score_order(cyprinodontiformes, Score) :-
    score_question('Le poisson a-t-il une petite bouche', 1, S1),
    score_question('Le poisson a-t-il des dents pointues', 1, S2),
    score_question('Le poisson vit-il dans les rivieres de plusieurs parties du monde', 1, S3),
    Score is S1 + S2 + S3.

score_order(anatides, Score) :-
    score_question('Le poisson est-il de taille moyenne', 1, S1),
    score_question('Le poisson a-t-il des dents pointues', 1, S2),
    score_question('Le poisson est-il de couleur bleue', 1, S3),
    score_question('Le poisson vit-il dans toutes les rivieres du monde', 1, S4),
    score_question('Le poisson possede-t-il des canaux labyrinthiques pour respirer hors de l\'eau', 1, S5),
    Score is S1 + S2 + S3 + S4 + S5.

score_order(cichlides, Score) :-
    score_question('Le poisson a-t-il une petite bouche', 1, S1),
    score_question('Le poisson vit-il dans toutes les rivieres du monde', 1, S2),
    score_question('Le poisson a-t-il la queue arrondie', 1, S3),
    Score is S1 + S2 + S3.

score_order(cypriniformes, Score) :-
    % Remarquez que le texte de la question doit être exactement le même.
    score_question('Le poisson a-t-il une bouche en position inferieure', 1, S1),
    score_question('Le poisson possede-t-il des barbillons autour de la bouche', 1, S2),
    score_question('Le poisson a-t-il des ecailles visibles', 1, S3),
    Score is S1 + S2 + S3.

score_order(perciformes, Score) :-
    score_question('Le poisson a-t-il deux nageoires dorsales distinctes', 1, S1),
    score_question('Le poisson a-t-il des nageoires epineuses', 1, S2),
    Score is S1 + S2.

score_order(siluriformes, Score) :-
    score_question('Le poisson a-t-il une peau sans ecailles', 1, S1),
    score_question('Le poisson possede-t-il des barbillons proeminents', 1, S2),
    Score is S1 + S2.

% determine_order/1 : Sélectionne l'ordre avec le meilleur score.
determine_order(Order) :-
    score_order(cyprinodontiformes, S1),
    score_order(anatides, S2),
    score_order(cichlides, S3),
    score_order(cypriniformes, S4),
    score_order(perciformes, S5),
    score_order(siluriformes, S6),
    List = [cyprinodontiformes-S1, anatides-S2, cichlides-S3, cypriniformes-S4, perciformes-S5, siluriformes-S6],
    max_list_scores(List, Order-Score),
    (Score > 0 -> true ; Order = inconnu).

% max_list_scores/2 : Renvoie le couple (Candidat-Score) avec le score maximum dans une liste.
max_list_scores([X], X).
max_list_scores([X|Xs], Max) :-
    max_list_scores(Xs, Temp),
    X = _-ScoreX,
    Temp = _-ScoreT,
    (ScoreX >= ScoreT -> Max = X ; Max = Temp).

% -----------------------------------------------------------------
% Section 5 : Système de Pondération pour la FAMILLE (Exemple)
% -----------------------------------------------------------------
/*
  Pour cet exemple, nous ne traitons qu'un cas pour l'ordre cyprinodontiformes.
  Si la question sur "Les rivières d'Amérique du Sud" vaut 'oui', alors on attribue la famille poeciliens.
*/
determine_family(cyprinodontiformes, poeciliens) :-
    score_question('Le poisson vit-il dans les rivieres d\'Amerique du Sud', 1, S),
    S >= 1, !.
determine_family(_, inconnu).

% -----------------------------------------------------------------
% Section 6 : Système de Pondération pour la RACE (Exemple)
% -----------------------------------------------------------------
/*
  Pour l'ordre cyprinodontiformes et la famille poeciliens,
  si la question sur "des taches le long du corps" vaut 'oui', la race sera gambusia_affinis.
*/
determine_race(cyprinodontiformes, poeciliens, gambusia_affinis) :-
    score_question('Le poisson a-t-il des taches le long de son corps', 1, S),
    S >= 1, !.
determine_race(_, _, inconnu).

% -----------------------------------------------------------------
% Section 7 : Classification Complète
% -----------------------------------------------------------------
/*
  classify_poisson/3 :
  Calcule d'abord l'ordre en appelant determine_order/1,
  puis la famille et la race correspondantes via determine_family/2 et determine_race/3.
*/
classify_poisson(Order, Family, Race) :-
    determine_order(Order),
    determine_family(Order, Family),
    determine_race(Order, Family, Race).

% -----------------------------------------------------------------
% Section 8 : Prédicats pour l'Utilisation Non-Interactive (Intégration avec Python)
% -----------------------------------------------------------------
/*
  init_reponses/1 :
  Prend une liste de couples Question-Réponse et les enregistre dans la base.
*/
init_reponses([]).
init_reponses([Question-Reponse | T]) :-
    assertz(connait(Question, Reponse)),
    init_reponses(T).

/*
  classify_reponses/4 :
  Efface la base, initialise avec les réponses fournies,
  active le mode non interactif et détermine la classification.
*/
classify_reponses(Responses, Order, Family, Race) :-
    effacer_base,
    init_reponses(Responses),
    assert(non_interactive_mode),
    classify_poisson(Order, Family, Race),
    retract(non_interactive_mode).

% -----------------------------------------------------------------
% LANCEMENT EN MODE INTERACTIF
% -----------------------------------------------------------------
% Pour lancer le système en mode interactif depuis le terminal, décommentez la ligne suivante :
% :- initialization(demarrer, main).
