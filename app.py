import streamlit as st
from pyswip import Prolog

# -----------------------------------------------------------------------------
# Fonctions Utilitaires
# -----------------------------------------------------------------------------
def prolog_escape(text):
    """
    Échappe les apostrophes dans une chaîne pour respecter la syntaxe Prolog.
    Par exemple, "d'Amerique" devient "d''Amerique".
    """
    return text.replace("'", "''")

def run_classification(responses, prolog):
    """
    Construit la requête Prolog à partir d'une liste de couples (question, réponse),
    exécute la requête et renvoie le résultat ainsi que la chaîne de requête générée.

    Args:
        responses (list of tuple): Liste des couples (question, réponse)
        prolog (Prolog): L'instance du moteur Prolog chargé

    Returns:
        result (list): Résultat de la requête (liste de dictionnaires)
        responses_prolog (str): La chaîne Prolog générée pour la requête
    """
    # Construit une liste Prolog sous forme de:
    # ['Question'-'Réponse', 'AutreQuestion'-'AutreRéponse', ...]
    responses_prolog = "[" + ", ".join([
        f"'{prolog_escape(question)}'-'{prolog_escape(answer)}'" 
        for question, answer in responses
    ]) + "]"
    # Construit la requête en appelant le prédicat non interactif
    query = f"classify_reponses({responses_prolog}, Ordre, Famille, Race)"
    result = list(prolog.query(query))
    return result, responses_prolog

def load_prolog():
    """
    Charge le fichier Prolog (poissons.pl) et renvoie l'instance Prolog.
    """
    prolog = Prolog()
    prolog.consult("poissons.pl")
    return prolog

# -----------------------------------------------------------------------------
# Initialisation de Prolog
# -----------------------------------------------------------------------------
try:
    prolog = load_prolog()
except Exception as e:
    st.error(f"Erreur lors du chargement de Prolog : {e}")

# -----------------------------------------------------------------------------
# Navigation dans l'application via la barre latérale
# -----------------------------------------------------------------------------
st.sidebar.title("Navigation")
page = st.sidebar.radio("Choisissez une page", ["Live Demo", "Documentation", "Code Source"])

# -----------------------------------------------------------------------------
# Page Live Demo
# -----------------------------------------------------------------------------
if page == "Live Demo":
    st.title("Système Expert de Classification des Poissons")
    st.markdown(
    """
    **Ce système expert** est basé sur un moteur Prolog qui identifie l'ordre, la famille et la race d'un poisson
    en fonction de réponses à une série de questions.
    
    **Fonctionnement :**
    - Vous répondez aux questions via l'interface.
    - Les réponses sont converties en une requête Prolog.
    - La requête est envoyée au moteur Prolog via pyswip.
    - Le résultat (classification) est affiché.
    
    > **Note :** Le mode non interactif est activé dans Prolog pour que toutes les réponses soient injectées
    > depuis l'interface, évitant ainsi les demandes en ligne de commande.
    """
    )
    
    # Collecte des réponses via des widgets interactifs
    st.subheader("Vos Réponses")
    q1 = st.radio("1. Le poisson a-t-il une petite bouche ?", ("oui", "non"), key="q1")
    q2 = st.radio("2. Le poisson a-t-il des dents pointues ?", ("oui", "non"), key="q2")
    q3 = st.radio("3. Le poisson vit-il dans les rivieres de plusieurs parties du monde ?", ("oui", "non"), key="q3")
    q4 = st.radio("4. Le poisson vit-il dans les rivieres d'Amerique du Sud ?", ("oui", "non"), key="q4")
    q5 = st.radio("5. Le poisson est-il male ?", ("oui", "non"), key="q5")
    q6 = st.radio("6. Le poisson possede-t-il un gonopode ?", ("oui", "non"), key="q6")
    q7 = st.radio("7. Le poisson a-t-il des taches le long de son corps ?", ("oui", "non"), key="q7")
    
    # Assemble la liste des réponses (les questions doivent correspondre exactement aux textes dans poissons.pl)
    responses = [
        ("Le poisson a-t-il une petite bouche", q1),
        ("Le poisson a-t-il des dents pointues", q2),
        ("Le poisson vit-il dans les rivieres de plusieurs parties du monde", q3),
        ("Le poisson vit-il dans les rivieres d'Amerique du Sud", q4),
        ("Le poisson est-il male", q5),
        ("Le poisson possede-t-il un gonopode", q6),
        ("Le poisson a-t-il des taches le long de son corps", q7),
    ]
    
    # Bouton pour lancer la classification
    if st.button("Classer le poisson"):
        with st.spinner("Classification en cours..."):
            result, responses_prolog = run_classification(responses, prolog)
            st.markdown("### Requête Prolog générée")
            st.code(responses_prolog, language="prolog")
            if result:
                res = result[0]
                st.success("Classification trouvée !")
                st.write("**Ordre :**", res["Ordre"])
                st.write("**Famille :**", res["Famille"])
                st.write("**Race :**", res["Race"])
            else:
                st.error("Impossible de classifier le poisson avec les informations fournies.")
    
    st.info("Tous les champs doivent être remplis pour que le système retourne une classification.")

# -----------------------------------------------------------------------------
# Page Documentation
# -----------------------------------------------------------------------------
elif page == "Documentation":
    st.title("Documentation et Explications")
    st.markdown(
    """
    ## Aperçu du Système Expert
    
    Ce système expert utilise un fichier Prolog (*poissons.pl*) pour déterminer la classification d'un poisson.
    Les points clés sont :
    
    - **Base de Connaissances Prolog :**  
      Contient les règles définissant l'ordre, la famille et la race des poissons.
    
    - **Interaction Non-Interactive :**  
      Le prédicat `classify_reponses/4` permet d'injecter des réponses (via une liste de couples) dans la base.
      Ainsi, l'interface graphique (Streamlit) peut envoyer directement toutes les réponses sans demande en ligne de commande.
    
    - **Intégration avec pyswip :**  
      L'interface Streamlit interroge Prolog via la bibliothèque `pyswip` pour obtenir le résultat de classification.
    
    ## Structure du Code Prolog (poissons.pl)
    
    - **Gestion de la Base de Connaissances :**  
      Les réponses de l'utilisateur sont stockées dans le prédicat dynamique `connait/2`.
    
    - **Mode Interactif vs Non Interactif :**  
      En mode interactif, le système demande à l'utilisateur les réponses via le terminal.
      En mode non interactif (activé par le flag `non_interactive_mode`), les réponses doivent être fournies en amont.
    
    - **Règles de Classification :**  
      Les règles sont définies par les prédicats `est_ordre/1`, `est_famille/2` et `est_race/3`.
      Exemple :
      
      ```prolog
      est_ordre(cyprinodontiformes) :-
          poser_question('Le poisson a-t-il une petite bouche', 'oui'),
          poser_question('Le poisson a-t-il des dents pointues', 'oui'),
          poser_question('Le poisson vit-il dans les rivieres de plusieurs parties du monde', 'oui').
      ```
    
    ## Déploiement sur Streamlit Cloud
    
    Pour déployer cette application sur Streamlit Cloud :
    
    1. **Fichiers requis :**
       - `app.py` (cette interface)
       - `poissons.pl` (le fichier Prolog)
       - `requirements.txt` (incluant au moins `streamlit` et `pyswip`)
       - `packages.txt` avec la ligne :  
         ```
         swi-prolog
         ```
    
    2. **Configuration :**
       Poussez votre code sur GitHub et connectez-le à Streamlit Cloud pour déployer l'application.
    """
    )
    
    with st.expander("Détails supplémentaires sur l'architecture du système"):
        st.markdown(
        """
        ### Architecture Complète
        
        - **Fichier Prolog (poissons.pl) :**  
          Contient toute la logique de classification, avec des règles pour l'ordre, la famille et la race.
          Il dispose de deux modes d'exécution (interactif et non interactif).
        
        - **Interface Python (app.py) :**  
          Utilise Streamlit pour offrir une interface graphique moderne et intuitive.
          La navigation se fait via une barre latérale qui permet d'accéder à la démo, la documentation et au code source.
        
        - **Intégration via pyswip :**  
          Permet d'interroger Prolog directement depuis Python.
          Le résultat de la requête est affiché en temps réel dans l'interface.
        """
        )

# -----------------------------------------------------------------------------
# Page Code Source
# -----------------------------------------------------------------------------
elif page == "Code Source":
    st.title("Code Source Complet")
    st.markdown("### Code de l'application Streamlit (app.py)")
    with st.expander("Voir le code de app.py"):
        st.code(
r'''import streamlit as st
from pyswip import Prolog

def prolog_escape(text):
    """Escape apostrophes in text for proper Prolog syntax."""
    return text.replace("'", "''")

def run_classification(responses, prolog):
    responses_prolog = "[" + ", ".join([
        f"'{prolog_escape(question)}'-'{prolog_escape(answer)}'" 
        for question, answer in responses
    ]) + "]"
    query = f"classify_reponses({responses_prolog}, Ordre, Famille, Race)"
    result = list(prolog.query(query))
    return result, responses_prolog

def load_prolog():
    prolog = Prolog()
    prolog.consult("poissons.pl")
    return prolog

try:
    prolog = load_prolog()
except Exception as e:
    st.error(f"Error loading Prolog: {e}")

st.sidebar.title("Navigation")
page = st.sidebar.radio("Choisissez une page", ["Live Demo", "Documentation", "Code Source"])

# [Rest of the code omitted for brevity...]
''', language="python")
    
    st.markdown("### Code du Système Expert Prolog (poissons.pl)")
    with st.expander("Voir le code de poissons.pl"):
        st.code(
r'''/* ==========================================================
 * SYSTEME EXPERT DE CLASSIFICATION DES POISSONS
 * ==========================================================
 * [Le code complet, commenté et documenté, se trouve dans ce fichier.]
 * Ce fichier définit :
 * - La gestion de la base de connaissances (connait/2)
 * - Le mode interactif et non interactif (avec le flag non_interactive_mode)
 * - Les règles de classification via est_ordre/1, est_famille/2, est_race/3
 * - Les prédicats init_reponses/1 et classify_reponses/4 pour l'intégration externe.
 */''', language="prolog")
    
    st.markdown("Pour plus de détails et pour télécharger l'intégralité du code, consultez le dépôt GitHub [ici](https://github.com/salmanekoraichi/Prolog).")
