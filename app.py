import streamlit as st
from pyswip import Prolog

def prolog_escape(text):
    """Échappe les apostrophes en doublant les guillemets simples."""
    return text.replace("'", "''")

# Initialisation de Prolog et chargement du fichier poissons.pl
prolog = Prolog()
prolog.consult("poissons.pl")  # Le fichier poissons.pl doit être dans le même dossier

st.title("Système Expert de Classification des Poissons")
st.write("Répondez aux questions ci-dessous pour identifier votre poisson.")

# Collecte des réponses via des widgets Streamlit
q1 = st.radio("Le poisson a-t-il une petite bouche ?", ("oui", "non"))
q2 = st.radio("Le poisson a-t-il des dents pointues ?", ("oui", "non"))
q3 = st.radio("Le poisson vit-il dans les rivières de plusieurs parties du monde ?", ("oui", "non"))
q4 = st.radio("Le poisson vit-il dans les rivières d'Amérique du Sud ?", ("oui", "non"))
q5 = st.radio("Le poisson est-il male ?", ("oui", "non"))
q6 = st.radio("Le poisson posséde-t-il un gonopode ?", ("oui", "non"))
q7 = st.radio("Le poisson a-t-il des taches le long de son corps ?", ("oui", "non"))

if st.button("Classer le poisson"):
    # Utilisation d'une liste de tuples pour représenter chaque couple question-réponse
    reponses = [
        ("Le poisson a-t-il une petite bouche", q1),
        ("Le poisson a-t-il des dents pointues", q2),
        ("Le poisson vit-il dans les rivieres de plusieurs parties du monde", q3),
        ("Le poisson vit-il dans les rivieres d'Amerique du Sud", q4),
        ("Le poisson est-il male", q5),
        ("Le poisson possede-t-il un gonopode", q6),
        ("Le poisson a-t-il des taches le long de son corps", q7),
    ]
    
    # Conversion de la liste Python en une chaîne formatée pour Prolog
    reponses_prolog = "[" + ", ".join([
        f"'{prolog_escape(question)}'-'{prolog_escape(answer)}'" 
        for question, answer in reponses
    ]) + "]"
    
    st.write("Initialisation des réponses en Prolog :")
    st.write(reponses_prolog)
    
    # Construction et exécution de la requête Prolog
    query = f"classify_reponses({reponses_prolog}, Ordre, Famille, Race)"
    result = list(prolog.query(query))
    
    if result:
        res = result[0]
        st.success("Classification trouvée :")
        st.write("Ordre :", res["Ordre"])
        st.write("Famille :", res["Famille"])
        st.write("Race :", res["Race"])
    else:
        st.error("Impossible de classifier le poisson avec les informations fournies.")
