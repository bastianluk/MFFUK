# Introduction to Recommender Systems

## What to focus on

Klíčové pojmy (v tomto byste neměli při zkoušce zaváhat):
 - co je doporučovací systém, co je jeho cílem, typický input & output,
 - základní taxonomie algoritmů (non-personalized, collaborative, content based, knowledge-based, hybrid) + příklady

Klíčové algoritmy (měli byste být schopni je poměrně detailně popsat - tak abych uvěřil, že je případně dokážete i naprogramovat):
 - user/item-based KNN,
 - faktorizace matic (stochastic gradient descend),
 - základní content-based metody (podobnosti CB vlastností a jejich úskalí, TF-IDF, Vector Space Model)
 - item2vec (word2vec pro situaci, kdy zaměníme sekvence zobrazených items za věty)
 - multi-armed bandits (znát detailně alespoň jednu metodu výběru ramen)

Vědět co jsou / na jakém principu fungují / co zhruba obsahují:
 - evaluace doporučovacích systémů ( off-line / user study / A/B testing; cross-validation; click through rate /  conversions / precision / recall / RMSE / nDCG / MAP; proč je nutné provádět evaluaci? Jak poznat že je jeden  doporučovač lepší než jiný?)
 - context-aware recommendation: způsoby zapojení kontextu + typické varianty kontextu (teprve bude probráno),
 - sequence-based / sequence-aware doporučování a vyhodnocování
 - explanations: proč jsou důležité + jak je vytvářet (teprve bude probráno),
 - implicit / explicit feedback: příklady, výhody a nevýhody, jak zpětnou vazbu zpracovávat
 - content-based vs. knowledge-based algoritmy,
 - typické nasazení doporučovacích algoritmů + jaké přístupy se hodí (homepage/kategorie/detail produktu; doporučování v  sociálních sítích; POIs - points of interest; hudební doporučovače; doporučování módy... )
 - typické problémy doporučovacích systémů (sparsity/cold start problem, quality of content, scalability, up-to-date  modely),
 - deep learning (convolutional neural network, autoencoders, recurrent neural networks, 2vec models + kdy se (ne)dají použít, jaké jsou jejich (ne)výhody)

Mít (argumenty podloženou) představu o vhodném návrhu RecSys dle
 - velikosti datové sady, sparsity, kvality CB dat
 - rychlosti stárnutí objektů / fluktuace uživatelů
 - domény + cílů uživatelů a provozovatele
 - cílových zařízeních,...
 - cca v rozsahu co jste dělali ve dvojcích/trojcích na cvičení podle zadaného webu

## Recommender System

### **Definition**

Recommender Systems
 - Users do not know what they want (or do not know how to ask for it)
 - RS tries to understand user’s needs through observed behavior (provide suitable results for these needs without being explicitly asked)
 - Implicit query

#### Search Engines

 - Users know in advance what they want (and is able to specify it)
 - Explicit query submitted by the user
 - Evaluation through known „correct“ answers for the query


#### Personalized Adds
 - Except from being stupid most of the time (blame the lack of feedback and devs. vision)
 - Mostly works extra-site, without elementary indication of user’s needs
   - Often, the basic principle of RecSys (mutual benefits) is violated


### Purpose and success criteria

Different perspectives/aspects
 - Depends on domain and purpose
 - No holistic evaluation scenario exists (there is no „correct“ recommender)

Retrieval perspective (search engine)
 - Reduce search costs (you could find it as well, but this is faster)
 - Provide "correct" proposals

Recommendation perspective
 - Serendipity – identify items from the Long Tail


### Approaches

#### Collaborative filtering

> Tell me what is popular among my peers

##### Problems

 - snowball effect
 - **coldstart**

#### Content-based

> Show me more of what I have liked

##### Problems

#### Knowledge-based

> Tell me what fits my needs the best

##### Problems

#### Hybrid

> Composition of before mentioned systems

### Lifecycle

 1. Get User Feedback
 2. Learn user preference
 3. Upon demand,  recommend objects

 - The process is asynchronous  by nature
 - That seriously complicate things

## Algorithms

### Simple non-personalized recommending algorithms

 - popularity based
 - > who did this, did that as well
   - SQL aggregation query
   - item to item recommendation

### Simple personalized recommending algorithms

 - user-based KNN
   - KNN - k nearest neighbours (k items that are the most similar to item X)
   - we have some list of past actions
   - based on list of past actions (like ratings of movies, with some similarity - cosine etc), find KNN
