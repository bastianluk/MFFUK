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

> RS are software agents that elicit the interests and preferences of individual consumers […] and make recommendations accordingly. They have the potential to support and improve the quality of the decisions consumers make while searching for and selecting products online. (Xiao & Benbasat 20071)

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

 - User profile & contextual parameters
 - community data

##### Problems

 - snowball effect
 - **coldstart**

#### Content-based

> Show me more of what I have liked

#### Knowledge-based

> Tell me what fits my needs the best

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

 1. popularity based
 2. > who did this, did that as well
   - SQL aggregation query
   - item to item recommendation

### Simple personalized recommending algorithms

 1. user-based KNN
   - KNN - k nearest neighbours (k items that are the most similar to item X)
   - we have some list of past actions
   - based on list of past actions (like ratings of movies, with some similarity - cosine etc), find KNN


## Collaborative filtering

(used to be; < 2014) The most prominent approach to generate recommendations
 - used by large, commercial e-commerce sites
 - well-understood, various algorithms and variations exist
 - applicable in many domains (book, movies, DVDs, ..)

Approach
 - use the "wisdom of the crowd" to recommend items

Basic assumption and idea
 - Users give ratings to catalog items (implicitly or explicitly)
 - Customers who had similar tastes in the past, will have similar tastes in the future

### Approach

Input
 - Only a matrix of given user–item ratings

Output types
 - A (numerical) prediction indicating to what degree the current user will like or dislike a certain item
   - Less relevant nowadays
   - Shown somewhere in the product description
 - A top-N list of recommended items
   - This is what you need in the end anyway

### User-based nearest-neighbor collaborative filtering

Assumption
 - user preferences remain stable and consistent over time
   - solution
     - decay of relevance
     - remove old data
     - detect changes in preference

Technique
- given active user Alice and an item i
  - find a set of users who liked the same items alice in the past and who rated i
  - use e.g. avg of their ratings to predict if alice likes i
  - repeat for all items unseen by the user and recommend the best-rated

To solve:
 1. how to measure similarity of users
    - Pearson correlation (Jaccard similarity, cosine similarity)
      - It takes into account the biases
      - Assumption: what is rated below average was disliked
 2. how many neighbors should be considered
    - can be trained
      - only some degree of similarity wanted etc
 3. how to generate the rating of the item from other users
    - average + weight based deviation

![userknn](notes-img/userknn.png)

User-based KNN is said to be "memory-based"
 - the rating matrix is directly used to find neighbors / make predictions
   - Everything is calculated at the time of the request
 - does not scale for most real-world scenarios
 - large e-commerce sites / social networks have tens of millions of customers and millions of items

Model-based approaches
 - based on an offline pre-processing or "model-learning" phase
   - Represent users and/or items as a set of features, which are easy to operate with
 - at run-time, only the learned model is used to make predictions
 - models are updated / re-trained periodically
 - large variety of techniques used
 - model-building and updating can be computationally expensive
 - **item-based KNN is an example for model-based approaches**

### Item-based nearest-neighbor collaborative filtering

Similarity between items, not users, to make predictions

> Out of the items rated by user A, we find the most similar items to item i based on rating by other users, and make the prediction based on the rating given by the user A to those similar items.

(Inverse of User based)

Advantage:
 - based on the ratio of users to items and stability of those vectors


 - Pre-processing approach by Amazon.com (in 2003)
 - Calculate all pair-wise item similarities in advance

![itembasedknnalg](notes-img/itembasedknnalg.png)

#### Pros:

 - well-understood, works well in some domains, no knowledge engineering required

#### Cons:
 - requires user community, sparsity problems, no integration of other knowledge sources, no explanation of results


#### Questions:

1. What is the best CF method?
   - In which situation and which domain? Inconsistent findings; always the same domains and data sets; differences between methods are often very small (1/100)
2. How to evaluate the prediction quality?
   - separate lecture on this – gets even more important nowdays
   - MAE / RMSE: What does an MAE of 0.7 actually mean?
   - Serendipity (novelty and surprising effect of recommendations)
     - Not yet fully understood (still true)
4. What about multi-dimensional ratings?
   - not many application domains - instead, what about implicit feedback

### Other approaches

 - Recurrsive CF
 - Graph based
   - Spreading activation
 - Association rule mining
   - Market Basket Analysis
 - Probabilistic methods

## Ratings

### Explicit ratings

 - **Probably the most precise ratings** (ehm... Attribute ratings, reviews, detailed implicit feedback nowadays...)
 - Most commonly used (1 to 5, 1 to 7 Likert response scales, likes/dislikes)
 - Research topics
   - Optimal granularity of scale; indication that 10-point scale is better accepted in movie dom.
     - Different domains addopted other common scales
   - Multidimensional ratings (multiple ratings per movie such as ratings for actors and sound)
     - Booking.com rating

Main problems
 - **Users not always willing to rate many items**
   - number of available ratings could be too small → sparse rating matrices → poor recommendation quality
 - How to stimulate users to rate more items?
 - What else to use?

### Implicit ratings (feedback)

 - Typically collected by the web shop or application in which the recommender system is embedded
 - When a customer buys an item, for instance, many recommender systems interpret this behavior as a positive rating
 - Clicks, page views, time spent on some page, demo downloads …
 - Implicit ratings can be collected constantly and do not require additional efforts from the side of the user
 - Implicit ratings can be used in addition to explicit ones; question of correctness of interpretation

Main problem
 - One cannot be sure whether the user **behavior** is **correctly interpreted**
 - For example, a user might not like all the books he or she has bought; the user also might have bought a book for someone else


## Cold start problem (Data sparsity problems)

How to recommend **new items** (invisible)? What to recommend to **new users** (nothing to recommend to him)?

### Straightforward approaches
 - Ask/force users to rate a set of items
   - they will hate you
 - Use another method (e.g., content-based, demographic or simply non-personalized) in the initial phase
   - bias problems from "previous versions", but generally OK
 - Default voting: assign default values to items that only one of the two users to be compared has rated (Breese et al. 1998)
   - ... And the performance is...

### Alternatives
 - Use better algorithms (beyond nearest-neighbor approaches)
 - Example:
   - In nearest-neighbor approaches, the set of sufficiently similar neighbors might be too small to make good predictions
   - Assume "transitivity" of neighborhoods

### Problem illustration

![coldstart](notes-img/coldstart.png)

## Matrix completion (factorization)

