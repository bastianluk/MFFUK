# Výroková a predikátová logika
---
---
---

# Algoritmy pro:

## 2-SAT 

- Výrok je v k-CNF, je-li v CNF a každá jeho klauzule má nejvýše k literálu˚. 
- k-SAT je následující problém (pro pevné k > 0)
  - INSTANCE: Výrok ϕ v k-CNF.
  - OTÁZKA: Je ϕ splnitelný?

2-SAT lze ˇrešit v lineárním ˇcase (vzhledem k délce ϕ).

Tvrzení
Rozklad orientovaného grafu (V,E) na silnˇe souvislé komponenty lze nalézt v ˇcaseO(|V|+|E|).
- Orientovaný graf G je silnˇe souvislý, pokud pro každé dva vrcholy u a v existují v G orientované cesty jak z u do v, tak i z v do u.
- Silnˇe souvislá komponenta grafu G je maximální silnˇe souvislý podgraf G.

Implikaˇcní graf výroku ϕ v 2-CNF je orientovaný graf Gϕ, v nˇemž vrcholy jsou promˇenné výroku ϕ nebo jejich negace, klauzuli l1 ∨l2 výroku ϕ reprezentujeme dvojicí hran l1 →l2, l2 →l1, klauzuli l1 výroku ϕ reprezentujeme hranou l1 →l1.

Tvrzení ϕ je splnitelný, právˇe když žádná silnˇe souvislá komponenta v Gϕ neobsahuje dvojici opaˇcných literálu˚. Du˚kaz Každé splˇnující ohodnocení ohodnotí všechny literály ze stejné komponenty stejnˇe. Implikace zleva doprava tedy platí.

Naopak, oznaˇcme G∗ ϕ graf vzniklý z Gϕ kontrakcí silnˇe souvislých komponent. 

Pozorování G∗ ϕ je acyklický, má tedy topologické uspoˇrádání <.
- Orientovaný graf je acyklický, neobsahuje-li orientovaný cyklus.
- Lineární uspoˇrádání < vrcholu˚ orientovaného grafu je topologické, pokud p < q pro každou hranu z p do q.

Nyní pro každou komponentu v rostoucím poˇradí dle <, nejsou-li její literály dosud ohodnocené, nastav je na 0 a literály v opaˇcné komponentˇe na 1.

Zbývá ukázat, že takto získané ohodnocení v splˇnuje ϕ. Kdyby ne, existovaly by v G∗ ϕ hrany p →q a q →p s v(p) = 1 a v(q) = 0. To je ve sporu s poˇradím nastavení komponent na 0 resp. 1, nebot’ p < q a q < p.
Du˚sledek 2-SAT je ˇrešitelný v lineárním ˇcase.


## Horn-SAT (dukaz korektnosti).

Horn-SAT
- Jednotková klauzule je klauzule obsahující jediný literál,
- Hornova klauzule je klauzule obsahující nejvýše jeden pozitivní literál,
¬p1 ∨···∨¬pn ∨q ∼ (p1 ∧···∧pn) →q
- Hornu˚v výrok je konjunkcí Hornových klauzulí,
- Horn-SAT je problém splnitelnosti daného Hornova výroku.

Algoritmus
(1) obsahuje-li ϕ dvojici jednotkových klauzulí l a l, není splnitelný,
(2) obsahuje-li ϕ jednotkovou klauzuli l, nastav l na 1, odstraˇn všechny klauzule obsahující l, odstraˇ n l ze všech klauzulí a opakuj od zaˇcátku,
(3) neobsahuje-li ϕ jednotkovou klauzuli, je splnitelný ohodnocením 0 všech zbývajících promˇenných.

Krok (2) se nazývá jednotková propagace.

Jednotková propagace
Pozorování
Necht’ ϕl je výrok získaný z ϕ jednotkovou propagací. Pak ϕl je splnitelný, právˇe když ϕ je splnitelný.

Du˚sledek
Algoritmus je korektní (ˇreší Horn-SAT).

Du˚kaz
Korektnost 1. kroku je zˇrejmá, v 2. kroku plyne z pozorování, v 3.kroku díky Hornovˇe tvaru, nebot’ každá zbývající klauzule obsahuje negativní literál.

Poznámka
Pˇrímoˇcará implementace vyžaduje kvadratický ˇcas, pˇri vhodné reprezentaci v pamˇeti lze dosáhnout lineárního ˇcasu (vzhledem k délce ϕ).


---
# Tablo metoda ve VL:

## syst. tablo

### (dokoncenost, kon. dukazu)

Systematické tablo - dokonˇcenost
Tvrzení
Pro každou teorii T a položku R je systematické tablo τ dokonˇcené.

Du˚kaz
- Necht’ τ = ∪τn je systematické tablo z T = {ϕ0,ϕ1,...}s R v koˇreni.
- Je-li vˇetev v τ bezesporná, je i každý její preﬁx v τn bezesporný.
- Je-li položka P neredukovaná na vˇetvi v τ, je neredukovaná na každém jejím preﬁxu v τn (na nˇemž leží).
- Do úrovnˇe každé položky P (vˇcetnˇe její) je v τ jen koneˇcnˇe položek.
- Kdyby P byla neredukovaná na nˇejaké bezesporné vˇetvi τ, pˇrišla by na ní ˇrada v nˇejakém kroku (2) a byla by zredukována krokem (3).
- Každá ϕn ∈T bude dle (4) nejpozdˇeji v τn+1 na každé bezesporné vˇetvi.
- Tedy systematické tablo τ obsahuje pouze dokonˇcené vˇetve.

Koneˇcnost du˚kazu˚
Lemma (König)
Každý nekoneˇcný, koneˇcnˇe vˇetvící se strom obsahuje nekoneˇcnou vˇetev.

Tvrzení
Je-li τ = ∪τn sporné tablo, je τn sporné koneˇcné tablo pro nˇejaké n.

Du˚kaz
- Necht’ S je množina vrcholu˚ stromu τ, jenž nad sebou neobsahují spor, tj. mezi pˇredky nemají dvojici Tϕ, Fϕ pro žádné ϕ. 
- Kdyby S byla nekoneˇcná, dle Königova lemmatu by podstrom τ na vrcholech S obsahoval nekoneˇcnou vˇetev, tedy by τ nebylo sporné tablo.
- Jelikož je S koneˇcné, všechny vrcholy z S leží do úrovnˇ e m pro nˇejaké m. 
- Tedy každý vrchol v úrovni m + 1 má nad sebou spor. Zvolme n tak, že τn se shoduje s τ do úrovnˇ e m + 1. Pak každá vˇetev v τn je sporná. 
- 
Du˚sledek 
Je-li systematické tablo τ du˚kazem (z teorie T), je τ koneˇcné.
Du˚kaz
Pˇri jeho konstrukci se prodlužují jen bezesporné vˇetve.

### korektnost

Korektnost
ˇ Rekneme, že položka P se shoduje s ohodnocením v, pokud P je Tϕ a v(ϕ) = 1 nebo pokud P je Fϕ a v(ϕ) = 0. Vˇetev V tabla se shoduje s v, shoduje-li se s v každá položka na V.

Lemma
Necht’ v je model teorie T, který se shoduje s položkou v koˇreni tabla τ = ∪τn z T. Pak v tablu τ existuje vˇetev shodující se s v.

Du˚kaz Indukcí nalezneme posloupnost V0,V1,... takovou, že pro každé n je Vn vˇetev v τn shodující se s v a Vn je obsažena ve Vn+1.
- Ovˇ eˇrením atomických tabel snadno zjistíme, že základ indukce platí.
- Pokud τn+1 vznikne z τn bez prodloužení Vn, položme Vn+1 = Vn.
- Vznikne-li τn+1 z τn pˇripojením Tϕ k Vn pro nˇejaké ϕ ∈T, necht’ Vn+1 je tato vˇetev. Jelikož v je model ϕ, shoduje se Vn+1 s v.
- Jinak τn+1 vznikne z τn prodloužením Vn o atomické tablo nˇejaké položky P na Vn. Jelikož se P shoduje s v a tvrzení platí pro atomická tabla, lze požadovanou vˇetev Vn+1 v τn+1 nalézt.

Vˇeta
Pro každou teorii T a formuli ϕ, je-li ϕ tablo dokazatelná z T, je ϕ pravdivá v T, tj. T |- ϕ ⇒ T |= ϕ.

Du˚kaz
- Necht’ ϕ je tablo dokazatelná z teorie T, tj. existuje sporné tablo τ s položkou Fϕ v koˇreni.
- Pro spor pˇredpokládejme, že ϕ není pravdivá v T, tj. existuje model v teorie T, ve kterém ϕ neplatí (protipˇríklad).
- Jelikož se položka Fϕ shoduje s v, dle pˇredchozího lemmatu v tablu τ existuje vˇetev shodující se s v.
- To ale není možné, nebot’ každá vˇetev tabla τ je sporná, tj. obsahuje dvojici Tψ, Fψ pro nˇejaké ψ.

### úplnost

Ukážeme, že bezesporná vˇetev v dokonˇceném tablu poskytuje protipˇríklad. 
Lemma
Necht’ V je bezesporná vˇetev dokonˇceného tabla τ. Pro následující ohodnocení v výrokových promˇenných platí, že V se shoduje s v.
v(p) =(1 pokud se Tp vyskytuje na V 0 jinak

Du˚kaz Indukcí dle struktury formule v položce vyskytující se na V.
- Je-li položka Tp na V, kde p je prvovýrok, je v(p) = 1 dle deﬁnice v.
- Je-li položka Fp na V, není Tp na V, jinak by V byla sporná, tedy v(p) = 0 dle deﬁnice v.
- Je-li T(ϕ∧ψ) na V, je Tϕ a Tψ na V, nebot’ τ je dokonˇcené. Dle indukˇcního pˇredpokladu je v(ϕ) = v(ψ) = 1, tedy v(ϕ∧ψ) = 1.
- Je-li F(ϕ∧ψ) na V, je Fϕ nebo Fψ na V, nebot’ τ je dokonˇcené. Dle indukˇcního pˇredpokladu je v(ϕ) = 0 nebo v(ψ) = 0, tedy v(ϕ∧ψ) = 0.
- Pro ostatní spojky obdobnˇe jako v pˇredchozích dvou pˇrípadech.

Ukážeme, že tablo metoda ve výrokové logice je i úplná.

Vˇeta
Pro každou teorii T a formuli ϕ, je-li ϕ pravdivá v T, je ϕ tablo dokazatelná z T, tj. T |= ϕ ⇒ T |- ϕ.

Du˚kaz Necht’ ϕ je pravdivá v T. Ukážeme, že libovolné dokonˇcené tablo (napˇ r. systematické) τ z teorie T s položkou Fϕ v koˇreni je sporné.
- Kdyby ne, necht’ V je nˇejaká bezesporná vˇetev tabla τ.
- Dle pˇredchozího lemmatu existuje ohodnocení v prvovýroku˚ takové, že V se shoduje s v, speciálnˇe s Fϕ, tj. v(ϕ) = 0.
- Jelikož vˇetev V je dokonˇcená, obsahuje Tψ pro každé ψ ∈T.
- Tedy v je modelem teorie T (nebot’ vˇetev V se shoduje s v).
- To je ale ve sporu s tím, že ϕ platí v každém modelu teorie T.

Tedy tablo τ je du˚kazem ϕ z T.


---
# Veta o kompaktnosti VL

Vˇeta
Teorie má model, právˇe když každá její koneˇcná ˇcást má model.

Du˚kaz 1
Implikace zleva doprava je zˇrejmá. Pokud teorie T nemá model, je sporná, tj. je z ní dokazatelný⊥systematickým tablem τ. Jelikož je τ koneˇcné, je⊥dokazatelný z nˇejaké koneˇcné T0 ⊆T, tj. T0 nemá model.
Poznámka Tento du˚kaz je založen na koneˇcnosti du˚kazu, korektnosti a úplnosti. Uved’me ještˇe druhý, pˇrímý du˚kaz (pomocí Königova lemmatu).

Du˚kaz 2
Necht’ T = {ϕi |i ∈N}. Uvažme strom S na koneˇcných binárních posloupnostech σ uspoˇrádaných prodloužením. Pˇ riˇcemž σ ∈S, právˇe když existuje ohodnocení v prodlužující σ takové, že v |= ϕi pro každé i ≤lth(σ).

Pozorování
S má nekoneˇcnou vˇetev, právˇe když T má model. Jelikož{ϕi |i ∈n}⊆T má model pro každé n ∈N, bude každá úroveˇn v S neprázdná. Tedy S je nekoneˇcný, navíc binární, a dle Königova lemmatu obsahuje nekoneˇcnou vˇetev.

## její dusledky.

Graf (V,E) je k-obarvitelný, pokud existuje c: V →k takové, že c(u) 6= c(v) pro každou hranu{u,v}∈E.

Vˇeta
Spoˇcetnˇe nekoneˇcný graf G = (V,E) je k-obarvitelný, právˇe když každý jeho koneˇcný podgraf je k-obarvitelný.

Du˚kaz
Implikace zleva doprava je zˇrejmá. Necht’ každý koneˇcný podgraf v G je k-obarvitelný.
Vezmˇeme P = {pu,i |u ∈V,i ∈k}a teorii T s axiomy pu,0 ∨···∨pu,k−1 pro všechna u ∈V, ¬(pu,i ∧pu,j) pro všechna u ∈V,i < j < k, ¬(pu,i ∧pv,i) pro všechna{u,v}∈E,i < k.
Platí, že G je k-obarvitelný, právˇe když T má model. Dle vˇety o kompaktnosti staˇcí dokázat, že každá koneˇcná T0 ⊆T má model. Necht’ G0 je podgraf na vrcholech u takových, že pu,i se vyskytuje v T0 pro nˇejaké i. Jelikož G0 je k-obarvitelný dle pˇredpokladu, má T0 model.


---
# Rezoluce ve VL:

## korektnost

Vˇeta (korektnost)
Je-li S rezolucí zamítnutelná, je S nesplnitelná.

Du˚kaz
Necht’ S |-R . KdybyV |= S pro nˇejaké ohodnoceníV, z korektnosti rezoluˇcního pravidla by platilo iV |= , což není možné.

## úplnost

Vˇeta
Je-li koneˇcná S nesplnitelná, je rezolucí zamítnutelná, tj. S |-R .

Du˚kaz Indukcí dle poˇctu promˇenných v S ukážeme, že S |-R .
- Nemá-li nesplnitelná S žádnou promˇennou, je S = {}a tedy S |-R , 
- Necht’ l je literál vyskytující se v S. Dle lemmatu, Sl a Sl jsou nesplnitelné. 
- Jelikož Sl a Sl mají ménˇe promˇenných než S, dle indukˇcního pˇredpokladu existují rezoluˇcní stromy Tl a Tl pro odvození  z Sl resp. Sl.
- Je-li každý list Tl z S, je Tl rezoluˇcním stromem  z S, tj. S |-R .
- Pokud ne, doplnˇením literálu l do každého listu, jenž není z S, (a do všech vrcholu˚ nad ním) získáme rezoluˇcní strom{l}z S.
- Obdobnˇe získáme rezoluˇcní strom{l}z S doplnˇením l ve stromu Tl,
- Rezolucí jejich koˇrenu˚{l}a{l}získáme rezoluˇcní strom  z S. 

Du˚sledek 
Je-li S nesplnitelná, je rezolucí zamítnutelná, tj. S |-R .

Du˚kaz
Plyne z pˇredchozího užitím vˇety o kompaktnosti.

## LI-rezoluce (úplnost pro Horn. formule)

Rezoluˇcní metodu mu˚žeme znaˇcnˇe omezit (bez ztráty úplnosti).
- Lineární du˚kaz (rezolucí) klauzule C z formule S je koneˇcná posloupnost dvojic (C0,B0),...,(Cn,Bn) taková, že C0 ∈S a pro každé i ≤n i) Bi ∈S nebo Bi = Cj pro nˇejaké j < i, a ii) Ci+1 je rezolventa Ci a Bi, kde Cn+1 = C.
- C0 zveme poˇcáteˇcní klauzule, Ci centrální klauzule, Bi boˇcní klauzule.
- C je lineárnˇe dokazatelná z S, psáno S |-L C, má-li lineární du˚kaz z S. 
- Lineární zamítnutí S je lineární du˚kaz  z S.
- S je lineárnˇe zamítnutelná, pokud S |-L .

Pozorování
Je-li S lineárnˇe zamítnutelná, je S nesplnitelná.

Du˚kaz
Každý lineární du˚kaz lze transformovat na (korektní) rezoluˇcní du˚kaz. 

Poznámka
Platí i úplnost, tj. je-li S nesplnitelná, je S lineárnˇe zamítnutelná.

Vˇeta
Je-li Hornova T splnitelná a T ∪{G}nesplnitelná pro cíl G, lze  odvodit LI-rezolucí z T ∪{G}zaˇcínající G.

Du˚kaz Dle vˇety o kompaktnosti mu˚žeme pˇredpokládat, že T je koneˇcná.
- Postupujeme indukcí dle poˇctu promˇenných v T.
- Dle pozorování, T obsahuje fakt{p}pro nˇejakou promˇennou p.
- Dle lemmatu je T0 = (T ∪{G})p = Tp ∪{Gp}nesplnitelná, pˇ riˇcemž Gp = G\{p}. 
- Je-li Gp = , je G = {p}a tedy  je rezolventa G a{p}∈T.
- Jinak, jelikož Tp je splnitelná (stejným ohodnocením, které splˇnuje T) a má ménˇe promˇenných, dle indukˇcního pˇredpokladu lze  odvodit LI-rezolucí z T0 zaˇcínající Gp.
- Doplnˇením literálu p do všech listu˚, jež nejsou v T ∪{G}, a všech vrcholu˚ pod ním získáme LI-odvození{p}z T ∪{G}zaˇcínající v G.
- Závˇereˇcnou rezolucí pomocí faktu{p}∈T získáme .

---
# Sémantika PL:

## veta o konstantách

Vˇeta
Necht’ ϕ je formule jazyka L s volnými promˇennými x1,...,xn a T je teorie jazyka L. Oznaˇcme L0 rozšíˇrení L o nové konstantní symboly c1,...,cn a T0 teorii T nad jazykem L0. Pak
T |= ϕ právˇe když T0 |= ϕ(x1/c1,...,xn/cn). 

Du˚kaz
(⇒) Je-liA0 model teorie T0, necht’Aje reduktA0 na L. Jelikož A|= ϕ[e] pro každé ohodnocení e, platí i 
A|= ϕ[e(x1/cA0 1 ,...,xn/cA0 n )], tj. A0 |= ϕ(x1/c1,...,xn/cn). 
(⇐) Je-liAmodel teorie T a e ohodnocení, necht’A0 je expanzeAna L0 o konstanty cA0 i = e(xi) pro všechna i. JelikožA0 |= ϕ(x1/c1,...,xn/cn)[e0] pro libovolné ohodnocení e0, platí i 
A0 |= ϕ[e(x1/cA0 1 ,...,xn/cA0 n )], tj. A|= ϕ[e].

## vlastnosti otevrených teorií  !!!



## Veta o dedukci  !!!



---
# Tablo metoda v PL:  !!!

## syst. tablo

### (dokon., kon. dukazu)



## význam axiomu rovnosti


---
# Tablo metoda v PL:

### korektnost



## kanonický model



### (s rovností)



### úplnost



---
# Löwenheim-Skolemova veta.

Vˇeta
Každá bezesporná teorie T spoˇcetného jazyka L bez rovnosti má spoˇcetnˇe nekoneˇcný model.

Du˚kaz
Necht’ τ je systematické tablo z T s F⊥v koˇreni. Jelikož je dokonˇcené a obsahuje bezespornou vˇetev V, nebot’⊥není dokazatelný z T, existuje kanonický modelAz V. Jelikož seAshoduje s V, jeho redukt na jazyk L je hledaným spoˇcetnˇe nekoneˇcným modelem T. 

# Veta o kompaktnosti PL

Teorie T0 jazyka L0 je extenze teorie T jazyka L o deﬁnice, pokud vznikla z T postupnou extenzí o deﬁnici relaˇcního ˇci funkˇcního symbolu.

Du˚sledek
Necht’ T0 je extenze teorie T o deﬁnice. Pak 
každý model teorie T lze jednoznaˇcnˇ e expandovat na model T0,
T0 je konzervativní extenze T,
pro každou formuli ϕ0 nad L0 existuje ϕ nad L taková, že T0 |= ϕ0 ↔ ϕ.

Napˇr. v teorii T = {(∃y)(x + y = 0),(x + y = 0)∧(x + z = 0) →y = z}nad L = h+,0,≤is rovností lze zavést < a unární funkˇcní symbol−axiomy
−x = y ↔ x + y = 0 
x < y ↔ x ≤y ∧ ¬(x = y)
Pak formule−x < y je v této extenzi o deﬁnice ekvivalentní formuli
(∃z)((z ≤y ∧ ¬(z = y)) ∧ x + z = 0).

## a její dusledky.  !!!



---
# Extenze o definice,

Tvrzení
Pro každou formuli ϕ0 nad L0 existuje ϕ nad L, t.ž. T0 |= ϕ0 ↔ ϕ. 

Du˚kaz 
Každou podformuli R(t1,...,tn) nahradíme za ψ0(x1/t1,...,xn/tn), kde ψ0 je vhodná varianta ψ zaruˇcující substituovatelnost všech termu˚

Tvrzení
Pro každou formuli ϕ0 nad L0 existuje ϕ nad L, t.ž. T0 |= ϕ0 ↔ ϕ.

Du˚kaz
Staˇcí uvážit ϕ0 s jediným výskytem f. Má-li ϕ0 více výskytu˚ f, lze postup aplikovat induktivnˇe (v pˇrípadˇe vnoˇrených výskytu˚ jdeme od vnitˇrních k vnˇejším). Oznaˇcme ϕ∗ formuli vzniklou z ϕ0 nahrazením termu f (t1,...,tn) za novou promˇennou z. Za ϕ vezmeme formuli 
(∃z)(ϕ∗ ∧ ψ0(x1/t1,...,xn/tn,y/z)),
kde ψ0 je vhodná varianta ψ zaruˇcující substituovatelnost všech termu˚. 
Necht’Aje model T0, e je ohodnocení, a = f A(t1,...,tn)[e]. Díky obˇema podmínkám platíA|= ψ0(x1/t1,...,xn/tn,y/z)[e] právˇe když e(z) = a. Tedy 
A|= ϕ[e] ⇔ A|= ϕ∗[e(z/a)] ⇔ A|= ϕ0[e] 
pro každé ohodnocení e, tj.A|= ϕ0 ↔ ϕ a tedy T0 |= ϕ0 ↔ ϕ.

# Skolemova veta

Vˇeta
Každá teorie T má otevˇrenou konzervativní extenzi T∗.

Du˚kaz Lze pˇredpokládat, že T je v uzavˇreném tvaru. Necht’ L je její jazyk. 
Nahrazením každého axiomu teorie T za ekvivalentní formuli v prenexním tvaru získáme ekvivalentní teorii T◦.
Nahrazením každého axiomu teorie T◦ za jeho Skolemovu variantu získáme teorii T0 rozšíˇreného jazyka L0. 
Jelikož je redukt každého modelu teorie T0 na jazyk L modelem teorie T, je T0 extenze T. 
Jelikož i každý model teorie T lze expandovat na model teorie T0, je to extenze konzervativní. 
Jelikož každý axiom teorie T0 je univerzální sentence, jejich nahrazením za otevˇrená jádra získáme otevˇrenou teorii T∗ ekvivalentní s T0.

Du˚sledek
Ke každé teorii existuje ekvisplnitelná otevˇrená teorie.

# Herbrandova veta.

Vˇeta
Necht’ T je otevˇrená teorie jazyka L bez rovnosti a s alespoˇn jedním konstantním symbolem. Pak
(a) T má Herbrandu˚v model, anebo 
(b) existuje koneˇcnˇe mnoho základních instancí axiomu˚ z T, jejichž konjunkce je nesplnitelná, a tedy T nemá model.

Du˚kaz
Necht’ T0 je množina všech základních instancí axiomu˚ z T. Uvažme dokonˇcené (napˇr. systematické) tablo τ z T0 v jazyce L (bez pˇridávání nových konstant) s položkou F⊥v koˇreni.
Obsahuje-li tablo τ bezespornou vˇetev V, kanonický model z vˇetve V je Herbrandovým modelem teorie T.
Jinak je τ sporné, tj. T0 |-⊥. Navíc je koneˇcné, tedy⊥je dokazatelný jen z koneˇcnˇe mnoha formulí T0, tj. jejich konjunkce je nesplnitelná. 

---
# Rezoluce v PL:

## korektnost

Nejprve ukážeme, že obecné rezoluˇcní pravidlo je korektní.

Tvrzení
Necht’ C je rezolventa klauzulí C1, C2. Pro každou L-strukturuA, A|= C1 a A|= C2 ⇒ A|= C.

Du˚kaz
Necht’ C1 = C0 1 t{A1,...,An}, C2 = C0 2 t{¬B1,...,¬Bm}, σ je nejobecnˇejší uniﬁkace pro S = {A1,...,An,B1,...,Bm}a C = C0 1σ∪C0 2σ.
Jelikož C1, C2 jsou otevˇrené, platí iA|= C1σ aA|= C2σ. 
Máme C1σ = C0 1σ∪{Sσ} a C2σ = C0 2σ∪{¬(Sσ)}.
Ukážeme, žeA|= C[e] pro každé e. Je-liA|= Sσ[e], pakA|= C0 2σ[e] a tedyA|= C[e]. JinakA6|= Sσ[e], pakA|= C0 1σ[e] a tedyA|= C[e].

Vˇeta (korektnost)
Je-li formule S rezolucí zamítnutelná, je S nesplnitelná.

Du˚kaz
Necht’ S |-R . KdybyA|= S pro nˇejakou strukturuA, z korektnosti rezoluˇcního pravidla by platilo iA|= , což není možné.

## úplnost

Lifting lemma
Rezoluˇcní du˚kaz na úrovni VL lze “zdvihnout” na úroveˇn PL.

Lemma
Necht’ C∗ 1 = C1τ1, C∗ 2 = C2τ2 jsou základní instance klauzulí C1, C2 neobsahující stejnou promˇennou a C∗ je rezolventa C∗ 1 a C∗ 2. Pak existuje rezolventa C klauzulí C1 a C2 taková, že C∗ = Cτ1τ2 je základní instance C. 

Du˚kaz
Pˇredpokládejme, že C∗ je rezolventa C∗ 1, C∗ 2 pˇres literál P(t1,...,tk).
Pak lze psát C1 = C0 1 t{A1,...,An}a C2 = C0 2 t{¬B1,...,¬Bm}, kde {A1,...,An}τ1 = {P(t1,...,tk)}a{¬B1,...,¬Bm}τ2 = {¬P(t1,...,tk)}.
Tedy (τ1τ2) uniﬁkuje S = {A1,...,An,B1,...,Bm}a je-li σ mgu pro S z uniﬁkaˇcního algoritmu, pak C = C0 1σ∪C0 2σ je rezolventa C1 a C2.
Navíc (τ1τ2) = σ(τ1τ2) z vlastnosti (∗) pro σ a tedy Cτ1τ2 = (C0 1σ∪C0 2σ)τ1τ2 = C0 1στ1τ2 ∪C0 2στ1τ2 = C0 1τ1 ∪C0 2τ2 = (C1 \{A1,...,An})τ1 ∪(C2 \{¬B1,...,¬Bm})τ2 = (C∗ 1 \{P(t1,...,tk)})∪(C∗ 2 \{¬P(t1,...,tk)}) = C∗.

Du˚sledek
Necht’ S0 je množina všech základních instancí klauzulí formule S. Je-li S0 |-R C0 (na úrovni VL), kde C0 je základní klauzule, pak existuje klauzule C a základní substituce σ t.ž. C0 = Cσ a S |-R C (na úrovni PL). Du˚kaz Indukcí dle délky rezoluˇcního odvození pomocí lifting lemmatu.

Vˇeta (úplnost)
Je-li formule S nesplnitelná, je S |-R .

Du˚kaz
Je-li S nesplnitelná, dle (du˚sledku) Herbrandovy vˇety je nesplnitelná i množina S0 všech základních instancí klauzulí z S. 
Dle úplnosti rezoluˇcní metody ve VL je S0 |-R  (na úrovni VL).
Dle pˇredchozího du˚sledku existuje klauzule C a substituce σ taková, že  = Cσ a S |-R C (na úrovni PL). 
Jediná klauzule, jejíž instance je , je klauzule C = .

##  LI-rezoluce

Vˇeta
S je lineárnˇe zamítnutelná, právˇe když S je nesplnitelná.
Du˚kaz 
(⇒) Každý lineární du˚kaz lze transformovat na rezoluˇcní du˚kaz. 
(⇐) Plyne z úplnosti lineární rezoluce ve VL (nedokazováno), nebot’ lifting lemma zachovává linearitu odvození.

Vˇeta
Je-li Hornova T splnitelná a T ∪{G}nesplnitelná pro cíl G, lze  odvodit LI-rezolucí z T ∪{G}zaˇcínající G.

Du˚kaz
Plyne z Herbrandovy vˇety, stejné vˇety ve VL a lifting lemmatu.

---
# Elementární ekvivalence !!!



# dusledky L.-S. vety

Vˇeta
Necht’ T je bezesporná teorie spoˇcetného jazyka L. Je-li L bez rovnosti, má T model, který je spoˇcetnˇe nekoneˇcný. Je-li L s rovností, má T model, který je spoˇcetný.

Du˚sledek
Ke každé struktuˇreAspoˇcetného jazyka bez rovnosti existuje spoˇcetnˇe nekoneˇcná elementárnˇe ekvivalentní strukturaB.

Du˚kaz
Teorie Th(A) je bezesporná, nebot’ má modelA. Dle pˇredchozí vˇety má spoˇcetnˇe nek. modelB. Jelikož je teorie Th(A) kompletní, jeA≡B.

Du˚sledek
Ke každé nekoneˇcné struktuˇreAspoˇcetného jazyka s rovností existuje spoˇcetnˇe nekoneˇcná elementárnˇe ekvivalentní strukturaB.

Du˚kaz 
Obdobnˇe jako výše. Jelikož vAneplatí sentence “existuje právˇ e n prvku˚” pro žádné n ∈N aA≡B, není B koneˇcná, tedy je nekoneˇcná.

---
# ω-kategoricnost

Teorie T je ω-kategorická, pokud má až na izomorﬁsmus právˇe jeden model kardinality ω, tj. spoˇcetnˇe nekoneˇcný.

Tvrzení
Teorie DeLO (tj. “bez koncu˚”) je ω-kategorická.

Du˚kaz
Necht’A,B|= DeLO s A = {ai}i∈N, B = {bi}i∈N. Indukcí dle n lze nalézt prosté parciální funkce hn ⊆hn+1 ⊂A×B zachovávající uspoˇrádání tak, že{ai}i<n ⊆dom(hn) a{bi}i<n ⊆rng(hn). PakA'B via h = ∪hn. 

Vˇeta
Necht’ jazyk L je spoˇcetný. 
(i) Je-li teorie T jazyka L bez rovnosti ω-kategorická, je kompletní. 
(ii) Je-li teorie T jazyka L s rovností ω-kategorická a bez koneˇcného modelu, je kompletní.

Du˚kaz
Každý model teorie T je elementárnˇe ekvivalentní s nˇejakým spoˇcetnˇe nekoneˇcným modelem T, ale ten je až na izomorﬁsmus jediný. Tedy všechny modely T jsou elementárnˇe ekvivalentní, tj. T je kompletní.

# podmínky pro konecnou a otevrenou axiomatizovatelnost.

Vˇeta
Necht’ K ⊆M(L) a K = M(L)\K, kde L je jazyk. Pak K je koneˇcnˇ e axiomatizovatelná, právˇe když K i K jsou axiomatizovatelné.

Du˚kaz
(⇒) Je-li T koneˇcná axiomatizace K v uzavˇreném tvaru, pak teorie s jediným axiomemWϕ∈T ¬ϕ axiomatizuje K.
(⇐). Necht’ T, S jsou teorie jazyka L takové, že M(T) = K, M(S) = K. Pak M(T ∪S) = M(T)∩M(S) = ∅ a dle vˇety o kompaktnosti existují koneˇcné T0 ⊆T a S0 ⊆S takové, že ∅ = M(T0∪S0) = M(T0)∩M(S0). Jelikož M(T) ⊆M(T0) ⊆M(S0) ⊆M(S) = M(T), je M(T) = M(T0), tj. koneˇcná T0 axiomatizuje K.

Vˇeta
Je-li teorie T otevˇrenˇe axiomatizovatelná, pak každá podstruktura modelu T je rovnˇež modelem T. 

Du˚kaz
Necht’ T0 je otevˇrená axiomatika M(T),A|= T0 aB⊆A. Víme, že pro každé ϕ ∈T0 jeB|= ϕ, nebot’ ϕ je otevˇrená. TedyB je modelem T0. 

---