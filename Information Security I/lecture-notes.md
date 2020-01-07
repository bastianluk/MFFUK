# Ochrana informací

## 1. Přednáška

Pepa

## 2. Přednáška

Pepa

## 3. Přednáška

Zminka o Orange Book-u
Ocekavani uzivatele - aby to fungovalo a stale stejne
ITSEC
  - uplne oprosteni se od premysleni orange booku


  - Encryption
  - Assurance
  - Integrity
    - vsechno pojmy spojene s prochazenim cesty dat a analyze per part of a chain

Sidetrack o katastralnim urade - lifespan certifikátů a dat na USB atd.

Kdo rychleji strili VS starost o to, jak prenaset a uchovavat choulostiva data rozhodujici o podstatnych vecech

BS7799 (Britska norma) >> ISO 17799 (Mezinarodni) (od te i ceska ekvivalence)>> ISO 2700x (soubor norem... zbytecne dlouhy, plno vtipu)

Normy i na tloustku zdi, vysku plotu apod... preskocime a jdeme na kryptografii.

## 4. Přednáška

Povídání o modelech - hlavně jde o to, aby si nemohl někdo sám sobě vydávat věci a být tím pádem sám sobě kontrolou.

### Jednoúrovňové

#### Monitor model

TBD

#### Information flow model

TBD


#### Clark-Wilson model

TBD

#### Chinese wall model

Neradit cizí čokoládovně. :D 

#### Graham-Denning model & Take-Grant system

"Nebudu se jimi zabírat - podívejte se na ně sami."
Ovládání a návrh řízení přístupu
Hlavně pozor na tranzitivitu a jiné - ať je transparentní, kdo má jaká privilegia

### Víceúrovňové

#### Military security model

KVP (stupeň utajení, oblast) - smím ze nějakého stroje s nějakým loginem přistoupit k těm a těm datům?

#### Svazový model

Částečné uspořádáním

#### Bell-La-Padula model (B-L model)

TBD
viz níže ale komunikace

#### Biba model

Zachování integrity informací, hierarchická klasifikace informací 

---

přeskočil povídání o bezpečnostní politice

---

### Autentizace (page 33)

Je třeba vědět, koho mám identifikovat ~ s kým/čím pracuju

## 5. Přednáška 

### Autentizace (page 33)

autentizace - kdo to je
autorizace - co může dělat

#### Hesla

zasadni problem s zapamatovanim si dostatecne bezpecnych hesel >>> psani na papir a potencialni point of breach

obcas problem s "invalid password" v loggách

#### Passphrase



#### Skupinová hesla

obvious problem

#### PIN

obvykle ve striktnim provedeni "3 pokusy a dost"

False positives VS time to unlock


## 6. Přednáška 

### Autorizace (page 46)

#### Systém rolí

##### Technické role
 - další třeba spojení systému "židlí" (se samotnou hierarchií) a práv, spolu s člověkem  
 

 - pomocí tohohle se řeší časové problémy ("role pouze do června"), nebo by extension i personální změny

##### Business role
 - hierarchie technických rolí >>> agregace různých informačních systémů (user friendly assignemnt of roles)

umožní to i lidem, kteří rozhodují o rolích, ale neumí s informačními systémy, aby přiřadil patřičná oprávnění


### Metody fyzické ochrany

- věci jako ochrana v podobě plotů (detekční bariéra, zpomalovací bariéra, dlouho nic/pole, budova), ochranky, pak trezorů atd...
- ochrana před povodněmi (obecně vodou - prasklá klimatizace na MFF) atd...
- požary v servrovně
potřebná součást plánu ochrany
- ztráta kvality napájení
ztráta proudu, nebo přepětí (blesk)
- chlazení
- IPXX rating (dust and water resistance)

### Prostorová ochrana

- "pan Lopata"
- blesk jinde ale svod na smolné místo

pretty straight forward věci

## 7. Přednáška 

#### Fyzicke zabezpeceni

"Nekdo" zodopovedny za

- decommision of devices
- mazani nebo obfuskace dat na disku, ktereho se chci zbavit...

#### Vyzařování

- nejenom radiove vlny
- treba i viditelnost displeje

#### Obnova provozu

- sustainability without user input for "extended" periods of time
- zálohy A PŘÍSTUP k nim

##### Záložní média

- mít je a mít je pokud možno fyzicky jinde, než jejich zdroj
- pravidelně dělané

RTO recovery time objective
RPO recovery point objective

- i moc lehce dostupne zalohy jsou blbe - lide potom zkousi blbosti, protoze maji lehky zpusob obnovy

## 8.  Přednáška

--missing--

## 9.  Přednáška

--missing--

- Operační systémy

## 10. Přednáška

- závěr operačních systémů
- integrita databaze
- SŘBD - systém řízení báze dat ~ "engine"

## 11. Přednáška

### Bezpečnost počítačových sítí (page 91)


