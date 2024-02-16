Ce dépôt contient un compilateur et un interpréteur du langage textuel sous
forme de bibliothèque.

[[_TOC_]]

# Installation

Les paquets peuvent être installé en utilisant `opam` :

`opam install .` pour le switch courant.

La compilation est faite avec `dune` :

`dune build`

# Architecture

Le logiciel est organisé en plusieurs modules correspondant grossièrement aux
étapes du pipeline de compilation et ses représentations intermédiaires :
- Le parser, dans `src/grammar/`, qui produit un AST
- L'AST, dans `src/surface/`, la première représentation de programme. Elle
  contient une version syntaxique et une autre avec une représentation interne
  des variables.
- Une passe de résolution de contexte, dans `src/compiler/contextualize.ml`, qui
  transforme la représentation syntaxique de l'AST en son alternative avec
  variables. Et calcule la forme définitive des contextes de valeurs.
- Une passe de transformation de l'AST contextualisé en la représentation
  intermédiaire avec des arbres de redistributions, dans
  `src/compiler/ast_to_ir.ml`.
- La représentation intermédiaire `IR`, dans `src/internal/`.
- Une passe de calcul de seuils pour les conditions de bascule d'événement, dans
  `src/compiler/conditionLifting.ml`.
- L'interpréteur, dans `src/interpreter/`, qui s'appuie sur la représentation
  `IR`.

# Utilisation

## Compilateur

Le compilateur s'utilise en deux étapes :
- Le parsing du programme donné, par les fonctions `ParserMain.parse_program`,
  `ParserMain.parse_string`, ou `ParserMain.parse_gen`, du module `Grammar`,
  selon la forme du programme d'entrée. Ces fonctions produisent un AST.
- Cet AST est ensuite fourni à la fonction `Compile.compile` du module
  `Compiler`, qui produit un IR avec seuils calculés.

## Interpréteur

L'interpréteur s'utilise en fournissant l'IR en sortie de compilation et un set
d'entrée de calcul à la fonction `Execution.compute_input_lines`. Ce set
d'entrée est définit par le type `Execution.computation_inputs`, qui est un
ensemble de valorisations numéroté (pour l'ordre d'évaluation) de variables
d'entrée.

L'exécution de l'interpréteur produit un ensemble de valorisation (numéroté
conformément aux entrées) de tous les éléments de la cascade, à chaque étape de
calcul.

## Description de programme

Des informations sur le programme compilés sont accessible aux travers du type
`Interface.program_desc` du module `Internal`. Utile notamment pour obtenir une
version plus accessible des informations sur les variables, pour l'interface
utilisateur autour de l'interpreteur.

# Description générale du langage

This is a declarative language that aims to be concise and to moslty mimic how
legal contracts specify revenue redistribution. It is meant to describe the flow
of revenue such as to facilitate the generation of comprehensive tally counts
and to be able to visualize this flow more easily.

## The cascade

The cascade (also refered as program) is the general object that describe how to
redistribute given revenue to relevant actors.

The main (programing) objects of the language are:
- [Inputs](#input). Values which are basis to a computation of the
  redistribution, such as raw tally count.
- [Actors](#actor). The various entities involved in the contracts. From/to
  which the money flows.
- [Operations](#operation). Actual description of a part of the redistribution.
- [Events](#event). Trigger points, such as specific dates, or when past
  redistribution reach a tipping point.

They will be the most widely used on concrete code and should be enough to
implement any complete redistribution. All of those will be explicitly declared
in the code.

## Cascade computation

While the cascade in itself is a descriptive object, it contains enough
information to run computations of how money flows in it, and therefore has a
model of computation.

The details of this model will be exposed along the descriptions of the various
constructs of the language. An overview of the way it works:
- The cascade has a state that contains informations about past flows of money
  and various occuring events.
- Upon receiving inputs (entries related to revenue, e.g. sales figures), a step
  of execution (a tick) will be triggered that will update the state with new
  informations (new lines in tally counts, updated redistribution figures, etc).
- The state keeps informations about the flow of money but do not hold any
  between ticks. *At each tick, all the money flowing in the cascade will be
  assigned to the receiving actors.*

This means that some things in the cascade, in particular flows of money, will
have two distinct views: one is the cumulation throughout the computation of
successives inputs, the other the "delta" between one step and the previous one.
It is important to keep in mind the distinction between cumulated values of
flows and tick-related deltas, e.g. the sum of money seen through a flow since
the beginning of time, and a quantity of money added at a tick respectively.

# Le langage en détails

## Entrées

Les entrées sont les valeurs que l'utilisateur fournira pour un calcul de
répartition ou un changement d'état de la cascade. Ce sont des valeurs
cumulatives. Elles sont déclarées avec un identifiant et un type de donnée.

```niagara
entree entreesSalles type entier
```

L'exemple ci-dessus est une déclaration avec le mot-clé `entree` d'une valeur
nommée `entreeSalles` (identifiant commençant avec une minuscule) de type
entier.

### Types d'entrées

Il existe plusieurs types de données distincts pouvant être associés dans un
cadre précis (cf. [les formules](#formula)). Ces types sont :
- `entier`, arbitrairement grand.
- `rationnel`, arbitrairement grand et précis, notamment utilisé pour les
  pourcentages.
- `argent`, une représentation finie, bornée au centime près.

### Assiettes d'entrée

Une autre forme d'entrée est l'assiette de distribution :

```niagara
entree assiette rbd
```

Cet exemple déclare l'entrée `rbd`, une somme d'argent (indiqué par le mot-clé
`assiette`) qui sera redistribué dans la cascade. Cette déclaration est
différente d'une avec `type argent`, cette dernière est une valeur indicative
qui ne sera pas redistribuée.

Plus de détails sur les assiettes en [section dédiée](#pools).

## Partenaires

Un partenaire est une entité liée à la cascade dont le rôle est de cumuler des
droits sur les revenus de la cascade (ainsi que des dûs). Cela correspond
principalement en un mandataire de contrat.

```niagara
acteur prod
```

Cet exemple déclare un partenaire nommé `prod` avec le mot-clé `acteur`.

Un partenaire est à la fois un ayant-droit et un débiteur selon les besoins. Ce
sont également des valeurs cumulatives. La prochaine section traitera de leurs
interactions avec la cascade.

## Opérations

Une opération est une construction qui décrit une partie du flot de répartition.
Il spécifie d'où part l'argent, où il arrive et en quel quantité. Exemple :

```niagara
operation 'op simple'
sur assiette rbd
quotepart 20% vers prod
```

L'opération est associée à une étiquette entre guillemets simples. Ici, le
transfert se fait depuis l'assiette `rdb` (spécifié par le mot clé `sur`), vers
le partenaire `prod` (spécifié par `vers`). Le montant est donnée ici comme une
part de l'assiette source avec le mot-clé `quotepart` suivi d'une valeur
rationnel, ici `20%`.

Ce code peut se lire comme "L'opération 'op simple' attribut 20% de l'assiette
`rbd` au partenaire `prod`".

Il existe un autre opérateur de transfert : `bonus`. Tandis que `quotepart` a
pour but de définir une répartition fractionnaire d'une assiette, `bonus` permet
de définir le transfert d'une somme fixe, le plus souvent sous réserve de
conditions spécifiques.

```niagara
operation 'transfert fixe'
par prod
bonus 5000$ vers scenariste
```

Cet exemple déclare que le partenaire (déclaré a priori) `scenariste` se voit
donné une somme de `5000$` par le partenaire `prod`. L'opérateur attend une
valeur monétaire uniquement.

Notez l'apparition d'un nouveau mot-clé dans cet exemple : `par`. Tout comme
`sur` est utilisé pour spécifié une assiette en tant que source de l'opération,
`par` signifie que cette source est un partenaire débiteur.

Plusieurs opérateurs peuvent être fournis dans une même opération :

```niagara
operation 'more parties'
sur assiette rbd
quotepart 20% vers prod
quotepart 10% vers sofica1
quotepart 5% vers sofica2
```

Cette opération a plusieurs destinations de transfert, et définit la répartition
d'un total de 35% de l'assiette `rbd` entre elles.

### Destinations

Dans les exemples jusqu'ici, la destination d'un transfert était associé
syntaxiquement à chaque opérateur présent. Il est également possible de
spécifier une destination par défaut pour tous les opérateurs :

```niagara
operation 'op simple 2' vers prod
sur assiette rbd
quotepart 20%
```

Cet exemple est strictement équivalent à l'opération `'op simple'` plus haut.
Cela n'empêche pas de respécifier une destination pour un opérateur spécifique,
dans ce cas cette destination prendra la précédence sur celle par défaut.

Il est à noter que la source d'une opération, elle, doit rester unique. On ne
peut définir de répartition depuis plusieurs sources à la fois.

### Assiettes

Nous avons vu que les assiettes peuvent être déclarées comme entrées de la
cascade et utilisé comme source d'opérations. Il est également possible de
créer des assiettes intermédiaires à la volée comme destination :

```niagara
operation 'base rnpp'
sur assiette rbd
quotepart 20% vers distrib
quotepart 80% vers assiette rnpp
```

Ici, le mot-clé `assiette` apparaît aussi pour spécifier que la destination est
une assiette intermédiaire. Cette assiette n'a pas besoin d'être déclarée au
préalable et peut être utilisé comme source d'opérations ultérieures. Plusieurs
opérations peuvent attribuer des sommes à une même assiettes.

### Formules

Jusqu'à présent les opérateurs n'utilisaient que des valeurs littérales, mais il
est possible de définir des formules plus complexes :

```niagara
operation 'proportionnelle' vers acteur1
par prod
bonus entreeSalles * 0.90$
```

Cet exemple déclare que le partenaire `acteur1` va avoir droit à 90 centimes par
entrée en salle.

Les formules et sous-formules peuvent être encadrer par des parenthèses.

#### Littéraux

Les valeurs littérales ont toutes des types reconnaissables syntaxiquement :
- Les entiers sont des caractères numériques uniquement : `128`
- Les rationnels sont des nombres avec un point décimal (si un des cotés du
  point vaut zéro, il peut être omis) : `0.5` `10.0` (respectivement, `.5`
  `10.`)
  - ils peuvent aussi être un entier (ou rationnel) suivi de `%` : `50%`
    (strictement équivalent à `50 / 100.0`)
- Les sommes d'argent sont des entiers ou des rationnels suivi de `$` : `9.99$`

#### Arithmétique

Les formules peuvent contenir les opérations arithmétiques habituelles `+`, `-`,
`*`, et `/`. Ces opérateurs sont polymorphes mais n'ont de sémantiques que pour
des combinaisons précises de types.

Suit une table pour les opérations autorisées avec le type de résultat, s'il
existe :

| ligne `+`/`-` colonne  | `entier`    | `rationnel` | `argent` |
| :---:                  | :---:       | :---:       | :---:    |
| **`entier`**           | `entier`    | `rationnel` | NA       |
| **`rationnel`**        | `rationnel` | `rationnel` | NA       |
| **`argent`**           | NA          | NA          | `argent` |

| ligne `*` colonne  | `entier`    | `rationnel` | `argent` |
| :---:              | :---:       | :---:       | :---:    |
| **`entier`**       | `entier`    | `rationnel` | `argent` |
| **`rationnel`**    | `rationnel` | `rationnel` | `argent` |
| **`argent`**       | `argent`    | `argent`    | NA       |

| ligne `/` colonne  | `entier`    | `rationnel` | `argent` |
| :---:              | :---:       | :---:       | :---:    |
| **`entier`**       | `entier`    | `rationnel` | NA       |
| **`rationnel`**    | `rationnel` | `rationnel` | NA       |
| **`argent`**       | `argent`    | `argent`    | NA       |

#### Total and current values

Pour les valeurs cumulatives, il existe un mécanisme pour distinguer le total
accumulé depuis le début des temps et la valeur traversant la cascade à
l'instant courant : deux opérateurs postposés `total` et `courant`
respectivement.

```niagara
operation 'delta' vers acteur1
par prod
bonus 1$ * entreesSalle courant

operation 'total' vers acteur1
par prod
bonus 1$ * entreesSalle total
```

Les deux opérations ci-dessus attribuent un bonus proportionnel à l'entrée
`entréeSalles`. Dans le premier cas, seulement la valeur ajoutée depuis la
valorisation précédente ne sera utilisé pour le calcul, tandis que dans le
second cas la valeur cumulée est utilisé à chaque fois que l'opération est
exécutée.

Ce second cas est vraisemblablement très différent de l'intention derrière une
telle opération, il faut donc utiliser la valeur cumulative avec prudence.

Ces opérateurs peuvent être omis. Dans le cas d'une formule de transfert, les
valeurs prendront leur forme à l'instant courant. L'exemple suivant est
strictement équivalent à l'opération `'delta'` au dessus :

```niagara
operation 'delta bis' vers acteur1
par prod
bonus 1$ * entreesSalle
```

## Événements

Les événements sont des objets utilisés pour conditionner les attributions selon
l'état de la cascade. Ils peuvent être déclarés et nommés :

```niagara
evenement recupCom
atteint quand distrib = 20000$
```

Cet exemple déclare un événement `recupCom` dont la condition de déclenchement
est que le partenaire `distrib` atteigne un total cumulé de `20000$` de revenus
dans la cascade.

Contrairement aux formules d'attributions, dans les formules d'événements les
valeurs cumulatives prennent par défaut leur forme cumulée. Dans l'exemple, il
est entendu que la valeur de `distrib` correspond à son accumulation depuis le
début des temps et non sa valeur de récupération à un instant donné.

Il existe un sens implicite à l'égalité de la formule : il est supposé que la
gauche de l'égalité "rattrape" la droite. Autrement dit, l'évènement n'est pas
atteint tant que `distrib` est strictement inférieur à `20000$` et l'est quand
il est supérieur ou égal à cette somme.

Il est possible de définir des événements plus complexe qui peuvent revenir à un
état non-atteint pendant le calcul. Par exemple, quand les deux cotés de l'égalité
sont des valeurs qui évoluent indépendemment.

### Conditions dans les opérations

Pour conditionner une attribution dans une opération, on définit la condition
avant les opérateurs d'attribution :

```niagara
operation 'com' vers distrib
sur assiette rbd
avant evenement recupCom
  quotepart 90%
apres evenement recupCom
  quotepart 20%
```

Ici, en utilisant l'évènement déclaré plus haut avec le mot-clé `evenement`, on
réduit la part attribuée à `distrib` de 90% à 20% une fois l'évènement atteint.

Le mot-clé temporel utilisé définit dans quel cas l'attribution qui suit doit
être prise en compte :
- `avant`, jusqu'à l'instant où l'événement est déclenché
- `apres`, à partir de l'instant où l'événement est déclenché
- `quand`, uniquement à l'instant où l'événement est déclenché. Le principal
  usage étant pour des `bonus` ayant vocation à être distribué ponctuellement.

Il est également possible d'écrire des formules d'événement directement dans la
condition sans l'avoir préalablement déclaré :

```niagara
operation 'evenement anonyme'
par prod
quand entreeSalles = 100000
 bonus 1000$ vers acteur1
```

Les conditions peuvent être imbriquées les une dans les autres en utilisant des
parenthèses :

```niagara
operation 'conditions imbriquées' vers distrib
sur assiette rbd
avant evenement a (
 apres evenement b
  quotepart 10%
)
```

Dans cet exemple, les deux conditions `après` ne seront pris en compte que dans
le cas où `a` n'a pas été déclenché. Autrement dit, si `b` est déclenché mais
que `a` l'est aussi, alors cette opération ne fait aucune attribution.

Un opération peut avoir une série de conditions :

```niagara
operation 'conditions multiples' vers distrib
sur assiette rbd
avant evenement a
 quotepart 20%
apres evenement a
 quotepart 10%
apres evenement b
 quotepart 5%
```

Cet exemple déclare que la part accordée est de 20% jusqu'à l'événement `a` et
10% ensuite, et 5% après l'événement `b`.

Il y a des restrictions sur la forme d'une tel série, on ne peut écrire des
conditions `avant` qui suivent des `après`. Sémantiquement, il existe une
priorité entres les différentes conditions, les `après` sont prioritaires sur
toutes les conditions de la série au dessus d'elles, et les `avant` ont
priorités sur les autres `avant` qui les suivent.

On peut clarifier le comportement de cette opération en en écrivant une avec des
conditions imbriquées :

```niagara
operation 'conditions multiples strict' vers distrib
sur assiette rbd
avant evenement b (
 avant evenement a
  quotepart 20%
 apres evenement a
  quotepart 10%
)
apres evenement b
 quotepart 5%
```

Il est possible de spécifier plusieurs opérateurs d'attributions à la suite.
S'il existe des conditions, elle seront appliquées de la même manière à tous ces
opérateurs :

```niagara
operation 'operateurs multiples'
par prod
quand evenement e1
 bonus 200$ vers celebrite1
 bonus 2$ vers agent1
```

# Fonctionnalités avancées

Tous les éléments de langages présentés jusqu'ici devrait permettre de
représenter des cascades complètes. Les fonctionnalités abordées par la suite
permettent d'exprimer plus naturellement et succinctement certains concepts
communs.

## Contextualisation

Une contextualisation est un moyen de subdiviser des assiettes et des transferts
sans avoir à dupliquer manuellement du code d'opérations.

### Déclaration

Les domaines de contextes doivent être déclarés en début de code et énumérer
tous leurs cas :

```niagara
contexte Support :
- Salles
- TV
- Video
- VOD
- SVOD

contexte Territoire :
- France
- Belgique
- Etranger
```

Cet exemple déclare deux domaines, `Support` et `Territoire`, avec
respectivement cinq et trois cas. Ces domaines n'ont pas de liens entre eux et
on peut y faire référence séparément. Les noms de domaines et de cas doivent
commencer avec une majuscule.

Définir des domaines de contextes signifie que les assiettes de la cascade
peuvent être subdivisées selon les besoins exprimés dans le reste du code (le
comment sera abordé dans les sections suivantes). Avec l'exemple du dessus, il
est possible en théorie d'avoir 15 subdivisions d'assiette au maximum, une par
couple distinct de cas de `Support` et `Territoire`.

À noter qu'il serait possible d'encoder ces subdivisions à la main en utilisant
explicitement des assiettes distinctes (e.g. `rbd_Salles_France`,
`rbd_VOD_Etranger`), mais cela demande potentiellement beaucoup plus d'efforts à
l'utilisateur.

### Contextualiser des opérations

Un fois déclarés, les contextes peuvent être spécifié sur des opération pour
restreindre leur champs d'application :

```niagara
operation 'tv france'
pour Support(TV)
pour Territoire(France)
sur assiette rbd
quotepart 20% vers distrib
quotepart 80% vers assiette rnpp
```

Intuitivement, cela indique que cette opération ne s'applique seulement à la
subdivision `TV, France` de l'assiette `rbd`.

Il est possible de donner plusieurs cas par domaine. Exemple : `pour
Territoire(France, Belgique)`. Ou l'integralité d'un domaine : `pour tout
Territoire`. Ce dernier cas est équivalent à ne pas mentionner le domaine du
tout : sans restriction, on suppose que l'opération s'applique à tout les cas
d'un domaine.

Cette restriction implique nécessairement que, indépendemment de toute autres
restriction par ailleurs, il existe au moins deux subdivisions de l'assiette,
`TV, France` et tout le reste.

Quand ces distinctions s'opèrent sur des valeurs d'entrées de la cascade, cela
induit la nécessité à l'utilisateur de spécifier le contexte de l'entrée qu'il
souhaite valoriser.

#### Propagation des contraintes contextuelles

Comme dit précédemment, la distinction de contextes et la subdivision se fait
par besoin, selon les contraintes données dans les opérations.

Pour que la cascade soit cohérente dans son ensemble, les distinctions de
contexte sur une assiette doivent se refléter sur les assiettes qui lui sont
réattribuées.

```niagara
operation 'tv sofica'
pour Support(TV)
sur assiette rnpp
quotepart 10% vers sofica
```

Cette opération contraint l'assiette `rnpp` a être distincte entre le support TV
et les autres. Soit l'opération suivante, en amont de `rnpp` :

```niagara
operation 'tv monde'
sur assiette rbd
quotepart 20% vers distrib
quotepart 80% vers assiette rnpp
```

Pour être cohérent et bien qu'il n'y ait aucune contraintes explicites sur
`rbd`, cette assiette *doit* être distinguable au moins entre le support TV et
les autres, puisqu'elle verse dans une assiette qui fait cette distinction.

À noter que si par ailleurs on a :

```niagara
operation 'salle france'
pour Support(Salles)
pour Territoire(France)
sur assiette rbd
quotepart 30% vers distrib
quotepart 70% vers assiette rnpp
```

Ici il existe une distinction explicite sur `rbd` pour `Salles, France` et le
reste. Combinée aux contraintes précédentes, l'assiette possède les subdivisions
suivante :
- `Salles, France`, explicite
- `TV, tout Support`, au travers de la contrainte sur `rnpp`
- et le reste, à défaut de plus d'information.

Cette première distinction, n'étant pas une contrainte sur `rnpp`, elle sera
possiblement absorbée sur les attributions vers cette assiette : `Salles,
France` peut verser dans la subdivision "`tout Support` sauf `TV`" sans violer
la cohérence de la cascade.

#### Rafinement local

Au delà de la contextualisation des opérations, il peut être utile de spécifier
un contexte sur une valeur précise. Le langage propose une notation pour la
contextualisation locale :

```niagara
operation 'rem entreesSalles' vers acteur1
par prod
bonus entreesSalles(France) * 0.20$
bonus entreeSalles(Belgique, Etranger) * 0.15$
```

On accole à l'identifiant de la valeur le nom des cas de domaines choisis,
séparés par des virgules, entre parenthèses. Par souci de lisibilité, on ne
mentionne pas le domaine comme pour les opérations.

## Attribution par défaut

Une cascade bien formée interdit d'avoir des assiettes qui ne sont pas
reditribuées dans leur integralité. Les attributions pouvant être fragmentées
sur plusieurs opérations chacune conditionnées par des événements arbiraires, il
est difficile de s'assurer de la complétude de la répartition.

La fonctionnalité d'attribution par défaut permet de spécifier la destination de
la part de l'assiette qui n'est pas attribuée explicitement dans les opérations :

```niagara
defaut sur assiette rbd vers assiette rnpp

defaut sur assiette rnpp vers prod
```

Ces deux déclarations de défauts attribuent le résiduel des assiettes sources
vers une autre assiette, et un partenaire respectivement. Sans plus de précision
de contexte (avec du raffinement local), toutes les subdivisions sont
concernées.

## Prise en charge de déficit

Dans certains cas, il est possible pour une assiette d'être répartie sur un
total de plus de 100% de sa valeur. Dans ces cas, il faut spécifier comment
injecter ce surplus dans la cascade.

```niagara
deficit sur assiette rbd par prod
```

Cette déclaration spécifie que si l'assiette `rbd` (ou une de ses subdivisions)
se voit répartie au delà de son total, le partenaire `prod` va prendre en charge
la différence en tant que débiteur.

## Label de couloirs partenaire

La valeur des partenaires ne peut pas être redistribuée, mais peut toujours être
utiliser dans des formules. Pour permettre un contrôle plus fin de cette somme,
il est possible d´étiqueter l'utilisation de partenaire pour identifier et faire
référence à des couloirs spécifiques :

```niagara
operation 'special treatment' vers distrib[special]
par prod[distribBonus]
bonus 5000$
```

Dans cet exemple, les deux partenaires présents sont étiquetés avec des
identifiants supplémentaires entre crochets. Ces labels deviennent des valeurs
cumulatives à existence propre, des subdivisions de la valeur global du
partenaire, dont on peut faire référence par ailleurs en réutilisant le label.

Il est toujours possible d'utiliser le nom du partenaire sans label, auquel cas
cela fait référence à la somme des différent couloirs.

## Rétrocession

La rétrocession est une opération de transfert d'un part d'une valeur ciblée :

```niagara
operation 'return' vers distrib
par prod
retrocession 20% sur assiette rnpp
```

Cet opération est une autre manière d'exprimer le code suivant :

```niagara
operation 'return' vers distrib
par prod
bonus assiette rnpp * 20%
```

À noter que l'assiette cible `rnpp` est uniquement utilisée ici comme une
valeur, l'opération ne définit aucune réattribution de cette valeur.

L'opérateur `retrocession` n'as de sens que dans une opération entre deux
partenaires.

## Constantes

Une constante est une valeur nommée déclarée.

```niagara
constante minimum_garanti_distrib : 30000$
```

Cet exemple déclare un somme d'argent constante qui peut être utilisé par
ailleurs dans le code :

```niagara
evenement recupMG
atteint quand distrib = minimum_garanti_distrib
```

Cet événement est déclenché quand le partenaire `distrib` atteins une valeur
égale à la constante, soit `30000$`.

Cette construction permet de paramétrer une cascade en gagnant en lisibilité.
