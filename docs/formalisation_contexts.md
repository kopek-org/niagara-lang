A niagara program with contexts can be seen as a graph, where each node is a pool, and edges between pools are operations. In this higher level representation, pools have context, which indicates where the money is coming from. Context can be seen as types, and asserting their coherence on the graph is akin to a typing problem.

This document proposes a formalization of contexts, define a typing systems, and proposes inference rules for the context of pools which are proven to give a correctly typed graph.

# Mathematical preliminaries

## Definitions

Let $X$ be a set. We define a partial equivalence relation (P.E.R.) $r$ on $X$ as a binary relation that is symmetric and transitive. Partial reflexivity is not an axiom but a theorem stating that

$$
\forall x, y, x \sim y \implies (x \sim x)
$$

which stems from the fact that if there are such an x and y, by symmetry $y \sim x$ and by transitivity $x \sim y \sim x$.

Like equivalence relation, P.E.R.s correspond directly to partial partitions of $X$. A partial partition of $X$ is a set of mutually disjoint subsets of $X$. Going from a P.E.R. $r_a$ to a partial partition $C_a$ and back is done by considering the equivalence classes of the relation.

To a P.E.R. $r_a$, we can associate its perimeter $p_a$, that is the set on which the equivalence relation is total (the union of the equivalence classes).

## Operations on P.E.R.

Let $r_a$ and $r_b$ be two P.E.R.

### Conjunction

The conjunction $r_c = r_a \wedge r_b$ is defined by: $x \sim_c y$ iff any of the following is true: 

- $x, y \in (p_a \cap p_b) \text{ and } x \sim_a y \text{ and } x \sim_b y$
- $x, y \in p_a \setminus p_b \text{ and } x \sim_a y$
- $x, y \in p_b \setminus p_a \text{ and } x \sim_b y$

The equivalence classes $C_c$ of $r_c$ are given by:

$$C_c = \{c_a \cap c_b, c_a \in C_a, c_b \in C_b\} \cup \{c_a \cap \overline{p_b}, c_a \in C_a\} \cup \{c_b \cap \overline{p_a}, c_b \in C_b\}$$

Conjunction is both associative and commutative.
### Disjunction

The disjunction $r_c = r_a \vee r_b$ is defined by the transitive cloture of ($x \sim y$ iif $x \sim_a y$ or $x \sim_b y$).

### Projection

Let $Y$ be a subset of $X$. The projection $r_c = r_a \downarrow Y$ of $r_a$ on $Y$ is defined by $x \sim_c y$ iff $x, y \in Y$ and $x \sim_a y$

The projection of the equivalence classes is $\{ c \cap Y, c \in C_a\}$

### Projection without loss

A projection is without loss iff, for all $x,y \in X$ such that $x \sim_a y$, $x \in Y$ implies $y \in Y$.

In terms of equivalence classes: for all $c$ in $C_a$, $c \subseteq Y$ or $c \subseteq \overline{Y}$.

### Order

We say that $r_a \leq r_b$ (is partially finer) iff for all $x,y \in X, x \sim_a y \Rightarrow x \sim_b y$.

In terms of equivalence classes, $\forall c_a \in C_a, \exists c_b \in C_b, c_a \subseteq c_b $

*Remark: disjunction is an upper bound for this order relation but conjunction is **not** a lower bound.*

## Properties

- $(r \downarrow P) \downarrow P = r \downarrow P$
- $r \downarrow P \leq r$
- $p_{r \downarrow P} \subseteq P$
- $(r_a \wedge r_b) \downarrow P = (r_a \downarrow P) \wedge (r_b \downarrow P)$
- Let $c$ be a class of $(\bigwedge r_i)$, then for all $i$, either $c \subseteq \overline{p_{r_i}}$ or there exist a class $d$ of $r_i$ such that $c \subseteq d$. 
# Type system


An operation has both an input and an input. It also has an associated projection on a set $P$.
Our types are partial equivalence relations. The input and output have types $r_i$ and $r_o$. We say that our operation is well typed if and only if:

- $r_i \downarrow P$ is without loss, and 
- $r_i \downarrow P \leq r_o$

Our graph is well typed iff all its operations are well typed and for all variables $v$, with type $r_v$, and corresponding  children operations $C$: 

$$p_v \subseteq \bigcup_{c \in C} (p_{r_c} \cap P_c)$$

Moreover it is tightly typed if it is correctly typed and for all variables $v$, with type $r_v$, and corresponding parent operations $P$: 

$$p_v \subseteq \bigcup_{p \in P} (p_{r_p} \cap P_p)$$

# Inference

## Backward inference

In backward inference we are gonna assign upper bound to each variables type with the following guarantees:

- When giving types to the entries, if all these types are less than their respective entry's upper bound, then there exists a correct typing of the whole graph. 
- For a correct typing of all individual operations, then if all types are below the bound, the typing is also correct for the graph as a whole.

The proof is done by induction.

The basis case is a graph with only entries that are also outputs (ie, a graph without any operation). The upper bounds for all variable is $\top$, the equivalence relation with only one class, $X$ itself.

Let's consider a graph with bounds for each variable respecting the property above. We add "upstream" nodes to this graph playing the role of "inputs" and pouring in any of the node of the original graph. For each added node $i$, we consider the set of nodes it is pouring into $O_i$. We define:

$$
r_i = \left(\bigwedge_{o\in O_i} r_o \downarrow P_o \right) \left\downarrow \left(\bigcap_{o \in O_i} p_{r_o} \cup \overline{P_o} \right)\right.
$$

Let's prove that this makes all added operations correctly typed.

Let's consider $r_i$ and one of its outputs in particular $\omega$.

We must show that $r_i \downarrow P_\omega$ is without loss and less than $r_\omega$

### Without loss
Let $a$ be a class of $r_i$. There exist a class $b$ of $\left(\bigwedge_{o\in O_i} r_o \downarrow P_o \right)$ so that $a = b \cap \left(\bigcap_{o \in O_i} p_{r_o} \cup \overline{P_o} \right)$.
The perimeter of $(r_\omega \downarrow P_\omega)$ is $(p_{r_\omega} \cap P_\omega)$. By one of the properties of section 1:
- either $b \subseteq \overline{p_{r_\omega} \cap P_\omega} = \overline{p_{r_\omega}} \cup \overline{P_\omega}$, in which case $a \subseteq (\overline{p_{r_\omega}} \cup \overline{P_\omega}) \cap (p_{r_\omega} \cup \overline{P_\omega}) = \overline{P_\omega}$
- or there exist a class $c$ of $(r_\omega \downarrow P_\omega)$ so that $b \subseteq c$, in which case $a \subseteq b \subseteq c \subseteq P_\omega$.

So either $a \subseteq P_\omega$ or $a \subseteq \overline{P_\omega}$. This being true for any $a$, the projection is without loss. QED.

###  $r_i \downarrow P_\omega \leq r_\omega$

Let $d$ be a class of $r_i \downarrow P_\omega$. There exist a class $a$ of $r_i$ so that $d = a \cap P_\omega$.

Taking the same $a$ and $b$ as above:
- either $b \subseteq \overline{p_{r_\omega} \cap P_\omega}$, in which case as above $a \subseteq\overline{P_\omega}$, and $a$ is not part of $r_i \downarrow P_\omega$ (here actually $d$ is the empty set, which is ill defined, we should rework all definitions to exclude the empty set but that does not change much).
- or there exist the same $c$ as above and $a$ is a subset of a class of $(r_\omega \downarrow P_\omega) \leq r_\omega$, do $d$ is a subset of a class of $r_\omega$.

QED.

Let's show the correctness of the graph as a whole.

We must show that $p_{r_i} \subseteq \bigcup_{o \in O_i} (p_{r_o} \cap P_o)$, which is immediate from the definition of $p_{r_i}$

$$
p_{r_i} = \left(\bigcup_{o \in O_i} p_{r_o} \cap P_o\right) \bigcap \left(\bigcap_{o \in O_i} p_{r_o} \cup \overline{P_o} \right)
$$

## Forward inference

In forward inference, we consider that for a variable $o$, we know the types $r_i$ of all its parents $v_p$, and the associated projection of the operations $P_i$. 

Claim: $r_o = \bigvee_{i \in I} r_i \downarrow P_i$ is the smallest possible type that makes all considered operations well typed.

### Proof

$r_o$ is an admissible type:

- For all $i$, $r_i \downarrow P_i$ is without loss by assumption,
- For all $i$, $r_i \downarrow P_i \leq r_o $ trivially,

$r_o$ is smaller than all other admissible types as it is a lower bound for $\leq$.
