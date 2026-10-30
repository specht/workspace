% Die Familie Bach über fünf Generationen.
% parent(Elternteil, Kind): Elternteil ist Mutter oder Vater von Kind.

parent(christoph, johann_ambrosius).

parent(johann_ambrosius, johann_christoph).
parent(johann_ambrosius, johann_jacob).
parent(johann_ambrosius, johann_sebastian).
parent(maria_elisabeth, johann_christoph).
parent(maria_elisabeth, johann_jacob).
parent(maria_elisabeth, johann_sebastian).

parent(johann_michael, maria_barbara).

parent(johann_sebastian, catharina_dorothea).
parent(johann_sebastian, wilhelm_friedemann).
parent(johann_sebastian, carl_philipp_emanuel).
parent(johann_sebastian, johann_gottfried_bernhard).
parent(maria_barbara, catharina_dorothea).
parent(maria_barbara, wilhelm_friedemann).
parent(maria_barbara, carl_philipp_emanuel).
parent(maria_barbara, johann_gottfried_bernhard).

parent(johann_sebastian, gottfried_heinrich).
parent(johann_sebastian, elisabeth_juliane_friederica).
parent(johann_sebastian, johann_christoph_friedrich).
parent(johann_sebastian, johann_christian).
parent(anna_magdalena, gottfried_heinrich).
parent(anna_magdalena, elisabeth_juliane_friederica).
parent(anna_magdalena, johann_christoph_friedrich).
parent(anna_magdalena, johann_christian).

parent(carl_philipp_emanuel, johann_sebastian_der_juengere).
parent(johann_christoph_friedrich, wilhelm_friedrich_ernst).

% male(Person) und female(Person)
male(christoph).
male(johann_ambrosius).
male(johann_christoph).
male(johann_jacob).
male(johann_sebastian).
male(johann_michael).
male(wilhelm_friedemann).
male(carl_philipp_emanuel).
male(johann_gottfried_bernhard).
male(gottfried_heinrich).
male(johann_christoph_friedrich).
male(johann_christian).
male(johann_sebastian_der_juengere).
male(wilhelm_friedrich_ernst).

female(maria_elisabeth).
female(maria_barbara).
female(anna_magdalena).
female(catharina_dorothea).
female(elisabeth_juliane_friederica).

% Regeln

father(F, C) :- parent(F, C), male(F).
mother(M, C) :- parent(M, C), female(M).

grandparent(G, C) :- parent(G, P), parent(P, C).

sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
