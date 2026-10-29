% Die zwölf Bezirke Berlins in vier Farben, so dass keine zwei Nachbarn
% dieselbe haben. Jede Variable ist ein Bezirk:
%   Cw Charlottenburg-Wilmersdorf  Fk Friedrichshain-Kreuzberg  Li Lichtenberg
%   Mh Marzahn-Hellersdorf         Mi Mitte                     Nk Neukölln
%   Pa Pankow                      Re Reinickendorf             Sp Spandau
%   Sz Steglitz-Zehlendorf         Ts Tempelhof-Schöneberg      Tk Treptow-Köpenick

colour(rot).
colour(gelb).
colour(gruen).
colour(blau).

% Jedes Nachbarpaar wird geprüft, sobald beide eine Farbe haben.
berlin(Cw, Fk, Li, Mh, Mi, Nk, Pa, Re, Sp, Sz, Ts, Tk) :-
    colour(Mi),
    colour(Pa), Pa \= Mi,
    colour(Re), Re \= Mi, Re \= Pa,
    colour(Cw), Cw \= Mi, Cw \= Re,
    colour(Fk), Fk \= Mi, Fk \= Pa,
    colour(Ts), Ts \= Mi, Ts \= Cw, Ts \= Fk,
    colour(Li), Li \= Fk, Li \= Pa,
    colour(Nk), Nk \= Fk, Nk \= Ts,
    colour(Sp), Sp \= Cw, Sp \= Re,
    colour(Sz), Sz \= Cw, Sz \= Sp, Sz \= Ts,
    colour(Tk), Tk \= Fk, Tk \= Li, Tk \= Nk,
    colour(Mh), Mh \= Li.
