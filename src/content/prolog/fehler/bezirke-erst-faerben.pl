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

berlin(Cw, Fk, Li, Mh, Mi, Nk, Pa, Re, Sp, Sz, Ts, Tk) :-
    colour(Mi),
    colour(Pa),
    colour(Re),
    colour(Cw),
    colour(Fk),
    colour(Ts),
    colour(Li),
    colour(Nk),
    colour(Sp),
    colour(Sz),
    colour(Tk),
    colour(Mh),
    Pa \= Mi,
    Re \= Mi,
    Re \= Pa,
    Cw \= Mi,
    Cw \= Re,
    Fk \= Mi,
    Fk \= Pa,
    Ts \= Mi,
    Ts \= Cw,
    Ts \= Fk,
    Li \= Fk,
    Li \= Pa,
    Nk \= Fk,
    Nk \= Ts,
    Sp \= Cw,
    Sp \= Re,
    Sz \= Cw,
    Sz \= Sp,
    Sz \= Ts,
    Tk \= Fk,
    Tk \= Li,
    Tk \= Nk,
    Mh \= Li,
    Mh \= Tk.
