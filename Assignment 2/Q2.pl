adj(a,b).
adj(a,g).
adj(b,c).
adj(b,i).
adj(c,d).
adj(d,e).
adj(d,j).
adj(e,l).
adj(f,g).
adj(g,h).
adj(h,i).
adj(i,j).
adj(j,k).
adj(k,l).
adj(l,m).

color(red).
color(yellow).
color(blue).

% question a
colorset([], []).
colorset([_|T], [HeadColor|TailColor]):- color(HeadColor), colorset(T, TailColor).

% question b
diffadjcolor(_,_,[],[]).
diffadjcolor(Glass,Color,[HofOtherGlasses|TofOtherGlasses],[HofOtherColors|TofOtherColors]):-
    adj(Glass,HofOtherGlasses),
    \+(Color == HofOtherColors),!,
    diffadjcolor(Glass,Color,TofOtherGlasses,TofOtherColors),!,% beside the 2nd, Glass should fit with other glasses
    diffadjcolor(HofOtherGlasses,HofOtherColors,TofOtherGlasses,TofOtherColors),!.%second glass should also fit with other glasses
diffadjcolor(Glass,Color,[HofOtherGlasses|TofOtherGlasses],[HofOtherColors|TofOtherColors]):-
    \+adj(Glass,HofOtherGlasses),
    diffadjcolor(Glass,Color,TofOtherGlasses,TofOtherColors),!,
    diffadjcolor(HofOtherGlasses,HofOtherColors,TofOtherGlasses,TofOtherColors),!.

valid([],[]).
valid([_],[_]).
valid([HGlass|TGlasses],[HColor|TColors]):- diffadjcolor(HGlass,HColor,TGlasses,TColors).

% question c
generate(Gs,Cs):-colorset(Gs,Cs),valid(Gs,Cs).