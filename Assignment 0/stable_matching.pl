:- use_module(library(clpfd)).

stableMatching(F1, F2, M) :-
		F2 = "coop_s_10x10.csv",
		compile("C:\\Users\\speng\\Downloads\\prolog\\staple10.pl"),
        initial_employ(M0),
        findall(M-P, employer_preferences(M,P), MPs0),
        list_to_assoc(MPs0, MPs),
        pairs_keys_values(MPs0, Men, _),
        findall(W-P, student_preferences(W,P), WPs0),
        empty_assoc(E),
        phrase(students_preferences(WPs0), [E], [WPs]),
        phrase(employed_employer(Men), [s(WPs,MPs,M0)], [s(_,_,M1)]),
        maplist(without_wrapper, M1, M2),
        keysort(M2, M).

stableMatching(F1, F2, M) :-
		F2 = "coop_s_3x3.txt",
		compile("C:\\Users\\speng\\Downloads\\prolog\\staple3.pl"),
        initial_employ(M0),
        findall(M-P, employer_preferences(M,P), MPs0),
        list_to_assoc(MPs0, MPs),
        pairs_keys_values(MPs0, Men, _),
        findall(W-P, student_preferences(W,P), WPs0),
        empty_assoc(E),
        phrase(students_preferences(WPs0), [E], [WPs]),
        phrase(employed_employer(Men), [s(WPs,MPs,M0)], [s(_,_,M1)]),
        maplist(without_wrapper, M1, M2),
        keysort(M2, M).

without_wrapper(user(M)-W, M-W).

initial_employ(M) :- findall(omega-W, student_preferences(W,_), M).

students_preferences([]) --> [].
students_preferences([W-Ps|WPs]) -->
        preferences_student(Ps, W, 0),
        students_preferences(WPs).

preferences_student([], _, _) --> [].
preferences_student([M|Ms], W, N0) -->
        state0_state(WPs0, WPs1),
        { put_assoc(wm(W,M), WPs0, N0, WPs1),
          N1 #= N0 + 1 },
        preferences_student(Ms, W, N1).

employed_employer([])     --> [].
employed_employer([M|Ms]) --> employing_employer(user(M)), employed_employer(Ms).

state0_state(S0, S), [S] --> [S0].

employing_employer(omega)     --> [].
employing_employer(user(Man)) -->
        state0_state(s(WPs,MPs0,M0), s(WPs,MPs1,M2)),
        { get_assoc(Man, MPs0, [BestStudent|_]),
          memberchk(CurrentEmployer-BestStudent, M0),
          (   CurrentEmployer = user(Current) ->
              get_assoc(wm(BestStudent,Current), WPs, CurrentPref),
              get_assoc(wm(BestStudent,Man), WPs, NewPref)
          ;   true
          ),
          (   (   CurrentEmployer == omega ; NewPref #< CurrentPref ) ->
              delete(M0, CurrentEmployer-BestStudent, M1),
              M2 = [user(Man)-BestStudent|M1],
              X = CurrentEmployer
          ;   M2 = M0, X = user(Man)
          ),
          (   X = user(R) ->
              get_assoc(R, MPs0, RPs0),
              delete(RPs0, BestStudent, RPs),
              put_assoc(R, MPs0, RPs, MPs1)
          ;   MPs1 = MPs0
          ) },
        employing_employer(X).
