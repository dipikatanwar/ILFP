male(shantanu).
female(ganga).
female(satyavati).
male(bheeshm).
male(chitrangada).
male(vichitravirya).
female(ambika).
female(ambalika).
male(dhritrashtra).
male(pandu).
female(gandhari).
female(kunti).
female(madri).
male(duryodhana).
female(dussala).
male(dushashana).
male(yudhishter).
male(bheem).
male(arjun).
male(nakul).
male(sehdev).
female(subhadra).
male(abhimanyu).
male(subala).
female(sudharma).
male(shakuni).
married(shantanu,ganga).
married(shantanu,satyavati).
child(bheeshm,shantanu).
child(bheeshm,ganga).
child(chitrangada,shantanu).
child(chitrangada,satyavati).
child(vichitravirya,shantanu).
child(vichitravirya,satyavati).
married(vichitravirya,ambika).
married(vichitravirya,ambalika).
child(dhritrashtra,ambika).
child(dhritrashtra,vichitravirya).
child(pandu,ambalika).
child(pandu,vichitravirya).
married(dhritrashtra,gandhari).
married(pandu,kunti).
married(pandu,madri).
child(duryodhana,dhritrashtra).
child(duryodhana,gandhari).
child(dushashana,dhritrashtra).
child(dushashana,gandhari).
child(dussala,dhritrashtra).
child(dussala,gandhari).
child(yudhishter,pandu).
child(yudhishter,kunti).
child(bheem,pandu).
child(bheem,kunti).
child(arjun,pandu).
child(arjun,kunti).
child(nakul,pandu).
child(nakul,madri).
child(sehdev,pandu).
child(sehdev,madri).
married(arjun,subhadra).
child(abhimanyu,arjun).
child(abhimanyu,subhadra).
married(abhimanyu,uttara).
child(shakuni,subala).
child(shakuni,sudharma).
child(gandhari,subala).
child(gandhari,sudharma).
male(krishna).
male(vasudeva).
female(devaki).
female(rohini).
male(balram).
female(hidimba).
male(ghatotgach).
female(draupady).
married(bheem,hidimba).
child(ghatotgach,hidimba).
child(ghatotgach,bheem).
married(yudhishter,draupady).
married(bheem,draupady).
married(arjun,draupady).
married(nakul,draupady).
married(sehdev,draupady).
married(vasudeva,devaki).
married(vasudeva,rohini).
child(krishna,vasudeva).
child(krishna,devaki).
child(subhadra,vasudeva).
child(subhadra,rohini).
child(balram,vasudeva).
child(balram,rohini).
male(karn).
male(surya).
married(surya,kunti).
child(karn,kunti).
child(karn,surya).

/* Rules */
father(X,Y):-
	male(X),child(Y,X).
	
mother(X,Y):-
	female(X),child(Y,X).
	
husband(X,Y):-
	male(X),female(Y),(married(X,Y);married(Y,X)).
	
wife(X,Y):-
	female(X),male(Y),(married(Y,X);married(X,Y)).

son(X,Y):-
	male(X),child(X,Y).

daughter(X,Y):-
	female(X),child(X,Y).

brother(X,Y):-
	male(X),
	father(F, Y), father(F,X),X \= Y,
	mother(M,X),mother(M,Y),X \= Y.
	
sister(X,Y):-
	female(X),
	father(F, Y), father(F,X),X \= Y,
	mother(M, X), mother(M,Y),X \= Y.
	
father_in_law(X,Y):-
	male(X),
	child(Z,X),(married(Y,Z);married(Z,Y)).
	
mother_in_law(X,Y):-
	female(X),
	child(Z,X),(married(Y,Z);married(Z,Y)).
	
son_in_law(X,Y):-
	male(X),
	husband(X,Z),child(Z,Y).
	
daughter_in_law(X,Y):-
	female(X),
	wife(X,Z),child(Z,Y).

uncle(X,Y):-
	male(X),
	siblings(X,Z),child(Y,Z).
	
aunt(X,Y):-
	female(X),
	wife(X,Z),uncle(Z,Y).
    
grandfather(X,Y):-
	male(X),
	father(X,Z),child(Y,Z).
	
grandmother(X,Y):-
	female(X),
	mother(X,Z),child(Y,Z).

grandson(X,Y):-
	male(X),
	child(X,Z),child(Z,Y).
	
granddaughter(X,Y):-
	female(X),
	child(X,Z),child(Z,Y).

brother_in_law(X,Y):-
	male(X),
	husband(X,Z),siblings(Z,Y);
	brother(X,Z),(married(Z,Y);married(Y,Z)).

sister_in_law(X,Y):-
	female(X),
	wife(X,Z),siblings(Z,Y);
	sister(X,Z),(married(Z,Y);married(Y,Z)).
	
cousin_brother(X,Y):-
	male(X),
	son(X,Z),uncle(Z,Y),(X \= Y).
	
cousin_sister(X,Y):-
	female(X),
	daughter(X,Z),uncle(Z,Y),(X \= Y).

step_father(X,Y):-
	male(X),
	husband(X,Z),child(Y,Z),\+(child(Y,X)).

step_mother(X,Y):-
	female(X),
	wife(X,Z),child(Y,Z),\+(child(Y,X)).

step_brother(X,Y):-
	male(X),
	child(X,Z),child(Y,Z),\+(brother(X,Y)),(X \= Y).
	
step_sister(X,Y):-
	female(X),
	child(X,Z),child(Y,Z),\+(sister(X,Y)),(X \= Y).

step_daughter(X,Y):-
	female(X),
	(step_father(Y,X);step_mother(Y,X)).
	
step_son(X,Y):-
	male(X),
	(step_father(Y,X);step_mother(Y,X)).

siblings(X,Y):-
	brother(X,Y);
	sister(X,Y);
	cousin_brother(X,Y);
	cousin_sister(X,Y);
	step_brother(X,Y);
	step_sister(X,Y).
	
/*
father(X,Y).
mother(X,Y).
husband(X,Y).
wife(X,Y).
son(X,Y).
daughter(X,Y).
brother(X,Y).
sister(X,Y).
father_in_law(X,Y).
mother_in_law(X,Y).
son_in_law(X,Y).
daughter_in_law(X,Y).
uncle(X,Y).
aunt(X,Y).
grandfather(X,Y).
grandmother(X,Y).
grandson(X,Y).
granddaughter(X,Y).
brother_in_law(X,Y).
sister_in_law(X,Y).
cousin_brother(X,Y).
cousin_sister(X,Y).
step_father(X,Y).
step_mother(X,Y).
step_brother(X,Y).
step_sister(X,Y).
step_daughter(X,Y).
step_son(X,Y).
siblings(X,Y).
grandfather(X,dhritrashtra).
grandfather(X,pandu).
grandfather(X,abhimanyu).
grandfather(X,arjun).
grandfather(X,bheem).
grandfather(X,yudhishter).
grandfather(X,nakul).
grandfather(X,sehdev).
grandfather(X,duryodhana).
grandfather(X,dussala).
grandfather(X,dushashana).
grandmother(X,dhritrashtra).
grandmother(X,pandu).
grandmother(X,abhimanyu).
grandmother(X,arjun).
grandmother(X,bheem).
grandmother(X,yudhishter).
grandmother(X,nakul).
grandmother(X,sehdev).
grandmother(X,duryodhana).
grandmother(X,dussala).
grandmother(X,dushashana).
father(X,dhritrashtra).
father(X,pandu).
father(X,abhimanyu).
father(X,arjun).
father(X,bheem).
father(X,yudhishter).
father(X,nakul).
father(X,sehdev).
father(X,duryodhana).
father(X,dussala).
father(X,dushashana).
mother(X,dhritrashtra).
mother(X,pandu).
mother(X,abhimanyu).
mother(X,arjun).
mother(X,bheem).
mother(X,yudhishter).
mother(X,nakul).
mother(X,sehdev).
mother(X,duryodhana).
mother(X,dussala).
mother(X,dushashana).
wife(X,dhritrashtra).
wife(X,pandu).
wife(X,abhimanyu).
wife(X,arjun).
wife(X,bheem).
wife(X,yudhishter).
wife(X,nakul).
wife(X,sehdev).
wife(X,duryodhana).
wife(X,dushashana).
wife(X,vasudeva).
husband(X,satyavati).
husband(X,kunti).
husband(X,gandhari).
husband(X,draupady).
husband(X,hidimba).
husband(X,subhadra).
husband(X,devaki).
son(dhritrashtra,X).
son(pandu,X).
son(abhimanyu,X).
son(arjun,X).
son(bheem,X).
son(yudhishter,X).
son(nakul,X).
son(sehdev,X).
son(duryodhana,X).
son(dushashana,X).
daughter(dussala,X).
daughter(gandhari,Y).
daughter(subhadra,Y).
brother(X,arjun).
brother(X,bheem).
brother(X,yudhishter).
brother(X,nakul).
brother(X,sehdev).
brother(X,duryodhana).
brother(X,dussala).
brother(X,dushashana).
brother(X,gandhari).
sister(shakuni,X).
father_in_law(shantanu,X).
father_in_law(arjun,X).
father_in_law(pandu,X).
mother_in_law(X,drapaudy).
mother_in_law(X,gandhari).
daughter_in_law(X,subhadra).
daughter_in_law(X,arjun).
uncle(X,abhimanyu).
uncle(X,duryodhana).
aunt(subhadra,X).
aunt(draupady,X).
grandson(dhritrashtra,X).
grandson(pandu,X).
grandson(abhimanyu,X).
grandson(arjun,X).
grandson(bheem,X).
grandson(yudhishter,X).
grandson(nakul,X).
grandson(sehdev,X).
grandson(duryodhana,X).
grandson(dushashana,X).
granddaughter(dussala,X).
brother_in_law(arjun,X).
brother_in_law(shakuni,X).
sister_in_law(subhadra,X).
cousin_brother(abhimanyu,Y).
step_brother(pandu,Y).
step_brother(arjun,X).
step_mother(draupady,X).
step_mother(madri,X).
step_mother(kunti,X).
siblings(arjun,X).
siblings(abhimanyu,X).
*/