:-dynamic geracoes/1.
:-dynamic tempo/1.
:-dynamic populacao/1.
:-dynamic dim_populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic melhor_geracao/1.
:-dynamic entregas/1.

s(X,[X|T],T).
s(X,[H|T],[H|R]):-s(X,T,R).
p([],[]).
p([H|T],P):-p(T,W), s(H,P,W).

gera_entregas:-
	inicializa_dados,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	tempo(TEMPO),
	gera_geracao(0,NG,TEMPO,PopOrd).

inicializa_dados:-write('Elementos da População: '),read_term(Populacao,[]),(retract(populacao(_));true), asserta(populacao(Populacao)),
        write('Numero de novas Geracoes: '),read(NG),(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Tempo limite: '),read(TEMPO),(retract(tempo(_));true), asserta(tempo(TEMPO)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(dim_populacao(_));true), asserta(dim_populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100,
	(retract(prob_cruzamento(_));true),	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100,
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)),
	length(Populacao,DPop),
	(retract(entregas(_));true), asserta(entregas(DPop)).

gera_populacao([FastestDeliveryList,ListAux|Pop]):-
	dim_populacao(TamPop),
        populacao(ListaEntregas),
	entregas(NumT),
	timeHeuristic(ListaEntregas,FastestDeliveryList),
        massAndTimeHeuristic(ListaEntregas,ListResult),
	DimPop is TamPop-2,
	gera_populacao(DimPop,ListaEntregas,NumT,Pop),
	(compara_lista(FastestDeliveryList,ListResult),!,troca_lista(ListResult,ListAux));ListAux is ListResult.

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaEntregas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaEntregas,NumT,Resto),
	gera_individuo(ListaEntregas,NumT,Ind),
	not(member(Ind,Resto)).
gera_populacao(TamPop,ListaEntregas,NumT,L):-
	gera_populacao(TamPop,ListaEntregas,NumT,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaEntregas,NumT,[G|Resto]):-
	random_permutation(ListaEntregas,ListaPermutada),
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaPermutada,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

troca_lista([H1,H2|T],[H2,H1|T]):-!.

compara_lista([],[]):-!.
compara_lista([],_):-!.
compara_lista([L1Head|L1Tail], List2):-
    member(L1Head, List2),!,
    compara_lista(L1Tail, List2).


retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*Time|Resto1]):-
	get_total_weight_delivery_list(Ind,DeliveryWeight),
	TotalWeight is DeliveryWeight + 7500,
	get_path_total_time(1,Ind,TotalWeight,Time),
	avalia_populacao(Resto,Resto1).

ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).

btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).


gera_geracao(G,_,Tempo,Pop):-
	buscar_tempo(Pop,Tempo1),
	Tempo1 =<Tempo,!,
	(retract(melhor_geracao(_));true), asserta(melhor_geracao(Pop)),
	write('Geracao '), write(G), write(':'), nl, write(Pop), nl.

buscar_tempo([H|_],Res):-
        separa_avaliacao(H,_,Res).
gera_geracao(G,G,_,Pop):-!,
	(retract(melhor_geracao(_));true), asserta(melhor_geracao(Pop)),
	write('Geracao '), write(G), write(':'), nl, write(Pop), nl.

gera_geracao(N,G,Tempo,Pop):-
        write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	random_permutation(Pop,ListaPermutada),
	cruzamento(ListaPermutada,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	comparar_geracoes(Pop,NPopOrd,NMix),
	algoritmo_nao_puramente_elitista(Pop,NMix,NovaPop),
	N1 is N+1,
	gera_geracao(N1,G,Tempo,NovaPop).


separa_avaliacao(Y*X,Y, X).
separa_avaliacao2(Y*X*Z,Y, X,Z).



separa_numero(Num, Y, X) :-
    Y is float_integer_part(Num),
    X is float_fractional_part(Num).



comparar_geracoes([H,H3|_],[H2,H4|Pop2],Res):-
        separa_avaliacao(H,_,Split1),
	separa_avaliacao(H2,_,Split2),
	separa_avaliacao(H3,_,Split3),
	separa_avaliacao(H4,_,Split4),
        ((Split1 < Split2,!,(Split3 <Split4,!, append([H,H3],Pop2,Res);append([H,H4],Pop2,Res)));
	(Split3 <Split4,!,append([H2,H3],Pop2,Res);nl,append([H2,H4],Pop2,Res))).

algoritmo_nao_puramente_elitista(Gantiga,Gnova,Res):-
	junta_geracoes(Gantiga,Gnova,JuntaPop),
	retirar_repetidos(JuntaPop,SortPop),
	ordena_populacao(SortPop,OrdPop),
	dim_populacao(Dimensao),
	verificar_elementos_pop(Dimensao,OrdPop,ResAux),
	separar_p_melhores(Dimensao,ResAux,ListaP,ListaT),
	obter_lista_aux(ListaT,ResAux1),
	append(ListaP,ResAux1,ResAux2),
	ordena_populacao(ResAux2,Res).

junta_geracoes([],[],[]):-!.

junta_geracoes([H|Gantiga],[H1|Gnova],[H,H1|Res]):-
	junta_geracoes(Gantiga,Gnova,Res).

retirar_repetidos(L,Res):-
	retirar_repetidos_aux(L,LAux,LAux1),
	retirar_repetidos2(LAux,LAux1,Res).

retirar_repetidos_aux([],[],[]):-!.
retirar_repetidos_aux([H1|T],[H3|Res],[T2|Res1]):-
	separa_avaliacao(H1,H3,T2),
	retirar_repetidos_aux(T,Res,Res1).

retirar_repetidos2([], [], []).
retirar_repetidos2([H1|T], [_|T1], Res):-
  member(H1, T),
  !,
  retirar_repetidos2(T, T1, Res).
retirar_repetidos2([H1|T], [H2|T1], [H1*H2|Res]):-
  retirar_repetidos2(T, T1, Res).


verificar_elementos_pop(0,_,[]):-!.

verificar_elementos_pop(Dimensao,[H|L],[H|L1]):-
		Dim is Dimensao-1,
		verificar_elementos_pop(Dim,L,L1).

separar_p_melhores2(0.0,L,[],L):-!.


separar_p_melhores2(P,[H|L],[H|ListaP],ListaT):-
        P1 is P-1,
	separar_p_melhores2(P1,L,ListaP,ListaT).

separar_p_melhores(Dimensao,L,ListaP,ListaT):-
	PAux is 0.20*Dimensao,
	(PAux >=1,!,separa_numero(PAux,P1,Dec),
	 (Dec>0,!,P is P1+1,separar_p_melhores2(P,L,ListaP,ListaT);separar_p_melhores2(P1,L,ListaP,ListaT));
	separar_p_melhores2(1,L,ListaP,ListaT)).

obter_lista_aux(ListaT, Res):-
	obter_lista_aux2(ListaT,ListaTAux),
	ordena_populacao_nao_elitista(ListaTAux,ListaTOrd),
	obter_avaliacao_correta(ListaTOrd,Res).

ordena_populacao_nao_elitista(PopAv,PopAvOrd):-
	bsort2(PopAv,PopAvOrd).

bsort2([X],[X]):-!.
bsort2([X|Xs],Ys):-
	bsort2(Xs,Zs),
	btroca2([X|Zs],Ys).

btroca2([X],[X]):-!.

btroca2([X*VX*VVX,Y*VY*VVY|L1],[Y*VY*VVY|L2]):-
	VX>VY,!,
	btroca2([X*VX*VVX|L1],L2).

btroca2([X|L1],[X|L2]):-btroca2(L1,L2).

obter_lista_aux2([],[]):-!.

obter_lista_aux2([H|L],[H1*H2*R|Res]):-
	random(0.0,1.0,R),
        separa_avaliacao(H,H1,T),
	H2 is T*R,
	obter_lista_aux2(L,Res).


obter_avaliacao_correta([],[]):-!.

obter_avaliacao_correta([H|L],[HH*H1|Res]):-
	separa_avaliacao2(H,HH,T,T1),
	H1 is T/T1,
	obter_avaliacao_correta(L,Res).


gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	entregas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).

sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	entregas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).

elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	entregas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).

insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	entregas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).

eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).

% Realiza regra de tres simples para calcular o tempo/energia de um trajeto com base no peso total atual
    calculate_cost(Weight,Value,Res):- Res is ((Weight*Value)/11800).

    % Calcula a energia disponivel no camiao apos realizar um trecho
    calculate_energy_disp(EActual,Energy,EnergyDisp):-EnergyDisp  is EActual - Energy.

    % Calcula o tempo necessario para carregar a bateria ate aos 80% (64kWh)
    battery_time_charge(EActual,ENeeded,C1,C2,TC,E):-
        TCAux is (64-EActual) * (60/48),
	check_energy_greater_than_16(64,ENeeded,C1,C2,TAux,E),
        ((TAux>TCAux,!,TC is TAux);TC is TCAux).

    check_energy_greater_than_16(Energy1,Energy2,C1,C2,T,E):-
	(Energy1-Energy2<16,!,
        ((C1\=5,!,dadosCam_t_e_ta(_,C1,C2,_,_,AddicionalTime));(dadosCam_t_e_ta(_,C2,C1,_,_,AddicionalTime))),
        ((AddicionalTime>0,!,T is AddicionalTime,E is 16);(battery_time_charge_final_city(Energy1,Energy2,T),E is Energy1+16-Energy2)));
        (T is 0,E is Energy1-Energy2).

    % Verifica se ÃƒÂ© necessario carregar o camiao para realizar o proximo trecho e retorna o tempo necessario de carga
    verify_charge(EAtual,ENeces,C1,C2,TimeRet,T,E):-(EAtual < ENeces,!,
        battery_time_charge(EAtual,ENeces,C1,C2,TAux1,E),((TAux1 >TimeRet,!,T is TAux1);T is TimeRet));
        (check_energy_greater_than_16(EAtual,ENeces,C1,C2,TAux,E),((TAux>TimeRet,!,T is TAux);(T is TimeRet))).

    battery_time_charge_final_city(EActual,ENeeded,TC):-
        (ENeeded >EActual,!,TC is (ENeeded+16-EActual)*(60/48));
        ((16> ENeeded,!,TC is (16-ENeeded) * (60/48));(TC is (EActual+16-ENeeded) * (60/48))).

    verify_charge_final_city(EActual,ENeeded,C1,C2,TimeRet,T):-
        (EActual < ENeeded,!,battery_time_charge_final_city(EActual,EActual-ENeeded,T));
        (check_energy_greater_than_16(EActual,ENeeded,C1,C2,TAux,_),((TAux>TimeRet,!,T is TAux); T is TimeRet)).

    % Retorna peso da entrega com base no ID da mesma
    get_weight_by_delivery_id(Id,Weight) :- entrega(Id,_,Weight,_,_,_).

    % Retorna o peso total da lista de entregas
    get_total_weight_delivery_list([],0).
    get_total_weight_delivery_list([H|T], R) :-
        get_weight_by_delivery_id(H,Weight),
        get_total_weight_delivery_list(T,R1),
        R is R1 + Weight.

    % Retorna o tempo e gasto energetico entre duas cidades de acordo com a carga atual
    get_time_and_energy_between_two_cities(C1,C2,Weight,TimeRes,EnergyRes):-
        % busca dados sobre o tempo e energia de uma cidade a outra com a carga maxima
        dadosCam_t_e_ta(_,C1,C2,TimeAux,EnergyAux,_),
        % calcula custo de tempo
        calculate_cost(Weight,TimeAux,TimeRes),
        % calcula custo de energia
        calculate_cost(Weight,EnergyAux,EnergyRes).

get_path_total_time(0,[H1],Weight,TimeRes,EnergyRes):-!,
        entrega(H1,_,_,C1,_,TimeRet), cidadeArmazem(C2),
        get_time_and_energy_between_two_cities(C1,C2,Weight,TimeRes1,EnergyRes1),
        verify_charge_final_city(EnergyRes,EnergyRes1,C1,C2,TimeRet,TimeRes2),
        TimeRes is TimeRes1+TimeRes2.

    get_path_total_time(1,[H1],Weight,TR):-!, cidadeArmazem(C1),
        entrega(H1,_,Mass,C2,_,TimeRet),
        get_time_and_energy_between_two_cities(C1,C2,Weight,TimeRes1,EnergyRes1),
        W1 is Weight - Mass,
        get_time_and_energy_between_two_cities(C2,C1,W1,TimeRes2,EnergyRes2),
        calculate_energy_disp(80,EnergyRes1,EnergyDisp1),
        verify_charge(EnergyDisp1,EnergyRes2,C1,C2,TimeRet,TimeCharge,_),
        TR is TimeRes1 + TimeRes2 +TimeCharge.

    get_path_total_time(1,[H1|T],Weight,TR):- cidadeArmazem(C1),
        entrega(H1,_,Mass,C2,_,_),
        get_time_and_energy_between_two_cities(C1,C2,Weight,TimeRes1,EnergySpent),
        W1 is Weight - Mass,
        calculate_energy_disp(80,EnergySpent,EnergyDisp),
        get_path_total_time(0,[H1|T],W1,R2,EnergyDisp),
        TR is TimeRes1 + R2.

    get_path_total_time(0,[H1,H2|T],Weight,TR,EnergyRes):-
        entrega(H1,_,_,C1,_,TimeRet), entrega(H2,_,Mass,C2,_,_),
	get_time_and_energy_between_two_cities(C1,C2,Weight,TimeRes1,EnergyRes1),
        verify_charge(EnergyRes,EnergyRes1,C1,C2,TimeRet,TC,E),
        W1 is Weight - Mass, get_path_total_time(0,[H2|T],W1,R2,E),
        TR is TimeRes1 + R2 + TC.


% 1 - Entrega no armazem ao qual chega mais depressa (tempo)
    getClosestDeliveryByTime(_,[DeliveryId],DeliveryId):-!.

    getClosestDeliveryByTime(Warehouse, [DeliveryId1,DeliveryId2|Tail],IdClosesteDelivery):-
        entrega(DeliveryId1,_,_,NextWarehouse1,_,_),
        entrega(DeliveryId2,_,_,NextWarehouse2,_,_),
        dadosCam_t_e_ta(_,Warehouse,NextWarehouse1,T1,_,_),
        dadosCam_t_e_ta(_,Warehouse,NextWarehouse2,T2,_,_),
        T1 < T2,!,
        getClosestDeliveryByTime(Warehouse,[DeliveryId1|Tail],IdClosesteDelivery).

    getClosestDeliveryByTime(Warehouse, [_,DeliveryId2|Tail],IdClosesteDelivery):-
        getClosestDeliveryByTime(Warehouse,[DeliveryId2|Tail],IdClosesteDelivery).

    % Serve para apagar a delivery que se encontra
    deleteDeliveryIdFromList([DeliveryId|T],DeliveryId,T):-!.
    deleteDeliveryIdFromList([H1|T],DeliveryId,[H1|Rest]):-
        deleteDeliveryIdFromList(T,DeliveryId,Rest).


    % Serve para construir a lista de entregas ordenadas
    getOrderedDeliveryListByTime(_,[IdDelivery],[IdDelivery]):-!.

    getOrderedDeliveryListByTime(Warehouse,DeliveryList,[FastestDeliveryId|Rest]):-
        getClosestDeliveryByTime(Warehouse,DeliveryList,FastestDeliveryId),
        deleteDeliveryIdFromList(DeliveryList,FastestDeliveryId,RemainingDeliveryIdsList),
        entrega(FastestDeliveryId,_,_,NextWarehouse,_,_),
        getOrderedDeliveryListByTime(NextWarehouse,RemainingDeliveryIdsList,Rest).

    timeHeuristic(ListDeliveries,FastestDeliveryList):-
        cidadeArmazem(StartingWarehouse),
        getOrderedDeliveryListByTime(StartingWarehouse,ListDeliveries,FastestDeliveryList).




% 3 - Combinar tempo com massa
    getBiggestRatio(_,[H], H):-!.
    getBiggestRatio(Warehouse,[H1,H2|T],HR):-
        (entrega(Warehouse,_,_,From,_,_); From is Warehouse),
        entrega(H1,_, M1, Id1,_,_), entrega(H2,_, M2, Id2,_,_),
        dadosCam_t_e_ta(_, From, Id1,T1,_,_), dadosCam_t_e_ta(_, From, Id2,T2,_,_),
        Ratio1 is M1/T1, Ratio2 is M2 / T2,
        Ratio1 > Ratio2,!,
        getBiggestRatio(Warehouse,[H1|T], HR).

    getBiggestRatio(Warehouse,[_,H2|T],HR):-
        getBiggestRatio(Warehouse,[H2|T], HR).

    deleteCityFromList([Elem|T],Elem,T):-!.
    deleteCityFromList([H|T],Elem,[H|Rest]):-
        deleteCityFromList(T,Elem,Rest).

    massAndTimeHeuristicAux(_,[H],[H]):-!.
    massAndTimeHeuristicAux(C1,L,[HR|Rest]):-!,
        getBiggestRatio(C1,L,HR), deleteCityFromList(L,HR,LR),
        massAndTimeHeuristicAux(HR,LR,Rest).
    massAndTimeHeuristicAux(C1,L,[HR|Rest]):-
        getBiggestRatio(C1,L,HR),
        deleteCityFromList(L,HR,LR),
        massAndTimeHeuristicAux(HR,LR,Rest).

    massAndTimeHeuristic(ListDeliveries,ListResult):-cidadeArmazem(C1),
        massAndTimeHeuristicAux(C1,ListDeliveries, ListResult).

%distribuicao dos camioes
distribuir_camioes(InputDeliveryIds,EC1,EC2,EC3):-
	buscar_massa_entregas(InputDeliveryIds,HeaviestFirst),
	maplist(nth0(0), HeaviestFirst,InputDeliveryIdsOrd),
	distribuir_camioes2(InputDeliveryIdsOrd,ECAux1,ECAux2,ECAux3),
	length(ECAux1,Dim1),length(ECAux2,Dim2),length(ECAux3,Dim3),
	(retract(entregas(_));true), asserta(entregas(Dim1)),
	ag_varios_camioes(ECAux1), melhor_geracao(EC1Aux2),
	(retract(entregas(_));true), asserta(entregas(Dim2)),
	ag_varios_camioes(ECAux2), melhor_geracao(EC2Aux2),
	(retract(entregas(_));true), asserta(entregas(Dim3)),
	ag_varios_camioes(ECAux3), melhor_geracao(EC3Aux2),
	obter_melhor_da_geracao(EC1Aux2,EC1),
	obter_melhor_da_geracao(EC2Aux2,EC2),
        obter_melhor_da_geracao(EC3Aux2,EC3).

obter_melhor_da_geracao([H|_],H).

distribuir_camioes2([],[],[],[]):-!.

distribuir_camioes2([H],[H],[],[]):-!.

distribuir_camioes2([H,H1],[H],[H1],[]):-!.


distribuir_camioes2([H,H1,H2|T],[H|EC1],[H1|EC2],[H2|EC3]):-
	distribuir_camioes3(T,EC1,EC2,EC3).

distribuir_camioes3([],[],[],[]):-!.
distribuir_camioes3([H],[H],[],[]):-!.

distribuir_camioes3([H,H1],[H],[H1],[]):-!.

distribuir_camioes3([H,H1,H2|T],[H2|EC1],[H1|EC2],[H|EC3]):-
	distribuir_camioes2(T,EC1,EC2,EC3).


buscar_massa_entregas(InputDeliveryIds,HeaviestFirst):-
get_mass_list_by_delivery_id(InputDeliveryIds, Deliveries),
        % Ordena a lista de entregas pelo segundo parametro 'Mass' e retorna uma lista de listas do tipo [ID,Mass]
        sort(2, @>=, Deliveries, HeaviestFirst).

get_mass_list_by_delivery_id([],[]).
    get_mass_list_by_delivery_id([Id|Ids],([[Id,M]|Masses])):-
        entrega(Id,_,M,_,_,_),
        get_mass_list_by_delivery_id(Ids,Masses).

ag_varios_camioes(ListaEntregas):-
	inicializa_dados2(ListaEntregas),
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	tempo(TEMPO),
	gera_geracao(0,NG,TEMPO,PopOrd).

inicializa_dados2(ListaEntregas):-(retract(populacao(_));true), asserta(populacao(ListaEntregas)),
        write('Numero de novas Geracoes: '),read(NG),(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Tempo limite: '),read(TEMPO),(retract(tempo(_));true), asserta(tempo(TEMPO)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(dim_populacao(_));true), asserta(dim_populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100,
	(retract(prob_cruzamento(_));true),	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100,
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

divisao_final(_,[],[],[],[]):-!.
divisao_final(N,[H|L],[H|EC1],EC2,EC3):-
	N =< 5,!,N1 is N+1,divisao_final(N1,L,EC1,EC2,EC3).

divisao_final(N,[H|L],EC1,[H|EC2],EC3):-
	N =< 11,!,N1 is N+1,divisao_final(N1,L,EC1,EC2,EC3).

divisao_final(N,[H|L],EC1,EC2,[H|EC3]):-
	N1 is N+1,divisao_final(N1,L,EC1,EC2,EC3).

