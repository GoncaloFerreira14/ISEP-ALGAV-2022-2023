consult('BC.pl').
% consult('Predicados.pl').

% 1a. Gerar todas as trajetórias possíveis.

    % Retorna o nome da cidade destino com base no ID da entrega
	get_city_by_delivery_id(Id,CityName):-
        entrega(Id,_,_,WarehouseId,_,_),
        idArmazem(CityName,WarehouseId).

    % Retorna lista de nomes das cidades destino com base numa lista de IDs de entrega
	get_cities_list_by_delivery_id([],[]).
	get_cities_list_by_delivery_id([H|T],[HR|Rest]):-
        get_city_by_delivery_id(H,HR),
        get_cities_list_by_delivery_id(T,Rest).

    % Retorna lista de permutacoes das varias solucoes de trajetorias possiveis com base numa lista de IDs de entrega
	all_deliveries_paths(LD,LR):-
        get_cities_list_by_delivery_id(LD,LC),
        cidadeArmazem(Id),
        idArmazem(A,Id),
		findall(Res,(permutation(LC,ResAux), append([A],ResAux,ResAux2),append(ResAux2,[A],Res)),LR).

% Testar com (Ex1): all_deliveries_paths([4439,4438,4445,4443,4449],LP).


% 1b. Avaliar essas trajetórias de acordo com o tempo para completar todas as
% entregas e voltar ao armazém base de Matosinhos e escolher a solução que
% permite a volta do camião mais cedo.

  % Realiza regra de tres simples para calcular o tempo/energia de um trajeto com base no peso total atual
    calculate_cost(Weight,Value,Res):- Res is ((Weight*Value)/11800).

    % Calcula a energia disponivel no camiao apos realizar um trecho
    calculate_energy_disp(EActual,Energy,EnergyDisp):-EnergyDisp  is EActual - Energy.

    % Calcula o tempo necessario para carregar a bateria ate aos 80% (64kWh)
    battery_time_charge(EActual,ENeeded,C1,C2,TC,E):-
        TCAux is (64-EActual) * (60/48),check_energy_greater_than_16(64,ENeeded,C1,C2,TAux,E),
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
        (ENeeded >EActual,!, TCAux is (ENeeded+16-EActual)*(60/48));
        ((16> ENeeded,!,TCAux is (16-ENeeded) * (60/48));(TCAux is (EActual+16-ENeeded) * (60/48))),TC is TCAux.

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

    % Predicado auxiliar para obter o caminho mais rapido
    get_fastest_path_auxiliar([L],W,L,Time):-!,
         get_path_total_time(1,L,W,Time).
    get_fastest_path_auxiliar([H1,H2|T],Weight,L,Time):-
         get_path_total_time(1,H1,Weight,R1),
         get_path_total_time(1,H2,Weight,R2),((R1 < R2, !,
        get_fastest_path_auxiliar([H1|T],Weight,L,Time));get_fastest_path_auxiliar([H2|T],Weight,L,Time)).

    % Retorna o caminho mais rapido
    all_deliveries_fastest_path(LD,LR,Time):-
        get_time(Ti),
        get_total_weight_delivery_list(LD,DeliveryWeight),
        TotalWeight is DeliveryWeight + 7500,
        findall(Res,permutation(LD,Res),AllLists),
        get_fastest_path_auxiliar(AllLists,TotalWeight,IdDelFastestPath,Time),
        get_cities_list_by_delivery_id(IdDelFastestPath,LR),
        get_time(Tf),
        TSol is Tf-Ti,
        write('Tempo de geracao da solucao = '), write(TSol).


% 1d. Heurísticas (3) que possam rapidamente gerar uma solução (não
% necessariamente a melhor) e avaliar a qualidade dessas heurísticas.
% Por exemplo, entregar no armazém mais próximo; efetuar de seguida a entrega com
% maior massa; combinar distância para a entrega com massa entregue).

% 1 - Entrega no armazem ao qual chega mais depressa (tempo)

    % Vai buscar a delivery que demora menos tempo de acordo com a cidade em que se encontra
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

    timeHeuristic(ListDeliveries, ResultCityPath, Time):-
        cidadeArmazem(StartingWarehouse),
        getOrderedDeliveryListByTime(StartingWarehouse,ListDeliveries,FastestDeliveryList),
        get_total_weight_delivery_list(FastestDeliveryList, Weight),
        TotalWeight is Weight + 7500,
        get_path_total_time(1,FastestDeliveryList,TotalWeight,Time),
        get_cities_list_by_delivery_id(FastestDeliveryList,ResultCityPath).


% 2 - Entrega no armazem no qual podemos libertar maior massa da entrega

    get_mass_list_by_delivery_id([],[]).
    get_mass_list_by_delivery_id([Id|Ids],([[Id,M]|Masses])):-
        entrega(Id,_,M,_,_,_),
        get_mass_list_by_delivery_id(Ids,Masses).

    deliveries_sorted_by_heaviest(InputDeliveryIds, DeliveryPath, TR) :-
        % Encontra todas as entregas e armazena numa lista chamada 'Deliveries'
        get_mass_list_by_delivery_id(InputDeliveryIds, Deliveries),
        % Ordena a lista de entregas pelo segundo parametro 'Mass' e retorna uma lista de listas do tipo [ID,Mass]
        sort(2, @>=, Deliveries, HeaviestFirst),
        % Retorna a lista apenas com o primeiro elemento que corresponde aos IDs das entregas
        maplist(nth0(0), HeaviestFirst, DeliveryIds),
        % Retorna o caminho com os nomes das cidades com base na lista de IDs de entrega resultante anterior
        get_cities_list_by_delivery_id(DeliveryIds, DeliveryPath),

        % Busca peso das entregas
        get_total_weight_delivery_list(DeliveryIds, W),
        Weight is W + 7500,

        % Retorna o tempo que demora a fazer as entregas por ordem descendente de massas
        get_path_total_time(1, DeliveryIds, Weight, TR).

    % Testar com: deliveries_sorted_by_heaviest([4439,4438,4445,4443,4449],Path).


% 3 - Combinar tempo com massa

    getWarehouseIdListByDeliveryIds([],[]):-!.
    getWarehouseIdListByDeliveryIds([H|T],[IdWarehouse|Rest]):-
        entrega(H,_,_,IdWarehouse,_,_),
        getWarehouseIdListByDeliveryIds(T,Rest).

    getWarehouseNameListByWarehouseId([],[]):-!.
    getWarehouseNameListByWarehouseId([H|T],[WarehouseName|Rest]):-
        idArmazem( WarehouseName, H),
        getWarehouseNameListByWarehouseId(T,Rest).

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

    massAndTimeHeuristic(ListDeliveries, WarehouseNamesList, TotalTime):-cidadeArmazem(C1),
        massAndTimeHeuristicAux(C1,ListDeliveries, ListResult),
        get_total_weight_delivery_list(ListResult, Weight),
        TotalWeight is Weight+7500,
        get_path_total_time(1,ListResult,TotalWeight,TotalTime),
        getWarehouseIdListByDeliveryIds(ListResult,WarehouseIDList),
        getWarehouseNameListByWarehouseId(WarehouseIDList, WarehouseNamesList).

    % Testar com: massAndTimeHeuristic([4439,4438,4445,4443,4449],L).
