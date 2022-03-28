% Diogo Alcobaca, numero 95553

:- [codigo_comum].


% dada uma lista de palavras, devolve uma lista ordenada
% em que cada elemento e uma lista com as letras de cada palavra
obtem_letras_palavras(Lst_Pals,Letras):-
    sort(Lst_Pals,Lst_Pals_Ord),
maplist(atom_chars,Lst_Pals_Ord,Letras).


% devolve um espaco duma fila
espaco_fila(Fila,Esp):-espaco_fila(Fila,0,[],Esp).
% caso em que a fila acaba
espaco_fila(R,Cont,Lst_Aux,Esp):- 
    R==[],
    Cont>=3,
    Esp=Lst_Aux.
% ir acumulando possiveis posicoes para o espaco
espaco_fila([P|R],Cont,Lst_Aux,Esp):- 
    P\=='#',
    Cont_Atlz is Cont+1,
    append(Lst_Aux,[P],Lst_Aux_Atlz),
    espaco_fila(R,Cont_Atlz,Lst_Aux_Atlz,Esp).
% se chegar a uma posicao negra e ha menos de 3 posicoes entao nao e espaco
espaco_fila([P|R],Cont,_,Esp):- 
    P=='#',
    Cont<3,
    espaco_fila(R,0,[],Esp).
% caso contrario, e espaco
espaco_fila([P|_],Cont,Lst_Aux,Esp):- 
    P=='#',
    Cont>=3,
    Esp=Lst_Aux.
% chama o predicado outra vez para detetar os espacos restantes
espaco_fila([P|R],Cont,_,Esp):- 
    P=='#',
    Cont>=3,
    espaco_fila(R,0,[],Esp).


% devolve todos os espacos de uma fila
espacos_fila(Fila,Espacos):-
    bagof(Espaco,espaco_fila(Fila,Espaco),Espacos_Aux),
    !,
    Espacos=Espacos_Aux.
espacos_fila(_,[]).


% devolve todos os espacos horizontais ou todos os espacos verticais de uma grelha
espacos_grelha([],Lst_Aux,Lst_Aux).
espacos_grelha([P|R],Lst_Aux,Espacos):-
    espacos_fila(P,Esp_Aux),
    !,
    append(Lst_Aux,Esp_Aux,Lst_Aux_Atlz),
    espacos_grelha(R,Lst_Aux_Atlz,Espacos).
espacos_grelha([_|R],Lst_Aux,Espacos):-
    espacos_grelha(R,Lst_Aux,Espacos).

% devolve todos os espacos numa grelha
espacos_puzzle(Grelha,Espacos_Total):-espacos_puzzle(Grelha,[],_,Espacos_Total).
espacos_puzzle(Grelha,Lst_Aux,Espacos_Horizts,Espacos_Total):-
    espacos_grelha(Grelha,Lst_Aux,Espacos_Horizts),
    mat_transposta(Grelha,Grelha_Transp),
    espacos_grelha(Grelha_Transp,Espacos_Horizts,Espacos_Total).

    
% verifica se um dado elemento esta dentro da lista dada como argumento
pertence(Elm, [P | _]):-Elm==P.
pertence(Elm, [_ | R]):- pertence(Elm, R).

% verifica se duas listas tem algum membro em comum
membro_comum([P_L1|_],L2):- pertence(P_L1,L2),!.
membro_comum([_|R_L1],L2):-membro_comum(R_L1,L2). 

% devolve os espacos (Esps_Com) da lista de espacos dada como argumento (Espacos) 
% que tem posicoes em comum com o espaco dado como argumento (Esp)
espacos_com_posicoes_comuns(Espacos,Esp,Esps_Com):-
    espacos_com_posicoes_comuns(Espacos,Esp,[],Esps_Com).
espacos_com_posicoes_comuns([],_,Lst_Aux,Lst_Aux).
espacos_com_posicoes_comuns([P_Esps|R_Esps],Esp,Lst_Aux,Esps_Com):-
    P_Esps==Esp,
    !,
    espacos_com_posicoes_comuns(R_Esps,Esp,Lst_Aux,Esps_Com).
espacos_com_posicoes_comuns([P_Esps|R_Esps],Esp,Lst_Aux,Esps_Com):-
    membro_comum(P_Esps,Esp),
    !,
    append(Lst_Aux,[P_Esps],Lst_Aux_Atlz),
    espacos_com_posicoes_comuns(R_Esps,Esp,Lst_Aux_Atlz,Esps_Com).
espacos_com_posicoes_comuns([_|R_Esps],Esp,Lst_Aux,Esps_Com):-
    espacos_com_posicoes_comuns(R_Esps,Esp,Lst_Aux,Esps_Com).


% cria uma copia de um elemento
copia_elemento(E,_):-var(E),!.
copia_elemento(E,E).

% cria uma copia de uma lista
copia_lista(Lst,Copia):-
    maplist(copia_elemento,Lst,Copia).

% devolve true se as duas listas dadas como argumentos tem o mesmo tamanho
mesmo_tamanho(L1,L2):-
    length(L1,T1),
    length(L2,T2),
    T1==T2.

% verifica se todos os elementos de uma palavra e de um espaco de tamanho igual
% unificam um a um. tem uma flag que passa de 0 a 1 depois de verificar que o tamanho
% da palavra e do espaco sao iguais
palavra_possivel_esp_aux(Pal,Esp,0):-
    mesmo_tamanho(Pal,Esp),
    palavra_possivel_esp_aux(Pal,Esp,1),
    !.
palavra_possivel_esp_aux([],[],1).
palavra_possivel_esp_aux([P_Pal|R_Pal],[P_Esp|R_Esp],1):-
    P_Pal=P_Esp,
    palavra_possivel_esp_aux(R_Pal,R_Esp,1).

% verifica se ha pelo menos uma palavra que possa preencher um espaco
espaco_comum_possivel(Esp,[P|_]):-
    palavra_possivel_esp_aux(P,Esp,0),
    !.
espaco_comum_possivel(Esp,[_|R]):-
    espaco_comum_possivel(Esp,R).

% verifica se uma palavra e possivel para um espaco, e sem impedir o 
% preenchimento de todos os outros espacos com a lista de palavras dada
palavra_possivel_esp(Pal,Esp,Espacos,Letras):-
    palavra_possivel_esp_aux(Pal,Esp,0),
    espacos_com_posicoes_comuns(Espacos,Esp,Esps_Com),
    % criacao de uma copia dos espacos com posicoes comuns ao espaco dado
    % de forma a nao alterar esses espacos na grelha original
    maplist(copia_lista,Esps_Com,Esps_Com_Copia),
    palavra_possivel_esp(Esps_Com_Copia,Letras).
palavra_possivel_esp([],_).
palavra_possivel_esp([P|R],Letras):-
    espaco_comum_possivel(P,Letras),
    palavra_possivel_esp(R,Letras).


% devolve todas as palavras possiveis para um espaco
palavras_possiveis_esp(Letras,Espacos,Esp,Pals_Possiveis):-
    findall(Pal,(member(Pal,Letras),palavra_possivel_esp(Pal,Esp,Espacos,Letras)),Pals_Possiveis).


% devolve todas as palavras possiveis para todos os espacos da grelha
palavras_possiveis(Letras,Espacos,Pals_Possiveis):-
    bagof([Esp,Pals_Possiveis_Esp],(member(Esp,Espacos),palavras_possiveis_esp(Letras,Espacos,Esp,Pals_Possiveis_Esp)),Pals_Possiveis).


% verifica se todas as palavras na lista de palavras tem,
% para uma determinada posicao, a mesma letra
letras_comuns_aux(Lst_Palavras,Index,Char):-
findall(Elm,(member(Pal,Lst_Palavras),nth1(Index,Pal,Elm)),Lst_Elms),
% o sort elemina elementos repetidos, logo, se todas as palavras tiverem 
% a mesma letra numa certa posicao,  a lista resultante so tera um elemento
sort(Lst_Elms,Elm_Comum),
length(Elm_Comum,1),
nth1(1,Elm_Comum,Char).


% vai encontrar a menor palavra da lista de modo a que o numero de letras por
% palavra a ser procurado nao ultrapassar o da menor palavra
letras_comuns(Lst_Palavras,Letras_Comuns):-
    findall(Tamanho,(member(Pal,Lst_Palavras),length(Pal,Tamanho)),Lst_Tamanhos),
    sort(Lst_Tamanhos,Lst_Tamanhos_Ord),
    nth1(1,Lst_Tamanhos_Ord,Tam_Min),
    Cont_Max is Tam_Min+1,
    letras_comuns(Lst_Palavras,1,Cont_Max,[],Letras_Comuns),
    !.

% devolve uma lista de pares (pos,ltr) que significa que todas as palavras
% da lista dada contem a letra ltr na posicao pos
letras_comuns(_,Cont_Max,Cont_Max,Lst_Aux,Lst_Aux).
letras_comuns(Lst_Palavras,Cont,Cont_Max,Lst_Aux,Letras_Comuns):-
    letras_comuns_aux(Lst_Palavras,Cont,Char),
    !,
    append(Lst_Aux,[(Cont,Char)],Lst_Aux_Atlz),
    Cont_Atlz is Cont+1,
    letras_comuns(Lst_Palavras,Cont_Atlz,Cont_Max,Lst_Aux_Atlz,Letras_Comuns).
letras_comuns(Lst_Palavras,Cont,Cont_Max,Lst_Aux,Letras_Comuns):-
    Cont_Atlz is Cont+1,
    letras_comuns(Lst_Palavras,Cont_Atlz,Cont_Max,Lst_Aux,Letras_Comuns).


% atribui a um espaco as letras comuns a todas 
% as palavras possiveis para esse espaco
atribui_comuns_esp(Pals_Possiveis):-
    nth1(1,Pals_Possiveis,Esp),
    nth1(2,Pals_Possiveis,Lst_Pals),
    letras_comuns(Lst_Pals,Letras_Comuns),
    atribui_comuns_esp(Esp,Letras_Comuns),
    !.
atribui_comuns_esp(_,[]).
atribui_comuns_esp(Esp,[P|R]):-
    P=(Index,Letra),
    nth1(Index,Esp,Letra),
    atribui_comuns_esp(Esp,R).

% faz o mesmo que atribui_comuns_esp, 
% mas para todos os espacos da grelha
atribui_comuns(Pals_Possiveis):-
    maplist(atribui_comuns_esp,Pals_Possiveis).


% retira palavras impossiveis para um
% espaco da lista de palavras associda
retira_impossiveis_esp(Pals_Possiveis,Novas_Pals_Possiveis):-
    nth1(1,Pals_Possiveis,Esp),
    nth1(2,Pals_Possiveis,Lst_Pals),
    exclude(\=(Esp),Lst_Pals,Lst_Pals_Atlz),
    append([Esp],[Lst_Pals_Atlz],Novas_Pals_Possiveis).

retira_impossiveis(Pals_Possiveis,Novas_Pals_Possiveis):-
    maplist(retira_impossiveis_esp,Pals_Possiveis,Novas_Pals_Possiveis).


% devolve uma lista com todas as palavras que, para um determinado 
% espaco, sao a unica palavra possivel para dito espaco
obtem_unicas(Esp_Pals_Possiveis,Unicas):-
    maplist(nth1(2),Esp_Pals_Possiveis,Pals_Possiveis),
    findall(Pal,(member(Pal,Pals_Possiveis),length(Pal,1)),Pals_Unicas),
    append(Pals_Unicas,Unicas).


% devolve true se uma dada lista nao contiver um dado elemento
nao_existe(Elm,Lst):-
    length(Lst,Tamanho_Inicial),
    exclude(==(Elm),Lst,Lst_Atlz),
    length(Lst_Atlz,Tamanho_Final),
    Tamanho_Inicial==Tamanho_Final.

% retira as palavras unicas geradas por obtem_unicas das listas
% de palavras que contem mais que uma palavra
retira_unicas_esp(Unicas,Pals_Possiveis,Novas_Pals_Possiveis):-
    nth1(1,Pals_Possiveis,Esp),
    nth1(2,Pals_Possiveis,Lst_Pals),
    length(Lst_Pals,Tamanho),
    Tamanho>1,
    !,
    findall(Pal,(member(Pal,Lst_Pals),nao_existe(Pal,Unicas)),Lst_Pals_Atlz),
    append([Esp],[Lst_Pals_Atlz],Novas_Pals_Possiveis).
retira_unicas_esp(_,Pals_Possiveis,Novas_Pals_Possiveis):-
    Novas_Pals_Possiveis=Pals_Possiveis.

retira_unicas(Pals_Possiveis,Novas_Pals_Possiveis):-
    obtem_unicas(Pals_Possiveis,Unicas),
    maplist(retira_unicas_esp(Unicas),Pals_Possiveis,Novas_Pals_Possiveis).


% simplifica uma lista de palavras possiveis atraves dos
% predicados anteriores e segundo as regras de simplificacao
simplifica(Pals_Possiveis,Novas_Pals_Possiveis):-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis,Pals_Sem_Impossiveis),
    retira_unicas(Pals_Sem_Impossiveis,Pals_Unicas),
    simplifica(Pals_Possiveis,Pals_Unicas,Novas_Pals_Possiveis).

% se atraves do processo de simplificacao nao tiverem occorido 
% alteracoes entao parar, caso contrario, continuar a simplificar
simplifica(Pals_Possiveis,Pals_Unicas,Novas_Pals_Possiveis):-
    Pals_Possiveis==Pals_Unicas,
    !,
    Novas_Pals_Possiveis=Pals_Unicas.
simplifica(_,Pals_Unicas,Novas_Pals_Possiveis):-
    simplifica(Pals_Unicas,Novas_Pals_Possiveis).


% dado um puzzle, devolver a lista de palavras 
% possiveis simplificadas para esse puzzle
inicializa(Puzzle,Pals_Possiveis):-
    nth1(1,Puzzle,Lst_Palavras),
    nth1(2,Puzzle,Grelha),
    obtem_letras_palavras(Lst_Palavras,Letras),
    espacos_puzzle(Grelha,Espacos),
    palavras_possiveis(Letras,Espacos,Pals_Possiveis_Aux),
    simplifica(Pals_Possiveis_Aux,Pals_Possiveis).


% ve se o numero de palavras associadas a um determinado espaco e maior que um
% dado numero (flag \== 0), ou se e exatamente igual ao um dado numero (flag == 0)
escolhe_menos_alternativas_esp(Pals_Possiveis,Cont,Flag_Cmp):-
    Flag_Cmp==0,
    !,
    nth1(2,Pals_Possiveis,Lst_Pals),
    length(Lst_Pals,Cont).
escolhe_menos_alternativas_esp(Pals_Possiveis,Cont,_):-
    nth1(2,Pals_Possiveis,Lst_Pals),
    length(Lst_Pals,Tamanho),
    Tamanho>Cont.

% devolve o primeiro espaco da grelha cuja lista de palavras e nao unitaria
escolhe_menos_alternativas(Pals_Possiveis,Escolha):-
    % devolver logo false caso todos os espacos tenham listas unitarias de palavras associadas
    bagof(Pal,(member(Pal,Pals_Possiveis),escolhe_menos_alternativas_esp(Pal,1,1)),_),
    escolhe_menos_alternativas(Pals_Possiveis,2,Escolha).
% obter o primeiro de todos os espacos com 2 palavras
% se nao existirem espacos com duas, pesquisar por 3, e assim adiante
escolhe_menos_alternativas(Pals_Possiveis,Cont,Escolha):-
    bagof(Pal,(member(Pal,Pals_Possiveis),escolhe_menos_alternativas_esp(Pal,Cont,0)),Pals_Possiveis_Atlz),
    !,
    nth1(1,Pals_Possiveis_Atlz,Escolha).
escolhe_menos_alternativas(Pals_Possiveis,Cont,Escolha):-
    Cont_Atlz is Cont+1,
    escolhe_menos_alternativas(Pals_Possiveis,Cont_Atlz,Escolha).


% dado um elemento Escolha (escolhido pelo predicado anterior) da grelha,
% unifica uma das suas palavras possiveis com o seu espaco, remove
% as restantes palavras possiveis da sua lista e, insere este elemento
% Escolha, agora modificado, de novo na grelha, substituindo o original
experimenta_pal(Escolha,Pals_Possiveis,Novas_Pals_Possiveis):-
    nth1(1,Escolha,Esp),
    nth1(2,Escolha,Lst_Pals),
    member(Pal,Lst_Pals),
    experimenta_pal(Pals_Possiveis,Esp,Pal,[],Novas_Pals_Possiveis).

% reconstroi a grelha, agora com o elemento Escolha modificado
experimenta_pal([],_,_,Lst_Aux,Lst_Aux).
experimenta_pal([P|R],Esp,Pal,Lst_Aux,Novas_Pals_Possiveis):-
    nth1(1,P,Esp_Aux),
    Esp_Aux==Esp,
    !,
    Esp_Aux=Pal,
    nth1(2,P,Lst_Pals),
    include(==(Pal),Lst_Pals,Lst_Pals_Atlz),
    append([Esp],[Lst_Pals_Atlz],P_Atlz),
    append(Lst_Aux,[P_Atlz],Lst_Aux_Atlz),
    experimenta_pal(R,Esp,Pal,Lst_Aux_Atlz,Novas_Pals_Possiveis).
experimenta_pal([P|R],Esp,Pal,Lst_Aux,Novas_Pals_Possiveis):-
    append(Lst_Aux,[P],Lst_Aux_Atlz),
    experimenta_pal(R,Esp,Pal,Lst_Aux_Atlz,Novas_Pals_Possiveis).


% simplifica uma lista de palavras possiveis, ja inicializada, que
% ainda tem posicoes por preencher, atraves dos predicados anteriores
resolve_aux(Pals_Possiveis,Novas_Pals_Possiveis):-
    escolhe_menos_alternativas(Pals_Possiveis,Escolha),
    !,
    experimenta_pal(Escolha,Pals_Possiveis,Pals_Experimenta),
    simplifica(Pals_Experimenta,Pals_Simplifica),
    resolve_aux(Pals_Simplifica,Novas_Pals_Possiveis),
    !.
    

% quando escolhe_menos_alternativas devolver false e porque todos os 
% espacos ja so tem uma palavra possivel cada, logo a lista esta resolvida
resolve_aux(Pals_Possiveis,Novas_Pals_Possiveis):-
    Pals_Possiveis=Novas_Pals_Possiveis.

% resolve o puzzle dado atraves dos predicados de resolucao desenvolvidos
resolve(Puz):- resolve(Puz,_).
resolve(Puz,Puz_Res):-
    inicializa(Puz,Puz_Inicializado),
    resolve_aux(Puz_Inicializado,Puz_Res).