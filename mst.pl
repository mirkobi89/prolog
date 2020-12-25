
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic vertex_key/4.
:- dynamic vertex_previous/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.

graph(g1).
vertex(g1, v1).
vertex(g1, v2).
vertex(g1, v3).
vertex(g1, v4).
arc(g1 ,v1, v2, 5).
arc(g1, v1, v3, 3).
arc(g1, v2, v4, 4).
arc(g1, v4, v3, 1).
arc(g1, v3, v1, 2).

%% creazione heap prova

heap_entry(h1, 1, 16, a).
heap_entry(h1, 2, 2, b).
heap_entry(h1, 3, 4, c).
heap_entry(h1, 4, 3, d).
heap_entry(h1, 5, 7, e).
heap_entry(h1, 6, 10, f).
heap_entry(h1, 7, 9, g).
heap_entry(h1, 8, 8, h).
heap_entry(h1, 9, 14, i).
heap_entry(h1, 10, 1, l).


new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :- retract(graph(G)).

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

graph_vertices(G, Vs) :- findall(V, vertex(G, V), Vs).
list_vertices(G) :- graph(G), listing(vertex(G, _)).

new_arc(G, U, V, Weight) :- arc(G, U, V, Weight).
new_arc(G, U, V, Weight) :- assert(arc(G, U, V, Weight)).
new_arc(G, U, V) :-new_arc(G, U, V, 1).

graph_arcs(G, Es) :- findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es).

vertex_neighbors(G, V, Ns) :- vertex(G, V), findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

adjs(G, V, Vs) :- vertex(G, V), findall(U, arc(G, V, U, _), Vs).

list_arcs(G) :- graph(G), listing(arc(G, _, _, _)).
list_graph(G) :- graph(G), list_arcs(G), list_vertices(G).

split(G, []),!.
split(G, [X | Rows]) :-
    update_database(G,X), split(G, Rows),!.

update_database(G,X) :-
    arg(1, X, U),
    new_vertex(G,U),
    arg(2, X, V),
    new_vertex(G,V),
    arg(3, X, W),
    new_arc(G,U,V,W).
read_graph(G, FileName) :- csv_read_file(FileName, Rows,
                                         [functor(arc),separator(0'\t)]),
			   split(G,Rows).

arcs_format([], []) :- !.

arcs_format([X | Xs], [Y | Ys]) :-
    X = arc(G, U, V, W),
    Y = arc(U, V, W),
    arcs_format(Xs, Ys),
    !.


write_graph(G, FileName) :- write_graph(G, FileName, graph).
write_graph(G, FileName, graph) :-
    findall(arc(V, N, W), arc(G, V, N, W), Arcs),
    csv_write_file(FileName, Arcs, [functor(arc), separator(0'\t)]).

%write_graph(G, FileName, graph) :-
%    findall(arc(V, N, W), arc(G, V, N, W), Arcs),
%    csv_write_file(FileName, Arcs, [functor(arc), separator(0'\t)]).

write_graph(G, FileName, Type):-
    arcs_format(G, Rows),
    csv_write_file(FileName, Rows, [functor(arc), separator(0'\t)]).
				  
%write_graph(G, FileName, Type) :- Type = 'graph',
%    list_arcs(G).
%write_graph(G, FileName, Type) :- Type = 'edges', write(G).

mst_prim_(G, Source).
mst_get(G, Source, PreorderTree).

% esempio di query
% mst_prim(nome_grafo, nodo_sorgente), mst_get(nome_grafo, nodo_sorgente, MST).

%MINHEAP
%S è la dimensione corrente dello heap
new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.
new_heap(H,S) :- assert(heap(H, S)), !.

min_heapify(H, P) :- heap_has_size(H, S), P = S, !.

min_heapify(H, P) :-
    R is (2*P)+1,
    heap_has_size(H, S), R =< S,
    heap_entry(H, P, K1, _V1),
    heap_entry(H, R, K2, _V2),
    K1 =< K2, !.

min_heapify(H, P) :-
    L is 2*P,
    heap_has_size(H, S), L =< S, 
    heap_entry(H, P, K1, V1),
    heap_entry(H, L, K2, V2),
    K1 > K2,
    retract(heap_entry(H, P, K1, V1)),
    retract(heap_entry(H, L, K2, V2)),
    assert(heap_entry(H, P, K2, V2)),
    assert(heap_entry(H, L, K1, V1)),
    min_heapify(H, L).

build_heap(_H, 0) :- !.
build_heap(H, S) :-
    min_heapify(H, S),
    S1 is S-1,
    build_heap(H, S1).


%% heapify(_H, K) :- K = 1, !.
%% heapify(H, K) :- heap_entry(H, K, K1, _V1),Div is floor(K/2), heap_entry(H, Div, K2, _V2),
%%                      K1 >= K2, !.
%% heapify(H, K) :- heap_entry(H, K, K1, V1),Div is floor(K/2), heap_entry(H, Div, K2, V2), K1 < K2,
%%                  retract(heap_entry(H, K, K1, V1)), retract(heap_entry(H, Div, K2, V2)),
%%                  assert(heap_entry(H, K, K2, V2)), assert(heap_entry(H, Div, K1, V1)), heapify(H, Div).
    
delete_heap(H) :-retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).

heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap_has_size(H,0).

heap_not_empty(H) :- heap_has_size(H,S), S>0.

heap_head(H, K, V) :- heap_entry(H, 1, K, V).

heap_insert(H, K, V) :-
    heap_has_size(H,S),
    S1 is S+1,
    modify_key(H, NewKey, OldKey, V).    

heap_extract(H, K, V) :-
    heap_has_size(H, S), S>1,
    S1 is S-1,
    retract(heap_entry(H, 1, K1, V1)),
    retract(heap(H, S)),
    new_heap(H, S1),
    min_heapify(H, 1).

modify_key(H, NewKey, OldKey, V).
    

list_heap(H) :- listing(heap_entry(H, _, _, _)).
