Design considerations
=====================

During the design of this library, there were quite a number of design
decisions that were taken and might not be immediately obvious.

This chapter documents these decisions, and perhaps the limitations they bring,
or the flexibility they offer.

This chapter is really about the internal design of the library, and can be
skipped if you are only interested in using the final product.

Nested generics
----------------

As we have seen, this library makes extensive use of generics, and generics
that receive other generics as parameters. This makes the library sometimes
harder to instantiate than one might like.

.. highlight:: ada

Let's take an example. A we saw, a lot of graph algorithms need to store
information in the vertex, like a color to know whether they were already
visited or not. These properties are stored in what we call a property
map. So one approach would be to take both a graph and a property map,
each defined independently of each other::

   generic
      type Graph is private;
      type Vertex is private;
   package Graph_Traits is
   end Graph_Traits;

   generic
      with package Graphs is new Graph_Traits (<>);
      type Key is private;
      type Value is private;
      with function Get (G : Graphs.Graph; K : Key) return Value;
   package Property_Maps is
   end Property_Maps;

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Property_Maps (<>);
   procedure Some_Algo;

Such a description however is useless to us. It is not possible to specify that
the `Color_Maps` is applied to a `Graphs.Vertex` and stores `Color`. So we
cannot in effect use the `Maps` in our algorithm.

What we really would like is a partial constraint applied to the formal
parameters, as in the following invalid Ada code::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Property_Maps
         (Graphs, Graphs.Vertex, Color, <>);   --  Invalid Ada
   procedure Some_Algo;

where the exact implementation of `Get` is unspecified, but we still want to
associate a `Color` with a `Vertex`.

Instead we have to use a slightly more complex definition for the property
maps, which helps us define parts of the parameter, and leave some others
unspecified::

   generic
      type Graph is private;
      type Key is private;
      type Value is private;
   package Property_Maps is
      generic
         with function Get (G : Graph; K : Key) return Value;
      package Map is
      end Map;
   end Property_Maps;

   generic
      type Graph is private;
      type Vertex is private;
   package Graph_Traits is
      package Color_Maps is new Property_Maps (Graph, Vertex, Color);
   end Graph_Traits;

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Graphs.Color_Maps (<>);
   procedure Some_Algo;

At this point, the compiler knows that Maps will be a pakage that provides a
`Get` function, that maps our `Vertex` to a `Color` appropriately.  This comes
at the cost of one extra level of instantiation (the `Color_Maps` in the
`Graph_Traits` package).

Pre and post conditions
-----------------------

This library is meant to be (at least for a subset) usable for SPARK users that
want to prove their code. To do so, the API needs to set pre and post conditions
on the relevant subprograms.

One of the difficulties, however, is that some of the preconditions need to be
computed. For instance, a tree layout algorithm would not work on a graph with
cycle. As it happens, there is an algorithm (based on depth-first-search) to
compute whether a graph has cycles.

This is however a generic algorithm, which needs to be instantiated with a
description of what a graph, vertex and edge are (the `Graph_Traits`), as
well as a `Color_Property_Map`. So we have the following::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is
         new Graphs.Color_Property_Maps.Exterior (<>);
   function Is_Acyclic (G : Graphs.Graph) return Booolean;

   generic
      with package Graphs is new Graph_Traits (<>);
   procedure Tree_Layout (G : Graphs.Graph);

However, as it is we cannot use Is_Acyclic in a precondition, since we have
no instance of it.

The only solution therefore is to pass the instance of Is_Acyclic directly
as a formal parameter. The following invalid code would be interesting::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is
         new Graphs.Color_Property_Maps.Exterior (<>);
   function Is_Acyclic (G : Graphs.Graph) return Boolean;

   --  Invalid Ada
   generic
      with package Graphs is new Graph_Traits (<>);
      with function Acyclic is new Is_Acylic (Graphs, <>);
   procedure Tree_Layout (G : Graphs.Graph)
      with Pre => Acyclic (G);

This is however invalid for two reasons:

  - As we saw before, we cannot use partial instantiation, so we need to also
    have an instance of `Color_Property_Maps` as a parameter::

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
       function Is_Acyclic (G : Graphs.Graph) return Boolean;

       --  Invalid Ada
       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
          with function Acyclic is new Is_Acylic (Graphs, Maps);
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic (G);

  - In fact, as opposed to formal packages, Ada does not let us indicate that a
    formal parameter is an instance of a specific generic subprogram.

    The simpler approach would be to let any value for `Acyclic`, as in::

       generic
          with package Graphs is new Graph_Traits (<>);
          with function Acyclic (G : Graphs.Graph) return Boolean;
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic (G);

    The main drawback here, though, is that the user could pass anything
    function as an actual parameter, so the algorithm does not know for a fact
    that it did receive an acyclic graph. So a better solution is to use a
    generic package instead::

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
       package Is_Acyclic is
          function Acyclic (G : Graphs.Graph) return Boolean;
       end Is_Acyclic;

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
          with package Acyclic is new Is_Acylic (Graphs, Maps);
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic.Acyclic (G);

    Since this all makes the instantiation of `Tree_Layout` harder, even for
    users that do not need the preconditions because they do not run them
    or prove their code. So the final idea is to have two versions of
    `Tree_Layout`: one without the preconditions, as we saw in the first
    example in this section, and one with the preconditions as we just saw,
    which instantiates and run the first version.

    That way, we have no code duplication, and yet have full preconditions
    for users who need them.


