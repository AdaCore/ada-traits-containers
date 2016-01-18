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
