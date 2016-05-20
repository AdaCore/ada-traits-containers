.. highlight:: ada

`Map` data type
===============

A map is an association between two elements (the key and the value).
Knowing the key (most often a `String`) gives fast (near constant
time) access to the corresponding value.

... to be completed

Iteration
---------

There exist several ways to iterate on maps, depending on what piece
of information you need to retrieve.

If you want to retrieve all the keys stored in the map (from which
you can then retrieve the values), you can use::

   declare
      M : Map;
   begin
      for Key of M loop
         ... M.Get (Key)   --  to access the value
      end loop;
   end;

Although retrieving an element from the key is very fast, it is
still a bit slower than to have a more direct access to it. For this,
you could use cursors instead::

   declare
      M : Map;
      C : Cursor := Map.First;
   begin
      while Map.Has_Element (C) loop
         ... M.Key (C)     --  to retrieve the key
         ... M.Element (C) --  to access the value
         Map.Next (C);
      end loop;
   end;

or the simpler::

   declare
      M : Map;
   begin
      for C in M loop
         ... M.Key (C)     --  to retrieve the key
         ... M.Element (C) --  to access the value
      end loop;
   end;

These loops are significantly faster than the previous for-of loop when
retrieving the values (but not faster if you are only interested in the
keys).

Note that the for-of loop differs from that of the standard Ada containers,
in that it returns the keys stored in the map, not its elements. This is
because once you have the key, it is easy and relatively fast to get the
value. However, with the standard Ada containers, once you have the value
there is no convenient way to retrieve the corresponding key.

