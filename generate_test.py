#!/usr/bin/env python
"""
This script is used to generate test for the various container combinations.
"""


class Comments(object):
    def __init__(self, **kwargs):
        self.comments = kwargs

    def __getattr__(self, key):
        return self.comments.get(key, '')


class List(object):
    type = "List"

    def __init__(
        self,
        elem_type,   # "integer", "string",...
        base,        # "controlled", "limited", ...
        definite,    # "definite", "indefinite", ...
        nodes,       # "bounded", "unbounded", ...
        instance,    # instantiation for the container "package Container is ..."
        withs,       # extra withs for the body
        comments=None, # instance of Comments
        favorite=False # Whether this should be highlighted in the results
    ):

        # We use two default strings (one short, one long), to test various
        # approaches of storing elements

        if elem_type.lower() == "integer":
            category = '%s %s' % (elem_type, self.type)
            append = "V2.Append (C);"
            expected = "2"

        elif elem_type.lower() == "string":
            category = '%s %s' % (elem_type, self.type)
            expected = "Items_Count"
            append = """
         if C mod 2 = 0 then
             V2.Append ("foo");
         else
             V2.Append ("foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo");
         end if;"""

        elif elem_type.lower() == "unbounded_string":
            category = 'String %s' % (self.type, )
            expected = "Items_Count"
            append = """
         if C mod 2 = 0 then
             V2.Append (To_Unbounded_String ("foo"));
         else
             V2.Append (To_Unbounded_String
                ("foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo"));
         end if;"""
        else:
            raise Exception("Unknown element type: %s" % elem_type)

        self.args = dict(
            base=base,
            definite=definite,
            nodes=nodes,
            expected=expected,
            category=category,
            type=self.type,
            elem_type=elem_type,
            instance=instance,
            withs=withs,
            copy='',
            call_count_if='',
            discriminant='',
            favorite=favorite,
            comments=comments or Comments(),
            clear='',       # Explicit clear the container
            clear_copy='',  # Explicit clear the copy of the container
            prefix='',      # Prefix for Element, Next and Has_Element
            adaptors='',    # Creating adaptors for standard containers
            append=append)

        if self.args['base'].lower() == "limited":
            # Need an explicit copy, since ":=" is not defined for limited types
            self.args['copy'] = '.Copy'
            self.args['clear'] = '\n      V.Clear;'
            self.args['clear_copy'] = '\n         V_Copy.Clear;'

        if self.args['nodes'].lower() == "bounded":
            self.args['discriminant'] = ' (Capacity => Items_Count)'

        self.args['test_name'] = \
            "{type}_{base}_{definite}_{nodes}_{elem_type}".format(**self.args)

    def __common(self):
        filename = self.args['test_name'].lower()

        ads = open("tests/generated/%s.ads" % filename, "w")
        ads.write("""
with Report; use Report;
pragma Style_Checks (Off);
procedure {test_name}
   (Stdout : not null access Output'Class);
            """.format(**self.args))
        ads.close()

        adb = open("tests/generated/%s.adb" % filename, "w")
        adb.write("""
{withs}
pragma Style_Checks (Off);
pragma Warnings (Off, "unit * is not referenced");
with Perf_Support;  use Perf_Support;
with Conts.Algorithms;
with Conts.Cursors.Adaptors;
pragma Warnings (On, "unit * is not referenced");
procedure {test_name}
   (Stdout : not null access Output'Class)
is
   {instance}
   use Container;{adaptors}

   procedure Run (V2 : in out Container.{type}'Class);
   --  Force dynamic dispatching for the container (if relevant), as a
   --  a way to check we do not waste time there.

   procedure Run (V2 : in out Container.{type}'Class) is
      It : Container.Cursor;
      Co : Natural;
   begin
      Stdout.Start_Test ("fill", "{comments.fill}");
      for C in 1 .. Items_Count loop
         {append}
      end loop;
      Stdout.End_Test;

      Stdout.Start_Test ("copy", "{comments.copy}");
      declare
         V_Copy : Container.{type}'Class := V2{copy};
         pragma Unreferenced (V_Copy);
      begin
         --  Measure the time before we destroy the copy
         Stdout.End_Test;{clear_copy}
      end;

      Co := 0;
      Stdout.Start_Test ("cursor loop", "{comments.cursorloop}");
      It := V2.First;
      while {prefix}Has_Element (It) loop
         if Predicate ({prefix}Element (It)) then
            Co := Co + 1;
         end if;
         It := {prefix}Next (It);
      end loop;
      Stdout.End_Test;
      Assert (Co, {expected});

      Co := 0;
      Stdout.Start_Test ("for-of loop", "{comments.forofloop}");
      for E of V2 loop
         if Predicate (E) then
            Co := Co + 1;
         end if;
      end loop;
      Stdout.End_Test;
      Assert (Co, {expected});

      Stdout.Start_Test ("count_if", "{comments.countif}");
      Co := Count_If (V2, Predicate'Access);
      Stdout.End_Test;
      Assert (Co, {expected});
   end Run;

begin
   Stdout.Start_Container_Test
      ("{base}", "{definite}", "{nodes}", "{category}", {favorite});
   for C in 1 .. Repeat_Count loop
      declare
         V : Container.{type}{discriminant};
      begin
         Stdout.Save_Container_Size (V'Size / 8);  --  in bytes
         Run (V);{clear}
      end;
   end loop;
   Stdout.End_Container_Test;
end {test_name};
""".format(**self.args))

        adb.close()

    def gen(self, use_cursor_convert=False):
        """
        Generate tests for the new containers
        """
        self.args['prefix'] = 'V2.'

        # When using reference types
        if use_cursor_convert:
            self.args['adaptors'] = ("""
       function Count_If is new Conts.Algorithms.Count_If_Convert
          (Container.Cursors_Forward_Convert);""").format(**self.args)
        else:
            self.args['adaptors'] = ("""
       function Count_If is new Conts.Algorithms.Count_If
          (Container.Cursors.Constant_Forward);""").format(**self.args)

        self.__common()

    def gen_ada2012(self,
                    disable_checks=False,
                    use_cursor_convert=False,
                    adaptors='{type}_Adaptors'):
        """
        Generate tests for the Ada 2012 containers
        """

        if use_cursor_convert:
            self.args['adaptors'] = (
            """
       package Adaptors is new Conts.Cursors.Adaptors.""" +
            adaptors + """ (Container);
       function Count_If is new Conts.Algorithms.Count_If_Convert
          (Adaptors.Cursors_Forward_Convert);
""").format(**self.args)

        else:
            self.args['adaptors'] = (
            """
       package Adaptors is new Conts.Cursors.Adaptors.""" +
            adaptors + """ (Container);
       function Count_If is new Conts.Algorithms.Count_If
          (Adaptors.Cursors.Constant_Forward);
""").format(**self.args)


        if disable_checks:
            self.args['adaptors'] = """
   pragma Suppress (Container_Checks);""" + self.args['adaptors']

        self.__common()


class Map(object):

    def __init__(
        self,
        elem_type,   # "integer", "string",...
        base,        # "controlled", "limited", ...
        key,         # "definite", "indefinite", ...
        value,       # "definite", "indefinite", ...
        nodes,       # "bounded", "unbounded", ...
        instance,    # instantiation for the container "package Container is ..."
        withs,       # extra withs for the body
        comments=None, # instance of Comments
        favorite=False # Whether this should be highlighted in the results
    ):
        type="Map"
        category = '%s %s' % (elem_type, type)
        append = """
        declare
           S : constant String := C'Img;
        begin
           --   ??? Can't use V2 (V'Img) := "foo"
           V2.Include (S (S'First + 1 .. S'Last), "foo");
        end;
"""
        self.args = dict(
            base=base,
            key=key,
            value=value,
            nodes=nodes,
            category=category,
            type=type,
            elem_type=elem_type,
            instance=instance,
            withs=withs,
            copy='',
            call_count_if='',
            discriminant='',
            favorite=favorite,
            comments=comments or Comments(),
            clear='',       # Explicit clear the container
            clear_copy='',  # Explicit clear the copy of the container
            prefix='',      # Prefix for Element, Next and Has_Element
            adaptors='',    # Creating adaptors for standard containers
            append=append)

        self.args['test_name'] = \
            "{type}_{base}_{key}_{value}_{nodes}_{elem_type}".format(
                **self.args)

    def __common(self):
        filename = self.args['test_name'].lower()

        ads = open("tests/generated/%s.ads" % filename, "w")
        ads.write("""
with Report; use Report;
pragma Style_Checks (Off);
procedure {test_name}
   (Stdout : not null access Output'Class);
            """.format(**self.args))
        ads.close()

        adb = open("tests/generated/%s.adb" % filename, "w")
        adb.write("""
{withs}
pragma Style_Checks (Off);
pragma Warnings (Off, "unit * is not referenced");
with Perf_Support;  use Perf_Support;
with Conts.Algorithms;
with Conts.Cursors.Adaptors;
pragma Warnings (On, "unit * is not referenced");
procedure {test_name}
   (Stdout : not null access Output'Class)
is
   {instance}
   use Container;{adaptors}

   procedure Run (V2 : in out Container.{type}'Class);
   --  Force dynamic dispatching for the container (if relevant), as a
   --  a way to check we do not waste time there.

   procedure Run (V2 : in out Container.{type}'Class) is
      It : Container.Cursor;
      Co : Natural;
   begin
      Stdout.Start_Test ("fill", "{comments.fill}");
      for C in 1 .. Items_Count loop
         {append}
      end loop;
      Stdout.End_Test;

      Stdout.Start_Test ("copy", "{comments.copy}");
      declare
         V_Copy : Container.{type}'Class := V2{copy};
         pragma Unreferenced (V_Copy);
      begin
         --  Measure the time before we destroy the copy
         Stdout.End_Test;{clear_copy}
      end;

      Co := 0;
      Stdout.Start_Test ("cursor loop", "{comments.cursorloop}");
      It := V2.First;
      while {prefix}Has_Element (It) loop
         if Predicate ({prefix}Element (It)) then
            Co := Co + 1;
         end if;
         It := {prefix}Next (It);
      end loop;
      Stdout.End_Test;
      Assert (Co, Items_Count);

      Co := 0;
      Stdout.Start_Test ("for-of loop", "{comments.forofloop}");
      for E of V2 loop
         if Predicate (E) then
            Co := Co + 1;
         end if;
      end loop;
      Stdout.End_Test;
      Assert (Co, Items_Count);

      Stdout.Start_Test ("count_if", "{comments.countif}");
      Co := Count_If (V2, Predicate'Access);
      Stdout.End_Test;
      Assert (Co, Items_Count);

      Co := 0;
      Stdout.Start_Test ("find", "{comments.find}");
      for C in 1 .. Items_Count loop
         if Predicate (V2 ("1")) then
            Co := Co + 1;
         end if;
      end loop;
      Assert (Co, Items_Count);
   end Run;

begin
   Stdout.Start_Container_Test
      ("{base}", "{key}-{value}", "{nodes}", "{category}", {favorite});
   for C in 1 .. Repeat_Count loop
      declare
         V : Container.{type}{discriminant};
      begin
         Stdout.Save_Container_Size (V'Size / 8);  --  in bytes
         Run (V);{clear}
      end;
   end loop;
   Stdout.End_Container_Test;
end {test_name};
""".format(**self.args))

        adb.close()

    def gen_ada2012(self,
                    disable_checks=False,
                    use_cursor_convert=False,
                    adaptors='{type}'):
        """
        Generate tests for the Ada 2012 containers
        """

        self.args['adaptors'] = """
    package Adaptors is new Conts.Cursors.Adaptors.""" + adaptors + \
    """_Adaptors
        (Container);
    function Count_If is new Conts.Algorithms.Count_If_Convert
       (Adaptors.Cursors_Forward_Convert);"""

        self.__common()


class Vector (List):
    type = "Vector"


# Integer lists

List("Integer", "Ada12", "Def", "Bounded",
     "package Container is new Ada.Containers.Bounded_Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Bounded_Doubly_Linked_Lists;").gen_ada2012(
        adaptors='Bounded_List_Adaptors')
List("Integer", "Ada12", "Def", "Unbounded",
     "package Container is new Ada.Containers.Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Doubly_Linked_Lists;").gen_ada2012()
List("Integer", "Ada12", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" +
     " (Integer);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;").gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors')
List("Integer", "Ada12_No_Checks", "Def", "Unbounded",
     "package Container is new Ada.Containers.Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Doubly_Linked_Lists;",
     favorite=True).gen_ada2012(disable_checks=True)
List("Integer", "Controlled", "Indef", "Unbounded",
     "package Container is new Conts.Lists.Indefinite_Unbounded (Integer);",
     "with Conts.Lists.Indefinite_Unbounded;").gen()
List("Integer", "Controlled", "Def", "Unbounded",
     "package Container is new Conts.Lists.Definite_Unbounded (Integer);",
     "with Conts.Lists.Definite_Unbounded;",
     favorite=True,
     comments=Comments(forofloop=
          "Because of dynamic dispatching -- When avoided, we gain 40%")
    ).gen()
List("Integer", "Controlled", "Def", "Bounded",
     "package Container is new Conts.Lists.Definite_Bounded (Integer);",
     "with Conts.Lists.Definite_Bounded;").gen()
List("Integer", "Limited", "Def", "Bounded",
     "package Container is new Conts.Lists.Definite_Bounded_Limited (Integer);",
     "with Conts.Lists.Definite_Bounded_Limited;").gen()
List("Integer", "Limited", "Indef_Spark", "Unbounded_Spark",
     "package Container is new Conts.Lists.Indefinite_Unbounded_SPARK (Integer);",
     "with Conts.Lists.Indefinite_Unbounded_SPARK;").gen()

# String lists

List("String", "Ada12", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" +
     " (String);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     favorite=False).gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors')
List("String", "Ada12_No_Checks", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     favorite=True).gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors',
         disable_checks=True)
List("String", "Controlled", "Indef", "Unbounded",
     "package Container is new Conts.Lists.Indefinite_Unbounded (String);",
     "with Conts.Lists.Indefinite_Unbounded;",
     comments=Comments(cursorloop="Cost if for copying the string")).gen()
List("String", "Controlled", "Indef", "Unbounded_Ref",
     "package Container is new Conts.Lists.Indefinite_Unbounded_Ref (String);",
     "with Conts.Lists.Indefinite_Unbounded_Ref;",
     comments=Comments(
         countif="Conversion from Reference_Type to Element_Type"),
     favorite=True).gen(use_cursor_convert=True)
List("Unbounded_String", "Controlled", "Def", "Unbounded",
     "package Container is new Conts.Lists.Definite_Unbounded (Unbounded_String);",
     "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;\n" +
     "with Conts.Lists.Definite_Unbounded;",
     comments=Comments(
         cursorloop="Maybe because of the atomic counters or controlled elements")
    ).gen()
List("String", "Controlled", "Arrays", "Unbounded",
     "package Container renames Conts.Lists.Strings;",
     "with Conts.Lists.Strings;",
     comments=Comments(
         countif='conversion to String',
         fill='strange, since we are doing fewer mallocs. Faster if we only' +
             'preallocate a 1 element array')
    ).gen(use_cursor_convert=True)

# Integer vectors

p = " (Natural, Integer);"

Vector("Integer", "Ada12", "Def", "Bounded",
     "package Container is new Ada.Containers.Bounded_Vectors" + p,
     "with Ada.Containers.Bounded_Vectors;").gen_ada2012(
        adaptors='Bounded_Vector_Adaptors')
Vector("Integer", "Ada12", "Def", "Unbounded",
     "package Container is new Ada.Containers.Vectors" + p,
     "with Ada.Containers.Vectors;").gen_ada2012()
Vector("Integer", "Ada12", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p,
     "with Ada.Containers.Indefinite_Vectors;").gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_Vector_Adaptors')
Vector("Integer", "Ada12_No_Checks", "Def", "Unbounded",
     "package Container is new Ada.Containers.Vectors" + p,
     "with Ada.Containers.Vectors;",
     favorite=True).gen_ada2012(disable_checks=True)
Vector("Integer", "Controlled", "Indef", "Unbounded",
     "package Container is new Conts.Vectors.Indefinite_Unbounded" + p,
     "with Conts.Vectors.Indefinite_Unbounded;").gen()
Vector("Integer", "Controlled", "Def", "Unbounded",
     "package Container is new Conts.Vectors.Definite_Unbounded" + p,
     "with Conts.Vectors.Definite_Unbounded;",
       comments=Comments(
           cursorloop='test in Next to see if we reached end of loop'),
     favorite=True).gen()
Vector("Integer", "Controlled", "Def", "Bounded",
     "package Container is new Conts.Vectors.Definite_Bounded" + p,
     "with Conts.Vectors.Definite_Bounded;").gen()
Vector("Integer", "Limited", "Def", "Bounded",
     "package Container is new Conts.Vectors.Definite_Bounded_Limited" + p,
     "with Conts.Vectors.Definite_Bounded_Limited;").gen()

# String vectors

p = " (Natural, String);"

Vector("String", "Ada12", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p,
     "with Ada.Containers.Indefinite_Vectors;").gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_Vector_Adaptors')
Vector("String", "Ada12_No_Checks", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p,
     "with Ada.Containers.Indefinite_Vectors;",
     favorite=True).gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_Vector_Adaptors',
         disable_checks=True)
Vector("String", "Controlled", "Indef", "Unbounded",
     "package Container is new Conts.Vectors.Indefinite_Unbounded" + p,
     "with Conts.Vectors.Indefinite_Unbounded;").gen()
Vector("String", "Controlled", "Indef", "Unbounded_Ref",
     "package Container is new Conts.Vectors.Indefinite_Unbounded_Ref" + p,
     "with Conts.Vectors.Indefinite_Unbounded_Ref;", favorite=True).gen(
         use_cursor_convert=True)

# String maps

Map("StrStr", "Ada12_ordered", "Indef", "Indef", "Unbounded",
     "package Container is new Ada.Containers.Indefinite_Ordered_Maps" +
         " (String, String);",
     "with Ada.Containers.Indefinite_Ordered_Maps;").gen_ada2012(
         adaptors="Indefinite_Ordered_Maps")
Map("StrStr", "Ada12_hashed", "Indef", "Indef", "Unbounded",
     'package Container is new Ada.Containers.Indefinite_Hashed_Maps' +
         ' (String, String, Ada.Strings.Hash, "=");',
     "with Ada.Strings.Hash;\n" +
     "with Ada.Containers.Indefinite_Hashed_Maps;").gen_ada2012(
        adaptors="Indefinite_Hashed_Maps")
