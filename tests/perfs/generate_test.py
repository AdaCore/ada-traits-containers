#!/usr/bin/env python
"""
This script is used to generate test for the various container combinations.
"""

import os

output_dir = 'generated/'
if not os.path.isdir(output_dir):
    os.mkdir(output_dir)


############
# Comments #
############


class Comments(object):
    def __init__(self, **kwargs):
        self.comments = kwargs

    def __getattr__(self, key):
        return self.comments.get(key, '')

    
#############
# Templates #
#############

def wrap(name, text, group=True, expected=True):
    repl = dict(
      name=name,
      group="True" if group else "False",
      text=text)

    r = """

      Co := 0;
      Stdout.Start_Test ("%(name)s", "{comments.%(name)s}", Start_Group => %(group)s);""" % repl
      
    r += text
    r += "\n      Stdout.End_Test;" % repl

    if expected:
        r += """\n      Assert (Co, {expected}, "%(name)s");""" % repl
    return r


class Templates(object):
    """
    Text for the various steps to be performed in tests.
    """

    # Contents of the .ads file

    spec = """
with Report; use Report;
pragma Style_Checks (Off);
procedure {test_name} (Stdout : not null access Output'Class);"""

    # Header for the .adb file

    body_header = """
{withs}
pragma Style_Checks (Off);
pragma Warnings (Off, "unit * is not referenced");
with Perf_Support;  use Perf_Support;
with Ada.Finalization;
with Conts.Algorithms;
with Conts.Adaptors;
pragma Warnings (On, "unit * is not referenced");
procedure {test_name}
   (Stdout : not null access Output'Class)
is
   {suppress}{instance}
   use Container;{adaptors}

   procedure Run (V2 : in out Container.{type});
   --  Force dynamic dispatching for the container (if relevant), as a
   --  a way to check we do not waste time there.

   procedure Run (V2 : in out Container.{type}) is
      It : Container.Cursor;
      Co : Natural;
   begin"""

    # Footer for the .adb file

    body_footer = """
   end Run;

begin
   Stdout.Start_Container_Test ("{name}", "{category}", {favorite});
   for C in 1 .. Repeat_Count loop
      declare
         V : Container.{type}{discriminant};
      begin
         Stdout.Save_Container_Size (V'Size / 8);  --  in bytes
         Run (V);{clear}
      end;
   end loop;
   Stdout.End_Container_Test;
end {test_name};"""

    # Filling a list or a vector

    list_fill = wrap("fill", """
      for C in 1 .. Items_Count loop
         {append}
      end loop;""", group=True, expected=False)

    # Copying a list or a vector

    list_copy = wrap("copy", """
      declare
         V_Copy : Container.{type}'Class := V2{copy};
         pragma Unreferenced (V_Copy);
      begin
         --  Measure the time before we destroy the copy
         Stdout.End_Test;{clear_copy}
      end;""", group=False, expected=False)

    # Count with a cursor loop for list or vector (integers)

    list_int_cursor_loop = wrap("cursor loop", """
      It := V2.First;
      while {prefix}Has_Element (It) loop
         if {prefix}Element (It) <= 2 then
            Co := Co + 1;
         end if;
         It := {prefix}Next (It);
      end loop;""", group=True)

    # Count with a cursor loop for list or vector (strings)

    list_str_cursor_loop = wrap("cursor loop", """
      It := V2.First;
      while {prefix}Has_Element (It) loop
         if Perf_Support.Predicate ({get} (It)) then
            Co := Co + 1;
         end if;
         It := {prefix}Next (It);
      end loop;""", group=True)

    # Count with a for-of loop (integers)

    list_int_for_of_loop = wrap("for-of loop", """
      for E of V2 loop
         if E <= 2 then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    # Count with a for-of loop (strings)

    list_str_for_of_loop = wrap("for-of loop", """
      for E of V2 loop
         if Perf_Support.Predicate (E) then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    # Count_If with an algorithm

    list_count_if = wrap("count_if", """
      Co := Count_If (V2, Predicate'Access);""", group=False)

    # loop using Constant_Indexing

    int_int_indexing_loop = wrap("indexed", """
      for C in 1 .. Items_Count loop
         if V2 (C) <= 2 then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    int_str_indexing_loop = wrap("indexed", """
      for C in 1 .. Items_Count loop
         if Perf_Support.Predicate (V2 (C)) then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    str_str_indexing_loop = wrap("indexed", """
      for C in 1 .. Items_Count loop
         if Perf_Support.Predicate (V2 ("1")) then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    # Filling a map

    map_fill = wrap("fill", """
      for C in 1 .. Items_Count loop
         {append}
      end loop;""", group=True, expected=False)

    # Copying a map

    map_copy = wrap("copy", """
      declare
         V_Copy : Container.{type}'Class := V2{copy};
         pragma Unreferenced (V_Copy);
      begin
         --  Measure the time before we destroy the copy
         Stdout.End_Test;{clear_copy}
      end;""", group=False, expected=False)

    # cursor loop for map

    map_cursor_loop = wrap("cursor loop", """
      It := V2.First;
      while {prefix}Has_Element (It) loop
         if {cursor_loop_predicate} ({prefix}Element (It)) then
            Co := Co + 1;
         end if;
         It := {prefix}Next (It);
      end loop;""", group=True)

    # for-of loop for map

    map_for_of_loop = wrap("for-in/of loop", """

      --  Doing iteration on keys would be slower, with an extra lookup
      --     for Key of V2 loop
      --        if Predicate (V2.Get (Key)) then
      --  So instead we iterate on cursors                           
      for C in V2 loop
         if {cursor_loop_predicate} (V2.Element (C)) then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    map_ada2012_for_of_loop = wrap("for-in/of loop", """
      for E of V2 loop
         if Predicate (E) then
            Co := Co + 1;
         end if;
      end loop;""", group=False)

    # count_if for map

    map_count_if = wrap("count_if", """
      Co := Count_If (V2, Predicate'Access);""", group=False)

    # find for map

    map_find = wrap("find", """
      for C in 1 .. Items_Count loop
         if {find_predicate} (V2{get}) then
            Co := Co + 1;
         end if;
      end loop;""", group=True)

    @staticmethod
    def lists(elem_type):
        if elem_type == "integer":
            return (Templates.list_fill, Templates.list_copy,
                    Templates.list_int_cursor_loop,
                    Templates.list_int_for_of_loop, Templates.list_count_if)
        else:
            return (Templates.list_fill, Templates.list_copy,
                    Templates.list_str_cursor_loop,
                    Templates.list_str_for_of_loop, Templates.list_count_if)

    @staticmethod
    def vectors(elem_type):
        if elem_type == "integer":
            return (Templates.list_fill, Templates.list_copy,
                    Templates.list_int_cursor_loop,
                    Templates.list_int_for_of_loop, Templates.list_count_if,
                    Templates.int_int_indexing_loop)
        else:
            return (Templates.list_fill, Templates.list_copy,
                    Templates.list_str_cursor_loop,
                    Templates.list_str_for_of_loop, Templates.list_count_if,
                    Templates.int_str_indexing_loop)

    @staticmethod
    def maps(elem_type, std_ada=False):
        if elem_type == "intint":
            return (Templates.map_fill, Templates.map_copy,
                    Templates.map_cursor_loop,
                    Templates.map_for_of_loop
                       if not std_ada else Templates.map_ada2012_for_of_loop,                    
                    Templates.map_count_if, Templates.int_int_indexing_loop,
                    Templates.map_find)
        else:
            return (Templates.map_fill, Templates.map_copy,
                    Templates.map_cursor_loop,
                    Templates.map_for_of_loop
                       if not std_ada else Templates.map_ada2012_for_of_loop,                    
                    Templates.map_count_if, Templates.str_str_indexing_loop,
                    Templates.map_find)

#########
# Tests #
#########

class Tests(object):

    def write(self):
        filename = self.args['test_name'].lower()

        ads = open(os.path.join(output_dir, "%s.ads" % filename), "w")
        ads.write(Templates.spec.format(**self.args))
        ads.close()

        adb = open(os.path.join(output_dir, "%s.adb" % filename), "w")
        adb.write(Templates.body_header.format(**self.args))
        for t in self.tests():
            adb.write(t.format(**self.args))
        adb.write(Templates.body_footer.format(**self.args))
        adb.close()

        all_tests_withs.append(
            "with {test_name};".format(**self.args))
        all_tests.append(
            """   Run_Test ("{filename}", {test_name}'Access);""".format(**self.args))

    def gen(self, adaptor="Constant_Returned", adaptors="{type}_Adaptors",
            disable_checks=False, 
            cursor_loop_predicate="Predicate",
            find_predicate="Predicate"):

        self.args['suppress'] = ""
        if disable_checks:
            self.args['suppress'] = "pragma Suppress (Container_Checks);\n   "
            
        self.args['cursor_loop_predicate'] = cursor_loop_predicate
        self.args['find_predicate'] = find_predicate

        if self.ada2012:
            adapt = adaptors.format(**self.args)

            self.args['adaptors'] = """
   package Adaptors is new Conts.Adaptors.%s (Container);
   function Count_If is new Conts.Algorithms.Count_If
      (Adaptors.Cursors.Forward, Adaptors.Maps.%s);""" % (adapt, adaptor)

        else:
            self.args['prefix'] = 'V2.'
            self.args['adaptors'] = """
   function Count_If is new Conts.Algorithms.Count_If
      (Container.Cursors.Forward, Container.Maps.%s);""" % adaptor

        self.write()


########
# List #
########

class List(Tests):
    type = "List"

    def __init__(
        self,
        elem_type,   # "integer", "string",...
        instance,  # instantiation for the container "package Container is ..."
        withs,       # extra withs for the body
        unbounded,
        name,        # test name
        filename,    # suffix for filename
        limited=False,  # Whether we need explicit Copy and Clear
        comments=None,  # instance of Comments
        favorite=False,  # Whether this should be highlighted in the results
        ada2012=False
    ):
        self.elem_type = elem_type.lower()
        self.ada2012 = ada2012

        if ada2012:
            get = "Element"
        else:
            get = "V2.Element"

        # We use two default strings (one short, one long), to test various
        # approaches of storing elements

        if self.elem_type == "integer":
            category = '%s %s' % (elem_type, self.type)
            append = "V2.Append (C);"
            expected = "2"

        elif self.elem_type == "string":
            category = '%s %s' % (elem_type, self.type)
            expected = "Items_Count"
            if ada2012:
                get = "V2.Constant_Reference"

            append = """
         if C mod 2 = 0 then
             V2.Append ("foo");
         else
             V2.Append ("foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo");
         end if;"""

        elif self.elem_type == "unbounded_string":
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
            raise Exception("Unknown element type: %s" % self.elem_type)

        self.args = dict(
            name=name,
            filename=filename,
            expected=expected,
            category=category,
            type=self.type,
            elem_type=elem_type,
            instance=instance,
            get=get,
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

        if limited:
            # Need an explicit copy since ":=" is not defined for limited types
            self.args['copy'] = '.Copy'
            self.args['clear'] = '\n      V.Clear;'
            self.args['clear_copy'] = '\n         V_Copy.Clear;'

        if not unbounded:
            self.args['discriminant'] = ' (Capacity => Items_Count)'

        self.args['test_name'] = "{type}_{elem_type}_{filename}".format(
            **self.args)

    def tests(self):
        return Templates.lists(self.elem_type)

    
#######
# Map #
#######

class Map(Tests):

    def __init__(
        self,
        elem_type,   # "intint", "strstr",...
        instance,  # instantiation for the container "package Container is ..."
        withs,       # extra withs for the body
        unbounded,
        name,            # test name
        filename,        # suffix for filename
        limited=False,   # Whether we need explicit Copy and Clear
        comments=None,   # instance of Comments
        favorite=False,  # Whether this should be highlighted in the results
        ada2012=False
    ):
        type = "Map"
        category = '%s %s' % (elem_type, type)

        self.elem_type = elem_type.lower()
        self.ada2012 = ada2012

        if self.elem_type == "strstr":
            get_val = 'Image (C)'
            expected = "Items_Count"
            append = """
        --   ??? Can't use V2 (V'Img) := "foo"
        V2.{set} (Image (C), "foo");
"""

        elif self.elem_type == "intint":
            get_val = 'C'
            expected = "2"
            append = """
        V2.{set} (C, C);
"""

        if ada2012:
            set = "Include"
            get = '.Element (%s)' % get_val
        else:
            set = "Set"
            get = '.Get (%s)' % get_val

        self.args = dict(
            name=name,
            filename=filename,
            category=category,
            type=type,
            elem_type=elem_type,
            instance=instance,
            withs=withs,
            expected=expected,
            copy='',
            get=get,
            set=set,
            call_count_if='',
            discriminant='',
            favorite=favorite,
            cursor_loop_predicate='Predicate',
            find_predicate='Predicate',
            comments=comments or Comments(),
            clear='',       # Explicit clear the container
            clear_copy='',  # Explicit clear the copy of the container
            prefix='',      # Prefix for Element, Next and Has_Element
            adaptors='',    # Creating adaptors for standard containers
            append=append.format(set=set))

        if limited:
            # Need an explicit copy since ":=" is not defined for limited types
            self.args['copy'] = '.Copy'
            self.args['clear'] = '\n      V.Clear;'
            self.args['clear_copy'] = '\n         V_Copy.Clear;'

        if not unbounded:
            self.args['discriminant'] = ' (Capacity => Items_Count, ' + \
                ' Modulus => Default_Modulus (Items_Count))'

        self.args['test_name'] = "{type}_{elem_type}_{filename}".format(
            **self.args)

    def tests(self):
        return Templates.maps(self.elem_type, std_ada=self.ada2012)

    
##########
# Vector #
##########

class Vector(List):
    type = "Vector"

    def tests(self):
        return Templates.vectors(self.elem_type)


# Setup

all_tests_withs = []
all_tests = []

# Integer lists

i = " (Integer);"
ci = " (Integer);"
s = " (String);"
cs = " (String);"

List("Integer",
     "package Container is new Ada.Containers.Bounded_Doubly_Linked_Lists" + i,
     "with Ada.Containers.Bounded_Doubly_Linked_Lists;",
     unbounded=False, name="Ada12 Bounded", filename="ada12_bounded",
     ada2012=True
    ).gen(
        adaptors='Bounded_List_Adaptors')
List("Integer",
     "package Container is new Ada.Containers.Doubly_Linked_Lists" + i,
     "with Ada.Containers.Doubly_Linked_Lists;",
     unbounded=True, ada2012=True,
     name="Ada12 Definite Unbounded", filename="ada12_def_unbounded"
    ).gen()
List("Integer",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" +
     " (Integer);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     unbounded=True, ada2012=True,
     name="Ada12 Indefinite Unbounded", filename="ada12_indef_unbounded"
    ).gen(
         adaptor="Element",
         adaptors='Indefinite_List_Adaptors')
List("Integer",
     "package Container is new Ada.Containers.Doubly_Linked_Lists" + i,
     "with Ada.Containers.Doubly_Linked_Lists;",
     unbounded=True, ada2012=True,
     name="Ada12 Nochecks Def unbounded",
     filename="ada12_nocheck_def_unbounded",
     favorite=True).gen(disable_checks=True)
List("Integer",
     "package Container is new Conts.Lists.Indefinite_Unbounded" + ci,
     "with Conts.Lists.Indefinite_Unbounded;",
     unbounded=True,
     name="Indef Unbounded", filename="indef_unbounded"
    ).gen(adaptor="Element")
List("Integer",
     "package Container is new Conts.Lists.Definite_Unbounded" + ci,
     "with Conts.Lists.Definite_Unbounded;",
     unbounded=True,
     name="Def Unbounded", filename="def_unbounded",
     favorite=True,
     comments=Comments(forofloop=
          "Because of dynamic dispatching -- When avoided, we gain 40%")
    ).gen(adaptor="Constant_Returned")
List("Integer",
     "package Container is new Conts.Lists.Definite_Bounded" + ci,
     "with Conts.Lists.Definite_Bounded;",
     unbounded=False,
     name="Def Bounded", filename="def_bounded"
    ).gen(adaptor="Constant_Returned")
List("Integer",
     "package Container is new Conts.Lists.Indefinite_Unbounded_SPARK" + i,
     "with Conts.Lists.Indefinite_Unbounded_SPARK;",
    unbounded=True, limited=True,
    name="Limited Indef_Spark Unbounded_Spark",
    filename="indef_unbounded_spark").gen(adaptor="Element")

# String lists

List("String",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" + s
     + '\n   function Predicate (P : Container.Constant_Reference_Type) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     unbounded=True, ada2012=True,
     name="Ada12 Indefinite Unbounded", filename="ada12_indef_unbounded",
     favorite=False).gen(
         adaptor="Constant_Returned",
         adaptors='Indefinite_List_Adaptors')
List("String",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" + s
     + '\n   function Predicate (P : Container.Constant_Reference_Type) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     unbounded=True, ada2012=True,
     name="Ada12 Nochecks Indef Unbounded",
     filename="ada12_nocheck_indef_unbounded",
     favorite=True).gen(
         adaptor="Constant_Returned",
         adaptors='Indefinite_List_Adaptors',
         disable_checks=True)
List("String",
     "package Container is new Conts.Lists.Indefinite_Unbounded" + cs
     + '\n   function Predicate (P : Container.Constant_Returned) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Conts.Lists.Indefinite_Unbounded;",
     unbounded=True,
     name="Unbounded of Indef",
     filename="indef_unbounded",
     favorite=True,
     comments=Comments(cursorloop="Cost if for copying the string")).gen(
         adaptor="Constant_Returned")
List("Unbounded_String",
     "package Container is new Conts.Lists.Definite_Unbounded"
     + "(Unbounded_String);",
     "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;\n" +
     "with Conts.Lists.Definite_Unbounded;",
     unbounded=True,
     name="Unbounded of Unbounded_String",
     filename="def_unbounded_string",
     comments=Comments(
         cursorloop="Maybe because of the atomic counters or controlled elements")
    ).gen(adaptor="Constant_Returned")
List("String",
     "package Container is new Conts.Lists.Indefinite_Unbounded_SPARK" + cs
     + '\n   function Predicate (P : Container.Constant_Returned) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Conts.Lists.Indefinite_Unbounded_SPARK;",
     unbounded=True, limited=True,
     name="Limited Unbounded_SPARK of Indef_SPARK",
     filename="indef_unbounded_spark",
     favorite=True,
     comments=Comments(cursorloop="Cost if for copying the string")).gen(
         adaptor="Constant_Returned")
#List("String",
#     "package Container renames Conts.Lists.Strings;",
#     "with Conts.Lists.Strings;",
#     unbounded=True,
#     name="Unbounded string-specific",
#     filename="unbounded_strings",
#     comments=Comments(
#         countif='conversion to String',
#         fill='strange, since we are doing fewer mallocs. Faster if we only' +
#             'preallocate a 1 element array')
#    ).gen(adaptor="Element")

# Integer vectors

p = " (Positive, Integer);"
cp = " (Positive, Integer, Ada.Finalization.Controlled);"
lp = " (Positive, Integer);"

Vector("Integer",
     "package Container is new Ada.Containers.Bounded_Vectors" + p,
     "with Ada.Containers.Bounded_Vectors;",
       unbounded=False, ada2012=True,
       name="Ada12 Bounded",
       filename="ada12_bounded"
      ).gen(
        adaptors='Bounded_Vector_Adaptors')
Vector("Integer",
     "package Container is new Ada.Containers.Vectors" + p,
     "with Ada.Containers.Vectors;",
      unbounded=True, ada2012=True,
      name="Ada12 Definite Unbounded",
      filename="ada12_def_unbounded").gen()
Vector("Integer",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p,
     "with Ada.Containers.Indefinite_Vectors;",
       unbounded=True, ada2012=True,
       name="Ada12 Indefinite Unbounded",
       filename="ada12_indef_unbounded"
      ).gen(
         adaptor="Element",
         adaptors='Indefinite_Vector_Adaptors')
Vector("Integer",
     "package Container is new Ada.Containers.Vectors" + p,
     "with Ada.Containers.Vectors;",
       unbounded=True, ada2012=True,
       name="Ada12 Nochecks Definite Unbounded",
       filename="ada12_nochecks_definite_unbounded",
       favorite=True).gen(disable_checks=True)
Vector("Integer",
     "package Container is new Conts.Vectors.Indefinite_Unbounded" + cp,
     "with Conts.Vectors.Indefinite_Unbounded;",
       unbounded=True,
       name="Indef Unbounded",
       filename="indef_unbounded"
      ).gen(adaptor="Element")
Vector("Integer",
     "package Container is new Conts.Vectors.Definite_Unbounded" + cp,
     "with Conts.Vectors.Definite_Unbounded;",
       unbounded=True,
       name="Def Unbounded",
       filename="def_unbounded",
       comments=Comments(
           cursorloop='test in Next to see if we reached end of loop'),
     favorite=True).gen(adaptor="Constant_Returned")
Vector("Integer",
     "package Container is new Conts.Vectors.Definite_Bounded" + p,
     "with Conts.Vectors.Definite_Bounded;",
       unbounded=False,
       name="Def Bounded",
       filename="def_bounded"
      ).gen(adaptor="Constant_Returned")
Vector("Integer",
     "package Container is new Conts.Vectors.Indefinite_Unbounded_SPARK" + lp,
     "with Conts.Vectors.Indefinite_Unbounded_SPARK;",
       unbounded=True, limited=True,
       name="Limited Indef_SPARK Unbounded_SPARK",
       filename="indef_unbounded_spark"
      ).gen(adaptor="Element")

# String vectors

p = " (Positive, String);"
cp = " (Positive, String, Ada.Finalization.Controlled);"
lp = " (Positive, String);"

Vector("String",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p
     + '\n   function Predicate (P : Container.Constant_Reference_Type) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Ada.Containers.Indefinite_Vectors;",
       unbounded=True, ada2012=True,
       name="Ada12 Indefinite Unbounded",
       filename="ada12_indef_unbounded"
      ).gen(
         adaptor="Constant_Returned",
         adaptors='Indefinite_Vector_Adaptors')
Vector("String",
     "package Container is new Ada.Containers.Indefinite_Vectors" + p
     + '\n   function Predicate (P : Container.Constant_Reference_Type) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Ada.Containers.Indefinite_Vectors;",
       unbounded=True, ada2012=True,
       name="Ada12 Nochecks Indefinite Unbounded",
       filename="ada12_nochecks_indef_unbounded",
       favorite=True).gen(
         adaptor="Constant_Returned",
         adaptors='Indefinite_Vector_Adaptors',
         disable_checks=True)
Vector("String",
     "package Container is new Conts.Vectors.Indefinite_Unbounded" + cp
     + '\n   function Predicate (P : Container.Constant_Returned) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Conts.Vectors.Indefinite_Unbounded;",
       unbounded=True,
       name="Indef Unbounded",
       filename="indef_unbounded",
       favorite=True).gen(
         adaptor="Constant_Returned")
Vector("String",
     "package Container is new Conts.Vectors.Indefinite_Unbounded_SPARK" + lp
     + '\n   function Predicate (P : Container.Constant_Returned) return Boolean\n'
     + '      is (Perf_Support.Predicate (P)) with Inline;',
     "with Conts.Vectors.Indefinite_Unbounded_SPARK;",
       unbounded=True, limited=True,
       name="Limited Indef_SPARK Unbounded_SPARK",
       filename="indef_unbounded_spark").gen(
         adaptor="Constant_Returned")

# Integer-Integer maps

Map("IntInt",
    "package Container is new Ada.Containers.Ordered_Maps"
    +  " (Integer, Integer);",
    "with Ada.Containers.Ordered_Maps;",
    unbounded=True, ada2012=True,
    name="Ada12 ordered definite unbounded",
    filename="ada12_ordered_def_unbounded"
    ).gen(
         adaptor="Element",
         adaptors="Ordered_Maps_Adaptors")
Map("IntInt",
    'function Hash (K : Integer) return Conts.Hash_Type is\n'
     + '   (Conts.Hash_Type (K)) with Inline;\n'
     + 'package Container is new Ada.Containers.Hashed_Maps'
     + ' (Integer, Integer, Hash, "=");',
    "with Ada.Containers.Hashed_Maps;",
    unbounded=True, ada2012=True,
    name="Ada12 hashed definite unbounded",
    filename="ada12_hashed_def_unbounded"
    ).gen(
         adaptor="Element",
         adaptors="Hashed_Maps_Adaptors")
Map("IntInt",
    'function Hash (K : Integer) return Conts.Hash_Type is\n'
     + '      (Conts.Hash_Type (K)) with Inline;\n'
     + '   package Container is new Ada.Containers.Bounded_Hashed_Maps\n'
     + '      (Integer, Integer, Hash, "=");',
    "with Ada.Containers.Bounded_Hashed_Maps;",
    unbounded=False, ada2012=True,
    name="Ada12 hashed definite bounded",
    filename="ada12_hashed_def_bounded"
    ).gen(
         adaptor="Element",
         adaptors="Bounded_Hashed_Maps_Adaptors")
Map("IntInt",
    'function Hash (K : Integer) return Conts.Hash_Type is\n'
    + '      (Conts.Hash_Type (K)) with Inline;\n'
    + '   package Container is new Conts.Maps.Def_Def_Unbounded\n'
    + '      (Integer, Integer, Ada.Finalization.Controlled, Hash);\n',
    'with Conts.Maps.Def_Def_Unbounded;',
    unbounded=True,
    name="Hashed Def Def Unbounded",
    filename="hashed_def_def_unbounded",
    favorite=True).gen(adaptor="Constant_Returned")
Map("IntInt",
    'function Hash (K : Integer) return Conts.Hash_Type is\n'
    + '      (Conts.Hash_Type (K)) with Inline;\n'
    + '   package Container is new Conts.Maps.Def_Def_Unbounded\n'
    + '      (Integer, Integer, Ada.Finalization.Controlled, Hash);\n',
    'with Conts.Maps.Def_Def_Unbounded;',
    unbounded=True,
    name="Hashed Linear Probing Def Def Unbounded",
    filename="hashed_linear_probing_def_def_unbounded",
    favorite=True).gen(adaptor="Constant_Returned")

# String-String maps

Map("StrStr",
    "package Container is new Ada.Containers.Indefinite_Ordered_Maps"
    + " (String, String);",
    "with Ada.Containers.Indefinite_Ordered_Maps;",
    unbounded=True, ada2012=True,
    name="Ada12 Ordered Indefinite Unbounded",
    filename="ada12_ordered_indef_unbounded"
    ).gen(
         adaptor="Element",
         adaptors="Indefinite_Ordered_Maps_Adaptors")
Map("StrStr",
    'package Container is new Ada.Containers.Indefinite_Hashed_Maps'
    +  ' (String, String, Ada.Strings.Hash, "=");',
    "with Ada.Strings.Hash;\n" +
    "with Ada.Containers.Indefinite_Hashed_Maps;",
    unbounded=True, ada2012=True,
    name="Ada12 Hashed Indefinite Unbounded",
    filename="ada12_hashed_indef_unbounded"
    ).gen(
         adaptor="Element",
         adaptors="Indefinite_Hashed_Maps_Adaptors")
Map("StrStr",
    'package Container is new Conts.Maps.Indef_Indef_Unbounded\n'
    + '      (String, String, Ada.Finalization.Controlled, Ada.Strings.Hash);\n'
    + '   function Predicate (P : Container.Constant_Returned_Type) return Boolean\n'
    + '      is (Perf_Support.Predicate (P)) with Inline;\n'
    + '   function Ref_Predicate (P : Container.Constant_Returned_Type) return Boolean\n'
    + '      renames Predicate;',
    'with Conts.Maps.Indef_Indef_Unbounded, Ada.Strings.Hash;',
    unbounded=True,
    name="Hashed Indef-Indef Unbounded",
    filename="hashed_indef_indef_unbounded",
    favorite=True).gen(
        adaptor="Constant_Returned",
        cursor_loop_predicate='Ref_Predicate',
        find_predicate='Ref_Predicate')
Map("StrStr",
    'package Container is new Conts.Maps.Indef_Indef_Unbounded\n'
    + '      (String, String, Ada.Finalization.Controlled, Ada.Strings.Hash);\n'
    + '   function Predicate (P : Container.Constant_Returned_Type) return Boolean\n'
    + '      is (Perf_Support.Predicate (P)) with Inline;'
    + '   function Ref_Predicate (P : Container.Constant_Returned_Type) return Boolean\n'
    + '      renames Predicate;',
        'with Conts.Maps.Indef_Indef_Unbounded, Ada.Strings.Hash;',
    unbounded=True,
    name="Hashed Linear Probing Indef-Indef Unbounded",
    filename="hashed_linear_probing_indef_indef_unbounded",
    favorite=True).gen(
        adaptor="Constant_Returned",
        cursor_loop_predicate='Ref_Predicate',
        find_predicate='Ref_Predicate')
Map("StrStr",
    'package Container is new Conts.Maps.Indef_Indef_Unbounded_SPARK\n'
    + '      (String, String, Ada.Strings.Hash);\n'
    + '   function Predicate (P : Container.Constant_Returned_Type) return Boolean\n'
    + '      is (Perf_Support.Predicate (P)) with Inline;',
    'with Conts.Maps.Indef_Indef_Unbounded_SPARK, Ada.Strings.Hash;',
    unbounded=True, limited=True,
    name="Limited Hashed Indef_SPARK-Indef_SPARK Unbounded_SPARK",
    filename="hashed_indef_indef_unbounded_spark",
    favorite=True).gen(adaptor="Constant_Returned")

run_all = open(os.path.join(output_dir, "main-run_all.adb"), "w")
run_all.write("\n".join(all_tests_withs))
run_all.write("""
pragma Style_Checks (Off);
separate (Main)
procedure Run_All is
begin\n""")
run_all.write("\n".join(all_tests))
run_all.write("""
end Run_All;""")
run_all.close()
