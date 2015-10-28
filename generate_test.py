#!/usr/bin/env python
"""
This script is used to generate test for the various container combinations.
"""

ads = open("tests/generated_tests.ads", "w")
adb = open("tests/generated_tests.adb", "w")

spec_contents = ""
body_withs = ""
body_contents = ""

class Comments(object):
    def __init__(self, **kwargs):
        self.comments = kwargs

    def __getattr__(self, key):
        return self.comments.get(key, '')


class Test(object):

    def __init__(
        self,
        elem_type,   # "integer", "string",...
        base,        # "controlled", "limited", ...
        definite,    # "definite", "indefinite", ...
        nodes,       # "bounded", "unbounded", ...
        type,        # "List", "Vector", ...
        instance,    # instantiation for the container "package Container is ..."
        withs,       # extra withs for the body
        comments=None, # instance of Comments
        favorite=False # Whether this should be highlighted in the results
    ):

        # We use two default strings (one short, one long), to test various
        # approaches of storing elements

        if elem_type.lower() == "integer":
            category = '%s %s' % (elem_type, type)
            append = "V2.Append (2);"
        elif elem_type.lower() == "string":
            category = '%s %s' % (elem_type, type)
            append = 'V2.Append ((if C mod 2 = 0 then "foo" else ' + \
               '"foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo"));'
        elif elem_type.lower() == "unbounded_string":
            category = 'String %s' % (type, )
            append = 'V2.Append (To_Unbounded_String ((' + \
               ' if C mod 2 = 0 then "foo" else ' + \
               '"foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo")));'
        else:
            raise Exception("Unknown element type: %s" % elem_type)

        self.args = dict(
            base=base,
            definite=definite,
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

        if self.args['base'].lower() == "limited":
            # Need an explicit copy, since ":=" is not defined for limited types
            self.args['copy'] = '.Copy'
            self.args['clear'] = '\n      V.Clear;'
            self.args['clear_copy'] = '\n         V_Copy.Clear;'

        if self.args['nodes'].lower() == "bounded":
            self.args['discriminant'] = ' (Capacity => Items_Count)'

    def __common(self):
        global body_withs, body_contents, spec_contents

        spec_contents += """
    procedure Test_{base}_{definite}_{nodes}_{elem_type}
        (Stdout : not null access Output'Class);
""".format(**self.args)

        body_withs += """
{withs}""".format(**self.args)

        body_contents += """

    procedure Test_{base}_{definite}_{nodes}_{elem_type}
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
       end Run;

    begin
       Stdout.Start_Container_Test
          ("{base}", "{definite}", "{nodes}", "{category}", {favorite});
       for C in 1 .. Repeat_Count loop
          declare
             V : Container.{type}{discriminant};
          begin
             Run (V);{clear}
          end;
       end loop;
       Stdout.End_Container_Test;
    end Test_{base}_{definite}_{nodes}_{elem_type};
""".format(**self.args)

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


Test("Integer", "Ada12", "Definite", "Unbounded", "List",
     "package Container is new Ada.Containers.Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Doubly_Linked_Lists;").gen_ada2012()
Test("Integer", "Ada12", "Indefinite", "Unbounded", "List",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" +
     " (Integer);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;").gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors')
Test("Integer", "Ada12_No_Checks", "Definite", "Unbounded", "List",
     "package Container is new Ada.Containers.Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Doubly_Linked_Lists;",
     favorite=True).gen_ada2012(disable_checks=True)

Test("Integer", "Controlled", "Indefinite", "Unbounded", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded (Integer);",
     "with Conts.Lists.Indefinite_Unbounded;").gen()
Test("Integer", "Controlled", "Definite", "Unbounded", "List",
     "package Container is new Conts.Lists.Definite_Unbounded (Integer);",
     "with Conts.Lists.Definite_Unbounded;",
     favorite=True,
     comments=Comments(forofloop=
          "Because of dynamic dispatching -- When avoided, we gain 40%")
    ).gen()
Test("Integer", "Controlled", "Definite", "Bounded", "List",
     "package Container is new Conts.Lists.Definite_Bounded (Integer);",
     "with Conts.Lists.Definite_Bounded;").gen()
Test("Integer", "Limited", "Definite", "Bounded", "List",
     "package Container is new Conts.Lists.Definite_Bounded_Limited (Integer);",
     "with Conts.Lists.Definite_Bounded_Limited;").gen()
Test("Integer", "Limited", "Indefinite_Spark", "Unbounded_Spark", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded_SPARK (Integer);",
     "with Conts.Lists.Indefinite_Unbounded_SPARK;").gen()

Test("String", "Ada12", "Indefinite", "Unbounded", "List",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists" +
     " (String);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     favorite=False).gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors')
Test("String", "Ada12_No_Checks", "Indefinite", "Unbounded", "List",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;",
     favorite=True).gen_ada2012(
         use_cursor_convert=True,
         adaptors='Indefinite_List_Adaptors',
         disable_checks=True)

Test("String", "Controlled", "Indefinite", "Unbounded", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded (String);",
     "with Conts.Lists.Indefinite_Unbounded;",
     comments=Comments(cursorloop="Cost if for copying the string")).gen()
Test("String", "Controlled", "Indefinite", "Unbounded_Ref", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded_Ref (String);",
     "with Conts.Lists.Indefinite_Unbounded_Ref;",
     comments=Comments(
         countif="Conversion from Reference_Type to Element_Type"),
     favorite=True).gen(use_cursor_convert=True)
Test("Unbounded_String", "Controlled", "Definite", "Unbounded", "List",
     "package Container is new Conts.Lists.Definite_Unbounded (Unbounded_String);",
     "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;\n" +
     "with Conts.Lists.Indefinite_Unbounded;",
     comments=Comments(
         cursorloop="Maybe because of the atomic counters or controlled elements")
    ).gen()
Test("String", "Controlled", "Arrays", "Unbounded", "List",
     """
     package Container is
        package Elements is new Conts.Elements.Arrays
           (Positive, Character, String, Conts.Global_Pool);
        package Nodes is new Conts.Lists.Nodes.Unbounded
           (Elements.Traits, Ada.Finalization.Controlled, Conts.Global_Pool);
        package Lists is new Conts.Lists.Generics (Nodes.Traits);
        subtype Cursor is Lists.Cursor;
        type List is new Lists.List with null record
           with Iterable => (First => First_Primitive,
                             Next  => Next_Primitive,
                             Has_Element => Has_Element_Primitive,
                             Element => Element_Primitive);
        package Cursors is new Conts.Lists.Cursors (Lists, List);
        function From_Ref_To_Elem (R : Elements.Ref_Type) return String
           is (R.E.all) with Inline;
        package Cursors_Forward_Convert
           is new Conts.Cursors.Constant_Forward_Convert_Traits
            (Cursors.Constant_Forward, String, From_Ref_To_Elem);
     end Container;""",
     "with Conts.Lists.Nodes.Unbounded, Conts.Elements.Arrays;\n" + \
     "with Conts.Lists.Generics, Ada.Finalization, Conts.Lists.Cursors;",
     comments=Comments(
         countif='conversion to String',
         fill='strange, since we are doing fewer mallocs')
    ).gen(use_cursor_convert=True)

ads.write("with Report; use Report;\n")
ads.write("package Generated_Tests is\n")
ads.write("   pragma Style_Checks (Off);\n")
ads.write(spec_contents)
ads.write("end Generated_Tests;\n")
ads.close()

adb.write(body_withs)
adb.write("\n")
adb.write("with Perf_Support;  use Perf_Support;\n")
adb.write("with Conts.Algorithms;\n")
adb.write("with Conts.Cursors.Adaptors;\n")
adb.write("package body Generated_Tests is")
adb.write("   pragma Style_Checks (Off);\n")
adb.write(body_contents)
adb.write("end Generated_Tests;\n")
adb.close()
