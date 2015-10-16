#!/usr/bin/env python
"""
This script is used to generate test for the various container combinations.
"""

ads = open("tests/generated_tests.ads", "w")
adb = open("tests/generated_tests.adb", "w")

spec_contents = ""
body_withs = ""
body_contents = ""


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
        disable_count_if=False
    ):

        if elem_type.lower() == "integer":
            default = 2
        elif elem_type.lower() == "string":
            default = '"foo"'
        else:
            raise Exception("Unknown element type: %s" % elem_type)

        self.disable_count_if = disable_count_if
        self.args = dict(
            base=base,
            definite=definite,
            nodes=nodes,
            type=type,
            elem_type=elem_type,
            instance=instance,
            withs=withs,
            copy='',
            count_if='',
            discriminant='',
            prefix='',   # Prefix for Element, Next and Has_Element
            adaptors='', # Creating adaptors for standard containers
            default=default)

        if self.args['base'].lower() == "limited":
            # Need an explicit copy, since ":=" is not defined for limited types
            self.args['copy'] = '.Copy'
    
        if self.args['nodes'].lower() == "bounded":
            self.args['discriminant'] = ' (Capacity => Items_Count)'
    
        if not self.disable_count_if:
            self.args['count_if'] = """
         Stdout.Start_Test ("count_if");
         Co := Count_If (V2, Predicate'Access);
         Stdout.End_Test;
         Assert (Co, Items_Count);
    """

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
          Co : Natural := 0;
       begin
          Stdout.Start_Test ("fill");
          for C in 1 .. Items_Count loop
             V2.Append ({default});
          end loop;
          Stdout.End_Test;

          Stdout.Start_Test ("copy");
          declare
             V_Copy : Container.{type}'Class := V2{copy};
             pragma Unreferenced (V_Copy);
          begin
             --  Measure the time before we destroy the copy
             Stdout.End_Test;
          end;

          Stdout.Start_Test ("cursor loop");
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
          Stdout.Start_Test ("for-of loop");
          for E of V2 loop
             if Predicate (E) then
                Co := Co + 1;
             end if;
          end loop;
          Stdout.End_Test;
          Assert (Co, Items_Count);
          {count_if}
       end Run;

    begin
       Stdout.Start_Container_Test
          ("{base}", "{definite}", "{nodes}", "{type}", "{elem_type}");
       declare
          V : Container.{type}{discriminant};
       begin
          Run (V);
       end;
       Stdout.End_Container_Test;
    end Test_{base}_{definite}_{nodes}_{elem_type};
""".format(**self.args)

    def gen(self):
        """
        Generate tests for the new containers
        """
        self.args['prefix'] = 'V2.'
        self.args['adaptors'] = """
       function Count_If is new Conts.Algorithms.Count_If
          (Container.Cursors.Constant_Forward);
"""
        self.__common()

    def gen_ada2012(self, disable_checks=False, adaptors='{type}_Adaptors'):
        """
        Generate tests for the Ada 2012 containers
        """

        self.args['adaptors'] = ("""
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
     adaptors='Indefinite_List_Adaptors')
Test("Integer", "Ada12_No_Checks", "Definite", "Unbounded", "List",
     "package Container is new Ada.Containers.Doubly_Linked_Lists (Integer);",
     "with Ada.Containers.Doubly_Linked_Lists;").gen_ada2012(disable_checks=True)

Test("Integer", "Controlled", "Indefinite", "Unbounded", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded (Integer);",
     "with Conts.Lists.Indefinite_Unbounded;").gen()
Test("Integer", "Controlled", "Definite", "Unbounded", "List",
     "package Container is new Conts.Lists.Definite_Unbounded (Integer);",
     "with Conts.Lists.Definite_Unbounded;").gen()
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
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;").gen_ada2012(
     adaptors='Indefinite_List_Adaptors')
Test("String", "Ada12_No_Checks", "Indefinite", "Unbounded", "List",
     "package Container is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);",
     "with Ada.Containers.Indefinite_Doubly_Linked_Lists;").gen_ada2012(
         adaptors='Indefinite_List_Adaptors',
         disable_checks=True)

Test("String", "Controlled", "Indefinite", "Unbounded", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded (String);",
     "with Conts.Lists.Indefinite_Unbounded;").gen()
Test("String", "Controlled", "Indefinite", "Unbounded_Ref", "List",
     "package Container is new Conts.Lists.Indefinite_Unbounded_Ref (String);",
     "with Conts.Lists.Indefinite_Unbounded_Ref;",
     disable_count_if=True).gen()

ads.write("with Report; use Report;\n")
ads.write("package Generated_Tests is\n")
ads.write("   pragma Style_Checks (Off);\n")
ads.write(spec_contents)
ads.write("end Generated_Tests;\n")
ads.close();

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

