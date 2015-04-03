--  Support for output of the tests

package Report is

   type Time_Ref is (Ref_None, Ref_Fill, Ref_Loop);
   type Time_Ref_Array is array (Time_Ref) of Duration;

   type All_Cols is
      (Column_Title,
       Column_Fill,
       Column_Copy,
       Column_Loop,
       Column_For_Of,
       Column_Count_If,
       Column_Allocate,
       Column_Allocs,
       Column_Reallocs,
       Column_Frees);   --  Column_Frees must be last
   subtype Test_Cols is All_Cols range Column_Fill .. Column_Count_If;

   type Output is tagged record
      Show_Percent : Boolean := True;

      Basic        : Boolean := False;
      --  If true, only ASCII characters are used

      Current      : All_Cols := Column_Title;

      Fewer_Items  : Boolean;
      --  Whether the test is run on fewer items

      Ref          : Time_Ref_Array := (others => 0.0);
   end record;
   procedure Print_Header (Self : in out Output);
   procedure Reset (Self : in out Output);
   procedure Start_Line
      (Self : in out Output; Title : String; Fewer_Items : Boolean := False);
   procedure Print_Time
      (Self : in out Output; D : Duration; Extra : String := "");
   procedure Print_Not_Run (Self : in out Output; Extra : String := "");
   procedure Print_Size (Self : in out Output; Size : Natural);
   procedure Finish_Line (Self : in out Output);

   Stdout : Output;

end Report;
