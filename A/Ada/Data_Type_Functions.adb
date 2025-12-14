with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

procedure Data_Type_Functions is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Float_Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO.Unbounded_IO;
   -- you can use as many functions as you want inside a single procedure
   --------------------------------------------------
   -- INTEGER FUNCTION
   --------------------------------------------------
   function Square(N : Integer) return Integer is
   -- syntax is 
   -- function Function_Name(Parameter : Type) return Return_Type
   -- parameter is input to function
   -- return type is type of value function will return
   begin
      return N * N; 
      -- return keyword is used to return value from function to caller
   end Square;

   --------------------------------------------------
   -- FLOAT FUNCTION
   --------------------------------------------------
   function Average(A, B : Float) return Float is
   begin
      return (A + B) / 2.0;
   end Average;

   --------------------------------------------------
   -- STRING FUNCTION
   --------------------------------------------------
   function Greet(Name : String) return String is
   begin
      return "Hello " & Name;
   end Greet;

   --------------------------------------------------
   -- UNBOUNDED STRING FUNCTION
   --------------------------------------------------
   function Add_Title(Name : Unbounded_String) return Unbounded_String is
   begin
      return To_Unbounded_String("Mr. ") & Name;
   end Add_Title;

   --------------------------------------------------
   -- BOOLEAN FUNCTION
   --------------------------------------------------
   function Is_Adult(Age : Integer) return Boolean is
   begin
      return Age >= 18;
   end Is_Adult;

   --------------------------------------------------
   -- ARRAY TYPE AND FUNCTION
   --------------------------------------------------
   type Int_Array is array (1 .. 5) of Integer;
   -- defining array type of 5 integers
   function Sum_Array(A : Int_Array) return Integer is
      Total : Integer := 0;
   begin
      for I in A'Range loop
      -- for loop syntax is 
      -- for Variable in Range loop ... end loop;
         Total := Total + A(I);
      end loop;
      return Total;
   end Sum_Array;

   --------------------------------------------------
   -- RECORD TYPE AND FUNCTION
   --------------------------------------------------
   type Person is record
      Name : Unbounded_String;
      Age  : Integer;
   end record;

   function Person_Info(P : Person) return Unbounded_String is
   begin
      return P.Name & To_Unbounded_String(" (Age ") &
             To_Unbounded_String(Integer'Image(P.Age)) &
             To_Unbounded_String(")");
   end Person_Info;

   --------------------------------------------------
   -- VARIABLES
   --------------------------------------------------
   Num       : Integer := 4;
   X, Y      : Float := 10.0;
   Name_Str  : String := "Surya";
   U_Name    : Unbounded_String := To_Unbounded_String("Surya");
   Numbers   : Int_Array := (1, 2, 3, 4, 5);
   User      : Person := (U_Name, 19);

begin
   --------------------------------------------------
   -- FUNCTION CALLS
   --------------------------------------------------
   Put_Line("Square of Integer:");
   Put_Line(Integer'Image(Square(Num))); 
   -- image is used to convert integer to string

   New_Line; -- adding a new line for better output formatting

   Put_Line("Average of Floats:");
   Put(Average(X, Y), Fore => 1, Aft => 2, Exp => 0); 
   -- Fore is number of digits before decimal point
   -- Aft is number of digits after decimal point
   -- Exp is number of digits in exponent
   -- => is used to specify named parameters
   New_Line;

   New_Line;
   Put_Line("String Function:");
   Put_Line(Greet(Name_Str));

   New_Line;
   Put_Line("Unbounded String Function:");
   Put_Line(To_String(Add_Title(U_Name)));

   New_Line;
   Put_Line("Boolean Function:");
   if Is_Adult(19) then 
   -- calling boolean function 
   -- boolean functions return true or false
      Put_Line("User is Adult");
   else
      Put_Line("User is Minor");
   end if;

   New_Line;
   Put_Line("Array Function:");
   Put_Line("Sum = " & Integer'Image(Sum_Array(Numbers)));

   New_Line;
   Put_Line("Record Function:");
   Put_Line(To_String(Person_Info(User)));

end Data_Type_Functions;
