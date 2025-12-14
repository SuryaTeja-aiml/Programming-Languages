-- procedure to create a simple user profile
-- this is a comment, syntax is two hyphens

-- importing libraries
with Ada.Text_IO;
-- Ada is the root library, Text_IO is a package inside Ada
-- used for basic input/output like Put_Line and Put

with Ada.Integer_Text_IO;
-- used for input/output of Integer values

with Ada.Strings.Unbounded;
-- provides Unbounded_String (no fixed length strings)

with Ada.Text_IO.Unbounded_IO;
-- sub-package of Text_IO
-- used to read/write Unbounded_String using Get_Line

procedure User_Profile is
   -- procedure performs actions but does not return a value

   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO.Unbounded_IO;
   -- use clause avoids writing full package names repeatedly

   -- variable declarations
   -- ':' associates variable name with its type
   User_Name  : Unbounded_String;
   Age        : Integer;
   Is_Student : String (1 .. 3);
   -- fixed-length string to store "Yes" or "No"
   Interest   : Unbounded_String;
   -- unbounded string for interests

begin
   -- executable statements start here

   -- ask for name
   Put_Line("Enter your name:");
   Get_Line(User_Name);
   -- reads a full line and stores it in User_Name

   -- ask for age
   Put_Line("Enter your age:");
   Get(Age);
   -- reads integer input
   Skip_Line;
   -- clears leftover newline from input buffer

   -- ask if the user is a student
   Put_Line("Are you a student? (Yes/No):");
   Get(Is_Student);
   Skip_Line;
   -- clears newline after fixed string input

   -- print basic details
   Put_Line("----- USER PROFILE -----");
   Put_Line("Name: " & To_String(User_Name));
   -- To_String converts Unbounded_String to normal String
   Put_Line("Age: " & Integer'Image(Age));
   -- Integer'Image converts integer to string

   -- modify output based on student status
   if Is_Student = "Yes" then
      Put_Line("Category: Student");
      Put_Line("Suggested Interests: Learning, Coding, Exams");
   else
      Put_Line("Category: Non-Student");
      Put_Line("Suggested Interests: Work, Skills, Life");
   end if;
   -- end if closes the conditional block

   -- take interests until user types "stop"
   Put_Line("Enter your interests one by one.");
   Put_Line("Type 'stop' to finish.");

   loop
      Put("Interest: ");
      Get_Line(Interest);
      -- reads interest input

      exit when To_String(Interest) = "stop";
      -- exits loop if user types "stop"

      Put_Line("Added interest: " & To_String(Interest));
      -- prints the entered interest
   end loop;
   -- end of loop

   Put_Line("Profile input completed.");

end User_Profile;
-- end of procedure User_Profile
