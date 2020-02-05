with Ada.Text_IO; Use Ada.Text_IO;
with Ada.Strings.Unbounded; Use Ada.Strings.Unbounded;
package padic_numbers.IO is

   function Command_Line_Arguments return String;

   type argvarray is array(1..Ada.Command_Line.Argument_Count) of Unbounded_String;

   function Get_Command_Line_Arguments return argvarray;

   Procedure Put_Command_Line_Arguments(argv : in argvarray);

   Procedure Put_Command_Line_Count(argv : in argvarray);

   Procedure Get(HC : in out HenselCode);

   Procedure Put(X : in HenselCode);

end padic_numbers.IO;
