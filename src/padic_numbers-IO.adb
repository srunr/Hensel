with Ada.Text_IO; Use Ada.Text_IO;
package body padic_numbers.IO is

   function Command_Line_Arguments return String is
      r : Unbounded_String;
   begin
      for i in 1..Ada.Command_Line.Argument_Count loop
         r := r & Ada.Command_Line.Argument(i)&" ";
      end loop;
      return To_String(r);
   end Command_Line_Arguments;


   function Get_Command_Line_Arguments return argvarray is
      r : argvarray;
   begin
      for i in 1..Ada.Command_Line.Argument_Count loop
         r(i) := To_Unbounded_String(Ada.Command_Line.Argument(i));
      end loop;
      return r;
   end Get_Command_Line_Arguments;

   Procedure Put_Command_Line_Arguments(argv : in argvarray) is
   begin
      for i in 1..Ada.Command_Line.Argument_Count loop
         Put(To_String(argv(i))&" ");
      end loop;
   end Put_Command_Line_Arguments;

   Procedure Put_Command_Line_Count(argv : argvarray) is
   begin
      Put(Integer(argv'Length)'Image);
   end Put_Command_Line_Count;


   Procedure Get(hc : in out HenselCode) is
      HenselCode_error : exception;
      pragma Suppress (All_Checks);
      function Get_HC return HenselCode is
         argvec : argvarray := Get_Command_Line_Arguments;
         command : String := To_String(argvec(1));
         VariableHensel : Boolean := ( if command = "-d" then (To_String(argvec(4)) = "," ) else False);
         FixedHensel : Boolean := not VariableHensel;
         prime : MaxInteger := (if command = "-d" then MaxInteger'Value(To_String(argvec(argvec'Last - 1))) else 1);
         dot_pos : Integer := (if command = "-d" then Integer'Value(To_String(argvec(argvec'Last - 3))) else 0);
         reclen : Integer := Integer(Float(2)*Log(Float(Max_Int)/Float(0.618))/Log(Float(prime))); -- estimate size of Henselvec
         MaxLen : Integer := floor(log(Float(Max_int))/log(Float(prime)))-1; -- 190524 reduced by 1. for prime 2 then MaxLen = 63 generates contraint error when prime**(MaxLen).
         R : HenselCode := (size => MaxLen+3, vec => (0, others => -(prime+1)), FirstPerPos => 0, LastPerPos => 0,
                            LastPos => MaxLen, dot_pos => dot_pos,  prime => prime, RecoveryLength => reclen, CompletePeriod => False,
                            NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLen, ordinal => dot_pos, value => (0,1));

         hc_variable_digit_start : constant Integer := 7;
         hc_variable_digit_end : constant Integer :=  (if hc_variable_digit_start+Integer'Value(To_String(argvec(5)))-1 < MaxLen then
                                                          hc_variable_digit_start+Integer'Value(To_String(argvec(5)))-1 else MaxLen);

         hc_fixed_digit_start : constant Integer := 5;
         hc_fixed_digit_end : constant Integer := (if hc_fixed_digit_start+Integer'Value(To_String(argvec(3)))-1 < MaxLen then
                                                      hc_fixed_digit_start+Integer'Value(To_String(argvec(3)))-1 else MaxLen);

      begin
         -- Put_Line("-- Get : prime = "&prime'Image, True);
         -- Put_Line("-- Get : reclen = "&reclen'Image, True);
         -- Put_Line("-- Get : MaxLen = "&MaxLen'Image, True);
         -- Put_Line("-- Get : R.dot_pos = "&R.dot_pos'Image, True);
         -- if VariableHensel then
         --    Put_Line("-- Get : hc_variable_digit_start = "&hc_variable_digit_start'Image, True);
         --    Put_Line("-- Get : hc_variable_digit_end  = "&hc_variable_digit_end'Image, True);
         -- elsif FixedHensel then
         --    Put_Line("-- Get : hc_fixed_digit_start  = "&hc_fixed_digit_start 'Image, True);
         --    Put_Line("-- Get : hc_fixed_digit_end  = "&hc_fixed_digit_end'Image, True);
         -- end if ;
         -- Put_line("-- Get : R.size = "&R.size'Image,True);

         if prime = 1 then
            raise HenselCode_error;
         elsif VariableHensel then
            R.FirstPerPos := Integer'Value(To_String(argvec(3)));
            R.LastPerPos := Integer'Value(To_String(argvec(5)));
            -- Put_Line("-- Get : R.FirstPerPos  = "&R.FirstPerPos'Image, True);
            -- Put_Line("-- Get : R.LastPerPos = "&R.LastPerPos'Image, True);
            for i in hc_variable_digit_start..hc_variable_digit_end loop
               R.vec(i-hc_variable_digit_start+1) := MaxInteger'Value(To_String(argvec(i)));
               -- Put_Line("-- Get : R.vec("&Integer(i-hc_variable_digit_start+1)'Image&") = MaxInteger'Value(To_String(argvec("&i'Image&"))) = "
               --         &MaxInteger'Value(To_String(argvec(i)))'Image, True);
            end loop;
            R.vec(0) := MaxInteger(R.LastPerPos);
            R.vec(R.LastPerPos + 1) := MaxInteger(R.FirstPerPos);
         elsif FixedHensel then
            R.LastPerPos := Integer'Value(To_String(argvec(3)));
            R.FirstPerPos := 1;  -- 190713 Dummy value.
            for i in hc_fixed_digit_start..hc_fixed_digit_end loop
               R.vec(i-hc_fixed_digit_start+1) := MaxInteger'Value(To_String(argvec(i)));
               -- Put_Line("-- Get : R.vec("&Integer(i-hc_fixed_digit_start+1)'Image&") = MaxInteger'Value(To_String(argvec("&i'Image&"))) = "
               --         &MaxInteger'Value(To_String(argvec(i)))'Image, True);
            end loop;
         end if;
         return R;
      end Get_HC;

   begin
      hc := Get_HC;
      -- Put("-- Get : Hensel Code = "); Put_Hensel_code(hc,True); New_line;
   exception when HenselCode_error =>
         Put_Line("-- Get : HenselCode : HenselCode format error");
      when Constraint_Error =>
         Put_Line("-- Get : HenselCode : HenselCode constraint error");
   end Get;


   Procedure Put(X : in HenselCode) is
   begin
     Null;
   end Put;

end padic_numbers.IO;
