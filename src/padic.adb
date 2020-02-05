with Ada; use Ada;
With System; Use System;
with Text_IO; use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with padic_numbers;
with padic_numbers.IO;
with Ada.Strings.Unbounded; -- 190708 Added to enable use of Put_Command_Line_Argument.
with GNATCOLL; use GNATCOLL;
procedure padic is
   use padic_numbers;
   use padic_numbers.IO;
   use Integer_Rationals;
   use Supporting_Functions;
   use Integer_Prime;
   use Ada.Strings.Unbounded;

   argv : argvarray:= Get_Command_Line_Arguments;
   command : Unbounded_String := (if argv'size > 0 then argv(1) else To_Unbounded_String(" ")); -- 191228 Take care of case with empty argument list

   i : Integer := 0;
   -- 190709 Introduce new input parameters for command (-e,-e+,-e-,-e*,-e/,-d,-d+,-d-,-d*,-d/ ,-h, -help) e for encode and d for decode together with operation o = (+,-,*,/).
   a : MaxInteger := (if (command = "-e" or command = "-e+" or command = "-e-" or command = "-e*" or command = "-r" or command = "-r+" or command = "-r-" )
                      and argv'Length >= 2 then MaxInteger'Value(To_String(argv(2))) else 0);  -- use program with main  -o a b p
   b : MaxInteger := (if (command = "-e" or command = "-e+" or command = "-e-" or command = "-e*" or command = "-r" or command = "-r+" or command = "-r-" )
                      and argv'Length >= 3 then MaxInteger'Value(To_String(argv(3))) else 1);
   p : MaxInteger := (if (command = "-e" or command = "-e+" or command = "-e-" or command = "-e*" or command = "-r" or command = "-r+" or command = "-r-" )
                      and argv'Length >= 4 then MaxInteger'Value(To_String(argv(4))) else 1);  -- Check 190404 find ways to select prime !!!
   logg : Boolean := (if (command = "-e" or command = "-e+" or command = "-e-" or command = "-e*" or command = "-r" or command = "-r+" or command = "-r-" )
                      and argv'Length >= 5 then (To_String(argv(5)) = "-logg") else False);
   TestRun : Boolean := (if (command = "-e" or command = "-e+" or command = "-e-" or command = "-r" or command = "-r+" or command = "-r-" )
                         and argv'Length >= 6 then (To_String(argv(6)) = "-t") else False);

   imax : Integer := (if (command = "-e+" or command = "-e-" or command = "-r+" or command = "-r-")
                      and TestRun then Integer'Value(To_String(argv(7))) else 1);

   r : Integer := (if a /= 0 and p /= 1 then Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))) + 1 else -- 190805 Added +1 to recoverylength
                      Integer(Float(2)*Log(Float(MAX(Max_Int,b))/Float(0.618))/Log(Float(2)))+1); -- 190810 "+1" added here as well to estimate size of Henselvec
   ratio1, ratio2, ratio2b,ratio2c, ratio3, ratio4, ratio5, ratio6 : Rational :=(0,1);
   -- dot_pos5 : Integer;
   dot_pos6 : Integer;

   MaxLen : Integer := (if p /= 1 then floor(log(float(Max_Int))/log(float(p)))-1 else floor(log(float(Max_Int))/log(float(2)))-1); -- 190614 consitent with all other MaxLen calc !

   -- 190709 parameters for decode operation
   -- type decodetype is (variable, fixed);
   -- decindicator : decodetype := (if ((command = "-d") and (argv'Length >= 7) and argv(7) = "](") then variable else fixed);
   -- DecodeDot : Integer := (if (command = "-d" and argv'Length >= 3) then Integer'Value(To_String(argv(3))) else 0);

   H5,H6 : HenselCode := (size => MaxLen+3, vec => (0, others => -(p+1)), FirstPerPos => 0, LastPerPos => 0,
                          LastPos => MaxLen, dot_pos => 0, prime => p, RecoveryLength => r, CompletePeriod => False,
                          NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLen, ordinal => 0, value => (0,1));
   -- type Henselvec inside HenselCode is array (0..MaxLen) of Integer; -- make sure enough space for the periodic part. +3 for start and end of periodic part, and added one pos for negating vector
   -- (0, others => -(p+1)); -- mark untouched positions with impossible number for the algoritm to identify potential errors.

   abs_a : MaxInteger := abs a;

   MaxLenArr : array(1..primelistsize) of Integer := (floor(log(float(Max_Int))/log(float(ResidualPrimes(1))))-1,
                                                      floor(log(float(Max_Int))/log(float(ResidualPrimes(2))))-1,
                                                      floor(log(float(Max_Int))/log(float(ResidualPrimes(3))))-1);

   r1 : Integer := (if a /= 0 and p /= 1 then Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(ResidualPrimes(1)))) + 1 else -- 190805 Added +1 to recoverylength
                       Integer(Float(2)*Log(Float(MAX(Max_Int,b))/Float(0.618))/Log(Float(2)))+1);

   r2 : Integer := (if a /= 0 and p /= 1 then Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(ResidualPrimes(2)))) + 1 else -- 190805 Added +1 to recoverylength
                       Integer(Float(2)*Log(Float(MAX(Max_Int,b))/Float(0.618))/Log(Float(2)))+1);

   r3 : Integer := (if a /= 0 and p /= 1 then Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(ResidualPrimes(3)))) + 1 else -- 190805 Added +1 to recoverylength
                       Integer(Float(2)*Log(Float(MAX(Max_Int,b))/Float(0.618))/Log(Float(2)))+1);

   -- pragma Suppress_All;
   HenselCodeArray : HenselCodePtrArr := (HenselCodePtrArr'First => (new HenselCode'(size => MaxLenArr(1)+3, vec => (0, others => -(ResidualPrimes(1)+1)), FirstPerPos => 0, LastPerPos => 0,
                                          LastPos => MaxLenArr(1), dot_pos => 0, prime => ResidualPrimes(1), RecoveryLength => r1, CompletePeriod => False,
                                          NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLenArr(1), ordinal => 0, value => (0,1))),

                                          HenselCodePtrArr'First+1 => (new HenselCode'(size => MaxLenArr(2)+3, vec => (0, others => -(ResidualPrimes(2)+1)), FirstPerPos => 0, LastPerPos => 0,
                                          LastPos => MaxLenArr(2), dot_pos => 0, prime => ResidualPrimes(2), RecoveryLength => r1, CompletePeriod => False,
                                          NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLenArr(2), ordinal => 0, value => (0,1))),

                                          HenselCodePtrArr'First+2 =>  (new HenselCode'(size => MaxLenArr(3)+3, vec => (0, others => -(ResidualPrimes(3)+1)), FirstPerPos => 0, LastPerPos => 0,
                                          LastPos => MaxLenArr(3), dot_pos => 0, prime => ResidualPrimes(3), RecoveryLength => r1, CompletePeriod => False,
                                          NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLenArr(3), ordinal => 0, value => (0,1))));

   Procedure intro is
   begin -- 200203 Removed test for command = "-e" and "-e+"

      Put("-- Padic.adb : argv = "); Put_Command_Line_Arguments(argv); Put(" argc = "); Put_Command_Line_Count(argv); New_line;
      Put_Line("-- Padic.adb : Running  on a "&System_name'Image&" with Max_Int = "&Max_Int'Image&" **** AdaCore release 2019",true);
      Put("-- Padic.adb : Running in directory ",logg); Put_line(Current_Directory,logg);
      Put_Line("-- Padic.adb : Find the p-adic sequence of a number by entering a b p: "&a'Image&" "&b'Image&" "&p'Image,logg);
      Put("-- Padic.adb : Decompose(a) = "); PutList(Decompose(a),True); Put(" Decompose(b) = ",True); PutList(Decompose(b),True); Put(" Decompose(SQR(MAX(a,b)) = ");
      PutList(Decompose(MaxInteger(Sqrt(Float(MAX(abs_a,b))))),True); New_line;
      Put_Line("-- Padic.adb : Estimated recovery length : Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))) = "
               & Integer(Float(2)*Log(Float(MAX(abs_a,b))/Float(0.618))/Log(Float(p)))'Image,True);
      Put_Line("-- Padic.adb : Padic ordinal = "&Integer'Image(padic_ordinal_rational((a,b),p)),logg);
      Put_Line("-- Padic.adb : H6.vec'Length = "&Integer(H6.vec'Length)'Image&" H6.size = "&H6.size'Image&" MaxLen = "&MaxLen'Image,logg);
      -- Put("-- Padic.adb : Running in directory ",logg); Put_line(Current_Directory,logg);
      -- Put_Line("-- Padic.adb : Find the p-adic sequence of a number by entering a b p: "&a'Image&" "&b'Image&" "&p'Image,logg);
      -- Put_Line("-- Padic.adb : Estimated recovery length : Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))) = "
      --          & Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p)))'Image,True);
      -- Put_Line("-- Padic.adb : Padic ordinal = "&Integer'Image(padic_ordinal_rational((a,b),p)),logg);
      -- Put_Line("-- Padic.adb : H6.vec'Length = "&Integer(H6.vec'Length)'Image&" H6.size = "&H6.size'Image&" MaxLen = "&MaxLen'Image,logg);

   end intro;

   procedure help is
   begin
      Put_Line("-- Padic.adb -h : Command line argument error.");
      Put_Line("-- Padic.adb -h : Ex padic -eo a b p -logg -t 100 : Do encode of a/b with prime p and logg all and run 100 testruns with");
      Put_Line("-- Padic.adb -h : 1) (a,b)o(3+a,2*b), 2) (1+a,6+b)o(1+a,2+b), 3) (3+a,2*b)o(a,b)");
      Put_Line("-- Padic.adb -h : where o is the operations to test (+,-,*,/), a := a + 1 and b := b + 1 updated for each testrun");
      Put_Line("-- Padic.adb -h : if o omitted and set to ' ' then only encode of a/b is performed.");
      Put_Line("-- Padic.adb -h : Ex padic -d [ FirstPerPos , LastPerPos ]( a0 a1 a2 a3 a4 a5 a6 . . ., dot_pos )( prime ) for variable Hensel code.");
      Put_Line("-- Padic.adb -h : Ex padic -d [ length ]( a0 a1 a2 a3 a4 a5 a6 . . ., dot_pos )( prime ) for fixed Hensel code.");
      Put_line("-- Padic.adb -h : Do decode of the hensel code a0 a1 a2 a3 a4 a5 a6 . . . with dot_pos and prime");
      -- 190815 : Start of introducing residual encoding.
      Put_line("-- Padic.adb -h : Ex padic -r a b l -logg -t 100 : Do residual encode with length l of a/b with ResidualPrimes, logg all and run 100 testruns");
      Put("-- Padic.adb -h : where ResidualPrimes = "); for i in ResidualPrimes'Range loop Put(ResidualPrimes(i)'Image); end loop; New_line;
   end help;

begin -- padic

   -- 190708 Introduce a argument parser that selects functions to test that separate from severals tests from a single
   -- 190708 set -e for encode, set -e+ for add, set -d for decode ... for each function use -logg to print all
   -- 190709 and -t to indicate a sequence of tests and the last parameter to set the nr of tests.
   -- 190709 Ex padic -eo a b p -logg -t 100 : Do encode of a/b with prime p and logg all and run 100 testruns with
   -- 190709 1) (a,b)o(3+a,2*b), 2) (1+a,6+b)o(1+a,2+b), 3) (3+a,2*b)o(a,b) where o is the operations tested (+,-,*,/), a := a + 1 and b := b + 1 updated for each testrun.

   if command /= "-h" and command /= "-d" then intro; -- 200203 : removed test for command = "-e" and command = "-e+" and added test for command = "-h" and "-d"
   end if;


   if command = "-e" then

      if encode6(a,b,H6,dot_pos6,0,logg) then  -- 190708 remove use of encode5 and continue with encode6 only.
         Put_line(" ",logg);
         Put("-- Padic.adb : Encoded6 ordinal("&a'Image&" / "&b'Image&") = "); Put_Line(H6.ordinal'Image);
         Put("-- Padic.adb -e: Encoded6 P-adic sequence: ",true);Put_Hensel_code(H6,True); New_Line;
         -- Put("-- Padic.adb -e: EncodeFixHensel : "); Put_Fixed_Hensel_code(EncodeFixHensel(a,b,p,MaxLen,logg),MaxLen,True); New_line;
         -- H5.LastPos := H5.vec'Length;
         H6.LastPos := H6.vec'Length;
         -- Put_line("Min length5 for correct recovery of the rational is r = "&H5.RecoveryLength'Image&" MaxLen5 = "&MaxLen5'Image);
         -- Put_line("Min length6 for correct recovery of the rational is r = "&H5.RecoveryLength'Image&" MaxLen6 = "&MaxLen6'Image);
         -- ratio4 := Reduction(decode4(H5,logg)); ratio3 := (a,b) + (-ratio4);
         ratio6 := Reduction(decode4(H6,logg)); ratio5 := (a,b) + (-ratio6);
         -- Put_Line("-- Padic.adb : Decode4 of Encoded5: Decoded rational : "& Numerator(ratio4)'Image & "/" & Denominator(ratio4)'Image);
         Put_Line("-- Padic.adb -e: Decode4 of Encoded6: Decoded rational : "& Numerator(ratio6)'Image & "/" & Denominator(ratio6)'Image);
         -- Put_Line("-- Padic.adb -e: Decode4 of EncodeFixHensel : "&Image(Decode4(EncodeFixHensel(a,b,p,MaxLen,logg),logg)),True);

         -- Put_line("Encoded5 rational - Decoded rational : "&a'Image&"/"&b'image&" - "&
         --           ratio4.Numerator'Image&"/"&ratio4.Denominator'Image&" = "&ratio3.Numerator'Image&" / "&ratio3.Denominator'Image);

         -- Put_line("-- Padic.adb : Encoded6 rational - Decoded rational : "&a'Image&"/"&b'image&" - "&
         --            ratio6.Numerator'Image&"/"&ratio6.Denominator'Image&" = "&ratio5.Numerator'Image&" / "&ratio5.Denominator'Image,true);

         Put_line("-- Padic.adb -e: Encoded6 rational - Decode4(EncodeHensel) rational : "&Image(Reduction((a,b)))&" - "&
                    Image(Decode4(EncodeHensel(a,b,p),logg))&" = "&Image(Reduction((a,b)) + (-Decode4(EncodeHensel(a,b,p),logg))),True);

         -- Put_line("-- Padic.adb -e: Encoded6 rational - Decode4(EncodeFixHensel) rational : "&Image(Reduction((a,b)))&" - "&
         --           Image(Decode4(EncodeFixHensel(a,b,p,MaxLen,logg),logg))&" = "&Image(Reduction((a,b)) + (-Decode4(EncodeFixHensel(a,b,p,MaxLen,logg),logg))),True);
      else
         Put_Line("-- Padic.adb -e: No padic number found or Overflow",True);
      end if;
   elsif command = "-e+" then
      if TestRun or imax = 1 then
         Put_line("-- Padic.adb -e+ : First a/b = "&Image((a,b))&" Testruns = "&imax'Image,True);
         loop
            -- 190813 Remove tests of Add2 continue only with add3 and sub.
            -- New_line; Put_Line("-- Padic.adb -e+ : Add2 : "&Image((a,b))&" + "&Image((3+a,2*b))&" = "&Image((a,b)+(3+a,2*b))
            --                    &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)+(3+a,2*b)),Denominator((a,b)+(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            -- ratio1 := Decode4(add2(EncodeHensel(a,b,p),EncodeHensel(3+a,2*b,p)),logg);
            -- Put(" = "&Image(ratio1),True);
            -- if ratio1 = (a,b)+(3+a,2*b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            -- New_Line; Put_Line("-- Padic.adb -e+ : Add2 : "&Image((1+a,6+b))&" + "&Image((1+a,2+b))&" = "&Image((1+a,6+b)+(1+a,2+b))
            --                    &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((1+a,6+b)+(1+a,2+b)),Denominator((1+a,6+b)+(1+a,2+b))))/Float(0.618))/Log(Float(p)))'Image,True);
            -- ratio2 := Decode4(add2(EncodeHensel(1+a,6+b,p),EncodeHensel(1+a,2+b,p)),logg);
            -- Put(" = "&Image(ratio2),True);
            -- if ratio2 = (1+a,6+b)+(1+a,2+b) then Put(" Success"); else Put(" Failure"); end if;New_line;

            -- New_line; Put_Line("-- Padic.adb -e+ : Add2 : "&Image((3+a,2*b))&" + "&Image((a,b))&" = "&Image((a,b)+(3+a,2*b))
            --                    &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)+(3+a,2*b)),Denominator((a,b)+(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            -- ratio3 := Decode4(add2(EncodeHensel(3+a,2*b,p),EncodeHensel(a,b,p)),logg);
            -- Put(" = "&Image(ratio3),True);
            -- if ratio3 = (3+a,2*b)+(a,b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            Put_Line("-- Padic.adb -e+ : Add3 : "&Image((a,b))&" + "&Image((3+a,2*b))&" = "&Image((a,b)+(3+a,2*b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)+(3+a,2*b)),Denominator((a,b)+(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio1 := Decode4(add3(EncodeHensel(a,b,p),EncodeHensel(3+a,2*b,p),logg),logg);
            Put(" = "&Image(ratio1),True);
            if ratio1 = (a,b)+(3+a,2*b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            Put_Line("-- Padic.adb -e+ : Add3 : "&Image((1+a,6+b))&" + "&Image((1+a,2+b))&" = "&Image((1+a,6+b)+(1+a,2+b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((1+a,6+b)+(1+a,2+b)),Denominator((1+a,6+b)+(1+a,2+b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio2 := Decode4(add3(EncodeHensel(1+a,6+b,p),EncodeHensel(1+a,2+b,p),logg),logg);
            Put(" = "&Image(ratio2),True);
            if ratio2 = (1+a,6+b)+(1+a,2+b) then Put(" Success"); else Put(" Failure"); end if; New_line;

            Put_Line("-- Padic.adb -e+ : Add3 : "&Image((3+a,2*b))&" + "&Image((a,b))&" = "&Image((a,b)+(3+a,2*b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)+(3+a,2*b)),Denominator((a,b)+(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio3 := Decode4(add3(EncodeHensel(3+a,2*b,p),EncodeHensel(a,b,p),logg),logg);
            Put(" = "&Image(ratio3),True);
            if ratio3 = (3+a,2*b)+(a,b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            a := a+1; b := b+1; i := i + 1;
            if (a = 0) then a := a+1; -- 190611 Removing silly tests !
            elsif (1+a = 0) then a := a+1;
            elsif (3+a = 0) then a := a+1;
            elsif (6+b = 0) then b := b+1;
            elsif (2+b = 0) then b := b+1;
            end if;
            if a = 0 then a := a+1; end if;
            Put_line("-- Padic.adb -e+ : Next a/b = "&Image((a,b)),True);
            exit when i > imax;
         end loop;
      end if;
   elsif command = "-e-" then
      if TestRun or imax = 1 then
         Put_line("-- Padic.adb -e- : First a/b = "&Image((a,b))&" Testruns = "&imax'Image,True);
         loop
            Put_Line("-- Padic.adb -e- : Sub : "&Image((a,b))&" - "&Image((3+a,2*b))&" = "&Image((a,b)-(3+a,2*b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)-(3+a,2*b)),Denominator((a,b)-(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio1 := Decode4(Sub(EncodeHensel(a,b,p),EncodeHensel(3+a,2*b,p),logg),logg);
            Put(" = "&Image(ratio1),True); if ratio1 = (a,b)-(3+a,2*b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            Put_Line("-- Padic.adb -e- : Sub : "&Image((1+a,6+b))&" - "&Image((1+a,2+b))&" = "&Image((1+a,6+b)-(1+a,2+b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((1+a,6+b)-(1+a,2+b)),Denominator((1+a,6+b)-(1+a,2+b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio2 := Decode4(Sub(EncodeHensel(1+a,6+b,p),EncodeHensel(1+a,2+b,p),logg),logg);
            Put(" = "&Image(ratio2),True); if ratio2 = (1+a,6+b)-(1+a,2+b) then Put(" Success"); else Put(" Failure"); end if;New_line;

            Put_Line("-- Padic.adb -e- : Sub : "&Image((3+a,2*b))&" - "&Image((a,b))&" = "&Image((3+a,2*b)-(a,b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)-(3+a,2*b)),Denominator((a,b)-(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio3 := Decode4(Sub(EncodeHensel(3+a,2*b,p),EncodeHensel(a,b,p),logg),logg);
            Put(" = "&Image(ratio3),True); if ratio3 = (3+a,2*b)-(a,b) then Put(" Success"); else Put(" Failure"); end if; New_Line;
            a := a+1; b := b+1; i := i + 1;
            if (a = 0) then a := a+1; -- 190611 Removing silly tests !
            elsif (1+a = 0) then a := a+1;
            elsif (3+a = 0) then a := a+1;
            elsif (6+b = 0) then b := b+1;
            elsif (2+b = 0) then b := b+1;
            end if;
            if a = 0 then a := a+1; end if;
            Put_line("-- Padic.adb -e- : Next a/b = "&Image((a,b)),True);
            exit when i > imax;
         end loop;
      end if;
   elsif command = "-e*" then
      if TestRun or imax = 1 then
         Put_line("-- Padic.adb -e* : First a/b = "&Image((a,b))&" Testruns = "&imax'Image,True);
         loop
            Put_Line("-- Padic.adb -e* : Mult : "&Image((a,b))&" * "&Image((3+a,2*b))&" = "&Image((a,b)*(3+a,2*b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)*(3+a,2*b)),Denominator((a,b)*(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio1 := Decode4(mult(EncodeHensel(a,b,p),EncodeHensel(3+a,2*b,p),logg),logg);
            Put(" = "&Image(ratio1),True);
            if ratio1 = (a,b)*(3+a,2*b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            Put_Line("-- Padic.adb -e* : Mult : "&Image((1+a,6+b))&" * "&Image((1+a,2+b))&" = "&Image((1+a,6+b)*(1+a,2+b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((1+a,6+b)*(1+a,2+b)),Denominator((1+a,6+b)*(1+a,2+b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio2 := Decode4(mult(EncodeHensel(1+a,6+b,p),EncodeHensel(1+a,2+b,p),logg),logg);
            Put(" = "&Image(ratio2),True);
            if ratio2 = (1+a,6+b)*(1+a,2+b) then Put(" Success"); else Put(" Failure"); end if; New_line;

            Put_Line("-- Padic.adb -e* : Mult : "&Image((3+a,2*b))&" * "&Image((a,b))&" = "&Image((a,b)*(3+a,2*b))
                               &" Estimated recoverylength = "&Integer(Float(2)*Log(Float(MAX(Numerator((a,b)+(3+a,2*b)),Denominator((a,b)+(3+a,2*b))))/Float(0.618))/Log(Float(p)))'Image,True);
            ratio3 := Decode4(mult(EncodeHensel(3+a,2*b,p),EncodeHensel(a,b,p),logg),logg);
            Put(" = "&Image(ratio3),True);
            if ratio3 = (3+a,2*b)*(a,b) then Put(" Success"); else Put(" Failure"); end if; New_Line;

            a := a+1; b := b+1; i := i + 1;
            if (a = 0) then a := a+1; -- 190611 Removing silly tests !
            elsif (1+a = 0) then a := a+1;
            elsif (3+a = 0) then a := a+1;
            elsif (6+b = 0) then b := b+1;
            elsif (2+b = 0) then b := b+1;
            end if;
            if a = 0 then a := a+1; end if;
            Put_line("-- Padic.adb -e+ : Next a/b = "&Image((a,b)),True);
            exit when i > imax;
         end loop;
      end if;
   elsif command = "-e/" then
      Put_Line("-- Padic.adb -e/ : ");
   elsif command = "-r" then
      Put_Line("-- Padic.adb -r : ");
      -- 190815 function ResidualEncode(ain,bin : in MaxInteger; HenselCArray : in out HenselArrCode;  FixLength : Integer; logg : Boolean) return boolean
      -- 190816 Reuse the input p as the required length of residual representation
      if ResidualEncode(a,b,HenselCodeArray,Integer(p),logg) then
         Put_line(" ",logg);
      else
         Put_Line("-- Padic.adb -r: No residual number found or Overflow with listed primes in padic_numbers.ads",True);
      end if;
   elsif command = "-d" then
      Put("-- Padic.adb -d : argv = ");Put_Command_Line_Arguments(argv);
      Get(H6); ratio4 := Decode4(H6,logg); Put(" = "&Image(ratio4),True); New_line;
   elsif command =  "-h" or command = "-help" then
      help;
   else
      Put_Line("-- Padic.adb : use -h or -help to get a list of command options");
   end if;

end padic;
