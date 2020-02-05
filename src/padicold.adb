with Ada; use Ada;
With System; Use System;
with Text_IO; use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with padic_numbers;
procedure padic is
   use padic_numbers;
   use Integer_Rationals;
   use Supporting_Functions;
   use Integer_Prime;

   i : Integer := 1; -- used as index for printing the hensel vector:
   a : MaxInteger := MaxInteger'Value(Ada.Command_Line.Argument(1));  -- use program with main a b p or ./main a b p
   b : MaxInteger := MaxInteger'Value(Ada.Command_Line.Argument(2));
   p : MaxInteger := MaxInteger'Value(Ada.Command_Line.Argument(3));  -- Check 190404 find ways to select prime !!!
   r : Integer := Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))); -- estimate size of Henselvec
   ratio1, ratio2a, ratio2b,ratio2c, ratio3, ratio4 : Rational :=(0,1);
   dot_pos : Integer;
   MaxLen : Integer := floor(log(Float(Max_int)/log(Float(p))));

   H : HenselCode := (size => MaxLen+3, vec => (0, others => -(p+1)), First_per_pos => 0, Last_per_pos => 0,
                      Last_pos => MaxLen, dot_pos => 0, prime => p, RecoveryLength => r, CompletePeriod => False, NoPeriod => False);
   -- type Henselvec inside HenselCode is array (0..MaxLen) of Integer; -- make sure enough space for the periodic part. +3 for start and end of periodic part, and added one pos for negating vector
   -- (0, others => -(p+1)); -- mark untouched positions with impossible number for the algoritm to identify potential errors.

begin
   Put_line("***** Running padic on a "&System_name'Image&" with Max_Int = "&Max_Int'Image&" *************************************");
   Put("Running in directory "); Put_line(Current_Directory);
   Put_Line("Find the p-adic sequence of a number by entering a b p: ");
   Put("Decompose(a) = "); PutList(Decompose(a)); Put(" Decompose(b) = "); PutList(Decompose(b));Put_Line("");
   Put_Line(" Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))) = "& Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p)))'Image);
   Put_line("Padic ordinal = "&Integer(padic_ordinal_rational((a,b),p))'Image);
   if encode5(a,b, H, dot_pos) then
      New_line;
      Put("Encoded P-adic sequence: ");Put_Hensel_vector(H.vec,dot_pos); New_Line;
      H.Last_pos := H.vec'Length;
      Put("HenselCode record: ");Put_Hensel_Code(H); New_Line;
      Put_line("Min length for correct recovery of the rational is r = "&H.RecoveryLength'Image&" MaxLen = "&MaxLen'Image);
      -- if H.RecoveryLength <= H.Last_pos then -- do extended euclidian
         -- ratio1 := decode1(H.vec, H.prime, H.dot_pos);
         -- Put_Line("Decode1: Decoded rational : "& Numerator(ratio1)'Image & "/" & Denominator(ratio1)'Image);
         -- end if;
     --  if r >= H.RecoveryLength and r < MaxLen then
     --     ratio2a := decode2(H, r);
     --     Put_Line("Decode2(r): Decoded rational : "& Numerator(ratio2a)'Image & "/" & Denominator(ratio2a)'Image);
     --  end if;
     --  if r + 4 >= H.RecoveryLength and r + 4 < MaxLen then
     --     ratio2b := decode2(H, r+4); -- select 4 verification positions
     --     Put_Line("Decode2(r+4): Decoded rational : "& Numerator(ratio2b)'Image & "/" & Denominator(ratio2b)'Image);
     --  end if;
     --  if r + 4 >= H.RecoveryLength and r + 4 < MaxLen+2-Integer(p) then
     --     ratio2c := decode2(H, MaxLen-4); -- r < MaxInt+2-prime, use full available vector length.
     --     Put_Line("Decode2(Max_Len-1): Decoded rational : "& Numerator(ratio2c)'Image & "/" & Denominator(ratio2c)'Image);
     --  end if;
      ratio4 := decode4(H);
      Put_Line("Decode4: Decoded rational : "& Numerator(ratio4)'Image & "/" & Denominator(ratio4)'Image);
      -- ratio3 := decode3(H.vec, H.prime, H.dot_pos);
      -- Put_Line("Decode3: Decoded rational : "& Numerator(ratio3)'Image & "/" & Denominator(ratio3)'Image);
   else
      Put_Line("No padic number found or Overflow");
   end if;
end padic;
