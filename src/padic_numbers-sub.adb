separate (padic_numbers)
function Sub( A , B : in HenselCode; logg : Boolean) return HenselCode is  -- 190809 Function Sub added to package padic_numbers
   use Supporting_Functions;

   undefined, Assertion_Error, Subtraction_Validation_Error : exception;

   APeriod :  Integer := (if A.FirstPerPos /= 0 then A.LastPerPos - A.FirstPerPos + 1 else 0); -- 190507 : No period if A is a pos integer
   BPeriod : Integer := (if B.FirstPerPos /= 0 then B.LastPerPos - B.FirstPerPos + 1 else 0); -- 190507 : No period if B is a pos integer
   MaxABprime : MaxInteger := MAX(MaxInteger(A.prime),MaxInteger(B.prime));
   MaxLen : Integer := floor(log(Float(Max_int))/log(Float(MaxABprime)))-1; -- 190524 reduced by 1, max length of hensel vector to avoid contraint error of Max_int.

   --   According to A method for Hensel Code Overflow Detection by Xin Kau Li, Chao Lu Towson University and Jon A Sjögren, Air Force Office of scientific Research
   --   the maximum length of the periodic part is lcm(m,n) where m, n is the respective periodic length for A resp B.

   ABPerLength : MaxInteger := (if APeriod /= 0 and BPeriod /= 0 then LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)) else GCD(MaxInteger(Aperiod),MaxInteger(BPeriod)));

   MaxABRecoveryLength : Integer := Integer(MAX(MaxInteger(A.RecoveryLength),MaxInteger(B.RecoveryLength))+ABPerLength);
   MaxAB : Integer := Integer(MAX(MaxInteger(A.MaxLen),MaxInteger(B.MaxLen))); -- 190509 Max R.vec size to ensure to prevent constraint error for MaxInteger.


   R : HenselCode := (size => MaxAB+3, vec => (0, others => -(MaxABprime+1)), FirstPerPos => 0, LastPerPos => 0,
                      LastPos => MaxAB, dot_pos => 0,  prime => MaxABprime, RecoveryLength => MaxABRecoveryLength, CompletePeriod => False,
                      NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxAB, ordinal => 0, value => (0,1));


   procedure negate_hensel(H : in HenselCode; R : out HenselCode) is
      First_per_pos : Integer := H.FirstPerPos; -- start pos of periodic part of incoming vector
      Last_per_pos : Integer := H.LastPerPos; -- end position of incoming part
      vecsize :Integer := H.size;
      tmp : MaxInteger := H.vec(First_per_pos); -- save first periodic number
      prime : MaxInteger := H.prime;
      period : Integer := Last_per_pos - First_per_pos + 1;

    begin
      -- 190809 Handle specical cases : Negative and Positive integer !!!!
      -- Sub : B = [ 0 , 1 ]( 1 ,  0 )( 2 )
      -- Sub : Negate hensel B = [ 0 , 1 ]
      R.ordinal := H.ordinal;
      R.FirstPerPos := H.FirstPerPos;
      R.LastPerPos := H.LastPerPos;

      if First_per_pos = 0 then -- 190809 Pos integer
         for i in 1..Last_per_pos + 1 loop
            if i = 1 then
               R.vec(i) := prime - H.vec(i);
            elsif i <= Last_per_pos then
               R.vec(i) := prime - 1 - H.vec(i);
            elsif i = Last_per_pos + 1 then
               R.vec(i+1) := MaxInteger(Last_per_pos) + 1; -- new start pos of periodic part
               R.FirstPerPos := Last_per_pos + 1;
               R.vec(0) := MaxInteger(Last_per_pos) + 1; -- new start pos of periodic part
               R.LastPerPos := Last_per_pos + 1;
               R.vec(i) := MaxInteger(prime) - 1; -- set Hensel digit for negative integer
            end if;
         end loop;
      else
         for i in 1..Last_per_pos + 1 loop -- min Last_per_pos = 1 leading to range 1..2
            if i = 1 then
               R.vec(i) := prime - H.vec(i);
            elsif i <= Last_per_pos then
               R.vec(i) := prime - 1 - H.vec(i);
            elsif i = Last_per_pos + 1 then
               if Last_per_pos /= First_per_pos and First_per_pos = 1 then -- Adjust pos of periodic sequence when negating pure periodic due to separate handling of pos 1.
                  R.vec(i+1) := MaxInteger(First_per_pos) + 1; -- new start pos of periodic part
                  R.FirstPerPos := First_per_pos + 1;
                  R.vec(0) := MaxInteger(Last_per_pos) + 1; -- new end pos of periodic part
                  R.LastPerPos := Last_per_pos + 1;
                  R.vec(i) := MaxInteger(prime) - 1 - tmp; -- move single periodic number one step
               elsif Last_per_pos = First_per_pos and H.vec(Last_per_pos-1) = H.vec(Last_per_pos) and Last_per_pos > 1 then  -- 190414 Changed to H.vec(Last_per_pos-1) = H.vec(Last_per_pos)
                  -- Do not adjust length when the two last padic numbers are equal and the periodic length is 1 and pointing to the last padic nr in the vector.
                  -- If av(last_index) = av(last_index - 1) and Last_index - First_index = 0 then period sequence starts at pos Last_index
                  R.vec(i) := tmp; -- tmp contain the start number of the incoming periodic sequence that shall keep the periodic start and end position.
               elsif Last_per_pos = First_per_pos and Last_per_pos = 1 then -- handle special case of Hansel vector length of 1 and period = 1
                  R.vec(i+1) := MaxInteger(First_per_pos) + 1; -- new start pos of periodic part
                  R.FirstPerPos := First_per_pos + 1;
                  R.vec(0) := MaxInteger(Last_per_pos) + 1; -- new end pos of periodic part
                  R.LastPerPos := Last_per_pos + 1;
                  R.vec(i) := prime - 1 - tmp;
               end if;
            end if;
            -- if i <= Last_per_pos then
            --    Put_Line("-- Sub : Negate hensel B : R.vec("&i'Image&")= "&R.vec(i)'Image);
            -- end if;
         end loop;
      end if;

   end negate_hensel;

begin -- sub
   Put("-- Sub : A = ",logg); Put_Hensel_Code(A,logg); Put_line(" ",logg);
   Put("-- Sub : B = ",logg); Put_Hensel_Code(B,logg); Put_line(" ",logg);
   negate_hensel(B,R);
   Put("-- Sub : Negate hensel B = ",logg); Put_Hensel_Code(R,logg); Put_line(" ",logg);
   -- 190813 Remove comparasion with Add2 keep only Add3.
   -- R := add2(A,R); New_line;
   -- Put("-- Sub result using Add2 : ",True); Put_Hensel_code(R,True); New_line;
   -- negate_hensel(B,R);
   R := add3(A,R,False); New_line;
   Put("-- Sub result using Add3 : ",True); Put_Hensel_code(R,True);
   return R;

exception

   when undefined =>
      Put_line("-- Sub : undefined : subtraction is undefined for Henselcodes A and B with different primes");
      return R;

   when Constraint_Error =>  -- 190610 Added to check reason for Constraint Error !
      Put_line("-- Sub : Constraint_Error: R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image);
      return R;

   when Assertion_Error =>
      Put_Line("-- Sub : Assertion_Error");
      return R;

   when Subtraction_Validation_Error =>
      Put_Line("-- Sub : Subtraction_Validation_Error");
      return R;

end Sub;
