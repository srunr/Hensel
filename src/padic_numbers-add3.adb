separate (padic_numbers)
function add3( A , B : in HenselCode; logg : Boolean) return HenselCode is
use Supporting_Functions;
   undefined, Assertion_Error, Addition_Validation_Error : exception;
   z: Integer := 0;
   A_ord : Integer := A.ordinal;
   B_ord : Integer := B.ordinal;
   AB_ord : Integer := A_ord + B_ord;
   APeriod :  Integer := (if A.FirstPerPos /= 0 then A.LastPerPos - A.FirstPerPos + 1 else 0); -- 190507 : No period if A is a pos integer
   BPeriod : Integer := (if B.FirstPerPos /= 0 then B.LastPerPos - B.FirstPerPos + 1 else 0); -- 190507 : No period if B is a pos integer
   MaxABprime : MaxInteger := MAX(MaxInteger(A.prime),MaxInteger(B.prime));
   MaxLen : Integer := floor(log(Float(Max_int))/log(Float(MaxABprime)))-1; -- 190524 reduced by 1, max length of hensel vector to avoid contraint error of Max_int.
   -- 190716 : Corrected to floor(log(Float(Max_int))/log(Float(MaxABprime)))-1
   carry,Rtmp,R1tmp,Atmp,Btmp,LastCarryIndex : MaxInteger := 0;
   common_nonperiodic : Boolean := (abs(B.ordinal - A.ordinal) <= (if A.FirstPerPos < B.FirstPerPos then A.FirstPerPos else B.FirstPerPos)) and (A.FirstPerPos /= 1) and (B.FirstPerPos /= 1);
   -- 190610 When A or B only periodic then false ! Added condition "and (A.FirstPerPos /= 1) and (B.FirstPerPos /= 1)"
   -- 190618 Check if common_nonperiodic needs to set to False when leading zeroes extend the leading index A or B FirstPerPos.
   -- 190618 If this is the case then keep initial leading index vector A or B; FirstPerPos and LastPerPos,
   Keep_First_Last : Boolean := False;
   s : Integer := 0;
   t : Integer := 0;

   LeadingZeroes : Integer := 0;
   j : Integer := 1; -- 190812 index used in main addition loop
   LeadingZeroesEnd : boolean := False;

   function MaxDivisor(a : MaxInteger) return MaxInteger
     with Pre => (a /= 0) is
      s : Number_List := Decompose(a);
      r : MaxInteger := 1;
      i : Integer := 0;
   begin
      -- Put("-- Add3 : MaxDivisor input = "); PutList(s); New_line;
      for i in s'Range loop
         r := MAX(r,s(i));
      end loop;
      -- Put_Line("-- Add2 : MaxDivisor result = "&r'Image);
      return r;
   end MaxDivisor;

   function MinDivisor(a : MaxInteger) return MaxInteger
     with Pre => (a /= 0) is
      s : Number_List := Decompose(a);
      r : MaxInteger := MaxDivisor(a);
      i : Integer := 0;
   begin
      -- Put("-- Add3 : MinDivisor input = "); PutList(s); New_line;
      for i in s'Range loop
         r := MIN(r,s(i));
      end loop;
      -- Put_Line("-- Add2 : MinDivisor result = "&r'Image);
      return r;
   end MinDivisor;

   --   According to A method for Hensel Code Overflow Detection by Xin Kau Li, Chao Lu Towson University and Jon A Sj�gren, Air Force Office of scientific Research
   --   the maximum length of the periodic part is lcm(m,n) where m, n is the respective periodic length for A resp B.

   ABPerLength : MaxInteger := (if APeriod /= 0 and BPeriod /= 0 then LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)) else GCD(MaxInteger(Aperiod),MaxInteger(BPeriod)));
   -- 190617 Can not divide LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)) with MaxDivisor(LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)))
   -- 190617 C:\Users\soren\Documents\Ada2012projects\Hensel2\obj>padic -77 18 2 - -t 1 | grep Fail
   -- 190617 Add result : [ 8, 9]( 1 0 0 0 0 1 1 1 0,-1)< 2> = -7/ 50 Failure
   -- 190617 Add result : [ 6, 6]( 1 1 1 1 0 0, 1)< 2> =  30/ 1 Failure
   -- 190430 Corrected to use APeriod and BPeriod. 190507 Corrected to allow A or B to be pos integer with no period.
   -- 190615 Correction needed! Compare with x(3) := x(1) + x(2)    p(3) <= MAX(p(1),p(2)) + n(3) + 2 according to https://www.cs.toronto.edu/~hehner/ratno.pdf
   -- 190702 n(3) = Use n(3) = MinDivisor(LCM(APeriod,BPeriod))
   MaxABRecoveryLength : Integer := Integer(MAX(MaxInteger(A.RecoveryLength),MaxInteger(B.RecoveryLength))+ABPerLength);
   MaxAB : Integer := Integer(MAX(MaxInteger(A.MaxLen),MaxInteger(B.MaxLen))); -- 190509 Max R.vec size to ensure to prevent constraint error for MaxInteger.

   -- 190807 : ExitIndex will be set to small value if both A and B has period 1.
   -- Padic.adb -e+ : Add2 : -43/ 98 + -46/ 49 = -135/ 98 Estimated recoverylength =  5
   -- EncodeHensel of -46 /  49 = [ 3 , 3 ]( 3 0 6 , -2 )( 7 )
   -- EncodeHensel of -43 /  98 = [ 3 , 3 ]( 3 0 3 , -2 )( 7 )
   -- Addition2 : [ 3 , 3 ]( 3 0 3 , -2 )( 7 ) + [ 3 , 3 ]( 3 0 6 , -2 )( 7 ) = -205852/ 20176779 Failure
   -- Add2 : A.MaxLen =  21 B.MaxLen =  21 MaxLen =  21
   -- Add2 : MaxAB =  21 ExitIndex =  8 ABPerLength =  1
   -- 190807 : Test if ExitIndex is less than floor(MaxLen/2) and set to MaxAB as a mitigation.
   -- 190808 : Still producing failure !!!
   -- 190808 : See logg190808_1.txt
   -- Padic.adb -e+ : Add2 : -599/ 364 + -599/ 360 = -108419/ 32760 Estimated recoverylength =  31
   -- EncodeHensel of -599 /  360 = [ 6 , 17 ]( 1 0 1 1 0 1 1 1 0 0 1 0 0 1 0 1 1 , -3 )( 2 )
   -- EncodeHensel of -599 /  364 = [ 6 , 17 ]( 1 1 0 1 0 0 1 0 1 0 0 1 1 0 0 0 1 , -2 )( 2 )
   -- Addition2 : [ 6 , 17 ]( 1 1 0 1 0 0 1 0 1 0 0 1 1 0 0 0 1 , -2 )( 2 ) + [ 6 , 17 ]( 1 0 1 1 0 1 1 1 0 0 1 0 0 1 0 1 1 , -3 )( 2 ) =  2881/ 38744 Failure
   -- Add2 : A.MaxLen =  62 B.MaxLen =  62 MaxLen =  62
   -- Add2 : MaxAB =  62 ExitIndex =  56 ABPerLength =  12
   -- Add2 : A.DenFactor =  4 B.DenFactor =  8
   -- Add2 : A.NumFactor =  1 B.NumFactor =  1
   -- Add2 : R.RecoveryLength =  29
   -- Add2 : APeriod =  12 BPeriod =  12 A.FirstPerPos =  6 A.LastPerPos =  17 B.FirstPerPos =  6 B.LastPerPos =  17
   -- Add2 : LCM(APeriod,BPeriod) =  12
   -- Add2 : Decompose(LCM(APeriod,BPeriod)) =  2 2 3
   -- Add2 : MaxDivisor(LCM(APeriod,BPeriod)) =  3
   -- Add2 : MinDivisor(LCM(APeriod,BPeriod)) =  2
   -- Add2 : MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)) =  6
   -- Add2 : Theory R.FirstPerPos <= MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))+MinDivisor(LCM(APeriod,BPeriod))+3 =  11
   -- Add2 : Theory R.LastPerPos <= R.FirstPerPos + ABPerLength - 1 =  22
   -- Add2 result : [ 2 , 44 ]( 1 1 0 0 0 0 0 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 0 0 1 , -3 )( 2 )
   -- 190808 : However the result before validation is correct !
   -- Add2 : result before validation: [ 18 , 29 ]( 1 1 0 0 0 0 0 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 , -3 )( 2 ) = -108419/ 32760
   -- 190808 : Conclusion is to use the result and do the calc : -599/ 364 + -599/ 360 = -108419/ 32760 as a initial validation as performed in padic.adb.

   ExitIndex : MaxInteger := (if (abs(MaxInteger(A.ordinal) - MaxInteger(B.ordinal)) + MAX(MaxInteger(A.LastPerPos),MaxInteger(B.LastPerPos)) + 3*ABPerLength + 2)
                              < floor(float(MaxLen)/2.0) then MaxInteger(MaxAB) else abs(MaxInteger(A.ordinal) - MaxInteger(B.ordinal)) + MAX(MaxInteger(A.LastPerPos),MaxInteger(B.LastPerPos)) + 3*ABPerLength + 2) ;
   -- 190702 Tested 3*ABPerLength and it works better.
   -- 190527 Changed to "+ 2*ABPerLength + 2" instead of "+ ABPerLength + 2".
   --        padic 77 18 2 - -t | grep Fail does not catch any faliures.
   -- 190507 : Using A.LastPerPos and B.LastPerPos instead of FirstPosPos for calc of ExitIndex.
   -- 190508 . Added 2 according to "A new representation of the rational numbers for fast easy arithmetic" by Eric C.R. Hehner and R. N. S. Horspool
   -- 190509 : Limiting ExitIndex wihtin limits if HenselCode size. Check is this needed. Potenially only when period part size + integer size > MaxAB.

   type carryvec_type is array (1..ExitIndex) of Integer;
   carryvec : carryvec_type := (others => 0);


   R : HenselCode := (size => MaxAB+3, vec => (0, others => -(MaxABprime+1)), FirstPerPos => 0, LastPerPos => 0,
                      LastPos => MaxAB, dot_pos => 0,  prime => MaxABprime, RecoveryLength => MaxABRecoveryLength, CompletePeriod => False,
                      NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxAB, ordinal => 0, value => (0,1));



   procedure shift_vector_left(z : in Integer; h : in out HenselCode) is
      LastPos : Integer := (if h.LastPos > h.MaxLen then h.MaxLen else h.LastPos);  -- 190513 Limit index
   begin
      Put_line("-- Add3 : shift_vector_left :  h.vec("&LastPos'image&") = "&MaxInteger'Image(h.vec(LastPos)));
      for k in 1..z loop -- z steps to left
         for i in 1..LastPos-1 loop  -- one step to the left 190503 h.LastPerPos or h.Lastpos needs to be set to ExitIndex. Try to use LastPos !!!
            h.vec(i) := h.vec(i+1); -- Put_line("-- shift_vector_left :  h.vec("&i'image&") = "&MaxInteger'Image(h.vec(i+1)));
         end loop;
      end loop;

      -- if the hensel vector is representing a positive integer then set values of FirstPerPos and LastPerPos from A or B
      -- R.ordinal := R.ordinal + z;  -- 190503 Shift left always impact ordinal with + z. 190507 Move outside shift_vector

   end shift_vector_left;

   --  190430 : Padic.adb : Encoded6 P-adic sequence: [ 0, 2]( 1 1,-1)< 2>
   --  Padic.adb : Encoded6 rational - Decode4(EncodeHensel) rational :  3/ 2 -  3/ 2 =  0/ 1
   --  This test run does not give  0, 2]( 1 1,-1)< 2> as a result.
   --  All the digits of the repetetive sequence turn into 0, leading to positrive integer 3 as a result and then divided by prime = 2 and the end
   --  Challenge: How to detect that we have a integer as the result ?
   --  Proposed answer: if all digits after i > (A.ordinal - B.ordinal) = 1 - (-1) = 2 assuming B as leading index is 0 then we have an integer as result.
   --  Equal to calculating nr of zeroes from ExitIndex backwards to first pos /= 0 and compare with ABPerLength.
   --  If nr of Trailing zeroes > ABLength then it's an integer and then we can remove the lasting zeroes and keep the ordinal
   --  Padic.adb : Add :  2/ 3 +  5/ 6 =  3/ 2
   --  Addition : [ 2, 3]( 1 1 0, 1)< 2> + [ 3, 4]( 1 1 1 0,-1)< 2> =
   --  R.vec( 1) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   --  R.vec( 2) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   --  R.vec( 3) := ( 1 +  1 +  0) mod  2 =  0 s =  0 t =  1
   --  R.vec( 4) := ( 1 +  0 +  1) mod  2 =  0 s =  1 t =  0
   --  R.vec( 5) := ( 0 +  1 +  1) mod  2 =  0 s =  0 t =  1
   --  R.vec( 6) := ( 1 +  0 +  1) mod  2 =  0 s =  1 t =  0
   --  R.vec( 7) := ( 0 +  1 +  1) mod  2 =  0 s =  0 t =  1
   --  Add : R.ordinal = -1
   --  Add : R.dot_pos =  0
   --  Add result : [ 2, 4]( 1 1 0 0,-1)< 2> =  5/ 14
   --
   --  190501 Corrected with introduction of the functions traling_zeroes and period_one for prime = 5, Check for prime = 2
   --  Padic.adb : Add :  2/ 3 +  5/ 6 =  3/ 2
   --  Addition : [ 2, 3]( 4 1 3, 0)< 5> + [ 2, 3]( 1 4 0, 1)< 5> =
   --  Add : MaxAB =  27 ExitIndex =  5 ABPerLength =  2
   --  R.vec( 1) := ( 4 +  0 +  0) mod  5 =  4 s =  0 t =  0
   --  R.vec( 2) := ( 1 +  1 +  0) mod  5 =  2 s =  1 t =  0
   --  R.vec( 3) := ( 3 +  4 +  0) mod  5 =  2 s =  0 t =  1
   --  R.vec( 4) := ( 1 +  0 +  1) mod  5 =  2 s =  1 t =  0
   --  R.vec( 5) := ( 3 +  4 +  0) mod  5 =  2 s =  0 t =  1
   --  R.vec( 6) := ( 1 +  0 +  1) mod  5 =  2 s =  1 t =  0
   --  Add : R.ordinal =  0 R.FirstPerPos =  2 R.LastPerPos =  3
   --  Add : R.dot_pos =  0 ExitIndex =  5
   --  period_one z =  5
   --  Add result : [ 2, 2]( 4 2, 0)< 5> =  3/ 2
   --
   -- Test 190509
   -- Padic.adb : Add :  79/ 20 +  82/ 40 =  6/ 1
   -- Addition : [ 4, 4]( 1 0 2 1,-1)< 5> + [ 4, 4]( 4 0 4 3,-1)< 5> =
   -- Add : A.MaxLen =  27 B.MaxLen =  27
   -- Add : MaxAB =  27 ExitIndex =  7 ABPerLength =  1
   -- Add : A.DenFactor =  5 B.DenFactor =  5
   -- Add : A.NumFactor =  1 B.NumFactor =  1
   -- Add : R.RecoveryLength =  5
   -- Add : APeriod =  1 BPeriod =  1
   -- R.vec( 1) := ( 1 +  4 +  0) mod  5 =  0 s =  0 t =  0
   -- R.vec( 2) := ( 0 +  0 +  1) mod  5 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 2 +  4 +  0) mod  5 =  1 s =  0 t =  0
   -- R.vec( 4) := ( 1 +  3 +  1) mod  5 =  0 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  3 +  1) mod  5 =  0 s =  0 t =  0
   -- R.vec( 6) := ( 1 +  3 +  1) mod  5 =  0 s =  0 t =  0
   -- R.vec( 7) := ( 1 +  3 +  1) mod  5 =  0 s =  0 t =  0
   -- Add : R.ordinal = -1 R.FirstPerPos =  5 R.LastPerPos =  5 R.LastPos =  7
   -- Add : R.dot_pos =  0 ExitIndex =  7 NoPeriod = FALSE LastCarryIndex =  7 R.ordinal = -1
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 5, 5]( 0 1 1 0 0,-1)< 5>
   -- trailing_zeroes =  2
   -- leading_zeroes =  1
   -- Add result : [-1,-1](, 0)< 5>
   -- Decode4: Constraint_Error in decode4, returning (0,1)
   -- =  0/ 1 Failure


   function trailing_zeroes(z : out Integer; h : in HenselCode; LastIndex : Integer) return boolean is
      k : Integer := (if LastIndex > h.size then h.size - 2 else LastIndex);  -- 190509 : Limit search index from max end index
      j : Integer := 1;
      found_integer : Boolean := false;
   begin
      Put_line("-- Add3 : trailing_zeroes : LastIndex = "&LastIndex'Image&" h.size = "&h.size'Image,logg);

      while h.vec(k) = 0 and k > h.FirstPerPos loop
         k := k - 1;
         j := j + 1;
      end loop;

      z := j - 1;
      found_integer := ( z > (h.LastPerPos - h.FirstPerPos + 1));
      -- 190512 : Reduction of LastIndex to h.size-2 may lead to that z can't evalute to something  > (h.LastPerPos - h.FirstPerPos + 1)
      Put_Line("-- Add3 : trailing_zeroes : found_integer = "&found_integer'Image,logg);
      return found_integer;

   end trailing_zeroes;


   function period_one(z : out Integer; h : in HenselCode; LastIndex : Integer) return boolean is
      -- identify if the addition led to a periodic sequence of period one.
      -- if this is the case adjust FirsPerPos, LastPerPos and ordinal
      k : Integer := (if LastIndex > h.size then h.size - 2 else LastIndex);  -- 190509 : Limit search index from max end index
      j : Integer := 1;
      found_period_one : Boolean := false;
      LastMaxIndex : Integer := (if LastIndex > h.size then h.size - 2 else LastIndex);
   begin
      -- 190807 Do we need to limit with "and k >= h.FirstPerPos" loop will end ayway ??
      while h.vec(k) = h.vec(LastMaxIndex) and k >= 1 loop -- 190503 Changed to k >= h.FirstPerPos as we need to count this positions as well
         k := k - 1;
         j := j + 1;
      end loop;

      z := j-1;  -- 190503 while compare using k in LastIndex..FirstPerPos, if no equal hensel digits then z := 0.

      found_period_one := ( z > (h.LastPerPos - h.FirstPerPos + 1));

      return found_period_one;

   end period_one;

   function carry_spill_over( cv : in carryvec_type; start_index : in Integer) return MaxInteger is
      i : MaxInteger := MaxInteger(start_index);
   begin -- 190514 : Added to catch several carry spillover positions from non-periodic to periodic positions
      while i < MaxInteger(cv'Length-1) and i /= 0 loop
         Put_line("-- Add3 : carryvec("&i'Image&") = "&cv(i)'Image,logg);
         exit when cv(i) = 0;
         i := i + 1;
      end loop;
      return i;
      -- 190514 return first pos with no spill over from non-periodic
      --        if no spillover return start_index
      -- 190523 What if abs(B.ordinal - A.ordinal) > Min(A.FirstPerPos,B.FirstPerPos)
      --        then there can't be spillover from common non-periodic postions
      -- 190524 Added catch of contraint error for index i.
   exception when Constraint_Error =>
         Put_line("-- Add3 : carry_spill_over : Constraint_Error for i = "&i'Image,logg);
         return i;
   end carry_spill_over;

   -- 190528 Adding Period_Sum function for comparasion of period sequences as check if R.FirstPosPeriod adjustment is needed.
   function Period_Sum(h : in HenselCode; FirstIndex, Period : Integer) return MaxInteger is
      sum : MaxInteger := 0;
      per : Integer := (if Period > Maxlen - FirstIndex then Maxlen - FirstIndex else Period);
   begin
      Put_line("-- Add3 : Period_Sum : h.prime = "&h.prime'Image&" FirstIndex = "&FirstIndex'Image&" Period = "&Period'Image&" h.MaxLen = "&h.MaxLen'Image,logg);
      for i in FirstIndex..FirstIndex+per-1 loop
         sum := sum + h.vec(i)*h.prime**(i-1);
         Put_Line("-- Add3 : Period_Sum : sum("&i'Image&")= "&sum'Image,logg);
      end loop;
      Put_line("-- Add3 : Period_Sum = "&sum'Image,logg);
      return sum;
   end Period_Sum;

   function ValidateResult( H : in out HenselCode; logg : in Boolean) return Boolean is
      ratioA, ratioB, ratioH : Rational := (0,1);
   begin
      Put("-- Add3 : ValidateResult H input : ",logg); Put_Hensel_code(H,logg); Put_Line(" ",logg);
      ratioH := Decode4(H,False);
      H.FirstPerPos := 1;
      H.LastPerPos := H.LastPos;
      H.RecoveryLength := floor(float(H.LastPos)/2.0);
      Put("-- Add3 : ValidateResult recovery attempt input : ",logg); Put_Hensel_code(H,logg); Put_Line(" ",logg);
      ratioA := Decode4(H,False);
      Put_line("-- Add3 : ValidateResult : recovery ratioH = "&Image(ratioA), logg);
      ratioB := Decode4(EncodeVariableHensel(Numerator(ratioA), Denominator(ratioA), H.prime, H.FirstPerPos, H.LastPerPos),logg);
      if (ratioA = ratioB) and ratioA = ratioH then
         H.value := ratioH;
         return True;
      else return False;
      end if;

   end ValidateResult;

begin -- 190812 add3

   -- 190630 Start by checking if A and/or B = 0 then then return accordingly ...
   if Numerator(decode4(A,False)) = 0 and Numerator(decode4(B,False)) = 0 then
      return A;
   elsif Numerator(decode4(A,False)) = 0 then
      return B;
   elsif Numerator(decode4(B,False)) = 0 then
      return A;
   end if;

   if A.prime /= B.prime then
      raise undefined;
   end if;
   Put("-- Addition3 : ",True); Put_Hensel_Code(A,True);Put(" + ",True);Put_Hensel_Code(B,True);Put(" = ",True); New_Line;
   Put_Line("-- Add3 : A.MaxLen = "&A.MaxLen'Image&" B.MaxLen = "&B.MaxLen'Image&" MaxLen = "&MaxLen'Image,logg);
   Put_Line("-- Add3 : MaxAB = "&MaxAB'Image&" ExitIndex = "&ExitIndex'Image&" ABPerLength = "&ABPerLength'Image,logg);
   Put_Line("-- Add3 : A.DenFactor = "&A.DenFactor'Image&" B.DenFactor = "&B.DenFactor'Image,logg);
   Put_Line("-- Add3 : A.NumFactor = "&A.NumFactor'Image&" B.NumFactor = "&B.NumFactor'Image,logg);
   Put_Line("-- Add3 : R.RecoveryLength = "&R.RecoveryLength'Image,logg);
   Put_Line("-- Add3 : APeriod = "&Aperiod'Image&" BPeriod = "&BPeriod'Image&" A.FirstPerPos = "&A.FirstPerPos'Image&" A.LastPerPos = "&A.LastPerPos'Image&" B.FirstPerPos = "&B.FirstPerPos'Image&" B.LastPerPos = "&B.LastPerPos'Image,logg);
   Put("-- Add3 : Decompose(LCM(APeriod,BPeriod)) = ",logg);
   if LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)) /= 0 then
      PutList(Decompose(LCM(MaxInteger(Aperiod),MaxInteger(BPeriod))),logg); Put_Line(" ",logg);
      Put_Line("-- Add3 : MaxDivisor(LCM(APeriod,BPeriod)) = "&MaxDivisor(LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)))'Image,logg);
      Put_Line("-- Add3 : MinDivisor(LCM(APeriod,BPeriod)) = "&MinDivisor(LCM(MaxInteger(Aperiod),MaxInteger(BPeriod)))'Image,logg);
      Put_Line("-- Add3 : MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)) = "&MaxInteger(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)))'Image,logg);
      Put_Line("-- Add3 : Theory R.FirstPerPos <= MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))+MinDivisor(LCM(APeriod,BPeriod))+3 = "&
                 MaxInteger(MaxInteger(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)))+MinDivisor(LCM(MaxInteger(APeriod),MaxInteger(BPeriod)))+3)'Image,logg);
      Put_Line("-- Add3 : Theory R.LastPerPos <= R.FirstPerPos + ABPerLength - 1 = "&
                 MaxInteger(MaxInteger(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)))+MinDivisor(LCM(MaxInteger(APeriod),MaxInteger(BPeriod)))+3 + ABPerLength - 1)'Image,logg);
   else Put_Line(LCM(MaxInteger(Aperiod),MaxInteger(BPeriod))'Image,logg);
   end if;

   -- if A.ordinal = B.ordinal and A.FirstPerPos = 0 and B.FirstPerPos /= 0 then ExitIndex := MaxInteger(B.LastPerPos); end if;
   -- if A.ordinal = B.ordinal and B.FirstPerPos = 0 and A.FirstPerPos /= 0 then ExitIndex := MaxInteger(A.LastPerPos); end if;
   -- 100507 Test to remove : if A.ordinal = B.ordinal and A.FirstPerPos = 0 and B.FirstPerPos = 0 then ExitIndex := MAX(MaxInteger(A.LastPerPos), MaxInteger(B.LastPerPos))+1; end if;
   -- if A.ordinal /= B.ordinal and A.FirstPerPos = 0 and B.FirstPerPos = 0 then ExitIndex := abs(MaxInteger(A.LastPerPos) - MaxInteger(B.LastPerPos))+1; end if;


   --  R.ordinal := A.ordinal + B.ordinal;

   --  padding of zeroes only needed when i+A.ordinal < 1 or i+B.ordinal < 1
   --  loop should exit when combined A and B periodic period is exhusted.
   --  if A.ordinal > B.ordinal then A has the leading index otherwise B has the leading index R.vec(i) := A.vec(i) + adjusted B
   --  190426: Remember to use periodic part repeatedly in the adding algoritm for each Hensel digit !!
   --  Take care of where both ordinals are negative and positive > 0 !!!!
   --  Common prime factors in Num and Den after addition ?
   --  190426 Consider case A.ordinal < B.ordinal and A.ordinal > 0 B.ordinal for Integers [-2**63-1,2**63-1]
   --  Take care of ending zeroes that can transfer to adjusted ordinal
   --
   --
   --  Cases to handle  190525 updated with Case 4-15
   --     Case 0  : A pos rational + B pos rational (R(A)+R(B))
   --     Case 1  : A pos integer + B pos rational  (I(A)+R(B))
   --     Case 2  : A pos rational + B pos integer  (R(A)+I(B))
   --     Case 3  : A pos integer + B pos integer   (I(A)+I(B))
   --     Case 4  : A neg integer + B neg integer   (-I(A)-I(B)) leads to Case 3 and then negate result before return = -add(A,B)
   --     Case 5  : A neg integer + B pos rational  (-I(A)+R(B)) = sub(B,A)
   --     Case 6  : A neg integer + B neg rational  (-I(A)-R(B)) leads to Case 1 and then negate result before return = -add(A,B)
   --     Case 7  : A pos integer + B neg rational  (I(A)-R(B)) = sub(A,B)
   --     Case 8  : A neg rational + B pos integer  (-R(A)+I(B)) = sub(B,A)
   --     Case 9  : A neg rational + B neg integer  (-R(A)-I(B)) leads to Case 2 and then negate result before return = -add(A,B)
   --     Case 10 : A neg rational + B neg rational (-R(A)-R(B)) leads to Case 0 and then negate result before return = -add(A,B)
   --     Case 11 : A neg rational + B pos rational (-R(A)+R(B)) = sub(B,A)
   --     Case 12 : A pos integer + B neg integer   (I(A)-I(B)) = sub(A,B)
   --     Case 13 : A pos rational + B neg integer  (R(A)-I(B)) = sub(A,B)
   --     Case 14 : A pos rational + B neg rational (R(A)-R(B)) = sub(A,B)
   --     Case 15 : A neg integer + B pos integer   (-I(A)+I(B)) = sub(B,A)
   --     Note : A + B can result in a postive integer, ie nr of traling zeroes is larger than ABPerLength.
   --   Cases for each of the above :
   --   if A.ordinal < B.ordinal  then  -- padding B  for A.ordinal and B.ordinal included in Integers [-2**63-1,2**63-1]
   --     See Test 1
   --   elsif A.ordinal > B.ordinal then -- padding A Integers [-2**63-1,2**63-1]
   --     See Test 3
   --   elsif A.ordinal = 0 and B.ordinal = 0 then -- for Integers [-2**63-1,2**63-1]
   --     See Test 2
   --   end if;
   --

   for i in 1..MaxAB loop

      if A.ordinal < B.ordinal then -- A leading index. 190426 Test for both B.ordinal > 0 and < 0
         -- Put_Line("-- A leading index");
         if  i <= B.ordinal-A.ordinal then -- i < B.FirstPerPos + B.ordinal - 1
            -- A has leading index and Btmp = 0 up to the relative ordinal difference
            Btmp := 0;  -- i <= B.ordinal - A.ordinal. Ex 2-(-1) = 3 for A=41.3131... and B=.014040...
            -- 190508 complete if .. end if below copied from case when i > B.ordinal-A.ordinal branch as the A.FirstPerPos can start before B.ordinal-A.ordinal
            if i < A.FirstPerPos and A.FirstPerPos /=0 then
               Atmp := A.vec(i);
            elsif i >= A.FirstPerPos and A.FirstPerPos /=0 then
               Atmp := A.vec(A.FirstPerPos+s);  -- Adjust periodic
               s := (s + 1) mod APeriod;
            elsif A.FirstPerPos = 0 then -- A is a postive integer
               if i <= A.LastPerPos then Atmp := A.vec(i); else Atmp := 0; end if;
            end if;

         elsif i > B.ordinal-A.ordinal then -- i within adjusted B indicies
            -- s index for A to cater for the periodic part, t index B to cater for the periodoc part

            if i < A.FirstPerPos and A.FirstPerPos /=0 then
               Atmp := A.vec(i);
            elsif i >= A.FirstPerPos and A.FirstPerPos /=0 then
               Atmp := A.vec(A.FirstPerPos+s);  -- Adjust periodic
               s := (s + 1) mod APeriod;
            elsif A.FirstPerPos = 0 then -- A is a postive integer
               if i <= A.LastPerPos then Atmp := A.vec(i); else Atmp := 0; end if;
            end if;

            if i < B.FirstPerPos+B.ordinal-A.ordinal and B.FirstPerPos /= 0 then -- 190426 Use relative difference: + B.ordinal - A.ordinal
               Btmp := B.vec(i-B.ordinal+A.ordinal);   -- 190426 index to be adjusted with the relative difference = B.ordinal - A.ordinal
            elsif i >= B.FirstPerPos+B.ordinal-A.ordinal and B.FirstPerPos /= 0 then
               Btmp := B.vec(B.FirstPerPos+t);  -- Adjust periodic
               t := (t + 1) mod Bperiod;
            elsif B.FirstPerPos = 0 then -- B is a positive integer
               if i <= B.LastPerPos+B.ordinal-A.ordinal then Btmp := B.vec(i-B.ordinal+A.ordinal); else Btmp := 0; end if;
            end if;

         end if; --  if  i <= B.ordinal then
         R.ordinal := A.ordinal;

      elsif B.ordinal < A.ordinal then -- B leading index R.vec(i) := B.vec(i) + adjusted A. 190426 Test for both A.ordinal > 0 and < 0

         -- Put_Line("-- B leading index R.vec(i) := B.vec(i) + adjusted A.");
         if  i <= A.ordinal - B.ordinal then -- i < A.FirstPerPos + B.ordinal - 1
            -- B has leading index and Btmp = 0 up to the absolute ordinal difference
            Atmp := 0;
            -- 190508 complete if .. end if statement below copied from case when i > A.ordinal-B.ordinal branch as the B.FirstPerPos can start before A.ordinal-B.ordinal
            if i < B.FirstPerPos and B.FirstPerPos /= 0 then
               Btmp := B.vec(i);
            elsif i >= B.FirstPerPos and B.FirstPerPos /= 0 then
               Btmp := B.vec(B.FirstPerPos+t);  -- Adjust periodic
               t := (t + 1) mod BPeriod;
            elsif B.FirstPerPos = 0 then -- B is a positive integer
               if i <= B.LastPerPos then Btmp := B.vec(i); else Btmp := 0; end if;
            end if;

         elsif i > A.ordinal - B.ordinal then -- i within adjusted A indicies
            -- s index for A to cater for the periodic part, t index B to cater for the periodoc part

            if i < B.FirstPerPos and B.FirstPerPos /= 0 then
               Btmp := B.vec(i);
            elsif i >= B.FirstPerPos and B.FirstPerPos /= 0 then
               Btmp := B.vec(B.FirstPerPos+t);  -- Adjust periodic
               t := (t + 1) mod BPeriod;
            elsif B.FirstPerPos = 0 then -- B is a positive integer
               if i <= B.LastPerPos then Btmp := B.vec(i); else Btmp := 0; end if;
            end if;

            if i < A.FirstPerPos+A.ordinal-B.ordinal and A.FirstPerPos /= 0 then
               Atmp := A.vec(i-A.ordinal+B.ordinal);  -- 190429 Corrected. Compare with  Btmp := B.vec(i-B.ordinal+A.ordinal);
            elsif i >= A.FirstPerPos+A.ordinal-B.ordinal and A.FirstPerPos /= 0 then
               Atmp := A.vec(A.FirstPerPos+s);  -- Adjust periodic
               s := (s + 1) mod APeriod;
            elsif A.FirstPerPos = 0 then -- A is a positive integer
               if i <= A.LastPerPos+A.ordinal-B.ordinal then Atmp := A.vec(i-A.ordinal+B.ordinal); else Atmp := 0; end if;
            end if;

         end if; --  if  i <= B.ordinal then
         R.ordinal := B.ordinal;

      elsif B.ordinal = A.ordinal then -- s index for A to cater for the periodic part, t index B to cater for the periodoc part

         -- Put_Line("-- B.ordinal = A.ordinal");
         if i < B.FirstPerPos and B.FirstPerPos /= 0 then
            Btmp := B.vec(i);
         elsif i >= B.FirstPerPos and B.FirstPerPos /= 0 then
            Btmp := B.vec(B.FirstPerPos+t);
            t := (t + 1) mod BPeriod;
         elsif B.FirstPerPos = 0 then -- B is a positive integer
            if i <= B.LastPerPos then Btmp := B.vec(i); else Btmp := 0; end if; -- 190503 Make sure to set Btmp = 0 for i > LastPerPos
         end if;
         if i < A.FirstPerPos and A.FirstPerPos /= 0 then
            Atmp := A.vec(i);
         elsif i >= A.FirstPerPos and A.FirstPerPos /= 0 then
            Atmp := A.vec(A.FirstPerPos+s);
            s := (s + 1) mod Aperiod;
         elsif A.FirstPerPos = 0 then -- A is a positive integer
            if i <= A.LastPerPos then Atmp := A.vec(i); else Atmp := 0; end if; -- 190503 Make sure to set Atmp = 0 for i > LastPerPos
         end if;
         R.ordinal := A.ordinal;
      end if;

      exit when MaxInteger(i) > ExitIndex; -- 190503 Move here from just before end loop. No need to update R.vec(i) for an irrelevant index.

      -- 190507 Need to save index for last carry and use this to understand R.FirstPerPos  !!!!
      if carry > 0 then LastCarryIndex := MaxInteger(i); end if; -- 190507 Need to save index for last carry and use this to understand R.FirstPerPos when A or B is a positive integer

      Rtmp := (Atmp + Btmp + carry) mod MaxABprime; -- 190430 Error !!! Carry must be added before mod. Corrected ! -- 190812 Changed index from i to j to handle leading zeroes
      if i = 1 then R1tmp := Rtmp; end if; -- 190813 Save calculated first pos to start count leading zeroes.
      R.vec(j) := Rtmp;

      carryvec(MaxInteger(i)) := Integer(carry); -- 190509 save for checking carry spillover from non-periodic part to the periodic part.

      Put_line("-- R.vec("&j'image&") := ("&Atmp'Image&" + "&Btmp'Image&" + "&carry'Image&") mod "&MaxABprime'Image&" = "&R.vec(j)'Image&" s(next j) = "&s'image&" t(next j) = "&t'Image,logg);

      carry := (if (Atmp + Btmp + carry) >= MaxABprime then 1 else 0); -- 190430 Carry must be inlcuded here as well

      if R1tmp = 0 and Rtmp = 0 and not LeadingZeroesEnd then -- 190813 Start count leading zeroes
         LeadingZeroes := LeadingZeroes + 1;
      elsif R1tmp = 0 and Rtmp /= 0 and not LeadingZeroesEnd then  -- 190813 Stop count leading zeroes. If all zeroes up to MaxAB then result is [ 0 , 1 ]( 0 , 0 )(p)
         LeadingZeroesEnd := true;
         j := j + 1; -- 190813 only update index for R when leading zeroes ended
      else
         j := j + 1;
      end if;
   end loop;
   -- R parameters must be given value before exit
   -- FirstPerPos, LastPerPos, dot_pos, CompletePeriod => False, NoPeriod, DenFactor, NumFactor
   -- Case 1 or 2 above lead to that the period of A or B is preserved for R and starts after leading zeroes + A or B non-periodic digits + one carry
   -- Cases for the calculation of R.FirstPerPos and LastPerPos when A or B is a positive integer and A.ordinal /= B.ordinal
   -- 190508 Finding facts about incoming A and B.
   -- 1. A and B pos rationals and periodic
   -- 2.a. B pos integer A pos rational and B.LastPerPos <= abs(A.ordinal-B.ordinal)
   -- 2.b. B pos integer A pos rational and B.LastPerPos > abs(A.ordinal-B.ordinal)
   -- 3.a. A pos integer B pos rational and A.LastPerPos <= abs(A.ordinal-B.ordinal)
   -- 3.b. A pos integer B pos rational and A.LastPerPos > abs(A.ordinal-B.ordinal)
   -- 4. A or B pos integer : PerPos Case 1 : abs(A.ordinal-B.ordinal) = ABPeriod
   -- 5. A or B pos integer : PerPos Case 2 : abs(A.ordinal-B.ordinal) > ABPeriod
   -- 6. A or B pos integer : PerPos Case 3 : abs(A.ordinal-B.ordinal) < ABPeriod
   -- 7. A.ordinal < B.ordinal then A leading index
   -- 8. B.ordinal < A.ordinal then B leading index
   -- 9. A.ordinal = B.ordinal
   -- 10. A and B positive integer
   --
   -- 190813 identify if result is (0,1) by using LeadingZeroes
   if LeadingZeroes = MaxAB then
      R.vec(0) := 1;
      R.vec(2) := 0; -- First_per_pos = 0 for positive integer
      R.vec(1) := 0;
      R.dot_pos := 0;
      R.FirstPerPos := 0; -- Indicate no fractional part
      R.LastPerPos := 1;
      R.RecoveryLength := 0; -- Indicate nothing to recover
      R.NoPeriod := true;
      R.value := (0,1);
      return R;
   end if;

   R.LastPos := Integer(ExitIndex);
   -- Put_line("-- Add3 (491) : before adj : R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image);
   -- Put_line("-- Add3 (492) : before adj : A.ordinal = "&A.ordinal'Image&" A.FirstPerPos = "&A.FirstPerPos'Image&" A.LastPerPos = "&A.LastPerPos'Image&" A.LastPos = "&A.LastPos'Image);
   -- Put_line("-- Add3 (492) : before adj : B.ordinal = "&B.ordinal'Image&" B.FirstPerPos = "&B.FirstPerPos'Image&" B.LastPerPos = "&B.LastPerPos'Image&" B.LastPos = "&B.LastPos'Image);
   -- Put_line("-- Add3 (493) : before adj : common_nonperiodic = "&common_nonperiodic'Image);
   R.NoPeriod := (APeriod = 0) and (BPeriod = 0); -- 190502 Obviously no periodic part if both A and B are positive integer: Case 3
   if A.FirstPerPos = 0 and B.FirstPerPos /= 0 then
      Put_Line("-- Add3 : A.FirstPerPos = 0 and B.FirstPerPos /= 0",logg);
      -- Case 1 -- 190503 Introduce identification of Cases: A positive integer
      -- 190525 and B pos/neg rational and negative integer : Case 1, Case 7 and Case 12
      -- 190507 Integer(LastCarryIndex + 1); Seems not to work. Find better condition
      -- R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)));
      -- R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
      -- 190509 Verified in logg190508_9.txt to work with R.FirstPerPos := Integer(MAX(MaxInteger(B.FirstPerPos),LastCarryIndex + 1));
      if LastCarryIndex > 0 then
         Put_Line("-- Add3 : LastCarryIndex > 0",logg);
         R.FirstPerPos := Integer(MAX(MaxInteger(B.FirstPerPos)+ MaxInteger(abs(A.ordinal - B.ordinal)),LastCarryIndex + 1));
         -- 190508 Test new calc of R.FirstPerPos and R.LastPerPos for A or B is a positive integer
         -- 190513 Corrected R.FirstPerPos : Need to consider ordinal difference MaxInteger(abs(A.ordinal - B.ordinal)) in the overlapping case as well
      else
         Put_Line("-- Add3 : R.FirstPerPos := B.FirstPerPos + abs(A.ordinal - B.ordinal)",logg);
         R.FirstPerPos := B.FirstPerPos + abs(A.ordinal - B.ordinal); -- non overlapping positions during addition
      end if;
      R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
   elsif A.FirstPerPos /= 0 and B.FirstPerPos = 0 then
      Put_Line("-- Add3 : A.FirstPerPos /= 0 and B.FirstPerPos = 0",logg);
      -- Case 2 -- 190503 Introduce identification of Cases: B positive integer
      -- 190525 A pos/neg rational and neg integer: Case 2, Case 8 and Case 15
      -- 190507 Integer(LastCarryIndex + 1); Seems not to work. Find better condition
      -- R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)));
      -- R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
      -- 190509 Verified in logg190508_9.txt to work with R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),LastCarryIndex + 1));
      if LastCarryIndex > 0 then
         Put_Line("-- Add3 : LastCarryIndex > 0");
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos) + MaxInteger(abs(A.ordinal - B.ordinal)),LastCarryIndex + 1));
         -- 190508 Test new calc of R.FirstPerPos and R.LastPerPos for A or B is a positive integer
         -- 190413 Corrected R.FirstPerPos : Need to consider ordinal difference MaxInteger(abs(A.ordinal - B.ordinal)) in the overlapping case as well
      else
         Put_Line("-- Add3 : R.FirstPerPos := A.FirstPerPos + abs(A.ordinal - B.ordinal)",logg);
         R.FirstPerPos := A.FirstPerPos + abs(A.ordinal - B.ordinal); -- non overlapping positions during addition
      end if;
      R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
   elsif A.FirstPerPos /= 0 and B.FirstPerPos /= 0 then
      Put_Line("-- Add3 : A.FirstPerPos /= 0 and B.FirstPerPos /= 0",logg);
      -- 190525 : Case 0  : A and B are both pos rationals
      -- 190525 : Case 5  : A neg integer + B pos rational
      -- 190525 : Case 6  : A neg integer + B neg rational
      -- 192505 : Case 9  : A neg rational + B neg integer
      -- 190525 : Case 13 : A pos rational + B neg integer
      -- 190525 : Case 4  : A neg integer + B neg integer
      -- 190610 : Case 10 : A neg rational + B neg rational
      -- if A.LastPerPos = A.FirstPerPos or B.LastPerPos = B.FirstPerPos then -- A or B has period one and does not impact the R.period.
      -- R inherits the period from A or B. R.FirstPerPos starts after leading index A or B LastPerPos potentially adjusted after checking period consitens with proposed starting index
      -- Check carry spillover from non-periodic part.
      -- 190523 Check first that there are common non-periodic positions.
      -- 190610 common_nonperiodic : Boolean := (abs(B.ordinal - A.ordinal) <= (if A.FirstPerPos < B.FirstPerPos then A.FirstPerPos else B.FirstPerPos)) and (A.FirstPerPos /= 1) and (B.FirstPerPos /= 1);
      -- 190525 : Case 5, Case 6, Case 9 and Case 13 when APeriod or Bperiod = 1
      -- 190528 : When (APeriod = 1 and A.LastPerPos < B.FirstPerPos) or (BPeriod = 1 and B.LastPerPos < A.FirstPerPos) then
      -- 190528 : change R.FirstPerPos if Last Period of digits differs from the proposed first period of digits !
      -- 190528 : The initial setting of ExitIndex :
      -- 190528 :   ExitIndex : MaxInteger := abs(MaxInteger(A.ordinal) - MaxInteger(B.ordinal)) + MAX(MaxInteger(A.LastPerPos),MaxInteger(B.LastPerPos)) + 2*ABPerLength + 2;
      -- 190528 :   will allow at least 2 periods of the periodic sequence of R.
      -- 190528 : Find first period to check with last period by finding out how many periods there
      -- 190528 : are up to Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
      -- 190528 : Compare sum of digits in period 1 with last period.
      -- 190624 : Check if R.FirstPerPos + Integer(ABPerLength) - 1) > MaxLen then !!
      -- 190624 : If this is the case then Period_sum will not work !!

      if (((APeriod = 1) and (BPeriod /= 1) and (A.vec(A.FirstPerPos) = A.prime-1)
           and ((A.LastPerPos <= B.FirstPerPos + abs(A.ordinal - B.ordinal)) or (B.FirstPerPos = 1)))
          or ((BPeriod = 1) and (APeriod /= 1) and (B.vec(B.FirstPerPos) = B.prime-1)
              and ((B.LastPerPos <= A.FirstPerPos + abs(A.ordinal - B.ordinal)) or (A.FirstPerPos = 1))))  then
         -- 190526 A or B negative integer : Case 4,5,6 and Case 9,13
         -- 190611 Added condition "or (B.FirstPerPos = 1)" for A neg integer and B pure periodic and
         -- 190611 "or (A.FirstPerPos = 1)" for B neg integer and A pure priodic
         Put_Line("-- Add3 : A or B negative integer : Case 4,5,6 and Case 9,13",logg);
         -- 190610 Check A.vec(FirstPerPos) = prime-1 or B.vec(FirstPerPos) = prime-1 for negative integer !!
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal); -- 190526 Initial setting. Adjustment may occur in leading_zeroes
         Put_Line("-- Add3 : R.FirstPerPos = "&R.FirstPerPos'Image,logg);
         -- 190528 : if Period_Sum(First) /= Period_Sum(Last) then -- Adjust R.FirstPerPos and R.LastPerPos to next period.
         --             R.FirstPerPos := R.FirstPerPos + ABPerlength;
         --          end if;
         Put_Line("-- Add3 : 2*Integer(ABPerLength) + R.FirstPerPos = "&Integer(2*Integer(ABPerLength) + R.FirstPerPos)'Image&" < MaxLen = "&MaxLen'Image,logg);
         if (2*Integer(ABPerLength) + R.FirstPerPos < MaxLen) -- 190716 : Not enough hensel digits to compare 2 periods
         then
            Put_Line("-- Add3 : (2*Integer(ABPerLength) + R.FirstPerPos < MaxLen) = "&Boolean(2*Integer(ABPerLength) + R.FirstPerPos < MaxLen)'Image,logg);
            if (Period_Sum(R,R.FirstPerPos,Integer(ABPerLength)) /= Period_Sum(R,Integer(ExitIndex)-Integer(ABPerlength)-1,Integer(ABPerlength))) then
               -- 190528 handle case when 2 periods does not fit into R.FirstPerPos..MaxLen positions with an extra if condition.
               R.FirstPerPos := R.FirstPerPos + Integer(ABPerLength);
            end if;
         else
            R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
         end if;
         -- 190618 Consider another branch with (APeriod /= 1 and BPeriod /= 1 and Aperiod = BPeriod and A.FirstPerPos = B.FirstPerPos) then ....
      elsif (APeriod /= 1 and BPeriod /= 1 and Aperiod = BPeriod and A.FirstPerPos = B.FirstPerPos) and common_nonperiodic then
         -- 190702 Consider to compare last period with first period when at least 2 periods within ExitIndex
         Put_Line("-- Add3 : (APeriod /= 1 and BPeriod /= 1 and Aperiod = BPeriod and A.FirstPerPos = B.FirstPerPos)",logg);
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
      elsif (APeriod /= 1 and BPeriod /= 1) and common_nonperiodic then -- 190610 simplify and use common_nonperiodic
         Put_line("-- Add3 : (APeriod /= 1 and BPeriod /= 1) and common_nonperiodic",logg);
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
         Put_line("-- Add3 : carry_spill_over(carryvec,"&R.FirstPerPos'Image&") = "&carry_spill_over(carryvec,R.FirstPerPos)'Image,logg);
         R.FirstPerPos := Integer(carry_spill_over(carryvec,R.FirstPerPos));
         Put_Line("-- Add3 : R.FirstPerPos = "&R.FirstPerPos'Image,logg);
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
      elsif (APeriod = 1 and BPeriod = 1) and common_nonperiodic then --190528 Added condition " (APeriod = 1 and BPeriod = 1)"
         Put_Line("-- Add3 : (APeriod = 1 and BPeriod = 1) and common_nonperiodic",logg);
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
         Put_line("-- Add3 : carry_spill_over(carryvec,"&R.FirstPerPos'Image&") = "&carry_spill_over(carryvec,R.FirstPerPos)'Image,logg);
         R.FirstPerPos := Integer(carry_spill_over(carryvec,R.FirstPerPos));
         Put_Line("-- Add3 : R.FirstPerPos = "&R.FirstPerPos'Image,logg);
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
      elsif (APeriod = BPeriod) and not common_nonperiodic then -- 190805 Added branch '(APeriod = BPeriod) and not common_nonperiodic'
         Put_line("-- Add3 : (APeriod = BPeriod) and not common_nonperiodic",logg);
         R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
      elsif not common_nonperiodic then
         Put_Line("-- Add3 : not common_nonperiodic",logg);
         R.FirstPerPos := Integer(ExitIndex - 1) - Integer(ABPerLength*floor((float(ExitIndex - 2)/float(ABPerLength)))) + abs(A.ordinal - B.ordinal);
         -- 190805 Adding '+ abs(A.ordinal - B.ordinal)' to R.FirstPerPos. Test !!!
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
         Put_line("-- Add3 : R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image,logg);
      end if;

      -- 190805 Does this really work ???
      if (R.RecoveryLength > R.FirstPerPos + Integer(ABPerLength) - 1) and R.RecoveryLength <= Integer(ExitIndex) then
         Put_Line("-- Add3 : (R.RecoveryLength > R.FirstPerPos + Integer(ABPerLength) - 1) and R.RecoveryLength <= Integer(ExitIndex)",logg);
         -- 190526 Adjust for situation when R.RecoveryLength > R.FirstPerPos + Integer(ABPerLength) - 1
         R.LastPerPos := R.RecoveryLength; R.NoPeriod := True; -- 190618 Indicate that the full period can not be used for decoding.
         R.FirstPerPos := R.LastPerPos - Integer(ABPerLength) + 1;
      else
         R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
      end if;
      -- 190507 Should be using R.FirstPerPos + LCM(APeriod,BPeriod)
      -- 190525 Make sure that R.LastPerPos is at least R.RecoveryLength
   elsif R.NoPeriod then  -- Case 3 both A and B are postive integer
      Put_Line("-- Add3 : R.NoPeriod -- Case 3 both A and B are postive integer",logg);
      -- 190509 : Check last carry index to make sure that it's included in the [0..LastPerPos] as
      -- LastPerPos needs to be adjusted to include the last digit.
      R.FirstPerPos := 0;
      R.LastPerPos := Integer(MAX(MAX(MaxInteger(A.LastPerPos),MaxInteger(B.LastPerPos))+1,LastCarryIndex + 1));
   end if;


   Put_line("-- Add3 (634) : R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image,logg);
   -- R.dot_pos := R.ordinal; -- 190430 Error should not be setting dot_pos to ordinal here !!
   Put_line("-- Add3 : R.dot_pos = "&R.dot_pos'Image&" ExitIndex = "&ExitIndex'Image&" NoPeriod = "&R.NoPeriod'Image&" LastCarryIndex = "&LastCarryIndex'Image&" R.ordinal = "&R.ordinal'Image,logg);
   Put_line("-- Add3 : carry_spill_over(carryvec,"&R.FirstPerPos'Image&") = "&carry_spill_over(carryvec,R.FirstPerPos)'Image,logg);
   Put("-- Add3 : before adjustment of leading/trailing zeroes and period one : ",logg); Put_Hensel_code(R,logg); Put_line(" ",logg);

   if  trailing_zeroes(z,R,Integer(ExitIndex)) then -- result is a positive integer if trailing zeroes for all ABLastPerPos - ABFirstPerPos
      Put_line("-- Add3 : trailing_zeroes = "&z'Image,logg);
      R.LastPerPos := Integer(ExitIndex) - z;
      R.FirstPerPos := 0; -- indicate a postitive integer.
      R.NoPeriod := True;

      -- 190425 if LastPerPos - FirstPerPos = 1 and vec(LastPerPos) = vec(FirstPerPos) then adjust to period 1 and and set LastPerPos = FirstPerPos.
      -- elsif R.LastPerPos - R.FirstPerPos = 1 and R.vec(R.LastPerPos) = R.vec(R.FirstPerPos) then
      -- 190430 Check this. count trailing digits and compare with ABPerLength.
      -- R.LastPerPos := R.FirstPerPos; R.vec(0) :=  MaxInteger(R.LastPerPos); R.vec(R.LastPerPos+1) := MaxInteger(R.FirstPerPos);

   elsif period_one(z,R,Integer(ExitIndex)) then -- adjust to period 1 and and set LastPerPos = FirstPerPos.
      Put_Line("-- Add3 : period_one z = "&z'Image,logg);
      R.FirstPerPos := Integer(ExitIndex) - z + 1;
      R.LastPerPos := R.FirstPerPos;
      Put_Line("-- Add3 : period_one adjusted R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image,logg);
      R.NoPeriod := False;
   end if;

   -- 190425 remove leading zeroes and adjust ordinal  !!!
   if LeadingZeroes > 0 then -- normalize hensel vector to a unit.
      Put_line("-- Add3 : leading_zeroes = "&LeadingZeroes'Image,logg);
      z := LeadingZeroes;
      -- shift_vector_left(z,R); -- 190812 Not needed as index is not updated when leading zeroes.
      Put("-- Add3 : Hensel coded with no leading zeroes : ",logg); Put_Hensel_code(R,logg); Put_line(" ",logg);

      if not R.NoPeriod and A.FirstPerPos /= 0 and B.FirstPerPos /= 0 then -- 190503 Moved out of shift_vector. 190505 Added "and A.FristPerPos /= 0 and B.FirstPerPos /= 0"
         Put_line("-- Add3 : not R.NoPeriod and A.FirstPerPos /= 0 and B.FirstPerPos /= 0",logg);
         if R.LastPerPos = R.FirstPerPos then -- add of rationals A and B results with period 1 and leading zeroes
            Put_line("-- Add3 : R.LastPerPos = R.FirstPerPos ",logg);
            R.FirstPerPos := R.FirstPerPos - z;
            R.LastPerPos := R.FirstPerPos;
         elsif A.ordinal = B.ordinal and A.FirstPerPos = B.FirstPerPos and A.LastPerPos = B.LastPerPos and R.LastPerPos + z < R.size and z+1 < A.FirstPerPos then
            -- 190610 Added "and R.LastPerPos + z < R.size"
            Put_Line("-- Add3 : A.ordinal = B.ordinal and A.FirstPerPos = B.FirstPerPos and A.LastPerPos = B.LastPerPos and R.LastPerPos + z < R.size",logg);
            R.FirstPerPos := R.FirstPerPos + z;
            R.LastPerPos := R.LastPerPos + z;
            Put_Line("-- Add3 (675): R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image&" R.size = "&R.size'Image,logg);
         elsif A.ordinal = B.ordinal and A.FirstPerPos = B.FirstPerPos and z+1 >= A.FirstPerPos then -- 190613 Test z+1 >= A.FirstPerPos
            Put_Line("-- Add3 : A.ordinal = B.ordinal and A.FirstPerPos = B.FirstPerPos and z+1 >= A.FirstPerPos",logg);
            -- 190527 Set R.FirstPerPos and R.LastPerPos back to default : R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
            R.FirstPerPos := Integer(MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))) + abs(A.ordinal - B.ordinal);
            R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
            Put_Line("-- Add3 : (681) R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image&" R.size = "&R.size'Image,logg);
         end if;
         R.ordinal := R.ordinal + z;
         -- R.LastPos := R.LastPerPos; -- 190807 Why this ???
         R.dot_pos := R.ordinal;
      elsif A.FirstPerPos = 0 and B.FirstPerPos = 0 then -- Case 3 both A and B are postive integer
         Put_Line("-- Add3 : Case 3 both A and B are postive integer : A.FirstPerPos = 0 and B.FirstPerPos = 0",logg);
         R.ordinal := R.ordinal + z;
         -- R.LastPos := R.LastPerPos; -- 190807 Why this ???
         R.dot_pos := R.ordinal;
      elsif A.FirstPerPos /= 0 or B.FirstPerPos /= 0 then -- 190507 Added case when A or B is a rational, and
         R.ordinal := R.ordinal + z;
         Put_Line("-- Add3 : A.FirstPerPos /= 0 or B.FirstPerPos /= 0",logg);
         if ABPerLength = 1  and R.FirstPerPos - z > 0 then
            Put_Line("-- Add3 : ABPerLength = 1  and R.FirstPerPos - z > 0",logg);
            -- 190508 valid for A or B with period 1 and B or A is a pos integer and leading zeroes identified.
            -- 190514 Added condition and R.FirstPerPos - z > 0 as FirstPerPos must be >= 1.
            R.FirstPerPos := R.FirstPerPos - z;
            R.LastPerPos := R.FirstPerPos;
         elsif APeriod = BPeriod and A.FirstPerPos = B.FirstPerPos and z > A.FirstPerPos then
            Put_Line("-- Add3 : APeriod = BPeriod and A.FirstPerPos = B.FirstPerPos and z > A.FirstPerPos",logg);
            -- 190628 Check if nr of leading zeroes is larger than A.FirstPerPos = B.FirstPerPos
            -- 190628 if so there can't be any carry leakage into ABperiod after normalization.
            -- 190628 Keep the FirstPerPos and LastPerPos from A and/or B
            R.FirstPerPos := A.FirstPerPos;
            R.LastPerPos := R.FirstPerPos + Integer(ABPerLength) - 1;
         elsif APeriod = 1 or BPeriod = 1 then -- 190526 Need to identify when A or B is a negative integer, then the periodic sequence is inhereted from B or A.
            Put_Line("-- Add3 : APeriod = 1 or BPeriod = 1",logg);
            R.FirstPerPos := R.FirstPerPos - z;
            R.LastPerPos := R.LastPerPos - z;
         end if;
         -- R.LastPos := R.LastPerPos; -- 190807 Why this ???
         R.dot_pos := R.ordinal;
      else
         Put_line("-- Add3 : leading_zeroes : Failure : this branch should never happen ???",True);
         -- R.LastPos := R.LastPerPos; -- 190807 Why this ???
         R.dot_pos := R.ordinal;
      end if;
      Put_Line("-- Add3 : (719) R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image&" R.size = "&R.size'Image,logg);
   elsif A.FirstPerPos = 1 and B.FirstPerPos = 1 and ExitIndex - 1 - ABPerLength*Floor(float(ExitIndex - 2)/float(ABPerLength)) > 0 then
      -- 190610 Added condition for adjustment of R.FirstPerPos and R.LastPerPos when no leading zeroes
      -- 190610 Check R.FirstPerPos and R.LastPerPos consistency and calculate from ExitIndex backwards.
      Put_line("-- Add3 : A.FirstPerPos = 1 and B.FirstPerPos = 1 and ExitIndex - 1 - ABPerLength*Floor(float(ExitIndex - 2)/float(ABPerLength)) > 0",logg);
      if ExitIndex - 1 - ABPerLength*floor((float(ExitIndex - 2)/float(ABPerLength))) /= MaxInteger(R.FirstPerPos)  then
         Put_Line("-- Add3 : ExitIndex - 1 - ABPerLength*floor((float(ExitIndex - 2)/float(ABPerLength))) /= MaxInteger(R.FirstPerPos)",logg);
         R.FirstPerPos := Integer(ExitIndex - 1) - Integer(ABPerLength*floor((float(ExitIndex - 2)/float(ABPerLength)))) + abs(A.ordinal - B.ordinal); -- 190805 Added '+ abs(A.ordinal - B.ordinal)'
         -- R.LastPerPos := R.FirstPerPos + Integer(ABPerLength - 1);
         -- Put("-- Add result : A.FirstPerPos = 1 or B.FirstPerPos = 1 : ",True); Put_Hensel_code(R,True); new_line;
      end if;
   end if;

   if ABPerLength = 1 and R.vec(R.FirstPerPos) = R.prime - 1 then -- 190528 Indicate result being a negative integer.
      R.dot_pos := R.LastPerPos - 1;
   end if;

   Put("-- Add3 : result before validation: ",logg); Put_Hensel_code(R,logg); Put_Line(" = "&Image(Decode4(R,False)),logg);
   -- 190807 Explore to do decode using fixed size HenselCode and then encode again to
   -- find the FirstPerPos and the LastPerPos. Introduce boolean function ValidateResult(H).
   if ValidateResult(R,logg) then
       Put_Line("-- Add3 : R.MaxLen = "&R.MaxLen'Image,logg);  -- 190512 : Put_Lines added to check exit values
       Put_Line("-- Add3 : MaxAB = "&MaxAB'Image&" ExitIndex = "&ExitIndex'Image&" ABPerLength = "&ABPerLength'Image,logg);
       Put_Line("-- Add3 : R.DenFactor = "&R.DenFactor'Image,logg);
       Put_Line("-- Add3 : R.NumFactor = "&R.NumFactor'Image,logg);
       Put_line("-- Add3 : R.RecoveryLength = "&R.RecoveryLength'Image,logg);
       Put_line("-- Add3 : R.size = "&R.size'Image&" R.ordinal = "&R.ordinal'Image,logg);
       Put_line("-- Add3 : R.dot_pos = "&R.dot_pos'Image,logg);
       Put("-- Add3 result : ",True); Put_Hensel_code(R,True); Put(" = "&Image(R.value),True);
       return R;
   else raise Addition_Validation_error;
   end if;

   -- Put_Line("-- Add3 : R.MaxLen = "&R.MaxLen'Image);  -- 190512 : Put_Lines added to check exit values
   -- Put_Line("-- Add3 : MaxAB = "&MaxAB'Image&" ExitIndex = "&ExitIndex'Image&" ABPerLength = "&ABPerLength'Image);
   -- Put_Line("-- Add3 : R.DenFactor = "&R.DenFactor'Image);
   -- Put_Line("-- Add3 : R.NumFactor = "&R.NumFactor'Image);
   -- Put_line("-- Add3 : R.RecoveryLength = "&R.RecoveryLength'Image);
   -- Put_line("-- Add3 : R.size = "&R.size'Image&" R.ordinal = "&R.ordinal'Image);
   -- Put_line("-- Add3 : R.dot_pos = "&R.dot_pos'Image);
   -- Put("-- Add3 result : ",True); Put_Hensel_code(R,True); New_line;

   -- return R;

exception

   when undefined =>
      Put_line("-- Add3 : undefined : Addition undefined for Henselcodes A and B with different primes");
      return R;

   when Constraint_Error =>  -- 190610 Added to check reason for Constraint Error !
      Put_line("-- Add3 : Constraint_Error: R.ordinal = "&R.ordinal'Image&" R.FirstPerPos = "&R.FirstPerPos'Image&" R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image);
      return R;

   when Assertion_Error =>
      Put_Line("-- Add3 : Assertion_Error");
      return R;

   when Addition_Validation_Error =>
      Put_Line("-- Add3 : Addition_Validation_Error");
      return R;

end add3;
