with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
With Ada.Numerics.Elementary_Functions;
with Prime_Numbers;
with padic_numbers;
with System; use System;
package body padic_numbers is
   use Ada.Numerics;
   use Ada.Numerics.Elementary_Functions;
   use Integer_Rationals;
   use Supporting_Functions;
   use Integer_Prime;
   use padic_numbers;

   function floor(A : in Float) return integer is
   begin
      return Integer(Float'Floor(A));
   end floor;

   function floor(A : in Float) return MaxInteger is
   begin
      return MaxInteger(Float'Floor(A));
   end floor;

   function Ceiling(A : in Float) return Integer is
   begin
      return Integer(Float'Ceiling(a));
   end Ceiling;

   function Ceiling(A : in Float) return MaxInteger is
   begin
      return MaxInteger(Float'Ceiling(A));
   end Ceiling;

   procedure Put_Line(Item : String; logg : boolean) is
   begin
      if logg then Put_line(Item);
      end if;
   end Put_line;

   procedure Put(Item : String; logg : boolean) is
   begin
      if logg then Put(Item);
      end if;
   end Put;

   procedure Put_Hensel_vector(vec : in Henselvec; dot_pos : in Integer; logg : Boolean) is
      FirstPerpos : Integer := Integer(vec(Integer(vec(0))+1));
      LastPerpos : Integer := Integer(vec(0));

   begin
      Put("-- Put_Hensel_vector : Dot pos = "& dot_pos'Image&" FirstPerPos ="
          &FirstPerPos'Image&" LastPerPos = "&LastPerPos'Image&" Hensel vector = ",logg);

     if (FirstPerPos = LastPerPos) and (FirstPerPos = 1) then
        put("(",logg);Put(vec(1)'Image,logg); put(","&dot_pos'image&")",logg);
     else
        put("(",logg);
        for i in 1..LastPerPos loop
           Put(vec(i)'Image,logg);
        end loop;
        Put(",",logg);Put(dot_pos'Image,logg); Put(")",logg);
     end if;
   end Put_Hensel_vector;

   procedure Put_Hensel_code(HenselCodeRecord : in HenselCode; logg : Boolean) is
      LastPos : Integer := (if HenselCodeRecord.LastPerPos > HenselCodeRecord.MaxLen then HenselCodeRecord.MaxLen else HenselCodeRecord.LastPerPos);
   begin
     -- Put("-- Put_Hensel_code : Dot pos = "&HenselCodeRecord.dot_pos'Image,logg);
     -- Put(" HenselCodeRecord.ordinal = "&HenselCodeRecord.ordinal'Image,logg);
     -- Put(" First periodic pos ="&HenselCodeRecord.FirstPerPos'Image,logg);
     -- Put(" Last_pos = "&HenselCodeRecord.LastPerPos'Image,logg);

      Put("["&HenselCodeRecord.FirstPerPos'Image&" ,"&HenselCodeRecord.LastPerPos'Image&" ]",logg);
      if (HenselCodeRecord.FirstPerPos = HenselCodeRecord.LastPerPos) and (HenselCodeRecord.FirstPerPos = 1) then -- one Hensel digit and period 1
         Put("(",logg);Put(HenselCodeRecord.vec(1)'Image,logg);
         Put(" , "&HenselCodeRecord.ordinal'image&" )",logg);
         Put(")("&HenselCodeRecord.prime'Image&" )",logg);
      elsif HenselCodeRecord.LastPerPos >= 1 and HenselCodeRecord.FirstPerPos > 0 then -- periodic fraction and negative integers
         put("(",logg);
         for i in 1..LastPos loop -- 190509 limit to LastPos as the calculated LastPerPos can be larger
            Put(HenselCodeRecord.vec(i)'Image,logg);
         end loop;
         Put(" , ",logg);Put(HenselCodeRecord.ordinal'Image,logg); Put(" )("&HenselCodeRecord.prime'Image&" )",logg);
      elsif HenselCodeRecord.NoPeriod then -- positive integer added 190429
         put("(",logg);
         for i in 1..LastPos loop -- 190509 limit to LastPos as the calculated LastPerPos can be larger
            Put(HenselCodeRecord.vec(i)'Image,logg);
         end loop;
         Put(" , ",logg);Put(HenselCodeRecord.ordinal'Image,logg); Put(" )("&HenselCodeRecord.prime'Image&" )",logg);
      end if;

   end Put_Hensel_code;

   procedure Put_Hensel_Code(HenselCodeRecordPtr : in HenselCodePtr; logg : Boolean) is
         LastPos : Integer := (if HenselCodeRecordPtr.LastPerPos > HenselCodeRecordPtr.MaxLen then HenselCodeRecordPtr.MaxLen else HenselCodeRecordPtr.LastPerPos);
   begin
     -- Put("-- Put_Hensel_code : Dot pos = "&HenselCodeRecord.dot_pos'Image,logg);
     -- Put(" HenselCodeRecord.ordinal = "&HenselCodeRecord.ordinal'Image,logg);
     -- Put(" First periodic pos ="&HenselCodeRecord.FirstPerPos'Image,logg);
     -- Put(" Last_pos = "&HenselCodeRecord.LastPerPos'Image,logg);

      Put("["&HenselCodeRecordPtr.FirstPerPos'Image&" ,"&HenselCodeRecordPtr.LastPerPos'Image&" ]",logg);
      if (HenselCodeRecordPtr.FirstPerPos = HenselCodeRecordPtr.LastPerPos) and (HenselCodeRecordPtr.FirstPerPos = 1) then -- one Hensel digit and period 1
         Put("(",logg);Put(HenselCodeRecordPtr.vec(1)'Image,logg);
         Put(" , "&HenselCodeRecordPtr.ordinal'image&" )",logg);
         Put(")("&HenselCodeRecordPtr.prime'Image&" )",logg);
      elsif HenselCodeRecordPtr.LastPerPos >= 1 and HenselCodeRecordPtr.FirstPerPos > 0 then -- periodic fraction and negative integers
         put("(",logg);
         for i in 1..LastPos loop -- 190509 limit to LastPos as the calculated LastPerPos can be larger
            Put(HenselCodeRecordPtr.vec(i)'Image,logg);
         end loop;
         Put(" , ",logg);Put(HenselCodeRecordPtr.ordinal'Image,logg); Put(" )("&HenselCodeRecordPtr.prime'Image&" )",logg);
      elsif HenselCodeRecordPtr.NoPeriod then -- positive integer added 190429
         put("(",logg);
         for i in 1..LastPos loop -- 190509 limit to LastPos as the calculated LastPerPos can be larger
            Put(HenselCodeRecordPtr.vec(i)'Image,logg);
         end loop;
         Put(" , ",logg);Put(HenselCodeRecordPtr.ordinal'Image,logg); Put(" )("&HenselCodeRecordPtr.prime'Image&" )",logg);
      end if;

   end Put_Hensel_code;


   -- 190708 Modify to support fixed length Hensel code with a new procedure
   -- 190710 Change format so that all numbers have a leading space and a trailing space.  Change < p > to ( p ) to indicate prime number.
   procedure Put_Fixed_Hensel_code(HenselCodeRecord : in HenselCode; Length : in Integer; logg : Boolean) is
      LastPos : Integer := (if Length > HenselCodeRecord.vec'Size - 2 then HenselCodeRecord.Size - 2 else Length); -- 190810 Limit length.
   begin
      pragma Suppress_All;
      Put("["&Length'Image&" ]",logg); Put("(",logg);
      for i in 1..LastPos loop -- 190509 limit to LastPos as the calculated LastPerPos can be larger
         Put(HenselCodeRecord.vec(i)'Image,logg); -- 190810 raised CONSTRAINT_ERROR : padic_numbers.adb:103 index check failed. Corrected using pragma Suppress_All above.
      end loop;
      Put(" , ",logg);Put(HenselCodeRecord.ordinal'Image,logg); Put(" )("&HenselCodeRecord.prime'Image&" )",logg);

   end Put_Fixed_Hensel_code;


   function padic_ordinal_integer(x : in MaxInteger; prime : in MaxInteger) return Integer is
      k : Integer := 0;
      undefined : exception;
      Log_prime : Float := log(Float(prime));
   begin
      -- Put_Line("-- padic_ordinal_integer prime**k/x where Log_prime = "&Log_prime'Image);

      if x = 0 then raise undefined;
      end if;

      while x mod prime**k = 0 loop  -- 190423 Corrected condition to : x mod prime**k = 0
         -- Put_Line("-- padic_ordinal_integer prime**k/x where k = "& k'Image);
         k := k + 1;
      end loop;

      return k;

      exception When undefined => Put_line("-- Padic ordinal undefined for integer = "& x'Image);
         return Integer'Last;

   end padic_ordinal_integer;

   function padic_ordinal_rational(r : in Rational; prime : in MaxInteger) return Integer is
      undefined : exception;
      v : Constant Integer := padic_ordinal_integer(Numerator(r),prime) - padic_ordinal_integer(Denominator(r), prime);
      -- 190423 Introduced an integer constant to store result.
   begin

      -- Put_Line("-- padic_ordinal_rational for "&Numerator(r)'Image&"/"&Denominator(r)'Image&" = "&v'Image);

      if Numerator(r) = 0 then
         raise undefined;
      end if;

      return v;

      exception when undefined => Put_line("-- Padic ordinal undefined for rational = "&Image(r));
         return Integer'Last;

   end padic_ordinal_rational;

   function absprime(r : in Rational; prime : MaxInteger) return Rational is
      v : constant Integer := (if Numerator(r) /= 0 then padic_ordinal_rational(r,prime) else Integer'Last);
         -- 190423 Corrected to handle negative values.
         -- 190630 Not defined for 0, use MaxInteger'Last to indicate
   begin
      if v /= Integer'Last then
               if v >= 0 then
                  return (1,prime**v);
               else
                  return (prime**(abs(v)),1);
               end if;
      else return (MaxInteger'Last, 1);
      end if;
   end absprime;

   function decode2(h : in HenselCode; n0 : in Integer; logg : Boolean) return Rational is
      -- n is the length to decode. k digits used for verificaton. Compare Decode(a,b,r) = Decode(a,b,r+k).
      -- According to A method for Hensel Code Overflow Detection by Xin Kau Li, Chao Lu Towson University and Jon A Sjögren, Air Force Office of scientific Research
      -- if the length r-k > 4 and the numbers |a,b| <= sqrt((p**(r-k)-1)/2) where a is the Numerator and b is the denominator, then the decode algorithm give correct answers.
      -- http://www.jon-arny.com/httpdocs/Works/AMethodforHenselCodesOverflowDetection_Final.docx
      i : Integer := 0;
      hvec : Henselvec := h.vec;
      Num, Den : MaxInteger := 0;
      Last_periodic_pos : Integer := h.LastPerPos;
      First_periodic_pos : Integer := h.FirstPerPos; -- and also last position of the hensel array.
      prime : MaxInteger := h.prime;
      dot_pos : Integer := h.dot_pos;
      x0,tmp : Rational := (0,1);
      n : Integer := (if n0 < 5 then 2*n0 else n0); -- us at least 2 times n0 hensel digits to decode if less than 5.

      function sumx(h : in HenselCode; n : in Integer) return MaxInteger is  -- 190412 Create a continued fraction version that delivers more than 20 numbers precision.
         -- 190408: Check missing dotpos in sumx calc
         x : MaxInteger := 0;
         k : Integer := 0;
         j : Integer := 1;
         hvec : Henselvec:= h.vec;
         LastPosPeriodic : Integer := h.LastPerPos;
         FirstPosPeriodic : Integer := h.FirstPerPos;
         prime : MaxInteger := h.prime;
         r : Integer := (if n > h.MaxLen then h.MaxLen else n); -- 190513 : limit r to MaxLen-1 to avoid Constraint Error when calculating MaxInteger(hvec(j)*MaxInteger(prime)**(j-1));
         period : Integer := LastPosPeriodic - FirstPosPeriodic + 1;
      begin
         -- idea compare with: Integer(exp(Float(Float(FirstPosPeriodic+k-1)*Float(log(float(prime),Float(prime)))))) = prime**(FirstPosPeriodic+k-1)
         Put_line("-- Decode2 : sumx : r = "&r'Image,logg);
         while j in 1..r loop
            if j < FirstPosPeriodic then
               x := x + MaxInteger(hvec(j)*MaxInteger(prime)**(j-1));  -- potential overflow when x > 2^64-1  !!!! improvement with exception declaration !?
               j := j + 1;
            elsif j >= FirstPosPeriodic then
               x := x + MaxInteger(hvec(FirstPosPeriodic+k)*MaxInteger(prime**(j-1)));  -- potential overflow when x > 2^64-1  !!!! improvement with exception declaration !?
               k := (k + 1) mod period;
               j := j + 1;
            end if;
         end loop;
         Put_line("-- Decode2 : sumx : x = "&x'Image,logg);
         return x;

      exception when Constraint_Error =>
         Put_line("-- Decode2 : Constraint_Error in Padic_numbers.sumx = "&x'Image,logg);
         return x;

      end sumx;

      r : Integer := (if n > h.MaxLen then h.MaxLen else n);   -- 190513 : limit r to MaxLen to avoid Constraint Error when defining u : euclid_array := (MaxInteger(prime**(r)), x, others => 0);
      x : MaxInteger := sumx(h,r);
      MaxLen : Integer := h.MaxLen;  -- 190513 reuse already calculated MaxLen.


      type euclid_array is array(-1..MaxLen+5) of MaxInteger;  -- r must at least be covering index [-1,5] !!! Check

      u : euclid_array := (MaxInteger(prime**(r)), x, others => 0);  -- prime**(r) <= max_int leading to error if not r <= log(max_int)/log(prime).
      v : euclid_array := (0, 1, others => 0);
      q : euclid_array := (others => 0);
      m : MaxInteger := MaxInteger(float(float(prime)**Float(Float(r)/2.0))); -- exit critera for while loop

   begin
      -- Put_line("-- ..... Start Decode2 ....",logg);

      Put("-- Decode2 : r = "&r'Image,logg);Put_line(" x = " & x'Image,logg);
      Put("-- Decode2 : u(-1) = "& u(-1)'Image,logg); Put_line(" v(-1) = " &v(-1)'Image,logg);
      Put_line("-- Decode2 : Decoding length r = "&r'Image&" Must be <= "&MaxLen'Image,logg);

      -- dixon
      while abs(v(i)) < m and u(i) /=0 loop  -- avoid division with 0 with u(i) /=0, assume dot_pos = 0, Check 190404. Use abs(v(i)) !
        q(i) := floor(Float(Float(u(i-1))/float(u(i)))); Put("-- Decode2 : q("& i'Image &") = "& q(i)'Image,logg);
        u(i+1) := u(i-1) - q(i)*u(i); Put(" u(next i) = "& u(i+1)'Image,logg);
        v(i+1) := v(i-1) + q(i)*v(i); Put(" v(next i) = "& v(i+1)'Image,logg);
        i := i + 1; Put_Line("",logg);
      end loop;

      Num := MaxInteger(MaxInteger((-1)**(i-1))*u(i-1)); Put_line("-- Decode2 : Num = "& Num'Image,logg);
      Den := v(i-1); Put_line("-- Decode2 : Den = "& Den'Image,logg);

      return Reduction((Num,Den));

   exception when Constraint_Error =>
         Put_line("-- Decode2 : Constraint_Error",logg);
         return (0,1);

   end decode2;

   function decode4(h : in HenselCode; logg : Boolean) return Rational is
      i,j,t : Integer := 0;
      x0,x1,x2,x3,x4 : Rational := (0,1);
      hvec : Henselvec := h.vec;
      prime : MaxInteger := h.prime;
      r, tmp : Rational := (0,1);
      RecoveryLength : Integer := (if h.RecoveryLength+1 > h.MaxLen then h.MaxLen-8 else h.RecoveryLength+1); -- 190813 MaxLen/2 is too low value for Sub :  358/ 455 -  358/ 451 = -1432/ 205205. Use MaxLen-8 !
      -- 190614 Changed to if h.RecoveryLength+1 > h.MaxLen then Floor(Float(h.MaxLen/2)) ...
      -- 190513 : Better critera ? -- 190528 "+1"
      -- 190611 : Added "+1" in "if h.RecoveryLength+1 > h.MaxLen ..."

      LastPosPeriodic : Integer := (if h.LastPerPos > h.MaxLen then h.Maxlen else h.LastPerPos);
      -- 190512 : LastPosPeriodic needs to used with care in for loops. Try to erplace with h.Maxlen and verify

      FirstPosPeriodic : Integer := h.FirstPerPos;
      dot_pos : Integer := h.dot_pos;
      s : Integer:= FirstPosPeriodic - 1 - dot_pos; -- nr of non-periodic integers at dot_pos and onwards. Note when dot_pos negative it adds to s (the number of non-periodic numbers)
      period : Integer := LastPosPeriodic - FirstPosPeriodic + 1; -- nr of periodic integers. Must always be one or more.
      MaxLen : Integer := h.MaxLen;
      ordinal : Integer := h.ordinal;
      NumFactor, DenFactor : MaxInteger := 1; -- 190430 : Corrected to use the Factors according to the ordinal set in HenselCode record
      converged : Boolean := false; -- 100630 New criteria for Decode4 : Case 2,3 and 4 case.

   begin -- Decode4
      Put_line(" ",logg);
      Put("-- Decode4 : Encoded P-adic sequence input: ",logg); Put_Hensel_code(h,logg); Put_Line(" ",logg);

      Put("-- Decode4 : FirstPosPeriodic = "& FirstPosPeriodic'Image,logg); Put_line(" LastPosPeriodic = "& LastPosPeriodic'Image,logg);
      Put_Line("-- Decode4 : h.Num_factor = "&h.NumFactor'Image&" h.Den_factor = "&h.DenFactor'Image,logg);
      Put_line("-- Decode4 : h.First_per_pos = "&h.FirstPerPos'Image&" h.Last_per_pos = "&h.LastPerPos'Image,logg);
      Put_line("-- Decode4 : h.dot_pos = "&h.dot_pos'Image&" s = "&s'Image,logg);
      Put_line("-- Decode4 : MaxLen = h.MaxLen = "&MaxLen'Image&" RecoveryLength = "&RecoveryLength'Image,logg);
      Put_line("-- Decode4 : h.ordinal = "&h.ordinal'Image,logg);

      if h.ordinal < 0 then DenFactor := h.prime**(abs(h.ordinal)); -- 190430 : Corrected to use the Factors according to the ordinal set in HenselCode record
      elsif h.ordinal > 0 then NumFactor := h.prime**h.ordinal;
      end if;

      -- Encoding cases as preparation for decoding.
      -- Case 0 : r is a positive integer. The padic expansion is the reverse base p expansion.
      --          Decoding: FirstPosPeriodic = 0 and HenselCode.NoPeriodic is true. Direct decoding by power method.
      -- Case 1 : r is a negative integer. There is a j >= 1 such that R < p**(j), then p**(j) - R - p**(j) can be expressed as follows.
      --          p**(j) - R is an integer in {1, ..., p**(j) - 1} and we can write it in base p as c0 +...+ c(j-1)*p**(j-1) then
      --          for j > log(R)/log(p), alfa = sum(i=0,j-1,c(i)*p**(i)) + sum(i>=j,(prime-1)*p**(i). Leading to a periodic part with period 1 and digit p - 1.
      --          190329: Make sure that the periodic part has the proper position at encoding of negative integer.
      --          Ex is -77 => ( 1 1 2, 2) correct ? Check j = 4 then the 2 should be on pos 4 !!!
      --          We need an adjustment of the hensel code from ( 1 1 2, 2) to ( 0 0 1 1 2, 4) Check -77 = 0*3**3+0*3**2+1*3**1+1*3**0 + 2*(3**4)/(1-3**1) = 4 - 81 = -77 Correct
      --          Decoding: Dot_pos = LastPosPeriodic and period = 1 with last hensel digit equal to p-1. Use direct decoding
      -- Case 2 : r is a rational with r in [-1,0) and GCD(ain,p) = 1, GCD(bin,p) = 1, GCD(ain,bin) = 1 then p-adic expansion is purely periodic.
      --          Decoding: FirstPosPeriodic = 1 and LastPosPeriodic > 1. Dot_pos = 0 and s = 0, where s is the nr of non-periodic positions, i.e s = LastPosPeriodic - FirstPosPeriodoc.
      --          Use Dixon method/algoritm for decoding.
      -- Case 3 : r is a rational with GCD(ain,p) > 1 then write r = p**(n)*u. Then u = r/p**(n) is rational.
      --          Use Case 2 expansion for u and right shift n steps.
      --          Decoding: Decode according to Case 2 with Dixon algoritm
      -- Case 4 : r is rational with r.Denominator /= 1 and r < -1. The number r lies strictly between two negative integers -(M+1)<r<(-M)
      --          so - 1 < r + M < 0. Since the rational r added with an integer M is a rational, Case 3 apply.
      --          Let j be the smallest choice fitting the inequality a(0)+...a(j-1)*p**(j-1) > M.
      --          Then a(0)+...a(j-1)*p**(j-1) - M is a positive integer that is less than p**j - 1 so we can write the difference in base p as
      --          a(0)+...a(j-1)*p**(j-1) - M = a(0)'+...a(j-1)'*p**(j-1) with 0 <= a(i)' <= p-1 and Case 0 applies.
      --          Then the two parts together becomes r = a(0)'+...a(j-1)'*p**(j-1) + sum(i>=j, a(i)*p**(i)).
      --          Decoding : Use Dixon method/algoritm for decoding.
      -- Case 5 : r is rational with r.Denominator /= 1 and r < 0. Since p**(K)*r can be adjusted to be an integer by a large K
      --          we can use Case 1 and then divide by p**(K). Shift dot position K steps to the left.
      --          Decoding: Use Dixon method/algoritm for decoding.
      -- Reference: the p-adic expantion of rational numbers https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf
      -- p**2 < MaxInt and r < MaxInt+2-prime use dixon algoritm
      --


      if FirstPosPeriodic = 0 then -- Case 0 : positive Integer
         Put_line("-- Decode4 : Case 0 : positive Integer ",logg);
         tmp.Denominator := 1;

         if RecoveryLength = 0 then  -- catch the special case of (1,1)
             r := (1,1);
         else
           if LastPosPeriodic >= 1 then -- 190503 Test use LastPosPeriodic instead of dot_pos. Works better
              for i in 1..LastPosPeriodic loop  -- Check use last_pos instead for case 221/1 prime = 13 or 77/1 prime = 11
                  tmp.Numerator := MaxInteger(prime)**(i-1);
                  -- 190419 Changed to MaxInteger(prime)**(i-1) as we are using reverse_base_p_vector_periodic_part in encode6
                  Put_line("-- Decode4 : Case 0 : tmp.Numerator = "&Numerator(tmp)'Image&" tmp.Denominator = "&Denominator(tmp)'Image,logg);
                  x0 := x0 + (MaxInteger(hvec(i)),1)*tmp;
                  put_line("-- Decode4 : Case 0 : Integer only x0 = "&x0.Numerator'Image&"/"&x0.Denominator'Image,logg);
              end loop;
           else -- catch special case of (0,1)
               x0 := (0,1); -- 190503 Only executed of LastPosPeriodic <= 0. -- 190630 LastPosPeriodic = 0 when h = [0,0](0 0 0,0)<p>
               put_line("-- Decode4 : Case 0 : Integer only x0 = "&x0.Numerator'Image&"/"&x0.Denominator'Image,logg);
           end if;

           r := Reduction(x0);

         end if;
             -- Test to remove dot_pos = LastPosPeriodic - 1 as a condition. Not a valid condition as we store the dot_pos in the Hensel record and use with decoding !!!!
      elsif (dot_pos = LastPosPeriodic - 1 and period = 1 and h.vec(LastPosPeriodic) = prime - 1) or RecoveryLength = 0 then
         -- Case 1, Case 5 : Negative Integer
         Put_line("-- Decode4 : Case 1 and 5 : Negative Integer",logg);

         if RecoveryLength /= 0 then
            x2.Numerator := x2.Numerator + MaxInteger(hvec(LastPosPeriodic));
            put_line("-- Decode4 : Case 1 and 5 : periodic x2 = "&x2.Numerator'Image&"/"&x2.Denominator'Image,logg);

            for i in 1..s+dot_pos loop  -- s : Integer:= FirstPosPeriodic - 1 - dot_pos
              x1.Numerator := x1.Numerator + MaxInteger(hvec(i))*MaxInteger(prime)**(i-1);  -- 190422 Changed to MaxInteger(prime)**(i-1) as we need to use reverse_base-P_integer
              put_line("-- Decode4 : Case 1 and 5 : dot pos = "&dot_pos'Image&" x1 = "&x1.Numerator'Image&"/"&x1.Denominator'Image,logg);
            end loop;

            -- 190422: We need an adjustment of the hensel code from ( 1 1 2, 2) to ( 1 1 0 0 2, 4) when prime = 3
            -- Check -77 = 1*3**0+1*3**1+0*3**2+0*3**3 + 2*(3**4)/(1-3**1) = 4 - 81 = -77

            x4 := Reduction((MaxInteger(prime)**(LastPosPeriodic-1),MaxInteger(1-prime**(period))));  -- only needed when there is a periodic part, including a negative integer ex padic -77 1 3
            Put_line("-- Decode4 : Case 1 and 5 :  x4 = "&Numerator(x4)'Image&"/"&Denominator(x4)'Image,logg);

            x3 := Reduction(x2*x4);
            Put_line("-- Decode4 : Case 1 and 5 :x3 = "&Numerator(x3)'Image&"/"&Denominator(x3)'Image,logg);
            r := Reduction(x1+x3);
         elsif RecoveryLength = 0 then -- (-1,1)
            r := (-1,1);
         end if;

      elsif FirstPosPeriodic >= 1 then -- Case 2, Case 3. Case 4.
         Put_line("-- Decode4 : Case 2,3 and 4",logg);
         t := RecoveryLength; -- 190630 Changed to avoid calculating index in each loop.
         x1 := (0,1); x2 := (0,1);

         -- loop -- 190411: Test Use +4 Hensel Digits as suggested by Xin Kau Li, Chao Lu Towson University and Jon A Sjögren, Air Force Office of scientific Research
         --   x1 := decode2(h,RecoveryLength+t,logg); Put_Line("-- Decode4 : Case 2,3 and 4 : t = "&t'Image&" x1 ="&x1.Numerator'Image&"/"&x1.Denominator'Image,logg);
         --   Put_line("-- Decode4 : Case 2,3 and 4 : MaxInteger(RecoveryLength+t+4) <= MaxInteger(MaxLen)-prime+2 : "
         --            &MaxInteger(RecoveryLength+t+4)'Image&" <= "&MaxInteger'Image(MaxInteger(MaxLen)-prime+2),logg);
         --   if MaxInteger(RecoveryLength+t+4) <= MaxInteger(MaxInteger(MaxLen)-prime+2) then  -- 190422 Change to MaxInteger(MaxLen)-prime+2,
         --      -- 190514 Changed h.RecoveryLength+t+4 to RecoveryLength+t+4 as the local varable shall be used.
         --      x2 := decode2(h,RecoveryLength+t+4,logg); Put_Line("-- Decode4 : Case 2,3 and 4 : t = "&t'Image&" x2 ="&x1.Numerator'Image&"/"&x2.Denominator'Image,logg);
         --   else
         --      x2 := x1; -- No point to decode with larger MaxLen-prime+2 it will generate a contraint error. Just keep last and best decoded rational.
         --      Put_Line("-- Decode4 : Case 2,3 and 4 : MaxInteger(RecoveryLength+t+4) > MaxInteger(MaxLen)-prime+2 : t = "&t'Image,logg);
         --   end if;
         --
         --   exit when (x1.Numerator = x2.Numerator and x1.Denominator = x2.Denominator) or MaxInteger(RecoveryLength+t+4) > MaxInteger(MaxInteger(MaxLen)-prime+2);
         --   -- 190528 Test better exit criteria !!??
         --   t := t+1;
         -- end loop;

         loop -- 190630 New loop with better control of exit criteria
            x2 := decode2(h,t,logg); Put_Line("-- Decode4 : Case 2,3 and 4 : t = "&t'Image&" x2 ="&x2.Numerator'Image&"/"&x2.Denominator'Image,logg);
            if Numerator(x2) = Numerator(x1) and  Denominator(x2) = Denominator(x1) then converged := True; end if;
            exit when converged;
            exit when MaxInteger(t) > MaxInteger(MaxInteger(MaxLen)-prime+2); --
            t := t + 4; -- 190630 Use +4 Hensel Digits as suggested by Xin Kau Li, Chao Lu Towson University and Jon A Sjögren, Air Force Office of scientific Research
            x1 := x2;
         end loop;

         r := x2;

      end if;

      Put_line("-- Decode4 : converged = "&converged'image&" at t = "&t'image,logg);
      Put_line("-- Decode4 : s ="&s'image&" period = "&period'image,logg);
      Put_line("-- Decode4 : adjusted s ="&s'image,logg);
      Put_line("-- Decode4 : dot_pos = "&dot_pos'Image,logg);
      Put_line("-- Decode4 : x0 = "&Numerator(x0)'Image&"/"&Denominator(x0)'Image,logg);
      Put_line("-- Decode4 : x1 = "&Numerator(x1)'Image&"/"&Denominator(x1)'Image,logg);
      Put_line("-- Decode4 : x2 = "&Numerator(x2)'Image&"/"&Denominator(x2)'Image,logg);
      Put_line("-- Decode4 : x3 = "&Numerator(x3)'Image&"/"&Denominator(x3)'Image,logg);
      Put_line("-- Decode4 : x4 = "&Numerator(x3)'Image&"/"&Denominator(x3)'Image,logg);

      r := Reduction(r * (NumFactor,DenFactor));  -- 190430 : Corrected to use the Factors according to the ordinal set in HenselCode record

      Put_Line("-- Decode4 : r = "&Numerator(r)'Image&"/"&Denominator(r)'Image,logg);
      -- Put_Line("-- Decode4 : compare with Decode2 directly on any input = "&Image((NumFactor,DenFactor)*decode2(h,h.RecoveryLength,logg)),logg);

      return r;

   exception when Constraint_Error =>
      New_line;Put_Line("-- Decode4 : Constraint_Error in decode4, returning (0,1)",True);
      return (0,1);

   end decode4;

   function encode5(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer; logg : Boolean) return boolean is separate;

   function encode6(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer;  FixLength : Integer; logg : Boolean) return boolean is separate;

   function encode7(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer;  FixLength : Integer; logg : Boolean) return boolean is separate;

   function ResidualDecode(hcodePtrArr : HenselCodePtrArr; logg : Boolean) return Rational is separate;

   function ResidualEncode(ain,bin : in MaxInteger; HenselCArray : in out HenselCodePtrArr;  FixLength : Integer; logg : Boolean) return boolean is separate;

   function EncodeHensel(ain,bin : in MaxInteger; prime : in MaxInteger) return HenselCode is

      dot_pos : Integer := 0;
      reclen : Integer := Integer(Float(2)*Log(Float(MAX(ain,bin))/Float(0.618))/Log(Float(prime))); -- estimate size of Henselvec
      MaxLen : Integer := floor(log(float(Max_Int))/log(float(prime)))-1; -- 190524 reduced by 1. for prime 2 then MaxLen = 63 generates contraint error when prime**(MaxLen).
      R : HenselCode := (size => MaxLen+3, vec => (0, others => -(prime+1)), FirstPerPos => 0, LastPerPos => 0,
                        LastPos => MaxLen, dot_pos => 0,  prime => prime, RecoveryLength => reclen, CompletePeriod => False,
                        NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLen, ordinal => 0, value => (0,1));
   begin
      -- Put_line("-- EncodeHensel : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",True);
      if encode6(ain,bin,R,dot_pos,0,False) then
         R.dot_pos := dot_pos;
         -- R.ordinal := padic_ordinal_rational((ain,bin),prime); 190430 test this !!!
         Put("-- EncodeHensel of "&ain'Image&" / "&bin'Image&" = "); Put_Hensel_code(R,True); New_line;
      else
         -- 190611 Updated error message with "a/b = "&Image((ain,bin))"
         Put_line("-- EncodeHensel : Encode error : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",True);
      end if;

      return R;

   end EncodeHensel;

   function EncodeVariableHensel(ain,bin : in MaxInteger; prime : in MaxInteger; FirstPerPos, LastPerPos : out Integer ) return HenselCode is

      dot_pos : Integer := 0;
      reclen : Integer := Integer(Float(2)*Log(Float(MAX(ain,bin))/Float(0.618))/Log(Float(prime))); -- estimate size of Henselvec
      MaxLen : Integer := floor(log(float(Max_Int))/log(float(prime)))-1; -- 190524 reduced by 1. for prime 2 then MaxLen = 63 generates contraint error when prime**(MaxLen).
                                                                          -- 190813 Corrected as in padic.adb : floor(log(float(Max_Int))/log(float(p)))-1
      logg : Boolean := False;
      R : HenselCode := (size => MaxLen+3, vec => (0, others => -(prime+1)), FirstPerPos => 0, LastPerPos => 0,
                        LastPos => MaxLen, dot_pos => 0,  prime => prime, RecoveryLength => reclen, CompletePeriod => False,
                        NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => MaxLen, ordinal => 0, value => (0,1));
   begin
      -- Put_line("-- EncodeHensel : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",True);
      if encode6(ain,bin,R,dot_pos,0,False) then
         R.dot_pos := dot_pos;
         -- R.ordinal := padic_ordinal_rational((ain,bin),prime); 190430 test this !!!
         Put("-- EncodeVariableHensel of "&ain'Image&" / "&bin'Image&" = ",logg); Put_Hensel_code(R,logg); Put_line(" ",logg);
      else
         -- 190611 Updated error message with "a/b = "&Image((ain,bin))"
         Put_line("-- EncodeVariableHensel : Encode error : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
      end if;
      FirstPerPos := R.FirstPerPos;
      LastPerPos := R.LastPerPos;
      return R;

   end EncodeVariableHensel;

   function EncodeFixHensel(ain,bin : in MaxInteger; prime : in MaxInteger; Length : in Integer; logg : in Boolean) return HenselCode is

      dot_pos : Integer := 0;
      reclen : Integer := Integer(Float(2)*Log(Float(MAX(ain,bin))/Float(0.618))/Log(Float(prime))); -- estimate size of Henselvec
      MaxLen : Integer := floor(log(float(Max_Int))/log(float(prime)))-1; -- 190813 Corrected as calculated in padic.adb
      R : HenselCode := (size => MaxLen+3, vec => (0, others => 0), FirstPerPos => 0, LastPerPos => 0,  -- 190713 set all vec positions to 0.
                         LastPos => MaxLen, dot_pos => 0,  prime => prime, RecoveryLength => reclen, CompletePeriod => False,
                         NoPeriod => True, DenFactor => 1, NumFactor => 1, MaxLen => MaxLen, ordinal => 0, value => (0,1));
      period : Integer;
      t : Integer := 0; -- 190708 index within complete period

   begin
      Put_line("-- EncodeFixHensel : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
      if encode7(ain,bin,R,dot_pos,Length,logg) then -- 190819 Make variant for residual encoding. 190823 encode7 is the updated version for residual encoding
         -- 190707 identify all the cases as defined in encode6. --190917 Checking by decode shall be disabled with residual encoding !!
         R.dot_pos := dot_pos;
         -- R.ordinal := padic_ordinal_rational((ain,bin),prime); 190430 test this !!!
         Put("-- EncodeFixHensel of "&ain'Image&" / "&bin'Image&" = ",logg); Put_Hensel_code(R,logg); Put_Line(" ",logg);
      else
         -- 190611 Updated error message with "a/b = "&Image((ain,bin))"
         Put_line("-- EncodeFixHensel : Encode error : input : a/b = "&Image((ain,bin))&" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
      end if;
      -- 190707 Modify create the return R by expanding the periodic part up to the fix length if there is a periodic part and that the LastPerPos is less than Length.
      period := Integer(R.LastPerPos) - Integer(R.FirstPerPos) + 1; -- 190708 calc period -- 190809 move before if statement.
      Put_Line("-- EncodeFixHensel : period = "&period'Image,logg);
      if R.LastPerPos <= Length and R.CompletePeriod then
         Put_Line("-- EncodeFixHensel : R.LastPerPos <= Length and R.CompletePeriod = "&R.CompletePeriod'Image,logg);
         for i in R.LastPerPos+1..Length loop
            R.vec(i) := R.vec(R.FirstPerPos+t); Put("-- EncodeFixHensel : R.vec("&i'image&") = R.vec("&MaxInteger(R.FirstPerPos+t)'Image&") = "&R.vec(R.FirstPerPos+t)'Image,logg);
            t := (t + 1) mod period; Put_Line(" Next t = "&t'Image,logg);
         end loop;
      elsif R.LastPerPos <= Length and period = 1 then -- 190809 Added branch to handle case with negative integer
         for i in R.LastPerPos+1..Length loop
            R.vec(i) := R.vec(R.LastPerPos); Put("-- EncodeFixHensel : R.vec("&i'image&") = R.vec("&MaxInteger(R.FirstPerPos+t)'Image&") = "&R.vec(R.FirstPerPos+t)'Image,logg);
         end loop;
      end if;
      -- 190708 when length > R.LastPosPos and R.CompletePeriod = True then we know that it's a request for a fixed Henselcode
      -- 190708 Use the fact length > R.LastPosPos and R.CompletePeriod = True inside Put_Hensel_code(R,logg).
      R.LastPos := (if length > R.LastPerPos and R.CompletePeriod then length else R.LastPerPos);
      Put("-- EncodeFixHensel : Adjusted Fixed length = "&Length'Image&" Henselcode = ",logg); Put_Fixed_Hensel_code(R,Length,logg); Put_Line(" ", logg);
      return R;

   end EncodeFixHensel;


   -- 190707 make function add2( A , B : in HenselCode) return HenselCode separate
   -- 190707 function add2( A , B : in HenselCode) is separate
   -- 190707 Copy add function to padic_numbers-add2.adb
   function add2( A , B : in HenselCode) return HenselCode is separate;

   -- 190812 Introduce Add3 to make a version where leading zeroes is handled as conditional index update of array index.
   function add3( A , B : in HenselCode; logg : Boolean) return HenselCode is separate;

   function sub( A , B : in HenselCode; logg : Boolean) return Henselcode is separate;

   function add( A , B : in HenselCode) return HenselCode is separate;

   function mult( A , B : in HenselCode; logg : Boolean) return HenselCode is separate;

   -- Test 1.1.a
   -- Padic.adb : Add :  2/ 3 +  5/ 6 =  3/ 2
   -- Addition : [ 2, 3]( 4 1 3, 0)< 5> + [ 2, 3]( 1 4 0, 1)< 5> =
   -- R.vec( 1) := ( 4 +  0) mod  5 +  0 =  4 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  1) mod  5 +  0 =  2 s =  1 t =  0
   -- R.vec( 3) := ( 3 +  4) mod  5 +  0 =  2 s =  0 t =  1
   -- R.vec( 4) := ( 1 +  0) mod  5 +  1 =  2 s =  1 t =  0
   -- R.vec( 5) := ( 3 +  4) mod  5 +  0 =  2 s =  0 t =  1
   -- Add result: [ 2, 2]( 4 2, 0)< 5> =  3/ 2
   --
   -- Test 1.1.b
   -- Padic.adb : Add :  2/ 3 +  5/ 6 =  3/ 2
   -- Addition : [ 2, 3]( 4 1 3, 0)< 5> + [ 2, 3]( 1 4 0, 1)< 5> =
   -- R.vec( 1) := ( 4 +  0 +  0) mod  5 =  4 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  1 +  0) mod  5 =  2 s =  1 t =  0
   -- R.vec( 3) := ( 3 +  4 +  0) mod  5 =  2 s =  0 t =  1
   -- R.vec( 4) := ( 1 +  0 +  1) mod  5 =  2 s =  1 t =  0
   -- R.vec( 5) := ( 3 +  4 +  0) mod  5 =  2 s =  0 t =  1
   -- Add : R.ordinal =  0
   -- Add : R.dot_pos =  0
   -- Add result : [ 2, 2]( 4 2, 0)< 5> =  3/ 2
   --
   -- Test 1.2 190430 Error !!! Carry must be added before mod.
   -- Padic.adb : Add :  2/ 3 +  5/ 6 =  3/ 2
   -- Addition : [ 2, 3]( 1 1 0, 1)< 2> + [ 3, 4]( 1 1 1 0,-1)< 2> =
   -- R.vec( 1) := ( 0 +  1) mod  2 +  0 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 0 +  1) mod  2 +  0 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 1 +  1) mod  2 +  0 =  0 s =  0 t =  1
   -- R.vec( 4) := ( 1 +  0) mod  2 +  1 =  2 s =  1 t =  0
   -- R.vec( 5) := ( 0 +  1) mod  2 +  0 =  1 s =  0 t =  1
   -- R.vec( 6) := ( 1 +  0) mod  2 +  0 =  1 s =  1 t =  0
   -- R.vec( 7) := ( 0 +  1) mod  2 +  0 =  1 s =  0 t =  1
   -- Add : R.ordinal = -1
   -- Add : R.dot_pos =  0
   -- Add result : [ 2, 4]( 1 1 0 2,-1)< 2> = -11/ 14
   --
   -- Test 2.
   -- Padic.adb : Add :  1/ 6 +  1/ 2 =  2/ 3
   -- Addition : [ 2, 3]( 1 4 0, 0)< 5> + [ 2, 2]( 3 2, 0)< 5> =
   -- R.vec( 1) := ( 1 +  3) mod  5 +  0 =  4 s =  0 t =  0
   -- R.vec( 2) := ( 4 +  2) mod  5 +  0 =  1 s =  1 t =  0
   -- R.vec( 3) := ( 0 +  2) mod  5 +  1 =  3 s =  0 t =  0
   -- Add result: [ 2, 3]( 4 1 3, 0)< 5> =  2/ 3
   --
   -- Test 3.
   -- Padic.adb : Add :  5/ 6 +  2/ 3 =  3/ 2
   -- Addition : [ 2, 3]( 1 4 0, 1)< 5> + [ 2, 3]( 4 1 3, 0)< 5> =
   -- R.vec( 1) := ( 0 +  4) mod  5 +  0 =  4 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  1) mod  5 +  0 =  2 s =  0 t =  1
   -- R.vec( 3) := ( 4 +  3) mod  5 +  0 =  2 s =  1 t =  0
   -- R.vec( 4) := ( 0 +  1) mod  5 +  1 =  2 s =  0 t =  1
   -- R.vec( 5) := ( 4 +  3) mod  5 +  0 =  2 s =  1 t =  0
   -- Add result: [ 2, 2]( 4 2, 1)< 5> =  3/ 2

   -- 190507 Error ***** Padic.adb : Running padic.exe with parameters 77 7 2 on a SYSTEM_NAME_GNAT with Max_Int =  9223372036854775807 *************************************
   -- Padic.adb : Decompose(a) =  7 11 Decompose(b) =  7
   -- Padic.adb : Encoded6 P-adic sequence: [ 0, 4]( 1 1 0 1, 0)< 2>
   -- Padic.adb : Encoded6 rational - Decode4(EncodeHensel) rational :  11/ 1 -  11/ 1 =  0/ 1

   -- Padic.adb : Add :  77/ 7 +  80/ 14 =  117/ 7
   -- Addition : [ 0, 4]( 1 1 0 1, 0)< 2> + [ 2, 4]( 1 1 0 0, 3)< 2> =
   -- Add : MaxAB =  63 ExitIndex =  10 ABPerLength =  3
   -- Add : A.DenFactor =  1 B.DenFactor =  1
   -- Add : A.NumFactor =  1 B.NumFactor =  8
   -- R.vec( 1) := ( 1 +  0 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  0 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 4) := ( 1 +  1 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 5) := ( 0 +  1 +  1) mod  2 =  0 s =  0 t =  1
   -- R.vec( 6) := ( 0 +  0 +  1) mod  2 =  1 s =  0 t =  2
   -- R.vec( 7) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 8) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  1
   -- R.vec( 9) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  2
   -- R.vec( 10) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- Add : R.ordinal =  0 R.FirstPerPos =  2 R.LastPerPos =  4
   -- Add : R.dot_pos =  0 ExitIndex =  10 NoPeriod = FALSE LastCarryIndex =  6 R.ordinal =  0
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 2, 4]( 1 1 0 0, 0)< 2>
   -- Add result : [ 2, 4]( 1 1 0 0, 0)< 2> =  5/ 7
   --
   -- Test 190509 Noew it works fine to Add :  80/ 14 +  77/ 7 =  117/ 7
   -- Padic.adb : Add :  80/ 14 +  77/ 7 =  117/ 7
   -- Addition : [ 2, 4]( 1 1 0 0, 3)< 2> + [ 0, 4]( 1 1 0 1, 0)< 2> =
   -- Add : A.MaxLen =  63 B.MaxLen =  63
   -- Add : MaxAB =  63 ExitIndex =  12 ABPerLength =  3
   -- Add : A.DenFactor =  1 B.DenFactor =  1
   -- Add : A.NumFactor =  8 B.NumFactor =  1
   -- Add : R.RecoveryLength =  17
   -- Add : APeriod =  3 BPeriod =  0
   -- R.vec( 1) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 4) := ( 1 +  1 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  0 +  1) mod  2 =  0 s =  1 t =  0
   -- R.vec( 6) := ( 0 +  0 +  1) mod  2 =  1 s =  2 t =  0
   -- R.vec( 7) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 8) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 9) := ( 0 +  0 +  0) mod  2 =  0 s =  2 t =  0
   -- R.vec( 10) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 11) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 12) := ( 0 +  0 +  0) mod  2 =  0 s =  2 t =  0
   -- Add : R.ordinal =  0 R.FirstPerPos =  7 R.LastPerPos =  9 R.LastPos =  12
   -- Add : R.dot_pos =  0 ExitIndex =  12 NoPeriod = FALSE LastCarryIndex =  6 R.ordinal =  0
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 7, 9]( 1 1 0 0 0 1 0 1 0, 0)< 2>
   -- Add result : [ 7, 9]( 1 1 0 0 0 1 0 1 0, 0)< 2> =  117/ 7 Success

   -- 190509 : Validate R.FirstPerPos. See test run in logg190508_9.txt. Correct Answer is [ 5, 6]( 1 0 0 2 2 1,-1)< 3> = 35/ 24.
   -- Proposed action : Fix carry spillover from non-periodic part to beginning of periodic part of R. Adjust one step ! 190509 Works good.
   -- Padic.adb : Add :  10/ 16 +  10/ 12 =  35/ 24
   -- Addition : [ 2, 3]( 1 1 0, 0)< 3> + [ 3, 3]( 1 2 1,-1)< 3> =
   -- Add : A.MaxLen =  39 B.MaxLen =  39
   -- Add : MaxAB =  39 ExitIndex =  8 ABPerLength =  2
   -- Add : A.DenFactor =  1 B.DenFactor =  3
   -- Add : A.NumFactor =  1 B.NumFactor =  1
   -- Add : R.RecoveryLength =  8
   -- Add : APeriod =  2 BPeriod =  1
   -- R.vec( 1) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  2 +  0) mod  3 =  0 s =  0 t =  0
   -- R.vec( 3) := ( 1 +  1 +  1) mod  3 =  0 s =  1 t =  0
   -- R.vec( 4) := ( 0 +  1 +  1) mod  3 =  2 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  1 +  0) mod  3 =  2 s =  1 t =  0
   -- R.vec( 6) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- R.vec( 7) := ( 1 +  1 +  0) mod  3 =  2 s =  1 t =  0
   -- R.vec( 8) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- Add : R.ordinal = -1 R.FirstPerPos =  4 R.LastPerPos =  5
   -- Add : R.dot_pos =  0 ExitIndex =  8 NoPeriod = FALSE LastCarryIndex =  4 R.ordinal = -1
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 4, 5]( 1 0 0 2 2,-1)< 3>
   -- Add result : [ 4, 5]( 1 0 0 2 2,-1)< 3> = -26/ 3 Failure
   --
   -- 190509 Introduced carryvec to store carry for each hensel digit in add function.
   -- Padic.adb : Add :  10/ 16 +  10/ 12 =  35/ 24
   -- Addition : [ 2, 3]( 1 1 0, 0)< 3> + [ 3, 3]( 1 2 1,-1)< 3> =
   -- Add : A.MaxLen =  39 B.MaxLen =  39
   -- Add : MaxAB =  39 ExitIndex =  8 ABPerLength =  2
   -- Add : A.DenFactor =  1 B.DenFactor =  3
   -- Add : A.NumFactor =  1 B.NumFactor =  1
   -- Add : R.RecoveryLength =  8
   -- Add : APeriod =  2 BPeriod =  1
   -- R.vec( 1) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 1 +  2 +  0) mod  3 =  0 s =  0 t =  0
   -- R.vec( 3) := ( 1 +  1 +  1) mod  3 =  0 s =  1 t =  0
   -- R.vec( 4) := ( 0 +  1 +  1) mod  3 =  2 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  1 +  0) mod  3 =  2 s =  1 t =  0
   -- R.vec( 6) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- R.vec( 7) := ( 1 +  1 +  0) mod  3 =  2 s =  1 t =  0
   -- R.vec( 8) := ( 0 +  1 +  0) mod  3 =  1 s =  0 t =  0
   -- Add : R.ordinal = -1 R.FirstPerPos =  5 R.LastPerPos =  6 R.LastPos =  8
   -- Add : R.dot_pos =  0 ExitIndex =  8 NoPeriod = FALSE LastCarryIndex =  4 R.ordinal = -1
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 5, 6]( 1 0 0 2 2 1,-1)< 3>
   -- Add result : [ 5, 6]( 1 0 0 2 2 1,-1)< 3> =  35/ 24 Success

   -- Put_Line("-- Add : 2413 : R.FirstPerPos = "&R.FirstPerPos'Image&"R.LastPerPos = "&R.LastPerPos'Image&" R.LastPos = "&R.LastPos'Image);
   -- if R.LastPerPos > MaxAB then -- 190509 Limit index use to allowed interval [0..MaxAB+3] Check wihout this code
   --    R.vec(0) := MaxInteger(MaxAB); -- 190508 Moved outside leading_zeroes
   --    R.vec(MaxAB + 1) := MaxInteger(R.FirstPerPos);
   --    R.LastPos := MaxAB;
   -- else
   --     R.vec(0) := MaxInteger(R.LastPerPos);
   --     R.vec(R.LastPerPos + 1) := MaxInteger(R.FirstPerPos);
   -- end if;
   --
   -- Tests 190508
   -- Padic.adb : Add :  80/ 22 +  77/ 11 =  117/ 11
   -- Addition : [ 2, 11]( 1 1 1 1 0 1 0 0 0 1 0, 3)< 2> + [ 0, 3]( 1 1 1, 0)< 2> =
   -- Add : MaxAB =  63 ExitIndex =  24 ABPerLength =  10
   -- Add : A.DenFactor =  1 B.DenFactor =  1
   -- Add : A.NumFactor =  8 B.NumFactor =  1
   -- Add : R.RecoveryLength =  24
   -- R.vec( 1) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 4) := ( 1 +  0 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 6) := ( 1 +  0 +  0) mod  2 =  1 s =  2 t =  0
   -- R.vec( 7) := ( 1 +  0 +  0) mod  2 =  1 s =  3 t =  0
   -- R.vec( 8) := ( 0 +  0 +  0) mod  2 =  0 s =  4 t =  0
   -- R.vec( 9) := ( 1 +  0 +  0) mod  2 =  1 s =  5 t =  0
   -- R.vec( 10) := ( 0 +  0 +  0) mod  2 =  0 s =  6 t =  0
   -- R.vec( 11) := ( 0 +  0 +  0) mod  2 =  0 s =  7 t =  0
   -- R.vec( 12) := ( 0 +  0 +  0) mod  2 =  0 s =  8 t =  0
   -- R.vec( 13) := ( 1 +  0 +  0) mod  2 =  1 s =  9 t =  0
   -- R.vec( 14) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 15) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 16) := ( 1 +  0 +  0) mod  2 =  1 s =  2 t =  0
   -- R.vec( 17) := ( 1 +  0 +  0) mod  2 =  1 s =  3 t =  0
   -- R.vec( 18) := ( 0 +  0 +  0) mod  2 =  0 s =  4 t =  0
   -- R.vec( 19) := ( 1 +  0 +  0) mod  2 =  1 s =  5 t =  0
   -- R.vec( 20) := ( 0 +  0 +  0) mod  2 =  0 s =  6 t =  0
   -- R.vec( 21) := ( 0 +  0 +  0) mod  2 =  0 s =  7 t =  0
   -- R.vec( 22) := ( 0 +  0 +  0) mod  2 =  0 s =  8 t =  0
   -- R.vec( 23) := ( 1 +  0 +  0) mod  2 =  1 s =  9 t =  0
   -- R.vec( 24) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- Add : R.ordinal =  0 R.FirstPerPos =  2 R.LastPerPos =  11
   -- Add : R.dot_pos =  0 ExitIndex =  24 NoPeriod = FALSE LastCarryIndex =  0 R.ordinal =  0
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 2, 11]( 1 1 1 1 1 1 1 0 1 0 0, 0)< 2>
   -- Add result : [ 2, 11]( 1 1 1 1 1 1 1 0 1 0 0, 0)< 2> =  641/ 1023
   --
   -- Test 190509 Now it works fine to Add :  80/ 22 +  77/ 11 =  117/ 11
   -- Padic.adb : Add :  80/ 22 +  77/ 11 =  117/ 11
   -- Addition : [ 2, 11]( 1 1 1 1 0 1 0 0 0 1 0, 3)< 2> + [ 0, 3]( 1 1 1, 0)< 2> =
   -- Add : A.MaxLen =  63 B.MaxLen =  63
   -- Add : MaxAB =  63 ExitIndex =  26 ABPerLength =  10
   -- Add : A.DenFactor =  1 B.DenFactor =  1
   -- Add : A.NumFactor =  8 B.NumFactor =  1
   -- Add : R.RecoveryLength =  24
   -- Add : APeriod =  10 BPeriod =  0
   -- R.vec( 1) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 2) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 3) := ( 0 +  1 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 4) := ( 1 +  0 +  0) mod  2 =  1 s =  0 t =  0
   -- R.vec( 5) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 6) := ( 1 +  0 +  0) mod  2 =  1 s =  2 t =  0
   -- R.vec( 7) := ( 1 +  0 +  0) mod  2 =  1 s =  3 t =  0
   -- R.vec( 8) := ( 0 +  0 +  0) mod  2 =  0 s =  4 t =  0
   -- R.vec( 9) := ( 1 +  0 +  0) mod  2 =  1 s =  5 t =  0
   -- R.vec( 10) := ( 0 +  0 +  0) mod  2 =  0 s =  6 t =  0
   -- R.vec( 11) := ( 0 +  0 +  0) mod  2 =  0 s =  7 t =  0
   -- R.vec( 12) := ( 0 +  0 +  0) mod  2 =  0 s =  8 t =  0
   -- R.vec( 13) := ( 1 +  0 +  0) mod  2 =  1 s =  9 t =  0
   -- R.vec( 14) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 15) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 16) := ( 1 +  0 +  0) mod  2 =  1 s =  2 t =  0
   -- R.vec( 17) := ( 1 +  0 +  0) mod  2 =  1 s =  3 t =  0
   -- R.vec( 18) := ( 0 +  0 +  0) mod  2 =  0 s =  4 t =  0
   -- R.vec( 19) := ( 1 +  0 +  0) mod  2 =  1 s =  5 t =  0
   -- R.vec( 20) := ( 0 +  0 +  0) mod  2 =  0 s =  6 t =  0
   -- R.vec( 21) := ( 0 +  0 +  0) mod  2 =  0 s =  7 t =  0
   -- R.vec( 22) := ( 0 +  0 +  0) mod  2 =  0 s =  8 t =  0
   -- R.vec( 23) := ( 1 +  0 +  0) mod  2 =  1 s =  9 t =  0
   -- R.vec( 24) := ( 0 +  0 +  0) mod  2 =  0 s =  0 t =  0
   -- R.vec( 25) := ( 1 +  0 +  0) mod  2 =  1 s =  1 t =  0
   -- R.vec( 26) := ( 1 +  0 +  0) mod  2 =  1 s =  2 t =  0
   -- Add : R.ordinal =  0 R.FirstPerPos =  5 R.LastPerPos =  14 R.LastPos =  26
   -- Add : R.dot_pos =  0 ExitIndex =  26 NoPeriod = FALSE LastCarryIndex =  0 R.ordinal =  0
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 5, 14]( 1 1 1 1 1 1 1 0 1 0 0 0 1 0, 0)< 2>
   -- Add result : [ 5, 14]( 1 1 1 1 1 1 1 0 1 0 0 0 1 0, 0)< 2> =  117/ 11 Success

   -- 190630 Dicovered errot when adding 3/ 7 +  6/ 14 = 6/ 7.
   -- 190702 Changed ExitIndex : MaxInteger := abs(MaxInteger(A.ordinal) - MaxInteger(B.ordinal)) + MAX(MaxInteger(A.LastPerPos),MaxInteger(B.LastPerPos)) + 3*ABPerLength + 2; -- 190702 Test 3*ABPerLength and it works better.
   -- Padic.adb : Add :  3/ 7 +  6/ 14 =  6/ 7 Estimated recoverylength =  7
   -- EncodeHensel : input : a/b =  6/ 14 and  2 is a TRUE prime
   -- EncodeHensel : input : a/b =  3/ 7 and  2 is a TRUE prime
   -- Addition : [ 2, 4]( 1 0 1 0, 0)< 2> + [ 2, 4]( 1 0 1 0, 0)< 2> =  586/ 1 Failure

   -- 190702 Test after change of ExitIndex:
   -- Padic.adb : Add :  3/ 7 +  6/ 14 =  6/ 7 Estimated recoverylength =  7
   -- EncodeHensel : input : a/b =  6/ 14 and  2 is a TRUE prime
   -- EncodeHensel : input : a/b =  3/ 7 and  2 is a TRUE prime
   -- Addition : [ 2, 4]( 1 0 1 0, 0)< 2> + [ 2, 4]( 1 0 1 0, 0)< 2> =
   -- Add : A.MaxLen =  62 B.MaxLen =  62
   -- Add : MaxAB =  62 ExitIndex =  15 ABPerLength =  3
   -- Add : A.DenFactor =  1 B.DenFactor =  1
   -- Add : A.NumFactor =  1 B.NumFactor =  1
   -- Add : R.RecoveryLength =  12
   -- Add : APeriod =  3 BPeriod =  3 A.FirstPerPos =  2 A.LastPerPos =  4 B.FirstPerPos =  2 B.LastPerPos =  4
   -- Add : LCM(APeriod,BPeriod) =  3
   -- Add : Decompose(LCM(APeriod,BPeriod)) =  3
   -- Add : MaxDivisor(LCM(APeriod,BPeriod)) =  3
   -- Add : MinDivisor(LCM(APeriod,BPeriod)) =  3
   -- Add : MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos)) =  2
   -- Add : Theory R.FirstPerPos <= MAX(MaxInteger(A.FirstPerPos),MaxInteger(B.FirstPerPos))+MinDivisor(LCM(APeriod,BPeriod))+3 =  8
   -- Add : Theory R.LastPerPos <= R.FirstPerPos + ABPerLength - 1 =  10
   -- R.vec( 1) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  0 t(next i) =  0
   -- R.vec( 2) := ( 0 +  0 +  1) mod  2 =  1 s(next i) =  1 t(next i) =  1
   -- R.vec( 3) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  2 t(next i) =  2
   -- R.vec( 4) := ( 0 +  0 +  1) mod  2 =  1 s(next i) =  0 t(next i) =  0
   -- R.vec( 5) := ( 0 +  0 +  0) mod  2 =  0 s(next i) =  1 t(next i) =  1
   -- R.vec( 6) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  2 t(next i) =  2
   -- R.vec( 7) := ( 0 +  0 +  1) mod  2 =  1 s(next i) =  0 t(next i) =  0
   -- R.vec( 8) := ( 0 +  0 +  0) mod  2 =  0 s(next i) =  1 t(next i) =  1
   -- R.vec( 9) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  2 t(next i) =  2
   -- R.vec( 10) := ( 0 +  0 +  1) mod  2 =  1 s(next i) =  0 t(next i) =  0
   -- R.vec( 11) := ( 0 +  0 +  0) mod  2 =  0 s(next i) =  1 t(next i) =  1
   -- R.vec( 12) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  2 t(next i) =  2
   -- R.vec( 13) := ( 0 +  0 +  1) mod  2 =  1 s(next i) =  0 t(next i) =  0
   -- R.vec( 14) := ( 0 +  0 +  0) mod  2 =  0 s(next i) =  1 t(next i) =  1
   -- R.vec( 15) := ( 1 +  1 +  0) mod  2 =  0 s(next i) =  2 t(next i) =  2
   -- Add before adj at 2199 : R.ordinal =  0 R.FirstPerPos =  0 R.LastPerPos =  0 R.LastPos =  15
   -- Add before adj at 2200 : A.ordinal =  0 A.FirstPerPos =  2 A.LastPerPos =  4 A.LastPos =  43
   -- Add before adj at 2201 : B.ordinal =  0 B.FirstPerPos =  2 B.LastPerPos =  4 B.LastPos =  43
   -- Add before adj at 2202 : common_nonperiodic = TRUE
   -- Add : A.FirstPerPos /= 0 and B.FirstPerPos /= 0
   -- Add : (APeriod /= 1 and BPeriod /= 1 and Aperiod = BPeriod and A.FirstPerPos = B.FirstPerPos)
   -- Add : (R.RecoveryLength > R.FirstPerPos + Integer(ABPerLength) - 1) and R.RecoveryLength <= Integer(ExitIndex)
   -- Add : R.ordinal =  0 R.FirstPerPos =  10 R.LastPerPos =  12 R.LastPos =  15
   -- Add : R.dot_pos =  0 ExitIndex =  15 NoPeriod = TRUE LastCarryIndex =  13 R.ordinal =  0
   -- Add : carryvec( 10) =  1
   -- Add : carryvec( 11) =  0
   -- Add : carry_spill_over(carryvec, 10) =  11
   -- Add : before adjustment of leading/trailing zeroes and period one : [ 10, 12]( 0 1 0 1 0 0 1 0 0 1 0 0, 0)< 2>
   -- Add : trailing_zeroes : LastIndex =  15 h.size =  65
   -- Add : trailing_zeroes : found_integer = FALSE
   -- Add : leading_zeroes =  1
   -- Add : shift_vector_left :  h.vec( 15) =  0
   -- Add : After adjustment of leading zeroes : [ 10, 12]( 1 0 1 0 0 1 0 0 1 0 0 1, 0)< 2>
   -- Add : A.FirstPerPos /= 0 or B.FirstPerPos /= 0
   -- Add : R.ordinal =  1 R.FirstPerPos =  10 R.LastPerPos =  12 R.LastPos =  12 R.size =  65
   -- Add : R.MaxLen =  62
   -- Add : MaxAB =  62 ExitIndex =  15 ABPerLength =  3
   -- Add : R.DenFactor =  1
   -- Add : R.NumFactor =  1
   -- Add : R.RecoveryLength =  12
   -- Add : R.size =  65 R.ordinal =  1
   -- Add : R.dot_pos =  1
   -- Add result : [ 10, 12]( 1 0 1 0 0 1 0 0 1 0 0 1, 1)< 2>-- ..... Start Decode4 ....
   -- Decode4 : Encoded P-adic sequence input: [ 10, 12]( 1 0 1 0 0 1 0 0 1 0 0 1, 1)< 2>
   -- Decode4 : FirstPosPeriodic =  10 LastPosPeriodic =  12
   -- Decode4 : h.Num_factor =  1 h.Den_factor =  1
   -- Decode4 : h.First_per_pos =  10 h.Last_per_pos =  12
   -- Decode4 : h.dot_pos =  1 s =  8
   -- Decode4 : MaxLen = h.MaxLen =  62 RecoveryLength =  13
   -- Decode4 : h.ordinal =  1
   -- Decode4 : Case 2,3 and 4
   -- Decode2 : sumx : r =  13
   -- Decode2 : sumx : x =  2341
   -- ..... Start Decode2 ....
   -- r =  13 x =  2341
   -- u(-1) =  8192 v(-1) =  0
   -- Decoding length r =  13 Must be <=  62
   -- q( 0) =  3 u(next i) =  1169 v(next i) =  3
   -- q( 1) =  2 u(next i) =  3 v(next i) =  7
   -- q( 2) =  389 u(next i) =  2 v(next i) =  2726
   -- Num =  3
   -- Den =  7
   -- Decode4 : Case 2,3 and 4 : t =  13 x2 = 3/ 7
   -- Decode2 : sumx : r =  17
   -- Decode2 : sumx : x =  18725
   -- ..... Start Decode2 ....
   -- r =  17 x =  18725
   -- u(-1) =  131072 v(-1) =  0
   -- Decoding length r =  17 Must be <=  62
   -- q( 0) =  6 u(next i) =  18722 v(next i) =  6
   -- q( 1) =  1 u(next i) =  3 v(next i) =  7
   -- q( 2) =  6240 u(next i) =  2 v(next i) =  43686
   -- Num =  3
   -- Den =  7
   -- Decode4 : Case 2,3 and 4 : t =  17 x2 = 3/ 7
   -- Decode4: converged = TRUE at t =  17
   -- Decode4: s = 8 period =  3
   -- Decode4: adjusted s = 8
   -- Decode4: dot_pos =  1
   -- Decode4: x0 =  0/ 1
   -- Decode4: x1 =  3/ 7
   -- Decode4: x2 =  3/ 7
   -- Decode4: x3 =  0/ 1
   -- Decode4: x4 =  0/ 1
   -- Decode4: r =  6/ 7
   --   =  6/ 7 Success


end padic_numbers;
