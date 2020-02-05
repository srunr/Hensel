separate (padic_numbers)
function encode5(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer; logg : Boolean) return boolean is
      av : Henselvec := Hcode.vec;
      alfa : Rational := Reduction((ain,bin));
      r : Rational := alfa; -- Save initial input for later use inside procedure
      prime : MaxInteger := Hcode.prime;
      j,k : Integer := 0;

      MaxLen : Integer := floor(log(float(Max_Int))/log(float(prime)));

      M : Integer := 0; -- scaling factor for bringing r' into -1 <= r < 0 with r' = r + M
      type Rational_array is array(0..av'Length+5) of Rational; -- Size needs to be larger than av.
      alfa_arr : Rational_array:= (others => (0,1)); -- 190407 Changed to (0,1) default.
      -- similar to the length of the a_vector, use index O position to store index to the first periodic pos.
      found : boolean := false;

      n : Integer := padic_ordinal_rational((ain,bin),prime); -- dot position for Hensel code
      r_negative : boolean := ( alfa < 0);
      r_between_neg_1_and_zero : boolean := ((alfa < 0) and (alfa >= -1));
      r_less_than_neg_1 : boolean := (alfa < -1);

      a_num_p_factor : MaxInteger := GCD(Numerator(alfa),prime);
      alfa_Numerator_p_factor : Boolean := (a_num_p_factor > 1);

      a_den_p_factor : MaxInteger := GCD(Denominator(alfa),prime);
      alfa_Denominator_p_factor : Boolean := (a_den_p_factor > 1);

      alfa_ordinal : Integer := padic_ordinal_rational(alfa,prime);

      function PossibleDecode(a : in Rational; h : HenselCode) return boolean is
      begin
         return (Float(MAX(a.Numerator,a.Denominator)) <= sqrt(Float((h.prime**h.RecoveryLength-1))/2.0));
      end PossibleDecode;

      function member(A : in Rational; R_arr : in out Rational_array) return boolean is
         found : boolean := false;
      begin
         for i in 1..R_arr'Length-1 loop
            if A = R_arr(i) then
               found := true;
               R_arr(0) := (MaxInteger(i),1); -- Use the fact that (i,1) identify type rational.
            end if;
         end loop;
         return found;
      end member;

      procedure create_pure_periodic_vector(alfa : in out Rational; p : in MaxInteger; av : in out Henselvec; alfa_arr : in out Rational_array; logg : in boolean) is
         a : MaxInteger;
         i : Integer := 1; -- index for  loop, save pos 0 for the pos of the periodic sequence
         prime : MaxInteger := p;
      Begin
        loop
          a:= ((Numerator(alfa) mod prime)*(invmod(Denominator(alfa), prime))) mod prime;
          Put("-- Encode5 : Iter : "& i'Image &" Module : "& a'Image,logg);
          alfa_arr(i) := alfa;
          Put(" Input fraction = "& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
          alfa := (alfa - a)/prime; Put_Line(" Fraction for next :"& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
                -- put_line("Alfa_arr("& i'Image&") = "& Numerator(alfa_arr(i))'Image &"/"& Denominator(alfa_arr(i))'Image);
          if i >= av'Length - 3 then  -- pos 0 indicate last pos for hensel digit and last pos indicate pos for first periodic digit, leaving av'Length - 3 as hensel digit length before negation.
             av(0):= MaxInteger(i); -- Last pos of sequence.
             av(i+1) := 1; -- Use next index to store the first position of the not completed periodic sequence. pure_periodic then First_per_pos = 1
             av(i) := a; -- Last number in periodic sequence
             found := true; -- use up the available vector. May not complete the periodic period.
             exit;
          elsif  member(alfa,alfa_arr) then -- every p-adic sequence of a rational number is eventually periodic. See https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf
             av(0):= MaxInteger(i); -- Last pos of periodic sequence
             av(i+1) := 1; -- Use next index to store the first position of the periodic sequence. pure_periodic then First_per_pos = 1
             av(i) := a; -- Last number in periodic sequence
             found := true;
             exit;
          end if;
          av(i) := a;  -- only update when confirmed to be inside period.
          i := i + 1;
        end loop;
      end create_pure_periodic_vector;

      procedure negate_vector(av : in out Henselvec) is
         FirstPerPos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         LastPerPos : Integer := Integer(av(0)); -- end position of incoming part
         vecsize :Integer := av'Length - 1;
         tmp : MaxInteger := av(FirstPerPos); -- save pos to first periodic number
      begin
         -- bring positive r back by negate_hensel( ...) consider to create as with shift

         for i in 1..LastPerPos + 1 loop -- min Last_per_pos = 1 leading to range 1..2
            if i = 1 then
               av(i) := prime - av(i);
            elsif i <= LastPerPos then
               av(i) := prime - 1 - av(i);
            elsif i = LastPerPos + 1 then
               if LastPerPos /= FirstPerPos and FirstPerPos = 1 then -- Adjust pos of periodic sequence when negating pure periodic due to separate handling of pos 1.
                  av(i+1) := MaxInteger(FirstPerPos) + 1; -- new start pos of periodic part
                  av(0) := MaxInteger(LastPerPos) + 1; -- new end pos of periodic part
                  av(i) := MaxInteger(prime) - 1 - tmp; -- move single periodic number one step
               elsif LastPerPos = FirstPerPos and av(LastPerPos-1) = av(LastPerPos) and LastPerPos > 1 then --190414 Changed to av(LastPerPos-1) = av(LastPerPos)
                  -- Do not adjust length when the two last padic numbers are equal and the periodic length is one and pointing to the last padic nr in the vector.
                  -- If av(last_index) = av(last_index - 1) and Last_index - First_index = 0 then period sequence starts at pos Last_index
                 av(i) := tmp; -- tmp contain the start number of the incoming periodic sequence that shall keep the periodic start and end position.
               elsif LastPerPos = FirstPerPos and LastPerPos = 1 then -- handle special case of Hansel vector length of 1 and period = 1
                  av(i+1) := MaxInteger(FirstPerPos) + 1; -- new start pos of periodic part
                  av(0) := MaxInteger(LastPerPos) + 1; -- new end pos of periodic part
                  av(i) := prime - 1 - tmp;
               end if;
            end if;
         end loop;

      end negate_vector;

      procedure reverse_base_p_vector_periodic_part(Number : in MaxInteger; p : in Integer; vec : in out Henselvec) is
         k : Integer := 0;
         n : Integer := floor(log(float(Number),float(prime))); -- Corrected 190324 use floor() instead of Integer()
         Prime : MaxInteger := MaxInteger(p);
      begin
         vec(n+1) := 1; -- First pos of periodic sequence
         for k in 0..n loop
            vec(n-k+1) := (Number/prime**(n-k)) mod prime;
         end loop;
         vec(0) := MaxInteger(n+1); -- Last pos of periodic sequence
      end reverse_base_p_vector_periodic_part;

      procedure base_p_vector_integer_part(Num : in MaxInteger; p : in MaxInteger; vec : in out Henselvec) is
         k, index : Integer := 0;
         Number : MaxInteger := abs(Num);
         n : Integer := floor(Float(log(float(Number),float(prime)))); -- Corrected 190324 use floor() instead of Integer()
         Prime : MaxInteger := p;
      begin
         put_line("-- Encode5 : base_p_vector_integer_part : Number = "&Number'Image&" p = "&p'Image&" n = "&n'Image);

         for k in 0..n loop
            vec(k+1) := (Number/prime**(n-k)) mod prime;
            index := k+1;
            Put_line("-- Encode5 : vec("&index'Image&") = "&vec(k+1)'Image);
         end loop;

      end base_p_vector_integer_part;

      procedure reverse_base_p_vector_integer_part(Num : in MaxInteger; p : in MaxInteger; vec : in out Henselvec) is
         k, index : Integer := 0;
         Number : MaxInteger := abs(Num);
         n : Integer := floor(Float(log(float(Number),float(prime)))); -- Corrected 190324 use floor() instead of Integer()
         Prime : MaxInteger := p;
      begin
         put_line("-- Encode5 : reverse_base_p_vector_integer_part : Number = "&Number'Image&" p = "&p'Image&" n = "&n'Image);

         for k in 0..n loop
            vec(n-k+1) := (Number/prime**(n-k)) mod prime;
            index := n-k+1;
            Put_line("-- Encode5 : vec("&index'Image&") = "&vec(n-k+1)'Image);
         end loop;

      end reverse_base_p_vector_integer_part;

      procedure shift_vector(j : in integer; vec : in out Henselvec; logg : Boolean) is
         -- <= check:190317 length of remaining nr of hensel numbers after shifting and extend to same length as before shift, using periodic numbers
         FirstPerPos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         LastPerPos : Integer := Integer(av(0)); -- end position of incoming part
         period : Integer := LastPerPos - FirstPerPos + 1;
      begin
         Put("-- Encode5 : Encoded P-adic sequence to be shifted: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
         Put_line("-- Encode5 : First_pos = "&FirstPerPos'Image&" Last_pos = "&LastPerPos'Image,logg);
         Put_Line("-- Encode5 : Postions to shift = "&j'Image,logg);
         vec(LastPerPos+j+1) := MaxInteger(FirstPerPos) + MaxInteger(j); -- Move indicator of first pos of periodic to new last position.
         -- 190708 compared with encode6 : vec(Last_per_pos+j+1) := MaxInteger(First_per_pos) + MaxInteger(j);
         -- move periodic hensel digits first
         for i in 1..period loop
            vec(LastPerPos + j - i + 1) := vec(LastPerPos - i + 1); -- move from back end to new position
         end loop;

         if FirstPerPos > 1 then
           for i in 1..FirstPerPos-1 loop
              vec(FirstPerPos+j-i) := vec(FirstPerPos - i); -- move from back end to new position
           end loop;
         end if;

         for i in 1..j loop
            vec(i) := 0; -- fill integers parts with zeroes for postions that are less or equal than the shifted
         end loop;

         vec(0) := MaxInteger(LastPerPos) + MaxInteger(j); -- new last position of the periodic sequence
         Put("-- Encode5 : Encoded P-adic sequence shifted: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
      end shift_vector;

      procedure scale_shift_vector(j : in integer; vec : in out Henselvec; logg : Boolean) is --190407 New procedure for left shift of common denominators when scaling into [-1,0) interval.
         -- <= check:190317 length of remaining nr of hensel numbers after shifting and extend to same length as before shift, using periodic numbers
         FirstPerPos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         LastPerPos : Integer := Integer(av(0)); -- end position of incoming part
         period : Integer := LastPerPos - FirstPerPos + 1;
         tmp : MaxInteger;
      begin
         Put("-- Encode5 : Encoded P-adic sequence to left shifted: ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
         Put_line("-- Encode5 : First_pos = "&FirstPerPos'Image&" Last_pos = "&LastPerPos'Image,logg);
         Put_Line("-- Encode5 : Postions to rotate left = "&j'Image,logg);

         if period > 1 then
           for i in 1..j loop  -- rotation steps j and can be > period
              for k in 1..period-1 loop
                 tmp := vec(k);  -- tmp := vec(1), vec(2), ,,,, vec(period-1)
                 vec(k) := vec(k+1); -- vec(1) := vec(2),,,, vec(period-1) := vec(period);
                 vec(k+1) := tmp; -- vec(period) := vec(1);
              end loop;
            end loop;
         end if;

         Put("-- Encode5 : Encoded P-adic sequence left rotated: ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
      end scale_shift_vector;

      function prime_k_mod(num : in MaxInteger; p : in MaxInteger; logg : in Boolean) return Integer is
         k : Integer := 0;
         prime : MaxInteger := p;
      begin
         while num mod prime**k = 0 loop
            Put_line("-- Encode5 : "&num'image&" mod prime**"&k'image, logg);
            k := k + 1;
         end loop;
         return k - 1;
      end prime_k_mod;

      function find_truncation_sum( prime : in MaxInteger; m : in MaxInteger; vec : in out Henselvec; j : out Integer) return MaxInteger is  -- does only work with purely periodic hensel vectors as input.
         found : boolean := false;
         s, sum : MaxInteger := 0; -- sum up the vector digits using the repeated sequence and identfy the position j where the sum > m.
         -- n : Integer := Integer(log(float(m),float(prime)))-1; -- Floor !!! Does not work for the for loop n is not used !!!!!!!!!
         FirstPerPos : Integer := Integer(av(Integer(av(0))+1)); -- The postion after last period postion, contain the start pos of the periodic sequence.
         Last_pos_period : Integer := Integer(av(0)); -- zero position of incoming Hensel vector vec contain last position of the periodic sequence.
         k : Integer := 0;
         period : Integer := Last_pos_period - FirstPerPos + 1; -- For Q(prime = 3): 1/2 => .1 with period 1. First_pos_period = Last_pos_period.
      begin
         -- make sure to use all repeating vec numbers even if it's only one. Create a index k that is mod length of period.
         j := 1; -- initialize truncation position
         loop
            s := s + vec(k+1)*MaxInteger(prime)**(j-1);
            j := j + 1;
            if k > 0 and k mod period = 0 then
               k := 0;  -- start over after end of period
            else k := k + 1;
            end if;
            sum := s; -- collect sum as return value
            found := (s > m);
            exit when found;
         end loop;
         j := j - 1; -- j is the nr of shift right to to be used in shift procedure to make space for the encoded sum + M integer
         return sum;
      end find_truncation_sum;

      function CommonFactors(A : in out Rational; num_p_factor, den_p_factor : in out MaxInteger; k : out Integer; p : MaxInteger; Numerator_p_factor, Denominator_p_factor : in out Boolean; logg : in Boolean) return boolean is
         prime : MaxInteger := MaxInteger(p);
      begin
        num_p_factor := GCD(Numerator(A),prime);
        Numerator_p_factor := (num_p_factor > 1);
        den_p_factor := GCD(Denominator(A),prime);
        Denominator_p_factor := (den_p_factor > 1);
        if Numerator_p_factor then
           Put_line("-- Encode5 : Common p with numerator", logg);
           k := prime_k_mod(Numerator(A),prime, logg);
           Put_line("-- Encode5 : k = "&k'Image, logg);
           num_p_factor := prime**k;
           A.Numerator := A.Numerator / num_p_factor;  -- remember after encoding
        elsif Denominator_p_factor then
           Put_line("-- Encode5 : Common p with Denominator", logg);
           k := prime_k_mod(Denominator(A),prime, logg);
           Put_line("-- Encode5 : k = "&k'Image, logg);
           den_p_factor := prime**k;
           A.Denominator := A.Denominator / den_p_factor;  -- remember after encoding
         end if;
         return Numerator_p_factor or Denominator_p_factor;
      end CommonFactors;


   begin -- Encode 5
      Hcode.MaxLen := MaxLen;
      if (bin /= 0) and is_Prime(prime) then
         -- Put_Line("-- **** Start Encode 5 ****",logg);
         -- Check 190404 special case when negating Hensel vector with length 1 !!!!!! padic 1 3 7 and padic 1 21 7 Corrected 190405 in negate vector !
         Put_Line("-- Encode5 : Correct input:  b /= 0 and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
         Put_line("-- Encode5 : prime < sqr(Max_Int) = "&sqrt(Float(Max_Int))'Image&" Length < Max_Int + 2 - prime = "&Max_Int'Image&" + 2 - "&prime'Image,logg);
         Put_line("-- Encode5 : padic_ordinal_rational = "& n'Image&" r_negative = "&r_negative'Image&" r_between_neg_1_and_zero = "&r_between_neg_1_and_zero'Image,logg);
         Put_line("-- Encode5 : r_less_than_neg_1 = "&r_less_than_neg_1'Image,logg);
         Put_Line("-- Encode5 : a_num_p_factor = "&a_num_p_factor'Image&" alfa_Numerator_p_factor = "&alfa_Numerator_p_factor'Image,logg);
         Put_line("-- Encode5 : a_den_p_factor = "&a_den_p_factor'Image&" alfa_Denominator_p_factor = "&alfa_Denominator_p_factor'Image,logg);
         Put_line("-- Encode5 : absprime = "&Image(absprime(alfa,prime)),logg);
         Put_line("-- Encode5 : Numerator(alfa) = "& Numerator(alfa)'Image&" Denominator(alfa) = "&Denominator(alfa)'Image,logg);

         if CommonFactors(alfa,a_num_p_factor,a_den_p_factor,k,prime,alfa_Numerator_p_factor,alfa_Denominator_p_factor, logg) then
            -- make sure to separate from other commonfactors examination later during scaling into interval [-1,0).

            if alfa_Numerator_p_factor then
               Put_line("-- Encode5 : num_p_factor = "&a_num_p_factor'Image,logg);
               Hcode.NumFactor := a_num_p_factor; -- save in HenselCode record for use at decoding.
            end if;

            if alfa_Denominator_p_factor then
               Put_line("-- Encode5 : den_p_factor = "&a_den_p_factor'Image,logg);
               Hcode.DenFactor := a_den_p_factor; -- save in HenselCode record for use at decoding.
            end if;

         end if;
         -- 190409 Check padic_ordinal_rational((ain,bin),prime); after common factors
         Put_line("-- Encode5 : Adjusted after Common factors : padic_ordinal_rational = "& padic_ordinal_rational(alfa,prime)'Image,logg);
         Put_line("-- Encode5 : absprime = "&Image(absprime(alfa,prime)),logg);

         -- Check 190405: handle case when Commonfactors result in (1,1) ie both Num and Den can be reduced to (1,1) given the provided prime !!!!
         if alfa /= (1,1) and alfa /= (-1,1) then
            Put("-- Encode5 : Adjusted input:  b /= 0 and GCD("& alfa.Numerator'Image &","& alfa.Denominator'Image&") = "&MaxInteger'Image(GCD(alfa.Numerator,alfa.Denominator)),logg);
            Put("  and GCD("& alfa.Numerator'Image &","& prime'Image &") = "&MaxInteger'Image(GCD(alfa.Numerator,prime)),logg);
            Put(" and GCD("& alfa.Denominator'Image&","& prime'Image &") = "&MaxInteger'Image(GCD(alfa.Denominator,prime)),logg);
            Put_line("and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime");

            -- Case 0 : r is a positive integer. The padic expansion is the reverse base p expansion
            -- Case 1 : r is a negative integer. There is a j >= 1 such that R < prime**(j), then prime**(j) - R - prime**(j) can be expressed as follows.
            --          prime**(j) - R is an integer in {1, ..., prime**(j) - 1} and we can write it in base prime as c0 +...+ c(j-1)*prime**(j-1) then
            --          for j > log(R)/log(prime), alfa = sum(i=0,j-1,c(i)*prime**(i)) + sum(i>=j,(prime-1)*prime**(i). Leading to a periodic part with period 1 and digit prime - 1.
            --          190329: Make sure that the periodic part has the proper position at encoding of negative integer.
            --          Ex is -77 => ( 1 1 2, 2) correct ? Check j = 4 then the 2 should be on pos 4 !!!
            --          Seems that we need an adjustment of the hensel code from ( 1 1 2, 2) to ( 0 0 1 1 2, 4) Check -77 = 0*3**3+0*3**2+1*3**1+1*3**0 + 2*(3**4)/(1-3**1) = 4 - 81 = -77 Correct
            -- Case 2 : r is a rational with r in [-1,0) and GCD(ain,p) = 1, GCD(bin,p) = 1, GCD(ain,bin) = 1 then p-adic expansion is purely periodic.
            -- Case 3 : r is a rational with GCD(ain,p) > 1 then write r = p**(n)*u. Then u = r/p**(n) is rational.
            --          Use Case 2 expansion for u and right shift n steps.
            -- Case 4 : r is rational with r.Denominator /= 1 and r < -1. The number r lies strictly between two negative integers -(M+1)<r<(-M)
            --          so - 1 < r + M < 0. Since the rational r added with an integer M is a rational, Case 3 apply.
            --          Let j be the smallest choice fitting the inequality a(0)+...a(j-1)*p**(j-1) > M.
            --          Then a(0)+...a(j-1)*p**(j-1) - M is a positive integer that is less than p**j - 1 so we can write the difference in base p as
            --          a(0)+...a(j-1)*p**(j-1) - M = a(0)'+...a(j-1)'*p**(j-1) with 0 <= a(i)' <= p-1 and Case 0 applies.
            --          Then the two parts together becomes r = a(0)'+...a(j-1)'*p**(j-1) + sum(i>=j, a(i)*p**(i)).
            -- Case 5 : r is rational with r.Denominator /= 1 and r < 0. Since p**(K)*r can be adjusted to be an integer by a large K
            --          we can use Case 1 and then divide by p**(K). Shift dot position K steps to the left.
            -- Reference: the p-adic expantion of rational numbers https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf

            if not r_negative and alfa.Denominator /= 1 then -- assumption for all Cases, except Case 1 with Den = 1.
               alfa := -alfa; -- Case 0   !!! 190407 Compiler can not compile alfa * (-1) correctly !!!!!
            end if;
            r_between_neg_1_and_zero := ((alfa < 0) and (alfa >= -1));
            r_less_than_neg_1 := (alfa < -1);
            Put_line("-- Encode5 : padic_ordinal_rational = "&padic_ordinal_rational(alfa,prime)'Image&" r_negative = "&r_negative'Image&" r_between_neg_1_and_zero = "&r_between_neg_1_and_zero'Image,logg);
            Put_line("-- Encode5 : Adjusted common factors: Numerator(alfa) = "& Numerator(alfa)'Image&" Denominator(alfa) = "&Denominator(alfa)'Image,logg);

            if alfa.Denominator = 1 then -- r is integer
               Put_line("-- Encode5 : Case 0 or 1: alfa.Denominator = 1",logg);
               -- 190328: alfa := - r and there is a j >= 1 such that r < prime**(j) then  alfa = -r = (prime**(j)-r) - prime**(j).
               -- Then prime(j) - r is an integer in {1, ..., prime(j) - 1} we can write in base prime as c0 +...+ c(j-1)*prime**(j-1) then
               -- for j > log(r)/log(prime), alfa = sum(i=0,j-1,c(i)*prime**(i)) + sum(i>=j,(prime-1)*prime**(i).
               -- 190329: Make sure that the periodic part has the proper position at encoding of negative integer.
               -- Ex is -77 => ( 1 1 2, 2) correct ? Check j = 4 then the 2 should be on pos 4 !!!
               -- We need an adjustment of the hensel code from ( 1 1 2, 2) to ( 0 0 1 1 2, 4) Check -77 = 0*3**3+0*3**2+1*3**1+1*3**0 + 2*(3**4)/(1-3**1) = 4 - 81 = -77 Correct
               Put_line("-- Encode5 : Input Numerator(alfa) = "&Numerator(alfa)'Image,logg);

               if r_negative then  -- Case 1 Negative -- handle case (-1,1) !!!

                  j := Ceiling(Float(Float(log(Float(abs(alfa.Numerator))))/Float(log(Float(prime))))); -- Find the postion of periodic part to be used to set prime

                  alfa.Numerator := prime**(j) + alfa.Numerator; -- adjusted numerator corresponding to p**j - abs(r).
                  reverse_base_p_vector_integer_part(Numerator(alfa),prime, av); -- 190419 Corrected to to use reverse_base_p_vector_integer_part instead of base_p_integer_part

                  dot_pos := floor(Float(log(float(abs(Numerator(alfa))),float(prime))))+1; -- set initial dot_pos before adjustment.
                  av(0) := MaxInteger(dot_pos + 1); -- set last pos before adjustment and an extra space for the periodic digit
                  av(Integer(av(0))+1) := MaxInteger(dot_pos + 1); -- the last hensel digit is the periodic one with period 1
                  av(dot_pos+1) := prime - 1; -- all periodic digits are set to (prime-1) with period 1

                  Put("-- Encode5 : Case 1 r_negative before adjustment : Hensel vector = ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
                  Put_line("Adjusted with j = "&j'Image&" Numerator(alfa) = "&Numerator(alfa)'Image,logg);

                  shift_vector(j-dot_pos,av,logg); -- shift j-dot_pos dot positions

                  dot_pos := j;

                  Put("-- Encode5 : Case 1 r_negative after adjustment  : Hensel vector = ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
                  Hcode.dot_pos := dot_pos;
                  Hcode.LastPos := dot_pos+1;
                  Hcode.FirstPerPos := dot_pos+1;
                  Hcode.LastPerPos := dot_pos+1;
                  Hcode.vec := av;
                  Hcode.RecoveryLength := dot_pos+1;
               else -- Case 0 : r postive integer  -- Handle case alfa = (1,1)  !!!
                  reverse_base_p_vector_integer_part(Numerator(alfa),prime, av); -- 190419 Corrected to to use reverse_base_p_vector_integer_part instead of base_p_integer_part
                  dot_pos := floor(Float(log(float(abs(Numerator(alfa))),float(prime))))+1;
                  av(0) := MaxInteger(dot_pos);
                  av(dot_pos + 1) := 0; -- set FirstPerPosition = 0 to indicate no fractional part
                  Put("-- Encode5 : Case 0 not r_negative : Hensel vector = ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
                  -- all hensel numbers before lead to dot_pos = Last_pos
                  Hcode.dot_pos := dot_pos;
                  Hcode.LastPos := dot_pos;
                  Hcode.FirstPerPos := 0;
                  Hcode.vec := av;
                  Hcode.NoPeriod := (Hcode.FirstPerPos = 0);
                  Hcode.ordinal := -dot_pos; -- 190429
               end if;

               found := true;
               return found; -- no further actions !!!! ????

            elsif alfa.Denominator /=1 then -- r is a rational

               if r_between_neg_1_and_zero and GCD(alfa.Numerator,prime) = 1 and GCD(alfa.Denominator,prime) = 1 and GCD(alfa.Numerator,alfa.Denominator) = 1 then
                  -- Case 2 : r is a rational with r in [-1,0) and GCD(ain,p) = 1, GCD(bin,p) = 1, GCD(ain,bin) = 1 then p-adic expansion is purely periodic.
                  Put_line("-- Encode5 : Case 2: r_between_neg_1_and_zero",logg);
                  create_pure_periodic_vector(alfa, prime, av, alfa_arr, logg);
                  found := true;
                  dot_pos := 0;
                  Hcode.dot_pos := dot_pos;
               elsif r_between_neg_1_and_zero and GCD(alfa.Numerator, prime) > 1 and (alfa.Numerator in 1..prime-1) then
                  -- Case 3 : r is a rational with GCD(ain,p) > 1 then write r = p**(k)*u. u is in Zp, Then u = r/p**(k) is rational.
                  --          Use Case 2 expansion for u and right shift k steps.
                  Put_line("-- Encode5 : Case 3: r_between_neg_1_and_zero with common numerator factor",logg);
                  create_pure_periodic_vector(alfa, prime, av, alfa_arr, logg);
                  shift_vector(k,av,logg);  -- k calculated up front in Commonfactors
                  found := true;
                  dot_pos := k;
                  Hcode.dot_pos := dot_pos;
               elsif  r_less_than_neg_1 then --Case 4 190410: Use invmod method from create_pure_periodic here as well.
                  -- Case 4 : r is rational with r.Denominator /= 1 and r < -1. The number r lies strictly between two negative integers -(M+1)<r<(-M)
                  --          so - 1 < r + M < 0. Since the rational r added with an integer M is a rational, Case 3 apply.
                  --          Let j be the smallest choice fitting the inequality a(0)+...a(j-1)*p**(j-1) > M.
                  --          Then a(0)+...a(j-1)*p**(j-1) - M is a positive integer that is less than p**j - 1 so we can write the difference in base p as
                  --          a(0)+...a(j-1)*p**(j-1) - M = a(0)'+...a(j-1)'*p**(j-1) with 0 <= a(i)' <= p-1 and Case 0 applies.
                  --          Then the two parts together becomes r = a(0)'+...a(j-1)'*p**(j-1) + sum(i>=j, a(i)*p**(i)).
                  CASE4 : declare
                     A : Float := Float(Float(Numerator(alfa))/Float(Denominator(alfa)));
                     j : Integer;
                     Integer_part : MaxInteger;
                     scale_num_p_factor : MaxInteger := GCD(Numerator(alfa),prime);
                     scale_Numerator_p_factor : Boolean := (scale_num_p_factor > 1);
                     scale_den_p_factor : MaxInteger := GCD(Denominator(alfa),prime);
                     scale_Denominator_p_factor : Boolean := (scale_den_p_factor > 1);
                  begin
                     Put_line("-- Encode5 : Case 4: r_less_than_neg_1",logg);

                     -- scale and adjust into [-1, 0) interval
                     A := Float(Float(Numerator(alfa))/Float(Denominator(alfa))); -- update A in case of common p factors
                     M := floor(A)+1; -- alfa is in interval (-floor(A)..-floor(A)+1), select M = -floor(A)+1. Corrected 190323 from Integer(A).
                     Put_line("-- Encode5 : M = "&M'Image,logg);
                     alfa := alfa + MaxInteger(abs(M)); -- r' := r + M to ensure r' is witin [-1,0) then use Case 2
                     Put_line("-- Encode5 : r' := r + abs(M) : r' = "&Image(alfa),logg);
                     Put_line("-- Encode5 : Adjusted after M : padic_ordinal_rational = "& padic_ordinal_rational(alfa,prime)'Image,logg);
                     Put_line("-- Encode5 : Adjusted scale: Numerator(alfa) = "& Numerator(alfa)'Image&" Denominator(alfa) = "&Denominator(alfa)'Image,logg);
                     dot_pos := 0;
                     Hcode.dot_pos := dot_pos;
                     create_pure_periodic_vector(alfa, prime, av, alfa_arr, logg); -- same as Case 2
                     Integer_part := find_truncation_sum(prime, MaxInteger(abs(M)), av, j) + MaxInteger(M);
                     Put("-- Encode5 : Case 4: r_less_than_neg_1 before common Den factor after scaling av: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);

                     -- 190407: j is the truncation position on vector before common factor adjustment
                     -- Check for common factors again after scaling to [-1,0)
                     -- function CommonFactors(A : in out Rational; num_p_factor, den_p_factor : in out MaxInteger; k : out Integer; p : MaxInteger;
                     --                        Numerator_p_factor, Denominator_p_factor : in out Boolean) return boolean is

                     if CommonFactors(alfa,scale_num_p_factor,scale_den_p_factor,k,prime,scale_Numerator_p_factor,scale_Denominator_p_factor, logg) then
                        Put_line("-- Encode5 : scale common factors : k = "&k'Image&" scale_Numerator_p_factor = "&scale_num_p_factor'Image&" scale_Denominator_p_factor = "&scale_den_p_factor'Image,logg);
                     end if;

                     scale_shift_vector(j,av,logg); -- shift periodic part to back and keep period, rotate periodic part j times.
                     Put("-- Encode5 : Encoded P-adic sequence pure periodic av: ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);

                     if M /= 0 then
                        Put_line("-- Encode5 : M /= 0 and Integer_part to be shifted in = "&Integer_part'Image,logg);
                        shift_vector(j,av,logg); -- make space in the beginning of henselvec for the integer part.  190410: scale_shift shall be used. Se ex padic 77 18 5.
                        -- lift out truncated part and fill with zeroes and add lifted part to end
                        -- Adjust period start and period end.
                        -- reverse_base_p_vector_integer_part(Integer_part, prime,av);
                        reverse_base_p_vector_integer_part(Integer_part, prime,av);
                        Put_line("-- Encode5 : absprime Integer part = "&Image(absprime((Integer_part,1),prime)),logg);
                     end if;

                     Hcode.RecoveryLength := (if Integer(av(0)) < MaxLen then Integer(av(0)) else MaxLen);  -- set recovery length with restrictions to MaxLen

                     -- adjust common factors after shifting
                     dot_pos := j;
                     if scale_Numerator_p_factor then
                        dot_pos := -k+dot_pos;
                        Hcode.dot_pos := dot_pos; Put_line("-- Encode5 : adjust common factors -k+j after shifting: Hcode.dot_pos = "&Hcode.dot_pos'Image,logg);
                     end if;
                     if scale_Denominator_p_factor then
                        dot_pos := k+dot_pos;
                        Hcode.dot_pos := dot_pos; Put_line("-- Encode5 : adjust common factors k+j after shifting: Hcode.dot_pos = "&Hcode.dot_pos'Image,logg);
                     end if;
                     Hcode.dot_pos := dot_pos;
                     found := true;

                  end CASE4;

               elsif r_less_than_neg_1 and not (alfa_Numerator_p_factor or alfa_Denominator_p_factor) then -- Case 5.
                  -- Unreachable due to common factors already removed in CommonFactors function
                     Put_line("-- Encode5 : Case 5: r_less_than_neg_1 and no common factors",logg);
                     dot_pos := Ceiling(log(Float(alfa.Denominator))/log(Float(prime)));
                     Put_line("-- Encode5 : dot_pos = "&dot_pos'Image,logg);
                     alfa := alfa * prime**dot_pos;
                     Hcode.dot_pos := dot_pos;
                     Put_Line("-- Encode5 : alfa = "&Image(alfa),logg);
                     reverse_base_p_vector_integer_part(Numerator(alfa),prime, av); -- Adjusted to fit Case 1
                     found := true;
               end if;
               Put("-- Encode5 : Encoded P-adic sequence before negation: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
               if not r_negative then
                     negate_vector(av);
               end if;
               Put("-- Encode5 : Encoded P-adic sequence after negation: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);


               -- Put_line("Scaling and negation failed: dot position = "&n'Image&" scaling N = "& N'Image&" r_negative = "&r_negative'Image);

               -- r : Integer := Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))); -- estimate size of Henselvec
               Put_line("-- Encode5 : Estimated size of Henselvec for correct recovery : "&
                         Integer(Float(2)*Log(Float(MAX(alfa.Numerator,alfa.Denominator))/Float(0.618))/Log(Float(Integer(prime))))'Image,logg);
               Put_line("-- Encode5 : Hcode.RecoveryLength = "&Hcode.RecoveryLength'Image,logg);
               Hcode.vec := av;
               Hcode.FirstPerPos := Integer(av(Integer(av(0))+1));
               Hcode.LastPerPos := Integer(av(0));
               Hcode.dot_pos := dot_pos;
               Put_line("-- Encode5 : PossibleDecode is "&PossibleDecode(alfa,Hcode)'Image,logg);
               return found;
            end if;

            -- Unreachable code
            Put_line("-- Encode5 : Error in finding hensel code",logg);
            -- Put_line("Scaling and negation: dot position = "&dot_pos'Image&" scaling N = "& M'Image&" r_negative = "&r_negative'Image);
            -- Hcode.vec := av;
            -- Hcode.First_per_pos := Integer(av(Integer(av(0))+1));
            -- Hcode.Last_per_pos := Integer(av(0));
            -- Hcode.dot_pos := dot_pos;

            Put("-- Encode5 : Hcode.vec before exit of encode: ",logg);Put_Hensel_vector(Hcode.vec,dot_pos,logg); Put_line(" ",logg);
            return true;
         else
            -- alfa = (1,1) or alfa = (-1,1)

            if alfa = (1,1) then
               Put_line("-- Encode5 : Case 0 alfa = (1,1)",logg);
               av(0) := 1;
               av(2) := 0; -- First_per_pos = 0 for positive integer
               av(1) := 1;
               dot_pos := 0; Hcode.dot_pos := 0;
               Hcode.vec := av;
               Hcode.FirstPerPos := 0; -- Indicate no fractional part
               Hcode.LastPerPos := 1;
               Hcode.RecoveryLength := 0; -- Indicate nothing to recover
               Hcode.NoPeriod := true;
            elsif alfa = (-1,1) then
               Put_line("-- Encode5 : Case 0 alfa = (-1,1)",logg);
               av(0) := 1;
               av(2) := 1; -- First_per_pos = 1 for negative integer
               av(1) := 1;
               dot_pos := 0; Hcode.dot_pos := 0;
               Hcode.vec := av;
               Hcode.FirstPerPos := 1; -- Indicate fractional part (treated as negative integer in combination with NoPeriod at decoding)
               Hcode.LastPerPos := 1;
               Hcode.RecoveryLength := 0; -- Indicate nothing to recover
               Hcode.NoPeriod := true;
            end if;

            return True;
         end if;
      else
         Put_line("-- Encode5 : Incorrect input : bin = "& bin'Image &" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
         return False;
      end if;
   end encode5;
