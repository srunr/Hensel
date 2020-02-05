separate (padic_numbers)
function encode6(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer; FixLength : Integer; logg : Boolean) return boolean is
      av : Henselvec := Hcode.vec;
      abs_ain : MaxInteger := abs ain;
      alfa : Rational := Reduction((ain,bin));
      r : Rational := alfa; -- Save initial input for later use inside procedure
      prime : MaxInteger := Hcode.prime;
      j,k : Integer := 0;

      MaxLen : Integer := floor(log(float(Max_Int))/log(float(prime)))-1; -- 190524 reduced by 1. Prevent overflow for MaxInt type

      M : Integer := 0; -- scaling factor for bringing r' into -1 <= r < 0 with r' = r + M
      type Rational_array is array(0..av'Length+5) of Rational; -- Size needs to be larger than av.
      alfa_arr : Rational_array:= (others => (0,1)); -- 190407 Changed to (0,1) default.
      -- similar to the length of the a_vector, use index O position to store index to the first periodic pos.
      found : boolean := false;
      r_zero : boolean := (Numerator(alfa) = 0); -- 190630 Added to take care of zero input.
      n : Integer := (if not r_zero then padic_ordinal_rational((ain,bin),prime) else Integer'Last); -- dot position for Hensel code
      -- 190630 : padic ordinal is not defined for ain = 0. Use integer'Last to monitor

      r_negative : boolean := ( alfa < 0);
      r_between_neg_1_and_zero : boolean := ((alfa < 0) and (alfa >= -1));
      r_less_than_neg_1 : boolean := (alfa < -1);

      a_num_p_factor : MaxInteger := GCD(Numerator(alfa),prime);
      alfa_Numerator_p_factor : Boolean := (a_num_p_factor > 1);

      a_den_p_factor : MaxInteger := GCD(Denominator(alfa),prime);
      alfa_Denominator_p_factor : Boolean := (a_den_p_factor > 1);

      alfa_ordinal : Integer := (if not r_zero then padic_ordinal_rational(alfa,prime) else Integer'Last);
      -- 190630 Added condition r_zero

      UseFixLength : Boolean := (FixLength > 0); -- 190707 identify if Fix length encoding is required. 190813 Add parameter FixLength to list.

      -- 190810 : Add idenfication of potential Hensel overflow situation
      -- Encode6 : prime < sqr(Max_Int) =  3.03700E+09 Length < Max_Int + 2 - prime =  9223372036854775807 + 2 -  2 MaxLen =  62
      HenselOverflow : boolean := not (((prime < floor(sqrt(float(Max_Int)))) and (float(MaxLen) < float(float(Max_Int) + 2.0 - float(prime)))) or
                                  ((prime < floor(sqrt(float(MAX(abs_ain,bin))))) and (float(MaxLen) < float(float(MAX(abs_ain,bin))) + 2.0 - float(prime))));

      -- 190414 Error found. 190414 Corrected at line negate_vector line: 1141. Changed to av(Last_per_pos-1) = av(Last_per_pos)
      -- ***** Running padic.exe with parameters 5 3 7 on a SYSTEM_NAME_GNAT with Max_Int =  9223372036854775807 *************************************
      -- Running in directory C:\Users\soren\Documents\Ada2012projects\Hensel2\obj
      -- Find the p-adic sequence of a number by entering a b p:
      -- Decompose(a) =  5 Decompose(b) =  3
      --  Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))) =  2
      -- Padic ordinal =  0

      -- **** Start Encode 6 ****
      -- Correct input:  b /= 0 and  7 is a TRUE prime
      -- prime < sqr(Max_Int) =  3.03700E+09 Length < Max_Int + 2 - prime =  9223372036854775807 + 2 -  7 MaxLen =  22
      -- padic_ordinal_rational =  0 r_negative = FALSE r_between_neg_1_and_zero = FALSE
      -- r_less_than_neg_1 = FALSE
      -- a_num_p_factor =  1 alfa_Numerator_p_factor = FALSE
      -- a_den_p_factor =  1 alfa_Denominator_p_factor = FALSE
      -- absprime =  1/ 1
      -- Numerator(alfa) =  5 Denominator(alfa) =  3
      -- Adjusted after Common factors : padic_ordinal_rational =  0
      -- absprime =  1/ 1
      --  Adjusted input:  b /= 0 and GCD( 5, 3) =  1 and GCD( 5, 7) =  1 and GCD( 3, 7) =  1and  7 is a TRUE prime
      --  padic_ordinal_rational =  0 r_negative = FALSE r_between_neg_1_and_zero = FALSE
      -- Adjusted common factors: Numerator(alfa) = -5 Denominator(alfa) =  3
      -- Case 4_5: r_less_than_neg_1
      -- create_vector
      --  Iter :  1 Module :  3 Input fraction = -5/ 3 Fraction for next :-2/ 3
      --  Iter :  2 Module :  4 Input fraction = -2/ 3 Fraction for next :-2/ 3
      -- Encoded P-adic sequence before negation: -- Dot pos =  1 First periodic pos = 2 Last_pos =  2 Hensel vector = ( 3 4, 1)
      -- Encoded P-adic sequence after negation: -- Dot pos =  1 First periodic pos = 4 Last_pos =  2 Hensel vector = ( 4 2, 1)
      -- Estimated size of Henselvec for correct recovery :  2
      -- Hcode.RecoveryLength =  2
      -- PossibleDecode is TRUE

      -- ..... Start Decode4 ....
      -- Encoded P-adic sequence input: -- Dot pos =  1 First periodic pos = 4 Last_pos =  2 Hensel vector = ( 4 2, 1)
      -- FirstPosPeriodic =  4 LastPosPeriodic =  2
      -- h.Num_factor =  1 h.Den_factor =  1
      -- h.First_per_pos =  4 h.Last_per_pos =  2
      -- h.dot_pos =  1
      -- MaxLen = h.MaxLen =  22
      -- Decode4 : Case 2,3 and 4

      -- ..... Start Decode2 ....
      -- r =  4 x = -2530
      -- u(-1) =  2401 v(-1) =  0
      -- Decoding length r =  4 Must be <=  22
      -- q( 0) = -1 u(next i) = -129 v(next i) = -1
      -- q( 1) =  19 u(next i) = -79 v(next i) = -18
      -- q( 2) =  1 u(next i) = -50 v(next i) = -19
      -- q( 3) =  1 u(next i) = -29 v(next i) = -37
      -- q( 4) =  1 u(next i) = -21 v(next i) = -56
      -- Num = -29
      -- Den = -37

      -- ..... Start Decode2 ....
      -- r =  6 x = -156194
      -- u(-1) =  117649 v(-1) =  0
      -- Decoding length r =  6 Must be <=  22
      -- q( 0) = -1 u(next i) = -38545 v(next i) = -1
      -- q( 1) =  4 u(next i) = -2014 v(next i) = -3
      -- q( 2) =  19 u(next i) = -279 v(next i) = -58
      -- q( 3) =  7 u(next i) = -61 v(next i) = -409
      -- Num =  279
      -- Den = -58

      -- ..... Start Decode2 ....
      -- r =  6 x = -156194
      -- u(-1) =  117649 v(-1) =  0
      -- Decoding length r =  6 Must be <=  22
      -- q( 0) = -1 u(next i) = -38545 v(next i) = -1
      -- q( 1) =  4 u(next i) = -2014 v(next i) = -3
      -- q( 2) =  19 u(next i) = -279 v(next i) = -58
      -- q( 3) =  7 u(next i) = -61 v(next i) = -409
      -- Num =  279
      -- Den = -58

      -- ..... Start Decode2 ....
      -- r =  7 x = -1097386
      -- u(-1) =  823543 v(-1) =  0
      -- Decoding length r =  7 Must be <=  22
      -- q( 0) = -1 u(next i) = -273843 v(next i) = -1
      -- q( 1) =  4 u(next i) = -2014 v(next i) = -3
      -- q( 2) =  135 u(next i) = -1953 v(next i) = -406
      -- q( 3) =  1 u(next i) = -61 v(next i) = -409
      -- q( 4) =  32 u(next i) = -1 v(next i) = -13494
      -- Num = -61
      -- Den = -409

      -- ..... Start Decode2 ....
      -- r =  8 x = -7685730
      -- u(-1) =  5764801 v(-1) =  0
      -- Decoding length r =  8 Must be <=  22
      -- q( 0) = -1 u(next i) = -1920929 v(next i) = -1
      -- q( 1) =  4 u(next i) = -2014 v(next i) = -3
      -- q( 2) =  953 u(next i) = -1587 v(next i) = -2860
      -- Num = -2014
      -- Den = -3

      -- ..... Start Decode2 ....
      -- r =  8 x = -7685730
      -- u(-1) =  5764801 v(-1) =  0
      -- Decoding length r =  8 Must be <=  22
      -- q( 0) = -1 u(next i) = -1920929 v(next i) = -1
      -- q( 1) =  4 u(next i) = -2014 v(next i) = -3
      -- q( 2) =  953 u(next i) = -1587 v(next i) = -2860
      -- Num = -2014
      -- Den = -3
      -- s = 2 period = -1
      -- adjusted s = 2
      -- dot_pos =  1
      -- x0 =  0/ 1
      -- x1 =  2014/ 3
      -- x2 =  2014/ 3
      -- x3 =  0/ 1
      -- x4 =  0/ 1
      -- r =  2014/ 3
      -- Encode6 could not find a Hensel vector with prime =  7
      -- No padic number found or Overflow


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

      procedure create_pure_periodic_vector(alfa : in out Rational; p : in MaxInteger; av : in out Henselvec;
                                            alfa_arr : in out Rational_array; logg : Boolean) is
         a : MaxInteger;
         i : Integer := 1; -- index for  loop, save pos 0 for the pos of the periodic sequence
         prime : MaxInteger := p;
         MaxLen : Integer := floor(log(float(Max_Int))/log(float(p)));
      Begin
         Put_Line("-- Encode6 : create_pure_periodic_vector with Length = "&Integer(av'Length-3)'Image,logg);
         loop
         a:= ((Numerator(alfa) mod prime)*(invmod(Denominator(alfa), prime))) mod prime;
         Put("-- Encode6 : Iter : "& i'Image &" Module a = "&a'Image&" alfa = "&Image(alfa),logg);
         alfa_arr(i) := alfa;
         Put(" Input fraction = "&Image(alfa),logg);
         alfa := (alfa - a)/prime; Put_Line( " (alfa - a)/prime = Fraction for next :"&Image(alfa),logg);
         -- put_line("Alfa_arr("& i'Image&") = "& Numerator(alfa_arr(i))'Image &"/"& Denominator(alfa_arr(i))'Image);
         if i >= av'Length - 2 then  -- pos 0 indicate last pos for hensel digit and last pos indicate pos for first periodic digit, leaving av'Length - 3 as hensel digit length before negation.
            av(0):= MaxInteger(i); -- Last pos of sequence.
            av(i+1) := 1; -- Use next index to store the first position of the not completed periodic sequence. pure periodic then First per pos is 1
            av(i) := a; -- Last number in periodic sequence
            found := true; -- use up the available vector. May not complete the periodic period.
            Put_Line("-- Encode6 : create_pure_periodic_vector : full period member NOT found",logg);
            exit;
         elsif  member(alfa,alfa_arr) then -- every p-adic sequence of a rational number is eventually periodic. See https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf
            av(0):= MaxInteger(i); -- Last pos of periodic sequence
            av(i+1) := 1; -- Use next index to store the first position of the periodic sequence. pure periodic then First per pos is 1
            av(i) := a; -- Last number in periodic sequence
            found := true;
            Put_Line("-- Encode6 : create_pure_periodic_vector : full period member found",logg);
            exit;
         end if;
         av(i) := a;  -- only update when confirmed to be inside period.
         i := i + 1;
      end loop;
      Put_Line("-- Encode6 : create_pure_periodic_vector : member(alfa,alfa_arr) = "&member(alfa,alfa_arr)'Image,logg);
   end create_pure_periodic_vector;

      procedure create_vector(alfa : in out Rational; p : in MaxInteger; av : in out Henselvec; alfa_arr : in out Rational_array; logg : Boolean) is
         a : MaxInteger;
         i : Integer := 1; -- index for  loop, save pos 0 for the pos of the periodic sequence
         prime : MaxInteger := p;
      Begin
        Put_Line("-- Encode6 : create_vector",logg);
        loop
          a:= ((Numerator(alfa) mod prime)*(invmod(Denominator(alfa), prime))) mod prime;
          Put("-- Encode6 : Iter : "& i'Image &" Module : "& a'Image,logg);
          alfa_arr(i) := alfa;
          Put(" Input fraction = "& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
          alfa := (alfa - a)/prime; Put_Line(" Fraction for next :"& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
                -- put_line("Alfa_arr("& i'Image&") = "& Numerator(alfa_arr(i))'Image &"/"& Denominator(alfa_arr(i))'Image);
          if i >= av'Length - 3 then  -- pos 0 indicate last pos for hensel digit and last pos indicate pos for first periodic digit, leaving av'Length - 3 as hensel digit length before negation.
             av(0):= MaxInteger(i); -- Last pos of sequence.
             av(i+1) := 1; -- Use next index to store the first position of the not completed periodic sequence. Use 1 for the uncomplete periodic sequence.
             av(i) := a; -- Last number in periodic sequence
             found := true; -- use up the available vector. May not complete the periodic period.
             Put_Line("-- Encode6 : create_vector : full period member NOT found",logg);
             exit;
          elsif  member(alfa,alfa_arr) then -- every p-adic sequence of a rational number is eventually periodic. See https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf
             av(0):= MaxInteger(i); -- Last pos of periodic sequence
             av(i+1) := Numerator(alfa_arr(0)); -- Use next index to store the first position of the periodic sequence. fetch pos from alfa_arr(0) resulting from member function.
             av(i) := a; -- Last number in periodic sequence
             found := true;
             Put_Line("-- Encode6 : create_vector : full period member found",logg);
             exit;
          end if;
          av(i) := a;  -- only update when confirmed to be inside period.
          i := i + 1;
        end loop;
        Put_Line("-- Encode6 : create_vector : member(alfa,alfa_arr) = "&member(alfa,alfa_arr)'Image,logg);
      end create_vector;


      procedure negate_vector(av : in out Henselvec) is
         First_per_pos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         Last_per_pos : Integer := Integer(av(0)); -- end position of incoming part
         vecsize :Integer := av'Length - 1;
         tmp : MaxInteger := av(First_per_pos); -- save pos to first periodic number
      begin
         -- bring positive r back by negate_hensel( ...) consider to create as with shift

         for i in 1..Last_per_pos + 1 loop -- min Last_per_pos = 1 leading to range 1..2
            if i = 1 then
               av(i) := prime - av(i);
            elsif i <= Last_per_pos then
               av(i) := prime - 1 - av(i);
            elsif i = Last_per_pos + 1 then
               if Last_per_pos /= First_per_pos and First_per_pos = 1 then -- Adjust pos of periodic sequence when negating pure periodic due to separate handling of pos 1.
                  av(i+1) := MaxInteger(First_per_pos) + 1; -- new start pos of periodic part
                  av(0) := MaxInteger(Last_per_pos) + 1; -- new end pos of periodic part
                  av(i) := MaxInteger(prime) - 1 - tmp; -- move single periodic number one step
               elsif Last_per_pos = First_per_pos and av(Last_per_pos-1) = av(Last_per_pos) and Last_per_pos > 1 then  -- 190414 Changed to av(Last_per_pos-1) = av(Last_per_pos)
                  -- Do not adjust length when the two last padic numbers are equal and the periodic length is 1 and pointing to the last padic nr in the vector.
                  -- If av(last_index) = av(last_index - 1) and Last_index - First_index = 0 then period sequence starts at pos Last_index
                 av(i) := tmp; -- tmp contain the start number of the incoming periodic sequence that shall keep the periodic start and end position.
               elsif Last_per_pos = First_per_pos and Last_per_pos = 1 then -- handle special case of Hansel vector length of 1 and period = 1
                  av(i+1) := MaxInteger(First_per_pos) + 1; -- new start pos of periodic part
                  av(0) := MaxInteger(Last_per_pos) + 1; -- new end pos of periodic part
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
         put_line("-- Encode6 : base_p_vector_integer_part : Number = "&Number'Image&" p = "&p'Image&" n = "&n'Image);

         for k in 0..n loop
            vec(k+1) := (Number/prime**(n-k)) mod prime;
            index := k+1;
            Put_line("-- vec("&index'Image&") = "&vec(k+1)'Image);
         end loop;

      end base_p_vector_integer_part;

      procedure reverse_base_p_vector_integer_part(Num : in MaxInteger; p : in MaxInteger; vec : in out Henselvec; logg : Boolean) is
         k, index : Integer := 0;
         Number : MaxInteger := abs(Num);
         n : Integer := floor(Float(log(float(Number),float(prime)))); -- Corrected 190324 use floor() instead of Integer()
         Prime : MaxInteger := p;
      begin
         put_line("-- Encode6 : reverse_base_p_vector_integer_part : Number = "&Number'Image&" p = "&p'Image&" n = "&n'Image,logg);

         for k in 0..n loop
            vec(n-k+1) := (Number/prime**(n-k)) mod prime;
            index := n-k+1;
            Put_line("-- vec("&index'Image&") = "&vec(n-k+1)'Image,logg);
         end loop;

      end reverse_base_p_vector_integer_part;

      procedure shift_vector(j : in integer; vec : in out Henselvec; logg : Boolean) is
         -- <= check:190317 length of remaining nr of hensel numbers after shifting and extend to same length as before shift, using periodic numbers
         First_per_pos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         Last_per_pos : Integer := Integer(av(0)); -- end position of incoming part
         period : Integer := Last_per_pos - First_per_pos + 1;
      begin
         Put("-- Encode6 : Encoded P-adic sequence to be shifted: ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
         Put_line("-- Encode6 : First_pos = "&First_per_pos'Image&" Last_pos = "&Last_per_pos'Image,logg);
         Put_Line("-- Encode6 : Postions to shift = "&j'Image,logg);
         vec(Last_per_pos+j+1) := MaxInteger(First_per_pos) + MaxInteger(j); -- Move indicator of first pos of periodic to new last position.

         -- move periodic hensel digits first
         for i in 1..period loop
            vec(Last_per_pos + j - i + 1) := vec(Last_per_pos - i + 1); -- move from back end to new position
         end loop;

         if First_per_pos > 1 then
           for i in 1..First_per_pos-1 loop
              vec(First_per_pos+j-i) := vec(First_per_pos - i); -- move from back end to new position
           end loop;
         end if;

         for i in 1..j loop
            vec(i) := 0; -- fill integers parts with zeroes for postions that are less or equal than the shifted
         end loop;

         vec(0) := MaxInteger(Last_per_pos) + MaxInteger(j); -- new last position of the periodic sequence
         Put("-- Encode6 : Encoded P-adic sequence shifted: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
      end shift_vector;

      procedure scale_shift_vector(j : in integer; vec : in out Henselvec) is --190407 New procedure for left shift of common denominators when scaling into [-1,0) interval.
         -- <= check:190317 length of remaining nr of hensel numbers after shifting and extend to same length as before shift, using periodic numbers
         First_per_pos : Integer := Integer(av(Integer(av(0))+1)); -- start pos of periodic part of incoming vector
         Last_per_pos : Integer := Integer(av(0)); -- end position of incoming part
         period : Integer := Last_per_pos - First_per_pos + 1;
         tmp : MaxInteger;
      begin
         Put("-- Encode6 : Encoded P-adic sequence to left shifted: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
         Put_line("-- Encode6 : First_pos = "&First_per_pos'Image&" Last_pos = "&Last_per_pos'Image,logg);
         Put_Line("-- Encode6 : Postions to rotate left = "&j'Image,logg);

         if period > 1 then
           for i in 1..j loop  -- rotation steps j and can be > period
              for k in 1..period-1 loop
                 tmp := vec(k);  -- tmp := vec(1), vec(2), ,,,, vec(period-1)
                 vec(k) := vec(k+1); -- vec(1) := vec(2),,,, vec(period-1) := vec(period);
                 vec(k+1) := tmp; -- vec(period) := vec(1);
              end loop;
            end loop;
         end if;

         Put("-- Encode6 : Encoded P-adic sequence left rotated: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
      end scale_shift_vector;


      function prime_k_mod(num : in MaxInteger; p : in MaxInteger; logg : Boolean) return Integer is
         k : Integer := 0;
         prime : MaxInteger := p;
      begin
         while num mod prime**k = 0 loop
            Put_line("--  "&num'image&" mod prime**"&k'image,logg);
            k := k + 1;
         end loop;
         return k - 1;
      end prime_k_mod;

      function find_truncation_sum( prime : in MaxInteger; m : in MaxInteger; vec : in out Henselvec; j : out Integer) return MaxInteger is  -- does only work with purely periodic hensel vectors as input.
         found : boolean := false;
         s, sum : MaxInteger := 0; -- sum up the vector digits using the repeated sequence and identfy the position j where the sum > m.
         -- n : Integer := Integer(log(float(m),float(prime)))-1; -- Floor !!! Does not work for the for loop n is not used !!!!!!!!!
         FirstPerPos : Integer := Integer(av(Integer(av(0))+1)); -- The postion after last period postion, contain the start pos of the periodic sequence.
         LastPerPos : Integer := Integer(av(0)); -- zero position of incoming Hensel vector vec contain last position of the periodic sequence.
         k : Integer := 0;
         period : Integer := LastPerPos - FirstPerPos + 1; -- For Q(prime = 3): 1/2 => .1 with period 1. First_pos_period = Last_pos_period.
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

      function CommonFactors(A : in out Rational; num_p_factor, den_p_factor : in out MaxInteger; k : out Integer;
                             p : MaxInteger; Numerator_p_factor, Denominator_p_factor : in out Boolean; logg : boolean) return boolean is
         prime : MaxInteger := MaxInteger(p);
      begin
        num_p_factor := GCD(Numerator(A),prime);
        Numerator_p_factor := (num_p_factor > 1);
        den_p_factor := GCD(Denominator(A),prime);
        Denominator_p_factor := (den_p_factor > 1);
        if Numerator_p_factor then
           Put_line("-- Encode6 : Common p with numerator",logg);
           k := prime_k_mod(Numerator(A),prime,logg);
           Put_line("-- Encode6 : k = "&k'Image,logg);
           num_p_factor := prime**k;
           A.Numerator := A.Numerator / num_p_factor;  -- remember after encoding
        elsif Denominator_p_factor then
           Put_line("-- Encode6 : Common p with Denominator",logg);
           k := prime_k_mod(Denominator(A),prime,logg);
           Put_line("-- Encode6 : k = "&k'Image,logg);
           den_p_factor := prime**k;
           A.Denominator := A.Denominator / den_p_factor;  -- remember after encoding
         end if;
         return Numerator_p_factor or Denominator_p_factor;
      end CommonFactors;


   begin -- encode6
         -- 190707 identify if encode is expecting a fixed length by comparing incoming Hcode.MaxLen with the calculated MaxLen in this procedure
         -- 190707 if Hcode.MaxLen < floor(log(float(Max_Int))/log(float(prime)))-1 then a fixed length HenselCode is expected. Adjust the varable record before return.
         -- 190707 Added boolean FixLength in declarations above.

      if (ain /= 0) and (bin /= 0) and is_Prime(prime) then
         Hcode.value := Reduction((ain,bin)); -- 190813 Store rational in Henselcode record for use with validation.
         -- Put_Line("-- **** Start Encode 6 ****",logg);
         -- Check 190404 special case when negating Hensel vector with length 1 !!!!!! padic 1 3 7 and padic 1 21 7 Corrected 190405 in negate vector !
         Put_Line("-- Encode6 : Correct input:  b /= 0 and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
         if HenselOverflow then
            Put_Line("-- Encode6 : HenselOverflow : ain = "&ain'Image&" bin = "&bin'Image&" prime = "&prime'Image,True);
            Put_line("-- Encode6 : HenselOverflow : prime < sqrt(Max_Int) = "&sqrt(Float(Max_Int))'Image&" Length < Max_Int + 2 - prime = "&Max_Int'Image&" + 2 - "&prime'Image&" MaxLen = "&MaxLen'Image,True);
         end if;
         Put_line("-- Encode6 : padic_ordinal_rational = "& n'Image&" r_negative = "&r_negative'Image&" r_between_neg_1_and_zero = "&r_between_neg_1_and_zero'Image,logg);
         Put_line("-- Encode6 : r_less_than_neg_1 = "&r_less_than_neg_1'Image,logg);
         Put_Line("-- Encode6 : a_num_p_factor = "&a_num_p_factor'Image&" alfa_Numerator_p_factor = "&alfa_Numerator_p_factor'Image,logg);
         Put_line("-- Encode6 : a_den_p_factor = "&a_den_p_factor'Image&" alfa_Denominator_p_factor = "&alfa_Denominator_p_factor'Image,logg);
         Put_line("-- Encode6 : absprime = "&Image(absprime(alfa,prime)),logg);
         Put_line("-- Encode6 : Numerator(alfa) = "& Numerator(alfa)'Image&" Denominator(alfa) = "&Denominator(alfa)'Image,logg);
         Put_Line("-- Encode6 : FixLength is required = "&UseFixLength'Image&" FixLength = "&FixLength'Image,logg);
         Put_Line("-- Encode6 : av'Length = "&av'Length'Image&" Hcode.vec'Length = "&Hcode.vec'Length'Image&" Hcode.size = "&Hcode.size'Image,logg);
         Hcode.MaxLen := MaxLen;  -- Use calculated MaxLen internally in encode6 function.

         if CommonFactors(alfa,a_num_p_factor,a_den_p_factor,k,prime,alfa_Numerator_p_factor,alfa_Denominator_p_factor,logg) then
            -- make sure to separate from other commonfactors examination later during scaling into interval [-1,0) if used !!!

            if alfa_Numerator_p_factor then
               Put_line("-- Encode6 : num_p_factor = "&a_num_p_factor'Image,logg);
               Hcode.NumFactor := a_num_p_factor; -- save in HenselCode record for use at decoding.
            end if;

            if alfa_Denominator_p_factor then
               Put_line("-- Encode6 : den_p_factor = "&a_den_p_factor'Image,logg);
               Hcode.DenFactor := a_den_p_factor; -- save in HenselCode record for use at decoding.
            end if;

         end if;
         -- 190409 Check padic_ordinal_rational((ain,bin),prime); after common factors
         Put_line("-- Encode6 : Adjusted after Common factors : padic_ordinal_rational = "& padic_ordinal_rational(alfa,prime)'Image,logg);
         Put_line("-- Encode6 : absprime = "&Image(absprime(alfa,prime)),logg);

         -- Check 190405: handle case when Commonfactors result in (1,1) ie both Num and Den can be reduced to (1,1) given the provided prime !!!!
         if alfa /= (1,1) and alfa /= (-1,1) then
            Put("-- Encode6 : Adjusted input:  b /= 0 and GCD("& alfa.Numerator'Image &","& alfa.Denominator'Image&") = "&MaxInteger'Image(GCD(alfa.Numerator,alfa.Denominator)),logg);
            Put(" and GCD("& alfa.Numerator'Image &","& prime'Image &") = "&MaxInteger'Image(GCD(alfa.Numerator,prime)),logg);
            Put(" and GCD("& alfa.Denominator'Image&","& prime'Image &") = "&MaxInteger'Image(GCD(alfa.Denominator,prime)),logg);
            Put_line(" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);

            -- Case 0 : r is a positive integer. The padic expansion is the reverse base p expansion.
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
            Put_line("-- Encode6 : padic_ordinal_rational = "&padic_ordinal_rational(alfa,prime)'Image&" r_negative = "&r_negative'Image&" r_between_neg_1_and_zero = "&r_between_neg_1_and_zero'Image,logg);
            Put_line("-- Encode6 : Adjusted common factors: Numerator(alfa) = "& Numerator(alfa)'Image&" Denominator(alfa) = "&Denominator(alfa)'Image,logg);

            if alfa.Denominator = 1 then -- r is integer
               Put_line("-- Encode6 : Case 0 or 1: alfa.Denominator = 1",logg);
               -- 190328: alfa := - r and there is a j >= 1 such that r < prime**(j) then  alfa = -r = (prime**(j)-r) - prime**(j).
               -- Then prime(j) - r is an integer in {1, ..., prime(j) - 1} we can write in base prime as c0 +...+ c(j-1)*prime**(j-1) then
               -- for j > log(r)/log(prime), alfa = sum(i=0,j-1,c(i)*prime**(i)) + sum(i>=j,(prime-1)*prime**(i).
               -- 190329: Make sure that the periodic part has the proper position at encoding of negative integer.
               -- Ex is -77 => ( 1 1 2, 2) correct ? Check j = 4 then the 2 should be on pos 4 !!!
               -- 190422 We need an adjustment of the hensel code from ( 1 1 2, 2) to ( 1 1 0 0 2, 4) Check -77 = 1*3**0+1*3**1+0*3**2+0*3**3 + 2*(3**4)/(1-3**1) = 4 - 81 = -77 Correct
               -- 190422 Check <prime 5> -77 => (3 4 1 4, 3) j = 3  : -77 = 3*5**0+4*5**1+1*5**2 + 4*(5**3/(1-5**1) =? 3+20+25 - 125 = -77 Correct.
               Put_line("-- Encode6 : Input Numerator(alfa) = "&Numerator(alfa)'Image,logg);

               if r_negative then  -- Case 1 Negative -- handle case (-1,1) !!!

                  j := Ceiling(Float(Float(log(Float(abs(alfa.Numerator))))/Float(log(Float(prime))))); -- Find the postion of periodic part to be used to set prime
                  Put_Line("-- Encode6 : Before Adjustment Numerator(alfa) = "&MaxInteger'Image(Numerator(alfa)),logg);

                  alfa.Numerator := prime**(j) + alfa.Numerator; -- adjusted numerator corresponding to p**j - abs(r).
                  Put_line("-- Encode6 : Adjusted with j = "&j'Image&" Adjusted Numerator(alfa) = "&Numerator(alfa)'Image,logg);
                  reverse_base_p_vector_integer_part(Numerator(alfa),prime, av,logg); -- adjusted part
                  dot_pos := floor(Float(log(float(abs(Numerator(alfa))),float(prime))))+1; -- set dot_pos after adjustment.
                  av(dot_pos+1) := prime - 1; -- all periodic digits are set to (prime-1) with period 1
                  av(0) := MaxInteger(dot_pos+1); -- set last pos including space for the periodic digit
                  av(Integer(av(0))+1) := MaxInteger(dot_pos+1); -- the last hensel digit is the periodic one with period 1

                  if j > dot_pos then -- Adjust vector - padding zeroes needed.
                     av(0) := MaxInteger(j)+1; -- new last pos = j + 1
                     av(Integer(av(0))+1) :=  MaxInteger(j)+1; -- new periodic postion = old pos + j - 1
                     av(j+1) := prime - 1;
                     for i in dot_pos+1..j loop
                        av(i) := 0;
                     end loop;
                     dot_pos := j;
                  end if;

                  Put("-- Encode6 : Case 1 r_negative before adjustment : Hensel vector = ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);

                  -- 190422: shift_vector(j-dot_pos,av); -- shift j-dot_pos dot positions, replaced by : Adjust vector - padding zeroes needed.

                  Put("-- Encode6 : Case 1 r_negative after adjustment  : Hensel vector = ",logg); Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
                  Hcode.dot_pos := dot_pos;
                  Hcode.LastPos := dot_pos+1;
                  Hcode.FirstPerPos := dot_pos+1;
                  Hcode.LastPerPos := dot_pos+1;
                  Hcode.vec := av;
                  Hcode.RecoveryLength := dot_pos+1;
                  Hcode.CompletePeriod := True; -- 190707 Added to support identification of complete period
                  found := true;
                  Hcode.ordinal := padic_ordinal_rational(Reduction((ain,bin)),prime); --190429 Added to make floting point Hensel code of negative integer
               else -- Case 0 : r postive integer  -- Handle case alfa = (1,1)  !!!
                  reverse_base_p_vector_integer_part(Numerator(alfa),prime, Hcode.vec,logg); -- 190430 Test use Hcode.vec direcly instead of local varable av
                  dot_pos := floor(Float(log(float(abs(Numerator(alfa))),float(prime))))+1;
                  Hcode.vec(0) := MaxInteger(dot_pos);  -- 190430 Use Hcode.vec(i) drectly instead of local varable av(i)
                  Hcode.vec(dot_pos + 1) := 0; -- set FirstPerPosition = 0 to indicate no fractional part
                  Put("-- Encode6 : Case 0 not r_negative : Hensel vector = ",logg); Put_Hensel_vector(Hcode.vec,dot_pos,logg); Put_line(" ",logg);
                  -- all hensel numbers before lead to dot_pos = Last_pos
                  Hcode.dot_pos := dot_pos;
                  Hcode.LastPos := dot_pos;
                  Hcode.FirstPerPos := 0;
                  Hcode.LastPerPos := dot_pos; -- 190430 Missing line before this date
                  -- Hcode.vec := av; 190430 remove use of av(i) for Case 0 not r_negative
                  Hcode.NoPeriod := (Hcode.FirstPerPos = 0);
                  found := true;
                  Hcode.ordinal := padic_ordinal_rational(Reduction((ain,bin)),prime); --190429 Added to make floting point Hensel code of positive integer
               end if;

               if decode4(Hcode, logg) = Reduction((ain,bin)) then
                  Put_Line("-- Encode6 : check with Decode4 : rational = "&Image(Reduction((ain,bin))),logg);
                  return found;  -- 190411 Test decode to establish if prime and a/b works and report that prime can's be used for this a/b.
               else
                  Put_line("-- Encode6 : could not find a Hensel vector with prime = "&Hcode.Prime'Image,logg);
                  return False;
               end if;

            elsif alfa.Denominator /=1 then -- r is a rational

               if r_between_neg_1_and_zero and GCD(alfa.Numerator,prime) = 1 and GCD(alfa.Denominator,prime) = 1 and GCD(alfa.Numerator,alfa.Denominator) = 1 then
                  -- Case 2 : r is a rational with r in [-1,0) and GCD(ain,p) = 1, GCD(bin,p) = 1, GCD(ain,bin) = 1 then p-adic expansion is purely periodic.
                  Put_line("-- Encode6 : Case 2: r_between_neg_1_and_zero",logg);
                  create_pure_periodic_vector(alfa, prime, av, alfa_arr,logg);
                  Hcode.CompletePeriod := member(alfa,alfa_arr); -- 190707 Added to support identification of complete period
                  found := true;
                  dot_pos := 0;
                  Hcode.dot_pos := dot_pos;
               elsif r_between_neg_1_and_zero and GCD(alfa.Numerator, prime) > 1 then  -- Common factors already taken care in CommonFactors above. This code should be unreachable.
                  -- Case 3 : r is a rational with GCD(ain,p) > 1 then write r = p**(k)*u. u is in Zp, Then u = r/p**(k) is rational.
                  --          Use Case 2 expansion for u and right shift k steps.
                  Put_line("-- Encode6 : Case 3: r_between_neg_1_and_zero with common numerator factor",logg);
                  create_pure_periodic_vector(alfa, prime, av, alfa_arr,logg);
                  shift_vector(k,av,logg);  -- k calculated up front in Commonfactors
                  Hcode.CompletePeriod := member(alfa,alfa_arr); -- 190707 Added to support identification of complete period
                  found := true;
                  dot_pos := k;
                  Hcode.dot_pos := dot_pos;
               elsif  r_less_than_neg_1 then --Case 4_5
                  -- Case 4_5 : r is rational with r.Denominator /= 1 and r < -1. Use invmod method in create_vector, therefore Case 5 can be combined with 4.
                  CASE4_5 : declare
                     A : Float := Float(Float(Numerator(alfa))/Float(Denominator(alfa)));
                  begin
                     Put_line("-- Encode6 : Case 4_5: r_less_than_neg_1",logg);
                     create_vector(alfa, prime, av, alfa_arr,logg);
                     Hcode.CompletePeriod := member(alfa,alfa_arr); -- 190707 Added to support identification of complete period
                     Hcode.RecoveryLength := (if Integer(av(0)) < MaxLen then Integer(av(0)) else MaxLen);  -- set recovery length with restrictions to MaxLen
                     dot_pos := Integer(av(Integer(av(0))+1))-1; -- set dot_pos = First_per_pos-1
                     Hcode.dot_pos := dot_pos;
                     found := true;
                     Hcode.MaxLen := MaxLen;

                  end CASE4_5;

               end if;
               Put("-- Encode6 : Encoded P-adic sequence before negation: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);
               if not r_negative then
                     negate_vector(av);
               end if;
               Put("-- Encode6 : Encoded P-adic sequence after negation: ",logg);Put_Hensel_vector(av,dot_pos,logg); Put_line(" ",logg);

               -- Put_line("Scaling and negation failed: dot position = "&n'Image&" scaling N = "& N'Image&" r_negative = "&r_negative'Image);

               -- r : Integer := Integer(Float(2)*Log(Float(MAX(a,b))/Float(0.618))/Log(Float(p))); -- estimate size of Henselvec
               Put_line("-- Encode6 : Estimated size of Henselvec for correct recovery : "&
                         Integer(Float(2)*Log(Float(MAX(alfa.Numerator,alfa.Denominator))/Float(0.618))/Log(Float(Integer(prime))))'Image,logg);
               Put_line("-- Encode6 : Hcode.RecoveryLength = "&Hcode.RecoveryLength'Image,logg);
               Hcode.vec := av;
               Hcode.FirstPerPos := Integer(av(Integer(av(0))+1));
               Hcode.LastPerPos := Integer(av(0));
               Hcode.dot_pos := dot_pos;
               Hcode.ordinal := padic_ordinal_rational(Reduction((ain,bin)),prime); -- 190430 Introduced to support use of ordinal for r is a rational
               Put_line("-- Encode6 : PossibleDecode is "&PossibleDecode(alfa,Hcode)'Image,logg);

               if decode4(Hcode,logg) = Reduction((ain,bin)) then
                  Put_Line("-- Encode6 check with Decode4 : rational = "&Image(Reduction((ain,bin))),logg);
                  return found;  -- 190411 Test decode to establish if prime and a/b works and report that prime can's be used for this a/b.
               else
                  New_Line; Put_line("-- Encode6 : Error - could not find a Hensel vector with prime = "&Hcode.Prime'Image,True);
                  return False;
               end if;

            end if;

            -- Unreachable code
            Put_line("-- Encode6 : Error in finding hensel code",logg);
            return false;
         else -- alfa = (1,1) or alfa = (-1,1), 190630 or alfa = (0,1)

            if alfa = (1,1) then
               Put_line("-- Encode6 : Case 0 alfa = (1,1)",logg);
               Hcode.vec(0) := 1; -- 190430 Test use of Hcode directly instead of using av(i) the local vector !!!
               Hcode.vec(2) := 0; -- First_per_pos = 0 for positive integer
               Hcode.vec(1) := 1;
               dot_pos := 0; Hcode.dot_pos := 0;
               Hcode.FirstPerPos := 0; -- Indicate no fractional part
               Hcode.LastPerPos := 1;
               Hcode.RecoveryLength := 0; -- Indicate nothing to recover
               Hcode.NoPeriod := true;
               Hcode.ordinal := padic_ordinal_rational(Reduction((ain,bin)),prime); -- 190430 Introduced to support use of ordinal for (1,1) case
            elsif alfa = (-1,1) then
               Put_line("-- Encode6 : Case 0 alfa = (-1,1)",logg);
               Hcode.vec(0) := 1; -- 190430 Test use of Hcode directly instead of using av(i) the local vector !!!
               Hcode.vec(2) := 1; -- First_per_pos = 1 for negative integer
               Hcode.vec(1) := prime - 1;
               -- 190804 Corrected to 'prime - 1' from '1'.
               -- 190804 encoding of -1/1 for prime = 3,5,7, ... did not work properly.
               -- Padic.adb : argv = -e -1 1 3  argc =  4
               -- Padic.adb -e: Encoded6 P-adic sequence: [ 1 , 1 ]( 1 ,  0 ))( 3 )
               -- Padic.adb -e: EncodeFixHensel : [ 11 ]( 1 1 0 0 0 0 0 0 0 0 0 ,  0 )( 3 )
               -- Padic.adb -e: Decode4 of Encoded6: Decoded rational : -1/ 2
               -- Padic.adb -e: Decode4 of EncodeFixHensel : -1/ 2
               -- Padic.adb -e: Encoded6 rational - Decode4(EncodeHensel) rational : -1/ 1 - -1/ 2 = -1/ 2
               -- Padic.adb -e: Encoded6 rational - Decode4(EncodeFixHensel) rational : -1/ 1 - -1/ 2 = -1/ 2
               -- 190804 encoding of -1/1 for prime = 3,5,7, ... did not work properly.
               -- Padic.adb : argv = -e -1 1 5  argc =  4
               -- Padic.adb -e: Encoded6 P-adic sequence: [ 1 , 1 ]( 1 ,  0 ))( 5 )
               -- Padic.adb -e: EncodeFixHensel : [ 11 ]( 1 1 0 0 0 0 0 0 0 0 0 ,  0 )( 5 )
               -- Padic.adb -e: Decode4 of Encoded6: Decoded rational : -1/ 4
               -- Padic.adb -e: Decode4 of EncodeFixHensel : -1/ 4
               -- Padic.adb -e: Encoded6 rational - Decode4(EncodeHensel) rational : -1/ 1 - -1/ 4 = -3/ 4
               -- Padic.adb -e: Encoded6 rational - Decode4(EncodeFixHensel) rational : -1/ 1 - -1/ 4 = -3/ 4
               dot_pos := 0; Hcode.dot_pos := 0;
               Hcode.FirstPerPos := 1; -- Indicate fractional part (treated as negative integer in combination with NoPeriod at decoding)
               Hcode.LastPerPos := 1;
               Hcode.RecoveryLength := 0; -- Indicate nothing to recover
               Hcode.NoPeriod := true;
               Hcode.ordinal := padic_ordinal_rational(Reduction((ain,bin)),prime); -- 190430 Introduced to support use of ordinal for (-1,1) case
            elsif alfa = (0,1) then -- 190630 Branch added to take care of zero input.
               Put_line("-- Encode6 : Case 0 alfa = (0,1)",logg);
               Hcode.vec(0) := 1;
               Hcode.vec(2) := 0; -- First_per_pos = 0 for positive integer
               Hcode.vec(1) := 0;
               dot_pos := 0; Hcode.dot_pos := 0;
               Hcode.FirstPerPos := 0; -- Indicate no fractional part
               Hcode.LastPerPos := 1;
               Hcode.RecoveryLength := 0; -- Indicate nothing to recover
               Hcode.NoPeriod := true;
            end if;
            Put_Line("-- Encode6 : rational = "&Image(Reduction((ain,bin))),logg);
            return True;
         end if;
      else
         Put_line("-- Encode6 : Incorrect input : bin = "& bin'Image &" and "& prime'Image &" is a "& is_Prime(prime)'Image &" prime",logg);
         return False;
      end if;
   end encode6;
