separate (padic_numbers)
function ResidualEncode(ain,bin : in MaxInteger; HenselCArray : in out HenselCodePtrArr;  FixLength : Integer; logg : Boolean) return boolean is
   -- 190815 function added to do residual encoding

   htmp : aliased HenselCode := (size => HenselCArray(1).MaxLen+3, vec => (0, others => -(HenselCArray(1).prime+1)), FirstPerPos => 0, LastPerPos => 0,
                                 LastPos => HenselCArray(1).MaxLen, dot_pos => 0, prime => HenselCArray(1).prime, RecoveryLength => HenselCArray(1).RecoveryLength, CompletePeriod => False,
                                 NoPeriod => False, DenFactor => 1, NumFactor => 1, MaxLen => HenselCArray(1).MaxLen, ordinal => 0, value => (0,1));
   r : Rational := (0,1);
   found : Boolean := False;

   type Rational_array is array(0..htmp.size+5) of Rational;
   alfa_arr : Rational_array:= (others => (0,1));

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

   procedure create_vector(alfa : in out Rational; p : in MaxInteger; av : in out Henselvec; alfa_arr : in out Rational_array; logg : Boolean) is
      a : MaxInteger;
      i : Integer := 1; -- index for  loop, save pos 0 for the pos of the periodic sequence
      prime : MaxInteger := p;
   Begin
      Put_Line("-- ResidualEncode : create_vector",logg);
      loop
         a:= ((Numerator(alfa) mod prime)*(invmod(Denominator(alfa), prime))) mod prime;
         Put("-- ResidualEncode : Iter : "& i'Image &" Module : "& a'Image,logg);
         alfa_arr(i) := alfa;
         Put(" Input fraction = "& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
         alfa := (alfa - a)/prime; Put_Line(" Fraction for next :"& Numerator(alfa)'Image &"/"& Denominator(alfa)'Image,logg);
         -- put_line("Alfa_arr("& i'Image&") = "& Numerator(alfa_arr(i))'Image &"/"& Denominator(alfa_arr(i))'Image);
         if i >= av'Length - 3 then  -- pos 0 indicate last pos for hensel digit and last pos indicate pos for first periodic digit, leaving av'Length - 3 as hensel digit length before negation.
            av(0):= MaxInteger(i); -- Last pos of sequence.
            av(i+1) := 1; -- Use next index to store the first position of the not completed periodic sequence. Use 1 for the uncomplete periodic sequence.
            av(i) := a; -- Last number in periodic sequence
            found := true; -- use up the available vector. May not complete the periodic period.
            Put_Line("-- ResidualEncode : create_vector : full period member NOT found",logg);
            exit;
         elsif  member(alfa,alfa_arr) then -- every p-adic sequence of a rational number is eventually periodic. See https://kconrad.math.uconn.edu/blurbs/gradnumthy/rationalsinQp.pdf
            av(0):= MaxInteger(i); -- Last pos of periodic sequence
            av(i+1) := Numerator(alfa_arr(0)); -- Use next index to store the first position of the periodic sequence. fetch pos from alfa_arr(0) resulting from member function.
            av(i) := a; -- Last number in periodic sequence
            found := true;
            Put_Line("-- ResidualEncode : create_vector : full period member found",logg);
            exit;
         end if;
         av(i) := a;  -- only update when confirmed to be inside period.
         i := i + 1;
      end loop;
      Put_Line("-- ResidualEncode : create_vector : member(alfa,alfa_arr) = "&member(alfa,alfa_arr)'Image,logg);
   end create_vector;


begin -- ResidualEncode
   for i in 1..HenselCodePtrArr'Length loop
      declare
         htmPtr : HenselCodePtr := HenselCArray(i);
         PrimeList : Number_List := Decompose(htmPtr.prime); -- 190818 Enable setting a list of primes in padic_numbers including k powers of the prime.
         PrimePower : Integer := PrimeList'Length;
         prime : MaxInteger := PrimeList(1);
      begin
         Put_Line("-- ResidualEncode : PrimePower = "&PrimePower'Image&" htmPtr.prime = "&htmPtr.prime'Image);
         if PrimePower > 1 then
            htmPtr.prime := prime;
            htmPtr.ordinal := PrimePower;
         else htmPtr.ordinal := htmp.ordinal;
         end if;
         Put_Line("-- ResidualEncode : htmPtr.prime = "&htmPtr.prime'Image);
         htmp := EncodeFixHensel(ain,bin,htmPtr.prime,FixLength,logg); -- 190819 We cant reuse encode6 for residual encoding. 190829 Now using encode7
         -- 190904 Not possible to reuse EncodeFixHensel as the decoding for each residual prime is irrelevant !!!!

         for j in 1..FixLength loop -- 190819 expand

            htmPtr.vec(j) := htmp.vec(j);

         end loop;
         htmPtr.FirstPerPos := 1;
         htmPtr.LastPerPos := FixLength;
         htmPtr.LastPos := FixLength;
         htmPtr.dot_pos := htmp.dot_pos;
         htmPtr.value := Reduction((ain,bin));
         Put_line("-- ResidualEncode HenselCarray("&i'image&").FirstPerPos = "&htmp.FirstPerPos'Image&" .LastPerPos = "&htmp.LastPerPos'Image&
                    " .LastPos = "&htmp.LastPos'Image&" .RecoveryLength = "&htmp.RecoveryLength'Image);
         Put("-- ResidualEncode HenselCarray("&i'image&") = "); Put_Fixed_Hensel_code(HenselCodeRecord => htmp,Length => FixLength,logg => True); New_line;
      end;
   end loop;

   r := ResidualDecode(HenselCArray,True); Put_line("-- RedidualDecode r = "&Image(r));

   Return True;
end ResidualEncode;
