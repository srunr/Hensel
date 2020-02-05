separate (padic_numbers)
function ResidualDecode(hcodePtrArr : HenselCodePtrArr; logg : Boolean) return Rational is
   R : rational := (0,1);
   MaxLen : Integer := hcodePtrArr(1).MaxLen;
   i : Integer := 0; -- 190816 index for eucludian while loop, 190819 Must be set to 0 initially.

   function inv_mod (a : MaxInteger; n : MaxInteger) return MaxInteger with post=> (a * inv_mod'Result) mod n = 1 is
      -- To calculate the inverse we do as if we would calculate the GCD with the Euclid extended algorithm
      -- (but we just keep the coefficient on a)
      r : Maxinteger;
      function inverse (a, b, u, v : MaxInteger) return MaxInteger is
        (if b=0 then u else inverse (b, a mod b, v, u-(v*a)/b));
   begin
      r := inverse (a, n, 1, 0);
      while r < 0 loop r := r + n; end loop;
      return r;
   end inv_mod;

   function Pmult(ResPrime : PrimeArr) return MaxInteger is
      p : MaxInteger := 1;
   begin
      for i in 1..ResPrime'Length loop
         p := p*ResPrime(i); Put_Line("-- ResidualDecode : Pmult : p = "&p'image);
      end loop;
      Put_Line("-- ResidualDecode : PP = "&p'Image);
      return p;
   end Pmult;

   type PprimArr is array(1..ResidualPrimes'Last) of MaxInteger;
   PP : MaxInteger := Pmult(ResidualPrimes);

   function FindPiPrim(ResPrime : PrimeArr; Pproduct : MaxInteger ) return PprimArr is
      PprimResult : PprimArr := (others => 1);
      ptmp : MaxInteger;
   begin
      for i in ResPrime'Range loop
         -- ((Numerator(alfa) mod prime)*(invmod(Denominator(alfa), prime))) mod prime;
         -- PprimResult(i) := (invmod(Pproduct/ResPrime(i),ResPrime(i));
         ptmp := Pproduct/ResPrime(i); Put_Line("-- ResidualDecode : FindPiPrim : ptmp = "&ptmp'Image&" ResPrime("&i'Image&") = "&ResPrime(i)'Image);
         PprimResult(i) := inv_mod(ptmp,ResPrime(i)); Put_Line("-- ResidualDecode : PprimResult("&i'Image&") = "&PprimResult(i)'Image);
      end loop;
      Put("-- ResidualDecode : FindPiPrim : Pprim = "); for i in PprimResult'Range loop Put(PprimResult(i)'Image); end loop; new_line;
      return PprimResult;
   end FindPiPrim;

   PprimA : PprimArr := FindPiPrim(ResidualPrimes,PP);

   function initRForEachPrime(h : HenselCodePtr; ResPrime : MaxInteger) return MaxInteger is
      reach : MaxInteger := 0;
   begin
      Put("-- ResidualDecode : initRForEachPrime input HenselCodePtr = "); Put_Hensel_Code(h,True); Put_Line(" = "&Image(h.value),True);

      if h.LastPos = 1 then
         reach := (PP/ResPrime*inv_mod(PP/ResPrime,ResPrime)*((Numerator(h.value)*inv_mod(Denominator(h.value),ResPrime)) mod ResPrime)) mod PP;
      else
         for j in 1..h.LastPos loop
            reach := reach + h.vec(j)*ResPrime**(j-1); Put_Line("-- ResidualDecode : initRForEachPrime loop : reach = "&reach'Image&" h.vec("&j'Image&") = "&h.vec(j)'Image);
         end loop;
         reach:= reach*ResPrime**(h.ordinal+h.LastPos-1); -- 190917 taking care of point position for each prime
      end if;
      Put_Line("-- ResidualDecode : initRForEachPrime : h.ordinal = "&h.ordinal'Image);
      Put_Line("-- ResidualDecode : initRForEachPrime : reach(prime="&ResPrime'Image&") = "&reach'Image);
      return reach;
   end initRForEachPrime;

   function InitRAllPrime(HcodePtrArr : HenselCodePtrArr; PrimesMult : MaxInteger; PprimArray : PprimArr) return MaxInteger is
      r : MaxInteger := 0;
   begin
      for i in HcodePtrArr'Range loop
         declare
            htmpPtr : HenselCodePtr := HcodePtrArr(i);
            ri : MaxInteger := initRForEachPrime(htmpPtr,ResidualPrimes(i));
            tmp : MaxInteger := MaxInteger(float(PrimesMult)/float(ResidualPrimes(i))*float(PprimArray(i)));
         begin
            r := r + ((tmp*ri) mod PrimesMult);
            Put_Line("-- ResidualDecode : InitRAllPrime : tmp = "&tmp'Image&" ri = "&ri'Image&" tmp*ri = "&
                       MaxInteger(tmp*ri)'Image&" tmp*ri mod PrimesMult = "&MaxInteger(tmp*ri mod PrimesMult)'Image);
            -- PrimesMult/ResidualPrimes(i)*Pprim(i)*ri mod PrimesMult
         end;
      end loop;
      r := r mod PrimesMult;
      Put_Line("-- ResidualDecode : InitRAllPrime : r = "&r'Image);
      return r;
   end InitRAllPrime;

   type euclid_array is array(-1..MaxLen+5) of MaxInteger;

   u : euclid_array := (PP, InitRAllPrime(hcodePtrArr,PP,PprimA), others => 0);
   v : euclid_array := (0, 1, others => 0);
   q : euclid_array := (others => 0);
   m : MaxInteger := MaxInteger(sqrt(float(PP))); -- exit critera for while loop

begin -- ResidualDecode
   Put_line("-- ResidualDecode : u("&Integer(-1)'Image&") = "&u(-1)'Image&" u("&Integer(0)'Image&") = "&u(0)'Image&" m = "&m'Image,logg);
   Put_line("-- ResidualDecode : v("&Integer(-1)'Image&") = "&v(-1)'Image&" v("&Integer(0)'Image&") = "&v(0)'Image,logg);
   -- Dixon, reused from Decode2
   while abs(v(i)) < m and u(i) /= 0 loop
      q(i) := floor(Float(Float(u(i-1))/float(u(i)))); Put("-- ResidualDecode : q("& i'Image &") = floor("&Image((u(i-1),u(i)))&") = "& q(i)'Image,logg);
      u(i+1) := u(i-1) - q(i)*u(i); Put("  u("&Integer(i+1)'Image&") = "& u(i+1)'Image,logg);
      v(i+1) := v(i-1) + q(i)*v(i); Put("  v("&Integer(i+1)'Image&") = "& v(i+1)'Image,logg);
      i := i + 1; Put_Line("",logg);
   end loop;
   -- Num := MaxInteger(MaxInteger((-1)**(i-1))*u(i-1)); Put_line("-- Decode2 : Num = "& Num'Image,logg);
   -- Den := v(i-1); Put_line("-- Decode2 : Den = "& Den'Image,logg);
   R := Reduction((MaxInteger(MaxInteger((-1)**(i-1))*u(i-1)),v(i-1))); Put_Line("-- ResidualDecode : Result = "&Image(R));
   return Reduction(R);

end ResidualDecode;
