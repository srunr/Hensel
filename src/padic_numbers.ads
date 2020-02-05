with System; use System;
With Ada.Numerics.Elementary_Functions;
with Ada.Command_Line;
With Generic_Rational;
with Generic_Rational.supporting_functions;
with Prime_Numbers;
with Ada.Text_IO;
package padic_numbers is
   use Ada.Numerics.Elementary_Functions;

   type MaxInteger is new Long_Long_Integer'Base range Long_Long_Integer'Base'First..Long_Long_Integer'Base'Last; -- Use max resources provided by SW and HW.

   package Integer_Rationals is new Generic_Rational(MaxInteger); use Integer_Rationals;
   package Supporting_Functions is new Integer_Rationals.supporting_functions;

   package Integer_Prime is new Prime_Numbers(Number => MaxInteger,
                                         Zero => 0, One => 1, Two => 2,
                                              Image => MaxInteger'Image);
   use Integer_Prime;

   -- According to A method for Hensel Code Overflow Detection by Xin Kau Li, Chao Lu Towson University and Jon A Sjögren, Air Force Office of scientific Research
   -- if prime**2 < MaxInt and length r of hensel vector r < MaxInt+2-prime the arithmetic of addition, subtraction. multiplication and division within the limit of MaxInt as Long_Long_Integer.
   -- http://www.jon-arny.com/httpdocs/Works/AMethodforHenselCodesOverflowDetection_Final.docx

   --  190504: According to "A new representation of the rational numbers for fast easy arithmetic" by Eric C.R. Hehner and R. N. S. Horspool the
   --  in section 7.1. See https://www.cs.toronto.edu/~hehner/ratno.pdf.
   --  The length of bounds of resulting encoded rational with base b, can be estimed for the operators +,-,* and / as follows
   --  b : the base of the representation. Ex in decimal with base = 10, 12'7 = 7-120/99 = 191/33.
   --  p(i) : the length of the positive part in the base p representation of x(i).
   --  n(i) : the length of the negative part in the base p representation of x(i).
   --  phi(m) is the Euler totient function
   --          operation      Length bounds
   --  x(3) := x(1) + x(2)    p(3) <= MAX(p(1),p(2)) + n(3) + 2
   --  x(3) := x(1) - x(2)    n(3) is a divisor of LCM(n(1),n(2))
   --  x(3) := x(1) * x(2)    p(3) <= p(1) + p(2) + n(3) + 1
   --                         n(3) is a divisor of LCM(n(1),n(2))
   --  x(3) := x(1) / x(2)    p(3) <= p(1) + p(2) + n(3) + 1 if p(2) <= n(2)
   --                         p(3) <= p(1) - p(2) + n(3) + 1 if n(2) < p(2) <= p(1)
   --                         n(3) is a divisor of LCM(n(1),phi(x(2)) * (b**n(2) - 1))
   --  Fact: Effective calculation of phi(x) requires prime factors of x.
   --
   --  200209 multiplication : p(1) = A.FirstPerPos - 1, p(2) = B.FirstPerPos - 1, n(1) := A.LastPerPos - A.FirstPerPos + 1, n(2) = B.LastPerPos - B.FirstPerPos + 1
   --  200209 multiplication : n(3) = Decompose(LCM(n(1),n(2)), To get maxumim length use largest divisor use Max(n(3)) = MaxDivisor(Decompose(LCM(n(1),n(2))))
   --  200209 multiplication : p(3) <= R.FirstPerPos <= MAX(A.FirstPerPos, B.FirstPerPos) + MaxDivisor(Decompose(LCM(A.LastPerPos - A.FirstPerPos + 1,B.LastPerPos - B.FirstPerPos + 1))) + 1

   function floor(A : in Float) return Integer;

   function floor(A : in Float) return MaxInteger;

   function Ceiling(A : in Float) return Integer;

   function Ceiling(A : in Float) return MaxInteger;

   type Henselvec is array (Integer range <>) of MaxInteger;

   primelistsize : constant Integer := 3;

   type PrimeArr is array(1..primelistsize) of MaxInteger;

   ResidualPrimes : PrimeArr := (3, 4, 5);

   type HenselCode;

   type HenselCodePtr is access all HenselCode;

   type HenselCodePtrArr is array(1..primelistsize) of HenselCodePtr;

   type HenselCode(size : Integer) is record
      vec : Henselvec(0..size);
      FirstPerPos : Integer := 0;
      LastPerPos : Integer := 0;
      LastPos : Integer := 0;
      dot_pos : Integer := 0;
      prime : MaxInteger := 0;
      RecoveryLength : Integer  := 0;
      CompletePeriod : Boolean := False;
      NoPeriod : Boolean := False;
      DenFactor : MaxInteger := 1;
      NumFactor : MaxInteger := 1;
      MaxLen : Integer;
      ordinal : Integer;
      value : Rational := (0,1);
   end record;

   -- type HenselArrCode is array(1..primelistsize) of HenselCode(size => 1);

   procedure Put_Hensel_vector(vec : in Henselvec; dot_pos : in Integer; logg : Boolean);

   procedure Put_Hensel_Code(HenselCodeRecord : in HenselCode; logg : Boolean);

   procedure Put_Hensel_Code(HenselCodeRecordPtr : in HenselCodePtr; logg : Boolean);

   procedure Put_Fixed_Hensel_code(HenselCodeRecord : in HenselCode; Length : in Integer; logg : Boolean);

   function padic_ordinal_integer(x : in MaxInteger; prime : in MaxInteger) return Integer;

   function padic_ordinal_rational(r : in Rational; prime : in MaxInteger) return Integer;

   function absprime(r : in Rational; prime : MaxInteger) return Rational;

   function decode2(h : in HenselCode; n0 : in Integer; logg : Boolean) return Rational;

   function decode4(h : in HenselCode; logg : Boolean) return Rational;

   function ResidualDecode(hcodePtrArr : HenselCodePtrArr; logg : Boolean) return Rational;

   function encode5(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer; logg : Boolean) return boolean;

   function encode6(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer;  FixLength : Integer; logg : Boolean) return boolean;

   function encode7(ain,bin : in MaxInteger; Hcode : in out HenselCode; dot_pos : out Integer;  FixLength : Integer; logg : Boolean) return boolean;

   function ResidualEncode(ain,bin : in MaxInteger; HenselCArray : in out HenselCodePtrArr;  FixLength : Integer; logg : Boolean) return boolean;

   function EncodeHensel(ain,bin : in MaxInteger; prime : in MaxInteger) return HenselCode;

   function EncodeFixHensel(ain,bin : in MaxInteger; prime : in MaxInteger; Length : in Integer; logg : in Boolean) return HenselCode;

   function EncodeVariableHensel(ain,bin : in MaxInteger; prime : in MaxInteger; FirstPerPos, LastPerPos : out Integer ) return HenselCode;

   function add( A , B : in HenselCode) return HenselCode;

   function add2( A , B : in HenselCode) return HenselCode;

   function add3( A , B : in HenselCode; logg : Boolean) return HenselCode;

   function sub( A , B : in HenselCode; logg : Boolean) return Henselcode;

   function mult( A , B : in HenselCode; logg : Boolean) return Henselcode;

   procedure Put_Line(Item : String; logg : boolean);

   procedure Put(Item : String; logg : boolean);

end padic_numbers;
