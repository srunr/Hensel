with Text_IO; use Text_IO;
with Generic_Rational.supporting_functions;
package body Generic_Rational is
   package sfunc is new Generic_Rational.supporting_functions; use sfunc;

   function Inverse (A : Rational) return Rational is
   begin
      if A.Numerator > 0 then
         return (A.Denominator, A.Numerator);
      elsif A.Numerator < 0 then
         return (-A.Denominator, -A.Numerator);
      else
         raise Constraint_Error;
      end if;
   end Inverse;

   function "abs" (A : Rational) return Rational is
   begin
      return (abs A.Numerator, A.Denominator);
   end "abs";

   function "+" (A : Rational) return Rational is
   begin
      return A;
   end "+";

   function "-" (A : Rational) return Rational is
   begin
      return (-A.Numerator, A.Denominator);
   end "-";

   function "+" (A : Rational; B : Rational) return Rational is
      Common        : constant Number := GCD (A.Denominator, B.Denominator);
      A_Denominator : constant Number := A.Denominator / Common;
      B_Denominator : constant Number := B.Denominator / Common;
   begin
      return Reduction((A.Numerator * B_Denominator + B.Numerator * A_Denominator) /
                       (A_Denominator * B.Denominator));
      exception when Constraint_Error =>
        New_line; Put_line("-- Generic_Rational : Constraint_Error in R+R "&Image(A)&" * "&Image(B));
        return (1,1);
   end "+";

   function "+" (A : Rational; B : Number) return Rational is
   begin
      return Reduction((A.Numerator + B * A.Denominator) / A.Denominator);
   end "+";

   function "+" (A : Number; B : Rational) return Rational is
   begin
      return Reduction(B + A);
   end "+";

   function "-" (A : Rational; B : Rational) return Rational is
   begin
      return Reduction(A + (-B));  -- is this really correct  ??
   end "-";

   function "-" (A : Rational; B : Number) return Rational is
   begin
      return Reduction(A + (-B));
   end "-";

   function "-" (A : Number; B : Rational) return Rational is
   begin
      return Reduction(A + (-B));
   end "-";

   function "*" (A : Rational; B : Rational) return Rational is
   begin
      return Reduction((A.Numerator * B.Numerator) / (A.Denominator * B.Denominator));
   exception when Constraint_Error =>
      New_line; Put_line("-- Generic_Rational : Constraint_Error in R*R "&Image(A)&" * "&Image(B));
      return (1,1);
   end "*";

   function "*" (A : Rational; B : Number) return Rational is
      Common : constant Number := GCD (A.Denominator, abs B);
   begin
      return Reduction((A.Numerator * B / Common, A.Denominator / Common));
   end "*";

   function "*" (A : Number; B : Rational) return Rational is
   begin
      return Reduction(B * A);
   end "*";

   function "/" (A : Rational; B : Rational) return Rational is
   begin
      return Reduction(A * Inverse (B));
   end "/";

   function "/" (A : Rational; B : Number) return Rational is
      Common : constant Number := GCD (abs A.Numerator, abs B);
      A_Numerator : constant Number := A.Numerator / Common;
      B_inv : constant Rational := (1 , B / Common);

   begin
      --Put_Line(" func / Common = "& Common'Image &" A.Numerator = "& A.Numerator'Image &" B = "& B'Image &" A.Denominator = "& A.Denominator'Image);
      if B = 0 then
         raise Constraint_Error;
      elsif A.Numerator = 0 then
         return (0, 1);
      elsif A_Numerator > 0 xor B > 0 then
         return (-(abs (A_Numerator, A.Denominator)) * B_inv);
      else
         return ((A_Numerator, A.Denominator) * B_inv);
      end if;
   end "/";

   function "/" (A : Number; B : Rational) return Rational is
   begin
      return Reduction(Inverse (B) * A);
   end "/";

   function "/" (A : Number; B : Number) return Rational is
      Common : constant Number := GCD (abs A, abs B);
   begin
      if B = 0 then
         raise Constraint_Error;
      elsif A = 0 then
         return (0, 1);
      elsif A > 0 xor B > 0 then
         return Reduction((-(abs A / Common), abs B / Common));
      else
         return Reduction((abs A / Common, abs B / Common));
      end if;
   end "/";

   function ">" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function ">" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function ">" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function "<" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";

   function "<" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";

   function "<" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";

   function ">=" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function ">=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function ">=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function "<=" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "<=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "<=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator = 0;
   end "=";

   function "=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator = 0;
   end "=";

   function "=" (A , B : Rational) return Boolean is   -- Added 190407
      Diff : Rational := A + (-B); -- 190630 Testing better criteria
   begin
      -- Diff := A.Numerator*B.Denominator-B.Numerator*A.Denominator; -- 190410 Updated. 190411 moved inside begin .. end.
      return Diff.Numerator = 0;
   exception when Constraint_Error =>
      New_line; Put_line("-- Generic_Rational : Constraint_Error in R=R "&Image(A)&" = "&Image(B));
      return False;
   end "=";

   function Numerator (A : Rational) return Number is
   begin
      return A.Numerator;
   end Numerator;

   function Denominator (A : Rational) return Number is
   begin
      return A.Denominator;
   end Denominator;

   function Image(A : Rational) return String is
   begin
      return Number'Image(Numerator(A))&"/"&Number'Image(Denominator(A));
   end Image;

end Generic_Rational;
