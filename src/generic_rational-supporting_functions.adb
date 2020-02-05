with Ada.Text_IO; Use Ada.Text_IO;
package body Generic_Rational.supporting_functions is

   function GCD (A, B : Number) return Number is  -- According to http://rosettacode.org/wiki/Greatest_common_divisor#Ada
      M : Number := A;
      N : Number := B;
      T : Number;
   begin
     while N /= 0 loop
        T := M;
        M := N;
        N := T mod N;
     end loop;
     return M;
   end GCD;

   function LCM (A, B : Number) return Number is
   begin -- According to http://rosettacode.org/wiki/Least_common_multiple#Ada
      if A = 0 or B = 0 then
         return 0;
      end if;
      return abs (A) * (abs (B) / GCD (A, B));
   end LCM;

   function Reduction( r : Rational) return Rational is
     g : Number := GCD(abs Numerator(r), Denominator(r));
   begin
     return (Numerator(r)/g, Denominator(r)/g);
   end  Reduction;

   function invmod (a : Number; n : Number) return Number is
    -- To calculate the inverse we do as if we would calculate the GCD with the Euclid extended algorithm
    -- (but we just keep the coefficient on a)
       function inversemod (a, b, u, v : Number) return Number is
       begin
          if b=0 then
             return u;
          else return inversemod (b, a mod b, v, u-(v*a)/b);
          end if;
       end inversemod;

   begin
      return inversemod (a, n, 1, 0);
   end invmod;

   function MAX( a : Number; b : Number) return Number is
   begin
      if a > b then
         return a;
      else
         return b;
      end if;
   end MAX;

   function MIN( a : Number; b : Number) return Number is
   begin
      if a < b then
         return a;
      else
         return b;
      end if;
   end MIN;

   function XGCD (A, B : Number) return Rational is

   -- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

      s : Number := 0;
      old_s : Number := 1;
      t : Number := 1;
      old_t : Number := 0;
      r : Number := B;
      old_r : Number := A;
      pr_r : Number;
      pr_s : Number;
      pr_t : Number;
      quo : Number;

      function floor(a,b : Number) return Number is
      begin
         return a/b - a rem b; -- check !
      end floor;


   begin
      while r /= 0 loop
         quo := floor(old_r,r); Put_line("quo = "& quo'Image);

         pr_r := r; Put_line("r = "& r'Image);
         r := old_r - quo*pr_r; Put_line("new r = "& r'Image);
         old_r := pr_r; Put_line("old r = "& old_r'Image);

         pr_s := s; Put_line("s = "& s'Image);
         s := old_s - quo*pr_s; Put_line("new s = "& s'Image);
         old_s := pr_s; Put_line("old s = "& old_s'Image);

         pr_t := t; Put_line("t = "& t'Image);
         t := old_t - quo * pr_t; Put_line("new t = "& t'Image);
         old_t := pr_t; Put_line("old t = "& old_t'Image);
      end loop;
      if s = 0 then
         -- Put_line("Division by zero");
         return (0,1);
      end if;


      if s = 1 then return (t,1);  end if; -- (for avoiding denominators equal to 1)
      if (s < 0) and (t < 0) then return (abs(t),abs(s)); end if;
      return (-t,abs(s));
   end XGCD;

end Generic_Rational.supporting_functions;
