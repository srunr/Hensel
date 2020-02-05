generic package Generic_Rational.supporting_functions is
  function GCD (A, B : Number) return Number;
  function LCM (A, B : Number) return Number;
  function Reduction(R : Rational) return Rational;
  function invmod (a : Number; n : Number) return Number;
  function MAX( a : Number; b : Number) return Number;
  function MIN( a : Number; b : Number) return Number;
  function XGCD (A, B : Number) return Rational;
end Generic_Rational.supporting_functions;
