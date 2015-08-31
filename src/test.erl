-module(test).
-compile(export_all).

test() ->
    Fac = fun Fac(0) -> 1;
	      Fac(N) -> N*Fac(N-1) 
	  end,
    Fac(10).

		  
