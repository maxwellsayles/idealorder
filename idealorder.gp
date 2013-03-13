/**
 * Picks a random bit size between 32 and 72, nbits.
 * Computes two primes roughly nbits/2 in size, and their product N.
 * 
 * run with: gp -p 5000000 -q idealorder.gp
 * and then type "main()" at the command prompt
 * and enter the appropriate information.
 */

\\primeForms = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47];
\\primeForms = [11,13,17,19,23];
\\primeFormCount = matsize(primeForms)[2];
firstPrimeForm = 11;
primeFormCount = 5;

squareFrees = [1,2,3,5,6,7,10];
squareFreeCount = matsize(squareFrees)[2];

nbitsLo = 68;
nbitsHi = 71;

\\ compute the number of bits in n
numbits(n) = floor(log(n)/log(2)) + 1

\\ keep picking random n bit numbers until one of them is prime
\\ should run in expected polylogarithmic time
nbitprime(n) =
{
   local(p);
   p = random(2^(n-1)) + 2^(n-1);
   while (!isprime(p), p = random(2^(n-1)) + 2^(n-1));
   p;
}

\\ compute a random n (or n-1) bit non-square semi-prime
closeSemiprime(n) = 
{
   local(p,l,q);
   p = nbitprime(floor(n/2));
   l = numbits(p);
   q = nbitprime(n-l);
   while (p == q, q = nbitprime(n-l));
   p*q;
}

\\ repeat until we have an n bit, non-square, semiprime
semiprime(n) = 
{
   local(x);
   x = closeSemiprime(n);
   while (numbits(x) != n, 
	  x = closeSemiprime(n)
	 );
   x;
}

\\ Compute a negative descriminant of D=-n*k
\\ such that D mod 4 is either 0 or 1.
\\ if D=-n*k mod 4 is not 0 or 1, then use D=-4*n*k. 
getDiscriminant(n, k) = 
{
   local(D);
   D = -n*k;
   if(D % 4 == 0 || D % 4 == 1, ,D=4*D);
   D;
}

\\ index is a subset index computed by taking 
\\ the mod of the number of times a factor occurs
\\ this returns the product of the subset
computeSubsetProduct(factors, index) =
{
   local(count, product, i, m, d);
   count = matsize(factors)[1];

   product = 1;
   for (i=1, count,
	d = factors[i,2] + 1;
	m = index % d;
	product = product * (factors[i,1] ^ m);
	index = floor(index / d);
       );
   
   product;
}

\\ given a form, and a matrix
\\ representing the factorization of the class number,
\\ this computes the order of the form
findOrder(form, idform, h, hfactors) =
{
   local(count, i, f, e, form2, loop);

   count = matsize(hfactors)[1];
   
   \\ iterate over each factor
   e = h;
   for (i = 1, count,
	f = hfactors[i,1];

	\\ continue to remove f until e no longer gives the identity
	loop = e % f == 0;
	while (loop,
	       if (e % f == 0,
		   form2 = form ^ (e / f);
		   if (form2 == idform,
		       e = e / f,
		       loop = 0;
		      );
		   ,
		   loop = 0;
		   );
	      );
       );
   e;
}

\\ Attempt to find the order of a primeform
tryPrimeForm(D, h, hfactors, idform, p) =
{
   local(form);
   
   if (D % p == 0, return(0));

   trap(, return(0), form = qfbprimeform(D, p));

   findOrder(form, idform, h, hfactors);
}

factorMatrixToVector(factors) =
{
   local(count, f, i, j, res);

   res = [];
   count = matsize(factors)[1];
   for (i = 1, count,
	f = [factors[i,1]];
	for (j = 1, factors[i,2],
	     res = concat(res, f);
	    );
	);
   res;
}

isNonTrivialFactor(n, k, D, p, e) =
{
   local(form, a, b, c, m, g);

   if (e % 2 == 1, return(0));

   \\ compute an ambiguous form
   form = qfbprimeform(D, p)^(e/2);
   a = component(form, 1);
   b = component(form, 2);
   c = component(form, 3);

   \\ ambiguous forms are of three types
   \\ (a, 0, c), (a, a, c), and (a, b, a)
   if (b == 0 || a == b, 
       m = a);
   if (a == c, 
       m = b + 2*a);

   g = gcd(m, n);
   1 - (g == 1 || g == n || g == k);
}


doEachPrimeForm(n, k, filename) =
{
   local(D, h, hfactors, idform, i, p, e, isfactor, v);

   D = getDiscriminant(n, k);
   h = qfbclassno(D);
   hfactors = factor(h);
   idform = qfbprimeform(D, 1)^h;

   p = firstPrimeForm;
   i = primeFormCount;
   while (i > 0,
          e = tryPrimeForm(D, h, hfactors, idform, p);
          if (e != 0,
              isfactor = isNonTrivialFactor(n, k, D, p, e);
              v = factorMatrixToVector(factor(e));

              write(filename, n, " ", k, " ", p, " ", e, " ", isfactor, " ", v);
              i --;
          );
          p = nextprime(p+2);
   );
}

\\ do each square free multiplier on 
\\ a random nbit semi prime discriminant
doEachMultiplier(nbits, filename) = 
{  
   local(n, k);

   n = semiprime(nbits);
   for (k = 1, squareFreeCount,
	doEachPrimeForm(n, squareFrees[k], filename);
       );
}

\\ do a bunch of different discriminants of nbits size
doNBits(nbits) =
{
   local(i, filename);

   filename = Str("ideal-", nbits, ".txt");

   for (i = 1, 250000,
	print(i);
	doEachMultiplier(nbits, filename);
       );
}

\\ program entry
main() =
{
   local(nbits);

\\   setrand(extern("date +%s"));
   print("Enter a random number seed:");
   setrand(input());

   print("Enter the number of bits in the semiprime discriminant:");
   nbits = input();
   print("Working on ", nbits, " bit discriminants...");
   doNBits(nbits);

\\   for (nbits=nbitsLo, nbitsHi,
\\	print("Working on ", nbits, " bit discriminants...");
\\	doNBits(nbits);
\\       );
}

\\main();
\\quit;
