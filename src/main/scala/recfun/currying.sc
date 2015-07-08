def comb(f:Int => Int, c: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int =
if(a > b) zero
else c(f(a),comb(f,c,zero)(a+1,b))

def sum(f:Int => Int)(a: Int, b: Int) = comb(f,(x,y)=>x+y,0)(a,b)
def coe(f:Int => Int)(a: Int, b: Int) = comb(f,(x,y)=>y-x,0)(a,b)
def pro(f:Int => Int)(a: Int, b: Int) = comb(f,(x,y)=>x*y,1)(a,b)

def fact(n: Int) = pro(x => x)(1,n)

sum(x => x)(1,10)
coe(x => x)(1,10)
fact(5)