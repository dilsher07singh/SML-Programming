(* Dilsher Singh , dsingh, 20545381 *)

fun sumDigits (n)=
if n<10 then n
else
sumDigits(n div 10)+n mod 10;


fun check([])=0
 | check(lst)=check(tl(lst))+1;

fun oneorzero a b= if (a = b) then 1 else 0;


fun firstcheck([],big,pastcomp)=[]
| firstcheck(lst, big, pastcomp)= if(check(lst)=big) then hd(lst)::firstcheck(tl(lst),big,hd(lst))
else (hd(lst)+pastcomp)::firstcheck(tl(lst),big,(hd(lst)+pastcomp));


fun frequencyPrefixSum([],n)=[]
| frequencyPrefixSum(lst,n:int)=
let val y= map (oneorzero n) lst
in firstcheck(y,check(y),0) end;


datatype 'a llist = LList of 'a llist list| Elem of 'a;

fun flatten (Elem g)=[g]
| flatten (LList xe)= List.concat (map flatten xe);


fun biggest[]=0
| biggest[x] = x
|biggest(one::two::rep) =
if one>two then biggest(one::rep) else biggest(two::rep);



fun depth (Elem g)=0
| depth (LList xe)= biggest(List.map depth xe)+1;


fun  equal (LList([]),LList([])) = true
| equal (Elem d, Elem e) = if (e=d) then true else false
| equal (LList x, LList ([])) = false
| equal (Elem c, LList x) = false
| equal (LList x,  Elem g) = false
| equal (LList ([]), LList x) = false
| equal(LList(lst1), LList(lst2)) = (equal(hd(lst1),hd(lst2)) andalso equal (LList(tl(lst1)),LList(tl(lst2))));
