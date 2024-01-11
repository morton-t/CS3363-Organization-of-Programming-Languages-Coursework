(*
 Insertion Sort - ML
 Program may be compiled and ran through command 'sml p1.sml'
*)

fun iinsort([], comp, discrim) = [] (* Case 0: List is empty, return nil *)
|	iinsort([h], comp, discrim) = [h] (* Case 1: List contains one element, thus is sorted *)
|	iinsort(h::T, comp, discrim) = (* Case 2: List contains >1 element and may not be sorted; begin sort *) 

		(* Call iinsert function and begin sort with the head of list.
		   Recurse iinsort with the remaining list elements *)
		iinsert(iinsort(T, comp, discrim), h, comp, discrim)


and (* Use of and to handle mutual recursion between the two functions *)


iinsert([], elem, comp, discrim) = [elem] (* Case 0: list is empty, insert the element into the list *)
|	iinsert(h::T, elem, comp, discrim) = (* Case 1: list is not empty. Begin searching for where to insert the element *)


	(* Check discriminator before proceeding - list sort depends on discriminator truth *)
	(* If discriminator is true, proceed with steps to remove duplicates *)
	if discrim(elem, h) then

		(* Evaluate the comparator - if it returns true, we remove the element to be
		   inserted by returning the subset list as-is *)
		if comp(elem, h) then
			h::T

		(* If the comparator returns false, the element does not go at the head,
		   and the element is a duplicate, thus we remove the head element and
		   recurse iinsert to insert insert the element *)		
		else
			iinsert(T, elem, comp, discrim)

	(* If discriminator is false, proceed with steps to maintain duplicate entries *)				
	else	
		
		(* Evaluate the comparator - if it returns true, we insert the element 
		   at the head of the subset of the list*)
		if comp(elem, h) then
			elem::h::T

		(* If the comparator returns false, the element does not go at the head,
		   thus we recurse iinsert to find where to insert the element *)		
		else
			h::iinsert(T, elem, comp, discrim)


fun nd(a:int, b:int) = (* Accepts two integers and always returns false. *)
	1 = 2
